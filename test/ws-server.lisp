;;;; test/ws-server.lisp — a websocket echo server for the integration tests.
;;;;
;;;; libcurl's websocket support cannot be tested without a peer that speaks
;;;; the protocol, and the alternative to this is testing against something on
;;;; the public internet -- which would make the suite non-hermetic for the one
;;;; subsystem where an unreliable answer is least useful.
;;;;
;;;; The handshake needs SHA-1 and base64, so both are here.  A hand-written
;;;; SHA-1 in a test fixture is a risk in itself, so it is checked against the
;;;; published FIPS 180 vectors before anything relies on it -- see
;;;; SHA-1-MATCHES-THE-PUBLISHED-VECTORS in ws-tests.lisp.  If that test fails,
;;;; nothing else in the websocket suite means anything.
;;;;
;;;; Only what the tests need is implemented: a single connection at a time,
;;;; frames up to 64 KiB, no extensions, no fragmentation on the server side.

(in-package #:libcurl/test)

;;; SHA-1 ---------------------------------------------------------------------

(defun sha1 (octets)
  "The SHA-1 digest of OCTETS, as 20 octets."
  (let* ((message-length (length octets))
         (bit-length (* 8 message-length))
         ;; Pad to 56 mod 64, then eight bytes of big-endian bit count.
         (padded-length (* 64 (ceiling (+ message-length 9) 64)))
         (message (make-array padded-length :element-type '(unsigned-byte 8)
                                            :initial-element 0))
         (h (make-array 5 :element-type '(unsigned-byte 32)
                          :initial-contents '(#x67452301 #xEFCDAB89 #x98BADCFE
                                              #x10325476 #xC3D2E1F0)))
         (w (make-array 80 :element-type '(unsigned-byte 32))))
    (replace message octets)
    (setf (aref message message-length) #x80)
    (loop for i below 8
          do (setf (aref message (- padded-length 1 i))
                   (ldb (byte 8 (* 8 i)) bit-length)))
    (flet ((rotl (x n) (ldb (byte 32 0) (logior (ash x n) (ash x (- n 32))))))
      (loop for chunk from 0 below padded-length by 64
            do (loop for i below 16
                     do (setf (aref w i)
                              (logior (ash (aref message (+ chunk (* 4 i))) 24)
                                      (ash (aref message (+ chunk (* 4 i) 1)) 16)
                                      (ash (aref message (+ chunk (* 4 i) 2)) 8)
                                      (aref message (+ chunk (* 4 i) 3)))))
               (loop for i from 16 below 80
                     do (setf (aref w i)
                              (rotl (logxor (aref w (- i 3)) (aref w (- i 8))
                                            (aref w (- i 14)) (aref w (- i 16)))
                                    1)))
               (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2))
                     (d (aref h 3)) (e (aref h 4)))
                 (loop for i below 80
                       do (multiple-value-bind (f k)
                              (cond ((< i 20) (values (logior (logand b c)
                                                              (logand (ldb (byte 32 0)
                                                                           (lognot b))
                                                                      d))
                                                      #x5A827999))
                                    ((< i 40) (values (logxor b c d) #x6ED9EBA1))
                                    ((< i 60) (values (logior (logand b c)
                                                              (logand b d)
                                                              (logand c d))
                                                      #x8F1BBCDC))
                                    (t (values (logxor b c d) #xCA62C1D6)))
                            (let ((temp (ldb (byte 32 0)
                                             (+ (rotl a 5) f e k (aref w i)))))
                              (setf e d d c c (rotl b 30) b a a temp))))
                 (setf (aref h 0) (ldb (byte 32 0) (+ (aref h 0) a))
                       (aref h 1) (ldb (byte 32 0) (+ (aref h 1) b))
                       (aref h 2) (ldb (byte 32 0) (+ (aref h 2) c))
                       (aref h 3) (ldb (byte 32 0) (+ (aref h 3) d))
                       (aref h 4) (ldb (byte 32 0) (+ (aref h 4) e))))))
    (let ((digest (make-array 20 :element-type '(unsigned-byte 8))))
      (loop for i below 5
            do (loop for j below 4
                     do (setf (aref digest (+ (* 4 i) j))
                              (ldb (byte 8 (* 8 (- 3 j))) (aref h i)))))
      digest)))

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun base64 (octets)
  (with-output-to-string (out)
    (loop for i from 0 below (length octets) by 3
          for remaining = (- (length octets) i)
          for b0 = (aref octets i)
          for b1 = (if (> remaining 1) (aref octets (+ i 1)) 0)
          for b2 = (if (> remaining 2) (aref octets (+ i 2)) 0)
          do (write-char (char +base64-alphabet+ (ash b0 -2)) out)
             (write-char (char +base64-alphabet+
                               (logior (ash (logand b0 #x03) 4) (ash b1 -4)))
                         out)
             (write-char (if (> remaining 1)
                             (char +base64-alphabet+
                                   (logior (ash (logand b1 #x0F) 2) (ash b2 -6)))
                             #\=)
                         out)
             (write-char (if (> remaining 2)
                             (char +base64-alphabet+ (logand b2 #x3F))
                             #\=)
                         out))))

(defparameter +websocket-guid+ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "The fixed GUID RFC 6455 says to append to the client key before hashing.")

(defun websocket-accept-key (client-key)
  (base64 (sha1 (babel-encode (concatenate 'string client-key +websocket-guid+)))))

;;; Framing -------------------------------------------------------------------

(defun read-ws-frame (stream)
  "Read one client frame.  Returns (values opcode payload finalp), or NIL at EOF."
  (let ((first-byte (read-byte stream nil nil)))
    (when first-byte
      (let* ((finalp (logbitp 7 first-byte))
             (opcode (logand first-byte #x0F))
             (second-byte (read-byte stream))
             (maskedp (logbitp 7 second-byte))
             (length (logand second-byte #x7F)))
        (cond ((= length 126)
               (setf length (logior (ash (read-byte stream) 8) (read-byte stream))))
              ((= length 127)
               (setf length 0)
               (dotimes (i 8)
                 (setf length (logior (ash length 8) (read-byte stream))))))
        (let ((mask (when maskedp
                      (let ((m (make-array 4 :element-type '(unsigned-byte 8))))
                        (read-sequence m stream)
                        m)))
              (payload (make-array length :element-type '(unsigned-byte 8))))
          (read-sequence payload stream)
          ;; RFC 6455 requires client frames to be masked; libcurl masks.
          (when mask
            (dotimes (i length)
              (setf (aref payload i)
                    (logxor (aref payload i) (aref mask (mod i 4))))))
          (values opcode payload finalp))))))

(defun write-ws-frame (stream opcode payload &key (finalp t))
  "Write one server frame.  Server frames are never masked."
  (write-byte (logior (if finalp #x80 0) opcode) stream)
  (let ((length (length payload)))
    (cond ((< length 126) (write-byte length stream))
          ((< length 65536)
           (write-byte 126 stream)
           (write-byte (ldb (byte 8 8) length) stream)
           (write-byte (ldb (byte 8 0) length) stream))
          (t (write-byte 127 stream)
             (loop for i from 7 downto 0
                   do (write-byte (ldb (byte 8 (* 8 i)) length) stream))))
    (write-sequence payload stream)
    (force-output stream)))

(defconstant +ws-opcode-continuation+ #x0)
(defconstant +ws-opcode-text+ #x1)
(defconstant +ws-opcode-binary+ #x2)
(defconstant +ws-opcode-close+ #x8)
(defconstant +ws-opcode-ping+ #x9)
(defconstant +ws-opcode-pong+ #xA)

;;; The server ----------------------------------------------------------------

(defstruct (ws-server (:conc-name ws-))
  port listener thread (running t))

(defun serve-websocket (socket)
  (unwind-protect
       (let* ((stream (usocket:socket-stream socket))
              (request (read-request stream)))
         (when request
           (let ((key (request-header (request-headers request)
                                      "sec-websocket-key")))
             (cond
               ((null key)
                (send-response stream 400 :body "not a websocket handshake"))
               (t
                (write-ascii stream
                             (format nil "HTTP/1.1 101 Switching Protocols~C~C~
Upgrade: websocket~C~CConnection: Upgrade~C~C~
Sec-WebSocket-Accept: ~A~C~C~C~C"
                                     #\Return #\Newline #\Return #\Newline
                                     #\Return #\Newline
                                     (websocket-accept-key key)
                                     #\Return #\Newline #\Return #\Newline))
                (force-output stream)
                ;; Echo until the client closes.
                (loop
                  (multiple-value-bind (opcode payload finalp)
                      (read-ws-frame stream)
                    (declare (ignore finalp))
                    (when (null opcode) (return))
                    (cond
                      ((= opcode +ws-opcode-close+)
                       (write-ws-frame stream +ws-opcode-close+ payload)
                       (return))
                      ((= opcode +ws-opcode-ping+)
                       (write-ws-frame stream +ws-opcode-pong+ payload))
                      ((= opcode +ws-opcode-pong+))
                      (t
                       ;; Echo text as text and binary as binary, so the
                       ;; client can assert on the frame type it gets back.
                       (write-ws-frame stream opcode payload))))))))))
    (ignore-errors (usocket:socket-close socket))))

(defun start-ws-server ()
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0
                                          :reuse-address t
                                          :element-type '(unsigned-byte 8)))
         (server (make-ws-server :port (usocket:get-local-port listener)
                                 :listener listener)))
    (setf (ws-thread server)
          (bt:make-thread
           (lambda ()
             (loop while (ws-running server)
                   do (handler-case
                          (let ((socket (usocket:socket-accept listener)))
                            (bt:make-thread (lambda () (serve-websocket socket))
                                            :name "libcurl ws connection"))
                        (error () (return)))))
           :name "libcurl ws server"))
    server))

(defun stop-ws-server (server)
  (setf (ws-running server) nil)
  (ignore-errors (usocket:socket-close (ws-listener server)))
  (ignore-errors (bt:join-thread (ws-thread server)))
  (values))

(defmacro with-ws-server ((var) &body body)
  `(let ((,var (start-ws-server)))
     (unwind-protect (progn ,@body)
       (stop-ws-server ,var))))

(defun ws-server-url (server &optional (path "/"))
  (format nil "ws://127.0.0.1:~D~A" (ws-port server) path))
