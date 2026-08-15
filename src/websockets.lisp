;;;; src/websockets.lisp — the websocket API.
;;;;
;;;; Feature-gated at runtime rather than at build time, because whether it
;;;; works is a property of the libcurl that got loaded, not of this binding.
;;;; On macOS that distinction is live: the system libcurl in the dyld shared
;;;; cache ships the websockets *headers* but is built without ws/wss, so the
;;;; symbols resolve and then fail at connect time.  Asking
;;;; curl_version_info for the protocol list is the only honest test, and it
;;;; is what WEBSOCKETS-SUPPORTED-P does.
;;;;
;;;; libcurl's websocket support is still marked experimental upstream.  The
;;;; binding is complete, but that caveat belongs to libcurl and is passed on
;;;; rather than hidden.
;;;;
;;;; The model is that of a paused transfer: set :CONNECT-ONLY to 2 and
;;;; PERFORM does the HTTP upgrade and stops, after which frames are exchanged
;;;; with WS-SEND and WS-RECEIVE rather than through the write callback.

(in-package #:libcurl)

(cffi:defcfun ("curl_ws_recv" %curl-ws-recv) :int
  (handle :pointer) (buffer :pointer) (buflen :size)
  (received :pointer) (meta :pointer))

(cffi:defcfun ("curl_ws_send" %curl-ws-send) :int
  (handle :pointer) (buffer :pointer) (buflen :size) (sent :pointer)
  (fragsize curl-off-t) (flags :unsigned-int))

(cffi:defcfun ("curl_ws_meta" %curl-ws-meta) :pointer (handle :pointer))

(defstruct (ws-frame (:conc-name frame-))
  "Metadata about the websocket frame a receive belongs to."
  (flags '() :type list)
  (offset 0)
  (bytes-left 0)
  (length 0))

(setf (documentation 'frame-flags 'function)
      "What kind of frame this is, as a list of keywords: :TEXT, :BINARY,
:CONTINUATION, :CLOSE, :PING, :PONG, :OFFSET."
      (documentation 'frame-offset 'function)
      "Where this piece starts within the frame being assembled."
      (documentation 'frame-bytes-left 'function)
      "How many bytes of this frame have yet to arrive.

Non-zero means the frame is incomplete and WS-RECEIVE should be called again;
a frame larger than the receive buffer arrives over several calls."
      (documentation 'frame-length 'function)
      "The total size of the frame this piece belongs to.")

(defun websockets-supported-p ()
  "True when the loaded libcurl can actually speak ws:// and wss://.

Not the same as the symbols existing: macOS ships headers for a libcurl built
without websocket support."
  (and (protocol-supported-p :ws) t))

(defun ensure-websockets ()
  (unless (websockets-supported-p)
    (error 'unsupported-feature
           :name "websocket support (no ws:// in this libcurl's protocol list)"
           :message "The loaded libcurl was built without websockets.")))

(defparameter *ws-flags*
  '((:text . #.(ash 1 0))
    (:binary . #.(ash 1 1))
    (:continuation . #.(ash 1 2))
    (:close . #.(ash 1 3))
    (:ping . #.(ash 1 4))
    (:offset . #.(ash 1 5))
    (:pong . #.(ash 1 6))))

(defun ws-flags-value (flags)
  (let ((value 0))
    (dolist (flag (alexandria:ensure-list flags) value)
      (let ((bit (cdr (assoc flag *ws-flags*))))
        (unless bit
          (error 'curl-error :message (format nil "Unknown websocket flag ~S." flag)))
        (setf value (logior value bit))))))

(defun ws-flags-list (value)
  (loop for (name . bit) in *ws-flags*
        when (logtest value bit) collect name))

(defun %decode-ws-frame (pointer)
  (unless (cffi:null-pointer-p pointer)
    (make-ws-frame
     :flags (ws-flags-list
             (cffi:foreign-slot-value pointer '(:struct curl-ws-frame) 'flags))
     :offset (cffi:foreign-slot-value pointer '(:struct curl-ws-frame) 'offset)
     :bytes-left (cffi:foreign-slot-value pointer '(:struct curl-ws-frame) 'bytesleft)
     :length (cffi:foreign-slot-value pointer '(:struct curl-ws-frame) 'len))))

(defun ws-connect (url &rest options)
  "Open a websocket connection and return the handle, upgraded and idle.

CURLOPT_CONNECT_ONLY set to 2 is what asks libcurl to perform the HTTP upgrade
and then stop, leaving the connection open for WS-SEND and WS-RECEIVE.  The
caller closes the handle."
  (ensure-websockets)
  (let ((handle (make-easy-handle))
        (completed nil))
    (unwind-protect
         (progn
           (apply #'setopts handle :url url options)
           ;; 2, not 1: 1 is the plain "connect and stop" used for raw sockets,
           ;; 2 additionally performs the websocket upgrade.
           (setopt handle :connect-only 2)
           (perform handle)
           (setf completed t)
           handle)
      (unless completed (close-handle handle)))))

(defmacro with-websocket ((var url &rest options) &body body)
  "Run BODY with VAR bound to an open websocket, closed on exit."
  `(let ((,var (ws-connect ,url ,@options)))
     (unwind-protect (progn ,@body)
       (close-handle ,var))))

(defun ws-send (handle data &key (type :binary) (fragment-size 0) flags)
  "Send one websocket frame.  Returns the number of bytes sent.

TYPE is :TEXT, :BINARY, :PING, :PONG or :CLOSE.  FRAGMENT-SIZE is non-zero only
when sending a frame in pieces, in which case it is the total size of the frame
being assembled."
  (check-open handle)
  (ensure-websockets)
  (let* ((octets (coerce-to-octets data))
         (length (length octets))
         (bits (logior (ws-flags-value type) (ws-flags-value flags))))
    (cffi:with-foreign-object (sent :size)
      (setf (cffi:mem-ref sent :size) 0)
      (cffi:with-pointer-to-vector-data
          (pointer (if (plusp length)
                       octets
                       ;; A zero-length frame is legitimate -- an empty PING or
                       ;; CLOSE -- but the pointer still has to be valid.
                       (make-array 1 :element-type '(unsigned-byte 8))))
        (%check-easy (%curl-ws-send (handle-pointer handle) pointer length sent
                                    fragment-size bits)))
      (cffi:mem-ref sent :size))))

(defun ws-receive (handle &key (buffer-size 65536))
  "Receive part of a websocket frame.

Returns (values octets frame), or (values NIL NIL) when nothing is available
yet -- libcurl answers CURLE_AGAIN for a non-blocking read with no data, which
is a normal state rather than a failure.  A frame larger than BUFFER-SIZE
arrives over several calls; FRAME-BYTES-LEFT says how much of it remains."
  (check-open handle)
  (ensure-websockets)
  (cffi:with-foreign-object (received :size)
    (cffi:with-foreign-object (meta :pointer)
      (setf (cffi:mem-ref received :size) 0
            (cffi:mem-ref meta :pointer) (cffi:null-pointer))
      (let ((buffer (cffi:foreign-alloc :uint8 :count buffer-size)))
        (unwind-protect
             (let ((code (%curl-ws-recv (handle-pointer handle) buffer buffer-size
                                        received meta)))
               (cond
                 ;; CURLE_AGAIN: nothing to read right now.
                 ((= code (curlcode-value :again)) (values nil nil))
                 (t (%check-easy code)
                    (values (foreign-to-octets buffer (cffi:mem-ref received :size))
                            (%decode-ws-frame (cffi:mem-ref meta :pointer))))))
          (cffi:foreign-free buffer))))))

(defun ws-frame-info (handle)
  "Metadata for the frame currently being received, or NIL."
  (check-open handle)
  (ensure-websockets)
  (%decode-ws-frame (%curl-ws-meta (handle-pointer handle))))

(defun ws-close (handle &key (code 1000) reason)
  "Send a websocket close frame.

CODE is the RFC 6455 status -- 1000 is a normal closure -- and it goes in the
first two bytes of the payload, network byte order, followed by an optional
reason."
  (let* ((reason-octets (if reason (coerce-to-octets reason) #()))
         (payload (make-array (+ 2 (length reason-octets))
                              :element-type '(unsigned-byte 8))))
    (setf (aref payload 0) (ldb (byte 8 8) code)
          (aref payload 1) (ldb (byte 8 0) code))
    (replace payload reason-octets :start1 2)
    (ws-send handle payload :type :close)))

(defun ws-send-text (handle string)
  "Send STRING as a text frame."
  (ws-send handle (coerce-to-octets string) :type :text))

(defun ws-receive-text (handle &key (buffer-size 65536))
  "Receive a frame and decode it as UTF-8 text, or NIL if nothing is ready."
  (multiple-value-bind (octets frame) (ws-receive handle :buffer-size buffer-size)
    (when octets
      (values (octets-to-string octets) frame))))

;;; curl_ws_start_frame arrived in 8.21.0, so it is resolved at load time
;;; rather than declared: a DEFCFUN against a symbol an older libcurl does not
;;; export would fail on the first call rather than being reportable here.
(defvar *ws-start-frame-function*
  (cffi:foreign-symbol-pointer "curl_ws_start_frame"))

(defun ws-start-frame-supported-p ()
  "True when the loaded libcurl exports curl_ws_start_frame (8.21.0 or newer).

Separate from WEBSOCKETS-SUPPORTED-P: a libcurl can have websockets and still
lack this call, which is the newer way to begin a frame of known length."
  (and *ws-start-frame-function*
       (not (cffi:null-pointer-p *ws-start-frame-function*))))

(defun ws-start-frame (handle type frame-length)
  "Begin a frame of FRAME-LENGTH bytes, to be filled by later WS-SEND calls.

Requires libcurl 8.21.0 or newer; signals UNSUPPORTED-FEATURE otherwise."
  (check-open handle)
  (unless (ws-start-frame-supported-p)
    (error 'unsupported-feature
           :name "curl_ws_start_frame (libcurl 8.21.0 or newer)"
           :message "This libcurl does not export curl_ws_start_frame."))
  (%check-easy
   (cffi:foreign-funcall-pointer *ws-start-frame-function* ()
                                 :pointer (handle-pointer handle)
                                 :unsigned-int (ws-flags-value type)
                                 :int64 frame-length
                                 :int)))
