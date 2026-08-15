;;;; test/ws-tests.lisp — websockets, against a local echo server.
;;;;
;;;; Every test here is skipped when the loaded libcurl was built without
;;;; ws/wss, which is the case for the libcurl macOS ships in its dyld shared
;;;; cache.  Skipping rather than failing is deliberate: the binding is correct
;;;; either way, and a red suite on a machine whose libcurl simply lacks the
;;;; feature would train people to ignore it.

(in-package #:curlcl/test)

(in-suite websockets)

(defmacro with-websockets-or-skip (&body body)
  `(if (websockets-supported-p)
       (progn ,@body)
       (skip "the loaded libcurl (~A) was built without websocket support"
             (libcurl-version))))

(defun receive-octets-with-retry (handle &key (attempts 400))
  "Poll WS-RECEIVE until a frame arrives.

The connection is non-blocking, so libcurl answers CURLE_AGAIN -- reported as
(values NIL NIL) -- until the echo comes back.  That is a normal state rather
than a failure, so the caller polls."
  (loop repeat attempts
        do (multiple-value-bind (octets frame) (ws-receive handle)
             (when octets (return (values octets frame)))
             (sleep 0.005))
        finally (return (values nil nil))))

(defun receive-with-retry (handle &key (attempts 400))
  (multiple-value-bind (octets frame) (receive-octets-with-retry handle
                                                                 :attempts attempts)
    (values (when octets (curlcl::octets-to-string octets)) frame)))

(test sha1-matches-the-published-vectors
  ;; The handshake depends on the SHA-1 in ws-server.lisp, which is hand
  ;; written.  If this fails, every other websocket test is meaningless -- so
  ;; it is checked against FIPS 180 before anything relies on it.
  (flet ((hex (octets)
           (string-downcase (format nil "~{~2,'0X~}" (coerce octets 'list)))))
    (is (string= "da39a3ee5e6b4b0d3255bfef95601890afd80709"
                 (hex (sha1 #()))))
    (is (string= "a9993e364706816aba3e25717850c26c9cd0d89d"
                 (hex (sha1 (babel-encode "abc")))))
    (is (string= "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
                 (hex (sha1 (babel-encode
                             "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")))))
    ;; A message long enough to need more than one 64-byte block and to
    ;; exercise the length padding.
    (is (string= "34aa973cd4c4daa4f61eeb2bdbad27316534016f"
                 (hex (sha1 (babel-encode (make-string 1000000
                                                       :initial-element #\a))))))))

(test base64-matches-its-published-vectors
  (is (string= "" (base64 #())))
  (is (string= "Zg==" (base64 (babel-encode "f"))))
  (is (string= "Zm8=" (base64 (babel-encode "fo"))))
  (is (string= "Zm9v" (base64 (babel-encode "foo"))))
  (is (string= "Zm9vYmFy" (base64 (babel-encode "foobar")))))

(test the-handshake-key-matches-rfc-6455
  ;; The worked example from RFC 6455 section 1.3.
  (is (string= "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
               (websocket-accept-key "dGhlIHNhbXBsZSBub25jZQ=="))))

(test websocket-support-is-detected-not-assumed
  ;; The symbols exist in any libcurl new enough to have the header; whether
  ;; ws:// actually works is a different question, and this is the one that
  ;; matters.
  (is (eq (websockets-supported-p)
          (and (member "ws" (libcurl-protocols) :test #'string-equal) t))))

(test connecting-without-support-signals-rather-than-hanging
  (unless (websockets-supported-p)
    (signals unsupported-feature (ws-connect "ws://127.0.0.1:1/"))))

(test a-text-frame-round-trips
  (with-websockets-or-skip
    (with-ws-server (server)
      (with-websocket (handle (ws-server-url server))
        (ws-send-text handle "hello websocket")
        (multiple-value-bind (text frame) (receive-with-retry handle)
          (is (string= "hello websocket" text))
          (is (member :text (frame-flags frame))))))))

(test a-binary-frame-round-trips
  (with-websockets-or-skip
    (with-ws-server (server)
      (with-websocket (handle (ws-server-url server))
        (let ((payload (make-array 5 :element-type '(unsigned-byte 8)
                                     :initial-contents '(0 1 254 255 0))))
          (ws-send handle payload :type :binary)
          (multiple-value-bind (octets frame) (receive-octets-with-retry handle)
            (is (equalp payload octets))
            (is (member :binary (frame-flags frame)))))))))

(test several-frames-round-trip-in-order
  (with-websockets-or-skip
    (with-ws-server (server)
      (with-websocket (handle (ws-server-url server))
        (dolist (message '("one" "two" "three"))
          (ws-send-text handle message)
          (is (string= message (receive-with-retry handle))))))))

(test a-ping-is-answered-with-a-pong
  (with-websockets-or-skip
    (with-ws-server (server)
      (with-websocket (handle (ws-server-url server))
        (ws-send handle "are you there" :type :ping)
        (multiple-value-bind (octets frame) (receive-octets-with-retry handle)
          (declare (ignore octets))
          ;; libcurl may answer the ping itself, so accept either a :PONG
          ;; delivered to us or nothing at all -- what must not happen is an
          ;; error or a hang.
          (is (or (null frame) (member :pong (frame-flags frame))
                  (member :ping (frame-flags frame)))))))))

(test a-large-frame-arrives-across-several-reads
  ;; Bigger than the receive buffer, so FRAME-BYTES-LEFT has to be used to know
  ;; when the frame is complete.
  (with-websockets-or-skip
    (with-ws-server (server)
      (with-websocket (handle (ws-server-url server))
        (let ((payload (make-array 40000 :element-type '(unsigned-byte 8)
                                         :initial-element 65)))
          (ws-send handle payload :type :binary)
          (let ((collected (make-array 0 :element-type '(unsigned-byte 8)
                                         :adjustable t :fill-pointer t)))
            (loop repeat 500
                  do (multiple-value-bind (octets frame)
                         (ws-receive handle :buffer-size 8192)
                       (when octets
                         (loop for byte across octets
                               do (vector-push-extend byte collected))
                         (when (zerop (frame-bytes-left frame))
                           (return))))
                     (sleep 0.005))
            (is (= 40000 (length collected)))
            (is (every (lambda (b) (= b 65)) collected))))))))

(test closing-sends-a-close-frame
  (with-websockets-or-skip
    (with-ws-server (server)
      (with-websocket (handle (ws-server-url server))
        (finishes (ws-close handle :code 1000 :reason "done"))))))

(test start-frame-is-gated-on-the-libcurl-version
  ;; curl_ws_start_frame arrived in 8.21.0.  On anything older the binding must
  ;; say so rather than calling a symbol that is not there.
  (if (ws-start-frame-supported-p)
      (is (version-at-least-p 8 21))
      (with-easy (handle)
        (signals unsupported-feature
          (ws-start-frame handle :binary 10)))))
