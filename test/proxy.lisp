;;;; test/proxy.lisp — an in-process HTTP proxy for the CURLOPT_PROXY tests.
;;;;
;;;; A proxy is the only way to test proxying honestly.  Asserting that
;;;; CURLOPT_PROXY was accepted proves nothing -- every string is accepted --
;;;; and pointing it at a dead port only proves that connecting failed, which
;;;; it would have done anyway.  What has to be observed is that the request
;;;; went *there* instead of to the origin, and in the form a proxy expects.
;;;;
;;;; Two things distinguish a proxied request from a direct one, and this
;;;; fixture watches both:
;;;;
;;;;   The request line carries an absolute URI -- `GET http://host/path' --
;;;;   rather than just the path.  Nothing but a proxy request looks like that.
;;;;
;;;;   For an https URL the client opens a tunnel first, with
;;;;   `CONNECT host:443'.  The proxy sees the host and nothing else.
;;;;
;;;; It answers requests itself rather than forwarding them, reusing the origin
;;;; server's routing so every route is available through it.  Forwarding would
;;;; make the fixture a second HTTP client, which is the thing under test.
;;;; Nothing here needs the bytes to have made a second hop -- the tests are
;;;; about where libcurl sent them.
;;;;
;;;; The sharpest test this enables uses a hostname that does not resolve: with
;;;; a proxy the name is the proxy's problem and never looked up locally, so a
;;;; request for http://origin.invalid/ok succeeds through the proxy and fails
;;;; without it.  That cannot pass by accident.

(in-package #:curlcl/test)

(defstruct (test-proxy (:conc-name proxy-))
  port
  listener
  thread
  (running t)
  ;; Every request line seen, oldest first, exactly as it arrived.
  (log '() :type list)
  (lock (bt:make-lock "test proxy log"))
  ;; When true, demand Proxy-Authorization and answer 407 without it.
  (require-auth nil))

(defun proxy-request-target (path)
  "Split an absolute-form request target into (values path authority).

`GET http://host:port/a/b' is what a client sends to a proxy; AUTHORITY is
\"host:port\" and PATH is \"/a/b\".  Returns PATH unchanged and NIL when the
target is in origin form, which is what a client sends when it thinks it is
talking to the origin -- and is therefore the shape that means the proxy was
bypassed."
  (let ((mark (search "://" path)))
    (if (null mark)
        (values path nil)
        (let* ((after (+ mark 3))
               (slash (position #\/ path :start after)))
          (values (if slash (subseq path slash) "/")
                  (subseq path after (or slash (length path))))))))

(defun proxy-note (proxy request)
  (bt:with-lock-held ((proxy-lock proxy))
    (push request (proxy-log proxy))))

(defun handle-proxy-request (proxy request stream)
  (cond
    ;; An https URL through a proxy starts with a tunnel request.  Refusing it
    ;; is enough: the test is that the CONNECT arrived and names the origin,
    ;; not that a tunnel this fixture has no TLS for could be established.
    ((string-equal "CONNECT" (request-method request))
     (send-response stream 405 :body "no tunnels here" :close t)
     :close)

    ((and (proxy-require-auth proxy)
          (null (request-header (request-headers request) "proxy-authorization")))
     ;; 407, not 401: the challenge is from the proxy, and libcurl answers it
     ;; with Proxy-Authorization only if it understands which of the two it is.
     (send-response stream 407 :body "proxy authentication required"
                    :extra-headers
                    '(("Proxy-Authenticate" . "Basic realm=\"test proxy\""))))

    (t
     ;; Serve it from the origin's own routes, with the request rewritten into
     ;; origin form so the routing sees the path it expects.
     (multiple-value-bind (path authority) (proxy-request-target (request-path request))
       (declare (ignore authority))
       (let ((rewritten (copy-served-request request)))
         (setf (request-path rewritten) path)
         (handle-request proxy rewritten stream))))))

(defun serve-proxy-connection (proxy socket)
  (unwind-protect
       (let ((stream (usocket:socket-stream socket)))
         (loop for request = (handler-case (read-request stream) (error () nil))
               while request
               do (proxy-note proxy request)
                  (when (eq :close (handler-case (handle-proxy-request proxy request stream)
                                     (error () :close)))
                    (return))))
    (ignore-errors (usocket:socket-close socket))))

(defun start-test-proxy (&key require-auth)
  "Start a proxy on an ephemeral loopback port.  Returns a TEST-PROXY."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0
                                          :reuse-address t
                                          :element-type '(unsigned-byte 8)))
         (proxy (make-test-proxy :port (usocket:get-local-port listener)
                                 :listener listener
                                 :require-auth require-auth)))
    (setf (proxy-thread proxy)
          (bt:make-thread
           (lambda ()
             (accept-loop listener
                          (lambda () (proxy-running proxy))
                          (lambda (socket) (serve-proxy-connection proxy socket))
                          "curlcl test proxy connection"))
           :name "curlcl test proxy"))
    proxy))

(defun stop-test-proxy (proxy)
  ;; Unlike the origin server, which lives for the whole suite, a proxy is
  ;; started and stopped per test -- so this path runs constantly, and it is
  ;; where both of ACCEPT-LOOP's hazards were found.
  (stop-accepting (proxy-listener proxy)
                  (proxy-port proxy)
                  (lambda () (setf (proxy-running proxy) nil))
                  (proxy-thread proxy)))

(defun proxy-url (proxy)
  (format nil "http://127.0.0.1:~D" (proxy-port proxy)))

(defun proxy-requests (proxy)
  (bt:with-lock-held ((proxy-lock proxy))
    (reverse (proxy-log proxy))))

(defun proxy-saw-target-p (proxy target)
  "True when the proxy was asked for TARGET in absolute form."
  (find target (proxy-requests proxy) :key #'request-path :test #'string=))

(defmacro with-test-proxy ((var &key require-auth) &body body)
  `(let ((,var (start-test-proxy :require-auth ,require-auth)))
     (unwind-protect (progn ,@body)
       (stop-test-proxy ,var))))
