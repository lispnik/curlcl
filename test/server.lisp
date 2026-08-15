;;;; test/server.lisp — an in-process HTTP server for the integration tests.
;;;;
;;;; Small on purpose: it speaks just enough HTTP/1.1 to exercise the parts of
;;;; libcurl this binding drives, and nothing else.  Having it in-process means
;;;; the integration tests are hermetic -- no network, no fixture server to
;;;; start, no flakiness from someone else's rate limit -- and it can serve
;;;; deliberately hostile responses (a truncated body, a redirect loop, a
;;;; trickle of bytes) that no public endpoint will produce on demand.
;;;;
;;;; It binds port 0 and reports what the kernel assigned, so tests can run
;;;; concurrently and on a machine where anything might already be listening.
;;;; Keep-alive is supported because connection reuse is a thing worth testing.

(in-package #:libcurl/test)

(defstruct (test-server (:conc-name server-))
  port
  listener
  thread
  (running t)
  ;; Requests seen, newest first, so a test can assert on what libcurl actually
  ;; sent rather than only on what came back.
  (log '() :type list)
  (lock (bt:make-lock "test server log")))

(defstruct (request (:conc-name request-))
  method path version headers body)

(defvar *flaky-counts* (make-hash-table :test 'equal)
  "How many times each /flaky id has been requested.")
(defvar *flaky-lock* (bt:make-lock "flaky route"))

(defun reset-flaky (&optional id)
  (bt:with-lock-held (*flaky-lock*)
    (if id (remhash id *flaky-counts*) (clrhash *flaky-counts*))))

;;; Byte-level plumbing -------------------------------------------------------

(defun write-ascii (stream string)
  (loop for character across string
        do (write-byte (char-code character) stream)))

(defun write-octets (stream octets)
  (write-sequence octets stream))

(defun read-line-ascii (stream)
  "Read one CRLF-terminated line, or NIL at end of input."
  (let ((line (make-array 0 :element-type 'character
                            :adjustable t :fill-pointer t)))
    (loop for byte = (read-byte stream nil nil)
          do (cond ((null byte) (return (when (plusp (length line)) line)))
                   ((= byte 10) (return line))
                   ((= byte 13))        ; swallow CR
                   (t (vector-push-extend (code-char byte) line))))))

(defun parse-headers (stream)
  (loop for line = (read-line-ascii stream)
        while (and line (plusp (length line)))
        for colon = (position #\: line)
        when colon
          collect (cons (string-downcase (subseq line 0 colon))
                        (string-trim " " (subseq line (1+ colon))))))

(defun request-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defun read-chunked-body (stream)
  "Read a chunked request body.

Needed because an upload of unknown size -- which is what streaming from a pipe
produces -- goes out chunked, and a fixture that only understood Content-Length
would hang waiting for a body it had already been sent."
  (let ((body (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer t)))
    (loop for size-line = (read-line-ascii stream)
          for size = (when (and size-line (plusp (length size-line)))
                       (parse-integer size-line :radix 16 :junk-allowed t))
          while (and size (plusp size))
          do (let ((chunk (make-array size :element-type '(unsigned-byte 8))))
               (read-sequence chunk stream)
               (loop for byte across chunk do (vector-push-extend byte body))
               ;; The CRLF that terminates the chunk.
               (read-line-ascii stream))
          finally (when size (read-line-ascii stream)))
    (coerce body '(vector (unsigned-byte 8)))))

(defun read-request (stream)
  (let ((line (read-line-ascii stream)))
    (when (and line (plusp (length line)))
      (let* ((parts (uiop:split-string line :separator " "))
             (headers (parse-headers stream))
             (length (request-header headers "content-length"))
             (encoding (request-header headers "transfer-encoding"))
             (body (cond
                     ((and encoding (search "chunked" (string-downcase encoding)))
                      (read-chunked-body stream))
                     (length
                      (let ((octets (make-array (parse-integer length)
                                                :element-type '(unsigned-byte 8))))
                        (read-sequence octets stream)
                        octets)))))
        (make-request :method (first parts) :path (second parts)
                      :version (third parts) :headers headers :body body)))))

;;; Responses -----------------------------------------------------------------

(defun send-response (stream status &key (body "") (content-type "text/plain")
                                         extra-headers (close nil))
  (let ((octets (if (stringp body) (babel-encode body) body)))
    (write-ascii stream (format nil "HTTP/1.1 ~D ~A~C~C" status
                                (status-text status) #\Return #\Newline))
    (write-ascii stream (format nil "Content-Type: ~A~C~C" content-type
                                #\Return #\Newline))
    (write-ascii stream (format nil "Content-Length: ~D~C~C" (length octets)
                                #\Return #\Newline))
    (loop for (name . value) in extra-headers
          do (write-ascii stream (format nil "~A: ~A~C~C" name value
                                         #\Return #\Newline)))
    (write-ascii stream (format nil "Connection: ~A~C~C"
                                (if close "close" "keep-alive")
                                #\Return #\Newline))
    (write-ascii stream (format nil "~C~C" #\Return #\Newline))
    (write-octets stream octets)
    (force-output stream)))

(defun babel-encode (string)
  "UTF-8 octets for STRING, without pulling in a dependency for it."
  (cffi:with-foreign-string ((pointer length) string :null-terminated-p nil)
    (libcurl::foreign-to-octets pointer length)))

(defun status-text (status)
  (case status
    (200 "OK") (201 "Created") (204 "No Content") (301 "Moved Permanently")
    (302 "Found") (303 "See Other") (304 "Not Modified") (307 "Temporary Redirect")
    (308 "Permanent Redirect") (400 "Bad Request") (401 "Unauthorized")
    (403 "Forbidden") (404 "Not Found") (405 "Method Not Allowed")
    (418 "I'm a teapot") (429 "Too Many Requests") (500 "Internal Server Error")
    (502 "Bad Gateway") (503 "Service Unavailable") (t "Unknown")))

(defun path-query (path)
  "Split PATH into (values path alist-of-query-parameters)."
  (let ((mark (position #\? path)))
    (if (null mark)
        (values path '())
        (values (subseq path 0 mark)
                (loop for pair in (uiop:split-string (subseq path (1+ mark))
                                                     :separator "&")
                      for equals = (position #\= pair)
                      when equals
                        collect (cons (subseq pair 0 equals)
                                      (subseq pair (1+ equals))))))))

(defun query-integer (query name default)
  (let ((value (cdr (assoc name query :test #'string=))))
    (if value (or (parse-integer value :junk-allowed t) default) default)))

;;; Routing -------------------------------------------------------------------

(defun handle-request (server request stream)
  (declare (ignore server))
  (multiple-value-bind (path query) (path-query (request-path request))
    (cond
      ((string= path "/ok")
       (send-response stream 200 :body "ok"))

      ;; Reflects what libcurl actually sent, so a test can assert on the
      ;; request rather than only on the response.
      ((string= path "/echo")
       (send-response stream 200
                      :body (format nil "method=~A~%body=~A~%~{header ~A: ~A~%~}"
                                    (request-method request)
                                    (if (request-body request)
                                        (map 'string #'code-char (request-body request))
                                        "")
                                    (loop for (name . value) in (request-headers request)
                                          append (list name value)))))

      ((eql 0 (search "/status/" path))
       (let ((status (or (parse-integer path :start 8 :junk-allowed t) 500)))
         (send-response stream status :body (format nil "status ~D" status))))

      ;; N redirects then /ok, for exercising CURLOPT_FOLLOWLOCATION and
      ;; CURLOPT_MAXREDIRS.
      ((eql 0 (search "/redirect/" path))
       (let ((remaining (or (parse-integer path :start 10 :junk-allowed t) 0)))
         (if (plusp remaining)
             (send-response stream 302 :body ""
                            :extra-headers
                            (list (cons "Location"
                                        (format nil "/redirect/~D" (1- remaining)))))
             (send-response stream 302 :body ""
                            :extra-headers (list (cons "Location" "/ok"))))))

      ((string= path "/redirect-loop")
       (send-response stream 302 :body ""
                      :extra-headers (list (cons "Location" "/redirect-loop"))))

      ((string= path "/chunked")
       (send-chunked stream (query-integer query "n" 4)))

      ;; A slow trickle, for timeouts and progress callbacks.
      ((string= path "/drip")
       (send-drip stream (query-integer query "n" 5)
                  (query-integer query "ms" 50)))

      ((string= path "/large")
       (let ((size (query-integer query "bytes" 1000000)))
         (send-response stream 200
                        :body (make-array size :element-type '(unsigned-byte 8)
                                               :initial-element (char-code #\x))
                        :content-type "application/octet-stream")))

      ;; Duplicate header names, which a correct header representation has to
      ;; preserve rather than collapse.
      ((string= path "/headers/multi")
       (send-response stream 200 :body "multi"
                      :extra-headers '(("Set-Cookie" . "a=1")
                                       ("Set-Cookie" . "b=2")
                                       ("X-Repeated" . "first")
                                       ("X-Repeated" . "second"))))

      ((string= path "/auth/basic")
       (if (request-header (request-headers request) "authorization")
           (send-response stream 200 :body "authenticated")
           (send-response stream 401 :body "denied"
                          :extra-headers
                          '(("WWW-Authenticate" . "Basic realm=\"test\"")))))

      ((string= path "/cookie/set")
       (send-response stream 200 :body "cookie set"
                      :extra-headers '(("Set-Cookie" . "session=abc123; Path=/"))))

      ((string= path "/cookie/echo")
       (send-response stream 200
                      :body (or (request-header (request-headers request) "cookie")
                                "no cookie")))

      ;; Promises more than it delivers, then hangs up: libcurl reports
      ;; CURLE_PARTIAL_FILE.
      ((string= path "/close-early")
       (write-ascii stream (format nil "HTTP/1.1 200 OK~C~CContent-Length: 100~C~C~
Connection: close~C~C~C~C"
                                   #\Return #\Newline #\Return #\Newline
                                   #\Return #\Newline #\Return #\Newline))
       (write-ascii stream "only ten")
       (force-output stream)
       (return-from handle-request :close))

      ;; Fails the first N times it is asked, then succeeds.  Keyed by ID so
      ;; concurrent tests do not share a counter.  This is what makes retry
      ;; behaviour testable at all: a route that is reliably unreliable.
      ((string= path "/flaky")
       (let* ((id (or (cdr (assoc "id" query :test #'string=)) "default"))
              (failures (query-integer query "fail" 1))
              (status (query-integer query "status" 503))
              (seen (bt:with-lock-held (*flaky-lock*)
                      (incf (gethash id *flaky-counts* 0)))))
         (if (<= seen failures)
             (send-response stream status
                            :body (format nil "attempt ~D of ~D failing" seen failures))
             (send-response stream 200
                            :body (format nil "succeeded on attempt ~D" seen)))))

      ((string= path "/retry-after")
       (send-response stream 429 :body "slow down"
                      :extra-headers '(("Retry-After" . "1"))))

      (t (send-response stream 404 :body "not found")))
    (when (string-equal "close" (request-header (request-headers request) "connection"))
      :close)))

(defun send-chunked (stream count)
  (write-ascii stream (format nil "HTTP/1.1 200 OK~C~CContent-Type: text/plain~C~C~
Transfer-Encoding: chunked~C~C~C~C"
                              #\Return #\Newline #\Return #\Newline
                              #\Return #\Newline #\Return #\Newline))
  (dotimes (i count)
    (let ((piece (format nil "chunk~D " i)))
      (write-ascii stream (format nil "~X~C~C" (length piece) #\Return #\Newline))
      (write-ascii stream piece)
      (write-ascii stream (format nil "~C~C" #\Return #\Newline))))
  (write-ascii stream (format nil "0~C~C~C~C" #\Return #\Newline #\Return #\Newline))
  (force-output stream))

(defun send-drip (stream count delay-ms)
  ;; Each piece is exactly 8 bytes -- "drip" plus a 4-digit counter -- and the
  ;; advertised length has to agree.  It did not, once: the pieces were 7 bytes
  ;; against a promise of 8 apiece, and libcurl sat waiting for bytes that were
  ;; never coming, which is the correct thing for it to do and looked exactly
  ;; like a hang in the multi loop.
  (let ((body-length (* count 8)))
    (write-ascii stream (format nil "HTTP/1.1 200 OK~C~CContent-Type: text/plain~C~C~
Content-Length: ~D~C~C~C~C"
                                #\Return #\Newline #\Return #\Newline
                                body-length #\Return #\Newline #\Return #\Newline))
    (force-output stream)
    (dotimes (i count)
      (write-ascii stream (format nil "drip~4,'0D" i))
      (force-output stream)
      (sleep (/ delay-ms 1000.0)))))

;;; Lifecycle -----------------------------------------------------------------

(defun serve-connection (server socket)
  (unwind-protect
       (let ((stream (usocket:socket-stream socket)))
         ;; Keep-alive: keep reading requests off the same connection until the
         ;; client goes away or a handler asks to close.
         (loop for request = (handler-case (read-request stream) (error () nil))
               while request
               do (bt:with-lock-held ((server-lock server))
                    (push request (server-log server)))
                  (when (eq :close (handler-case (handle-request server request stream)
                                     (error () :close)))
                    (return))))
    (ignore-errors (usocket:socket-close socket))))

(defun start-test-server ()
  "Start the server on an ephemeral loopback port.  Returns a TEST-SERVER."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0
                                          :reuse-address t
                                          :element-type '(unsigned-byte 8)))
         (server (make-test-server :port (usocket:get-local-port listener)
                                   :listener listener)))
    (setf (server-thread server)
          (bt:make-thread
           (lambda ()
             (loop while (server-running server)
                   do (handler-case
                          (let ((socket (usocket:socket-accept listener)))
                            ;; A thread per connection, so tests that run
                            ;; several transfers at once against this server
                            ;; are not serialised by it.
                            (bt:make-thread
                             (lambda () (serve-connection server socket))
                             :name "libcurl test connection"))
                        (error () (return)))))
           :name "libcurl test server"))
    server))

(defun stop-test-server (server)
  (setf (server-running server) nil)
  (ignore-errors (usocket:socket-close (server-listener server)))
  (ignore-errors (bt:join-thread (server-thread server)))
  (values))

(defun server-url (server path)
  (format nil "http://127.0.0.1:~D~A" (server-port server) path))

(defun server-requests (server)
  (bt:with-lock-held ((server-lock server))
    (reverse (server-log server))))

(defun clear-server-log (server)
  (bt:with-lock-held ((server-lock server))
    (setf (server-log server) '())))

(defmacro with-test-server ((var) &body body)
  `(let ((,var (start-test-server)))
     (unwind-protect (progn ,@body)
       (stop-test-server ,var))))

;;; One server for the whole suite, started on demand.  Starting a fresh one per
;;; test would be tidier but costs a thread and a port each time, and nothing
;;; here keeps per-test state beyond the request log.
(defvar *server* nil)

(defun ensure-server ()
  (or *server* (setf *server* (start-test-server))))

(defun test-url (path)
  (server-url (ensure-server) path))
