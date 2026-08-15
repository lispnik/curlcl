;;;; src/client-request.lisp — the HTTP client.
;;;;
;;;; What this layer does and does not do is a deliberate split.
;;;;
;;;; Delegated to libcurl, because it is already correct there and a Lisp
;;;; reimplementation would be strictly worse: redirect following and the
;;;; method-rewriting rules that go with it, the cookie engine and jar format,
;;;; content decompression, TLS verification, HTTP/2 and HTTP/3, proxies, and
;;;; every authentication scheme.
;;;;
;;;; Done here, because libcurl has no equivalent: retry and backoff, charset
;;;; decoding, the request and response objects, form and multipart encoding of
;;;; Lisp values, and connection pooling across handles.
;;;;
;;;; The verbs are named HTTP-GET and HTTP-POST rather than GET and POST
;;;; because CL:GET and CL:DELETE already exist, and shadowing them in a
;;;; library that other code will :USE causes more trouble than the shorter
;;;; name is worth.

(in-package #:libcurl)

(defparameter *default-user-agent*
  (format nil "libcurl-lisp/~A libcurl/~A"
          "0.1.0" (or (ignore-errors (libcurl-version)) "unknown"))
  "Sent unless the caller says otherwise.  libcurl sends none by default, and
a request with no User-Agent is rejected or throttled by enough servers to make
silence the wrong default.")

(defparameter *default-timeout* 300
  "Seconds before a whole transfer is abandoned.  libcurl's own default is no
limit at all, which turns one unlucky request into a hung program.")

(defparameter *default-connect-timeout* 30)

(defun normalise-headers (headers)
  "Accept headers as an alist, a plist, or a list of \"Name: value\" strings.

All three appear in real code and none is obviously right, so all three are
taken.  Returns a list of strings in libcurl's form."
  (cond
    ((null headers) '())
    ;; A list of strings is already what libcurl wants.
    ((every #'stringp headers) (copy-list headers))
    ;; An alist: ((\"Accept\" . \"application/json\") ...)
    ((every #'consp headers)
     (mapcar (lambda (pair)
               (format nil "~A: ~A" (string (car pair))
                       (if (consp (cdr pair)) (second pair) (cdr pair))))
             headers))
    ;; A plist: (:accept \"application/json\" ...)
    (t (loop for (name value) on headers by #'cddr
             collect (format nil "~A: ~A" (header-name-string name) value)))))

(defun header-name-string (name)
  "A keyword like :CONTENT-TYPE becomes \"Content-Type\"."
  (if (stringp name)
      name
      (let ((text (string-downcase (substitute #\- #\_ (string name)))))
        (with-output-to-string (out)
          (loop with capitalise = t
                for character across text
                do (write-char (if capitalise (char-upcase character) character) out)
                   (setf capitalise (char= character #\-)))))))

(defun url-encode-form (alist)
  "Encode an alist as application/x-www-form-urlencoded.

Uses libcurl's own escaper through a scratch handle, so the encoding matches
what libcurl would do elsewhere."
  (with-easy (scratch)
    (with-output-to-string (out)
      (loop for (name . value) in alist
            for first = t then nil
            do (unless first (write-char #\& out))
               (write-string (url-escape scratch (string name)) out)
               (write-char #\= out)
               (write-string (url-escape scratch (princ-to-string
                                                 (if (consp value)
                                                     (first value)
                                                     value)))
                             out)))))

(defun apply-method (handle method)
  "Set the request method, using the option libcurl prefers for each.

CURLOPT_CUSTOMREQUEST changes the method string and nothing else, which is
right for DELETE and PATCH but wrong for HEAD: libcurl would go on expecting a
body that is never coming and wait for it.  HEAD therefore goes through
CURLOPT_NOBODY, and GET and POST through the options that also set up the rest
of their behaviour.  Any other method is passed through verbatim."
  (case method
    (:get (setopt handle :httpget t))
    (:head (setopt handle :nobody t))
    (:post (setopt handle :post t))
    (t (unless (or (stringp method) (symbolp method))
         (error 'curl-error
                :message (format nil "Cannot use ~S as an HTTP method." method)))
       (setopt handle :customrequest (string-upcase (string method)))))
  method)

(defun apply-content (handle content multipart method)
  "Attach a request body.  Returns the Content-Type it implies, or NIL.

CONTENT may be a string or octets (sent as-is), or an alist (form-encoded).
MULTIPART is a list of part plists and goes through curl_mime.

A POST with no body still needs one declared.  CURLOPT_POST tells libcurl to
expect a request body, and with no size and no read callback it goes looking
for one -- historically on stdin.  An explicit empty body makes it send
Content-Length: 0 and move on."
  (cond
    (multipart (set-mime-body handle multipart) nil)
    ((null content)
     (when (eq method :post) (setopt handle :postfields ""))
     nil)
    ((or (stringp content) (typep content '(array (unsigned-byte 8) (*))))
     (setopt handle :postfields content)
     nil)
    ((and (consp content) (every #'consp content))
     (setopt handle :postfields (url-encode-form content))
     "application/x-www-form-urlencoded")
    (t (error 'curl-error
              :message (format nil "Cannot use ~S as a request body." content)))))

(defun plist-value (plist key default)
  "Like GETF, but distinguishes an explicit NIL from an absent key.

That distinction is load-bearing for :RETRY, where NIL means \"do not retry
this one\" and absent means \"use the batch policy\".  GETF collapses the two."
  (loop for (present-key value) on plist by #'cddr
        when (eq present-key key) return value
        finally (return default)))

(defun make-body-sink (handle output on-data)
  "Install a write callback and return a closure yielding the collected body.

With OUTPUT or ON-DATA the body is handed on as it arrives and nothing is
accumulated, which is what makes a large download not have to fit in memory."
  (cond
    (on-data
     (setf (callback-function handle :write)
           (lambda (octets) (funcall on-data octets) t))
     (lambda () (make-array 0 :element-type '(unsigned-byte 8))))
    (output
     (setf (callback-function handle :write)
           (lambda (octets) (write-sequence octets output) t))
     (lambda () (make-array 0 :element-type '(unsigned-byte 8))))
    (t
     (let ((chunks '()) (total 0))
       (setf (callback-function handle :write)
             (lambda (octets) (push octets chunks) (incf total (length octets)) t))
       (lambda ()
         ;; One allocation of the final size rather than repeated concatenation.
         (let ((body (make-array total :element-type '(unsigned-byte 8)))
               (offset 0))
           (dolist (chunk (nreverse chunks) body)
             (replace body chunk :start1 offset)
             (incf offset (length chunk)))))))))

;; The defaults live here rather than only in REQUEST, because REQUEST
;; forwards just the options the caller actually supplied -- so a default
;; declared only there would never reach this function.
(defun configure-request (handle url &key (method :get) headers content multipart
                                          timeout connect-timeout
                                          (follow-redirects t) max-redirects
                                          user-agent accept-encoding
                                          basic-auth bearer-auth
                                          cookie-jar cookies
                                          proxy verify-ssl ca-file verbose
                                          http-version range referer
                                          output on-data on-header)
  "Apply every request option to HANDLE.  Returns the body-collecting closure."
  (setopt handle :url url)
  (apply-method handle method)
  (let ((implied-type (apply-content handle content multipart method))
        (header-lines (normalise-headers headers)))
    ;; A Content-Type implied by the body is only added when the caller did not
    ;; set one; theirs wins.
    (when (and implied-type
               (notany (lambda (line) (eql 0 (search "content-type:" line
                                                     :test #'char-equal)))
                       header-lines))
      (push (format nil "Content-Type: ~A" implied-type) header-lines))
    (when header-lines
      (setopt handle :httpheader header-lines)))
  (setopt handle :timeout (or timeout *default-timeout*))
  ;; CURLOPT_CONNECTTIMEOUT, with no underscore -- so the keyword is
  ;; :CONNECTTIMEOUT even though the argument here reads better hyphenated.
  (setopt handle :connecttimeout (or connect-timeout *default-connect-timeout*))
  (setopt handle :useragent (or user-agent *default-user-agent*))
  ;; An empty string means "every encoding this libcurl was built with", and
  ;; libcurl transparently decompresses the result.
  (setopt handle :accept-encoding (or accept-encoding ""))
  (when follow-redirects
    (setopt handle :followlocation t)
    (setopt handle :maxredirs (or max-redirects 30)))
  (when referer (setopt handle :referer referer))
  (when range (setopt handle :range range))
  (when verbose (setopt handle :verbose t))
  (when proxy (setopt handle :proxy proxy))
  (when ca-file (setopt handle :cainfo (uiop:native-namestring ca-file)))
  (when http-version
    (setopt handle :http-version (ecase http-version
                                   (:http/1.0 1) (:http/1.1 2)
                                   (:http/2 3) (:http/3 30))))
  ;; Only ever weakened deliberately, and worth having to type.
  (unless (or (null verify-ssl) (eq verify-ssl :default))
    (setopt handle :ssl-verifypeer (if (eq verify-ssl :none) nil t))
    (setopt handle :ssl-verifyhost (if (eq verify-ssl :none) 0 2)))
  (when (eq verify-ssl :none)
    (setopt handle :ssl-verifypeer nil)
    (setopt handle :ssl-verifyhost 0))
  (when basic-auth
    (setopt handle :httpauth 1)         ; CURLAUTH_BASIC
    (setopt handle :userpwd (format nil "~A:~A" (car basic-auth)
                                    (if (consp (cdr basic-auth))
                                        (second basic-auth)
                                        (cdr basic-auth)))))
  (when bearer-auth
    (setopt handle :httpauth 64)        ; CURLAUTH_BEARER
    (setopt handle :xoauth2-bearer bearer-auth))
  ;; An empty COOKIEFILE turns the cookie engine on without reading anything,
  ;; which is what makes cookies survive a redirect chain within one request.
  (setopt handle :cookiefile (if cookie-jar
                                 (uiop:native-namestring cookie-jar)
                                 ""))
  (when cookie-jar
    (setopt handle :cookiejar (uiop:native-namestring cookie-jar)))
  (when cookies
    (setopt handle :cookie (if (stringp cookies)
                               cookies
                               (format nil "~{~A~^; ~}"
                                       (mapcar (lambda (pair)
                                                 (format nil "~A=~A" (car pair)
                                                         (cdr pair)))
                                               cookies)))))
  (when on-header
    (setf (callback-function handle :header)
          (lambda (octets)
            (funcall on-header (string-trim '(#\Return #\Newline)
                                            (octets-to-string octets
                                                              :encoding :latin-1)))
            t)))
  (make-body-sink handle output on-data))

(defun request (url &rest options
                &key (method :get) headers content multipart
                     timeout connect-timeout
                     (follow-redirects t) max-redirects
                     user-agent accept-encoding basic-auth bearer-auth
                     cookie-jar cookies proxy verify-ssl ca-file verbose
                     http-version range referer
                     output on-data on-header on-retry
                     force-binary force-string
                     retry session)
  "Perform an HTTP request and return a RESPONSE.

Returns (values response status headers), so the common cases stay short:

  (request \"https://example.com/\")
  (request \"https://example.com/\" :method :post :content '((\"a\" . \"1\")))

A non-2xx status is *not* an error -- it is a response, and the caller decides.
Only transport failures signal.

  :CONTENT      a string or octets sent as-is, or an alist form-encoded
  :MULTIPART    a list of part plists, sent through curl_mime
  :HEADERS      an alist, a plist, or a list of \"Name: value\" strings
  :OUTPUT       a stream to write the body to as it arrives
  :ON-DATA      a function called with each chunk, instead of accumulating
  :ON-HEADER    a function called with each response header line
  :RETRY        an attempt count, a plist, or a RETRY-POLICY
  :SESSION      a SESSION whose connections and cookies to reuse
  :VERIFY-SSL   :NONE disables certificate checking; do not

Streaming is via :OUTPUT or :ON-DATA.  There is no lazy body stream, because
the easy interface has already run the transfer to completion by the time
REQUEST returns; pretending otherwise would be a stream that is really a
buffer."
  (declare (ignorable headers content multipart timeout connect-timeout
                      follow-redirects max-redirects user-agent accept-encoding
                      basic-auth bearer-auth cookie-jar cookies proxy verify-ssl
                      ca-file verbose http-version range referer output on-data
                      on-header))
  (let ((policy (make-retry retry))
        (configure (loop for (key value) on options by #'cddr
                         unless (member key '(:retry :session :force-binary
                                              :force-string :on-retry))
                           append (list key value))))
    (with-retries (policy method :on-retry on-retry)
      (%request-once url configure
                     :method method
                     :session session
                     :force-binary force-binary
                     :force-string force-string))))

(defun %request-once (url configure &key method session force-binary force-string)
  "One attempt, with no retry logic.  Returns a RESPONSE."
  (flet ((run (handle)
           (let ((body (apply #'configure-request handle url configure)))
             (perform handle)
             (make-response-from-handle handle (funcall body)
                                        :request-method method
                                        :force-binary force-binary
                                        :force-string force-string))))
    (if session
        (with-session-handle (handle session) (run handle))
        (with-easy (handle) (run handle)))))

;;; The verbs -----------------------------------------------------------------

(macrolet ((define-verb (name method documentation)
             `(defun ,name (url &rest options)
                ,documentation
                (apply #'request url :method ,method options))))
  (define-verb http-get :get
    "GET URL.  See REQUEST for the options.")
  (define-verb http-post :post
    "POST to URL.  :CONTENT may be a string, octets, or an alist to form-encode.")
  (define-verb http-put :put
    "PUT to URL.")
  (define-verb http-patch :patch
    "PATCH URL.")
  (define-verb http-delete :delete
    "DELETE URL.")
  (define-verb http-head :head
    "HEAD URL.  libcurl is told not to expect a body, so this does not hang.")
  (define-verb http-options :options
    "OPTIONS on URL."))

(defun download (url destination &rest options)
  "GET URL straight to DESTINATION, a pathname, without buffering it in memory.

Returns the RESPONSE, whose body is empty because the bytes went to the file."
  (with-open-file (stream destination :direction :output
                                      :element-type '(unsigned-byte 8)
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
    (apply #'request url :output stream options)))

;;; Many at once --------------------------------------------------------------

(defun request-many (requests &key session (max-connections 8)
                                   on-complete retry on-retry
                                   (poll-timeout-ms 1000))
  "Run REQUESTS concurrently on one thread and return their outcomes in order.

Each element of REQUESTS is either a URL or a list of (URL . OPTIONS) taking
the same options as REQUEST.  Results come back positionally, so the Nth
outcome corresponds to the Nth request; a request that failed at the transport
level yields the CONDITION in its place rather than aborting the batch, because
with several transfers in flight one failure is a fact about that transfer and
not about the call.

ON-COMPLETE, if given, is called with (index outcome) at the moment that
request reaches its final outcome, not when the batch does -- so it can drive a
progress display.  The return value cannot arrive until the slowest request is
done, which in a batch of mixed durations is long after the quick ones were.

:RETRY takes the same specifications as REQUEST -- an attempt count, a plist,
or a RETRY-POLICY -- and applies to every request in the batch; an individual
request can override it with its own :RETRY.  ON-RETRY is called with
(index attempt delay reason) before each wait.

Retrying here is scheduled rather than sequential.  A failed request waits out
its backoff while the rest of the batch keeps transferring, and is re-added
when its delay expires; nothing blocks on anything else's recovery.  That is
the whole difficulty of retrying inside a batch, and the reason this is a loop
of its own rather than a call to RUN-TRANSFERS.

One caveat worth stating plainly: a retried request delivers its body from the
beginning.  With the default buffering that is invisible, because the buffer is
replaced for each attempt.  With :OUTPUT or :ON-DATA it is not -- the consumer
will have already seen whatever the failed attempt managed to deliver, and will
then see the whole of the successful one.  Retrying a streamed request is only
safe if the consumer can tolerate that."
  (let ((count (length requests)))
    (if (zerop count)
        '()
        (with-multi (multi :max-total-connections max-connections)
          (let ((results (make-array count :initial-element nil))
                (bodies (make-array count :initial-element nil))
                (handles (make-array count :initial-element nil))
                (methods (make-array count :initial-element :get))
                (urls (make-array count :initial-element nil))
                ;; Indexed rather than looked up with NTH: outcomes arrive in
                ;; completion order, so a list walk would be quadratic.
                (option-lists (make-array count :initial-element nil))
                (policies (make-array count :initial-element nil))
                (attempts (make-array count :initial-element 0))
                ;; (due-time . index) for requests waiting out a backoff.
                (scheduled '())
                ;; Requests with no final outcome yet.  Every one of them is
                ;; either running in the multi or sitting in SCHEDULED, which
                ;; is what makes the loop below terminate.
                (outstanding count))
            (labels
                ((now () (/ (get-internal-real-time)
                            internal-time-units-per-second 1.0))
                 (transfer-options (options)
                   (loop for (key value) on options by #'cddr
                         unless (member key '(:retry :session :force-binary
                                              :force-string :on-retry))
                           append (list key value)))
                 (start (index)
                   (let ((handle (aref handles index)))
                     (when (plusp (aref attempts index))
                       ;; A retry: this handle has already run once.  Reset
                       ;; rather than replace it, so its connection cache
                       ;; survives -- but reset clears every option, including
                       ;; the share, so that has to go back on.
                       (reset-handle handle)
                       (when session
                         (attach-share handle (session-share session))))
                     ;; A fresh body sink per attempt.  Reusing the old one
                     ;; would prepend whatever the failed attempt managed to
                     ;; deliver to the successful response.
                     (setf (aref bodies index)
                           (apply #'configure-request handle (aref urls index)
                                  (transfer-options (aref option-lists index)))
                           (getf (handle-plist handle) :request-index) index)
                     (incf (aref attempts index))
                     (add-transfer multi handle)))
                 (finish (index outcome)
                   (setf (aref results index) outcome)
                   (decf outstanding)
                   (when on-complete (funcall on-complete index outcome)))
                 (schedule (index delay reason)
                   (when on-retry
                     (funcall on-retry index (aref attempts index) delay reason))
                   (push (cons (+ (now) delay) index) scheduled))
                 (settle (index outcome retryable-p)
                   (let ((policy (aref policies index)))
                     (if (and (< (aref attempts index) (retry-max-attempts policy))
                              (funcall retryable-p policy outcome)
                              (retryable-method-p policy (aref methods index)))
                         (schedule index
                                   (retry-delay policy (aref attempts index)
                                                :retry-after
                                                (when (typep outcome 'response)
                                                  (response-retry-after outcome)))
                                   outcome)
                         (finish index outcome))))
                 (process (result)
                   (let* ((handle (result-handle result))
                          (index (getf (handle-plist handle) :request-index))
                          (options (aref option-lists index)))
                     (if (result-successful-p result)
                         (settle index
                                 (make-response-from-handle
                                  handle (funcall (aref bodies index))
                                  :request-method (aref methods index)
                                  :force-binary (getf options :force-binary)
                                  :force-string (getf options :force-string))
                                 #'retryable-response-p)
                         (settle index
                                 ;; Turn the result into the condition it would
                                 ;; have been, without signalling.
                                 (handler-case (signal-failed-transfers
                                                (list result))
                                   (curl-error (condition) condition))
                                 #'retryable-condition-p))))
                 (start-due ()
                   (let ((moment (now)))
                     (setf scheduled
                           (remove-if (lambda (entry)
                                        (when (<= (car entry) moment)
                                          (start (cdr entry))
                                          t))
                                      scheduled))))
                 (wait-time ()
                   ;; Never sleep past the next retry.  With nothing running,
                   ;; curl_multi_poll sleeps out the timeout, which is exactly
                   ;; the wait a pending backoff wants.
                   (if scheduled
                       (max 0 (min poll-timeout-ms
                                   (ceiling (* 1000 (- (reduce #'min scheduled
                                                               :key #'car)
                                                       (now))))))
                       poll-timeout-ms)))
              (unwind-protect
                   (progn
                     (loop for index below count
                           for specification in requests
                           do (destructuring-bind (url &rest options)
                                  (alexandria:ensure-list specification)
                                (setf (aref handles index)
                                      (if session
                                          (acquire-handle session)
                                          (make-easy-handle))
                                      (aref urls index) url
                                      (aref option-lists index) options
                                      (aref methods index) (or (getf options :method)
                                                               :get)
                                      ;; A request may override the batch
                                      ;; policy, including with an explicit NIL
                                      ;; to opt out of retrying -- which GETF
                                      ;; could not tell from not asking.
                                      (aref policies index)
                                      (make-retry (plist-value options :retry retry)))
                                (start index)))
                     (loop while (plusp outstanding)
                           do (multi-perform multi)
                              (mapc #'process (read-multi-messages multi))
                              (start-due)
                              (when (plusp outstanding)
                                (multi-poll multi :timeout-ms (wait-time))))
                     (coerce results 'list))
                (loop for index below count
                      for handle = (aref handles index)
                      when handle
                        do (ignore-errors
                            (if session
                                (release-handle session handle)
                                (close-handle handle)))))))))))
