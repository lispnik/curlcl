;;;; test/easy-tests.lisp — the easy handle, against the in-process server.
;;;;
;;;; These are the first tests that move real bytes.  They lean on the local
;;;; server for anything where the *shape* of the response matters (chunked,
;;;; truncated, redirecting, slow), because those are exactly the cases a
;;;; public endpoint cannot be asked for on demand.

(in-package #:libcurl/test)

(in-suite easy)

(defun collect-body (handle)
  "Install a write callback and return a closure yielding the body so far."
  (let ((chunks '()))
    (setf (callback-function handle :write)
          (lambda (octets) (push octets chunks) t))
    (lambda ()
      (apply #'concatenate '(vector (unsigned-byte 8)) (reverse chunks)))))

(defun body-string (octets)
  (map 'string #'code-char octets))

(defun fetch (path &rest options)
  "GET PATH from the test server.  Returns (values body-string handle-status)."
  (with-easy (handle)
    (let ((body (collect-body handle)))
      (apply #'setopts handle :url (test-url path) options)
      (perform handle)
      (values (body-string (funcall body))
              (getinfo handle :response-code)))))

(test a-simple-get-returns-its-body
  (multiple-value-bind (body status) (fetch "/ok")
    (is (string= "ok" body))
    (is (= 200 status))))

(test the-write-callback-sees-every-byte-of-a-large-body
  ;; A megabyte arrives in many chunks, so this also checks that the callback
  ;; is re-entered correctly and that nothing is dropped between calls.
  (with-easy (handle)
    (let ((total 0) (calls 0))
      (setf (callback-function handle :write)
            (lambda (octets) (incf total (length octets)) (incf calls) t))
      (setopt handle :url (test-url "/large?bytes=1000000"))
      (perform handle)
      (is (= 1000000 total))
      (is (< 1 calls) "a megabyte arrived in a single callback call")
      (is (= 1000000 (getinfo handle :size-download-t))))))

(test chunked-transfer-encoding-is-decoded
  ;; libcurl removes the chunk framing, so the callback must see only payload.
  (multiple-value-bind (body status) (fetch "/chunked?n=3")
    (is (= 200 status))
    (is (string= "chunk0 chunk1 chunk2 " body))))

(test headers-reach-the-header-callback-and-not-the-body
  (with-easy (handle)
    (let ((headers '()) (body-bytes 0))
      (setf (callback-function handle :header)
            (lambda (octets) (push (string-trim '(#\Return #\Newline)
                                                (body-string octets))
                                   headers)
              t)
            (callback-function handle :write)
            (lambda (octets) (incf body-bytes (length octets)) t))
      (setopt handle :url (test-url "/ok"))
      (perform handle)
      (setf headers (reverse headers))
      (is (search "200" (first headers)) "status line missing from headers")
      (is (find-if (lambda (h) (eql 0 (search "Content-Type:" h))) headers))
      ;; The body callback must not have received the headers as well.
      (is (= 2 body-bytes)))))

(test duplicate-header-names-are-all-delivered
  ;; A header representation that collapsed duplicates would lose Set-Cookie,
  ;; which is the one header that most needs to repeat.
  (with-easy (handle)
    (let ((headers '()))
      (setf (callback-function handle :header)
            (lambda (octets) (push (string-trim '(#\Return #\Newline)
                                                (body-string octets))
                                   headers)
              t)
            (callback-function handle :write) (lambda (o) (declare (ignore o)) t))
      (setopt handle :url (test-url "/headers/multi"))
      (perform handle)
      (is (= 2 (count-if (lambda (h) (eql 0 (search "Set-Cookie:" h))) headers)))
      (is (= 2 (count-if (lambda (h) (eql 0 (search "X-Repeated:" h))) headers))))))

(test a-post-sends-its-body
  ;; :POSTFIELDS is routed to CURLOPT_COPYPOSTFIELDS so libcurl owns the copy;
  ;; if it were not, this body would be freed before the transfer read it.
  (with-easy (handle)
    (let ((body (collect-body handle)))
      (setopts handle :url (test-url "/echo") :postfields "hello=world")
      (perform handle)
      (let ((text (body-string (funcall body))))
        (is (search "method=POST" text))
        (is (search "body=hello=world" text))))))

(test a-post-body-survives-the-handle-outliving-its-lisp-string
  ;; The pointed end of the same issue: the Lisp string is long gone and a GC
  ;; has run by the time the transfer happens.
  (with-easy (handle)
    (let ((body (collect-body handle)))
      (setopt handle :url (test-url "/echo"))
      (setopt handle :postfields (concatenate 'string "payload=" "0123456789"))
      (sb-ext:gc :full t)
      (perform handle)
      (is (search "body=payload=0123456789" (body-string (funcall body)))))))

(test custom-headers-are-sent
  (with-easy (handle)
    (let ((body (collect-body handle)))
      (setopts handle :url (test-url "/echo")
                      :httpheader '("X-Test: yes" "X-Other: 42"))
      (perform handle)
      (let ((text (body-string (funcall body))))
        (is (search "x-test: yes" text))
        (is (search "x-other: 42" text))))))

(test an-slist-outlives-the-lisp-list-that-built-it
  ;; CURLOPT_HTTPHEADER borrows the slist rather than copying it, so the
  ;; binding has to own it until the handle is closed.
  (with-easy (handle)
    (let ((body (collect-body handle)))
      (setopt handle :url (test-url "/echo"))
      (setopt handle :httpheader (list (format nil "X-Generated: ~D" 99)))
      (sb-ext:gc :full t)
      (perform handle)
      (is (search "x-generated: 99" (body-string (funcall body)))))))

(test redirects-are-followed-only-when-asked
  (with-easy (handle)
    (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t))
    (setopt handle :url (test-url "/redirect/2"))
    (perform handle)
    ;; Without :followlocation libcurl stops at the first 302.
    (is (= 302 (getinfo handle :response-code))))
  (multiple-value-bind (body status) (fetch "/redirect/2" :followlocation t)
    (is (= 200 status))
    (is (string= "ok" body))))

(test a-redirect-loop-hits-the-limit-and-signals
  (with-easy (handle)
    (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t))
    (setopts handle :url (test-url "/redirect-loop") :followlocation t :maxredirs 3)
    (handler-case (progn (perform handle) (fail "expected a redirect limit error"))
      (easy-error (c)
        (is (eq :too-many-redirects (curl-error-code-name c)))))))

(test a-truncated-body-is-reported-not-silently-accepted
  ;; The server promises 100 bytes, sends eight, and hangs up.  Quietly
  ;; returning the short body would be the dangerous outcome.
  (with-easy (handle)
    (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t))
    (setopt handle :url (test-url "/close-early"))
    (handler-case (progn (perform handle) (fail "expected a partial-file error"))
      (easy-error (c)
        (is (eq :partial-file (curl-error-code-name c)))))))

(test an-http-error-status-is-not-an-error-unless-asked
  ;; libcurl treats 4xx as a successful transfer by default; :failonerror is
  ;; what turns it into a failure.  Both behaviours are worth pinning.
  (multiple-value-bind (body status) (fetch "/status/404")
    (is (= 404 status))
    (is (string= "status 404" body)))
  (with-easy (handle)
    (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t))
    (setopts handle :url (test-url "/status/404") :failonerror t)
    (handler-case (progn (perform handle) (fail "expected :failonerror to signal"))
      (easy-error (c)
        (is (eq :http-returned-error (curl-error-code-name c)))))))

;;; Callback safety -----------------------------------------------------------

(test a-condition-in-a-write-callback-surfaces-as-itself
  ;; The core of the safety design: the condition is caught at the C boundary,
  ;; the callback returns libcurl's abort sentinel, and PERFORM re-signals the
  ;; original from Lisp.  The caller must see their own error, not
  ;; CURLE_WRITE_ERROR.
  (with-easy (handle)
    (setf (callback-function handle :write)
          (lambda (octets) (declare (ignore octets)) (error 'file-error :pathname "/x")))
    (setopt handle :url (test-url "/ok"))
    (handler-case (progn (perform handle) (fail "expected the callback error"))
      (callback-error (c)
        (is (eq :write (callback-error-kind c)))
        (is (typep (callback-error-cause c) 'file-error))
        ;; libcurl's own view is kept, but as context rather than as the story.
        (is (eq :write-error (curl-error-code-name c)))))))

(test a-callback-returning-nil-aborts-the-transfer
  (with-easy (handle)
    (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) nil))
    (setopt handle :url (test-url "/ok"))
    (handler-case (progn (perform handle) (fail "expected an abort"))
      (easy-error (c)
        (is (eq :write-error (curl-error-code-name c)))
        ;; No Lisp condition was signalled, so this is a plain transfer error.
        (is (not (typep c 'callback-error)))))))

(test only-the-first-callback-condition-is-kept
  ;; libcurl may call the write callback again before it notices the abort;
  ;; the first condition is the interesting one.
  (with-easy (handle)
    (let ((calls 0))
      (setf (callback-function handle :write)
            (lambda (octets)
              (declare (ignore octets))
              (error "failure number ~D" (incf calls))))
      (setopt handle :url (test-url "/large?bytes=200000"))
      (handler-case (progn (perform handle) (fail "expected a callback error"))
        (callback-error (c)
          (is (search "failure number 1" (princ-to-string (callback-error-cause c)))))))))

(test a-progress-callback-can-cancel-a-transfer
  (with-easy (handle)
    (let ((seen 0))
      (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t)
            (callback-function handle :progress)
            (lambda (dltotal dlnow ultotal ulnow)
              (declare (ignore dltotal ultotal ulnow))
              (incf seen)
              ;; Cancel once anything has arrived.
              (if (plusp dlnow) :abort t)))
      (setopts handle :url (test-url "/drip?n=10&ms=20") :noprogress nil)
      (handler-case (progn (perform handle) (fail "expected the progress abort"))
        (easy-error (c)
          (is (eq :aborted-by-callback (curl-error-code-name c)))))
      (is (plusp seen) "the progress callback never ran"))))

(test a-read-callback-supplies-an-upload-body
  (with-easy (handle)
    (let ((body (collect-body handle))
          (remaining (babel-encode "uploaded-by-callback")))
      (setf (callback-function handle :read)
            (lambda (capacity)
              (if (zerop (length remaining))
                  :eof
                  (let ((piece (subseq remaining 0 (min capacity (length remaining)))))
                    (setf remaining (subseq remaining (length piece)))
                    piece))))
      (setopts handle :url (test-url "/echo")
                      :post t
                      :postfieldsize-large 20)
      (perform handle)
      (is (search "body=uploaded-by-callback" (body-string (funcall body)))))))

(test a-debug-callback-sees-the-protocol-trace
  (with-easy (handle)
    (let ((kinds '()))
      (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t)
            (callback-function handle :debug)
            (lambda (type octets) (declare (ignore octets)) (pushnew type kinds)))
      (setopts handle :url (test-url "/ok") :verbose t)
      (perform handle)
      (is (member :header-out kinds) "no outgoing headers were traced")
      (is (member :header-in kinds) "no incoming headers were traced"))))

;;; Lifetime ------------------------------------------------------------------

(test closing-a-handle-releases-its-registry-key
  (let ((before (libcurl::live-callback-count)))
    (let ((handle (make-easy-handle)))
      (is (= (1+ before) (libcurl::live-callback-count)))
      (close-handle handle)
      (is (= before (libcurl::live-callback-count))))))

(test closing-twice-is-harmless
  (let ((handle (make-easy-handle)))
    (close-handle handle)
    (finishes (close-handle handle))
    (is (handle-closed-p handle))))

(test using-a-closed-handle-signals-rather-than-crashing
  ;; Reaching into a freed CURL* would be undefined behaviour, so the check is
  ;; on the Lisp side of the boundary.
  (let ((handle (make-easy-handle)))
    (close-handle handle)
    (signals libcurl::handle-closed (setopt handle :url "http://example.com/"))
    (signals libcurl::handle-closed (perform handle))
    (signals libcurl::handle-closed (getinfo handle :response-code))))

(test many-handles-do-not-leak-keys-or-resources
  ;; The accounting that stands in for a finalizer: create and destroy enough
  ;; handles that a leak of one key per handle would be obvious.
  (let ((before (libcurl::live-callback-count)))
    (dotimes (i 500)
      (let ((handle (make-easy-handle)))
        (setopts handle :url (test-url "/ok") :httpheader '("X-A: 1" "X-B: 2"))
        (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t))
        (close-handle handle)))
    (is (= before (libcurl::live-callback-count)))))

(test released-keys-are-reused-rather-than-growing-without-bound
  (let ((first-key nil))
    (let ((handle (make-easy-handle)))
      (setf first-key (libcurl::cb-key (libcurl::handle-callbacks handle)))
      (close-handle handle))
    (let ((handle (make-easy-handle)))
      (unwind-protect
           (is (= first-key (libcurl::cb-key (libcurl::handle-callbacks handle))))
        (close-handle handle)))))

(test resetting-a-handle-keeps-it-usable
  ;; curl_easy_reset clears the error buffer, CURLOPT_PRIVATE and every
  ;; callback, all of which the binding needs; RESET-HANDLE re-installs them.
  (with-easy (handle)
    (let ((body (collect-body handle)))
      (setopt handle :url (test-url "/ok"))
      (perform handle)
      (is (string= "ok" (body-string (funcall body))))
      (reset-handle handle)
      ;; The private key must survive, or the handle can no longer be traced.
      (is (eq handle (libcurl::handle-from-pointer (handle-pointer handle))))
      (let ((second (collect-body handle)))
        (setopt handle :url (test-url "/status/418"))
        (perform handle)
        (is (= 418 (getinfo handle :response-code)))
        (is (string= "status 418" (body-string (funcall second))))))))

(test a-duplicated-handle-writes-to-its-own-callback
  ;; curl_easy_duphandle copies CURLOPT_WRITEDATA verbatim, so without
  ;; re-pointing it the duplicate would deliver into the original's closure.
  (with-easy (handle)
    (let ((original '()))
      (setf (callback-function handle :write)
            (lambda (octets) (push octets original) t))
      (setopt handle :url (test-url "/ok"))
      (let ((copy (duplicate-handle handle)))
        (unwind-protect
             (let ((copied '()))
               (setf (callback-function copy :write)
                     (lambda (octets) (push octets copied) t))
               (perform copy)
               (is (null original) "the duplicate wrote into the original's closure")
               (is (= 1 (length copied)))
               (is (string= "ok" (body-string (first copied)))))
          (close-handle copy))))))

(test a-duplicated-handle-has-its-own-error-buffer
  ;; The same trap for CURLOPT_ERRORBUFFER: the copy would otherwise overwrite
  ;; the original's message.
  (with-easy (handle)
    (setopt handle :url (test-url "/ok"))
    (let ((copy (duplicate-handle handle)))
      (unwind-protect
           (is (not (cffi:pointer-eq (handle-error-buffer handle)
                                     (handle-error-buffer copy))))
        (close-handle copy)))))

;;; Options and info ----------------------------------------------------------

(test setopt-accepts-lisp-values
  (with-easy (handle)
    (finishes (setopt handle :followlocation t))
    (finishes (setopt handle :followlocation nil))
    (finishes (setopt handle :maxredirs 5))
    (finishes (setopt handle :url "http://example.com/"))
    (finishes (setopt handle :httpheader '("A: 1")))
    (finishes (setopt handle :maxfilesize-large #x180000000))))

(test setopt-signals-for-an-option-that-does-not-exist
  (with-easy (handle)
    (signals unsupported-option (setopt handle :not-a-real-option 1))))

(test getinfo-reads-timing-and-sizes-after-a-transfer
  (with-easy (handle)
    (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t))
    (setopt handle :url (test-url "/large?bytes=50000"))
    (perform handle)
    (is (= 50000 (getinfo handle :size-download-t)))
    (is (plusp (getinfo handle :total-time-t)))
    (is (string= (test-url "/large?bytes=50000") (getinfo handle :effective-url)))
    (is (= 200 (getinfo handle :response-code)))))

(test url-escaping-round-trips
  (with-easy (handle)
    (let ((escaped (url-escape handle "a b&c=d/e")))
      (is (string= "a%20b%26c%3Dd%2Fe" escaped))
      (is (string= "a b&c=d/e" (body-string (url-unescape handle escaped)))))))
