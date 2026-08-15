;;;; test/client-tests.lisp — the HTTP client.

(in-package #:libcurl/test)

(in-suite client)

;;; The basics ----------------------------------------------------------------

(test a-get-returns-a-response
  (let ((response (http-get (test-url "/ok"))))
    (is (= 200 (response-status response)))
    (is (string= "ok" (response-body response)))
    (is (string= (test-url "/ok") (response-url response)))
    (is (successful-response-p response))
    (is (eq :get (response-request-method response)))))

(test a-non-2xx-status-is-a-response-not-an-error
  ;; The single most consequential API decision in this layer.  A 404 is an
  ;; answer, and making the caller handle a condition for it turns ordinary
  ;; control flow into exception handling.
  (let ((response (http-get (test-url "/status/404"))))
    (is (= 404 (response-status response)))
    (is (not (successful-response-p response)))
    (is (string= "status 404" (response-body response))))
  (let ((response (http-get (test-url "/status/500"))))
    (is (= 500 (response-status response)))))

(test a-transport-failure-does-signal
  ;; The other side of that decision: nothing answered, so there is no response
  ;; to return and a condition is the only honest thing.
  (signals easy-error (http-get "http://127.0.0.1:1/nothing-here" :timeout 5))
  (handler-case (http-get (test-url "/close-early"))
    (easy-error (c) (is (eq :partial-file (curl-error-code-name c))))))

(test timings-and-metadata-are-captured
  (let ((response (http-get (test-url "/ok"))))
    (is (plusp (getf (response-timings response) :total)))
    (is (member (response-version response) '(:http/1.1 :http/1.0)))
    (is (= 0 (response-redirect-count response)))))

;;; Headers -------------------------------------------------------------------

(test response-headers-are-captured-with-their-duplicates
  (let ((response (http-get (test-url "/headers/multi"))))
    (is (equal '("a=1" "b=2") (response-header-values response "set-cookie")))
    (is (equal '("first" "second") (response-header-values response "x-repeated")))
    ;; Indexed access reaches each one.
    (is (string= "first" (header-value (response-header response "x-repeated"))))
    (is (string= "second" (header-value (response-header response "x-repeated"
                                                          :index 1))))))

(test response-header-lookup-is-case-insensitive
  (let ((response (http-get (test-url "/ok"))))
    (is (string= "text/plain" (response-header-value response "Content-Type")))
    (is (string= "text/plain" (response-header-value response "CONTENT-TYPE")))
    (is (string= "text/plain" (response-content-type response)))
    (is (null (response-header-value response "x-absent")))))

(test request-headers-are-accepted-in-three-shapes
  ;; An alist, a plist and a list of strings all appear in real code, and none
  ;; is obviously right, so all three work.
  (dolist (headers (list '(("X-Shape" . "alist"))
                         '(:x-shape "plist")
                         '("X-Shape: strings")))
    (let ((response (http-post (test-url "/echo") :headers headers)))
      (is (search "x-shape:" (string-downcase (response-body response)))
          "headers given as ~S did not arrive" headers))))

(test keyword-header-names-are-capitalised
  (is (string= "Content-Type" (libcurl::header-name-string :content-type)))
  (is (string= "X-Api-Key" (libcurl::header-name-string :x-api-key)))
  (is (string= "already-given" (libcurl::header-name-string "already-given"))))

(test the-header-callback-sees-each-line
  (let ((lines '()))
    (http-get (test-url "/ok") :on-header (lambda (line) (push line lines)))
    (setf lines (reverse lines))
    (is (search "200" (first lines)))
    (is (find-if (lambda (l) (eql 0 (search "Content-Type:" l))) lines))))

;;; Bodies --------------------------------------------------------------------

(test a-post-sends-a-string-body-verbatim
  (let ((response (http-post (test-url "/echo") :content "raw body text")))
    (is (search "method=POST" (response-body response)))
    (is (search "body=raw body text" (response-body response)))))

(test a-post-form-encodes-an-alist-and-sets-the-content-type
  (let ((response (http-post (test-url "/echo")
                             :content '(("name" . "a value") ("other" . "x&y")))))
    (is (search "body=name=a%20value&other=x%26y" (response-body response)))
    (is (search "content-type: application/x-www-form-urlencoded"
                (string-downcase (response-body response))))))

(test a-caller-supplied-content-type-wins-over-the-implied-one
  (let ((response (http-post (test-url "/echo")
                             :content '(("a" . "1"))
                             :headers '(("Content-Type" . "application/json")))))
    (is (search "content-type: application/json"
                (string-downcase (response-body response))))
    (is (not (search "x-www-form-urlencoded"
                     (string-downcase (response-body response)))))))

(test a-multipart-body-goes-through-mime
  (let ((response (http-post (test-url "/echo")
                             :multipart '((:name "field" :data "value")
                                          (:name "file" :data "contents"
                                           :filename "x.txt")))))
    (is (search "multipart/form-data" (response-body response)))
    (is (search "name=\"field\"" (response-body response)))
    (is (search "filename=\"x.txt\"" (response-body response)))))

(test every-verb-reaches-the-server
  (dolist (entry '((http-get . "GET") (http-post . "POST") (http-put . "PUT")
                   (http-patch . "PATCH") (http-delete . "DELETE")
                   (http-options . "OPTIONS")))
    (let ((response (funcall (car entry) (test-url "/echo"))))
      (is (search (format nil "method=~A" (cdr entry)) (response-body response))
          "~A did not send ~A" (car entry) (cdr entry)))))

(test head-returns-headers-without-waiting-for-a-body
  ;; CURLOPT_CUSTOMREQUEST "HEAD" would leave libcurl waiting for a body that
  ;; is never coming; CURLOPT_NOBODY is what makes this terminate.
  (let ((response (http-head (test-url "/ok"))))
    (is (= 200 (response-status response)))
    (is (zerop (length (response-body response))))
    (is (string= "text/plain" (response-content-type response)))))

;;; Decoding ------------------------------------------------------------------

(test a-textual-body-is-decoded-to-a-string
  (let ((response (http-get (test-url "/ok"))))
    (is (stringp (response-body response)))))

(test a-binary-body-stays-octets
  (let ((response (http-get (test-url "/large?bytes=1000"))))
    (is (typep (response-body response) '(array (unsigned-byte 8) (*))))
    (is (= 1000 (length (response-body response))))))

(test decoding-can-be-forced-either-way
  (let ((as-octets (http-get (test-url "/ok") :force-binary t))
        (as-string (http-get (test-url "/large?bytes=10") :force-string t)))
    (is (typep (response-body as-octets) '(array (unsigned-byte 8) (*))))
    (is (stringp (response-body as-string)))))

(test content-type-parsing-splits-media-type-from-charset
  (multiple-value-bind (media charset)
      (parse-content-type "text/html; charset=UTF-8")
    (is (string= "text/html" media))
    (is (string= "UTF-8" charset)))
  (multiple-value-bind (media charset) (parse-content-type "application/json")
    (is (string= "application/json" media))
    (is (null charset)))
  (multiple-value-bind (media charset)
      (parse-content-type "text/plain;charset=\"iso-8859-1\"")
    (is (string= "text/plain" media))
    (is (string= "iso-8859-1" charset))))

(test json-and-suffixed-types-count-as-textual
  (is (libcurl::textual-media-type-p "text/html"))
  (is (libcurl::textual-media-type-p "application/json"))
  (is (libcurl::textual-media-type-p "application/vnd.api+json"))
  (is (libcurl::textual-media-type-p "image/svg+xml"))
  (is (not (libcurl::textual-media-type-p "application/octet-stream")))
  (is (not (libcurl::textual-media-type-p "image/png"))))

(test an-unrecognised-charset-comes-back-as-octets
  ;; Guessing would be worse than handing back what arrived: a body silently
  ;; mis-decoded is harder to notice than one that is obviously not a string.
  (let ((octets (coerce-to-test-octets "hello")))
    (is (typep (decode-body octets "text/plain; charset=x-nonesuch")
               '(array (unsigned-byte 8) (*))))
    (is (stringp (decode-body octets "text/plain; charset=utf-8")))))

(defun coerce-to-test-octets (string)
  (libcurl::coerce-to-octets string))

(test a-body-that-lies-about-its-charset-still-comes-back
  ;; The transfer succeeded; a decoding failure must not turn that into an
  ;; error, so the octets are returned instead.
  (let ((invalid (make-array 3 :element-type '(unsigned-byte 8)
                               :initial-contents '(#xC3 #x28 #xFF))))
    (is (typep (decode-body invalid "text/plain; charset=utf-8")
               '(array (unsigned-byte 8) (*))))))

;;; Streaming -----------------------------------------------------------------

(test output-writes-the-body-to-a-stream-as-it-arrives
  ;; A real binary stream, so this also pins that the write callback hands over
  ;; octets rather than characters.
  (uiop:with-temporary-file (:pathname path)
    (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (let ((response (http-get (test-url "/large?bytes=5000") :output out)))
        (is (= 200 (response-status response)))
        ;; Nothing was buffered in the response.
        (is (zerop (length (response-body response))))))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (is (= 5000 (file-length in))))))

(test on-data-receives-every-chunk-and-nothing-is-buffered
  (let ((total 0) (calls 0))
    (let ((response (http-get (test-url "/large?bytes=200000")
                              :on-data (lambda (octets)
                                         (incf total (length octets))
                                         (incf calls)))))
      (is (= 200000 total))
      (is (plusp calls))
      (is (zerop (length (response-body response)))))))

(test download-writes-a-file
  (uiop:with-temporary-file (:pathname path)
    (let ((response (download (test-url "/large?bytes=3000") path)))
      (is (= 200 (response-status response))))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (is (= 3000 (file-length in))))))

;;; Redirects and cookies -----------------------------------------------------

(test redirects-are-followed-by-default
  (let ((response (http-get (test-url "/redirect/3"))))
    (is (= 200 (response-status response)))
    (is (string= "ok" (response-body response)))
    (is (= 4 (response-redirect-count response)))
    ;; The URL reported is the one actually fetched.
    (is (search "/ok" (response-url response)))))

(test redirect-following-can-be-turned-off
  (let ((response (http-get (test-url "/redirect/1") :follow-redirects nil)))
    (is (= 302 (response-status response)))
    (is (response-header-value response "location"))))

(test a-redirect-loop-is-stopped-by-the-limit
  (handler-case
      (progn (http-get (test-url "/redirect-loop") :max-redirects 3)
             (fail "expected a redirect limit error"))
    (easy-error (c) (is (eq :too-many-redirects (curl-error-code-name c))))))

(test cookies-can-be-sent-explicitly
  (let ((response (http-get (test-url "/cookie/echo")
                            :cookies '(("a" . "1") ("b" . "2")))))
    (is (search "a=1" (response-body response)))
    (is (search "b=2" (response-body response)))))

;;; Retry ---------------------------------------------------------------------

(test a-retryable-status-is-retried-until-it-succeeds
  (reset-flaky "retry-basic")
  (let ((response (http-get (test-url "/flaky?id=retry-basic&fail=2")
                            :retry '(:max-attempts 5 :initial-delay 0.01))))
    (is (= 200 (response-status response)))
    (is (search "succeeded on attempt 3" (response-body response)))))

(test retries-give-up-after-the-limit-and-return-the-last-response
  (reset-flaky "retry-exhaust")
  (let ((response (http-get (test-url "/flaky?id=retry-exhaust&fail=10")
                            :retry '(:max-attempts 3 :initial-delay 0.01))))
    ;; The last response is returned rather than signalled: it is still a
    ;; response, and the caller may want to see it.
    (is (= 503 (response-status response)))))

(test a-non-retryable-status-is-not-retried
  (reset-flaky "retry-404")
  (let ((response (http-get (test-url "/flaky?id=retry-404&fail=5&status=404")
                            :retry '(:max-attempts 4 :initial-delay 0.01))))
    (is (= 404 (response-status response)))
    ;; Exactly one attempt was made: a 404 will not become a 200 by asking again.
    (is (search "attempt 1 of 5" (response-body response)))))

(test post-is-not-retried-unless-asked
  ;; Only the caller knows whether repeating a POST duplicates an effect, so
  ;; the default is not to.
  (reset-flaky "retry-post")
  (let ((response (http-post (test-url "/flaky?id=retry-post&fail=3")
                             :retry '(:max-attempts 4 :initial-delay 0.01))))
    (is (= 503 (response-status response)))
    (is (search "attempt 1 of 3" (response-body response))))
  (reset-flaky "retry-post-allowed")
  (let ((response (http-post (test-url "/flaky?id=retry-post-allowed&fail=2")
                             :retry '(:max-attempts 5 :initial-delay 0.01
                                      :non-idempotent t))))
    (is (= 200 (response-status response)))))

(test a-transport-failure-is-retried
  (let ((attempts 0))
    (handler-case
        (http-get "http://127.0.0.1:1/refused"
                  :timeout 5
                  :retry '(:max-attempts 3 :initial-delay 0.01)
                  :on-retry (lambda (attempt delay reason)
                              (declare (ignore delay reason))
                              (setf attempts attempt)))
      (easy-error () nil))
    ;; Two retries after the first attempt.
    (is (= 2 attempts))))

(test backoff-grows-and-is-bounded
  (let ((policy (make-retry '(:initial-delay 1.0 :multiplier 2.0 :max-delay 10.0
                              :jitter 0.0))))
    (is (= 1.0 (libcurl::retry-delay policy 1)))
    (is (= 2.0 (libcurl::retry-delay policy 2)))
    (is (= 4.0 (libcurl::retry-delay policy 3)))
    ;; Capped rather than doubling forever.
    (is (= 10.0 (libcurl::retry-delay policy 20)))))

(test jitter-spreads-the-delay-without-going-negative
  ;; Without jitter a fleet that failed together retries together, which is how
  ;; a struggling server is kept down.
  (let ((policy (make-retry '(:initial-delay 1.0 :multiplier 1.0 :jitter 0.5))))
    (let ((delays (loop repeat 50 collect (libcurl::retry-delay policy 1))))
      (is (every (lambda (d) (<= 0 d 1.5)) delays))
      (is (< 1 (length (remove-duplicates delays :test #'=)))
          "jitter produced identical delays"))))

(test retry-after-is-honoured-over-the-computed-backoff
  (let ((policy (make-retry '(:initial-delay 30.0 :max-delay 60.0))))
    (is (= 2 (libcurl::retry-delay policy 1 :retry-after 2))))
  (is (= 5 (libcurl::parse-retry-after "5")))
  (is (= 0 (libcurl::parse-retry-after "0")))
  (is (null (libcurl::parse-retry-after "nonsense")))
  ;; The HTTP-date form is honoured too, through libcurl's own date parser.
  ;; A date already past yields 0 rather than a negative delay: the two clocks
  ;; need not agree, and a server saying "now" is the sensible reading.
  (is (= 0 (libcurl::parse-retry-after "Wed, 21 Oct 2015 07:28:00 GMT")))
  (let ((soon (libcurl::parse-retry-after
               (multiple-value-bind (second minute hour day month year)
                   (decode-universal-time (+ (get-universal-time) 120) 0)
                 (format nil "~A, ~2,'0D ~A ~D ~2,'0D:~2,'0D:~2,'0D GMT"
                         "Mon" day
                         (aref #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
                                 "Aug" "Sep" "Oct" "Nov" "Dec") (1- month))
                         year hour minute second)))))
    (is (and soon (<= 110 soon 130))
        "a date two minutes out gave ~S seconds" soon)))

(test retry-specifications-are-accepted-in-several-shapes
  (is (= 1 (libcurl::retry-max-attempts (make-retry nil))))
  (is (= 3 (libcurl::retry-max-attempts (make-retry 3))))
  (is (= 7 (libcurl::retry-max-attempts (make-retry '(:max-attempts 7)))))
  (let ((policy (make-retry-policy :max-attempts 2)))
    (is (eq policy (make-retry policy)))))

;;; Sessions ------------------------------------------------------------------

(test a-session-serves-several-requests
  (with-session (session)
    (dotimes (i 5)
      (let ((response (http-get (test-url "/ok") :session session)))
        (is (= 200 (response-status response)))
        (is (string= "ok" (response-body response)))))))

(test a-session-reuses-its-handles
  ;; The pool is the point: after the first request a handle is idle and the
  ;; next request takes it rather than building a new one.
  (with-session (session :max-idle 4)
    (is (null (session-pool-for-test session)))
    (http-get (test-url "/ok") :session session)
    (is (= 1 (length (session-pool-for-test session))))
    (let ((pooled (first (session-pool-for-test session))))
      (http-get (test-url "/ok") :session session)
      (is (= 1 (length (session-pool-for-test session))))
      (is (eq pooled (first (session-pool-for-test session)))
          "the second request did not reuse the pooled handle"))))

(defun session-pool-for-test (session)
  (libcurl::session-pool session))

(test a-session-carries-cookies-between-requests
  ;; What the shared cookie jar buys: a login on one request is visible to the
  ;; next without the caller threading anything through.
  (with-session (session)
    (http-get (test-url "/cookie/set") :session session)
    (let ((response (http-get (test-url "/cookie/echo") :session session)))
      (is (search "session=abc123" (response-body response))))))

(test cookies-do-not-leak-between-sessions
  (with-session (first-session)
    (http-get (test-url "/cookie/set") :session first-session)
    (with-session (second-session)
      (let ((response (http-get (test-url "/cookie/echo") :session second-session)))
        (is (search "no cookie" (response-body response)))))))

(test session-cookies-can-be-inspected-and-cleared
  (with-session (session)
    (http-get (test-url "/cookie/set") :session session)
    (let ((cookies (session-cookies session)))
      (is (listp cookies))
      (is (find-if (lambda (line) (search "session" line)) cookies)))
    (clear-session-cookies session)
    (let ((response (http-get (test-url "/cookie/echo") :session session)))
      (is (search "no cookie" (response-body response))))))

(test the-pool-is-bounded
  (with-session (session :max-idle 2)
    ;; Run several requests sequentially; at most MAX-IDLE handles are kept.
    (dotimes (i 6) (http-get (test-url "/ok") :session session))
    (is (<= (length (session-pool-for-test session)) 2))))

(test a-closed-session-refuses-further-requests
  (let ((session (make-session)))
    (http-get (test-url "/ok") :session session)
    (close-session session)
    (is (session-closed-p session))
    (signals curl-error (http-get (test-url "/ok") :session session))
    (finishes (close-session session))))

(test a-session-works-from-several-threads
  (with-session (session)
    (let* ((count 6)
           (results (make-array count :initial-element nil))
           (threads (loop for i below count
                          collect (let ((index i))
                                    (bt:make-thread
                                     (lambda ()
                                       (setf (aref results index)
                                             (handler-case
                                                 (response-body
                                                  (http-get (test-url "/ok")
                                                            :session session))
                                               (error (c) c))))
                                     :name "session thread")))))
      (mapc #'bt:join-thread threads)
      (dotimes (i count)
        (is (equal "ok" (aref results i))
            "thread ~D got ~S" i (aref results i))))))

;;; Many at once --------------------------------------------------------------

(test request-many-returns-responses-in-order
  (let ((responses (request-many (list (test-url "/status/201")
                                       (test-url "/status/202")
                                       (test-url "/status/203")))))
    (is (= 3 (length responses)))
    (is (equal '(201 202 203) (mapcar #'response-status responses)))))

(test request-many-takes-options-per-request
  (let ((responses (request-many
                    (list (list (test-url "/echo") :method :post :content "one")
                          (list (test-url "/echo") :method :put :content "two")))))
    (is (search "method=POST" (response-body (first responses))))
    (is (search "body=one" (response-body (first responses))))
    (is (search "method=PUT" (response-body (second responses))))
    (is (search "body=two" (response-body (second responses))))))

(test a-failure-in-a-batch-does-not-abort-the-others
  ;; The result sits in its own slot as a condition; the rest still complete.
  (let ((responses (request-many (list (test-url "/ok")
                                       (test-url "/close-early")
                                       (test-url "/status/404")))))
    (is (= 3 (length responses)))
    (is (typep (first responses) 'response))
    (is (typep (second responses) 'easy-error))
    (is (eq :partial-file (curl-error-code-name (second responses))))
    (is (= 404 (response-status (third responses))))))

(test request-many-reports-progress-as-each-finishes
  ;; Not merely that ON-COMPLETE is called with the right arguments, but that
  ;; it is called *when each request finishes*.  The first version of this
  ;; delegated to RUN-TRANSFERS, which accumulates internally and returns only
  ;; once nothing is running -- so every callback fired in a burst at the end,
  ;; and a progress bar driven by it would have sat at zero and jumped to done.
  ;; The requests below take roughly 50, 200, 400 and 800 milliseconds.
  (let ((completed '())
        (start (get-internal-real-time)))
    (flet ((elapsed () (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second 1.0)))
      (request-many (list (test-url "/drip?n=1&ms=50")
                          (test-url "/drip?n=4&ms=50")
                          (test-url "/drip?n=8&ms=50")
                          (test-url "/drip?n=16&ms=50"))
                    :on-complete (lambda (index outcome)
                                   (push (list index (response-status outcome)
                                               (elapsed))
                                         completed)))
      (setf completed (nreverse completed))
      (is (= 4 (length completed)))
      (is (equal '(0 1 2 3) (sort (mapcar #'first completed) #'<)))
      (is (every (lambda (entry) (= 200 (second entry))) completed))
      ;; The quickest must be reported well before the slowest.  A burst at the
      ;; end would put every timestamp within a millisecond of the others.
      (let ((earliest (reduce #'min completed :key #'third))
            (latest (reduce #'max completed :key #'third)))
        (is (< 0.2 (- latest earliest))
            "every callback fired at once (~,3Fs apart); ON-COMPLETE is not ~
reporting progress" (- latest earliest))))))

(test run-transfers-reports-each-result-as-it-is-read
  ;; The same guarantee one layer down, since RUN-TRANSFERS is public API.
  (with-multi (multi)
    (let ((handles (list (make-collecting-handle "/drip?n=1&ms=50")
                         (make-collecting-handle "/drip?n=12&ms=50")))
          (seen '())
          (start (get-internal-real-time)))
      (unwind-protect
           (progn
             (dolist (handle handles) (add-transfer multi handle))
             (run-transfers multi
                            :on-result
                            (lambda (result)
                              (declare (ignore result))
                              (push (/ (- (get-internal-real-time) start)
                                       internal-time-units-per-second 1.0)
                                    seen)))
             (is (= 2 (length seen)))
             (is (< 0.2 (abs (- (first seen) (second seen))))
                 "both results were reported at the same moment"))
        (dolist (handle handles) (close-handle handle))))))

(test request-many-handles-an-empty-list
  (is (null (request-many '()))))

(test request-many-can-use-a-session
  (with-session (session)
    (let ((responses (request-many (loop repeat 4 collect (test-url "/ok"))
                                   :session session)))
      (is (= 4 (length responses)))
      (is (every (lambda (r) (= 200 (response-status r))) responses)))))
