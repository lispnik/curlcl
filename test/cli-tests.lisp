;;;; test/cli-tests.lisp — curlcl's argument handling.
;;;;
;;;; The pieces that turn a curl command line into request options are ordinary
;;;; functions and are tested as such.  Whether the whole program behaves like
;;;; curl is checked against the real curl by the end-to-end tests at the
;;;; bottom, which run the built binary if there is one and skip if not.

(in-package #:curlcl/test)

(in-suite cli)

(test splitting-on-the-first-separator-only
  (multiple-value-bind (before after) (curlcl/cli::split-once "user:pass:word" #\:)
    (is (string= "user" before))
    ;; The rest is kept whole, so a password containing a colon survives.
    (is (string= "pass:word" after)))
  (multiple-value-bind (before after) (curlcl/cli::split-once "nocolon" #\:)
    (is (string= "nocolon" before))
    (is (null after))))

(test form-arguments-are-parsed-the-way-curl-spells-them
  (is (equal '(:name "field" :data "value")
             (curlcl/cli::parse-form-part "field=value")))
  ;; @ means a file rather than a literal value.
  (is (equal '(:name "upload" :file "/tmp/x.png")
             (curlcl/cli::parse-form-part "upload=@/tmp/x.png")))
  (is (equal '(:name "upload" :file "/tmp/x.png" :content-type "image/png")
             (curlcl/cli::parse-form-part "upload=@/tmp/x.png;type=image/png")))
  (is (equal '(:name "f" :data "v" :content-type "text/plain")
             (curlcl/cli::parse-form-part "f=v;type=text/plain")))
  (signals error (curlcl/cli::parse-form-part "no-equals-sign")))

(test remote-name-derives-a-filename-from-the-url
  (is (string= "file.tar.gz"
               (curlcl/cli::url-filename "https://example.com/a/b/file.tar.gz")))
  (is (string= "page" (curlcl/cli::url-filename "https://example.com/page")))
  ;; curl uses the last segment; with none, something has to be chosen.
  (is (string= "index.html" (curlcl/cli::url-filename "https://example.com/")))
  (is (string= "index.html" (curlcl/cli::url-filename "https://example.com"))))

(defun make-test-response (&key (status 200) (url "https://example.com/")
                                (content-type "text/html") (size 559)
                                (version :http/2) (redirects 0)
                                (timings '(:total 250000)))
  (make-instance 'curlcl:response
                 :status status :url url :version version
                 :headers (list (curlcl::make-http-header :name "Content-Type"
                                                           :value content-type))
                 :body (make-array 0 :element-type '(unsigned-byte 8))
                 :timings timings :redirect-count redirects
                 :size-download size))

(test write-out-expands-the-variables-curl-defines
  (let ((response (make-test-response)))
    (is (string= "200" (curlcl/cli::expand-write-out "%{http_code}" response)))
    (is (string= "text/html"
                 (curlcl/cli::expand-write-out "%{content_type}" response)))
    (is (string= "559" (curlcl/cli::expand-write-out "%{size_download}" response)))
    (is (string= "https://example.com/"
                 (curlcl/cli::expand-write-out "%{url_effective}" response)))
    ;; curl prints the bare version number, not a keyword.
    (is (string= "2" (curlcl/cli::expand-write-out "%{http_version}" response)))
    (is (string= "0.250000" (curlcl/cli::expand-write-out "%{time_total}" response)))))

(test write-out-handles-escapes-and-literal-text
  (let ((response (make-test-response)))
    (is (string= (format nil "code=200~%")
                 (curlcl/cli::expand-write-out "code=%{http_code}\\n" response)))
    (is (string= (format nil "a~Cb" #\Tab)
                 (curlcl/cli::expand-write-out "a\\tb" response)))
    (is (string= "no variables here"
                 (curlcl/cli::expand-write-out "no variables here" response)))))

(test an-unknown-write-out-variable-is-left-visible
  ;; Silently expanding to nothing would hide a typo; curl prints the variable
  ;; back, and so does this.
  (let ((response (make-test-response)))
    (is (string= "%{no_such_thing}"
                 (curlcl/cli::expand-write-out "%{no_such_thing}" response)))))

(test the-exit-code-is-the-curlcode
  ;; Scripts check curl's exit status, so ours has to be the same number.
  (is (= 6 (curlcl/cli::exit-code-for
            (make-condition 'curlcl:easy-error
                            :code 6 :code-name :couldnt-resolve-host))))
  (is (= 28 (curlcl/cli::exit-code-for
             (make-condition 'curlcl:easy-error
                             :code 28 :code-name :operation-timedout))))
  ;; Anything that is not a libcurl failure gets curl's "failed to initialise".
  (is (= 2 (curlcl/cli::exit-code-for (make-condition 'simple-error)))))

;;; End to end, against the real curl -----------------------------------------

(defparameter *curlcl-binary*
  ;; ASDF's program-op appends .exe on Windows, so the built name is not the
  ;; :build-pathname verbatim.  Both spellings are tried rather than
  ;; feature-conditionalised, so a binary built either way is found.
  (or (find-if #'probe-file
               (list (asdf:system-relative-pathname :curlcl "bin/curlcl")
                     (asdf:system-relative-pathname :curlcl "bin/curlcl.exe")))
      (asdf:system-relative-pathname :curlcl "bin/curlcl"))
  "The built driver.  Tests using it skip when it has not been built.")

(defun run-program-capturing (program arguments)
  "Run PROGRAM and return (values stdout exit-code stderr).

The child's stderr used to be discarded, which made a failure here say only
that the output was empty and the code was not zero -- no help at all when a
whole platform's worth of these fail at once, as they did the first time this
suite ran on Windows.  It is echoed to the test output instead, so the reason
the driver gave is in the log next to the assertion that noticed."
  (multiple-value-bind (output error-output code)
      (uiop:run-program (cons program arguments)
                        :output :string :error-output :string
                        :ignore-error-status t)
    (when (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                      error-output)))
      (format *test-dribble* "~&[curlcl~{ ~A~}] exit ~D, stderr: ~A~%"
              arguments code (string-right-trim '(#\Newline #\Return) error-output)))
    (values output code error-output)))

(defun null-device ()
  "The path that discards what is written to it.

/dev/null on Unix and NUL on Windows.  Spelled out because -o takes a path and
gets it from the command line, so there is nothing to translate it for us."
  (if (uiop:os-windows-p) "NUL" "/dev/null"))

(defmacro with-curlcl-or-skip (&body body)
  `(if (probe-file *curlcl-binary*)
       (progn ,@body)
       (skip "bin/curlcl has not been built; run `make build'")))

(test the-driver-fetches-from-the-local-server
  (with-curlcl-or-skip
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" (test-url "/ok")))
      (is (= 0 code))
      (is (string= "ok" output)))))

(defun native-namestring-of (pathname)
  (uiop:native-namestring pathname))

(test the-driver-reports-status-through-write-out
  (with-curlcl-or-skip
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" "-o" (null-device)
                                     "-w" "%{http_code}"
                                     (test-url "/status/418")))
      (is (= 0 code))
      (is (string= "418" output)))))

(test the-driver-exits-with-the-curlcode
  ;; The contract scripts depend on: a refused connection is 7, as curl's is.
  (with-curlcl-or-skip
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" "http://127.0.0.1:1/nothing"))
      (declare (ignore output))
      (is (= 7 code)))))

(test the-driver-fails-on-http-errors-only-when-asked
  (with-curlcl-or-skip
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" (test-url "/status/404")))
      (is (= 0 code) "a 404 without --fail is not an error")
      (is (string= "status 404" output)))
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" "-f" (test-url "/status/404")))
      (declare (ignore output))
      ;; CURLE_HTTP_RETURNED_ERROR, which is what curl --fail exits with.
      (is (= 22 code)))))

(test the-driver-follows-redirects-only-with-location
  (with-curlcl-or-skip
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" "-L" (test-url "/redirect/2")))
      (is (= 0 code))
      (is (string= "ok" output)))
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" "-o" (null-device) "-w" "%{http_code}"
                                     (test-url "/redirect/2")))
      (is (= 0 code))
      (is (string= "302" output)))))

(test the-driver-posts-data-and-sends-headers
  (with-curlcl-or-skip
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" "-d" "a=1" "-d" "b=2"
                                     "-H" "X-From: curlcl"
                                     (test-url "/echo")))
      (is (= 0 code))
      (is (search "method=POST" output))
      ;; Several -d arguments are joined with & as curl does.
      (is (search "body=a=1&b=2" output))
      (is (search "x-from: curlcl" (string-downcase output))))))

(defun count-substring (needle haystack)
  "How many non-overlapping times NEEDLE occurs in HAYSTACK."
  (loop with step = (max 1 (length needle))
        for start = 0 then (+ found step)
        for found = (search needle haystack :start2 start)
        while found
        count 1))

(defun dead-url ()
  "A URL nothing is listening on, for the retry timings.

Port 1 rather than an ephemeral one closed just beforehand: the same address
A-PROXY-THAT-IS-NOT-LISTENING-FAILS-AS-THE-PROXY has used across all three CI
platforms, and it cannot be won by a racing process the way a just-released
ephemeral port can."
  "http://127.0.0.1:1/")

(defmacro with-data-file ((path content) &body body)
  "Write CONTENT to a temporary file and bind PATH to its namestring."
  `(uiop:with-temporary-file (:pathname file :stream out :direction :output
                              :element-type '(unsigned-byte 8))
     (write-sequence (curlcl::coerce-to-octets ,content) out)
     (finish-output out)
     :close-stream
     (let ((,path (uiop:native-namestring file)))
       ,@body)))

(defun echo-body-of (output)
  "The body /echo reported, out of its whole reflection of the request.

Only this line can be compared between two clients: /echo also echoes the
request headers, and curl's User-Agent is not curlcl's, so whole outputs
differ for reasons that have nothing to do with what is being tested."
  (let* ((start (search "body=" output))
         (end (and start (position #\Newline output :start (+ start 5)))))
    (when start (subseq output (+ start 5) end))))

(defun echoed-body (&rest arguments)
  "Run curlcl with ARGUMENTS against /echo and return the body it reported."
  (echo-body-of (run-program-capturing (native-namestring-of *curlcl-binary*)
                                       (append (list "-s") arguments
                                               (list (test-url "/echo"))))))

(test -d-reads-a-file-and-strips-its-line-breaks
  ;; curl's -d @file joins the lines; --data-binary is the form that does not.
  ;; Before this, @file was not read at all -- the literal string "@/tmp/..."
  ;; was posted, and the request succeeded with the wrong body.
  (with-curlcl-or-skip
    (with-data-file (path (format nil "one~%two~%"))
      (is (string= "onetwo" (echoed-body "-d" (format nil "@~A" path)))))))

(test --data-binary-reads-a-file-verbatim
  (with-curlcl-or-skip
    (with-data-file (path (format nil "one~%two~%"))
      ;; The trailing newline is kept too, so /echo's own line ends up empty.
      (is (string= "one" (echoed-body "--data-binary" (format nil "@~A" path)))))))

(test --data-raw-does-not-treat-@-as-a-file
  (with-curlcl-or-skip
    (is (string= "@not-a-file" (echoed-body "--data-raw" "@not-a-file")))))

(test --data-urlencode-form-encodes-the-content-only
  ;; Form encoding, not plain percent-encoding: a space is + here.  The name
  ;; is left alone -- it is already a key, and encoding it would change it.
  (with-curlcl-or-skip
    (is (string= "name=a+b%26c" (echoed-body "--data-urlencode" "name=a b&c")))
    (is (string= "a+b%26c" (echoed-body "--data-urlencode" "=a b&c")))))

(test -d-reads-standard-input-for-@-dash
  (with-curlcl-or-skip
    (multiple-value-bind (output error-output code)
        (uiop:run-program (list (native-namestring-of *curlcl-binary*)
                                "-s" "-d" "@-" (test-url "/echo"))
                          :input (make-string-input-stream "from-stdin")
                          :output :string :error-output :string
                          :ignore-error-status t)
      (declare (ignore error-output))
      (is (= 0 code))
      (is (search "body=from-stdin" output)))))

(test the-data-flags-agree-with-curl-byte-for-byte
  ;; The check that matters for these: asserting our own idea of the encoding
  ;; is what let --data-urlencode ship %20 where curl sends +, and the test
  ;; would have agreed with the bug.  Compare against the real thing instead.
  (with-curlcl-or-skip
    ;; By exit status, for the reason spelled out in
    ;; THE-DRIVER-AGREES-WITH-CURL-WHERE-CURL-IS-AVAILABLE: RUN-PROGRAM with
    ;; :OUTPUT NIL returns NIL even on success, so the obvious test skips
    ;; unconditionally and the comparison never runs.
    (if (eql 0 (nth-value 2 (uiop:run-program '("curl" "--version")
                                              :output nil :error-output nil
                                              :ignore-error-status t)))
        (with-data-file (path (format nil "a b&c=d+e~%"))
          (dolist (arguments (list (list "-d" "a=1" "-d" "b=2")
                                   (list "-d" (format nil "@~A" path))
                                   (list "--data-binary" (format nil "@~A" path))
                                   (list "--data-raw" "@literal")
                                   (list "--data-urlencode" "k=a b&c")
                                   (list "--data-urlencode" "=a b~c/d")
                                   (list "--data-urlencode" (format nil "@~A" path))
                                   (list "--data-urlencode" (format nil "n@~A" path))))
            (let ((ours (echo-body-of
                         (run-program-capturing
                          (native-namestring-of *curlcl-binary*)
                          (append (list "-s") arguments (list (test-url "/echo"))))))
                  (theirs (echo-body-of
                           (run-program-capturing
                            "curl"
                            (append (list "-s") arguments (list (test-url "/echo")))))))
              (is (string= ours theirs)
                  "curlcl sent ~S where curl sent ~S, for ~{~A ~}"
                  ours theirs arguments))))
        (skip "curl is not installed"))))

(test -D-writes-the-response-headers-to-a-file
  (with-curlcl-or-skip
    (uiop:with-temporary-file (:pathname dump)
      (multiple-value-bind (output code)
          (run-program-capturing (native-namestring-of *curlcl-binary*)
                                 (list "-s" "-D" (uiop:native-namestring dump)
                                       "-o" (null-device) (test-url "/ok")))
        (declare (ignore output))
        (is (= 0 code))
        (let ((dumped (uiop:read-file-string dump)))
          (is (search "HTTP/1.1 200" dumped))
          (is (search "Content-Length:" dumped)))))))

(test -D-keeps-the-headers-of-every-url-not-just-the-last
  ;; Truncating per transfer would leave only the final set, which is the
  ;; obvious implementation and the wrong one.
  (with-curlcl-or-skip
    (uiop:with-temporary-file (:pathname dump)
      (run-program-capturing (native-namestring-of *curlcl-binary*)
                             (list "-s" "-D" (uiop:native-namestring dump)
                                   "-o" (null-device) "-o" (null-device)
                                   (test-url "/ok") (test-url "/ok")))
      (let ((dumped (uiop:read-file-string dump)))
        (is (= 2 (count-substring "HTTP/1.1 200" dumped)))))))

(test --dump-header-still-writes-when-fail-hides-the-body
  ;; -D is read afterwards to find out what happened; emptying it on the
  ;; failure it exists to explain would be perverse.
  (with-curlcl-or-skip
    (uiop:with-temporary-file (:pathname dump)
      (run-program-capturing (native-namestring-of *curlcl-binary*)
                             (list "-s" "-f" "-D" (uiop:native-namestring dump)
                                   (test-url "/status/404")))
      (is (search "404" (uiop:read-file-string dump))))))

(test --oauth2-bearer-sends-an-authorization-header
  (with-curlcl-or-skip
    (let ((output (run-program-capturing
                   (native-namestring-of *curlcl-binary*)
                   (list "-s" "--oauth2-bearer" "T0KEN" (test-url "/echo")))))
      (is (search "authorization: bearer t0ken" (string-downcase output))))))

(test --fail-with-body-keeps-the-body-and-still-exits-22
  (with-curlcl-or-skip
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" "--fail-with-body"
                                     (test-url "/status/404")))
      (is (= 22 code))
      (is (plusp (length output))))))

(test --retry-connrefused-decides-whether-a-refusal-is-retried
  ;; The library retries a refused connection by default and curl does not, so
  ;; the driver has to subtract it.  Timed rather than counted: with no retry
  ;; the refusal comes back at once, and each retry adds its delay.
  (with-curlcl-or-skip
    (let ((dead (dead-url)))
      (flet ((seconds (arguments)
               (let ((start (get-internal-real-time)))
                 (run-program-capturing (native-namestring-of *curlcl-binary*)
                                        (append (list "-s") arguments (list dead)))
                 (/ (- (get-internal-real-time) start)
                    internal-time-units-per-second))))
        (let ((without (seconds (list "--retry" "2" "--retry-delay" "1")))
              (with (seconds (list "--retry" "2" "--retry-delay" "1"
                                   "--retry-connrefused"))))
          (is (< without 1) "a refusal was retried without --retry-connrefused")
          (is (<= 2 with) "--retry-connrefused did not retry a refusal"))))))

(test --retry-delay-is-a-fixed-delay-not-a-starting-point
  ;; curl backs off exponentially only when --retry-delay is absent; given
  ;; one, it waits exactly that long each time.  Multiplying it made
  ;; `--retry-delay 1 --retry 3' take seven seconds where curl takes three.
  (with-curlcl-or-skip
    (let* ((dead (dead-url))
           (start (get-internal-real-time)))
      (run-program-capturing (native-namestring-of *curlcl-binary*)
                             (list "-s" "--retry" "3" "--retry-delay" "1"
                                   "--retry-connrefused" dead))
      (let ((elapsed (/ (- (get-internal-real-time) start)
                        internal-time-units-per-second)))
        ;; Three fixed one-second waits, not 1+2+4.
        (is (<= 2.5 elapsed) "the retries did not wait at all")
        (is (< elapsed 6) "~,1Fs suggests the delay is still being doubled"
            elapsed)))))

(test --retry-max-time-stops-the-retrying
  (with-curlcl-or-skip
    (let* ((dead (dead-url))
           (start (get-internal-real-time)))
      (run-program-capturing (native-namestring-of *curlcl-binary*)
                             (list "-s" "--retry" "20" "--retry-delay" "1"
                                   "--retry-connrefused" "--retry-max-time" "2"
                                   dead))
      (let ((elapsed (/ (- (get-internal-real-time) start)
                        internal-time-units-per-second)))
        ;; Twenty retries a second apart, cut off after two seconds.  Generous
        ;; on the upper bound: the budget bounds when a retry may start, and
        ;; the attempt that started inside it still finishes.
        (is (< elapsed 8) "~,1Fs: --retry-max-time did not bound the retrying"
            elapsed)))))

(test the-driver-runs-transfers-in-parallel
  ;; Timed against the sequential run rather than a fixed threshold.  A wall
  ;; clock bound has to be tight enough to prove overlap and loose enough to
  ;; survive a loaded shared runner, and there is no such number -- an earlier
  ;; version asserting "under 0.6s" passed locally and failed in CI.  Comparing
  ;; the two runs in the same conditions is self-calibrating.
  (with-curlcl-or-skip
    (let ((url (test-url "/drip?n=4&ms=50")))
      (flet ((timed (&rest arguments)
               (let ((start (get-internal-real-time)))
                 (let ((code (nth-value 1 (run-program-capturing
                                           (native-namestring-of *curlcl-binary*)
                                           arguments))))
                   (values (/ (- (get-internal-real-time) start)
                              internal-time-units-per-second 1.0)
                           code)))))
        (multiple-value-bind (sequential sequential-code)
            (timed "-s" "-o" (null-device) "-o" (null-device)
                   "-o" (null-device) "-o" (null-device) url url url url)
          (multiple-value-bind (parallel parallel-code)
              (timed "-s" "-Z" "-o" (null-device) "-o" (null-device)
                     "-o" (null-device) "-o" (null-device) url url url url)
            (is (= 0 sequential-code))
            (is (= 0 parallel-code))
            ;; Four overlapping 0.2s transfers should beat four consecutive
            ;; ones by a wide margin; half is conservative enough that only a
            ;; genuine loss of overlap fails it.
            (is (< parallel (* 0.7 sequential))
                "parallel took ~,2Fs against ~,2Fs sequential; they did not overlap"
                parallel sequential)))))))

(test the-driver-agrees-with-curl-where-curl-is-available
  ;; The comparison that matters: same URL, same body.  Skipped when there is
  ;; no curl to compare against.
  (with-curlcl-or-skip
    ;; Detected by exit status: RUN-PROGRAM with :OUTPUT NIL returns NIL as its
    ;; first value even on success, so testing that would skip unconditionally.
    (if (eql 0 (nth-value 2 (uiop:run-program '("curl" "--version")
                                              :output nil :error-output nil
                                              :ignore-error-status t)))
        (let ((ours (run-program-capturing (native-namestring-of *curlcl-binary*)
                                           (list "-s" (test-url "/large?bytes=5000"))))
              (theirs (run-program-capturing "curl"
                                             (list "-s" (test-url "/large?bytes=5000")))))
          (is (string= ours theirs)))
        (skip "curl is not installed"))))

(test the-driver-streams-an-upload-from-a-file
  (with-curlcl-or-skip
    (uiop:with-temporary-file (:pathname path :stream out :direction :output
                               :element-type '(unsigned-byte 8))
      (write-sequence (curlcl::coerce-to-octets "uploaded-by-curlcl") out)
      (finish-output out)
      :close-stream
      (multiple-value-bind (output code)
          (run-program-capturing (native-namestring-of *curlcl-binary*)
                                 (list "-s" "-T" (uiop:native-namestring path)
                                       (test-url "/echo")))
        (is (= 0 code))
        (is (search "method=PUT" output))
        (is (search "body=uploaded-by-curlcl" output))))))

(test the-driver-uploads-from-standard-input
  ;; `-T -' is why the upload has to stream: stdin has no length to buffer by.
  (with-curlcl-or-skip
    (multiple-value-bind (output error-output code)
        (uiop:run-program (list (native-namestring-of *curlcl-binary*)
                                "-s" "-T" "-" (test-url "/echo"))
                          :input (make-string-input-stream "piped-in")
                          :output :string :error-output :string
                          :ignore-error-status t)
      (declare (ignore error-output))
      (is (= 0 code))
      (is (search "body=piped-in" output)))))

(test the-driver-writes-nothing-when-fail-is-given
  ;; `curl --fail' outputs nothing at all on a server error, which is only
  ;; achievable by aborting before the body arrives.
  (with-curlcl-or-skip
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" "-f" (test-url "/status/404")))
      (is (= 22 code))
      (is (string= "" output)
          "--fail wrote ~D bytes of the error body" (length output)))))

(test the-driver-writes-the-error-body-without-fail
  (with-curlcl-or-skip
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" (test-url "/status/404")))
      (is (= 0 code))
      (is (string= "status 404" output)))))

(test parallel-output-files-are-complete-and-closed
  (with-curlcl-or-skip
    (uiop:with-temporary-file (:pathname a)
      (uiop:with-temporary-file (:pathname b)
        (multiple-value-bind (output code)
            (run-program-capturing
             (native-namestring-of *curlcl-binary*)
             (list "-s" "-Z"
                   "-o" (uiop:native-namestring a)
                   "-o" (uiop:native-namestring b)
                   (test-url "/large?bytes=40000")
                   (test-url "/large?bytes=40000")))
          (declare (ignore output))
          (is (= 0 code))
          (with-open-file (in a :element-type '(unsigned-byte 8))
            (is (= 40000 (file-length in))))
          (with-open-file (in b :element-type '(unsigned-byte 8))
            (is (= 40000 (file-length in)))))))))

(test fail-still-expands-write-out
  ;; curl runs --write-out whatever the transfer did: `curl -f -w %{http_code}'
  ;; prints 404 and exits 22.  Routing --fail through CURLOPT_FAILONERROR
  ;; destroyed the response, so there was nothing left to expand from and a
  ;; common scripting idiom printed nothing.
  (with-curlcl-or-skip
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" "-f" "-w" "%{http_code}"
                                     (test-url "/status/404")))
      (is (= 22 code))
      (is (string= "404" output)))))

(test fail-still-allows-retrying-a-retryable-status
  ;; And it must not disable --retry.  Under FAILONERROR a 503 arrived as a
  ;; condition whose code is not retryable, so exactly one request was made.
  (with-curlcl-or-skip
    (reset-flaky "cli-fail-retry")
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" "-f" "--retry" "5" "--retry-delay" "0"
                                     (test-url "/flaky?id=cli-fail-retry&fail=2")))
      (is (= 0 code) "--fail stopped --retry from retrying")
      (is (search "succeeded on attempt 3" output)))))

(test fail-suppresses-headers-as-well-as-the-body
  ;; "no output at all" includes the headers -i would otherwise print.
  (with-curlcl-or-skip
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" "-f" "-i" (test-url "/status/404")))
      (is (= 22 code))
      (is (string= "" output)))))

(test fail-catches-a-401-that-failonerror-lets-slip
  ;; libcurl documents CURLOPT_FAILONERROR as not fail-safe, 401 and 407 in
  ;; particular when authentication is involved.  Watching the status line has
  ;; no such gap.
  (with-curlcl-or-skip
    (multiple-value-bind (output code)
        (run-program-capturing (native-namestring-of *curlcl-binary*)
                               (list "-s" "-f" (test-url "/auth/basic")))
      (is (= 22 code))
      (is (string= "" output)))))

(test status-lines-are-recognised
  (is (= 404 (curlcl/cli::parse-status-line "HTTP/1.1 404 Not Found")))
  (is (= 200 (curlcl/cli::parse-status-line "HTTP/2 200")))
  ;; An ordinary header is not a status line.
  (is (null (curlcl/cli::parse-status-line "Content-Type: text/plain")))
  (is (null (curlcl/cli::parse-status-line ""))))

(test the-driver-truncates-its-output-file-between-retries
  ;; curl -o file --retry, and the reason the driver says :RETRY-STREAMED and
  ;; then resets its own sink.  /flaky answers the failing attempts with a body
  ;; too, so a driver that kept writing to the same open file would leave
  ;; "attempt 1 of 2 failing" in front of the real answer and exit 0.
  (with-curlcl-or-skip
    (reset-flaky "cli-retry-truncate")
    (uiop:with-temporary-file (:pathname path)
      (multiple-value-bind (output code)
          (run-program-capturing
           (native-namestring-of *curlcl-binary*)
           (list "-s" "-o" (uiop:native-namestring path)
                 "--retry" "5" "--retry-delay" "0"
                 (test-url "/flaky?id=cli-retry-truncate&fail=2")))
        (declare (ignore output))
        (is (= 0 code))
        (let ((written (file-contents path)))
          (is (string= "succeeded on attempt 3" written)
              "the file holds ~S, so a failed attempt's body survived"
              written))))))

(test the-driver-truncates-each-parallel-output-between-retries
  ;; The same property under --parallel, where the retries of several
  ;; transfers interleave and each has to reset only its own destination.
  (with-curlcl-or-skip
    (reset-flaky "cli-par-a")
    (reset-flaky "cli-par-b")
    (uiop:with-temporary-file (:pathname first-path)
      (uiop:with-temporary-file (:pathname second-path)
        (multiple-value-bind (output code)
            (run-program-capturing
             (native-namestring-of *curlcl-binary*)
             (list "-s" "-Z"
                   "-o" (uiop:native-namestring first-path)
                   "-o" (uiop:native-namestring second-path)
                   "--retry" "5" "--retry-delay" "0"
                   (test-url "/flaky?id=cli-par-a&fail=1")
                   (test-url "/flaky?id=cli-par-b&fail=2")))
          (declare (ignore output))
          (is (= 0 code))
          (is (string= "succeeded on attempt 2" (file-contents first-path)))
          (is (string= "succeeded on attempt 3" (file-contents second-path))))))))
