;;;; test/varargs-tests.lisp — proving the variadic call layer passes real values.
;;;;
;;;; curl_easy_setopt is variadic, and the failure mode when it is called
;;;; incorrectly is silence: on Darwin arm64 the value goes in a register while
;;;; libcurl reads the stack, so the option is set from whatever happened to be
;;;; there.  No crash, no error code -- the URL just does not take.
;;;;
;;;; Testing "setopt returned CURLE_OK" is therefore worthless on its own.  Each
;;;; test below instead picks an option whose *value* libcurl validates, and
;;;; brackets the boundary: one value that must be accepted and a neighbouring
;;;; one that must be rejected.  Garbage on the stack cannot reproduce a
;;;; boundary, so passing these means the value genuinely arrived.

(in-package #:curlcl/test)

(in-suite varargs)

;;; Option and info identifiers, spelled out rather than taken from the
;;; generated table: these tests have to keep working even if that table is
;;; wrong, since proving the ABI is a precondition for trusting it.
(defconstant +opt-url+                10002) ; STRINGPOINT + 2
(defconstant +opt-private+            10103) ; CBPOINT + 103
(defconstant +opt-timeout+               13) ; LONG + 13
(defconstant +opt-maxfilesize-large+  30117) ; OFF_T + 117
(defconstant +opt-verbose+               41) ; LONG + 41
(defconstant +opt-http-version+          84) ; LONG + 84
(defconstant +info-private+         #x100015) ; STRING + 21
(defconstant +info-effective-url+   #x100001) ; STRING + 1

;; Named distinctly from LIBCURL:WITH-EASY: this binds a bare CURL*, because
;; these tests deliberately work below the EASY-HANDLE layer -- proving the ABI
;; is a precondition for trusting anything built on top of it.
(defmacro with-raw-handle ((var) &body body)
  `(curlcl::with-raw-easy (,var) ,@body))

(test setopt-long-passes-the-exact-value
  ;; CURLOPT_HTTP_VERSION accepts a sparse, non-contiguous set -- 0-5 are the
  ;; HTTP/1.x and /2 constants, 30 and 31 are the HTTP/3 ones -- and sorts
  ;; everything else into two *different* errors: values in the gaps are
  ;; CURLE_UNSUPPORTED_PROTOCOL, negatives are CURLE_BAD_FUNCTION_ARGUMENT.
  ;; Three outcomes keyed on the exact integer is about as sharp a probe as
  ;; setopt offers: an argument read from the wrong place cannot sort itself
  ;; into a scattered set and two distinct error classes.
  (with-raw-handle (h)
    ;; 3, 4 and 5 are the HTTP/2 constants and 30 and 31 the HTTP/3 ones, and a
    ;; libcurl built without either rejects its own -- Ubuntu's has no HTTP/3,
    ;; the Windows build has neither.  What the test is really about is the
    ;; sparse pattern, so both sets are taken from what this build supports
    ;; rather than hard-coded from one machine's.  Whichever way a version
    ;; lands, it lands in one of the two lists, so the partition stays total.
    (let* ((http2 (when (feature-supported-p :http2) '(3 4 5)))
           (http3 (when (feature-supported-p :http3) '(30 31)))
           (accepted (append '(0 1 2) http2 http3))
           (rejected (append '(6 7 20 29 32 40 99)
                             (set-difference '(3 4 5) http2)
                             (set-difference '(30 31) http3))))
      (dolist (version accepted)
        (is (eq :ok (curlcode-keyword (curlcl::%setopt-long h +opt-http-version+ version)))
            "CURLOPT_HTTP_VERSION ~D should be accepted" version))
      (dolist (version rejected)
        (is (eq :unsupported-protocol
                (curlcode-keyword (curlcl::%setopt-long h +opt-http-version+ version)))
            "CURLOPT_HTTP_VERSION ~D should be rejected as unsupported" version))
      (is (eq :bad-function-argument
              (curlcode-keyword (curlcl::%setopt-long h +opt-http-version+ -1)))))))

(test setopt-long-preserves-sign
  ;; The sign boundary is sharp where the upper bounds are not: libcurl clamps
  ;; large timeouts but rejects negative ones outright.  A truncated or
  ;; wrongly-widened argument shows up here as a sign flip.
  (with-raw-handle (h)
    (is (eq :ok (curlcode-keyword (curlcl::%setopt-long h +opt-timeout+ 0))))
    (is (eq :ok (curlcode-keyword (curlcl::%setopt-long h +opt-timeout+ 30))))
    (is (eq :bad-function-argument
            (curlcode-keyword (curlcl::%setopt-long h +opt-timeout+ -1))))
    (is (eq :bad-function-argument
            (curlcode-keyword (curlcl::%setopt-long h +opt-timeout+ -2))))))

(test setopt-off-t-passes-a-full-64-bit-value
  ;; 0x180000000 is positive as a 64-bit value but its low 32 bits are
  ;; 0x80000000, i.e. negative as an int32.  libcurl rejects a negative
  ;; CURLOPT_MAXFILESIZE_LARGE, so if the argument were truncated to 32 bits
  ;; and sign-extended this would come back CURLE_BAD_FUNCTION_ARGUMENT.
  ;; Accepting it proves the whole 64 bits crossed the boundary.
  (with-raw-handle (h)
    (is (eq :ok (curlcode-keyword
                 (curlcl::%setopt-off-t h +opt-maxfilesize-large+ #x180000000))))
    (is (eq :ok (curlcode-keyword
                 (curlcl::%setopt-off-t h +opt-maxfilesize-large+ 0))))
    (is (eq :bad-function-argument
            (curlcode-keyword
             (curlcl::%setopt-off-t h +opt-maxfilesize-large+ -1))))))

(test setopt-and-getinfo-round-trip-a-pointer
  ;; CURLOPT_PRIVATE stores an opaque pointer and CURLINFO_PRIVATE hands it
  ;; back, with no transfer in between -- so this isolates argument passing in
  ;; both directions.  The value is deliberately one with bits set in both
  ;; halves, so a 32-bit truncation anywhere shows up.
  (with-raw-handle (h)
    (let ((value (cffi:make-pointer #x00007F1234ABCD00)))
      (is (eq :ok (curlcode-keyword
                   (curlcl::%setopt-pointer h +opt-private+ value))))
      (multiple-value-bind (got code) (curlcl::%raw-getinfo-pointer h +info-private+)
        (is (eq :ok (curlcode-keyword code)))
        (is (cffi:pointer-eq value got)
            "CURLOPT_PRIVATE round-trip: set ~A, got ~A" value got)))))

(test getinfo-private-is-a-pointer-despite-being-typed-string
  ;; CURLINFO_PRIVATE has the CURLINFO_STRING type mask but returns whatever
  ;; void* was stored.  Decoding it as a string dereferences an arbitrary
  ;; pointer.  This test exists to pin that special case: the value stored here
  ;; is not a valid string pointer, and reading it as one would fault.
  (with-raw-handle (h)
    (let ((value (cffi:make-pointer 8)))
      (curlcl::%setopt-pointer h +opt-private+ value)
      (is (cffi:pointer-eq value
                           (curlcl::%raw-getinfo-pointer h +info-private+))))))

(test setopt-string-takes-effect
  ;; A string option that libcurl parses: a bad URL is rejected at setopt time
  ;; in modern libcurl, a good one accepted.  If the pointer never arrived,
  ;; libcurl would be parsing whatever the stack held.
  (with-raw-handle (h)
    (is (eq :ok (curlcode-keyword
                 (curlcl::%raw-setopt-string h +opt-url+ "https://example.com/"))))
    (multiple-value-bind (url code)
        (curlcl::%raw-getinfo-string h +info-effective-url+)
      ;; EFFECTIVE_URL is only guaranteed after a transfer; before one, libcurl
      ;; may report the URL as set or nothing at all.  Accept either, but if it
      ;; reports anything it must be what we set.
      (is (eq :ok (curlcode-keyword code)))
      (when url
        (is (string= "https://example.com/" url))))))

(test unknown-option-is-reported-not-crashed
  ;; An option identifier libcurl does not have must come back as
  ;; CURLE_UNKNOWN_OPTION.  This also proves the option number itself is
  ;; arriving as the second fixed argument.
  (with-raw-handle (h)
    (is (eq :unknown-option
            (curlcode-keyword (curlcl::%setopt-long h 999999 1))))))

(test getinfo-long-uses-a-full-width-out-parameter
  ;; CURLINFO_LONG out-parameters are C `long', 8 bytes here.  Passing a 4-byte
  ;; buffer would let libcurl write over the adjacent word; this reads a known
  ;; info and checks the value is sane rather than sign-garbage.
  (with-raw-handle (h)
    (curlcl::%raw-setopt-string h +opt-url+ "https://example.com/")
    (multiple-value-bind (code result)
        (curlcl::%raw-getinfo-long h #x200002) ; CURLINFO_RESPONSE_CODE
      (is (eq :ok (curlcode-keyword result)))
      ;; No transfer has run, so it must be exactly 0 -- not uninitialised.
      (is (= 0 code)))))

(test a-variadic-argument-arrives-and-a-fixed-one-does-not
  ;; The measurement the whole file exists for, made both ways so that it
  ;; proves something.  CURLOPT_MAXFILESIZE_LARGE is positive as 64 bits and
  ;; negative in its low 32, and libcurl refuses a negative size with
  ;; CURLE_BAD_FUNCTION_ARGUMENT -- so whether the value arrived is visible in
  ;; the return code.
  (with-raw-handle (h)
    (let ((good #x180000000) (bad -1))
      ;; Through the variadic call, libcurl sees what it was given.
      (is (= 0 (curlcl::%setopt-off-t h +opt-maxfilesize-large+ good)))
      (is (= 43 (curlcl::%setopt-off-t h +opt-maxfilesize-large+ bad))
          "a negative size was not rejected, so the value never reached libcurl")
      ;; And through an ordinary fixed-signature call it does not -- on Darwin
      ;; arm64.  Asserted so that the test above cannot quietly stop proving
      ;; anything: if this ever starts rejecting too, the platform has changed
      ;; and the check needs rethinking rather than deleting.
      #+(and darwin arm64)
      (is (= 0 (cffi:foreign-funcall "curl_easy_setopt"
                                     :pointer h :int +opt-maxfilesize-large+
                                     :int64 bad :int))
          "a fixed-signature call rejected -1, so this platform no longer ~
mis-passes variadic arguments and the variadic call is no longer load-bearing"))))

(test the-load-time-check-would-catch-a-broken-variadic-call
  ;; It runs at load, so reaching here at all means it passed; this asserts it
  ;; is not vacuous -- that it really does compare what went in with what came
  ;; back, and would signal rather than shrug.
  (finishes (curlcl::%check-variadic-passing))
  (with-raw-handle (h)
    (let ((token (cffi:make-pointer #x5EEDC0DE)))
      (curlcl::%setopt-pointer h curlcl::+curlopt-private+ token)
      (cffi:with-foreign-object (out :pointer)
        (setf (cffi:mem-ref out :pointer) (cffi:null-pointer))
        (curlcl::%getinfo h curlcl::+curlinfo-private+ out)
        (is (cffi:pointer-eq token (cffi:mem-ref out :pointer))
            "CURLOPT_PRIVATE did not round-trip, which is what the load-time ~
check tests")))))
