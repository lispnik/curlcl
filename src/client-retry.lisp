;;;; src/client-retry.lisp — retrying what is worth retrying.
;;;;
;;;; libcurl has no retry logic of its own -- the curl command-line tool
;;;; implements --retry above the library -- so this is one of the few places
;;;; the client layer has to do real work rather than delegate.
;;;;
;;;; What is retryable is a judgement, and getting it wrong in either direction
;;;; is costly: retrying a 400 wastes time on a request that will never
;;;; succeed, and retrying a non-idempotent POST can duplicate an order.  So
;;;; the default set is deliberately narrow -- transport failures that say
;;;; nothing was processed, plus the two status codes that explicitly mean "try
;;;; again" -- and POST and PATCH are excluded unless the caller says
;;;; otherwise, because only they know whether their endpoint is idempotent.

(in-package #:libcurl)

(defparameter *retryable-codes*
  '(:couldnt-connect
    :couldnt-resolve-host
    :couldnt-resolve-proxy
    :operation-timedout
    :ssl-connect-error
    :send-error
    :recv-error
    :got-nothing
    :partial-file
    :http2
    :http2-stream
    :quic-connect-error
    :no-connection-available)
  "CURLcodes worth retrying.

All of these are transport-level: the request either never reached the server
or the response never came back whole.  Notably absent is
CURLE_HTTP_RETURNED_ERROR, which means the server answered and said no.")

(defparameter *retryable-statuses* '(408 425 429 500 502 503 504)
  "HTTP statuses worth retrying.

429 and 503 are the server explicitly asking; 408 and 425 are about the
request never having been processed; the 5xx entries are the ones that are
usually transient.  501 and 505 are excluded because they will not change.")

(defparameter *idempotent-methods* '(:get :head :put :delete :options :trace)
  "Methods safe to retry without being asked.

POST and PATCH are excluded: only the caller knows whether repeating one
duplicates an effect.  :RETRY-NON-IDEMPOTENT overrides this.")

(defvar *jitter-random-state* (make-random-state t)
  "A random state of its own, for backoff jitter.

SBCL does not guarantee the global *RANDOM-STATE* is safe against concurrent
use, and the failure mode is precisely the one jitter exists to prevent:
several threads backing off together would draw correlated delays and retry in
lockstep.

Reseeded on image restore.  Seeded once at load time it would be baked into a
dumped executable, so every process started from that binary -- and every copy
of it on a fleet -- would draw the identical backoff sequence, which is the
same lockstep by another route.")

(defvar *jitter-lock* (bt:make-lock "libcurl retry jitter"))

(defun %reseed-jitter ()
  (setf *jitter-random-state* (make-random-state t)))

(uiop:register-image-restore-hook '%reseed-jitter nil)

(defun jitter-factor ()
  "A value in [-1, 1), drawn under a lock from a private random state."
  (bt:with-lock-held (*jitter-lock*)
    (- (random 2.0d0 *jitter-random-state*) 1.0d0)))

(defstruct (retry-policy (:conc-name retry-))
  "How and whether to retry a failed request."
  (max-attempts 1 :type (integer 1))
  ;; Seconds before the first retry; doubled by MULTIPLIER each time.
  (initial-delay 0.5d0)
  (multiplier 2.0d0)
  (max-delay 30.0d0)
  ;; Jitter as a fraction of the delay.  Without it, a fleet of clients that
  ;; failed together retries together, which is how a struggling server is
  ;; kept down.
  (jitter 0.25d0)
  (codes *retryable-codes*)
  (statuses *retryable-statuses*)
  (non-idempotent nil)
  ;; Honour a Retry-After header in preference to the computed backoff: the
  ;; server knows better than we do.
  (respect-retry-after t))

(defun make-retry (specification)
  "Coerce SPECIFICATION into a RETRY-POLICY.

Accepts NIL (no retries), an integer (that many attempts, default backoff), a
plist of RETRY-POLICY slots, or a policy, so the common cases stay short:

  :retry 3
  :retry '(:max-attempts 5 :initial-delay 1.0 :non-idempotent t)"
  (etypecase specification
    (null (make-retry-policy :max-attempts 1))
    (retry-policy specification)
    (integer (make-retry-policy :max-attempts (max 1 specification)))
    (cons (apply #'make-retry-policy specification))))

(defun retry-delay (policy attempt &key retry-after)
  "Seconds to wait before ATTEMPT (1-based), or RETRY-AFTER when the server said.

Jitter is applied to the computed delay but not to an explicit Retry-After:
that one is an instruction, not an estimate."
  (if (and retry-after (retry-respect-retry-after policy))
      (min retry-after (retry-max-delay policy))
      (let* ((base (min (* (retry-initial-delay policy)
                           (expt (retry-multiplier policy) (1- attempt)))
                        (retry-max-delay policy)))
             (jitter (retry-jitter policy)))
        (if (plusp jitter)
            ;; Full-width jitter around the base delay, floored at zero.
            (max 0d0 (+ base (* base jitter (jitter-factor))))
            base))))

(defun retryable-condition-p (policy condition)
  "True when CONDITION is a transport failure this policy retries."
  (and (typep condition 'easy-error)
       (not (typep condition 'callback-error))
       (member (curl-error-code-name condition) (retry-codes policy))
       t))

(defun retryable-response-p (policy response)
  (and (member (response-status response) (retry-statuses policy)) t))

(defun retryable-method-p (policy method)
  (or (retry-non-idempotent policy)
      (member method *idempotent-methods*)
      ;; A method we do not recognise is treated as unsafe.
      nil))

(defun parse-retry-after (value)
  "Seconds from a Retry-After header value, or NIL.

The header may be a delay in seconds or an HTTP date, and both are honoured:
the date form goes through libcurl's own parser, which accepts every spelling
seen in the wild.  A date already past yields 0 rather than a negative delay,
since the two clocks need not agree and a server saying \"now\" is the sensible
reading."
  (when value
    (let* ((trimmed (string-trim " " value))
           (seconds (parse-integer trimmed :junk-allowed t)))
      (cond ((and seconds (<= 0 seconds)) seconds)
            (seconds 0)                 ; a negative delay means "now"
            (t (let ((universal (parse-http-date trimmed)))
                 (when universal
                   (max 0 (- universal (get-universal-time))))))))))

(defun response-retry-after (response)
  (parse-retry-after (response-header-value response "retry-after")))

(defmacro with-retries ((policy method &key on-retry) &body body)
  "Run BODY under POLICY, retrying transport failures and retryable statuses.

BODY is expected to return a RESPONSE.  ON-RETRY, if given, is called with
(attempt delay reason) before each wait, which is what a caller hooks logging
onto."
  (alexandria:with-gensyms (attempt result condition delay reason retry-after
                            policy-var method-var hook)
    `(let ((,policy-var ,policy)
           (,method-var ,method)
           (,hook ,on-retry)
           (,attempt 0))
       (loop
         (incf ,attempt)
         (let ((,reason nil) (,retry-after nil) (,result nil))
           (handler-case (setf ,result (progn ,@body))
             (easy-error (,condition)
               (when (or (>= ,attempt (retry-max-attempts ,policy-var))
                         (not (retryable-condition-p ,policy-var ,condition))
                         (not (retryable-method-p ,policy-var ,method-var)))
                 (error ,condition))
               (setf ,reason ,condition)))
           (when (and (null ,reason) ,result)
             ;; A response arrived; it may still be one worth retrying.
             (if (and (< ,attempt (retry-max-attempts ,policy-var))
                      (retryable-response-p ,policy-var ,result)
                      (retryable-method-p ,policy-var ,method-var))
                 (setf ,reason ,result
                       ,retry-after (response-retry-after ,result))
                 (return ,result)))
           (let ((,delay (retry-delay ,policy-var ,attempt
                                      :retry-after ,retry-after)))
             (when ,hook (funcall ,hook ,attempt ,delay ,reason))
             (when (plusp ,delay) (sleep ,delay))))))))
