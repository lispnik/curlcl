;;;; src/conditions.lisp — the condition hierarchy.
;;;;
;;;; Deliberately free of foreign calls, so it can load before the library is
;;;; even open and be used to report a failure to open it.  Callers pass in
;;;; already-decoded text; nothing here calls curl_easy_strerror.
;;;;
;;;; The shape worth noting is CALLBACK-ERROR.  A Lisp condition signalled
;;;; inside a write or read callback cannot be allowed to unwind into C, so it
;;;; is caught at the callback boundary and stashed; once curl_easy_perform has
;;;; returned, PERFORM re-signals it wrapped in a CALLBACK-ERROR carrying the
;;;; original as its cause.  That way the caller sees their own FILE-ERROR
;;;; rather than the CURLE_WRITE_ERROR libcurl reports for it.

(in-package #:libcurl)

(define-condition curl-condition (condition)
  ()
  (:documentation "Root of every condition this library signals."))

(define-condition curl-error (curl-condition error)
  ((code :initarg :code :initform nil :reader curl-error-code
         :documentation "The integer result code from libcurl, or NIL.")
   (code-name :initarg :code-name :initform nil :reader curl-error-code-name
              :documentation "CODE as a keyword, e.g. :OPERATION-TIMEDOUT.")
   (message :initarg :message :initform nil :reader curl-error-message
            :documentation "libcurl's own description of CODE.")
   (detail :initarg :detail :initform nil :reader curl-error-detail
           :documentation
           "Text from CURLOPT_ERRORBUFFER, when one was in effect.  Usually far
more specific than MESSAGE -- it names the host, file or certificate at
fault -- so reports prefer it."))
  (:documentation "Base class for failures reported by libcurl itself."))

(defun %report-curl-error (c stream what)
  (format stream "~A~@[ ~A~]: ~A"
          what
          (or (curl-error-code-name c) (curl-error-code c))
          (or (curl-error-detail c)
              (curl-error-message c)
              "unknown error")))

(define-condition easy-error (curl-error)
  ((url :initarg :url :initform nil :reader curl-error-url
        :documentation "The URL being transferred, when known."))
  (:report (lambda (c s)
             (%report-curl-error c s "libcurl transfer failed")
             (when (curl-error-url c)
               (format s " (~A)" (curl-error-url c)))))
  (:documentation "A CURLcode failure from the easy interface."))

(define-condition multi-error (curl-error)
  ()
  (:report (lambda (c s) (%report-curl-error c s "libcurl multi call failed"))))

(define-condition share-error (curl-error)
  ()
  (:report (lambda (c s) (%report-curl-error c s "libcurl share call failed"))))

(define-condition url-error (curl-error)
  ((url :initarg :url :initform nil :reader curl-error-url))
  (:report (lambda (c s)
             (%report-curl-error c s "libcurl URL call failed")
             (when (curl-error-url c)
               (format s " (~S)" (curl-error-url c))))))

;;; libcurl has no curl_header_strerror, unlike every other code family, so
;;; MESSAGE here is filled from a table this library maintains by hand.
(define-condition header-error (curl-error)
  ()
  (:report (lambda (c s) (%report-curl-error c s "libcurl header call failed"))))

(define-condition callback-error (curl-error)
  ((cause :initarg :cause :reader callback-error-cause
          :documentation "The condition the user's callback actually signalled.")
   (kind :initarg :kind :initform nil :reader callback-error-kind
         :documentation "Which callback signalled, e.g. :WRITE or :PROGRESS."))
  (:report (lambda (c s)
             (format s "Lisp condition signalled inside the ~(~A~) callback: ~A"
                     (or (callback-error-kind c) "libcurl")
                     (callback-error-cause c))))
  (:documentation
   "Signalled after a transfer whose callback signalled a Lisp condition.  The
transfer was aborted at the callback boundary rather than allowing a non-local
exit to unwind into C.  CALLBACK-ERROR-CAUSE holds the original condition."))

(define-condition unsupported-option (curl-error)
  ((name :initarg :name :reader unsupported-option-name
         :documentation "The option keyword that is not available.")
   (running-version :initarg :running-version :initform nil
                    :reader unsupported-option-running-version))
  (:report (lambda (c s)
             (format s "The loaded libcurl~@[ (~A)~] does not support the ~
option ~S."
                     (unsupported-option-running-version c)
                     (unsupported-option-name c))))
  (:documentation
   "Signalled instead of letting libcurl return CURLE_UNKNOWN_OPTION, so the
report names the option and the version that lacks it."))

(define-condition unsupported-feature (curl-error)
  ((name :initarg :name :reader unsupported-feature-name))
  (:report (lambda (c s)
             (format s "The loaded libcurl was built without ~A."
                     (unsupported-feature-name c)))))

(define-condition library-not-found (curl-error)
  ((candidates :initarg :candidates :initform nil
               :reader library-not-found-candidates))
  (:report (lambda (c s)
             (format s "Could not load libcurl.~@[  Tried: ~{~A~^, ~}.~]~
~%Set the LIBCURL_LIBRARY environment variable to an explicit path, or call ~
LOAD-LIBCURL with one."
                     (library-not-found-candidates c)))))
