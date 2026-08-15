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

;;; The readers are declared before the conditions that supply their methods so
;;; that each carries its own documentation.  A slot's :DOCUMENTATION describes
;;; the slot and is not what DESCRIBE or a documentation generator finds when
;;; asked about the function.

(defgeneric curl-error-code (condition)
  (:documentation
   "The integer result code libcurl returned, or NIL if the failure did not
come from a libcurl call.  Kept as an integer rather than only a keyword
because a libcurl newer than this binding can return a code it has no name
for."))

(defgeneric curl-error-code-name (condition)
  (:documentation
   "CURL-ERROR-CODE as a keyword -- :OPERATION-TIMEDOUT, :COULDNT-RESOLVE-HOST
-- or the integer unchanged when this binding has no name for it."))

(defgeneric curl-error-message (condition)
  (:documentation
   "libcurl's own one-line description of the result code, from
curl_easy_strerror and its siblings.  Generic by nature: it describes the code,
not the transfer.  Prefer CURL-ERROR-DETAIL when there is one."))

(defgeneric curl-error-detail (condition)
  (:documentation
   "The text libcurl wrote to CURLOPT_ERRORBUFFER for this transfer, or NIL.

Far more specific than CURL-ERROR-MESSAGE -- it names the host, file or
certificate at fault -- which is why the report methods prefer it."))

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
  (:report (lambda (c s) (%report-curl-error c s "libcurl multi call failed")))
  (:documentation
   "A CURLMcode failure from the multi interface.

Its codes are a different family from CURLcode with their own numbering, so the
class is distinct rather than the code being folded into EASY-ERROR."))

(define-condition share-error (curl-error)
  ()
  (:report (lambda (c s) (%report-curl-error c s "libcurl share call failed")))
  (:documentation "A CURLSHcode failure from the share interface."))

(define-condition url-error (curl-error)
  ((url :initarg :url :initform nil :reader curl-error-url))
  (:report (lambda (c s)
             (%report-curl-error c s "libcurl URL call failed")
             (when (curl-error-url c)
               (format s " (~S)" (curl-error-url c)))))
  (:documentation
   "A CURLUcode failure from the URL parser.

Signalled for input libcurl cannot parse.  A URL that merely lacks a component
is not this: URL-PART answers NIL for a missing port or query, since that is a
fact about the URL rather than a failure to read it."))

;;; libcurl has no curl_header_strerror, unlike every other code family, so
;;; MESSAGE here is filled from a table this library maintains by hand.
(define-condition header-error (curl-error)
  ()
  (:report (lambda (c s) (%report-curl-error c s "libcurl header call failed")))
  (:documentation
   "A CURLHcode failure from the header API.

CURL-ERROR-MESSAGE comes from a table maintained by hand in headers.lisp, since
libcurl ships no curl_header_strerror to ask."))

(defgeneric callback-error-cause (condition)
  (:documentation
   "The condition the user's callback actually signalled.

The point of the wrapper: without it the caller would see libcurl's
CURLE_WRITE_ERROR and not the FILE-ERROR that caused it."))

(defgeneric callback-error-kind (condition)
  (:documentation
   "Which callback signalled -- :WRITE, :READ, :PROGRESS and so on -- or NIL."))

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

(defgeneric unsupported-option-name (condition)
  (:documentation "The option keyword the loaded libcurl does not have."))

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
  ((name :initarg :name :reader unsupported-feature-name
         :documentation "What is missing, as text fit to print in a report."))
  (:report (lambda (c s)
             (format s "The loaded libcurl was built without ~A."
                     (unsupported-feature-name c))))
  (:documentation
   "The loaded libcurl was built without something the call needs.

A property of the build rather than of the version: the same libcurl release
may or may not have websockets, HTTP/3 or a given TLS backend, so this is
raised from a runtime feature check rather than a version comparison."))

(define-condition library-not-found (curl-error)
  ((candidates :initarg :candidates :initform nil
               :reader library-not-found-candidates
               :documentation "The names and paths that were tried, in order."))
  (:report (lambda (c s)
             (format s "Could not load libcurl.~@[  Tried: ~{~A~^, ~}.~]~
~%Set the LIBCURL_LIBRARY environment variable to an explicit path, or call ~
LOAD-LIBCURL with one."
                     (library-not-found-candidates c))))
  (:documentation
   "No libcurl could be opened at all.

Signalled from conditions.lisp, which makes no foreign calls of its own, so
this is reportable even though nothing else in the library can run."))
