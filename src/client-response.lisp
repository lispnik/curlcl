;;;; src/client-response.lisp — what a request gives back.
;;;;
;;;; A RESPONSE captures everything worth keeping from a finished transfer, so
;;;; the easy handle behind it can be reset and returned to a pool immediately.
;;;; That matters for the session layer: libcurl's info values and header
;;;; structures are only valid until the next transfer on the same handle, so
;;;; anything a caller might look at later has to be copied out now.
;;;;
;;;; Headers keep their order and their duplicates.  Collapsing them into a
;;;; plist or a hash table loses Set-Cookie, which is the header most likely to
;;;; repeat and least safe to lose, so lookup is a scan over an ordered vector
;;;; -- and a response has a handful of headers, not thousands.

(in-package #:libcurl)

(defgeneric response-status (response)
  (:documentation "The HTTP status code, as an integer."))

(defgeneric response-body (response)
  (:documentation
   "The response body: octets, or a string when the charset was known.

Empty when the body was streamed past rather than collected -- with :OUTPUT or
:ON-DATA the bytes went straight to their destination and were never buffered.
RESPONSE-SIZE-DOWNLOAD reports how many arrived in that case."))

(defgeneric response-url (response)
  (:documentation
   "The effective URL, after any redirects -- CURLINFO_EFFECTIVE_URL.

Not the URL that was requested: with :MAX-REDIRECTS greater than zero these
differ, and it is this one the body actually came from."))

(defgeneric response-version (response)
  (:documentation
   "The HTTP version the transfer used: :HTTP/1.0, :HTTP/1.1, :HTTP/2, :HTTP/3
or NIL.

What was negotiated, not what was asked for, so this is the way to find out
whether an HTTP/2 request got HTTP/2."))

(defgeneric response-request-method (response)
  (:documentation
   "The method the request used, as a keyword, or NIL.

Kept on the response because a redirect can change it: libcurl turns a POST
into a GET on a 301 or 302 unless told otherwise, so the method that fetched
the body is not always the one that was asked for."))

(defgeneric response-timings (response)
  (:documentation
   "A plist of microsecond timings from getinfo.

Keys are :NAMELOOKUP, :CONNECT, :APPCONNECT, :PRETRANSFER, :STARTTRANSFER,
:TOTAL and :REDIRECT.  These are libcurl's CURLINFO_*_TIME_T values, which are
integer microseconds and not the older floating-point seconds."))

(defgeneric response-redirect-count (response)
  (:documentation "How many redirects were followed to reach this response."))

(defgeneric response-size-download (response)
  (:documentation
   "Bytes of body received, from getinfo rather than from the body itself.

The distinction matters exactly when the body was not kept: with :OUTPUT or
:ON-DATA, (LENGTH (RESPONSE-BODY R)) is zero however much arrived."))

(defgeneric response-size-upload (response)
  (:documentation "Bytes of request body sent, from getinfo."))

(defclass response ()
  ((status :initarg :status :reader response-status
           :documentation "The HTTP status code, as an integer.")
   (headers :initarg :headers :reader response-header-list
            :documentation "An ordered list of HTTP-HEADER, duplicates and all.")
   (body :initarg :body :accessor response-body
         :documentation "Octets, or a string when the charset was known.")
   (url :initarg :url :reader response-url
        :documentation "The effective URL, after any redirects.")
   (version :initarg :version :initform nil :reader response-version)
   (request-method :initarg :request-method :initform nil
                   :reader response-request-method)
   (timings :initarg :timings :initform '() :reader response-timings
            :documentation "A plist of microsecond timings from getinfo.")
   (redirect-count :initarg :redirect-count :initform 0
                   :reader response-redirect-count)
   ;; Taken from getinfo rather than measured from BODY, because with :OUTPUT
   ;; or :ON-DATA the body was streamed past us and never buffered -- so the
   ;; body's length is zero however many bytes actually arrived.
   (size-download :initarg :size-download :initform 0
                  :reader response-size-download)
   (size-upload :initarg :size-upload :initform 0
                :reader response-size-upload))
  (:documentation "A completed HTTP response."))

(defmethod print-object ((response response) stream)
  (print-unreadable-object (response stream :type t)
    (format stream "~D ~A~@[ ~D bytes~]"
            (response-status response)
            (response-url response)
            (when (response-body response) (length (response-body response))))))

(defmethod response-headers ((response response) &key &allow-other-keys)
  (response-header-list response))

(defmethod response-header ((response response) name &key (index 0)
                            &allow-other-keys)
  "The INDEXth header called NAME, or NIL.  Case-insensitive, as HTTP requires."
  (let ((seen -1))
    (find-if (lambda (header)
               (and (string-equal name (header-name header))
                    (= (incf seen) index)))
             (response-header-list response))))

(defun response-header-values (response name)
  "Every value for NAME, in order.  The right accessor for Set-Cookie."
  (loop for header in (response-header-list response)
        when (string-equal name (header-name header))
          collect (header-value header)))

(defun response-content-type (response)
  "The Content-Type header verbatim, or NIL -- parameters and all.

Use PARSE-CONTENT-TYPE to split off the charset."
  (response-header-value response "content-type"))

(defun successful-response-p (response)
  "True when the status is 2xx.

Only 2xx: a 304 is a useful answer but not a successful one, and treating 3xx
as success would hide an unfollowed redirect."
  (<= 200 (response-status response) 299))

;;; Charset ------------------------------------------------------------------

(defun parse-content-type (content-type)
  "Split a Content-Type into (values media-type charset).

CHARSET is NIL when the header does not name one, which is not the same as
\"assume UTF-8\" -- the caller decides what to do about it."
  (when content-type
    (let* ((parts (uiop:split-string content-type :separator ";"))
           (media (string-downcase (string-trim " " (first parts))))
           (charset nil))
      (dolist (parameter (rest parts))
        (let* ((trimmed (string-trim " " parameter))
               (equals (position #\= trimmed)))
          (when (and equals (string-equal "charset" (subseq trimmed 0 equals)))
            (setf charset (string-trim "\" " (subseq trimmed (1+ equals)))))))
      (values media charset))))

(defparameter *textual-media-types*
  '("application/json" "application/xml" "application/javascript"
    "application/x-www-form-urlencoded" "application/graphql")
  "Media types decoded as text even though they are not text/*.")

(defun textual-media-type-p (media-type)
  (and media-type
       (or (eql 0 (search "text/" media-type))
           (member media-type *textual-media-types* :test #'string-equal)
           ;; The +json and +xml structured-syntax suffixes.
           (let ((plus (position #\+ media-type :from-end t)))
             (and plus (member (subseq media-type (1+ plus)) '("json" "xml")
                               :test #'string-equal))))))

(defun charset-to-encoding (charset)
  "A CFFI external format for CHARSET, or NIL if we do not recognise it."
  (when charset
    (cond ((string-equal charset "utf-8") :utf-8)
          ((string-equal charset "utf8") :utf-8)
          ((string-equal charset "us-ascii") :ascii)
          ((string-equal charset "ascii") :ascii)
          ((string-equal charset "iso-8859-1") :latin-1)
          ((string-equal charset "latin-1") :latin-1)
          ((string-equal charset "utf-16") :utf-16)
          ((string-equal charset "windows-1252") :windows-1252)
          (t nil))))

(defun decode-body (octets content-type &key force-binary force-string)
  "Decode OCTETS according to CONTENT-TYPE.

Returns a string when the type is textual and the charset is one we can
decode, and the octets otherwise.  A body whose charset is declared but
unrecognised comes back as octets rather than being guessed at: silently
mis-decoding is worse than handing back what actually arrived.

Decoding failure is not fatal either -- a body that claims UTF-8 and is not
still gets returned as octets, since the transfer itself succeeded."
  (cond
    (force-binary octets)
    (t (multiple-value-bind (media charset) (parse-content-type content-type)
         (let ((encoding (or (charset-to-encoding charset)
                             ;; No charset given: assume UTF-8 for types that
                             ;; are textual by definition, which is what the
                             ;; relevant RFCs now say for JSON and friends.
                             (when (and (null charset)
                                        (textual-media-type-p media))
                               :utf-8))))
           (cond ((and force-string (null encoding))
                  (handler-case (octets-to-string octets :encoding :utf-8)
                    (error () octets)))
                 ((and encoding (or force-string (textual-media-type-p media)))
                  (handler-case (octets-to-string octets :encoding encoding)
                    (error () octets)))
                 (t octets)))))))

(defun response-text (response &key (encoding :utf-8))
  "The body as a string, decoding octets if it is not one already."
  (let ((body (response-body response)))
    (if (stringp body)
        body
        (octets-to-string body :encoding encoding))))

(defun response-octets (response)
  "The body as octets, encoding a string if it is not octets already."
  (let ((body (response-body response)))
    (if (stringp body) (coerce-to-octets body) body)))

;;; Building one from a finished handle ---------------------------------------

(defparameter *timing-infos*
  '((:total . :total-time-t)
    (:namelookup . :namelookup-time-t)
    (:connect . :connect-time-t)
    (:appconnect . :appconnect-time-t)
    (:pretransfer . :pretransfer-time-t)
    (:starttransfer . :starttransfer-time-t)
    (:redirect . :redirect-time-t))
  "Timings collected into a response, in microseconds.  The _T variants are
used throughout: the older CURLINFO_*_TIME doubles are deprecated.")

(defun collect-timings (handle)
  (loop for (key . info) in *timing-infos*
        for value = (ignore-errors (getinfo handle info))
        when value append (list key value)))

(defun http-version-keyword (value)
  (case value
    (1 :http/1.0) (2 :http/1.1) (3 :http/2) (4 :http/3)
    (t nil)))

(defun make-response-from-handle (handle body &key request-method
                                                   force-binary force-string)
  "Capture everything worth keeping from a finished transfer.

Everything libcurl owns -- headers, info strings -- is copied out here, because
it is only valid until the next transfer on this handle, and the session layer
resets and reuses handles immediately."
  (let* ((headers (response-headers handle))
         (content-type (let ((header (find "content-type" headers
                                           :key #'header-name
                                           :test #'string-equal)))
                         (when header (header-value header)))))
    (make-instance 'response
                   :status (getinfo handle :response-code)
                   :headers headers
                   :body (decode-body body content-type
                                      :force-binary force-binary
                                      :force-string force-string)
                   :url (getinfo handle :effective-url)
                   :version (http-version-keyword
                             (ignore-errors (getinfo handle :http-version)))
                   :request-method request-method
                   :timings (collect-timings handle)
                   :redirect-count (or (ignore-errors
                                        (getinfo handle :redirect-count))
                                       0)
                   :size-download (or (ignore-errors
                                       (getinfo handle :size-download-t))
                                      0)
                   :size-upload (or (ignore-errors
                                     (getinfo handle :size-upload-t))
                                    0))))
