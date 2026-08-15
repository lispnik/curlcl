;;;; src/url.lisp — the URL API.
;;;;
;;;; libcurl's own URL parser, which is worth binding rather than reaching for
;;;; a Lisp one: it is the parser libcurl will actually use for the transfer,
;;;; so a URL it accepts here is one it accepts there, quirks included.
;;;;
;;;; The memory rule is unusual enough to be worth stating: curl_url_get hands
;;;; back a string the caller must release with curl_free -- not free, and not
;;;; curl_slist_free_all -- and curl_url_cleanup does *not* release strings
;;;; previously returned.  Every accessor here copies into Lisp and frees
;;;; immediately, so no caller ever holds one.
;;;;
;;;; A missing component is not an error.  curl_url_get answers CURLUE_NO_HOST,
;;;; CURLUE_NO_PORT and friends for "there isn't one", which is an ordinary
;;;; answer about an ordinary URL, so those come back as NIL.  Malformed input
;;;; still signals.

(in-package #:libcurl)

(cffi:defcfun ("curl_url" %curl-url) :pointer)
(cffi:defcfun ("curl_url_cleanup" %curl-url-cleanup) :void (handle :pointer))
(cffi:defcfun ("curl_url_dup" %curl-url-dup) :pointer (handle :pointer))
(cffi:defcfun ("curl_url_get" %curl-url-get) :int
  (handle :pointer) (part :int) (out :pointer) (flags :unsigned-int))
(cffi:defcfun ("curl_url_set" %curl-url-set) :int
  (handle :pointer) (part :int) (content :pointer) (flags :unsigned-int))

(defparameter *url-flags*
  '((:default-port . #.(ash 1 0))
    (:no-default-port . #.(ash 1 1))
    (:default-scheme . #.(ash 1 2))
    (:non-support-scheme . #.(ash 1 3))
    (:path-as-is . #.(ash 1 4))
    (:disallow-user . #.(ash 1 5))
    (:urldecode . #.(ash 1 6))
    (:urlencode . #.(ash 1 7))
    (:append-query . #.(ash 1 8))
    (:guess-scheme . #.(ash 1 9))
    (:no-authority . #.(ash 1 10))
    (:allow-space . #.(ash 1 11))
    (:punycode . #.(ash 1 12))
    (:puny2idn . #.(ash 1 13))
    ;; 8.8.0 and later; harmless to pass to an older libcurl, which ignores
    ;; bits it does not know.
    (:get-empty . #.(ash 1 14))
    (:no-guess-scheme . #.(ash 1 15))))

(defun url-flags-value (flags)
  (let ((value 0))
    (dolist (flag (alexandria:ensure-list flags) value)
      (let ((bit (cdr (assoc flag *url-flags*))))
        (unless bit
          (error 'curl-error :message (format nil "Unknown URL flag ~S." flag)))
        (setf value (logior value bit))))))

(defclass url ()
  ((pointer :initarg :pointer :reader url-pointer)
   (closed-p :accessor url-closed-p :initform nil))
  (:documentation "A CURLU*, libcurl's parsed URL."))

(defmethod print-object ((url url) stream)
  (print-unreadable-object (url stream :type t)
    (if (url-closed-p url)
        (write-string "closed" stream)
        (format stream "~S" (ignore-errors (url-string url))))))

(defun %check-url (code &key url)
  (unless (zerop code)
    (error 'url-error
           :code code
           :code-name (curlcode-keyword code 'curlucode)
           :message (%curl-url-strerror code)
           :url url))
  code)

(defun make-url (&optional string &rest flags)
  "Parse STRING, or make an empty URL when it is omitted."
  (let ((pointer (%curl-url)))
    (when (cffi:null-pointer-p pointer)
      (error 'url-error :message "curl_url returned NULL"))
    (let ((url (make-instance 'url :pointer pointer))
          (completed nil))
      (unwind-protect
           (progn (when string (setf (url-part url :url) (values string flags)))
                  (setf completed t)
                  url)
        (unless completed (close-url url))))))

(defun close-url (url)
  "Release URL.  Idempotent."
  (unless (url-closed-p url)
    (setf (url-closed-p url) t)
    (%curl-url-cleanup (url-pointer url)))
  (values))

(defmacro with-url ((var &optional string) &body body)
  "Run BODY with VAR bound to a parsed URL, released on exit."
  `(let ((,var (make-url ,@(when string (list string)))))
     (unwind-protect (progn ,@body)
       (close-url ,var))))

(defun duplicate-url (url)
  (let ((pointer (%curl-url-dup (url-pointer url))))
    (when (cffi:null-pointer-p pointer)
      (error 'url-error :message "curl_url_dup returned NULL"))
    (make-instance 'url :pointer pointer)))

(defconstant +curlue-no-part-first+ 10
  "CURLUE_NO_SCHEME.  Codes from here through CURLUE_NO_ZONEID all mean
\"this URL has no such component\", which is an answer rather than a failure.")
(defconstant +curlue-no-part-last+ 18) ; CURLUE_NO_ZONEID

(defun url-part (url part &rest flags)
  "The named component of URL, or NIL when it has none.

PART is :URL, :SCHEME, :USER, :PASSWORD, :OPTIONS, :HOST, :PORT, :PATH,
:QUERY, :FRAGMENT or :ZONEID.  FLAGS are keywords from *URL-FLAGS*, most
usefully :URLDECODE."
  (check-type part symbol)
  (cffi:with-foreign-object (out :pointer)
    (setf (cffi:mem-ref out :pointer) (cffi:null-pointer))
    (let ((code (%curl-url-get (url-pointer url)
                               (curlcode-value part 'curl-upart)
                               out
                               (url-flags-value flags))))
      (cond
        ;; "No such component" is a fact about the URL, not an error.
        ((<= +curlue-no-part-first+ code +curlue-no-part-last+) nil)
        (t (%check-url code)
           (let ((pointer (cffi:mem-ref out :pointer)))
             (unless (cffi:null-pointer-p pointer)
               ;; libcurl allocated this and curl_url_cleanup will not release
               ;; it, so it is copied and freed here rather than handed out.
               (unwind-protect (cffi:foreign-string-to-lisp pointer)
                 (%curl-free pointer)))))))))

(defun (setf url-part) (value url part &rest flags)
  "Set a component of URL.  A NIL value removes it."
  (check-type part symbol)
  ;; MAKE-URL passes (values string flags) through here, so a list value for
  ;; the whole-URL case carries its own flags.
  (multiple-value-bind (content extra-flags)
      (if (and (consp value) (eq part :url))
          (values (first value) (rest value))
          (values value nil))
    (let ((all-flags (append (alexandria:ensure-list flags)
                             (alexandria:ensure-list extra-flags))))
      (if (null content)
          (%check-url (%curl-url-set (url-pointer url)
                                     (curlcode-value part 'curl-upart)
                                     (cffi:null-pointer)
                                     (url-flags-value all-flags)))
          (cffi:with-foreign-string (c-string (string content))
            (%check-url (%curl-url-set (url-pointer url)
                                       (curlcode-value part 'curl-upart)
                                       c-string
                                       (url-flags-value all-flags))
                        :url (string content))))))
  value)

(defun url-string (url &rest flags)
  "The whole URL as a string."
  (apply #'url-part url :url flags))

(defun parse-url (string &rest flags)
  "Parse STRING and return its components as a plist.

Convenience over WITH-URL for the common case of wanting the pieces rather
than a handle to keep."
  (with-url (url)
    (setf (url-part url :url) (cons string flags))
    (loop for part in '(:scheme :user :password :options :host :port :path
                        :query :fragment :zoneid)
          for value = (url-part url part)
          when value append (list part value))))

(defun url-join (base relative)
  "Resolve RELATIVE against BASE, the way a redirect would be resolved.

This is libcurl's own relative-URL resolution: setting :URL on a handle that
already holds one is exactly what it does when following a Location header."
  (with-url (url base)
    (setf (url-part url :url) relative)
    (url-string url)))
