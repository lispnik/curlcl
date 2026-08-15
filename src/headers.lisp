;;;; src/headers.lisp — the header API.
;;;;
;;;; curl_easy_header lets libcurl do the header parsing, which is worth having
;;;; over scraping the header callback: it folds continuation lines, knows
;;;; which headers came from which stage of a redirect chain, and distinguishes
;;;; trailers, CONNECT headers and 1xx headers from the real response.
;;;;
;;;; Two things to be careful with.  The struct libcurl returns belongs to
;;;; libcurl and is invalidated by the next transfer or by cleanup, so
;;;; everything is copied into Lisp before returning.  And this is the one code
;;;; family with no strerror function in libcurl at all, so the messages come
;;;; from the table in types.lisp.

(in-package #:libcurl)

(cffi:defcfun ("curl_easy_header" %curl-easy-header) :int
  (handle :pointer) (name :string) (index :size) (origin :unsigned-int)
  (request :int) (out :pointer))

(cffi:defcfun ("curl_easy_nextheader" %curl-easy-nextheader) :pointer
  (handle :pointer) (origin :unsigned-int) (request :int) (previous :pointer))

(defstruct (http-header (:conc-name header-))
  "One response header, copied out of libcurl's own parse."
  (name "" :type string)
  (value "" :type string)
  ;; How many headers share this name, and which of them this is.  Both matter
  ;; for Set-Cookie, which legitimately repeats.
  (amount 1 :type integer)
  (index 0 :type integer)
  (origin '() :type list))

(defparameter *header-origins*
  '((:header . #.(ash 1 0))
    (:trailer . #.(ash 1 1))
    (:connect . #.(ash 1 2))
    (:1xx . #.(ash 1 3))
    (:pseudo . #.(ash 1 4))))

(defun header-origin-value (origins)
  (let ((value 0))
    (dolist (origin (alexandria:ensure-list origins) value)
      (let ((bit (cdr (assoc origin *header-origins*))))
        (unless bit
          (error 'header-error
                 :message (format nil "Unknown header origin ~S." origin)))
        (setf value (logior value bit))))))

(defun header-origin-list (value)
  (loop for (name . bit) in *header-origins*
        when (logtest value bit) collect name))

(defun %check-header (code)
  (unless (zerop code)
    (error 'header-error
           :code code
           :code-name (curlcode-keyword code 'curlhcode)
           ;; libcurl has no curl_header_strerror, so this table is ours.
           :message (or (cdr (assoc (curlcode-keyword code 'curlhcode)
                                    *header-code-messages*))
                        "Unknown header error")))
  code)

(defun %decode-header (pointer)
  (make-http-header
   :name (cffi:foreign-string-to-lisp
          (cffi:foreign-slot-value pointer '(:struct curl-header) 'name))
   :value (cffi:foreign-string-to-lisp
           (cffi:foreign-slot-value pointer '(:struct curl-header) 'value))
   :amount (cffi:foreign-slot-value pointer '(:struct curl-header) 'amount)
   :index (cffi:foreign-slot-value pointer '(:struct curl-header) 'index)
   :origin (header-origin-list
            (cffi:foreign-slot-value pointer '(:struct curl-header) 'origin))))

(defconstant +curlhe-missing+ 2)
(defconstant +curlhe-noheaders+ 3)
(defconstant +curlhe-badindex+ 1)

(defun response-header (handle name &key (index 0) (origin :header) (request -1))
  "The response header called NAME, or NIL if there is none.

INDEX selects among headers repeating that name.  REQUEST is which request in
a redirect chain to ask about: -1 is the last, 0 the first.  Comparison is
case-insensitive, as HTTP requires."
  (check-open handle)
  (cffi:with-foreign-object (out :pointer)
    (setf (cffi:mem-ref out :pointer) (cffi:null-pointer))
    (let ((code (%curl-easy-header (handle-pointer handle) name index
                                   (header-origin-value origin) request out)))
      (cond
        ;; All three mean "not present", which is an answer about the response
        ;; rather than a failure to ask.
        ((member code (list +curlhe-missing+ +curlhe-noheaders+ +curlhe-badindex+))
         nil)
        (t (%check-header code)
           (let ((pointer (cffi:mem-ref out :pointer)))
             (unless (cffi:null-pointer-p pointer)
               ;; Owned by libcurl and invalidated by the next perform, so it
               ;; is copied out rather than returned.
               (%decode-header pointer))))))))

(defun response-header-value (handle name &key (index 0) (origin :header)
                                               (request -1))
  "The value of a response header, or NIL."
  (let ((header (response-header handle name :index index :origin origin
                                             :request request)))
    (when header (header-value header))))

(defun response-headers (handle &key (origin :header) (request -1))
  "Every response header, in order, as a list of HTTP-HEADER structs.

Duplicates are preserved: Set-Cookie appearing three times yields three
entries, which is the only representation that can be right."
  (check-open handle)
  (let ((origin-bits (header-origin-value origin)))
    (loop with previous = (cffi:null-pointer)
          for pointer = (%curl-easy-nextheader (handle-pointer handle)
                                               origin-bits request previous)
          until (cffi:null-pointer-p pointer)
          collect (%decode-header pointer)
          do (setf previous pointer))))
