;;;; src/easy-raw.lisp — raw bindings for the easy interface.
;;;;
;;;; The unadorned foreign layer: DEFCFUNs, libcurl's own option introspection,
;;;; and getinfo accessors that take numeric identifiers.  It loads before the
;;;; option tables because they are validated against the introspection
;;;; functions here; the keyword-driven API and the EASY-HANDLE class are built
;;;; on top of both, in src/easy.lisp.
;;;;
;;;; Note what is *not* here: curl_easy_setopt and curl_easy_getinfo are
;;;; variadic and cannot be declared with DEFCFUN at all.  They are reached
;;;; through src/varargs.lisp.

(in-package #:curlcl)

;;; Raw bindings --------------------------------------------------------------

(cffi:defcfun ("curl_easy_init" %curl-easy-init) :pointer)
(cffi:defcfun ("curl_easy_cleanup" %curl-easy-cleanup) :void (handle :pointer))
(cffi:defcfun ("curl_easy_perform" %curl-easy-perform) :int (handle :pointer))
(cffi:defcfun ("curl_easy_duphandle" %curl-easy-duphandle) :pointer (handle :pointer))
(cffi:defcfun ("curl_easy_reset" %curl-easy-reset) :void (handle :pointer))
(cffi:defcfun ("curl_easy_pause" %curl-easy-pause) :int
  (handle :pointer) (bitmask :int))
(cffi:defcfun ("curl_easy_upkeep" %curl-easy-upkeep) :int (handle :pointer))

(cffi:defcfun ("curl_easy_recv" %curl-easy-recv) :int
  (handle :pointer) (buffer :pointer) (buflen :size) (n :pointer))
(cffi:defcfun ("curl_easy_send" %curl-easy-send) :int
  (handle :pointer) (buffer :pointer) (buflen :size) (n :pointer))

(cffi:defcfun ("curl_easy_escape" %curl-easy-escape) :pointer
  (handle :pointer) (string :pointer) (length :int))
(cffi:defcfun ("curl_easy_unescape" %curl-easy-unescape) :pointer
  (handle :pointer) (string :pointer) (inlength :int) (outlength :pointer))

;;; libcurl allocates with its own allocator, so anything it hands back that
;;; the caller owns has to go back through curl_free, not the C library's free.
(cffi:defcfun ("curl_free" %curl-free) :void (pointer :pointer))

(cffi:defcfun ("curl_slist_append" %curl-slist-append) :pointer
  (list :pointer) (data :string))
(cffi:defcfun ("curl_slist_free_all" %curl-slist-free-all) :void (list :pointer))

;;; Declared here rather than beside the rest of the mime API because
;;; src/memory.lisp releases mime handles as part of a handle's resource sweep,
;;; and it loads first.
(cffi:defcfun ("curl_mime_free" %curl-mime-free) :void (mime :pointer))

;;; Option introspection ------------------------------------------------------
;;;
;;; curl_easy_option_* lets the *loaded* libcurl describe its own options, which
;;; is how the binding tells "this option does not exist in this build" apart
;;; from "you misspelled it", and how the test suite validates the generated
;;; table against reality rather than against the headers it was made from.

(cffi:defcenum curl-easytype
  (:long 0) (:values 1) (:off-t 2) (:object 3) (:string 4)
  (:slist 5) (:cbptr 6) (:blob 7) (:function 8))

(cffi:defcstruct curl-easyoption
  (name :pointer)
  (id :int)
  (type curl-easytype)
  (flags :unsigned-int))

(cffi:defcfun ("curl_easy_option_by_name" %curl-easy-option-by-name) :pointer
  (name :string))
(cffi:defcfun ("curl_easy_option_by_id" %curl-easy-option-by-id) :pointer
  (id :int))
(cffi:defcfun ("curl_easy_option_next" %curl-easy-option-next) :pointer
  (previous :pointer))

(defun known-option (c-name)
  "Describe C-NAME as the loaded libcurl knows it, or NIL if it has no such option.

Returns (values id type alias-p).  C-NAME is the C spelling without the
CURLOPT_ prefix, as curl_easy_option_by_name expects -- e.g. \"URL\"."
  (let ((p (%curl-easy-option-by-name c-name)))
    (unless (cffi:null-pointer-p p)
      (values (cffi:foreign-slot-value p '(:struct curl-easyoption) 'id)
              (cffi:foreign-slot-value p '(:struct curl-easyoption) 'type)
              (logbitp 0 (cffi:foreign-slot-value
                          p '(:struct curl-easyoption) 'flags))))))

(defun map-known-options (function)
  "Call FUNCTION with (name id type alias-p) for every option the loaded libcurl has."
  (loop for p = (%curl-easy-option-next (cffi:null-pointer))
          then (%curl-easy-option-next p)
        until (cffi:null-pointer-p p)
        do (funcall function
                    (cffi:foreign-string-to-lisp
                     (cffi:foreign-slot-value p '(:struct curl-easyoption) 'name))
                    (cffi:foreign-slot-value p '(:struct curl-easyoption) 'id)
                    (cffi:foreign-slot-value p '(:struct curl-easyoption) 'type)
                    (logbitp 0 (cffi:foreign-slot-value
                                p '(:struct curl-easyoption) 'flags)))))

;;; Low-level handle helpers --------------------------------------------------
;;;
;;; These take numeric option and info identifiers.  They exist so the ABI
;;; layer is testable before the generated option table lands on top of it, and
;;; so the typed layer has one place to funnel through.

(defmacro with-raw-easy ((var) &body body)
  "Run BODY with VAR bound to a bare CURL*, cleaned up on exit."
  `(let ((,var (%curl-easy-init)))
     (when (cffi:null-pointer-p ,var)
       (error 'easy-error :message "curl_easy_init returned NULL"))
     (unwind-protect (progn ,@body)
       (%curl-easy-cleanup ,var))))

(defun %check-easy (code &key url detail)
  "Signal an EASY-ERROR unless CODE is CURLE_OK.  Returns CODE."
  (unless (zerop code)
    (error 'easy-error
           :code code
           :code-name (curlcode-keyword code)
           :message (%curl-easy-strerror code)
           :detail detail
           :url url))
  code)

(defun %raw-setopt-string (handle option string)
  "Set a string option.  libcurl copies CURLOPT_*STRINGPOINT values, so the
temporary foreign string is safe here -- but note that CURLOPT_POSTFIELDS does
NOT copy and must not use this path."
  (cffi:with-foreign-string (c-string string)
    (%setopt-pointer handle option c-string)))

(defun %raw-getinfo-long (handle info)
  "Read a CURLINFO_LONG.  The out-parameter is a C `long' -- 8 bytes on LP64.
Allocating 4 would corrupt the adjacent word."
  (cffi:with-foreign-object (out :long)
    (setf (cffi:mem-ref out :long) 0)
    (let ((code (%getinfo handle info out)))
      (values (cffi:mem-ref out :long) code))))

(defun %raw-getinfo-off-t (handle info)
  ;; CURL-OFF-T is a named CFFI type, so it has to be quoted here; the keyword
  ;; types elsewhere in this file are self-evaluating and do not.
  (cffi:with-foreign-object (out 'curl-off-t)
    (setf (cffi:mem-ref out 'curl-off-t) 0)
    (let ((code (%getinfo handle info out)))
      (values (cffi:mem-ref out 'curl-off-t) code))))

(defun %raw-getinfo-double (handle info)
  (cffi:with-foreign-object (out :double)
    (setf (cffi:mem-ref out :double) 0d0)
    (let ((code (%getinfo handle info out)))
      (values (cffi:mem-ref out :double) code))))

(defun %raw-getinfo-string (handle info)
  "Read a CURLINFO_STRING.  The string belongs to libcurl and must not be freed;
it is only valid until the next perform or cleanup, so it is copied out here."
  (cffi:with-foreign-object (out :pointer)
    (setf (cffi:mem-ref out :pointer) (cffi:null-pointer))
    (let ((code (%getinfo handle info out)))
      (let ((p (cffi:mem-ref out :pointer)))
        (values (unless (cffi:null-pointer-p p) (cffi:foreign-string-to-lisp p))
                code)))))

(defun %raw-getinfo-pointer (handle info)
  (cffi:with-foreign-object (out :pointer)
    (setf (cffi:mem-ref out :pointer) (cffi:null-pointer))
    (let ((code (%getinfo handle info out)))
      (values (cffi:mem-ref out :pointer) code))))
