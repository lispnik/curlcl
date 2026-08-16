;;;; src/varargs.lisp — calling libcurl's variadic setters correctly.
;;;;
;;;; curl_easy_setopt, curl_easy_getinfo, curl_multi_setopt and
;;;; curl_share_setopt are all declared `(handle, option, ...)'.  That third
;;;; argument is a genuine variadic argument, and how it is passed is
;;;; ABI-dependent:
;;;;
;;;;   x86-64 SysV  variadic args go in registers, the same place a fixed
;;;;                third argument would go -- so a plain CFFI call happens
;;;;                to work, which is exactly why this bug goes unnoticed.
;;;;   AArch64      the standard procedure call standard does the same, which
;;;;                is why Linux on arm64 is not affected either.
;;;;   Darwin arm64 variadic args go on the STACK.  A plain fixed-signature
;;;;                call puts the value in x2, libcurl reads the stack, and
;;;;                gets whatever was there.  No crash, no error -- setting an
;;;;                option simply does not take, or takes garbage.
;;;;
;;;; The hazard is entirely real and easy to demonstrate: setting
;;;; CURLOPT_MAXFILESIZE_LARGE to -1 through an ordinary CFFI:FOREIGN-FUNCALL
;;;; returns CURLE_OK on an Apple Silicon Mac, where libcurl would answer
;;;; CURLE_BAD_FUNCTION_ARGUMENT had it actually seen the -1.
;;;;
;;;; What handles it is CFFI's FOREIGN-FUNCALL-VARARGS, which says "these
;;;; arguments are fixed and these are variadic" and whose SBCL backend splices
;;;; &optional into the alien signature on Darwin arm64 -- SBCL's way of
;;;; declaring a genuinely variadic alien call, so the compiler emits stack
;;;; passing.  This file used to reach into cffi-libffi and prepare cifs with
;;;; ffi_prep_cif_var by hand, because CFFI had no way to express it; it does
;;;; now, and dropping that removed the binding's only need for a C toolchain
;;;; at build time along with its most version-coupled code.
;;;;
;;;; The functions are called by name rather than through saved pointers, so
;;;; nothing here has to be rebuilt after an image dump: SBCL re-resolves an
;;;; :extern symbol through the linkage table at startup, where a saved
;;;; pointer would refer to a library reopened at a different address.

(in-package #:curlcl)

;;; The typed entry points.  These are what the option and info layers call,
;;; and which of them is used is decided by the option's spelled type -- see
;;; src/options.lisp.

(defun %setopt-pointer (handle option value)
  (cffi:foreign-funcall-varargs "curl_easy_setopt"
                                (:pointer handle :int option)
                                :pointer value :int))

(defun %setopt-long (handle option value)
  (cffi:foreign-funcall-varargs "curl_easy_setopt"
                                (:pointer handle :int option)
                                :long value :int))

(defun %setopt-off-t (handle option value)
  (cffi:foreign-funcall-varargs "curl_easy_setopt"
                                (:pointer handle :int option)
                                :int64 value :int))

(defun %getinfo (handle info out-pointer)
  "Call curl_easy_getinfo(HANDLE, INFO, OUT-POINTER).

The trailing argument is always a pointer to caller-allocated storage.
Allocating storage of the wrong width for the info's type corrupts adjacent
memory -- see the info table for the per-type widths."
  (cffi:foreign-funcall-varargs "curl_easy_getinfo"
                                (:pointer handle :int info)
                                :pointer out-pointer :int))

(defun %multi-setopt-pointer (handle option value)
  (cffi:foreign-funcall-varargs "curl_multi_setopt"
                                (:pointer handle :int option)
                                :pointer value :int))

(defun %multi-setopt-long (handle option value)
  (cffi:foreign-funcall-varargs "curl_multi_setopt"
                                (:pointer handle :int option)
                                :long value :int))

(defun %multi-setopt-off-t (handle option value)
  (cffi:foreign-funcall-varargs "curl_multi_setopt"
                                (:pointer handle :int option)
                                :int64 value :int))

(defun %share-setopt-pointer (handle option value)
  (cffi:foreign-funcall-varargs "curl_share_setopt"
                                (:pointer handle :int option)
                                :pointer value :int))

(defun %share-setopt-long (handle option value)
  (cffi:foreign-funcall-varargs "curl_share_setopt"
                                (:pointer handle :int option)
                                :long value :int))

;;; Proving it at load time.
;;;
;;; The failure this guards against is silent -- every option quietly taking a
;;; wrong value -- so it must not be left to a test suite a downstream user
;;; never runs.  The check that used to live here verified that cffi-libffi's
;;; internals were where we expected; this one verifies the thing that
;;; actually matters, which is that a value handed to a variadic argument
;;; arrives.
;;;
;;; A round trip rather than a rejection: CURLOPT_PRIVATE stores a pointer and
;;; CURLINFO_PRIVATE hands it back, so this asks only that libcurl keep what it
;;; was given.  Asserting instead that some invalid value is refused would tie
;;; the library's ability to load to libcurl's validation of one option, and
;;; break the day that changed.  The two constants are written out because the
;;; option and info tables load after this file.

(defconstant +curlopt-private+ 10103)
(defconstant +curlinfo-private+ #x100015)

(defun %check-variadic-passing ()
  "Signal unless a variadic argument reaches libcurl intact."
  (let ((handle (cffi:foreign-funcall "curl_easy_init" :pointer)))
    (when (cffi:null-pointer-p handle)
      (error 'curl-error
             :message "curl_easy_init returned NULL while checking variadic ~
argument passing"))
    (unwind-protect
         (let ((token (cffi:make-pointer #x5EEDC0DE)))
           (%setopt-pointer handle +curlopt-private+ token)
           (cffi:with-foreign-object (out :pointer)
             (setf (cffi:mem-ref out :pointer) (cffi:null-pointer))
             (%getinfo handle +curlinfo-private+ out)
             (let ((returned (cffi:mem-ref out :pointer)))
               (unless (cffi:pointer-eq returned token)
                 (error "Variadic arguments are not reaching libcurl: stored ~
~A in CURLOPT_PRIVATE and read back ~A.  Every option set through this binding ~
would take a wrong value silently.  This means CFFI's FOREIGN-FUNCALL-VARARGS ~
is not making a true variadic call on this platform -- most likely an SBCL too ~
old to accept &optional in an alien signature.  See src/varargs.lisp."
                        token returned)))))
      (cffi:foreign-funcall "curl_easy_cleanup" :pointer handle :void))))

(%check-variadic-passing)
