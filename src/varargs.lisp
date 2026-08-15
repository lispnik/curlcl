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
;;;;   Darwin arm64 variadic args go on the STACK.  A plain CFFI call puts
;;;;                the value in x2, libcurl reads the stack, and gets
;;;;                whatever was there.  No crash, no error -- setting a URL
;;;;                simply does not take, or takes garbage.
;;;;
;;;; So every one of these calls goes through libffi's ffi_prep_cif_var, which
;;;; is the only portable way to say "two fixed arguments, one variadic".  CFFI
;;;; itself cannot express this: DEFCFUN has no variadic support, and
;;;; FOREIGN-FUNCALL builds a fixed signature.
;;;;
;;;; The machinery is reused from cffi-libffi rather than re-groveled: it
;;;; already loads libffi, knows the ffi_cif layout, and can build ffi_type
;;;; descriptors.  Those are internal symbols, which makes this file the most
;;;; version-coupled part of the library -- hence %CHECK-LIBFFI-BINDINGS below,
;;;; so a CFFI upgrade that moves them fails loudly at load time instead of
;;;; silently mis-passing arguments.
;;;;
;;;; Only three trailing argument types exist across the whole API (a pointer,
;;;; a long, or a curl_off_t), and getinfo's out-parameter is always a pointer,
;;;; so three cifs are prepared once and shared.  A cif is read-only during
;;;; ffi_call, so sharing them across threads is safe.

(in-package #:curlcl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %check-libffi-bindings ()
    "Verify the cffi-libffi internals this file depends on still exist."
    (let ((missing
            (remove-if (lambda (spec)
                         (destructuring-bind (kind symbol) spec
                           (ecase kind
                             (:function (fboundp symbol))
                             (:type (ignore-errors
                                     (cffi::parse-type symbol) t)))))
                       '((:function cffi::libffi/call)
                         (:function cffi::make-libffi-type-descriptor)
                         (:function cffi::parse-type)
                         (:type (:struct cffi::ffi-cif))))))
      (when missing
        (error "This libcurl binding calls variadic libcurl functions through ~
cffi-libffi's internals, and the following are missing from the CFFI in use: ~
~{~S~^, ~}.  See src/varargs.lisp." missing))))
  (%check-libffi-bindings))

;;; ffi_prep_cif_var is the one piece cffi-libffi does not surface: it binds
;;; ffi_prep_cif (all-fixed) and ffi_call, but not the variadic preparer.
(cffi:defcfun ("ffi_prep_cif_var" %ffi-prep-cif-var) cffi::status
  (cif :pointer)
  (abi cffi::abi)
  (nfixed :uint)
  (ntotal :uint)
  (rtype :pointer)
  (atypes :pointer))

(defun %ffi-type (type)
  "The static ffi_type* describing TYPE.  Built-ins return libffi's globals."
  (cffi::make-libffi-type-descriptor (cffi::parse-type type)))

(defun %prepare-variadic-cif (arg-type)
  "Prepare a cif for `int f(void *, int, ARG-TYPE)' with the last arg variadic.

The cif and its type array are allocated for the lifetime of the image on
purpose: there are exactly three of them and they are shared by every option
call the process ever makes."
  (let ((cif (cffi:foreign-alloc '(:struct cffi::ffi-cif)))
        (atypes (cffi:foreign-alloc :pointer :count 3)))
    (setf (cffi:mem-aref atypes :pointer 0) (%ffi-type :pointer)   ; the handle
          (cffi:mem-aref atypes :pointer 1) (%ffi-type :int)       ; the option
          (cffi:mem-aref atypes :pointer 2) (%ffi-type arg-type))  ; variadic
    (let ((status (%ffi-prep-cif-var cif :default-abi
                                     2 3          ; 2 fixed, 3 total
                                     (%ffi-type :int) atypes)))
      (unless (eq status :ok)
        (error "ffi_prep_cif_var failed for trailing type ~S: ~S" arg-type status)))
    cif))

(defvar *variadic-cifs* (make-hash-table :test 'eq)
  "Trailing argument type -> prepared ffi_cif.  Populated once at load time.")

(defun %variadic-cif (arg-type)
  (or (gethash arg-type *variadic-cifs*)
      (error "No prepared cif for trailing argument type ~S." arg-type)))

(defun %initialize-variadic-cifs ()
  (clrhash *variadic-cifs*)
  (dolist (type '(:pointer :long :int64))
    (setf (gethash type *variadic-cifs*) (%prepare-variadic-cif type))))

(defvar *setopt-function* nil)
(defvar *getinfo-function* nil)
(defvar *multi-setopt-function* nil)
(defvar *share-setopt-function* nil)

(defun %initialize-variadic-functions ()
  (setf *setopt-function* (cffi:foreign-symbol-pointer "curl_easy_setopt")
        *getinfo-function* (cffi:foreign-symbol-pointer "curl_easy_getinfo")
        *multi-setopt-function* (cffi:foreign-symbol-pointer "curl_multi_setopt")
        *share-setopt-function* (cffi:foreign-symbol-pointer "curl_share_setopt"))
  (dolist (entry (list (cons "curl_easy_setopt" *setopt-function*)
                       (cons "curl_easy_getinfo" *getinfo-function*)
                       (cons "curl_multi_setopt" *multi-setopt-function*)
                       (cons "curl_share_setopt" *share-setopt-function*)))
    (when (or (null (cdr entry)) (cffi:null-pointer-p (cdr entry)))
      (error "Could not resolve ~A in the loaded libcurl." (car entry)))))

(defun %call-variadic (function handle option value arg-type)
  "Call FUNCTION(HANDLE, OPTION, VALUE) with VALUE passed variadically.

ARG-TYPE is the CFFI type of the trailing argument and selects the prepared
cif.  Returns the int result, which the caller decodes as the appropriate
CURL*code."
  (let ((cif (%variadic-cif arg-type)))
    ;; One 8-byte slot serves all three trailing types on every platform we
    ;; support, so the value buffer does not have to vary with ARG-TYPE; the
    ;; MEM-REF below writes it at its natural width.  RVALUE is 8 bytes because
    ;; libffi widens integer returns to ffi_arg.
    (cffi:with-foreign-objects ((avalues :pointer 3)
                                (rvalue :int64)
                                (a-handle :pointer)
                                (a-option :int)
                                (a-value :int64))
      (setf (cffi:mem-ref a-handle :pointer) handle
            (cffi:mem-ref a-option :int) option
            (cffi:mem-ref a-value arg-type) value
            (cffi:mem-aref avalues :pointer 0) a-handle
            (cffi:mem-aref avalues :pointer 1) a-option
            (cffi:mem-aref avalues :pointer 2) a-value)
      (cffi::libffi/call cif function rvalue avalues)
      (cffi:mem-ref rvalue :int))))

;;; The typed entry points.  Everything above is private; these are what the
;;; option and info layers call.

(defun %setopt-pointer (handle option value)
  (%call-variadic *setopt-function* handle option value :pointer))

(defun %setopt-long (handle option value)
  (%call-variadic *setopt-function* handle option value :long))

(defun %setopt-off-t (handle option value)
  (%call-variadic *setopt-function* handle option value :int64))

(defun %getinfo (handle info out-pointer)
  "Call curl_easy_getinfo(HANDLE, INFO, OUT-POINTER).

The trailing argument is always a pointer to caller-allocated storage, so this
shares the :POINTER cif.  Allocating storage of the wrong width for the info's
type corrupts adjacent memory -- see the info table for the per-type widths."
  (%call-variadic *getinfo-function* handle info out-pointer :pointer))

(defun %multi-setopt-pointer (handle option value)
  (%call-variadic *multi-setopt-function* handle option value :pointer))

(defun %multi-setopt-long (handle option value)
  (%call-variadic *multi-setopt-function* handle option value :long))

(defun %multi-setopt-off-t (handle option value)
  (%call-variadic *multi-setopt-function* handle option value :int64))

(defun %share-setopt-pointer (handle option value)
  (%call-variadic *share-setopt-function* handle option value :pointer))

(defun %share-setopt-long (handle option value)
  (%call-variadic *share-setopt-function* handle option value :long))

;;; Prepared state does not survive an image dump: the cifs point at foreign
;;; memory and the function pointers at a library that will be reopened at a
;;; different address.
(defun %reinitialize-varargs ()
  (%initialize-variadic-cifs)
  (%initialize-variadic-functions))

(uiop:register-image-restore-hook '%reinitialize-varargs nil)

(%reinitialize-varargs)
