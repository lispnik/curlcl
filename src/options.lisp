;;;; src/options.lisp — the option table and how a keyword becomes a setopt call.
;;;;
;;;; libcurl encodes an option's argument type in its numeric identifier:
;;;; the value is a base plus a small ordinal, where the base is 0 for long,
;;;; 10000 for pointer-ish, 20000 for function pointers, 30000 for curl_off_t
;;;; and 40000 for blobs.  But only five *numeric* bases exist while nine type
;;;; names are spelled in the header, and the distinctions that collapse --
;;;; string vs. slist vs. blob-pointer vs. opaque callback data, all 10000 --
;;;; are exactly the ones a binding needs, because they decide who owns the
;;;; memory and how long it has to live.  So the table is generated from the
;;;; header's *spelled* type rather than derived from the number, and the
;;;; spelled type is what drives dispatch.
;;;;
;;;; The data lives in src/options-table.lisp, which is generated; this file is
;;;; the machinery around it.
;;;;
;;;; Availability is a separate question from existence.  This binding may be
;;;; loaded against a libcurl older than the headers the table was generated
;;;; from, so an option in the table is not necessarily an option the running
;;;; library has.  Rather than let libcurl answer that with a bare
;;;; CURLE_UNKNOWN_OPTION, each option is checked once against
;;;; curl_easy_option_by_name and the result cached on the entry.

(in-package #:curlcl)

(defstruct (curl-option (:conc-name option-))
  "One entry of libcurl's option table."
  (keyword nil :type symbol)
  (id 0 :type integer)
  ;; One of :LONG :VALUES :STRINGPOINT :OBJECTPOINT :CBPOINT :SLISTPOINT
  ;; :FUNCTIONPOINT :OFF-T :BLOB -- the type as spelled in the header.
  (kind nil :type symbol)
  (c-name "" :type string)
  (deprecated nil)
  (replacement nil)
  (alias-of nil)
  ;; :UNKNOWN until the loaded libcurl has been asked; then T or NIL.
  (availability :unknown)
  ;; Whether the deprecation warning has already been issued.  Its own slot
  ;; rather than a sentinel in AVAILABILITY, which means something else.
  (warned nil))

;;; DEFSTRUCT has nowhere to put a docstring for the accessors it defines.
(setf (documentation 'option-id 'function)
      "The integer CURLOPT_* value, which is its type's base plus its ordinal."
      (documentation 'option-kind 'function)
      "The option's type as spelled in curl.h: :LONG, :VALUES, :STRINGPOINT,
:OBJECTPOINT, :CBPOINT, :SLISTPOINT, :FUNCTIONPOINT, :OFF-T or :BLOB.

Nine names over five numeric bases, and the spelling is what matters: the base
cannot tell a string from an slist from a blob, and they differ in who owns the
memory."
      (documentation 'option-deprecated 'function)
      "The libcurl version that deprecated this option, or NIL.

Deprecation is warned about when the option is used, not when this table is
loaded -- a warning at load time would fire for options the program never
touches.")

(defvar *options* (make-hash-table :test 'eq)
  "Option keyword -> CURL-OPTION.")

(defvar *options-by-id* (make-hash-table :test 'eql)
  "Option id -> CURL-OPTION.  Aliases lose to their canonical name: several
distinct names share one id (CURLOPT_SSLKEYPASSWD, CURLOPT_SSLCERTPASSWD and
CURLOPT_KEYPASSWD are all 10026, and both CURLOPT_OBSOLETE40 and
CURLOPT_OBSOLETE72 are literally 9999), so this map is lossy by construction
and is only for diagnostics.")

(defun register-options (entries)
  "Populate the option table from generated data.  Called by options-table.lisp."
  (clrhash *options*)
  (clrhash *options-by-id*)
  (dolist (entry entries)
    (destructuring-bind (keyword id kind c-name deprecated replacement alias-of)
        entry
      (let ((option (make-curl-option :keyword keyword :id id :kind kind
                                      :c-name c-name :deprecated deprecated
                                      :replacement replacement
                                      :alias-of alias-of)))
        (setf (gethash keyword *options*) option)
        ;; Canonical names win; an alias never displaces one.
        (unless (and (gethash id *options-by-id*) alias-of)
          (setf (gethash id *options-by-id*) option)))))
  (hash-table-count *options*))

(defun find-option (keyword)
  "The CURL-OPTION named KEYWORD, or NIL."
  (gethash keyword *options*))

(defun option-supported-p (option)
  "True when the loaded libcurl has OPTION.  Asked once, then cached.

The table is generated from whatever headers were installed when it was made,
which may be newer than the library actually loaded -- so membership in the
table is not availability."
  (let ((cached (option-availability option)))
    (if (eq cached :unknown)
        (setf (option-availability option)
              ;; curl_easy_option_by_name wants the name without the prefix.
              (and (known-option (subseq (option-c-name option)
                                         (length "CURLOPT_")))
                   t))
        cached)))

(defun ensure-option (keyword)
  "Return the CURL-OPTION for KEYWORD, signalling if it is unusable.

Signals UNSUPPORTED-OPTION both for a name this binding has never heard of and
for one the running libcurl lacks -- the report distinguishes them."
  (let ((option (find-option keyword)))
    (cond ((null option)
           (error 'unsupported-option
                  :name keyword
                  :running-version (ignore-errors (libcurl-version))
                  :message "No such libcurl option is known to this binding."))
          ((not (option-supported-p option))
           (error 'unsupported-option
                  :name keyword
                  :running-version (ignore-errors (libcurl-version))
                  :message "The loaded libcurl does not provide this option."))
          (t option))))

(defun warn-if-deprecated (option)
  "Warn the first time a deprecated option is used.

Deliberately at use rather than at load: the table always contains every
deprecated option, and warning about their mere existence would be noise.

The latch is set before warning rather than after, so a handler that declines
to return -- MUFFLE-WARNING, or one that turns the warning into an error --
still leaves the option marked and cannot cause it to warn twice."
  (when (and (option-deprecated option)
             (not (option-warned option)))
    (setf (option-warned option) t)
    (warn 'deprecated-option
          :name (option-keyword option)
          :since (option-deprecated option)
          :replacement (option-replacement option))))

(defun option-argument-class (option)
  "How OPTION's value crosses the ABI boundary: :LONG, :OFF-T or :POINTER.

Collapsing nine spelled types onto three is safe *here* because this only
selects the libffi cif.  Ownership and conversion are decided from the spelled
KIND by the caller, before it gets this far."
  (ecase (option-kind option)
    ((:long :values) :long)
    ((:off-t) :off-t)
    ((:stringpoint :objectpoint :cbpoint :slistpoint :functionpoint :blob)
     :pointer)))

(defun map-options (function)
  "Call FUNCTION with each CURL-OPTION in the table."
  (maphash (lambda (key option) (declare (ignore key)) (funcall function option))
           *options*))

(defun option-count ()
  (hash-table-count *options*))

;;; Multi options -------------------------------------------------------------
;;;
;;; CURLMOPT_* uses the very same CURLOPT() macro and the same base offsets, so
;;; it gets the same treatment in a separate table.

(defvar *multi-options* (make-hash-table :test 'eq)
  "Multi option keyword -> CURL-OPTION.")

(defun register-multi-options (entries)
  (clrhash *multi-options*)
  (dolist (entry entries)
    (destructuring-bind (keyword id kind c-name deprecated replacement alias-of)
        entry
      (setf (gethash keyword *multi-options*)
            (make-curl-option :keyword keyword :id id :kind kind :c-name c-name
                              :deprecated deprecated :replacement replacement
                              :alias-of alias-of
                              ;; There is no curl_multi_option_by_name, so
                              ;; availability cannot be probed the way easy
                              ;; options can; assume present and let libcurl
                              ;; answer with CURLM_UNKNOWN_OPTION.
                              :availability t))))
  (hash-table-count *multi-options*))

(defun find-multi-option (keyword)
  (gethash keyword *multi-options*))

(defun ensure-multi-option (keyword)
  (or (find-multi-option keyword)
      (error 'unsupported-option
             :name keyword
             :running-version (ignore-errors (libcurl-version))
             :message "No such libcurl multi option is known to this binding.")))
