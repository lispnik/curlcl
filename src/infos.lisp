;;;; src/infos.lisp — the CURLINFO table and how a keyword becomes a getinfo call.
;;;;
;;;; A CURLINFO constant encodes its return type in the high bits of its value:
;;;; 0x100000 string, 0x200000 long, 0x300000 double, 0x400000 slist *or*
;;;; pointer, 0x500000 socket, 0x600000 curl_off_t.  That determines how much
;;;; storage the out-parameter needs, and getting it wrong is not a graceful
;;;; failure -- libcurl writes eight bytes into whatever you allocated.
;;;;
;;;; Two traps in that scheme are worth stating plainly, because both produce
;;;; wrong answers rather than errors:
;;;;
;;;;   CURLINFO_SLIST and CURLINFO_PTR are the same number (0x400000).  Nothing
;;;;   in the value says whether the result is a curl_slist you must free with
;;;;   curl_slist_free_all or an opaque pointer you must not free at all.  Only
;;;;   five infos use that mask, so the generator resolves them by name.
;;;;
;;;;   CURLINFO_PRIVATE carries the *string* mask but returns whatever void*
;;;;   was stored with CURLOPT_PRIVATE.  Decoding it as a string dereferences
;;;;   an arbitrary pointer.  The generator overrides it to :POINTER.
;;;;
;;;; The data lives in src/infos-table.lisp, which is generated.

(in-package #:libcurl)

(defconstant +curlinfo-mask+     #x0fffff)
(defconstant +curlinfo-typemask+ #xf00000)

(defconstant +curlinfo-string+ #x100000)
(defconstant +curlinfo-long+   #x200000)
(defconstant +curlinfo-double+ #x300000)
(defconstant +curlinfo-slist+  #x400000)
(defconstant +curlinfo-ptr+    #x400000) ; deliberately the same as slist
(defconstant +curlinfo-socket+ #x500000)
(defconstant +curlinfo-off-t+  #x600000)

(defparameter *info-type-masks*
  '((#x100000 . :string)
    (#x200000 . :long)
    (#x300000 . :double)
    (#x400000 . :slist)                 ; ...or :POINTER; resolved by name
    (#x500000 . :socket)
    (#x600000 . :off-t))
  "Type mask -> kind, for checking a table entry against its own identifier.
The 0x400000 row is only half the story, which is the point: the generator
resolves slist-versus-pointer by name because the number cannot.")

(defstruct (curl-info (:conc-name info-))
  "One entry of libcurl's getinfo table."
  (keyword nil :type symbol)
  (id 0 :type integer)
  ;; :STRING :LONG :DOUBLE :SLIST :POINTER :SOCKET :OFF-T -- resolved by the
  ;; generator, not merely masked off the id, because of the traps above.
  (kind nil :type symbol)
  (c-name "" :type string)
  (deprecated nil))

(setf (documentation 'info-id 'function)
      "The integer CURLINFO_* value; its type is encoded in the high bits."
      (documentation 'info-kind 'function)
      "The info's result type: :STRING, :LONG, :DOUBLE, :SLIST, :POINTER,
:SOCKET or :OFF-T.

Resolved by the generator rather than masked off the id at runtime, because the
id does not always say: CURLINFO_SLIST and CURLINFO_PTR are the same value, and
CURLINFO_PRIVATE is typed STRING but returns a bare pointer -- decoding that as
a string would dereference whatever the caller stored.")

(defvar *infos* (make-hash-table :test 'eq)
  "Info keyword -> CURL-INFO.")

(defun register-infos (entries)
  "Populate the info table from generated data.  Called by infos-table.lisp."
  (clrhash *infos*)
  (dolist (entry entries)
    (destructuring-bind (keyword id kind c-name deprecated) entry
      (setf (gethash keyword *infos*)
            (make-curl-info :keyword keyword :id id :kind kind
                            :c-name c-name :deprecated deprecated))))
  (hash-table-count *infos*))

(defun find-info (keyword)
  "The CURL-INFO named KEYWORD, or NIL."
  (gethash keyword *infos*))

(defun ensure-info (keyword)
  (or (find-info keyword)
      (error 'unsupported-option
             :name keyword
             :running-version (ignore-errors (libcurl-version))
             :message "No such libcurl info is known to this binding.")))

(defun info-type-mask (id)
  "The declared type bits of ID, for checking a table entry against its number."
  (logand id +curlinfo-typemask+))

(defun map-infos (function)
  (maphash (lambda (key info) (declare (ignore key)) (funcall function info))
           *infos*))

(defun info-count ()
  (hash-table-count *infos*))

(defun %getinfo-typed (handle keyword)
  "Read KEYWORD from HANDLE, a raw CURL*.  Returns (values value code).

Storage for the out-parameter is sized from the info's kind: a CURLINFO_LONG
wants a C `long' (eight bytes here, not four), a CURLINFO_SOCKET wants an int
(four), and everything else wants eight.  This is the single place that
mapping is made, so it is the single place it can be got wrong."
  (let ((info (ensure-info keyword)))
    (ecase (info-kind info)
      (:string (%raw-getinfo-string handle (info-id info)))
      (:long (%raw-getinfo-long handle (info-id info)))
      (:double (%raw-getinfo-double handle (info-id info)))
      (:off-t (%raw-getinfo-off-t handle (info-id info)))
      ;; The socket is the one narrow out-parameter in the whole API.
      (:socket (cffi:with-foreign-object (out 'curl-socket-t)
                 (setf (cffi:mem-ref out 'curl-socket-t) 0)
                 (let ((code (%getinfo handle (info-id info) out)))
                   (values (cffi:mem-ref out 'curl-socket-t) code))))
      (:pointer (%raw-getinfo-pointer handle (info-id info)))
      ;; An slist result is libcurl's to allocate and the caller's to free, so
      ;; it is converted to a Lisp list and released here rather than handed
      ;; out raw.  A :POINTER result, by contrast, must not be freed at all --
      ;; which is the distinction the shared 0x400000 mask cannot express.
      (:slist (multiple-value-bind (pointer code)
                  (%raw-getinfo-pointer handle (info-id info))
                (if (cffi:null-pointer-p pointer)
                    (values nil code)
                    (unwind-protect (values (slist-to-list pointer) code)
                      (%curl-slist-free-all pointer))))))))
