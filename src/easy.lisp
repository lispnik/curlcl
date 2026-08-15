;;;; src/easy.lisp — the easy handle, as a Lisp object.
;;;;
;;;; An EASY-HANDLE owns a CURL*, the foreign memory libcurl borrows from it,
;;;; an error buffer, and a registry key that every callback finds its closures
;;;; through.  Release order is the whole game:
;;;;
;;;;   1. curl_easy_cleanup, which is still reading the error buffer, any
;;;;      slists, and any borrowed payload right up until it returns;
;;;;   2. the foreign memory, now that nothing can read it;
;;;;   3. the registry key, last, so no in-flight callback can find a freed
;;;;      state -- and so a key cannot be reused while libcurl still holds it.
;;;;
;;;; Two libcurl behaviours need active defence rather than documentation:
;;;;
;;;;   curl_easy_duphandle copies *every* option, including CURLOPT_WRITEDATA
;;;;   and CURLOPT_ERRORBUFFER.  The copy therefore points at the *original's*
;;;;   registry key and error buffer, so without re-pointing them the dup's
;;;;   output lands in the original's buffer and its errors overwrite the
;;;;   original's message.  DUPLICATE-HANDLE re-installs both.
;;;;
;;;;   CURLOPT_POSTFIELDS does not copy its argument.  Rather than make callers
;;;;   reason about that, :POSTFIELDS is routed to CURLOPT_COPYPOSTFIELDS so
;;;;   libcurl owns the copy -- which also makes a duplicated handle safe.  The
;;;;   size has to be set first, because libcurl reads it when the copy is made.

(in-package #:libcurl)

(defgeneric handle-pointer (handle)
  (:documentation
   "The raw CURL* this handle wraps.

Exported for the sake of anything this binding does not cover: it is the value
to pass to a foreign call made by hand.  Its lifetime is the handle's, so it
must not outlive CLOSE-HANDLE."))

(defgeneric handle-error-buffer (handle)
  (:documentation
   "The foreign CURLOPT_ERRORBUFFER attached to this handle, or NIL.

Read through CURL-ERROR-DETAIL rather than directly; this exists because the
buffer's lifetime is part of the handle's release order, which RESET-HANDLE
also has to respect."))

(defgeneric handle-plist (handle)
  (:documentation
   "Arbitrary Lisp data attached to this handle.  SETFable.

CURLOPT_PRIVATE is not available for this -- the binding uses it to map a bare
CURL* back to its handle -- so this is where callers put their own.  The multi
interface makes it necessary rather than merely convenient: results arrive as
bare handles, and the caller has to find whatever they set up alongside."))

(defgeneric handle-share (handle)
  (:documentation
   "The SHARE-HANDLE this handle is attached to, or NIL.  SETFable.

Held so the share cannot be collected or closed while a transfer is still
using it."))

(defgeneric handle-closed-p (handle)
  (:documentation
   "True once CLOSE-HANDLE has run.  Every operation checks it, so a use after
close signals HANDLE-CLOSED rather than reaching libcurl with a dead CURL*."))

(defclass easy-handle ()
  ((pointer :initarg :pointer :reader handle-pointer
            :documentation "The CURL*.")
   ;; All three default to NIL rather than being unbound: %SET-OPTION reads the
   ;; error buffer to attach detail to any condition it signals, and it runs
   ;; during setup, before the buffer exists.
   (callbacks :reader handle-callbacks :initform nil)
   (resources :reader handle-resources :initform nil)
   (error-buffer :reader handle-error-buffer :initform nil)
   (plist :accessor handle-plist :initform '()
          :documentation "Arbitrary Lisp data attached to this handle.

CURLOPT_PRIVATE is not available for this -- the binding uses it to map a bare
CURL* back to its handle -- so callers who need to associate something with a
transfer put it here.  The multi interface makes this necessary rather than
merely convenient: results come back as bare handles, and the caller has to
find whatever they set up alongside.")
   (share :accessor handle-share :initform nil
          :documentation "A SHARE-HANDLE this is attached to, kept reachable so
it cannot be collected or closed out from under the transfer.")
   (closed-p :accessor handle-closed-p :initform nil))
  (:documentation
   "A libcurl easy handle and everything whose lifetime is tied to it."))

(defmethod print-object ((handle easy-handle) stream)
  (print-unreadable-object (handle stream :type t :identity t)
    (if (handle-closed-p handle)
        (write-string "closed" stream)
        (format stream "~@[~A~]"
                (ignore-errors (getinfo (handle-pointer handle) :effective-url))))))

(define-condition handle-closed (curl-error)
  ((handle :initarg :handle :reader handle-closed-handle
           :documentation "The handle that was used after being closed."))
  (:report (lambda (c s)
             (declare (ignore c))
             (write-string "This libcurl handle has already been closed." s)))
  (:documentation
   "A handle was used after CLOSE-HANDLE.

Signalled in preference to letting the call through: the CURL* has been freed
by then, so libcurl would be reading memory it no longer owns."))

(declaim (inline check-open))
(defun check-open (handle)
  (when (handle-closed-p handle)
    (error 'handle-closed :handle handle))
  handle)

(defun make-easy-handle ()
  "Create an easy handle with an error buffer and callback state installed.

Prefer WITH-EASY.  A handle created here and dropped without CLOSE-HANDLE
leaks a socket, a connection cache and a registry key: there is no finalizer,
deliberately, because the registry must hold the state strongly for as long as
libcurl might call into it, which would keep the handle alive anyway."
  (let ((pointer (%curl-easy-init)))
    (when (cffi:null-pointer-p pointer)
      (error 'easy-error :message "curl_easy_init returned NULL"))
    (let ((handle (make-instance 'easy-handle :pointer pointer))
          (completed nil))
      (unwind-protect
           (progn
             (setf (slot-value handle 'resources) (make-foreign-resources)
                   (slot-value handle 'callbacks)
                   (make-callback-state :handle-pointer pointer))
             ;; The error buffer comes first so that any condition signalled by
             ;; the setup below can already carry libcurl's own explanation.
             (setf (slot-value handle 'error-buffer)
                   (allocate-error-buffer (handle-resources handle)))
             (%set-option handle :errorbuffer (handle-error-buffer handle))
             (let ((state (handle-callbacks handle)))
               (setf (cb-handle state) handle)
               (register-callback-state state)
               ;; CURLOPT_PRIVATE carries the same key, which is what lets a
               ;; bare CURL* -- the one a CURLMsg hands back, say -- be traced
               ;; to this object.  The binding owns this option, as it owns the
               ;; *DATA slots.
               (%set-option handle :private (callback-key-pointer (cb-key state))))
             ;; libcurl's default is to use signals for the DNS timeout, which
             ;; is hostile to a Lisp runtime that installs its own handlers.
             (%set-option handle :nosignal 1)
             ;; Install the write and header trampolines up front, with no
             ;; closures behind them.  libcurl's built-in write callback sends
             ;; the response body to *stdout*, so a handle used without setting
             ;; one would print the body to the terminal -- a surprising and
             ;; occasionally disastrous default for a library.  With the
             ;; trampoline in place and no closure, the body is discarded.
             (%install-callback handle :write)
             (%install-callback handle :header)
             ;; The read side is worse than startling: libcurl's built-in read
             ;; callback is fread on CURLOPT_READDATA, which defaults to
             ;; *stdin*.  An upload with no read callback set therefore blocks
             ;; on the terminal, or crashes when stdin is not a usable stream.
             ;; With the trampoline in place and no closure it reports EOF.
             (%install-callback handle :read)
             (setf completed t)
             handle)
        ;; Do not leak the CURL* or a registry key if setup fails partway.
        (unless completed
          (close-handle handle))))))

(defun close-handle (handle)
  "Release HANDLE and everything it owns.  Idempotent."
  (unless (handle-closed-p handle)
    (setf (handle-closed-p handle) t)
    ;; Order matters; see the file header.  The slots are checked because this
    ;; also runs on the failure path of MAKE-EASY-HANDLE, where the handle may
    ;; be only partly built.
    (%curl-easy-cleanup (handle-pointer handle))
    (when (handle-resources handle)
      (release-resources (handle-resources handle)))
    (when (handle-callbacks handle)
      (release-callback-state (cb-key (handle-callbacks handle))))
    (setf (handle-share handle) nil))
  (values))

(defmacro with-easy ((var) &body body)
  "Run BODY with VAR bound to a fresh EASY-HANDLE, closed on exit."
  `(let ((,var (make-easy-handle)))
     (unwind-protect (progn ,@body)
       (close-handle ,var))))

;;; Setting options -----------------------------------------------------------

(defun %set-option (handle option value)
  "Set OPTION on HANDLE, dispatching on the option's declared kind.

This is where the spelled type earns its keep: all of :STRINGPOINT,
:SLISTPOINT, :CBPOINT and :OBJECTPOINT are the same number to libcurl, but they
want a copied string, an owned slist, a raw key and a borrowed buffer
respectively."
  (let* ((entry (ensure-option option))
         (pointer (handle-pointer handle))
         (resources (handle-resources handle))
         (id (option-id entry)))
    ;; Note: no deprecation warning here.  This is the internal entry point,
    ;; used by the binding to install its own error buffer and callbacks --
    ;; including deprecated ones like CURLOPT_IOCTLFUNCTION -- and warning
    ;; about options the caller never mentioned would be noise.  SETOPT warns.
    (%check-easy
     (ecase (option-kind entry)
       ((:long :values)
        (%setopt-long pointer id (%coerce-to-long value)))
       ((:off-t)
        (%setopt-off-t pointer id value))
       ((:stringpoint)
        ;; libcurl copies these, so a temporary is safe.
        (if (null value)
            (%setopt-pointer pointer id (cffi:null-pointer))
            (cffi:with-foreign-string (c-string (string value))
              (%setopt-pointer pointer id c-string))))
       ((:slistpoint)
        (%setopt-pointer pointer id
                         (if (null value)
                             (cffi:null-pointer)
                             (own-slist resources (mapcar #'string value)))))
       ((:blob)
        (%setopt-pointer pointer id
                         (if (null value) (cffi:null-pointer)
                             (own-blob resources value))))
       ((:functionpoint :cbpoint :objectpoint)
        (%setopt-pointer pointer id (%coerce-to-pointer value resources))))
     :detail (error-buffer-text (handle-error-buffer handle)))))

(defun %coerce-to-long (value)
  "Booleans are the natural Lisp spelling of libcurl's 0/1 long options."
  (etypecase value
    (integer value)
    (null 0)
    ((eql t) 1)))

(defun %coerce-to-pointer (value resources)
  (etypecase value
    (null (cffi:null-pointer))
    (cffi:foreign-pointer value)
    ;; An octet vector or string here means the caller wants libcurl to read
    ;; from memory we must keep alive for the transfer.
    ((or string (array (unsigned-byte 8) (*)))
     (own-octets resources (coerce-to-octets value)))))

(defun setopt (handle option value)
  "Set OPTION on HANDLE.  OPTION is a keyword such as :URL or :FOLLOWLOCATION.

The keyword is the C name mechanically transformed: drop CURLOPT_, downcase,
underscores to hyphens.  CURLOPT_SSL_VERIFYPEER is :SSL-VERIFYPEER.

Values are the obvious Lisp ones -- a string for a string option, T or NIL for
a boolean, an integer for a number, a list of strings for a header list, octets
or a string for a blob."
  (check-open handle)
  (case option
    ;; Routed to CURLOPT_COPYPOSTFIELDS so libcurl owns the copy; see the file
    ;; header.  The size must be set before the copy is made, and setting it
    ;; explicitly also allows a body containing NUL bytes, which the
    ;; string-length default would truncate.
    (:postfields
     (let ((octets (coerce-to-octets value)))
       (setopt handle :postfieldsize-large (length octets))
       (cffi:with-pointer-to-vector-data (pointer octets)
         (%check-easy (%setopt-pointer (handle-pointer handle)
                                       (option-id (ensure-option :copypostfields))
                                       pointer)))))
    (t (%set-option handle option value)))
  value)

(defun setopts (handle &rest plist)
  "Set several options: (SETOPTS h :url \"...\" :followlocation t)."
  (loop for (option value) on plist by #'cddr
        do (setopt handle option value))
  handle)

;;; Reading information -------------------------------------------------------

(defun getinfo (handle info)
  "Read INFO from HANDLE, which may be an EASY-HANDLE or a bare CURL*.

Given an EASY-HANDLE this signals on failure and returns the value; given a
raw pointer -- the form the multi interface has to work with -- it returns
(values value code) and leaves the checking to the caller."
  (etypecase handle
    (easy-handle
     (check-open handle)
     (multiple-value-bind (value code)
         (%getinfo-typed (handle-pointer handle) info)
       (%check-easy code)
       value))
    (cffi:foreign-pointer
     (%getinfo-typed handle info))))

(defun handle-from-pointer (pointer)
  "The EASY-HANDLE wrapping a CURL*, or NIL.

Recovered through CURLOPT_PRIVATE, which the binding sets to the handle's
registry key when the handle is created.  This is what lets the multi interface
report results in terms of Lisp objects."
  (unless (cffi:null-pointer-p pointer)
    (let ((key (%raw-getinfo-pointer pointer (info-id (ensure-info :private)))))
      (let ((state (and key (%lookup-state key))))
        (and state (cb-handle state))))))

;;; Callbacks -----------------------------------------------------------------

(defun %install-callback (handle slot)
  (destructuring-bind (name function-option data-option trampoline)
      (find-callback-options slot)
    (declare (ignore name))
    (let ((state (handle-callbacks handle)))
      ;; Both options are always set, and the trampoline stays installed even
      ;; when the closure is nil -- in which case it acts as a no-op sink: the
      ;; write trampoline discards, the read one reports EOF.  Neither
      ;; alternative works.
      ;;
      ;; Nulling the function pointer alone leaves the data slot holding a
      ;; registry key, which dangles after curl_easy_duphandle if the original
      ;; handle is closed first.
      ;;
      ;; Nulling the data slot as well hands libcurl's *built-in* callbacks a
      ;; NULL where they expect a FILE*.  CURLOPT_READDATA set to NULL makes
      ;; the default read function call fread on it, which segfaults; and
      ;; CURLOPT_HEADERDATA set with no header function makes libcurl route
      ;; headers into the write callback instead.  "Never set" and "set to
      ;; NULL" are different states, and there is no value that is both safe
      ;; and inert.
      (%set-option handle data-option (callback-key-pointer (cb-key state)))
      (%set-option handle function-option (cffi:get-callback trampoline))
      (pushnew slot (cb-installed state))))
  slot)

(defun callback-function (handle slot)
  "The Lisp closure installed for SLOT, or NIL."
  (funcall (callback-slot-accessor slot) (handle-callbacks handle)))

(defun (setf callback-function) (function handle slot)
  "Install FUNCTION as HANDLE's SLOT callback, or NIL to remove it.

SLOT is one of the names in CALLBACK-SLOT-NAMES: :WRITE, :HEADER, :READ,
:PROGRESS, :DEBUG, :SEEK and so on.  The contract per slot:

  WRITE, HEADER, INTERLEAVE  (octets) -- return NIL or :ABORT to fail the
      transfer, :PAUSE to pause it, anything else to accept the data.
  READ  (max-bytes) -- return octets or a string, NIL or :EOF for end of
      input, :ABORT to fail, :PAUSE to pause.
  PROGRESS  (dltotal dlnow ultotal ulnow) -- return NIL or :ABORT to cancel.
  DEBUG  (type octets) -- type is :TEXT, :HEADER-IN, :DATA-OUT and so on.
  SEEK  (offset whence) -- whence is :SET, :CURRENT or :END; return :OK,
      :FAIL or :CANTSEEK.

A condition signalled inside any of them is caught at the boundary and
re-signalled from PERFORM as a CALLBACK-ERROR, so it never unwinds into C.
A non-local exit out of one is undefined behaviour and must not be attempted."
  (check-open handle)
  (setf (callback-slot (handle-callbacks handle) slot) function)
  (%install-callback handle slot)
  function)

;;; Performing ----------------------------------------------------------------

(defun %perform-once (handle)
  (let ((state (handle-callbacks handle)))
    (setf (cb-condition state) nil
          (cb-condition-kind state) nil)
    (clear-error-buffer (handle-error-buffer handle))
    (let ((code (%curl-easy-perform (handle-pointer handle))))
      ;; A condition from a callback wins over libcurl's code.  libcurl reports
      ;; CURLE_WRITE_ERROR or CURLE_ABORTED_BY_CALLBACK, which is true but
      ;; useless: the caller's actual error is the one the callback signalled.
      (when (cb-condition state)
        (error 'callback-error
               :cause (cb-condition state)
               :kind (cb-condition-kind state)
               :code code
               :code-name (curlcode-keyword code)
               :message (%curl-easy-strerror code)))
      (unless (zerop code)
        (error 'easy-error
               :code code
               :code-name (curlcode-keyword code)
               :message (%curl-easy-strerror code)
               :detail (error-buffer-text (handle-error-buffer handle))
               :url (ignore-errors (getinfo handle :effective-url))))
      handle)))

(defun perform (handle)
  "Run the transfer, signalling on failure.  Returns HANDLE.

Two restarts are established: RETRY runs the transfer again on the same handle,
and IGNORE-ERROR abandons it and returns NIL.  RETRY is genuinely useful here,
since most libcurl failures are transient network conditions and the handle is
still perfectly good afterwards."
  (check-open handle)
  (loop
    (let ((outcome
            (restart-case (return (%perform-once handle))
              (retry ()
                :report "Perform the transfer again."
                :retry)
              (ignore-error ()
                :report "Abandon the transfer and return NIL."
                nil))))
      (unless (eq outcome :retry)
        (return nil)))))

(defun reset-handle (handle)
  "Return HANDLE to its initial state, keeping the CURL* and its connections.

curl_easy_reset clears every option, which includes the ones the binding needs
for itself -- the error buffer, CURLOPT_PRIVATE and every callback -- so they
are re-installed here.  The connection cache survives, which is the reason to
reset rather than close and reopen."
  (check-open handle)
  (%curl-easy-reset (handle-pointer handle))
  ;; libcurl has dropped every pointer it borrowed, so the memory behind them
  ;; can go now.  That includes the error buffer -- and the slot must be
  ;; cleared in the same breath, because %SET-OPTION reads it to attach detail
  ;; to any condition it might signal.  Leaving the freed pointer in place made
  ;; the very next option call read freed memory.
  (release-resources (handle-resources handle))
  (setf (slot-value handle 'error-buffer) nil)
  (setf (slot-value handle 'error-buffer)
        (allocate-error-buffer (handle-resources handle)))
  (%set-option handle :errorbuffer (handle-error-buffer handle))
  (%set-option handle :nosignal 1)
  (let ((state (handle-callbacks handle)))
    (setf (cb-condition state) nil
          (cb-condition-kind state) nil)
    (%set-option handle :private (callback-key-pointer (cb-key state)))
    ;; Re-install by what was installed rather than by what has a closure: a
    ;; slot whose closure was cleared still has our trampoline wired, and
    ;; deliberately so.
    (dolist (slot (reverse (cb-installed state)))
      (%install-callback handle slot)))
  handle)

(defun duplicate-handle (handle)
  "Copy HANDLE, including its options and its Lisp callbacks.

curl_easy_duphandle copies the *values* of every option, which for
CURLOPT_WRITEDATA and CURLOPT_ERRORBUFFER means the copy points into the
original.  Left alone, the duplicate would write its body into the original's
buffer and its error text over the original's message.  Both are re-pointed
here, along with CURLOPT_PRIVATE.

Note that options whose memory the original owns -- an slist of headers, say --
are copied by libcurl at the pointer level, so they remain valid only while the
original lives.  Re-set any such option on the duplicate if it is to outlive
the handle it came from."
  (check-open handle)
  (let ((pointer (%curl-easy-duphandle (handle-pointer handle))))
    (when (cffi:null-pointer-p pointer)
      (error 'easy-error :message "curl_easy_duphandle returned NULL"))
    (let ((copy (make-instance 'easy-handle :pointer pointer)))
      (setf (slot-value copy 'resources) (make-foreign-resources)
            (slot-value copy 'callbacks)
            (make-callback-state :handle-pointer pointer))
      (let ((state (handle-callbacks copy)))
        (setf (cb-handle state) copy)
        (register-callback-state state)
        ;; Inherit the closures...
        (dolist (slot (callback-slot-names))
          (setf (callback-slot state slot)
                (funcall (callback-slot-accessor slot) (handle-callbacks handle))))
        ;; ...then re-point every installed slot at this handle's own key.
        ;; Only the installed ones: wiring a trampoline into a slot the
        ;; original never used would change behaviour, and some function
        ;; options (CURLOPT_SSL_CTX_FUNCTION on a backend that has no SSL_CTX)
        ;; are rejected outright by setopt.
        (%set-option copy :private (callback-key-pointer (cb-key state)))
        (dolist (slot (reverse (cb-installed (handle-callbacks handle))))
          (%install-callback copy slot)))
      (setf (slot-value copy 'error-buffer)
            (allocate-error-buffer (handle-resources copy)))
      (%set-option copy :errorbuffer (handle-error-buffer copy))
      copy)))

;;; Odds and ends -------------------------------------------------------------

(defun pause-transfer (handle &key (direction :all))
  "Pause receiving, sending, or both.  Resume with RESUME-TRANSFER."
  (check-open handle)
  (%check-easy (%curl-easy-pause (handle-pointer handle)
                                 (ecase direction
                                   (:receive +curlpause-recv+)
                                   (:send +curlpause-send+)
                                   (:all +curlpause-all+))))
  handle)

(defun resume-transfer (handle)
  "Unpause a transfer that a callback paused, and return HANDLE.

curl_easy_pause with CURLPAUSE_CONT.  A write callback returning
:PAUSE stops the transfer where it is; this is what starts it again.  Note that
unpausing can deliver buffered data immediately, from inside this call."
  (check-open handle)
  (%check-easy (%curl-easy-pause (handle-pointer handle) +curlpause-cont+))
  handle)

(defun url-escape (handle string)
  "Percent-encode STRING using libcurl's encoder."
  (check-open handle)
  (cffi:with-foreign-string ((c-string length) string :null-terminated-p nil)
    (let ((result (%curl-easy-escape (handle-pointer handle) c-string length)))
      (when (cffi:null-pointer-p result)
        (error 'easy-error :message "curl_easy_escape failed"))
      ;; libcurl allocated this with its own allocator, so it goes back through
      ;; curl_free rather than the C library's free.
      (unwind-protect (cffi:foreign-string-to-lisp result)
        (%curl-free result)))))

(defun url-unescape (handle string)
  "Decode percent-encoding in STRING.  Returns octets, since the result need
not be valid text in any particular encoding."
  (check-open handle)
  (cffi:with-foreign-object (out-length :int)
    (cffi:with-foreign-string ((c-string length) string :null-terminated-p nil)
      (let ((result (%curl-easy-unescape (handle-pointer handle) c-string length
                                         out-length)))
        (when (cffi:null-pointer-p result)
          (error 'easy-error :message "curl_easy_unescape failed"))
        (unwind-protect (foreign-to-octets result (cffi:mem-ref out-length :int))
          (%curl-free result))))))

;;; TLS session export --------------------------------------------------------
;;;
;;; libcurl 8.21.0 lets a TLS session ticket be exported and re-imported, so a
;;; later process can resume rather than repeat a full handshake.  Resolved
;;; rather than declared, since an older libcurl does not export the symbols.

(defvar *ssls-import-function*
  (cffi:foreign-symbol-pointer "curl_easy_ssls_import"))

(defun tls-session-import-supported-p ()
  "True when the loaded libcurl exports curl_easy_ssls_import (8.21.0 or newer).

Resolved at load time rather than declared, so an older libcurl reports the
absence here instead of failing at the first call."
  (and *ssls-import-function* (not (cffi:null-pointer-p *ssls-import-function*))))

(defun import-tls-session (handle session-key shmac sdata)
  "Re-import a previously exported TLS session, so a handshake can be resumed.

SHMAC and SDATA are the octet vectors that came out of the export callback,
along with the session key that identifies them.  Requires libcurl 8.21.0."
  (check-open handle)
  (unless (tls-session-import-supported-p)
    (error 'unsupported-feature
           :name "curl_easy_ssls_import (libcurl 8.21.0 or newer)"
           :message "This libcurl does not export curl_easy_ssls_import."))
  (let ((shmac-octets (coerce-to-octets shmac))
        (sdata-octets (coerce-to-octets sdata)))
    (cffi:with-pointer-to-vector-data (shmac-pointer shmac-octets)
      (cffi:with-pointer-to-vector-data (sdata-pointer sdata-octets)
        (cffi:with-foreign-string (key session-key)
          (%check-easy
           (cffi:foreign-funcall-pointer
            *ssls-import-function* ()
            :pointer (handle-pointer handle)
            :pointer key
            :pointer shmac-pointer :size (length shmac-octets)
            :pointer sdata-pointer :size (length sdata-octets)
            :int))))))
  handle)

;;; TLS session export --------------------------------------------------------
;;;
;;; The other half of IMPORT-TLS-SESSION.  Note curl_ssls_export_cb is declared
;;; as a function *type* rather than a pointer-to-function typedef, which is
;;; unusual style but identical in ABI terms.

(defvar *ssls-export-function*
  (cffi:foreign-symbol-pointer "curl_easy_ssls_export"))

(defun tls-session-export-supported-p ()
  "True when the loaded libcurl exports curl_easy_ssls_export (8.21.0 or newer)."
  (and *ssls-export-function* (not (cffi:null-pointer-p *ssls-export-function*))))

(cffi:defcallback %ssls-export-trampoline :int
    ((handle :pointer) (userdata :pointer) (session-key :pointer)
     (shmac :pointer) (shmac-length :size)
     (sdata :pointer) (sdata-length :size)
     (valid-until curl-off-t) (ietf-tls-id :int) (alpn :pointer)
     (earlydata-max :size))
  (declare (ignorable handle))
  (let ((state (%lookup-state userdata)))
    (if (null state)
        (curlcode-value :bad-function-argument)
        (with-callback-guard (state :ssls-export (curlcode-value :bad-function-argument))
          (let ((function (cb-ssls-export state)))
            (when function
              (funcall function
                       (unless (cffi:null-pointer-p session-key)
                         (cffi:foreign-string-to-lisp session-key))
                       (foreign-to-octets shmac shmac-length)
                       (foreign-to-octets sdata sdata-length)
                       (list :valid-until valid-until
                             :ietf-tls-id ietf-tls-id
                             :alpn (unless (cffi:null-pointer-p alpn)
                                     (cffi:foreign-string-to-lisp alpn))
                             :earlydata-max earlydata-max)))
            0)))))

(defun export-tls-sessions (handle function)
  "Call FUNCTION for each resumable TLS session HANDLE holds.

FUNCTION receives (session-key shmac sdata properties), where the first three
are what IMPORT-TLS-SESSION needs to resume later and PROPERTIES is a plist of
:VALID-UNTIL, :IETF-TLS-ID, :ALPN and :EARLYDATA-MAX.  Requires libcurl
8.21.0."
  (check-open handle)
  (unless (tls-session-export-supported-p)
    (error 'unsupported-feature
           :name "curl_easy_ssls_export (libcurl 8.21.0 or newer)"
           :message "This libcurl does not export curl_easy_ssls_export."))
  (let ((state (handle-callbacks handle)))
    (setf (cb-ssls-export state) function)
    (unwind-protect
         (%check-easy (cffi:foreign-funcall-pointer
                       *ssls-export-function* ()
                       :pointer (handle-pointer handle)
                       :pointer (cffi:get-callback '%ssls-export-trampoline)
                       :pointer (callback-key-pointer (cb-key state))
                       :int))
      (setf (cb-ssls-export state) nil)))
  handle)
