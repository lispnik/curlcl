;;;; src/callbacks.lisp — safe Lisp callbacks, via static trampolines.
;;;;
;;;; Every libcurl callback carries a void* userdata pointer.  Every single one
;;;; -- the only exception in the whole API is the curl_global_init_mem
;;;; allocator set, which is a process-wide singleton with nothing to
;;;; parameterise.  That makes runtime-minted C function pointers unnecessary
;;;; here: there is one static CFFI callback per C signature, the binding puts
;;;; a small integer in libcurl's userdata slot, and the trampoline looks the
;;;; Lisp closure up in a table.
;;;;
;;;; The table is a simple-vector indexed by that integer.  Reads take no lock:
;;;; a reader grabs the vector into a local and indexes it, and growth
;;;; allocates a larger vector, copies, and replaces the global -- so a reader
;;;; holding the old one still sees valid data.  Only registration and release
;;;; take the lock, and those happen once per handle rather than once per
;;;; received chunk.  Keys start at 1 so that a null userdata -- which is what
;;;; libcurl passes if a *DATA option was never set -- can never be mistaken
;;;; for key 0.
;;;;
;;;; Safety is the other half.  A Lisp condition must never unwind through a
;;;; C frame, so every trampoline body runs inside a handler that catches
;;;; SERIOUS-CONDITION, stashes it on the handle, and returns the abort value
;;;; that *this particular* callback uses -- they are all different, and several
;;;; are 32-bit magic numbers returned from functions declared size_t, so
;;;; widening them to 64 bits would tell libcurl an absurd byte count was
;;;; consumed rather than aborting.  PERFORM then re-signals the stashed
;;;; condition, so the caller sees their own error rather than
;;;; CURLE_WRITE_ERROR.
;;;;
;;;; What this cannot catch is a non-local exit -- a THROW or GO across the
;;;; boundary out of a user closure.  HANDLER-CASE does not intercept those and
;;;; nothing else can; it is undefined behaviour and documented as such.
;;;;
;;;; Float traps are deliberately not masked.  Easy-interface callbacks run on
;;;; the thread that called curl_easy_perform, so the FPU control word is
;;;; already SBCL's and a trap arrives as an ordinary Lisp condition that the
;;;; handler below catches.  Masking would cost a control-register round trip
;;;; per chunk to defend against a situation libcurl does not create: it never
;;;; invokes a user callback from its own resolver threads.

(in-package #:libcurl)

;;; The registry --------------------------------------------------------------

(defvar *callback-table* (make-array 64 :initial-element nil)
  "Key -> CALLBACK-STATE.  Replaced wholesale on growth; never mutated in a way
that would invalidate a reader holding an earlier vector.")

(defvar *callback-lock* (bt:make-lock "libcurl callback registry"))

(defvar *callback-free-keys* '()
  "Released keys, for reuse.  Guarded by *CALLBACK-LOCK*.")

(defvar *callback-next-key* 1
  "Next never-used key.  Starts at 1: key 0 would be indistinguishable from the
null userdata libcurl passes when a *DATA option was never set.")

(defstruct (callback-state (:conc-name cb-))
  "The Lisp side of one handle's callbacks, reachable from a C userdata slot."
  (key 0 :type fixnum)
  ;; The CURL* this state belongs to, so callbacks handed a CURL* can be
  ;; matched back to their handle.
  (handle-pointer (cffi:null-pointer))
  ;; The EASY-HANDLE (or MULTI-HANDLE) owning this state.  Untyped because
  ;; those classes are defined in files that load after this one.
  (handle nil)
  ;; A condition signalled inside a callback, kept until PERFORM can re-signal
  ;; it from Lisp.
  (condition nil)
  (condition-kind nil)
  ;; The closures themselves.
  write read header progress debug seek sockopt opensocket closesocket
  ssl-ctx prereq resolver-start fnmatch chunk-begin chunk-end trailer
  hsts-read hsts-write ssh-key ssh-host-key interleave ioctl)

(defun register-callback-state (state)
  "Assign STATE a key and install it.  Returns the key."
  (bt:with-lock-held (*callback-lock*)
    (let ((key (or (pop *callback-free-keys*)
                   (prog1 *callback-next-key* (incf *callback-next-key*)))))
      (when (>= key (length *callback-table*))
        ;; Copy into a larger vector and swap it in.  A reader that already
        ;; grabbed the old vector keeps working against it, which is safe: its
        ;; own key was registered before any callback using it could fire.
        (let ((bigger (make-array (max (* 2 (length *callback-table*)) (1+ key))
                                  :initial-element nil)))
          (replace bigger *callback-table*)
          (setf *callback-table* bigger)))
      (setf (cb-key state) key
            (svref *callback-table* key) state)
      key)))

(defun release-callback-state (key)
  "Release KEY.  Must not run until libcurl can no longer call back into it --
that is, after curl_easy_cleanup has returned."
  (bt:with-lock-held (*callback-lock*)
    (when (and (plusp key) (< key (length *callback-table*)))
      (setf (svref *callback-table* key) nil)
      (push key *callback-free-keys*)))
  (values))

(declaim (inline %lookup-state))
(defun %lookup-state (userdata)
  "The CALLBACK-STATE for a userdata pointer, or NIL.

The hot path, run once per received chunk: one global read, a bounds check and
an SVREF, with no lock.  NIL means the handle is gone, and every trampoline
treats that as a failure rather than dispatching into nothing."
  (let ((key (cffi:pointer-address userdata))
        (table *callback-table*))
    ;; Declared so the bounds check compiles to an SVREF rather than a generic
    ;; sequence call; this runs once per received chunk.
    (declare (type simple-vector table))
    (when (and (plusp key) (< key (length table)))
      (svref table key))))

(defun callback-key-pointer (key)
  "KEY as the pointer to hand libcurl for a *DATA option."
  (cffi:make-pointer key))

(defun stash-callback-condition (state kind condition)
  "Remember CONDITION so PERFORM can re-signal it once C is out of the picture.

Only the first is kept: a failing write callback is usually invoked again
before libcurl notices the abort, and the first condition is the interesting
one."
  (unless (cb-condition state)
    (setf (cb-condition state) condition
          (cb-condition-kind state) kind))
  condition)

(defmacro with-callback-guard ((state kind failure) &body body)
  "Run BODY, converting any serious condition into FAILURE.

FAILURE is evaluated only on the error path, and is whatever value libcurl
documents as \"abort\" for this particular callback."
  (alexandria:once-only (state)
    `(handler-case (progn ,@body)
       (serious-condition (condition)
         (stash-callback-condition ,state ,kind condition)
         ,failure))))

(defmacro define-trampoline (name return-type lambda-list
                             (&key state-var userdata kind failure)
                             &body body)
  "Define a static CFFI callback that dispatches through the registry.

The shape is the same every time: find the state, fail if it is gone, and run
BODY guarded.  Writing it out per callback would be twenty repetitions of the
same four lines and one chance each to get the failure value wrong.

Arguments are declared IGNORABLE here rather than in each body, because the
body is spliced inside a handler and so cannot carry its own declarations."
  (let ((argument-names (mapcar #'first lambda-list)))
    `(cffi:defcallback ,name ,return-type ,lambda-list
       (declare (ignorable ,@argument-names))
       (let ((,state-var (%lookup-state ,userdata)))
         (if (null ,state-var)
             ,failure
             (with-callback-guard (,state-var ,kind ,failure)
               ,@body))))))

;;; Transfer callbacks --------------------------------------------------------
;;;
;;; The Lisp-facing contract for each is documented on the corresponding
;;; accessor in easy.lisp.  In brief: returning NIL or :ABORT aborts the
;;; transfer, :PAUSE pauses it, and anything else means "handled".

(defun %write-result (value n)
  "Translate a write/header callback's return value into libcurl's."
  (case value
    ((nil :abort) +curl-writefunc-error+)
    ((:pause) +curl-writefunc-pause+)
    (t (if (integerp value) value n))))

(define-trampoline %write-trampoline :size
    ((buffer :pointer) (size :size) (nitems :size) (userdata :pointer))
    (:state-var state :userdata userdata :kind :write :failure +curl-writefunc-error+)
  (let ((n (* size nitems))
        (function (cb-write state)))
    (if (null function)
        n                               ; no callback: discard, but do not fail
        (%write-result (funcall function (foreign-to-octets buffer n)) n))))

;;; Same C signature as write, but a different userdata slot (CURLOPT_HEADERDATA
;;; rather than CURLOPT_WRITEDATA), so it needs its own trampoline to know which
;;; closure to reach for.
(define-trampoline %header-trampoline :size
    ((buffer :pointer) (size :size) (nitems :size) (userdata :pointer))
    (:state-var state :userdata userdata :kind :header :failure +curl-writefunc-error+)
  (let ((n (* size nitems))
        (function (cb-header state)))
    (if (null function)
        n
        (%write-result (funcall function (foreign-to-octets buffer n)) n))))

(define-trampoline %read-trampoline :size
    ((buffer :pointer) (size :size) (nitems :size) (userdata :pointer))
    (:state-var state :userdata userdata :kind :read :failure +curl-readfunc-abort+)
  (let ((capacity (* size nitems))
        (function (cb-read state)))
    (if (null function)
        0                               ; no callback: immediate EOF
        (let ((result (funcall function capacity)))
          (case result
            ((nil :eof) 0)
            ((:abort) +curl-readfunc-abort+)
            ((:pause) +curl-readfunc-pause+)
            (t (let ((octets (coerce-to-octets result)))
                 (when (> (length octets) capacity)
                   (error "A libcurl read callback returned ~D bytes for a ~D-byte ~
buffer." (length octets) capacity))
                 (octets-to-foreign octets buffer))))))))

(define-trampoline %xferinfo-trampoline :int
    ((userdata :pointer) (dltotal curl-off-t) (dlnow curl-off-t)
     (ultotal curl-off-t) (ulnow curl-off-t))
    (:state-var state :userdata userdata :kind :progress :failure 1)
  (let ((function (cb-progress state)))
    (if (null function)
        0
        (case (funcall function dltotal dlnow ultotal ulnow)
          ((nil :abort) 1)              ; non-zero aborts with ABORTED_BY_CALLBACK
          ((:continue) +curl-progressfunc-continue+)
          (t 0)))))

(define-trampoline %debug-trampoline :int
    ((handle :pointer) (type :int) (data :pointer) (size :size) (userdata :pointer))
    (:state-var state :userdata userdata :kind :debug :failure 0)
  (let ((function (cb-debug state)))
    (when function
      ;; The data is not NUL-terminated, so SIZE is the only thing that says
      ;; where it ends.
      (funcall function (curlcode-keyword type 'curl-infotype)
               (foreign-to-octets data size)))
    ;; Anything but zero is documented as reserved, so a failing debug callback
    ;; cannot abort the transfer -- the condition is stashed and surfaces later.
    0))

(define-trampoline %seek-trampoline :int
    ((userdata :pointer) (offset curl-off-t) (origin :int))
    (:state-var state :userdata userdata :kind :seek :failure +curl-seekfunc-fail+)
  (let ((function (cb-seek state)))
    (if (null function)
        +curl-seekfunc-cantseek+
        (case (funcall function offset (case origin (0 :set) (1 :current) (2 :end)))
          ((nil :fail) +curl-seekfunc-fail+)
          ((:cantseek) +curl-seekfunc-cantseek+)
          (t +curl-seekfunc-ok+)))))

(define-trampoline %sockopt-trampoline :int
    ((userdata :pointer) (fd curl-socket-t) (purpose :int))
    (:state-var state :userdata userdata :kind :sockopt :failure +curl-sockopt-error+)
  (let ((function (cb-sockopt state)))
    (if (null function)
        +curl-sockopt-ok+
        (case (funcall function fd (curlcode-keyword purpose 'curl-socktype))
          ((nil :error) +curl-sockopt-error+)
          ((:already-connected) +curl-sockopt-already-connected+)
          (t +curl-sockopt-ok+)))))

(define-trampoline %opensocket-trampoline curl-socket-t
    ((userdata :pointer) (purpose :int) (address :pointer))
    (:state-var state :userdata userdata :kind :opensocket :failure +curl-socket-bad+)
  (let ((function (cb-opensocket state)))
    (if (null function)
        +curl-socket-bad+
        (let ((result (funcall function (curlcode-keyword purpose 'curl-socktype)
                               address)))
          (if (integerp result) result +curl-socket-bad+)))))

(define-trampoline %closesocket-trampoline :int
    ((userdata :pointer) (fd curl-socket-t))
    (:state-var state :userdata userdata :kind :closesocket :failure 1)
  (let ((function (cb-closesocket state)))
    (if (null function) 0 (if (funcall function fd) 0 1))))

(define-trampoline %ssl-ctx-trampoline :int
    ((handle :pointer) (ssl-ctx :pointer) (userdata :pointer))
    (:state-var state :userdata userdata :kind :ssl-ctx
     :failure (curlcode-value :aborted-by-callback))
  (let ((function (cb-ssl-ctx state)))
    (if (null function)
        0
        (if (funcall function ssl-ctx) 0 (curlcode-value :aborted-by-callback)))))

(define-trampoline %prereq-trampoline :int
    ((userdata :pointer) (primary-ip :pointer) (local-ip :pointer)
     (primary-port :int) (local-port :int))
    (:state-var state :userdata userdata :kind :prereq :failure +curl-prereqfunc-abort+)
  (let ((function (cb-prereq state)))
    (if (null function)
        +curl-prereqfunc-ok+
        (if (funcall function
                     (unless (cffi:null-pointer-p primary-ip)
                       (cffi:foreign-string-to-lisp primary-ip))
                     primary-port
                     (unless (cffi:null-pointer-p local-ip)
                       (cffi:foreign-string-to-lisp local-ip))
                     local-port)
            +curl-prereqfunc-ok+
            +curl-prereqfunc-abort+))))

(define-trampoline %resolver-start-trampoline :int
    ((resolver-state :pointer) (reserved :pointer) (userdata :pointer))
    (:state-var state :userdata userdata :kind :resolver-start :failure 1)
  (let ((function (cb-resolver-start state)))
    (if (null function) 0 (if (funcall function resolver-state) 0 1))))

(define-trampoline %fnmatch-trampoline :int
    ((userdata :pointer) (pattern :string) (string :string))
    (:state-var state :userdata userdata :kind :fnmatch :failure +curl-fnmatchfunc-fail+)
  (let ((function (cb-fnmatch state)))
    (if (null function)
        +curl-fnmatchfunc-nomatch+
        (if (funcall function pattern string)
            +curl-fnmatchfunc-match+
            +curl-fnmatchfunc-nomatch+))))

(define-trampoline %chunk-begin-trampoline :long
    ((transfer-info :pointer) (userdata :pointer) (remains :int))
    (:state-var state :userdata userdata :kind :chunk-begin
     :failure +curl-chunk-bgn-func-fail+)
  (let ((function (cb-chunk-begin state)))
    (if (null function)
        +curl-chunk-bgn-func-ok+
        (case (funcall function transfer-info remains)
          ((nil :fail) +curl-chunk-bgn-func-fail+)
          ((:skip) +curl-chunk-bgn-func-skip+)
          (t +curl-chunk-bgn-func-ok+)))))

;;; CURLOPT_CHUNK_BGN_FUNCTION and CURLOPT_CHUNK_END_FUNCTION share one
;;; CURLOPT_CHUNK_DATA slot, which the registry handles without comment: both
;;; trampolines find the same state and reach for different closures.
(define-trampoline %chunk-end-trampoline :long
    ((userdata :pointer))
    (:state-var state :userdata userdata :kind :chunk-end
     :failure +curl-chunk-end-func-fail+)
  (let ((function (cb-chunk-end state)))
    (if (null function)
        +curl-chunk-end-func-ok+
        (if (funcall function) +curl-chunk-end-func-ok+ +curl-chunk-end-func-fail+))))

(define-trampoline %trailer-trampoline :int
    ((list-out :pointer) (userdata :pointer))
    (:state-var state :userdata userdata :kind :trailer :failure +curl-trailerfunc-abort+)
  (let ((function (cb-trailer state)))
    (if (null function)
        +curl-trailerfunc-abort+
        (let ((trailers (funcall function)))
          (if (null trailers)
              +curl-trailerfunc-abort+
              ;; libcurl takes ownership of this chain and frees it itself, so
              ;; it is deliberately not recorded against the handle.
              (let ((head (cffi:null-pointer)))
                (dolist (string trailers)
                  (setf head (cffi:with-foreign-string (c string)
                               (cffi:foreign-funcall "curl_slist_append"
                                                     :pointer head :pointer c
                                                     :pointer))))
                (setf (cffi:mem-ref list-out :pointer) head)
                +curl-trailerfunc-ok+))))))

;;; The HSTS entry's includeSubDomains is a C bitfield, which CFFI cannot
;;; express; it is read and written as bit 0 of the containing word.
(defun hsts-entry-include-subdomains-p (entry)
  (logbitp 0 (cffi:foreign-slot-value entry '(:struct curl-hstsentry) 'flags)))

(defun (setf hsts-entry-include-subdomains-p) (value entry)
  (let ((flags (cffi:foreign-slot-value entry '(:struct curl-hstsentry) 'flags)))
    (setf (cffi:foreign-slot-value entry '(:struct curl-hstsentry) 'flags)
          (if value (logior flags 1) (logandc2 flags 1))))
  value)

(defun hsts-entry-to-list (entry)
  "Decode a struct curl_hstsentry into (name include-subdomains-p expire)."
  (let ((name (cffi:foreign-slot-value entry '(:struct curl-hstsentry) 'name)))
    (list (unless (cffi:null-pointer-p name) (cffi:foreign-string-to-lisp name))
          (hsts-entry-include-subdomains-p entry)
          (cffi:foreign-string-to-lisp
           (cffi:foreign-slot-pointer entry '(:struct curl-hstsentry) 'expire)
           :max-chars 17))))

(define-trampoline %hsts-read-trampoline :int
    ((handle :pointer) (entry :pointer) (userdata :pointer))
    (:state-var state :userdata userdata :kind :hsts-read
     :failure (curlcode-value :fail 'curlsts-code))
  (let ((function (cb-hsts-read state)))
    (if (null function)
        (curlcode-value :done 'curlsts-code)
        (let ((next (funcall function)))
          (if (null next)
              (curlcode-value :done 'curlsts-code)
              (destructuring-bind (name include-subdomains expire) next
                (let ((namelen (cffi:foreign-slot-value
                                entry '(:struct curl-hstsentry) 'namelen))
                      (target (cffi:foreign-slot-value
                               entry '(:struct curl-hstsentry) 'name)))
                  ;; libcurl provides the buffer and its size; overrunning it
                  ;; would corrupt its heap, so the name is truncated to fit.
                  (cffi:with-foreign-string ((source length) name
                                             :null-terminated-p nil)
                    (let ((n (min length (1- namelen))))
                      (dotimes (i n)
                        (setf (cffi:mem-aref target :uint8 i)
                              (cffi:mem-aref source :uint8 i)))
                      (setf (cffi:mem-aref target :uint8 n) 0))))
                (setf (hsts-entry-include-subdomains-p entry) include-subdomains)
                (cffi:lisp-string-to-foreign
                 expire
                 (cffi:foreign-slot-pointer entry '(:struct curl-hstsentry) 'expire)
                 18)
                (curlcode-value :ok 'curlsts-code)))))))

(define-trampoline %hsts-write-trampoline :int
    ((handle :pointer) (entry :pointer) (index :pointer) (userdata :pointer))
    (:state-var state :userdata userdata :kind :hsts-write
     :failure (curlcode-value :fail 'curlsts-code))
  (let ((function (cb-hsts-write state)))
    (when function
      (funcall function
               (hsts-entry-to-list entry)
               (cffi:foreign-slot-value index '(:struct curl-index) 'index)
               (cffi:foreign-slot-value index '(:struct curl-index) 'total)))
    (curlcode-value :ok 'curlsts-code)))

(define-trampoline %ssh-key-trampoline :int
    ((handle :pointer) (known-key :pointer) (found-key :pointer)
     (match :int) (userdata :pointer))
    (:state-var state :userdata userdata :kind :ssh-key
     :failure (curlcode-value :reject 'curl-khstat))
  (let ((function (cb-ssh-key state)))
    (if (null function)
        (curlcode-value :reject 'curl-khstat)
        (let ((result (funcall function
                               (%decode-khkey known-key)
                               (%decode-khkey found-key)
                               (curlcode-keyword match 'curl-khmatch))))
          (if (keywordp result)
              (or (curlcode-value result 'curl-khstat)
                  (curlcode-value :reject 'curl-khstat))
              (curlcode-value :reject 'curl-khstat))))))

(defun %decode-khkey (pointer)
  (unless (cffi:null-pointer-p pointer)
    (let ((key (cffi:foreign-slot-value pointer '(:struct curl-khkey) 'key)))
      (list (unless (cffi:null-pointer-p key) (cffi:foreign-string-to-lisp key))
            (cffi:foreign-slot-value pointer '(:struct curl-khkey) 'keytype)))))

(define-trampoline %ssh-host-key-trampoline :int
    ((userdata :pointer) (keytype :int) (key :pointer) (keylen :size))
    (:state-var state :userdata userdata :kind :ssh-host-key :failure 1)
  (let ((function (cb-ssh-host-key state)))
    (if (null function)
        1                               ; refuse by default
        (if (funcall function (curlcode-keyword keytype 'curl-khtype)
                     (foreign-to-octets key keylen))
            0                           ; CURLE_OK accepts the host key
            1))))

(define-trampoline %interleave-trampoline :size
    ((buffer :pointer) (size :size) (nitems :size) (userdata :pointer))
    (:state-var state :userdata userdata :kind :interleave
     :failure +curl-writefunc-error+)
  (let ((n (* size nitems))
        (function (cb-interleave state)))
    (if (null function)
        n
        (%write-result (funcall function (foreign-to-octets buffer n)) n))))

(define-trampoline %ioctl-trampoline :int
    ((handle :pointer) (command :int) (userdata :pointer))
    (:state-var state :userdata userdata :kind :ioctl
     :failure (curlcode-value :failrestart 'curlioerr))
  (let ((function (cb-ioctl state)))
    (if (null function)
        (curlcode-value :unknowncmd 'curlioerr)
        (if (funcall function (curlcode-keyword command 'curliocmd))
            (curlcode-value :ok 'curlioerr)
            (curlcode-value :failrestart 'curlioerr)))))

;;; Which option pair drives which trampoline ---------------------------------
;;;
;;; Each entry is (slot function-option data-option trampoline).  Setting a
;;; closure sets both options: the function pointer and the key that finds it.
;;; Note CURLOPT_PROGRESSDATA is a #define for CURLOPT_XFERINFODATA, so the old
;;; and new progress callbacks genuinely share one data slot -- which the
;;; registry makes a non-issue, since the key is per handle rather than per
;;; callback.

(defparameter *callback-options*
  '((:write         :writefunction      :writedata          %write-trampoline)
    (:header         :headerfunction     :headerdata         %header-trampoline)
    (:read           :readfunction       :readdata           %read-trampoline)
    (:progress       :xferinfofunction   :xferinfodata       %xferinfo-trampoline)
    (:debug          :debugfunction      :debugdata          %debug-trampoline)
    (:seek           :seekfunction       :seekdata           %seek-trampoline)
    (:sockopt        :sockoptfunction    :sockoptdata        %sockopt-trampoline)
    (:opensocket     :opensocketfunction :opensocketdata     %opensocket-trampoline)
    (:closesocket    :closesocketfunction :closesocketdata   %closesocket-trampoline)
    (:ssl-ctx        :ssl-ctx-function   :ssl-ctx-data       %ssl-ctx-trampoline)
    (:prereq         :prereqfunction     :prereqdata         %prereq-trampoline)
    (:resolver-start :resolver-start-function :resolver-start-data
                                                            %resolver-start-trampoline)
    (:fnmatch        :fnmatch-function   :fnmatch-data       %fnmatch-trampoline)
    (:chunk-begin    :chunk-bgn-function :chunk-data         %chunk-begin-trampoline)
    (:chunk-end      :chunk-end-function :chunk-data         %chunk-end-trampoline)
    (:trailer        :trailerfunction    :trailerdata        %trailer-trampoline)
    (:hsts-read      :hstsreadfunction   :hstsreaddata       %hsts-read-trampoline)
    (:hsts-write     :hstswritefunction  :hstswritedata      %hsts-write-trampoline)
    (:ssh-key        :ssh-keyfunction    :ssh-keydata        %ssh-key-trampoline)
    (:ssh-host-key   :ssh-hostkeyfunction :ssh-hostkeydata   %ssh-host-key-trampoline)
    (:interleave     :interleavefunction :interleavedata     %interleave-trampoline)
    (:ioctl          :ioctlfunction      :ioctldata          %ioctl-trampoline)))

(defun callback-slot-accessor (slot)
  "The CALLBACK-STATE reader for SLOT."
  (ecase slot
    (:write #'cb-write) (:header #'cb-header) (:read #'cb-read)
    (:progress #'cb-progress) (:debug #'cb-debug) (:seek #'cb-seek)
    (:sockopt #'cb-sockopt) (:opensocket #'cb-opensocket)
    (:closesocket #'cb-closesocket) (:ssl-ctx #'cb-ssl-ctx)
    (:prereq #'cb-prereq) (:resolver-start #'cb-resolver-start)
    (:fnmatch #'cb-fnmatch) (:chunk-begin #'cb-chunk-begin)
    (:chunk-end #'cb-chunk-end) (:trailer #'cb-trailer)
    (:hsts-read #'cb-hsts-read) (:hsts-write #'cb-hsts-write)
    (:ssh-key #'cb-ssh-key) (:ssh-host-key #'cb-ssh-host-key)
    (:interleave #'cb-interleave) (:ioctl #'cb-ioctl)))

(defun (setf callback-slot) (function state slot)
  (ecase slot
    (:write (setf (cb-write state) function))
    (:header (setf (cb-header state) function))
    (:read (setf (cb-read state) function))
    (:progress (setf (cb-progress state) function))
    (:debug (setf (cb-debug state) function))
    (:seek (setf (cb-seek state) function))
    (:sockopt (setf (cb-sockopt state) function))
    (:opensocket (setf (cb-opensocket state) function))
    (:closesocket (setf (cb-closesocket state) function))
    (:ssl-ctx (setf (cb-ssl-ctx state) function))
    (:prereq (setf (cb-prereq state) function))
    (:resolver-start (setf (cb-resolver-start state) function))
    (:fnmatch (setf (cb-fnmatch state) function))
    (:chunk-begin (setf (cb-chunk-begin state) function))
    (:chunk-end (setf (cb-chunk-end state) function))
    (:trailer (setf (cb-trailer state) function))
    (:hsts-read (setf (cb-hsts-read state) function))
    (:hsts-write (setf (cb-hsts-write state) function))
    (:ssh-key (setf (cb-ssh-key state) function))
    (:ssh-host-key (setf (cb-ssh-host-key state) function))
    (:interleave (setf (cb-interleave state) function))
    (:ioctl (setf (cb-ioctl state) function))))

(defun find-callback-options (slot)
  (or (find slot *callback-options* :key #'first)
      (error "No such libcurl callback: ~S" slot)))

(defun callback-slot-names ()
  (mapcar #'first *callback-options*))

;;; Registry state does not survive an image dump: the keys refer to closures
;;; that were reachable only from handles which are themselves gone.
(defun %reset-callback-registry ()
  (setf *callback-table* (make-array 64 :initial-element nil)
        *callback-free-keys* '()
        *callback-next-key* 1))

(uiop:register-image-dump-hook '%reset-callback-registry)

(defun live-callback-count ()
  "How many callback states are registered.

Exported for leak tests: creating and destroying handles must return this to
where it started."
  (bt:with-lock-held (*callback-lock*)
    (count-if-not #'null *callback-table*)))
