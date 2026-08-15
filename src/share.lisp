;;;; src/share.lisp — sharing a cookie jar, DNS cache and connections.
;;;;
;;;; A share handle is what lets several easy handles pool their DNS results,
;;;; TLS sessions, cookies and -- since 7.57.0 -- their *connections*.  That
;;;; last one is what makes connection reuse work across handles rather than
;;;; only within one, which is the whole basis of the session layer.
;;;;
;;;; libcurl requires lock and unlock callbacks before a share may be used from
;;;; more than one thread, and provides none itself.  Supplying them is not
;;;; optional here: a session hands its share to whatever thread performs a
;;;; request, so the binding installs a lock per shared data type and always
;;;; registers the callbacks.  Per data type rather than one global lock,
;;;; because the cookie jar and the connection cache are contended by different
;;;; work and there is no reason to serialise them against each other.

(in-package #:libcurl)

(cffi:defcfun ("curl_share_init" %curl-share-init) :pointer)
(cffi:defcfun ("curl_share_cleanup" %curl-share-cleanup) :int (share :pointer))

(defconstant +curlshopt-share+ 1)
(defconstant +curlshopt-unshare+ 2)
(defconstant +curlshopt-lockfunc+ 3)
(defconstant +curlshopt-unlockfunc+ 4)
(defconstant +curlshopt-userdata+ 5)

;;; One lock per curl_lock_data value.  CURL_LOCK_DATA_LAST is 8, so the vector
;;; is sized to cover every value libcurl can pass.
(defconstant +lock-data-count+ 8)

(defclass share-handle ()
  ((pointer :initarg :pointer :reader share-pointer)
   (callbacks :reader share-callbacks :initform nil)
   (locks :reader share-locks
          :initform (coerce (loop for i below +lock-data-count+
                                  collect (bt:make-lock
                                           (format nil "libcurl share ~D" i)))
                            'vector))
   (closed-p :accessor share-closed-p :initform nil))
  (:documentation "A CURLSH*, shared state for a group of easy handles."))

(defmethod print-object ((share share-handle) stream)
  (print-unreadable-object (share stream :type t :identity t)
    (when (share-closed-p share) (write-string "closed" stream))))

(defun %check-share (code)
  (unless (zerop code)
    (error 'share-error
           :code code
           :code-name (curlcode-keyword code 'curlshcode)
           :message (%curl-share-strerror code)))
  code)

;;; The lock callbacks.  These are not user-supplied closures, so they do not
;;; go through the callback-slot machinery -- they find the share through the
;;; registry and take its own lock directly.  Both return void, so there is no
;;; failure value to report: an error here can only be a bug in this file.

(cffi:defcallback %share-lock-trampoline :void
    ((handle :pointer) (data :int) (access :int) (userdata :pointer))
  (declare (ignorable handle access))
  (let ((state (%lookup-state userdata)))
    (when state
      (let ((share (cb-handle state)))
        (when (and share (< -1 data +lock-data-count+))
          ;; libcurl distinguishes shared from exclusive access, but a plain
          ;; mutex for both is correct if coarse, and correctness is what this
          ;; is for.
          (bt:acquire-lock (aref (share-locks share) data)))))))

(cffi:defcallback %share-unlock-trampoline :void
    ((handle :pointer) (data :int) (userdata :pointer))
  (declare (ignorable handle))
  (let ((state (%lookup-state userdata)))
    (when state
      (let ((share (cb-handle state)))
        (when (and share (< -1 data +lock-data-count+))
          (bt:release-lock (aref (share-locks share) data)))))))

(defparameter *default-shared-data* '(:dns :ssl-session :connect)
  "What a share covers unless told otherwise.

Cookies are deliberately not in this list: sharing a cookie jar between
unrelated transfers is a behaviour change, not just an optimisation, so it has
to be asked for.  The session layer asks for it.")

(defun make-share (&key (share *default-shared-data*))
  "Create a share handle covering the given data types.

SHARE is a list drawn from :COOKIE, :DNS, :SSL-SESSION, :CONNECT, :PSL and
:HSTS.  Lock callbacks are always installed, so the result is safe to use from
several threads."
  (let ((pointer (%curl-share-init)))
    (when (cffi:null-pointer-p pointer)
      (error 'share-error :message "curl_share_init returned NULL"))
    (let ((object (make-instance 'share-handle :pointer pointer))
          (completed nil))
      (unwind-protect
           (let ((state (make-callback-state :handle-pointer pointer)))
             (setf (slot-value object 'callbacks) state
                   (cb-handle state) object)
             (register-callback-state state)
             (%check-share
              (%share-setopt-pointer pointer +curlshopt-lockfunc+
                                     (cffi:get-callback '%share-lock-trampoline)))
             (%check-share
              (%share-setopt-pointer pointer +curlshopt-unlockfunc+
                                     (cffi:get-callback '%share-unlock-trampoline)))
             (%check-share
              (%share-setopt-pointer pointer +curlshopt-userdata+
                                     (callback-key-pointer (cb-key state))))
             (dolist (data share)
               (share-data object data))
             (setf completed t)
             object)
        (unless completed (close-share object))))))

(defun share-data (share data)
  "Add DATA to what SHARE covers."
  ;; CURLSHOPT_SHARE takes the curl_lock_data value as a long through varargs,
  ;; one call per type -- it is not a bitmask.
  (%check-share (%share-setopt-long (share-pointer share) +curlshopt-share+
                                    (curlcode-value data 'curl-lock-data)))
  share)

(defun unshare-data (share data)
  "Remove DATA from what SHARE covers."
  (%check-share (%share-setopt-long (share-pointer share) +curlshopt-unshare+
                                    (curlcode-value data 'curl-lock-data)))
  share)

(defun close-share (share)
  "Release SHARE.  Signals if any easy handle is still attached.

curl_share_cleanup is the one cleanup function in libcurl that can fail: it
answers CURLSHE_IN_USE rather than pulling shared state out from under a live
transfer."
  (unless (share-closed-p share)
    (setf (share-closed-p share) t)
    (let ((code (%curl-share-cleanup (share-pointer share))))
      (when (share-callbacks share)
        (release-callback-state (cb-key (share-callbacks share))))
      (%check-share code)))
  (values))

(defmacro with-share ((var &rest options) &body body)
  "Run BODY with VAR bound to a share handle, released on exit."
  `(let ((,var (make-share ,@options)))
     (unwind-protect (progn ,@body)
       (close-share ,var))))

(defun attach-share (handle share)
  "Attach SHARE to HANDLE, so it joins the shared caches.

The share is also stored on the handle, which keeps it reachable: a share
collected or closed while a transfer is using it would take the transfer's
connection cache with it."
  (check-open handle)
  (setopt handle :share (share-pointer share))
  (setf (handle-share handle) share)
  handle)

(defun detach-share (handle)
  "Detach whatever share HANDLE is attached to."
  (check-open handle)
  (setopt handle :share nil)
  (setf (handle-share handle) nil)
  handle)
