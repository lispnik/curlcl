;;;; src/client-session.lisp — connection reuse and shared state.
;;;;
;;;; Two things make a sequence of requests to the same host fast: reusing the
;;;; TCP and TLS connection, and reusing the DNS answer.  libcurl gives both
;;;; for free *within* one easy handle, because a handle carries its own
;;;; connection cache -- so a session is, at bottom, a pool of easy handles
;;;; that get reset rather than closed.
;;;;
;;;; A share handle covering CURL_LOCK_DATA_CONNECT extends that across
;;;; handles, which is what makes the pool more than a formality: without it,
;;;; two requests taken from the pool at once would open two connections to the
;;;; same host, and the share is also what lets cookies and TLS sessions be
;;;; common to the whole session.
;;;;
;;;; curl_easy_reset is deliberate here.  It clears every option -- which is
;;;; why the binding re-installs its own error buffer, private key and
;;;; callbacks afterwards -- but it keeps the connection cache, which closing
;;;; and reopening the handle would throw away.

(in-package #:curlcl)

(defgeneric session-share (session)
  (:documentation
   "The SHARE-HANDLE backing this session.

Shares cookies, DNS, TLS sessions and the connection cache across every handle
in the pool -- sharing CONNECT is what makes the pooling real rather than
per-handle."))

(defgeneric session-defaults (session)
  (:documentation
   "Options merged into every request made through this session.  SETFable.

A request that names the same option wins; these are defaults, not
overrides."))

(defgeneric session-closed-p (session)
  (:documentation "True once CLOSE-SESSION has run."))

(defclass session ()
  ((share :reader session-share)
   (pool :initform '() :accessor session-pool
         :documentation "Idle handles, ready to be reset and reused.")
   (lock :initform (bt:make-lock "curlcl session pool") :reader session-lock)
   (max-idle :initarg :max-idle :reader session-max-idle
             :documentation "How many idle handles to keep; beyond this,
released handles are closed rather than pooled.")
   (defaults :initarg :defaults :initform '() :accessor session-defaults
             :documentation "Options merged into every request made through
this session, unless the request overrides them.")
   (live-count :initform 0 :accessor session-live-count)
   (closed-p :initform nil :accessor session-closed-p))
  (:documentation "A pool of easy handles sharing connections and cookies."))

(defmethod print-object ((session session) stream)
  (print-unreadable-object (session stream :type t :identity t)
    (if (session-closed-p session)
        (write-string "closed" stream)
        (format stream "~D idle, ~D live"
                (length (session-pool session)) (session-live-count session)))))

(defun make-session (&key (max-idle 8) defaults (cookies t) (share-connections t))
  "Create a session.

DEFAULTS is a plist of REQUEST options applied to every request made through
it -- a base set of headers, a timeout, a proxy.  COOKIES adds the cookie jar
to the shared state, so a login on one request is visible to the next.
SHARE-CONNECTIONS is what makes the pool actually pool: without it each handle
keeps its own connection cache."
  (let* ((shared (append '(:dns :ssl-session)
                         (when share-connections '(:connect))
                         (when cookies '(:cookie))))
         (session (make-instance 'session :max-idle max-idle :defaults defaults)))
    (setf (slot-value session 'share) (make-share :share shared))
    session))

(defun close-session (session)
  "Close every pooled handle and release the share.  Idempotent.

The share is released last: it must outlive every handle attached to it, and
curl_share_cleanup answers CURLSHE_IN_USE rather than pulling shared state out
from under one."
  (unless (session-closed-p session)
    (setf (session-closed-p session) t)
    (let ((handles (bt:with-lock-held ((session-lock session))
                     (prog1 (session-pool session)
                       (setf (session-pool session) '())))))
      (dolist (handle handles)
        (ignore-errors (close-handle handle))))
    (ignore-errors (close-share (session-share session))))
  (values))

(defmacro with-session ((var &rest options) &body body)
  "Run BODY with VAR bound to a session, closed on exit."
  `(let ((,var (make-session ,@options)))
     (unwind-protect (progn ,@body)
       (close-session ,var))))

(defun acquire-handle (session)
  "Take an idle handle from SESSION, or make one.

A pooled handle has already been reset, so it arrives configured only with what
the binding needs and with its connection cache intact."
  (when (session-closed-p session)
    (error 'curl-error :message "This session has been closed."))
  (let ((handle (bt:with-lock-held ((session-lock session))
                  (incf (session-live-count session))
                  (pop (session-pool session)))))
    (or handle
        (let ((new (make-easy-handle)))
          (attach-share new (session-share session))
          new))))

(defun release-handle (session handle)
  "Return HANDLE to SESSION, or close it if the pool is full.

Reset rather than closed: the connection cache survives a reset and is the
whole point of keeping the handle."
  (bt:with-lock-held ((session-lock session))
    (decf (session-live-count session)))
  (cond
    ((or (session-closed-p session) (handle-closed-p handle))
     (ignore-errors (close-handle handle)))
    (t
     (handler-case
         (progn
           (reset-handle handle)
           ;; RESET-HANDLE cleared the share along with everything else.
           (attach-share handle (session-share session))
           (let ((pooled (bt:with-lock-held ((session-lock session))
                           (cond ((< (length (session-pool session))
                                     (session-max-idle session))
                                  (push handle (session-pool session))
                                  t)
                                 (t nil)))))
             (unless pooled (close-handle handle))))
       ;; A handle that will not reset cleanly is not one to keep.
       (error () (ignore-errors (close-handle handle))))))
  (values))

(defmacro with-session-handle ((var session) &body body)
  "Run BODY with VAR bound to a handle from SESSION, returned on exit."
  (alexandria:once-only (session)
    `(let ((,var (acquire-handle ,session)))
       (unwind-protect (progn ,@body)
         (release-handle ,session ,var)))))

(defun session-cookies (session)
  "Every cookie the session holds, in Netscape cookie-file format.

Read through a handle attached to the shared jar, since the jar itself is not
directly addressable."
  (with-session-handle (handle session)
    (getinfo handle :cookielist)))

(defun clear-session-cookies (session)
  "Discard every cookie the session holds."
  (with-session-handle (handle session)
    (setopt handle :cookielist "ALL"))
  (values))
