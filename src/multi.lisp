;;;; src/multi.lisp — many transfers on one thread.
;;;;
;;;; The multi interface drives any number of easy handles concurrently without
;;;; a thread each.  Two ways to drive it are bound:
;;;;
;;;;   The polling loop -- curl_multi_perform plus curl_multi_poll -- which is
;;;;   what RUN-TRANSFERS uses and what almost every caller wants.
;;;;
;;;;   The socket API -- curl_multi_socket_action with socket and timer
;;;;   callbacks -- for integrating into an event loop that already exists.
;;;;
;;;; Results arrive as CURLMsg structures, whose payload is a union of a void*
;;;; and a CURLcode.  CFFI lays the union out correctly, so `result' is read
;;;; through a slot rather than a hard-coded offset -- but a test pins both the
;;;; size and the offset anyway, because reading it from the wrong place yields
;;;; a plausible wrong CURLcode rather than an obvious crash.
;;;;
;;;; Note that CURLM_CALL_MULTI_PERFORM is -1 and is *not* an error: it is a
;;;; legacy "call me again immediately" signal.  Any check that treats non-zero
;;;; as failure has to special-case it.

(in-package #:libcurl)

(cffi:defcfun ("curl_multi_init" %curl-multi-init) :pointer)
(cffi:defcfun ("curl_multi_cleanup" %curl-multi-cleanup) :int (multi :pointer))
(cffi:defcfun ("curl_multi_add_handle" %curl-multi-add-handle) :int
  (multi :pointer) (easy :pointer))
(cffi:defcfun ("curl_multi_remove_handle" %curl-multi-remove-handle) :int
  (multi :pointer) (easy :pointer))
(cffi:defcfun ("curl_multi_perform" %curl-multi-perform) :int
  (multi :pointer) (running :pointer))
(cffi:defcfun ("curl_multi_poll" %curl-multi-poll) :int
  (multi :pointer) (extra-fds :pointer) (extra-nfds :unsigned-int)
  (timeout-ms :int) (numfds :pointer))
(cffi:defcfun ("curl_multi_wait" %curl-multi-wait) :int
  (multi :pointer) (extra-fds :pointer) (extra-nfds :unsigned-int)
  (timeout-ms :int) (numfds :pointer))
(cffi:defcfun ("curl_multi_wakeup" %curl-multi-wakeup) :int (multi :pointer))
(cffi:defcfun ("curl_multi_info_read" %curl-multi-info-read) :pointer
  (multi :pointer) (queued :pointer))
(cffi:defcfun ("curl_multi_timeout" %curl-multi-timeout) :int
  (multi :pointer) (timeout :pointer))
(cffi:defcfun ("curl_multi_socket_action" %curl-multi-socket-action) :int
  (multi :pointer) (socket curl-socket-t) (events :int) (running :pointer))
(cffi:defcfun ("curl_multi_assign" %curl-multi-assign) :int
  (multi :pointer) (socket curl-socket-t) (data :pointer))

(defclass multi-handle ()
  ((pointer :initarg :pointer :reader multi-pointer)
   (callbacks :reader multi-callbacks :initform nil)
   (transfers :accessor multi-transfers :initform '()
              :documentation "Easy handles currently added, kept reachable: a
handle collected while libcurl still has it would be catastrophic, and libcurl
holds only the raw CURL*.")
   (closed-p :accessor multi-closed-p :initform nil))
  (:documentation "A CURLM*, driving several easy handles at once."))

(defmethod print-object ((multi multi-handle) stream)
  (print-unreadable-object (multi stream :type t :identity t)
    (if (multi-closed-p multi)
        (write-string "closed" stream)
        (format stream "~D transfer~:P" (length (multi-transfers multi))))))

(defconstant +curlm-call-multi-perform+ -1)

(defun %check-multi (code)
  "Signal unless CODE is success.  CURLM_CALL_MULTI_PERFORM is success."
  (unless (or (zerop code) (= code +curlm-call-multi-perform+))
    (error 'multi-error
           :code code
           :code-name (curlcode-keyword code 'curlmcode)
           :message (%curl-multi-strerror code)))
  code)

(defun make-multi (&rest options)
  "Create a multi handle.  OPTIONS are CURLMOPT_ keywords and values."
  (let ((pointer (%curl-multi-init)))
    (when (cffi:null-pointer-p pointer)
      (error 'multi-error :message "curl_multi_init returned NULL"))
    (let ((multi (make-instance 'multi-handle :pointer pointer))
          (completed nil))
      (unwind-protect
           (let ((state (make-callback-state :handle-pointer pointer)))
             (setf (slot-value multi 'callbacks) state
                   (cb-handle state) multi)
             (register-callback-state state)
             (loop for (option value) on options by #'cddr
                   do (multi-setopt multi option value))
             (setf completed t)
             multi)
        (unless completed (close-multi multi))))))

(defun close-multi (multi)
  "Remove every transfer and release MULTI.  Idempotent.

Handles are removed first: curl_multi_cleanup on a multi that still holds easy
handles leaves those handles in a state where their own cleanup misbehaves."
  (unless (multi-closed-p multi)
    (setf (multi-closed-p multi) t)
    (dolist (easy (copy-list (multi-transfers multi)))
      (ignore-errors (remove-transfer multi easy)))
    (%curl-multi-cleanup (multi-pointer multi))
    (when (multi-callbacks multi)
      (release-callback-state (cb-key (multi-callbacks multi)))))
  (values))

(defmacro with-multi ((var &rest options) &body body)
  "Run BODY with VAR bound to a multi handle, released on exit."
  `(let ((,var (make-multi ,@options)))
     (unwind-protect (progn ,@body)
       (close-multi ,var))))

(defun multi-setopt (multi option value)
  "Set a CURLMOPT_ option.  OPTION is a keyword such as :MAX-TOTAL-CONNECTIONS."
  (let ((entry (ensure-multi-option option))
        (pointer (multi-pointer multi)))
    (%check-multi
     (ecase (option-kind entry)
       ((:long :values) (%multi-setopt-long pointer (option-id entry)
                                            (%coerce-to-long value)))
       ((:off-t) (%multi-setopt-off-t pointer (option-id entry) value))
       ((:objectpoint :cbpoint :functionpoint :stringpoint :slistpoint :blob)
        (%multi-setopt-pointer pointer (option-id entry)
                               (if (null value)
                                   (cffi:null-pointer)
                                   value)))))
    value))

(defun add-transfer (multi easy)
  "Add EASY to MULTI.  The handle must not already belong to another multi."
  (check-open easy)
  (%check-multi (%curl-multi-add-handle (multi-pointer multi)
                                        (handle-pointer easy)))
  (push easy (multi-transfers multi))
  easy)

(defun remove-transfer (multi easy)
  "Remove EASY from MULTI.  Safe to call on a handle that was never added."
  (%check-multi (%curl-multi-remove-handle (multi-pointer multi)
                                           (handle-pointer easy)))
  (setf (multi-transfers multi) (delete easy (multi-transfers multi)))
  easy)

(defun multi-perform (multi)
  "Drive all transfers as far as they will go without blocking.

Returns the number still running."
  (cffi:with-foreign-object (running :int)
    (setf (cffi:mem-ref running :int) 0)
    (%check-multi (%curl-multi-perform (multi-pointer multi) running))
    (cffi:mem-ref running :int)))

(defun multi-poll (multi &key (timeout-ms 1000))
  "Wait until something can be done, or TIMEOUT-MS elapses.

Returns how many file descriptors were ready.  Unlike curl_multi_wait, poll
handles the no-descriptors case by sleeping rather than spinning, which is why
it is the default here."
  (cffi:with-foreign-object (numfds :int)
    (setf (cffi:mem-ref numfds :int) 0)
    (%check-multi (%curl-multi-poll (multi-pointer multi) (cffi:null-pointer) 0
                                    timeout-ms numfds))
    (cffi:mem-ref numfds :int)))

(defun multi-wait (multi &key (timeout-ms 1000))
  "Like MULTI-POLL, but returns immediately when there is nothing to wait on.

A loop built on this must guard against spinning; MULTI-POLL usually wants to
be used instead."
  (cffi:with-foreign-object (numfds :int)
    (setf (cffi:mem-ref numfds :int) 0)
    (%check-multi (%curl-multi-wait (multi-pointer multi) (cffi:null-pointer) 0
                                    timeout-ms numfds))
    (cffi:mem-ref numfds :int)))

(defun multi-wakeup (multi)
  "Interrupt a MULTI-POLL in progress, from another thread."
  (%check-multi (%curl-multi-wakeup (multi-pointer multi)))
  multi)

(defun multi-timeout (multi)
  "How long libcurl is willing to wait, in milliseconds, or NIL for \"no limit\"."
  (cffi:with-foreign-object (timeout :long)
    (setf (cffi:mem-ref timeout :long) 0)
    (%check-multi (%curl-multi-timeout (multi-pointer multi) timeout))
    (let ((value (cffi:mem-ref timeout :long)))
      (unless (minusp value) value))))

(defstruct (transfer-result (:conc-name result-))
  "The outcome of one transfer, as reported by a CURLMsg."
  handle
  (code 0)
  (code-name nil)
  (condition nil))

(defun result-successful-p (result)
  (and (zerop (result-code result)) (null (result-condition result))))

(defun read-multi-messages (multi)
  "Drain the message queue.  Returns a list of TRANSFER-RESULT.

Each completed transfer is also removed from MULTI, since libcurl considers it
finished and holding it adds nothing."
  (let ((raw '()))
    ;; Drain the queue first, copying the values out, and only then remove the
    ;; handles.  Removing one *while* iterating invalidates the queue being
    ;; walked -- the CURLMsg points into the easy handle's own memory, and
    ;; curl_multi_remove_handle is documented to end its validity.  Doing it
    ;; inline made curl_multi_info_read re-report a message it had already
    ;; handed back, so a run with several completed transfers never terminated.
    (cffi:with-foreign-object (queued :int)
      (loop for message = (%curl-multi-info-read (multi-pointer multi) queued)
            until (cffi:null-pointer-p message)
            do (let ((kind (cffi:foreign-slot-value message '(:struct curl-msg) 'msg)))
                 ;; CURLMSG_DONE is the only message libcurl defines.
                 (when (= kind (curlcode-value :done 'curl-msg-type))
                   (push (cons (cffi:foreign-slot-value
                                message '(:struct curl-msg) 'easy-handle)
                               ;; The union member; CFFI knows the offset.
                               (cffi:foreign-slot-value
                                (cffi:foreign-slot-pointer
                                 message '(:struct curl-msg) 'data)
                                '(:union curl-msg-data) 'result))
                         raw)))))
    (loop for (easy-pointer . code) in (nreverse raw)
          for easy = (handle-from-pointer easy-pointer)
          collect (make-transfer-result
                   :handle easy
                   :code code
                   :code-name (curlcode-keyword code)
                   ;; A condition signalled in a callback outranks libcurl's
                   ;; code, exactly as in PERFORM.
                   :condition (when easy (cb-condition (handle-callbacks easy))))
          do (when easy (remove-transfer multi easy)))))

(defun run-transfers (multi &key (timeout-ms 1000))
  "Drive MULTI until every transfer has finished.  Returns their results.

The ordinary way to use the multi interface: perform, wait for something to
happen, repeat.  Results come back as TRANSFER-RESULT structs rather than as
signalled conditions, because with several transfers in flight a failure is a
fact about one of them rather than about the call -- see SIGNAL-FAILED-TRANSFERS
for the other behaviour."
  (let ((results '()))
    (loop
      (let ((running (multi-perform multi)))
        (setf results (append results (read-multi-messages multi)))
        (when (zerop running)
          ;; One last drain: the message for the final transfer is queued by
          ;; the same perform that dropped the running count to zero.
          (setf results (append results (read-multi-messages multi)))
          (return))
        (multi-poll multi :timeout-ms timeout-ms)))
    results))

(defun signal-failed-transfers (results)
  "Signal the first failure in RESULTS, or return RESULTS unchanged.

For callers who would rather have a condition than inspect every result."
  (dolist (result results results)
    (unless (result-successful-p result)
      (let ((handle (result-handle result)))
        (if (result-condition result)
            (error 'callback-error
                   :cause (result-condition result)
                   :kind (cb-condition-kind (handle-callbacks handle))
                   :code (result-code result)
                   :code-name (result-code-name result)
                   :message (%curl-easy-strerror (result-code result)))
            (error 'easy-error
                   :code (result-code result)
                   :code-name (result-code-name result)
                   :message (%curl-easy-strerror (result-code result))
                   :detail (when handle
                             (error-buffer-text (handle-error-buffer handle)))
                   :url (when handle
                          (ignore-errors (getinfo handle :effective-url)))))))))

;;; The socket API ------------------------------------------------------------
;;;
;;; For embedding in an existing event loop: libcurl reports which sockets it
;;; wants watched through CURLMOPT_SOCKETFUNCTION and how long to wait through
;;; CURLMOPT_TIMERFUNCTION, and the loop calls SOCKET-ACTION as things happen.

(define-trampoline %socket-trampoline :int
    ((easy :pointer) (socket curl-socket-t) (what :int)
     (userdata :pointer) (socketp :pointer))
    (:state-var state :userdata userdata :kind :socket :failure 1)
  (let ((function (cb-socket state)))
    (if (null function)
        0
        (progn (funcall function socket
                        (case what
                          (#.+curl-poll-in+ :in)
                          (#.+curl-poll-out+ :out)
                          (#.+curl-poll-inout+ :in-out)
                          (#.+curl-poll-remove+ :remove)
                          (t :none))
                        (handle-from-pointer easy)
                        socketp)
               0))))

(define-trampoline %timer-trampoline :int
    ((multi :pointer) (timeout-ms :long) (userdata :pointer))
    (:state-var state :userdata userdata :kind :timer :failure -1)
  (let ((function (cb-timer state)))
    (if (null function)
        0
        (progn (funcall function (unless (minusp timeout-ms) timeout-ms))
               0))))

(defun (setf multi-socket-function) (function multi)
  "Install the callback libcurl uses to say which sockets to watch.

Called as (FUNCTION socket what easy-handle socket-data), where WHAT is :IN,
:OUT, :IN-OUT or :REMOVE."
  (let ((state (multi-callbacks multi)))
    (setf (cb-socket state) function)
    (if function
        (progn
          (multi-setopt multi :socketfunction (cffi:get-callback '%socket-trampoline))
          (multi-setopt multi :socketdata (callback-key-pointer (cb-key state))))
        (progn (multi-setopt multi :socketfunction nil)
               (multi-setopt multi :socketdata nil))))
  function)

(defun (setf multi-timer-function) (function multi)
  "Install the callback libcurl uses to ask for a timer.

Called as (FUNCTION timeout-ms), where NIL means \"cancel the timer\"."
  (let ((state (multi-callbacks multi)))
    (setf (cb-timer state) function)
    (if function
        (progn
          (multi-setopt multi :timerfunction (cffi:get-callback '%timer-trampoline))
          (multi-setopt multi :timerdata (callback-key-pointer (cb-key state))))
        (progn (multi-setopt multi :timerfunction nil)
               (multi-setopt multi :timerdata nil))))
  function)

(defconstant +curl-socket-timeout+ -1
  "The socket value meaning \"the timeout fired\" rather than a real socket.")

(defun socket-action (multi &key (socket +curl-socket-timeout+) events)
  "Tell libcurl that something happened on SOCKET.  Returns the running count.

EVENTS is a list of :IN, :OUT and :ERR.  Called with no arguments it reports a
timeout, which is what an expired timer should do."
  (cffi:with-foreign-object (running :int)
    (setf (cffi:mem-ref running :int) 0)
    (let ((mask 0))
      (dolist (event (alexandria:ensure-list events))
        (setf mask (logior mask (ecase event
                                  (:in +curl-cselect-in+)
                                  (:out +curl-cselect-out+)
                                  (:err +curl-cselect-err+)))))
      (%check-multi (%curl-multi-socket-action (multi-pointer multi) socket mask
                                               running)))
    (cffi:mem-ref running :int)))
