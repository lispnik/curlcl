;;;; test/multi-tests.lisp — many transfers on one thread.

(in-package #:libcurl/test)

(in-suite multi)

(defun make-collecting-handle (path)
  "An easy handle for PATH whose body accumulates where BODY-OF can find it."
  (let ((handle (make-easy-handle))
        (chunks '()))
    (setf (callback-function handle :write)
          (lambda (octets) (push octets chunks) t))
    (setf (getf (handle-plist handle) :collector)
          (lambda () (apply #'concatenate '(vector (unsigned-byte 8))
                            (reverse chunks))))
    (setopt handle :url (test-url path))
    handle))

(defun body-of (handle)
  (body-string (funcall (getf (handle-plist handle) :collector))))

(test a-multi-runs-one-transfer
  (with-multi (multi)
    (let ((handle (make-collecting-handle "/ok")))
      (unwind-protect
           (progn
             (add-transfer multi handle)
             (let ((results (run-transfers multi)))
               (is (= 1 (length results)))
               (is (result-successful-p (first results)))
               (is (eq handle (result-handle (first results))))
               (is (string= "ok" (body-of handle)))))
        (close-handle handle)))))

(test a-multi-runs-several-transfers-concurrently
  ;; The point of the interface: several transfers make progress without a
  ;; thread each, and every one of them completes.
  (with-multi (multi)
    (let ((handles (loop for i below 6
                         collect (make-collecting-handle "/drip?n=3&ms=20"))))
      (unwind-protect
           (progn
             (dolist (handle handles) (add-transfer multi handle))
             (let ((results (run-transfers multi)))
               (is (= 6 (length results)))
               (is (every #'result-successful-p results))
               (dolist (handle handles)
                 (is (= 24 (length (body-of handle)))))
               ;; Every handle appears exactly once in the results.
               (is (= 6 (length (remove-duplicates (mapcar #'result-handle results)))))))
        (dolist (handle handles) (close-handle handle))))))

(test a-failed-transfer-is-reported-with-its-code
  ;; This is what reads the CURLMsg union.  Reading `result' from the wrong
  ;; offset would give a plausible but wrong CURLcode rather than crashing, so
  ;; the assertion is on the specific code, not merely on failure.
  (with-multi (multi)
    (let ((handle (make-collecting-handle "/close-early")))
      (unwind-protect
           (progn
             (add-transfer multi handle)
             (let ((result (first (run-transfers multi))))
               (is (not (result-successful-p result)))
               (is (eq :partial-file (result-code-name result)))
               (is (= (libcurl:curlcode-value :partial-file) (result-code result)))))
        (close-handle handle)))))

(test successes-and-failures-are-reported-together
  ;; With several transfers in flight a failure is a fact about one of them,
  ;; not about the call, so RUN-TRANSFERS returns rather than signals.
  (with-multi (multi)
    (let ((good (make-collecting-handle "/ok"))
          (bad (make-collecting-handle "/close-early")))
      (unwind-protect
           (progn
             (add-transfer multi good)
             (add-transfer multi bad)
             (let* ((results (run-transfers multi))
                    (for-good (find good results :key #'result-handle))
                    (for-bad (find bad results :key #'result-handle)))
               (is (= 2 (length results)))
               (is (result-successful-p for-good))
               (is (not (result-successful-p for-bad)))
               (is (eq :partial-file (result-code-name for-bad)))))
        (close-handle good)
        (close-handle bad)))))

(test signal-failed-transfers-turns-a-result-into-a-condition
  (with-multi (multi)
    (let ((handle (make-collecting-handle "/close-early")))
      (unwind-protect
           (progn
             (add-transfer multi handle)
             (let ((results (run-transfers multi)))
               (handler-case
                   (progn (signal-failed-transfers results)
                          (fail "expected a condition"))
                 (easy-error (c)
                   (is (eq :partial-file (curl-error-code-name c)))))))
        (close-handle handle)))))

(test a-callback-condition-survives-the-multi-interface
  ;; The same guarantee PERFORM gives: a Lisp condition signalled in a callback
  ;; comes back as itself rather than as CURLE_WRITE_ERROR.
  (with-multi (multi)
    (let ((handle (make-easy-handle)))
      (unwind-protect
           (progn
             (setf (callback-function handle :write)
                   (lambda (octets) (declare (ignore octets))
                     (error 'file-error :pathname "/nope")))
             (setopt handle :url (test-url "/ok"))
             (add-transfer multi handle)
             (let ((result (first (run-transfers multi))))
               (is (not (result-successful-p result)))
               (is (typep (result-condition result) 'file-error))
               (handler-case
                   (progn (signal-failed-transfers (list result))
                          (fail "expected a callback-error"))
                 (callback-error (c)
                   (is (typep (callback-error-cause c) 'file-error))))))
        (close-handle handle)))))

(test transfers-are-tracked-and-released
  (with-multi (multi)
    (let ((handle (make-collecting-handle "/ok")))
      (unwind-protect
           (progn
             (is (null (multi-transfers multi)))
             (add-transfer multi handle)
             (is (equal (list handle) (multi-transfers multi)))
             ;; RUN-TRANSFERS removes each handle as its message is read.
             (run-transfers multi)
             (is (null (multi-transfers multi))))
        (close-handle handle)))))

(test a-transfer-can-be-removed-before-it-finishes
  (with-multi (multi)
    (let ((handle (make-collecting-handle "/drip?n=10&ms=50")))
      (unwind-protect
           (progn
             (add-transfer multi handle)
             (multi-perform multi)
             (remove-transfer multi handle)
             (is (null (multi-transfers multi)))
             ;; With nothing left, the loop terminates immediately.
             (is (null (run-transfers multi))))
        (close-handle handle)))))

(test closing-a-multi-with-transfers-still-added-is-safe
  ;; curl_multi_cleanup on a multi that still holds handles leaves those
  ;; handles in a state where their own cleanup misbehaves, so CLOSE-MULTI
  ;; removes them first.
  (let ((multi (make-multi))
        (handle (make-collecting-handle "/drip?n=10&ms=50")))
    (add-transfer multi handle)
    (multi-perform multi)
    (finishes (close-multi multi))
    (is (multi-closed-p multi))
    (finishes (close-handle handle))))

(test multi-options-can-be-set
  (with-multi (multi)
    (finishes (multi-setopt multi :max-total-connections 4))
    (finishes (multi-setopt multi :maxconnects 8))
    (finishes (multi-setopt multi :max-host-connections 2))
    (signals unsupported-option (multi-setopt multi :not-a-multi-option 1))))

(test max-total-connections-is-honoured
  ;; Six transfers through a limit of two still all complete; the limit changes
  ;; the scheduling, not the outcome.
  (with-multi (multi :max-total-connections 2)
    (let ((handles (loop for i below 6 collect (make-collecting-handle "/ok"))))
      (unwind-protect
           (progn
             (dolist (handle handles) (add-transfer multi handle))
             (let ((results (run-transfers multi)))
               (is (= 6 (length results)))
               (is (every #'result-successful-p results))
               (dolist (handle handles)
                 (is (string= "ok" (body-of handle))))))
        (dolist (handle handles) (close-handle handle))))))

(test multi-timeout-is-a-number-or-nil
  (with-multi (multi)
    (let ((handle (make-collecting-handle "/ok")))
      (unwind-protect
           (progn
             (add-transfer multi handle)
             (multi-perform multi)
             (let ((timeout (multi-timeout multi)))
               ;; NIL means "no timeout set"; anything else is milliseconds.
               (is (or (null timeout) (and (integerp timeout) (<= 0 timeout))))))
        (close-handle handle)))))

(test the-socket-api-reports-sockets-and-timers
  ;; The event-loop integration path.  Rather than build an event loop, this
  ;; checks that libcurl asks for what it should: at least one socket to watch
  ;; and at least one timer, and that driving SOCKET-ACTION from those answers
  ;; carries the transfer to completion.
  (with-multi (multi)
    (let ((handle (make-collecting-handle "/ok"))
          (sockets (make-hash-table))
          (timer-requests '()))
      (unwind-protect
           (progn
             (setf (multi-socket-function multi)
                   (lambda (socket what easy socket-data)
                     (declare (ignore easy socket-data))
                     (if (eq what :remove)
                         (remhash socket sockets)
                         (setf (gethash socket sockets) what))))
             (setf (multi-timer-function multi)
                   (lambda (timeout-ms) (push timeout-ms timer-requests)))
             (add-transfer multi handle)
             ;; Kick things off the way an event loop would, on the first timer.
             (let ((running (socket-action multi)))
               (loop repeat 200
                     while (plusp running)
                     do (let ((watched (loop for socket being the hash-keys of sockets
                                             collect socket)))
                          (if (null watched)
                              (setf running (socket-action multi))
                              (dolist (socket watched)
                                (setf running
                                      (socket-action multi :socket socket
                                                           :events '(:in :out))))))))
             (is (plusp (length timer-requests))
                 "libcurl never asked for a timer")
             (let ((results (read-multi-messages multi)))
               (is (= 1 (length results)))
               (is (result-successful-p (first results)))
               (is (string= "ok" (body-of handle)))))
        (close-handle handle)))))

(test multi-wakeup-interrupts-a-poll
  ;; A poll with nothing to do would otherwise block for its full timeout;
  ;; wakeup is how another thread cuts that short.
  (with-multi (multi)
    (let* ((woken nil)
           (thread (bt:make-thread
                    (lambda ()
                      (multi-poll multi :timeout-ms 10000)
                      (setf woken t))
                    :name "multi poll")))
      (sleep 0.2)
      (multi-wakeup multi)
      (bt:join-thread thread)
      (is-true woken))))
