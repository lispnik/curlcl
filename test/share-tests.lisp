;;;; test/share-tests.lisp — shared cookies, DNS, TLS sessions and connections.

(in-package #:libcurl/test)

(in-suite share)

(test a-share-can-be-created-and-closed
  (let ((share (make-share)))
    (is (not (share-closed-p share)))
    (close-share share)
    (is (share-closed-p share))
    (finishes (close-share share))))

(test with-share-releases-on-a-non-local-exit
  (let ((captured nil))
    (ignore-errors
     (with-share (share)
       (setf captured share)
       (error "unwinding")))
    (is (share-closed-p captured))))

(test lock-callbacks-are-installed-so-a-share-is-thread-safe
  ;; libcurl provides no locking of its own, and a share used from more than
  ;; one thread without callbacks corrupts its caches.  The binding always
  ;; installs them, so this checks the registry entry exists rather than
  ;; leaving it to the caller to remember.
  (with-share (share)
    (is (not (null (libcurl::share-callbacks share))))
    (is (plusp (libcurl::cb-key (libcurl::share-callbacks share))))
    (is (= libcurl::+lock-data-count+ (length (libcurl::share-locks share))))))

(test a-share-releases-its-registry-key
  (let ((before (libcurl::live-callback-count)))
    (with-share (share)
      (is (not (null share)))
      (is (= (1+ before) (libcurl::live-callback-count))))
    (is (= before (libcurl::live-callback-count)))))

(test data-types-can-be-added-and-removed
  (with-share (share :share '(:dns))
    (finishes (share-data share :cookie))
    (finishes (unshare-data share :cookie))
    (finishes (share-data share :ssl-session))))

(test attaching-a-share-keeps-it-reachable-from-the-handle
  ;; A share collected or closed while a transfer is using it would take the
  ;; connection cache with it, so the handle holds a reference.
  (with-share (share)
    (with-easy (handle)
      (attach-share handle share)
      (is (eq share (handle-share handle)))
      (detach-share handle)
      (is (null (handle-share handle))))))

(test cookies-are-shared-between-handles
  ;; The observable point of a share: one handle receives a cookie and another,
  ;; which never saw the Set-Cookie, sends it.
  (with-share (share :share '(:cookie))
    (with-easy (setter)
      (attach-share setter share)
      (setf (callback-function setter :write) (lambda (o) (declare (ignore o)) t))
      ;; An empty cookie file turns libcurl's cookie engine on.
      (setopts setter :url (test-url "/cookie/set") :cookiefile "")
      (perform setter))
    (with-easy (reader)
      (attach-share reader share)
      (let ((body (collect-body reader)))
        (setopts reader :url (test-url "/cookie/echo") :cookiefile "")
        (perform reader)
        (is (search "session=abc123" (body-string (funcall body)))
            "the second handle did not send the shared cookie")))))

(test an-unshared-cookie-jar-is-not-visible-to-other-handles
  ;; The control for the previous test: without a share, nothing crosses.
  (with-easy (setter)
    (setf (callback-function setter :write) (lambda (o) (declare (ignore o)) t))
    (setopts setter :url (test-url "/cookie/set") :cookiefile "")
    (perform setter))
  (with-easy (reader)
    (let ((body (collect-body reader)))
      (setopts reader :url (test-url "/cookie/echo") :cookiefile "")
      (perform reader)
      (is (search "no cookie" (body-string (funcall body)))))))

(test a-share-works-from-several-threads-at-once
  ;; What the lock callbacks are for.  Without them libcurl's shared caches
  ;; race; with them this should simply pass.
  (with-share (share :share '(:dns :connect))
    (let* ((count 8)
           (results (make-array count :initial-element nil))
           (threads
             (loop for i below count
                   collect (let ((index i))
                             (bt:make-thread
                              (lambda ()
                                (handler-case
                                    (with-easy (handle)
                                      (attach-share handle share)
                                      (let ((body (collect-body handle)))
                                        (setopt handle :url (test-url "/ok"))
                                        (perform handle)
                                        (setf (aref results index)
                                              (body-string (funcall body)))))
                                  (error (c)
                                    (setf (aref results index) c))))
                              :name (format nil "share test ~D" index))))))
      (mapc (lambda (thread) (bt:join-thread thread)) threads)
      (dotimes (i count)
        (is (equal "ok" (aref results i))
            "thread ~D returned ~S" i (aref results i))))))
