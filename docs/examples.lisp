;;;; docs/examples.lisp — every Lisp example shown on the home page.
;;;;
;;;; docs/index.html quotes from this file, and `make docs-check' runs it.  The
;;;; page claims its Lisp was executed, so this is what makes that true and
;;;; what stops it quietly ceasing to be true: an example that stops working is
;;;; a failing command rather than something a reader discovers.
;;;;
;;;; Everything here talks to example.com, so it needs a network.  Each example
;;;; is a function named for its section on the page.

(defpackage #:curlcl/docs
  (:use #:cl)
  (:export #:run-all))

(in-package #:curlcl/docs)

(defmacro example (name &body body)
  "Define an example and register it, so RUN-ALL needs no separate list."
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *examples*)))

(defvar *examples* '())

;;; 1. Hello, transfer ---------------------------------------------------------

(example hello-transfer
  (curl:with-easy (handle)
    (curl:setopts handle :url "https://example.com/" :followlocation t)
    (curl:perform handle)
    (curl:getinfo handle :response-code)))

;;; 2. Options and info --------------------------------------------------------

(example options-and-info
  (curl:with-easy (handle)
    (curl:setopts handle :url "https://example.com/"
                         :useragent "curlcl/docs"
                         :timeout 30
                         :ssl-verifypeer t)
    (curl:perform handle)
    (list :code (curl:getinfo handle :response-code)
          :url (curl:getinfo handle :effective-url)
          :type (curl:getinfo handle :content-type)
          :bytes (curl:getinfo handle :size-download))))

;;; 3. Taking the body ---------------------------------------------------------

(example write-callback
  (let ((total 0))
    (curl:with-easy (handle)
      (setf (curl:callback-function handle :write)
            (lambda (octets) (incf total (length octets)) t))
      (curl:setopts handle :url "https://example.com/")
      (curl:perform handle))
    total))

;;; 4. Many at once ------------------------------------------------------------

(example multi-transfers
  (let ((handles (loop repeat 3
                       collect (let ((h (curl:make-easy-handle)))
                                 (curl:setopts h :url "https://example.com/")
                                 (setf (curl:callback-function h :write)
                                       (lambda (octets) (declare (ignore octets)) t))
                                 h))))
    (unwind-protect
         (curl:with-multi (multi)
           (dolist (h handles) (curl:add-transfer multi h))
           (mapcar #'curl:result-code-name (curl:run-transfers multi)))
      (mapc #'curl:close-handle handles))))

;;; 5. URLs --------------------------------------------------------------------

(example url-parsing
  (list :parts (curl:parse-url "https://example.com:8443/a/b?q=1#frag")
        :rebuilt (curl:with-url (u "https://example.com/a")
                   (setf (curl:url-part u :path) "/b/c")
                   (curl:url-string u))))

;;; 6. Multipart ---------------------------------------------------------------

(example multipart-body
  (curl:with-easy (handle)
    (let ((mime (curl:make-mime handle)))
      (curl:add-mime-part mime :name "field" :data "value")
      (curl:add-mime-part mime :name "note" :data "hello"
                               :content-type "text/plain")
      (curl:attach-mime handle mime))
    (setf (curl:callback-function handle :write)
          (lambda (octets) (declare (ignore octets)) t))
    (curl:setopts handle :url "https://example.com/")
    (curl:perform handle)
    (curl:getinfo handle :response-code)))

;;; 7. Sharing -----------------------------------------------------------------

(example sharing
  (curl:with-share (share)
    (curl:share-data share :dns)
    (curl:share-data share :ssl-session)
    (loop repeat 2
          collect (curl:with-easy (handle)
                    (curl:attach-share handle share)
                    (setf (curl:callback-function handle :write)
                          (lambda (octets) (declare (ignore octets)) t))
                    (curl:setopts handle :url "https://example.com/")
                    (curl:perform handle)
                    (curl:getinfo handle :response-code)))))

;;; 8. Response headers --------------------------------------------------------

(example response-headers
  (let ((response (curl:http-get "https://example.com/")))
    (list :content-type (curl:response-header-values response "content-type")
          :how-many (length (curl:response-headers response)))))

;;; 9. Websockets --------------------------------------------------------------
;;;
;;; The predicate rather than a connection: whether ws:// works at all is a
;;; property of the libcurl that got loaded, and macOS ships one built without
;;; it, so an example that connected would fail there for a reason that is not
;;; a fault.  The page says as much.

(example websockets
  ;; The page quotes this value directly, so return it rather than a keyword
  ;; standing in for it.
  (curl:websockets-supported-p))

;;; 10. Errors -----------------------------------------------------------------

(example errors-as-conditions
  (handler-case
      (curl:with-easy (handle)
        (curl:setopts handle :url "https://no-such-host.invalid/" :timeout 10)
        (curl:perform handle))
    (curl:easy-error (condition)
      (list :code (curl:curl-error-code condition)
            :name (curl:curl-error-code-name condition)))))

;;; 11. The client layer -------------------------------------------------------

(example the-client-layer
  (let ((response (curl:http-get "https://example.com/")))
    (list :status (curl:response-status response)
          :ok (curl:successful-response-p response)
          :text-p (stringp (curl:response-text response)))))

(example several-at-once
  (mapcar (lambda (outcome)
            (if (typep outcome 'curl:response)
                (curl:response-status outcome)
                (type-of outcome)))
          (curl:request-many (list "https://example.com/"
                                   "https://example.com/"))))

(example a-session
  (curl:with-session (session)
    (list (curl:response-status (curl:http-get "https://example.com/"
                                               :session session))
          (curl:response-status (curl:http-get "https://example.com/"
                                               :session session)))))

;;; Running them ---------------------------------------------------------------

(defun run-all ()
  "Run every example, printing what it returned.  True when all of them ran."
  (format t "~&libcurl ~A at ~A~%~%"
          (curl:libcurl-version) (curl:libcurl-pathname))
  (let ((failures 0))
    (dolist (name (reverse *examples*))
      (handler-case
          (format t "~&~(~A~)~%  => ~S~%" name (funcall name))
        (error (condition)
          (incf failures)
          (format t "~&~(~A~)~%  !! ~A~%" name condition))))
    (format t "~&~%~[all examples ran~:;~:*~D failed~]~%" failures)
    (zerop failures)))
