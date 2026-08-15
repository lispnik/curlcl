;;;; test/headers-tests.lisp — the parsed header API.

(in-package #:libcurl/test)

(in-suite headers)

(defun perform-for-headers (path)
  "Perform a transfer and leave the handle open so its headers can be read."
  (let ((handle (make-easy-handle)))
    (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t))
    (setopt handle :url (test-url path))
    (perform handle)
    handle))

(test a-header-can-be-looked-up-by-name
  (let ((handle (perform-for-headers "/ok")))
    (unwind-protect
         (let ((header (response-header handle "content-type")))
           (is (not (null header)))
           (is (string-equal "content-type" (header-name header)))
           (is (string= "text/plain" (header-value header)))
           (is (member :header (header-origin header))))
      (close-handle handle))))

(test header-lookup-is-case-insensitive
  ;; Required by HTTP, and the case libcurl reports is not necessarily the case
  ;; the server sent.
  (let ((handle (perform-for-headers "/ok")))
    (unwind-protect
         (progn
           (is (string= "text/plain" (response-header-value handle "Content-Type")))
           (is (string= "text/plain" (response-header-value handle "CONTENT-TYPE")))
           (is (string= "text/plain" (response-header-value handle "content-type"))))
      (close-handle handle))))

(test a-missing-header-is-nil-rather-than-an-error
  (let ((handle (perform-for-headers "/ok")))
    (unwind-protect
         (progn
           (is (null (response-header handle "x-not-present")))
           (is (null (response-header-value handle "x-not-present"))))
      (close-handle handle))))

(test every-header-can-be-enumerated
  (let ((handle (perform-for-headers "/ok")))
    (unwind-protect
         (let ((headers (response-headers handle)))
           (is (plusp (length headers)))
           (is (every (lambda (h) (typep h 'http-header)) headers))
           (is (find "content-type" headers :key #'header-name :test #'string-equal))
           (is (find "content-length" headers :key #'header-name :test #'string-equal)))
      (close-handle handle))))

(test repeated-headers-are-reported-individually
  ;; The case that a naive alist gets wrong.  Set-Cookie is sent twice and both
  ;; must survive, with AMOUNT saying how many share the name and INDEX saying
  ;; which this is.
  (let ((handle (perform-for-headers "/headers/multi")))
    (unwind-protect
         (let ((cookies (remove-if-not
                         (lambda (h) (string-equal "set-cookie" (header-name h)))
                         (response-headers handle))))
           (is (= 2 (length cookies)))
           (is (every (lambda (h) (= 2 (header-amount h))) cookies))
           (is (equal '(0 1) (sort (mapcar #'header-index cookies) #'<)))
           (is (equal '("a=1" "b=2")
                      (sort (mapcar #'header-value cookies) #'string<))))
      (close-handle handle))))

(test a-repeated-header-can-be-indexed
  (let ((handle (perform-for-headers "/headers/multi")))
    (unwind-protect
         (let ((first (response-header handle "x-repeated" :index 0))
               (second (response-header handle "x-repeated" :index 1))
               (third (response-header handle "x-repeated" :index 2)))
           (is (string= "first" (header-value first)))
           (is (string= "second" (header-value second)))
           ;; Past the end is "no such header", not an error.
           (is (null third)))
      (close-handle handle))))

(test headers-from-earlier-requests-in-a-redirect-chain-are-reachable
  ;; REQUEST -1 is the final response; 0 is the first.  Being able to ask about
  ;; the redirect itself is most of the point of this API over scraping the
  ;; header callback.
  (let ((handle (make-easy-handle)))
    (unwind-protect
         (progn
           (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t))
           (setopts handle :url (test-url "/redirect/1") :followlocation t)
           (perform handle)
           ;; The last response is the 200 from /ok, which has no Location.
           (is (null (response-header handle "location")))
           ;; The first was a 302 that did.
           (is (not (null (response-header handle "location" :request 0)))))
      (close-handle handle))))

(test asking-for-headers-before-a-transfer-is-not-an-error
  (with-easy (handle)
    (is (null (response-header handle "content-type")))
    (is (null (response-headers handle)))))
