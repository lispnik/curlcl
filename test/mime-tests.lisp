;;;; test/mime-tests.lisp — multipart bodies.

(in-package #:libcurl/test)

(in-suite mime)

(defun post-mime (parts)
  "POST a multipart body built from PARTS and return what the server saw."
  (with-easy (handle)
    (let ((body (collect-body handle)))
      (setopt handle :url (test-url "/echo"))
      (set-mime-body handle parts)
      (perform handle)
      (body-string (funcall body)))))

(test a-mime-body-is-sent-as-multipart-form-data
  (let ((echoed (post-mime '((:name "field" :data "value")))))
    (is (search "method=POST" echoed))
    (is (search "multipart/form-data" echoed)
        "Content-Type was not multipart/form-data")
    (is (search "name=\"field\"" echoed))
    (is (search "value" echoed))))

(test several-parts-are-all-sent
  (let ((echoed (post-mime '((:name "one" :data "first")
                             (:name "two" :data "second")
                             (:name "three" :data "third")))))
    (dolist (name '("one" "two" "three"))
      (is (search (format nil "name=\"~A\"" name) echoed)
          "part ~S missing from the request" name))
    (dolist (value '("first" "second" "third"))
      (is (search value echoed)))))

(test a-part-can-declare-a-filename-and-type
  (let ((echoed (post-mime '((:name "upload" :data "PNGDATA"
                              :filename "picture.png"
                              :content-type "image/png")))))
    (is (search "filename=\"picture.png\"" echoed))
    (is (search "image/png" echoed))))

(test a-part-can-be-streamed-from-a-file
  ;; curl_mime_filedata reads at transfer time rather than now, so the file has
  ;; to still be there when PERFORM runs.
  (uiop:with-temporary-file (:pathname path :stream stream :direction :output)
    (write-string "contents-from-disk" stream)
    (finish-output stream)
    :close-stream
    (let ((echoed (post-mime (list (list :name "f" :file path)))))
      (is (search "contents-from-disk" echoed))
      ;; libcurl derives the reported filename from the path.
      (is (search "filename=" echoed)))))

(test binary-data-survives-a-mime-part
  ;; Octets rather than a string, and containing NUL bytes -- anything that
  ;; measured the payload with strlen would stop at the second byte.  The
  ;; values stay below 128 only so the echo server can hand them back
  ;; unchanged; the NULs are the part under test.
  (with-easy (handle)
    (let ((body (collect-body handle))
          (octets (make-array 6 :element-type '(unsigned-byte 8)
                                :initial-contents '(1 0 2 0 3 4))))
      (setopt handle :url (test-url "/echo"))
      (set-mime-body handle (list (list :name "bin" :data octets)))
      (perform handle)
      (let ((echoed (funcall body)))
        ;; The server echoes the raw body, so every byte must appear.
        (is (search (coerce (map 'list #'code-char octets) 'string)
                    (body-string echoed)))))))

(test a-part-can-carry-its-own-headers
  (let ((echoed (post-mime '((:name "x" :data "y"
                              :headers ("X-Part-Header: present"))))))
    (is (search "X-Part-Header: present" echoed))))

(test a-mime-is-released-with-its-handle
  ;; The mime is registered against the handle and freed after
  ;; curl_easy_cleanup; freeing it before would pull the body out from under
  ;; libcurl.  This checks the bookkeeping rather than the free itself.
  (let ((handle (make-easy-handle)))
    (unwind-protect
         (progn
           (set-mime-body handle '((:name "a" :data "b")))
           (is (find :mime (libcurl::resources-items (libcurl::handle-resources handle))
                     :key #'car)))
      (close-handle handle))
    (is (null (libcurl::resources-items (libcurl::handle-resources handle))))))

(test nested-subparts-are-adopted-by-their-parent
  ;; libcurl takes ownership of a mime attached as subparts, so the binding
  ;; must stop tracking it or it would be freed twice.
  (with-easy (handle)
    (let* ((inner (make-mime handle))
           (outer (make-mime handle)))
      (add-mime-part inner :name "inner-field" :data "inner-value")
      (add-mime-part outer :name "nested" :subparts inner)
      (is (libcurl::mime-freed-p inner)
          "the adopted mime is still tracked for release")
      ;; Only the outer mime remains in the handle's resource list.
      (is (= 1 (count :mime (libcurl::resources-items (libcurl::handle-resources handle))
                      :key #'car))))))
