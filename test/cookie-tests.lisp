;;;; test/cookie-tests.lisp — the cookie engine, and the jar as a file.
;;;;
;;;; Cookies within one transfer were already covered.  What was not is the
;;;; part that touches the filesystem: CURLOPT_COOKIEJAR names a file libcurl
;;;; writes by itself, and CURLOPT_COOKIEFILE names one it reads.  A jar that is
;;;; never written and a jar that is never read both look exactly like "no
;;;; cookies were set", which is also what a working engine looks like when the
;;;; server sent none -- so nothing short of reading the file back distinguishes
;;;; them.
;;;;
;;;; The detail that makes this worth a test rather than a docstring: the jar is
;;;; written when the handle is *cleaned up*, not when the transfer ends.  That
;;;; puts it squarely inside this binding's release order, where
;;;; curl_easy_cleanup has to run before the owned foreign strings it is still
;;;; reading are freed -- the CURLOPT_COOKIEJAR path being one of them.  A
;;;; binding that freed the string first would write the jar to a freed pointer,
;;;; and the failure would be a corrupt filename rather than a crash.

(in-package #:libcurl/test)

(in-suite cookies)

(defun file-contents (path)
  (with-open-file (in path :direction :input :external-format :utf-8)
    (let ((text (make-string (file-length in))))
      (subseq text 0 (read-sequence text in)))))

(test the-jar-is-written-when-the-handle-is-closed
  ;; Not when the transfer finishes: the file is still absent or empty at that
  ;; point, and only CLOSE-HANDLE makes libcurl flush it.  Asserting the
  ;; before-and-after is what pins the ordering rather than merely observing
  ;; that a file eventually appeared.
  (uiop:with-temporary-file (:pathname jar)
    (delete-file jar)
    (let ((handle (make-easy-handle)))
      (unwind-protect
           (progn
             (setopts handle :url (test-url "/cookie/set")
                             ;; An empty COOKIEFILE turns the engine on without
                             ;; reading anything; without it nothing is
                             ;; recorded and the jar comes out empty.
                             :cookiefile ""
                             :cookiejar (uiop:native-namestring jar)
                             :timeout 10)
             (perform handle)
             (is (= 200 (getinfo handle :response-code)))
             (is (null (probe-file jar))
                 "the jar was written before the handle was closed"))
        (close-handle handle))
      (is (probe-file jar) "closing the handle did not write the jar")
      (let ((text (file-contents jar)))
        ;; Netscape format: a comment header, then tab-separated fields.
        (is (search "Netscape HTTP Cookie File" text))
        (is (search "session" text))
        (is (search "abc123" text))))))

(test a-jar-written-by-one-handle-is-read-by-another
  ;; The round trip, and the reason the jar exists at all.  Two handles that
  ;; share nothing but the file: the second sends a cookie it was never told
  ;; about, because it read it off disk.
  (uiop:with-temporary-file (:pathname jar)
    (delete-file jar)
    (let ((writer (make-easy-handle)))
      (unwind-protect
           (progn
             (setopts writer :url (test-url "/cookie/set")
                             :cookiefile ""
                             :cookiejar (uiop:native-namestring jar)
                             :timeout 10)
             (perform writer))
        (close-handle writer)))
    (with-easy (reader)
      (let ((body (collect-body reader)))
        (setopts reader :url (test-url "/cookie/echo")
                        :cookiefile (uiop:native-namestring jar)
                        :timeout 10)
        (perform reader)
        (let ((echoed (body-string (funcall body))))
          (is (search "session=abc123" echoed)
              "the second handle sent ~S rather than the jarred cookie" echoed))))))

;;; The control for the round trip -- that a handle with the engine on but no
;;; jar sends nothing -- is AN-UNSHARED-COOKIE-JAR-IS-NOT-VISIBLE-TO-OTHER-HANDLES
;;; in the share suite, which asserts exactly that and needs it for its own
;;; reasons.  Repeating it here would be a second copy of one assertion, not a
;;; second test.

(test the-cookie-engine-can-be-read-back-without-a-file
  ;; CURLINFO_COOKIELIST returns the jar's contents as an slist, which is worth
  ;; asserting on for its own sake: CURLINFO_SLIST and CURLINFO_PTR are the same
  ;; numeric value, so an info decoded by masking its id rather than by its
  ;; recorded type would hand back a raw pointer here and look like a cookie
  ;; count of one.
  (with-easy (handle)
    (setopts handle :url (test-url "/cookie/set") :cookiefile "" :timeout 10)
    (perform handle)
    (let ((cookies (getinfo handle :cookielist)))
      (is (listp cookies))
      (is (= 1 (length cookies)))
      (is (every #'stringp cookies))
      (let ((line (first cookies)))
        (is (search "session" line))
        (is (search "abc123" line))
        ;; Netscape format is tab-separated, which is what makes these lines
        ;; feedable back through CURLOPT_COOKIELIST.
        (is (find #\Tab line))))))

(test cookies-can-be-loaded-from-a-line-rather-than-a-file
  ;; The other direction of the same format: a line written by one handle is
  ;; accepted by another through CURLOPT_COOKIELIST, with no file involved.
  (let ((line nil))
    (with-easy (source)
      (setopts source :url (test-url "/cookie/set") :cookiefile "" :timeout 10)
      (perform source)
      (setf line (first (getinfo source :cookielist))))
    (is (not (null line)))
    (with-easy (target)
      (let ((body (collect-body target)))
        (setopts target :url (test-url "/cookie/echo")
                        :cookiefile ""
                        :cookielist line
                        :timeout 10)
        (perform target)
        (is (search "session=abc123" (body-string (funcall body))))))))

(test flushing-writes-the-jar-without-closing-the-handle
  ;; CURLOPT_COOKIELIST takes commands as well as cookies, and "FLUSH" is the
  ;; way to get the jar on disk while the handle is still in use -- which is the
  ;; only option for a long-lived pooled handle that may not be closed for
  ;; hours.
  (uiop:with-temporary-file (:pathname jar)
    (delete-file jar)
    (with-easy (handle)
      (setopts handle :url (test-url "/cookie/set")
                      :cookiefile ""
                      :cookiejar (uiop:native-namestring jar)
                      :timeout 10)
      (perform handle)
      (is (null (probe-file jar)))
      (setopt handle :cookielist "FLUSH")
      (is (probe-file jar) "FLUSH did not write the jar")
      (is (search "abc123" (file-contents jar))))))

(test clearing-the-engine-drops-what-it-held
  ;; "ALL" empties the jar in memory.  Worth checking because it is the same
  ;; option taking a command rather than data, and because a cleared engine
  ;; that still sent cookies would be a leak of one user's session into the
  ;; next request on a pooled handle.
  (with-easy (handle)
    (setopts handle :url (test-url "/cookie/set") :cookiefile "" :timeout 10)
    (perform handle)
    (is (= 1 (length (getinfo handle :cookielist))))
    (setopt handle :cookielist "ALL")
    (is (null (getinfo handle :cookielist)))))

;;; Through the client --------------------------------------------------------

(test the-client-persists-cookies-through-its-jar
  ;; The same round trip at the level most callers use, where the jar is both
  ;; read and written by the same :COOKIE-JAR argument, and the handle is closed
  ;; inside the request rather than by the test.
  (uiop:with-temporary-file (:pathname jar)
    (delete-file jar)
    (http-get (test-url "/cookie/set") :cookie-jar jar)
    (is (probe-file jar))
    (let ((response (http-get (test-url "/cookie/echo") :cookie-jar jar)))
      (is (search "session=abc123" (response-body response))
          "the client sent ~S" (response-body response)))))

(test a-client-request-without-the-jar-does-not-see-those-cookies
  ;; Control for the above: the cookies came from the file, not from anything
  ;; the client kept between calls.
  (uiop:with-temporary-file (:pathname jar)
    (delete-file jar)
    (http-get (test-url "/cookie/set") :cookie-jar jar)
    (let ((response (http-get (test-url "/cookie/echo"))))
      (is (string= "no cookie" (response-body response))))))
