;;;; test/url-tests.lisp — libcurl's URL parser.

(in-package #:libcurl/test)

(in-suite url)

(test a-url-splits-into-its-components
  (with-url (u "https://user:secret@example.com:8443/a/b?x=1&y=2#frag")
    (is (string= "https" (url-part u :scheme)))
    (is (string= "user" (url-part u :user)))
    (is (string= "secret" (url-part u :password)))
    (is (string= "example.com" (url-part u :host)))
    (is (string= "8443" (url-part u :port)))
    (is (string= "/a/b" (url-part u :path)))
    (is (string= "x=1&y=2" (url-part u :query)))
    (is (string= "frag" (url-part u :fragment)))))

(test a-missing-component-is-nil-rather-than-an-error
  ;; CURLUE_NO_PORT and its siblings are answers about the URL, not failures to
  ;; parse it; turning them into conditions would make ordinary URLs throw.
  (with-url (u "http://example.com/")
    (is (null (url-part u :query)))
    (is (null (url-part u :fragment)))
    (is (null (url-part u :user)))
    ;; The path is always present, defaulting to "/".
    (is (string= "/" (url-part u :path)))))

(test a-malformed-url-still-signals
  (signals url-error (make-url "http://"))
  (signals url-error (make-url "not a url at all"))
  (handler-case (make-url "!!!")
    (url-error (c)
      (is (not (null (curl-error-code-name c))))
      (is (plusp (length (princ-to-string c)))))))

(test components-can-be-replaced
  (with-url (u "http://example.com/one")
    (setf (url-part u :scheme) "https"
          (url-part u :host) "elsewhere.test"
          (url-part u :path) "/two")
    (is (string= "https://elsewhere.test/two" (url-string u)))))

(test the-default-port-is-reported-only-when-asked
  (with-url (u "https://example.com/")
    (is (null (url-part u :port)))
    (is (string= "443" (url-part u :port :default-port)))))

(test query-values-can-be-appended
  (with-url (u "http://example.com/?a=1")
    (setf (url-part u :query :append-query) "b=2")
    (is (string= "a=1&b=2" (url-part u :query)))))

(test percent-encoding-is-decoded-on-request
  (with-url (u "http://example.com/a%20b")
    (is (string= "/a%20b" (url-part u :path)))
    (is (string= "/a b" (url-part u :path :urldecode)))))

(test parse-url-returns-a-plist
  (let ((parts (parse-url "http://example.com:81/p?q=1")))
    (is (string= "http" (getf parts :scheme)))
    (is (string= "example.com" (getf parts :host)))
    (is (string= "81" (getf parts :port)))
    (is (string= "/p" (getf parts :path)))
    (is (string= "q=1" (getf parts :query)))
    ;; Absent components are absent from the plist rather than present as NIL.
    (is (not (member :fragment parts)))))

(test relative-urls-resolve-the-way-a-redirect-would
  ;; This is libcurl's own resolution, so it matches what following a Location
  ;; header will actually do.
  (is (string= "http://example.com/b"
               (url-join "http://example.com/a" "b")))
  (is (string= "http://example.com/x/y"
               (url-join "http://example.com/x/z" "y")))
  (is (string= "https://other.test/p"
               (url-join "http://example.com/a" "https://other.test/p")))
  (is (string= "http://example.com/top"
               (url-join "http://example.com/deep/path" "/top"))))

(test a-duplicated-url-is-independent
  (with-url (original "http://example.com/one")
    (let ((copy (duplicate-url original)))
      (unwind-protect
           (progn
             (setf (url-part copy :path) "/two")
             (is (string= "/one" (url-part original :path)))
             (is (string= "/two" (url-part copy :path))))
        (close-url copy)))))

(test closing-a-url-twice-is-harmless
  (let ((u (make-url "http://example.com/")))
    (close-url u)
    (finishes (close-url u))
    (is (url-closed-p u))))

(test an-unknown-flag-is-rejected-rather-than-ignored
  ;; Silently dropping a misspelled flag would mean the caller gets different
  ;; behaviour than they asked for, with nothing to see.
  (with-url (u "http://example.com/")
    (signals curl-error (url-part u :host :no-such-flag))))
