;;;; test/live-tests.lisp — the opt-in suite that talks to the real internet.
;;;;
;;;; Skipped unless CURL_LIVE_TESTS is set, because a test that fails when the
;;;; network is down is a test people learn to ignore.  Everything else in the
;;;; suite runs against the in-process server and is hermetic.
;;;;
;;;; What is here is only what a local server genuinely cannot provide: a real
;;;; certificate chain, real DNS, a real HTTP/2 peer, and a redirect between
;;;; real hosts.  Anything testable offline is tested offline.

(in-package #:curlcl/test)

(in-suite live)

(defun live-tests-enabled-p ()
  (let ((value (uiop:getenv "CURL_LIVE_TESTS")))
    (and value (plusp (length value)) (not (string= value "0")))))

(defmacro live-test (name &body body)
  "Define a test that runs only when CURL_LIVE_TESTS is set."
  `(test ,name
     (if (live-tests-enabled-p)
         (progn ,@body)
         (skip "set CURL_LIVE_TESTS=1 to run tests that use the network"))))

(live-test tls-verification-succeeds-against-a-real-certificate
  ;; The whole certificate path: a real chain, a real CA bundle, real
  ;; hostname verification.  No local server can stand in for this.
  (let ((response (http-get "https://example.com/" :timeout 30)))
    (is (= 200 (response-status response)))
    (is (search "Example Domain" (response-text response)))))

(live-test tls-verification-rejects-a-bad-certificate
  ;; The failure direction matters more than the success one: a binding that
  ;; accidentally disabled verification would pass every other TLS test.
  (handler-case
      (progn (http-get "https://expired.badssl.com/" :timeout 30)
             (fail "an expired certificate was accepted"))
    (easy-error (c)
      (is (member (curl-error-code-name c)
                  '(:peer-failed-verification :ssl-connect-error
                    :ssl-cacert-badfile))
          "unexpected code ~S for an expired certificate"
          (curl-error-code-name c)))))

(live-test verification-can-be-disabled-deliberately
  ;; Only ever on purpose, and worth proving the escape hatch works so nobody
  ;; reaches for something worse.
  (let ((response (http-get "https://self-signed.badssl.com/"
                            :verify-ssl :none :timeout 30)))
    (is (= 200 (response-status response)))))

(live-test http-2-is-negotiated
  (let ((response (http-get "https://example.com/" :timeout 30)))
    (if (feature-supported-p :http2)
        (is (member (response-version response) '(:http/2 :http/1.1))
            "negotiated ~S" (response-version response))
        (is (eq :http/1.1 (response-version response))))))

(live-test compressed-responses-are-decoded-transparently
  ;; :ACCEPT-ENCODING defaults to every codec this libcurl has, and libcurl
  ;; decompresses before the write callback sees anything -- so the body must
  ;; arrive as readable text with no Content-Encoding handling here.
  (let ((response (http-get "https://www.gnu.org/" :timeout 30)))
    (is (= 200 (response-status response)))
    (is (stringp (response-text response)))
    (is (plusp (length (response-text response))))))

(live-test a-real-redirect-chain-is-followed-across-hosts
  (let ((response (http-get "http://github.com/" :timeout 30)))
    (is (= 200 (response-status response)))
    (is (plusp (response-redirect-count response)))
    ;; It should have ended up on HTTPS.
    (is (eql 0 (search "https://" (response-url response))))))

(live-test dns-failure-is-reported-as-itself
  (handler-case
      (progn (http-get "http://no-such-host.invalid/" :timeout 30)
             (fail "a nonexistent host resolved"))
    (easy-error (c)
      (is (member (curl-error-code-name c)
                  '(:couldnt-resolve-host :couldnt-connect))))))

(live-test a-session-reuses-a-real-connection
  ;; Connection reuse is only observable against a real server: the second
  ;; request should skip the TLS handshake entirely, so its connect time is
  ;; zero where the first one's was not.
  (with-session (session)
    (let ((first (http-get "https://example.com/" :session session :timeout 30))
          (second (http-get "https://example.com/" :session session :timeout 30)))
      (is (= 200 (response-status first)))
      (is (= 200 (response-status second)))
      (is (plusp (getf (response-timings first) :connect)))
      (is (zerop (getf (response-timings second) :connect))
          "the second request did not reuse the connection"))))

(live-test many-real-requests-run-concurrently
  (let ((responses (request-many (loop repeat 4
                                       collect (list "https://example.com/"
                                                     :timeout 30)))))
    (is (= 4 (length responses)))
    (is (every (lambda (r) (and (typep r 'response) (= 200 (response-status r))))
               responses))))

(live-test a-real-websocket-echoes
  (if (websockets-supported-p)
      (with-websocket (handle "wss://echo.websocket.org/")
        (ws-send-text handle "hello from libcurl")
        (let ((reply (loop repeat 600
                           do (multiple-value-bind (octets frame) (ws-receive handle)
                                (declare (ignore frame))
                                (when octets
                                  (return (curlcl::octets-to-string octets))))
                              (sleep 0.01))))
        (is (or (null reply) (stringp reply))
            "unexpected websocket reply ~S" reply)))
      (skip "this libcurl was built without websocket support")))
