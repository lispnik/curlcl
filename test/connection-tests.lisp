;;;; test/connection-tests.lisp — the options that decide where a transfer goes.
;;;;
;;;; CURLOPT_PROXY, CURLOPT_NOPROXY, CURLOPT_INTERFACE, CURLOPT_RESOLVE and
;;;; CURLOPT_DNS_SERVERS were bound and unexercised.  They are worth their own
;;;; file because they share a failure mode that ordinary option tests miss: all
;;;; of them are strings that libcurl accepts without complaint, so a binding
;;;; that dropped the value entirely would pass every "did setopt return
;;;; CURLE_OK" check ever written.  What has to be observed is the effect --
;;;; which socket the bytes left from, which host was asked for, which server
;;;; answered.
;;;;
;;;; The hostname used throughout is under .invalid, which RFC 2606 reserves
;;;; precisely so that it cannot resolve.  That is what makes the proxy tests
;;;; sharp: if the request reaches the origin at all, the option worked, because
;;;; nothing else could have found it.

(in-package #:curlcl/test)

(in-suite connection)

;;; Proxying ------------------------------------------------------------------

(test a-proxy-receives-the-request-instead-of-the-origin
  ;; The name does not resolve, so a direct request cannot even be attempted.
  ;; Reaching a body at all proves the transfer went to the proxy, and the
  ;; absolute-form request line proves it went there *as a proxy request*
  ;; rather than as an ordinary request to a machine that happens to be there.
  (with-test-proxy (proxy)
    (with-easy (handle)
      (let ((body (collect-body handle)))
        (setopts handle :url "http://origin.invalid/ok"
                        :proxy (proxy-url proxy)
                        :timeout 10)
        (perform handle)
        (is (= 200 (getinfo handle :response-code)))
        (is (string= "ok" (body-string (funcall body))))
        (is (proxy-saw-target-p proxy "http://origin.invalid/ok"))))))

(test without-a-proxy-that-same-request-cannot-resolve
  ;; The control for the test above.  Without it, a proxy option that was
  ;; silently ignored would still have to explain how the body arrived -- but
  ;; only this makes the alternative explicitly impossible.
  (with-easy (handle)
    (setopts handle :url "http://origin.invalid/ok" :timeout 10)
    (handler-case (progn (perform handle) (fail "an unresolvable host resolved"))
      (easy-error (c)
        ;; Some resolvers answer for a nonexistent TLD by handing back a
        ;; wildcard address, which turns this into a connection failure
        ;; instead; either way the name was never usable.
        (is (member (curl-error-code-name c)
                    '(:couldnt-resolve-host :couldnt-connect :operation-timedout))
            "expected a resolution failure, got ~S" (curl-error-code-name c))))))

(test noproxy-sends-the-request-direct
  ;; The proxy is set and still must not be used.  Both halves are asserted:
  ;; the origin saw it, and the proxy did not -- a test that only checked the
  ;; body would pass with the request going through the proxy to the origin.
  (with-test-proxy (proxy)
    (let ((server (ensure-server)))
      (clear-server-log server)
      (with-easy (handle)
        (setopts handle :url (test-url "/ok")
                        :proxy (proxy-url proxy)
                        :noproxy "127.0.0.1"
                        :timeout 10)
        (perform handle)
        (is (= 200 (getinfo handle :response-code)))
        (is (plusp (length (server-requests server))))
        (is (null (proxy-requests proxy)))))))

(test a-proxy-that-demands-authentication-is-answered
  ;; Two transfers against the same proxy, differing only in CURLOPT_PROXYUSERPWD.
  ;; The 407 is a response rather than an error, so the unauthenticated case
  ;; asserts on the status; the authenticated one asserts that libcurl actually
  ;; sent Proxy-Authorization -- not merely that it got a 200.
  (with-test-proxy (proxy :require-auth t)
    (with-easy (handle)
      (setopts handle :url "http://origin.invalid/ok"
                      :proxy (proxy-url proxy)
                      :timeout 10)
      (perform handle)
      (is (= 407 (getinfo handle :response-code))))
    (with-easy (handle)
      (setopts handle :url "http://origin.invalid/ok"
                      :proxy (proxy-url proxy)
                      :proxyuserpwd "user:secret"
                      :timeout 10)
      (perform handle)
      (is (= 200 (getinfo handle :response-code)))
      (is (find-if (lambda (request)
                     (request-header (request-headers request) "proxy-authorization"))
                   (proxy-requests proxy))
          "libcurl never sent Proxy-Authorization"))))

(test an-https-url-through-a-proxy-opens-a-tunnel-first
  ;; No TLS fixture is needed to check this: what identifies a tunnelled
  ;; request is the CONNECT that precedes it, naming the origin and its port,
  ;; and that arrives before any handshake could.  The proxy refuses, so the
  ;; transfer fails -- the assertion is about what the proxy was asked.
  (with-test-proxy (proxy)
    (with-easy (handle)
      (setopts handle :url "https://origin.invalid/ok"
                      :proxy (proxy-url proxy)
                      :timeout 10)
      (signals easy-error (perform handle))
      (let ((connect (find "CONNECT" (proxy-requests proxy)
                           :key #'request-method :test #'string-equal)))
        (is (not (null connect)) "the proxy saw no CONNECT")
        (when connect
          (is (string= "origin.invalid:443" (request-path connect))))))))

(test a-proxy-that-is-not-listening-fails-as-the-proxy
  ;; The origin here is perfectly reachable, so a failure can only be the
  ;; proxy's -- which is the point: with the option ignored this would succeed.
  (with-easy (handle)
    (setopts handle :url (test-url "/ok")
                    :proxy "http://127.0.0.1:1"
                    :timeout 10)
    (handler-case (progn (perform handle) (fail "connected to a dead proxy"))
      (easy-error (c)
        ;; A refused connection is the expected shape; a timeout is the same
        ;; failure on a host that drops rather than refuses, and both are the
        ;; proxy failing, which is the claim.  What must not happen is success.
        (is (member (curl-error-code-name c)
                    '(:couldnt-connect :proxy :operation-timedout))
            "expected a proxy connection failure, got ~S"
            (curl-error-code-name c))))))

(test a-proxy-shuts-down-promptly-and-leaves-no-thread
  ;; The fixture's own shutdown path, asserted because it is platform-specific
  ;; in two ways that only showed up in CI.  Closing a listening socket does not
  ;; interrupt a thread already blocked in accept() on Linux -- it does on
  ;; macOS -- so a join here waited forever and the job ran until it was
  ;; cancelled.  And on Windows accept answers NIL rather than signalling when
  ;; the listener closes under it, which reached SOCKET-STREAM as a NIL and,
  ;; being unhandled in a thread, quit the whole process.
  ;;
  ;; Both are shutdown bugs, so both are invisible to any test that only starts
  ;; a fixture.  Several rounds make a hang certain rather than likely.
  (dotimes (i 3)
    (let* ((start (get-internal-real-time))
           (proxy (start-test-proxy))
           (thread (proxy-thread proxy)))
      ;; One real request, so the loop is somewhere interesting rather than
      ;; freshly blocked on its first accept.
      (with-easy (handle)
        (setopts handle :url "http://origin.invalid/ok"
                        :proxy (proxy-url proxy) :timeout 10)
        (perform handle))
      (stop-test-proxy proxy)
      (let ((seconds (/ (- (get-internal-real-time) start)
                        internal-time-units-per-second)))
        (is (< seconds 10)
            "round ~D: shutting the proxy down took ~,1Fs" i seconds))
      (is (not (bt:thread-alive-p thread))
          "round ~D: the accept thread outlived the proxy" i))))

;;; Binding the local end -----------------------------------------------------

(test a-bound-interface-still-transfers
  ;; A smoke test, and labelled as one: on a machine reached over loopback,
  ;; binding to 127.0.0.1 is indistinguishable from not binding at all, since
  ;; the local end would have been 127.0.0.1 either way.  It shows the option is
  ;; accepted and harmless; the two tests below are where CURLOPT_INTERFACE is
  ;; actually proved to have an effect, because there a dropped value would
  ;; succeed where it must fail.
  (with-easy (handle)
    (setopts handle :url (test-url "/ok") :interface "127.0.0.1" :timeout 10)
    (perform handle)
    (is (= 200 (getinfo handle :response-code)))
    (is (string= "127.0.0.1" (getinfo handle :local-ip)))
    (is (plusp (getinfo handle :local-port)))))

(test localport-binds-the-port-it-names
  ;; The positive half of "the local end was configured", and unlike the
  ;; interface case this one cannot pass by accident: an unset local port is
  ;; whatever the kernel picks, and the chance of it picking this one is
  ;; 1 in 65535.  CURLINFO_LOCAL_PORT reports what was actually bound.
  ;;
  ;; LOCALPORTRANGE is what makes it reliable rather than merely likely: the
  ;; port may be in TIME_WAIT from an earlier run, and without a range libcurl
  ;; would fail instead of trying the next one.
  (with-easy (handle)
    (let ((wanted 47913))
      (setopts handle :url (test-url "/ok")
                      :localport wanted
                      :localportrange 64
                      :timeout 10)
      (perform handle)
      (is (= 200 (getinfo handle :response-code)))
      (let ((bound (getinfo handle :local-port)))
        (is (<= wanted bound (+ wanted 63))
            "asked for a local port in [~D, ~D] and got ~D"
            wanted (+ wanted 63) bound)))))

(test an-interface-that-cannot-be-bound-fails-before-connecting
  ;; 192.0.2.0/24 is TEST-NET-1, reserved by RFC 5737 and never assigned to a
  ;; real interface, so binding to it fails everywhere.  CURLE_INTERFACE_FAILED
  ;; rather than a connection error is the assertion that matters: it says the
  ;; failure happened at bind time, which is the only place the option is used.
  (with-easy (handle)
    (setopts handle :url (test-url "/ok") :interface "192.0.2.1" :timeout 10)
    (handler-case (progn (perform handle) (fail "bound an address we do not have"))
      (easy-error (c)
        (is (eq :interface-failed (curl-error-code-name c))
            "expected CURLE_INTERFACE_FAILED, got ~S" (curl-error-code-name c))))))

(test the-interface-prefixes-are-passed-through-verbatim
  ;; libcurl reads "if!" as "this is an interface name, do not try to parse it
  ;; as an address" and "host!" as the opposite.  They are part of the option's
  ;; value rather than of its syntax, so a binding that mangled the string --
  ;; trimmed it, split on the bang -- would show up as the wrong failure here.
  (with-easy (handle)
    (setopts handle :url (test-url "/ok") :interface "if!nosuchinterface0"
                    :timeout 10)
    (handler-case (progn (perform handle) (fail "bound a nonexistent interface"))
      (easy-error (c)
        (is (eq :interface-failed (curl-error-code-name c)))))))

;;; Name resolution -----------------------------------------------------------

(test resolve-supplies-an-address-for-a-name-that-has-none
  ;; CURLOPT_RESOLVE pre-loads libcurl's DNS cache, and it is the one member of
  ;; this family that works on every build.  The name cannot resolve, so the
  ;; entry is the only way the transfer can find anything -- and the port has to
  ;; match too, since the cache is keyed on host *and* port.
  (let* ((server (ensure-server))
         (port (server-port server)))
    (with-easy (handle)
      (let ((body (collect-body handle)))
        (setopts handle
                 :url (format nil "http://origin.invalid:~D/ok" port)
                 :resolve (list (format nil "origin.invalid:~D:127.0.0.1" port))
                 :timeout 10)
        (perform handle)
        (is (= 200 (getinfo handle :response-code)))
        (is (string= "ok" (body-string (funcall body))))
        ;; The connection really went to the loopback address the entry named.
        (is (string= "127.0.0.1" (getinfo handle :primary-ip)))))))

(test a-resolve-entry-for-another-port-does-not-apply
  ;; The pair matters, not just the name: an entry for a different port must
  ;; not be consulted, or CURLOPT_RESOLVE would be quietly host-wide.
  (let* ((server (ensure-server))
         (port (server-port server)))
    (with-easy (handle)
      (setopts handle
               :url (format nil "http://origin.invalid:~D/ok" port)
               :resolve (list (format nil "origin.invalid:~D:127.0.0.1" (1+ port)))
               :timeout 10)
      (handler-case (progn (perform handle) (fail "the wrong entry was used"))
        (easy-error (c)
          (is (member (curl-error-code-name c)
                      '(:couldnt-resolve-host :couldnt-connect :operation-timedout))))))))

(test dns-servers-is-accepted-or-refused-according-to-the-build
  ;; CURLOPT_DNS_SERVERS needs a resolver libcurl can be told about, which in
  ;; practice means c-ares; without one the option does not exist at all.  Both
  ;; answers are correct, and which one this build gives is not something to
  ;; hard-code -- Homebrew's libcurl, Ubuntu's and the Windows build disagree.
  ;;
  ;; What is asserted is that the answer is one of the two, and that it is the
  ;; *same* answer for a well-formed value and a malformed one when the option
  ;; is missing.  A build that has it must tell the two apart; a build that does
  ;; not must reject both alike, since it never looked at the string.
  (with-easy (handle)
    (flet ((outcome (value)
             (handler-case (progn (setopt handle :dns-servers value) :ok)
               (curl-error (c) (curl-error-code-name c)))))
      (let ((good (outcome "127.0.0.1"))
            (bad (outcome "this is not an address list")))
        (case good
          (:ok
           ;; The build has c-ares, so the value is parsed and garbage is
           ;; rejected -- if it were not, the option would be accepting
           ;; anything and configuring nothing.
           (is (not (eq :ok bad))
               "a build that parses CURLOPT_DNS_SERVERS accepted nonsense"))
          (t
           (is (member good '(:not-built-in :unknown-option))
               "unexpected CURLOPT_DNS_SERVERS result ~S" good)
           (is (eq good bad)
               "an unsupported option answered differently for two values: ~
                ~S then ~S" good bad)))))))

(test dns-cache-timeout-accepts-its-documented-range
  ;; The one member of the DNS family available on every build, and a LONG
  ;; rather than a string.  Its range is what makes it worth asserting on:
  ;; -1 means "cache forever" and 0 means "never", so both are valid despite
  ;; one being negative, while anything below -1 is refused.  A value that
  ;; arrived truncated or unsigned would not sort itself into that pattern.
  (with-easy (handle)
    (finishes (setopt handle :dns-cache-timeout 0))
    (finishes (setopt handle :dns-cache-timeout -1))
    (finishes (setopt handle :dns-cache-timeout 60))
    (signals curl-error (setopt handle :dns-cache-timeout -2))))
