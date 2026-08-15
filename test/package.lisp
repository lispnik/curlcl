;;;; test/package.lisp

(defpackage #:libcurl/test
  (:use #:cl #:fiveam #:libcurl)
  ;; FiveAM's TEST is the test-defining macro here, not anything of ours.
  (:shadowing-import-from #:fiveam #:test)
  (:export #:run-tests
           #:all-tests
           ;; Suites.
           #:library #:types #:varargs #:tables #:easy
           #:url #:headers #:mime #:share #:multi #:websockets #:client))

(in-package #:libcurl/test)

(def-suite all-tests
  :description "Every libcurl test.")

(def-suite library
  :description "Loading libcurl, and what the loaded library reports about itself."
  :in all-tests)

(def-suite types
  :description "Hand-written foreign layouts, against the real ones."
  :in all-tests)

(def-suite varargs
  :description "The libffi variadic call layer for setopt and getinfo."
  :in all-tests)

(def-suite tables
  :description "The generated option and info tables, against the loaded library."
  :in all-tests)

(def-suite easy
  :description "The easy handle, moving real bytes against the local server."
  :in all-tests)

(def-suite url
  :description "libcurl's URL parser."
  :in all-tests)

(def-suite headers
  :description "Response headers, as libcurl parsed them."
  :in all-tests)

(def-suite mime
  :description "Multipart bodies."
  :in all-tests)

(def-suite share
  :description "Shared cookies, DNS, TLS sessions and connections."
  :in all-tests)

(def-suite multi
  :description "Many transfers on one thread."
  :in all-tests)

(def-suite client
  :description "The HTTP client: requests, responses, retry, sessions."
  :in all-tests)

(def-suite websockets
  :description "Websockets, against a local echo server; skipped when the
loaded libcurl was built without ws/wss."
  :in all-tests)

(defun run-tests ()
  "Run every libcurl test.  Returns true when they all pass."
  (let ((results (run 'all-tests)))
    (explain! results)
    (results-status results)))
