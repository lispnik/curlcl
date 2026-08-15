;;;; test/package.lisp

(defpackage #:libcurl/test
  (:use #:cl #:fiveam #:libcurl)
  ;; FiveAM's TEST is the test-defining macro here, not anything of ours.
  (:shadowing-import-from #:fiveam #:test)
  (:export #:run-tests
           #:all-tests
           ;; Suites.
           #:library #:types #:varargs))

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

(defun run-tests ()
  "Run every libcurl test.  Returns true when they all pass."
  (let ((results (run 'all-tests)))
    (explain! results)
    (results-status results)))
