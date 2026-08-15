;;;; libcurl.asd
;;;;
;;;; Three systems.  #:libcurl is the binding and the HTTP client built on it.
;;;; #:libcurl/generator is a build-time tool that parses the installed curl
;;;; headers and emits src/options-table.lisp and src/infos-table.lisp; those
;;;; files are committed, so the generator is never needed to build or use the
;;;; library, and it deliberately does not depend on #:libcurl.
;;;; #:libcurl/cli builds bin/curlcl, a curl-compatible driver, with ASDF's
;;;; program-op -- `make build', or (asdf:make :libcurl/cli).
;;;; #:libcurl/test is the FiveAM suite.  It runs an HTTP server and a
;;;; websocket echo server inside the image, so the integration tests are
;;;; hermetic; the one suite that uses the real network is skipped unless
;;;; CURL_LIVE_TESTS is set.

(asdf:defsystem #:libcurl
  :description "A comprehensive, idiomatic Common Lisp binding to libcurl."
  :long-description
  "Complete coverage of the libcurl C API -- the easy interface with all of its
options and info values, multi, share, the URL API, MIME, the header API and
websockets -- wrapped so that it reads like Lisp: CLOS handles, keyword-named
options, WITH- macros for every foreign resource, and every error surfaced
through the condition system.

Two things distinguish it.  First, curl_easy_setopt and its siblings are
genuine variadic functions, and under the Darwin arm64 ABI variadic arguments
are passed on the stack rather than in registers, so an ordinary CFFI call
silently hands libcurl garbage; every option and info call therefore goes
through libffi's ffi_prep_cif_var with cifs prepared once at load time.
Second, callbacks are safe: a Lisp condition signalled inside a write, read or
progress callback can never unwind into C.  It is caught at the boundary, the
callback returns the abort sentinel libcurl expects for that particular
callback, and the original condition is re-signalled from Lisp once
curl_easy_perform has returned -- so the caller sees their own error, not a
generic CURLE_WRITE_ERROR."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :maintainer "Matthew Kennedy <burnsidemk@gmail.com>"
  :mailto "burnsidemk@gmail.com"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/lispnik/libcurl"
  :source-control (:git "https://github.com/lispnik/libcurl.git")
  :bug-tracker "https://github.com/lispnik/libcurl/issues"
  :serial t
  ;; cffi-libffi is not optional: it is what makes the variadic setopt/getinfo
  ;; calls correct on stack-passing ABIs.  See src/varargs.lisp.
  :depends-on (#:cffi #:cffi-libffi #:alexandria #:bordeaux-threads)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "conditions") ; no foreign calls: pure classes
                             (:file "library")    ; loading, version, features, global init
                             (:file "types")      ; ctypes, enums, structs
                             (:file "varargs")    ; the libffi variadic call layer
                             (:file "easy-raw")   ; raw bindings + option introspection
                             (:file "memory")     ; owned foreign memory, octets
                             (:file "options")
                             (:file "options-table") ; generated
                             (:file "infos")
                             (:file "infos-table")   ; generated
                             (:file "callbacks")  ; registry + trampolines
                             (:file "easy")       ; the EASY-HANDLE class
                             (:file "url")        ; the URL parser
                             (:file "headers")    ; parsed response headers
                             (:file "mime")       ; multipart bodies
                             (:file "share")      ; shared cookies/DNS/connections
                             (:file "multi")      ; many transfers on one thread
                             (:file "websockets")    ; feature-gated at runtime
                             ;; The HTTP client, built on everything above.
                             (:file "client-response")
                             (:file "client-retry")
                             (:file "client-session") ; before client-request:
                                        ; WITH-SESSION-HANDLE is a macro
                             (:file "client-request"))))
  :in-order-to ((test-op (test-op #:libcurl/test))))

(asdf:defsystem #:libcurl/cli
  :description "curlcl, a curl-compatible command-line driver for libcurl."
  :long-description
  "A working curl(1) workalike rather than a demo: option names, defaults,
output destinations and exit codes follow curl, so most curl command lines
work unchanged and scripts checking the exit status keep working -- the codes
are libcurl's own CURLcode values.  Holding to that constraint is what forces
the library underneath to cover what a real client needs rather than what is
convenient to expose."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :maintainer "Matthew Kennedy <burnsidemk@gmail.com>"
  :mailto "burnsidemk@gmail.com"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/lispnik/libcurl"
  :source-control (:git "https://github.com/lispnik/libcurl.git")
  :bug-tracker "https://github.com/lispnik/libcurl/issues"
  :serial t
  :depends-on (#:libcurl #:clingon #:alexandria)
  :components ((:module "src"
                :components ((:file "cli"))))
  ;; Build with (asdf:make :libcurl/cli), producing bin/curlcl.  The dumped
  ;; image reopens libcurl and re-prepares the libffi cifs through the restore
  ;; hooks in src/library.lisp and src/varargs.lisp; without those the binary
  ;; would start with a stale library handle and mis-pass every option.
  :build-operation "program-op"
  :build-pathname "bin/curlcl"
  :entry-point "libcurl/cli:main")

(asdf:defsystem #:libcurl/generator
  :description "Generates libcurl's option and info tables from the curl headers."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :maintainer "Matthew Kennedy <burnsidemk@gmail.com>"
  :mailto "burnsidemk@gmail.com"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/lispnik/libcurl"
  :source-control (:git "https://github.com/lispnik/libcurl.git")
  :bug-tracker "https://github.com/lispnik/libcurl/issues"
  :serial t
  ;; Deliberately does NOT depend on #:libcurl: it is a text-processing tool
  ;; that writes two of that system's source files, so requiring the system it
  ;; generates for would be circular the first time it is run.
  :depends-on (#:cl-ppcre #:alexandria)
  :components ((:module "generator"
                :serial t
                :components ((:file "generate-tables")))))

(asdf:defsystem #:libcurl/test
  :description "Tests for libcurl."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :maintainer "Matthew Kennedy <burnsidemk@gmail.com>"
  :mailto "burnsidemk@gmail.com"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/lispnik/libcurl"
  :source-control (:git "https://github.com/lispnik/libcurl.git")
  :bug-tracker "https://github.com/lispnik/libcurl/issues"
  :serial t
  :depends-on (#:libcurl #:libcurl/cli #:fiveam #:usocket #:bordeaux-threads)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "library-tests")
                             (:file "types-tests")
                             (:file "varargs-tests")
                             (:file "tables-tests")
                             (:file "server")     ; in-process HTTP fixture
                             (:file "proxy")      ; in-process HTTP proxy
                             (:file "callback-tests")
                             (:file "easy-tests")
                             (:file "url-tests")
                             (:file "headers-tests")
                             (:file "mime-tests")
                             (:file "share-tests")
                             (:file "multi-tests")
                             (:file "ws-server")  ; websocket echo fixture
                             (:file "ws-tests")
                             (:file "connection-tests")
                             (:file "cookie-tests")
                             (:file "client-tests")
                             (:file "cli-tests")  ; needs bin/curlcl for some
                             (:file "live-tests"))))   ; opt-in; needs network
  ;; ASDF ignores whatever a TEST-OP perform method returns, so reporting
  ;; failure by returning NIL would leave `asdf:test-system' -- and therefore
  ;; CI -- green on a suite that failed.  Signal.
  :perform (test-op (o c)
             (declare (ignore o c))
             (unless (uiop:symbol-call :libcurl/test '#:run-tests)
               (error "The libcurl test suite failed."))))
