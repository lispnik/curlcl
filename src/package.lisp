;;;; src/package.lisp — the single LIBCURL package.
;;;;
;;;; One package for the whole library, per the usual convention here.  The
;;;; CURL nickname is what callers actually type.  Foreign symbols are never
;;;; exported: the raw %-prefixed DEFCFUNs are internal, and everything public
;;;; is a hand-written wrapper that takes Lisp values and signals Lisp
;;;; conditions.

(defpackage #:libcurl
  (:nicknames #:curl)
  (:use #:cl)
  (:export
   ;; Loading, identity and capabilities of the libcurl we are bound to.
   ;; These matter more than usual here: macOS ships one libcurl in the dyld
   ;; shared cache and Homebrew installs another, and they differ in options,
   ;; info values and whether websockets exist at all.  Everything
   ;; version-sensitive is decided at runtime by asking the loaded library.
   #:load-libcurl
   #:libcurl-loaded-p
   #:libcurl-pathname
   #:libcurl-version
   #:libcurl-version-number
   #:libcurl-version-info
   #:libcurl-features
   #:libcurl-protocols
   #:feature-supported-p
   #:protocol-supported-p
   #:version-at-least-p
   #:global-init
   #:global-cleanup

   ;; Conditions.  Every failure path in the library ends in one of these;
   ;; no wrapper returns a bare CURLcode to the caller.
   #:curl-condition
   #:curl-error
   #:curl-error-code
   #:curl-error-code-name
   #:curl-error-message
   #:curl-error-detail
   #:easy-error
   #:multi-error
   #:share-error
   #:url-error
   #:header-error
   #:callback-error
   #:callback-error-cause
   #:callback-error-kind
   #:unsupported-option
   #:unsupported-option-name
   #:unsupported-feature
   #:library-not-found

   ;; Error-code translation, exported because a caller handling an EASY-ERROR
   ;; often wants to compare against a keyword rather than a magic integer.
   #:curlcode-keyword
   #:curlcode-value))
