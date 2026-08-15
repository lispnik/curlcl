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

   #:handle-closed

   ;; Error-code translation, exported because a caller handling an EASY-ERROR
   ;; often wants to compare against a keyword rather than a magic integer.
   #:curlcode-keyword
   #:curlcode-value

   ;; The easy handle.  WITH-EASY is the interface that should normally be
   ;; used: there are no finalizers, so a handle dropped without CLOSE-HANDLE
   ;; leaks a socket and a connection cache until the image exits.
   #:easy-handle
   #:make-easy-handle
   #:close-handle
   #:with-easy
   #:handle-pointer
   #:handle-closed-p
   #:handle-error-buffer
   #:handle-share
   #:handle-from-pointer

   ;; Driving a transfer.
   #:setopt
   #:setopts
   #:getinfo
   #:perform
   #:reset-handle
   #:duplicate-handle
   #:pause-transfer
   #:resume-transfer
   #:url-escape
   #:url-unescape

   ;; Callbacks.  SETF of CALLBACK-FUNCTION installs a Lisp closure; the
   ;; contract for each slot is on that function's documentation.
   #:callback-function
   #:callback-slot-names
   #:live-callback-count

   ;; Option and info introspection, for callers that want to ask what this
   ;; libcurl supports before using it.
   #:find-option
   #:find-info
   #:option-id
   #:option-kind
   #:option-deprecated
   #:option-supported-p
   #:info-id
   #:info-kind))
