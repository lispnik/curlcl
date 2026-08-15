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
   #:handle-plist
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
   #:info-kind

   ;; The URL parser.  Worth using over a Lisp one because it is the parser
   ;; libcurl will apply to the transfer, quirks included.
   #:url
   #:make-url
   #:close-url
   #:with-url
   #:duplicate-url
   #:url-pointer
   #:url-part
   #:url-closed-p
   #:url-string
   #:parse-url
   #:url-join

   ;; Response headers, as libcurl parsed them.
   #:http-header
   #:header-name
   #:header-value
   #:header-amount
   #:header-index
   #:header-origin
   #:response-header
   #:response-header-value
   #:response-headers

   ;; Multipart bodies.  Only curl_mime_* is bound; curl_formadd is deprecated
   ;; throughout and variadic with a sentinel list.
   #:mime
   #:make-mime
   #:mime-pointer
   #:mime-freed-p
   #:add-mime-part
   #:attach-mime
   #:set-mime-body

   ;; Shared cookies, DNS, TLS sessions and connections.  Lock callbacks are
   ;; always installed, so a share is safe across threads.
   #:share-handle
   #:make-share
   #:share-closed-p
   #:share-pointer
   #:close-share
   #:with-share
   #:share-data
   #:unshare-data
   #:attach-share
   #:detach-share

   ;; The multi interface.  RUN-TRANSFERS is the ordinary way in; the socket
   ;; API below it is for embedding in an event loop that already exists.
   #:multi-handle
   #:make-multi
   #:close-multi
   #:with-multi
   #:multi-pointer
   #:multi-closed-p
   #:multi-setopt
   #:multi-transfers
   #:add-transfer
   #:remove-transfer
   #:multi-perform
   #:multi-poll
   #:multi-wait
   #:multi-wakeup
   #:multi-timeout
   #:read-multi-messages
   #:run-transfers
   #:signal-failed-transfers
   #:transfer-result
   #:result-handle
   #:result-code
   #:result-code-name
   #:result-condition
   #:result-successful-p
   #:multi-socket-function
   #:multi-timer-function
   #:socket-action))
