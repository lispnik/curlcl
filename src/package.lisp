;;;; src/package.lisp — the single CURLCL package.
;;;;
;;;; One package for the whole library, per the usual convention here.  The
;;;; CURL nickname is what callers actually type.  Foreign symbols are never
;;;; exported: the raw %-prefixed DEFCFUNs are internal, and everything public
;;;; is a hand-written wrapper that takes Lisp values and signals Lisp
;;;; conditions.

(defpackage #:curlcl
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
   #:global-trace
   #:parse-http-date
   #:available-ssl-backends
   #:select-ssl-backend

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
   #:unsafe-retry
   #:unsafe-retry-sink

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
   #:import-tls-session
   #:tls-session-import-supported-p
   #:export-tls-sessions
   #:tls-session-export-supported-p

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
   #:add-streaming-mime-part

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
   #:socket-action
   #:multi-handles
   #:multi-waitfds
   #:multi-waitfds-supported-p
   #:assign-socket-data
   #:multi-push-function
   #:push-header
   #:multi-statistic
   #:multi-notify-function
   #:enable-multi-notification
   #:disable-multi-notification
   #:multi-notifications-supported-p

   ;; Websockets.  Feature-gated at runtime: macOS ships the headers for a
   ;; libcurl built without ws/wss, so the symbols resolve and then fail at
   ;; connect time.  libcurl marks this API experimental; that caveat is
   ;; passed on rather than hidden.
   #:websockets-supported-p
   #:ws-connect
   #:with-websocket
   #:ws-send
   #:ws-send-text
   #:ws-receive
   #:ws-receive-text
   #:ws-close
   #:ws-frame-info
   #:ws-start-frame
   #:ws-start-frame-supported-p
   #:ws-frame
   #:frame-flags
   #:frame-offset
   #:frame-bytes-left
   #:frame-length

   ;; The HTTP client.  The verbs are HTTP-GET and HTTP-POST rather than GET
   ;; and POST because CL:GET and CL:DELETE already exist, and shadowing them
   ;; in a library other code will :USE costs more than the shorter name is
   ;; worth.  A non-2xx status is a response, not an error; only transport
   ;; failures signal.
   #:request
   #:http-get
   #:http-post
   #:http-put
   #:http-patch
   #:http-delete
   #:http-head
   #:http-options
   #:download
   #:stream-reader
   #:stream-seeker
   #:request-many

   ;; Responses.
   #:response
   #:response-status
   #:response-body
   #:response-url
   #:response-version
   #:response-request-method
   #:response-timings
   #:response-redirect-count
   #:response-size-download
   #:response-size-upload
   #:response-header-values
   #:response-content-type
   #:response-text
   #:response-octets
   #:successful-response-p
   #:parse-content-type
   #:decode-body

   ;; Retry policy.  libcurl has none of its own; POST and PATCH are excluded
   ;; from retries unless asked, since only the caller knows whether repeating
   ;; one duplicates an effect.
   #:retry-policy
   #:make-retry-policy
   #:make-retry
   #:*retryable-codes*
   #:*retryable-statuses*
   #:*idempotent-methods*
   #:*jitter-random-state*

   ;; Sessions: pooled handles over a share, so connections, DNS answers, TLS
   ;; sessions and cookies are common to a run of requests.
   #:session
   #:make-session
   #:close-session
   #:with-session
   #:session-share
   #:session-closed-p
   #:session-defaults
   #:acquire-handle
   #:release-handle
   #:with-session-handle
   #:session-cookies
   #:clear-session-cookies

   ;; Defaults worth overriding.
   #:*default-user-agent*
   #:*default-timeout*
   #:*default-connect-timeout*))
