;;;; src/library.lisp — finding, loading and interrogating libcurl.
;;;;
;;;; Which libcurl gets loaded is a real decision on macOS, not a formality.
;;;; The name "libcurl.4.dylib" resolves through the dyld shared cache to the
;;;; system library, which on current macOS is 8.7.1 built against
;;;; SecureTransport/LibreSSL and *without* websockets; Homebrew's is much
;;;; newer, built against OpenSSL, and has ws/wss -- but only reachable by
;;;; absolute path.  We therefore try the Homebrew paths first and fall back to
;;;; the system one.  Note that the system library has no file on disk at all
;;;; (it lives only in the shared cache), so PROBE-FILE is not a valid way to
;;;; test for it -- only dlopen can answer.
;;;;
;;;; Everything downstream that depends on the version asks this file rather
;;;; than assuming, because the two libcurls differ in options, info values,
;;;; error codes and available protocols.
;;;;
;;;; The strerror family lives here, ahead of the subsystems that own the rest
;;;; of their C API, because conditions signalled during setup need it.

(in-package #:libcurl)

(cffi:define-foreign-library libcurl
  ;; Absolute Homebrew paths first: a bare soname would be answered by the
  ;; older system library in the shared cache before Homebrew is ever
  ;; consulted.  /opt/homebrew is Apple Silicon, /usr/local is Intel.
  (:darwin (:or "/opt/homebrew/opt/curl/lib/libcurl.4.dylib"
                "/usr/local/opt/curl/lib/libcurl.4.dylib"
                "libcurl.4.dylib"
                "libcurl.dylib"))
  (:unix (:or "libcurl.so.4" "libcurl.so"))
  (:windows (:or "libcurl.dll" "libcurl-x64.dll"))
  (t (:default "libcurl")))

(defvar *libcurl-pathname* nil
  "Namestring of the libcurl actually loaded, or NIL if it is not loaded yet.")

(defun libcurl-pathname ()
  "The path of the libcurl in use, as reported by CFFI after loading."
  *libcurl-pathname*)

(defun libcurl-loaded-p ()
  (and *libcurl-pathname* t))

(defun load-libcurl (&optional pathname)
  "Load libcurl and return its pathname.

PATHNAME, or the LIBCURL_LIBRARY environment variable, overrides the search
order entirely -- use either to pin a specific build.  Signals LIBRARY-NOT-FOUND
if nothing loads."
  (let ((explicit (or pathname (uiop:getenv "LIBCURL_LIBRARY"))))
    (handler-case
        (let ((library (if explicit
                           (cffi:load-foreign-library explicit)
                           (cffi:use-foreign-library libcurl))))
          (setf *libcurl-pathname*
                (let ((path (ignore-errors (cffi:foreign-library-pathname library))))
                  (if path (namestring path) (princ-to-string (or explicit "libcurl"))))))
      (cffi:load-foreign-library-error (e)
        (declare (ignore e))
        (error 'library-not-found
               :candidates (if explicit
                               (list explicit)
                               '("/opt/homebrew/opt/curl/lib/libcurl.4.dylib"
                                 "/usr/local/opt/curl/lib/libcurl.4.dylib"
                                 "libcurl.4.dylib" "libcurl.so.4" "libcurl.so")))))))

;;; The strerror family -------------------------------------------------------
;;;
;;; Declared with :int rather than the enum types, because a newer libcurl may
;;; return a code this build of the binding has never heard of and CFFI's enum
;;; translation would refuse it.  Decoding is done tolerantly in types.lisp.

(cffi:defcfun ("curl_easy_strerror" %curl-easy-strerror) :string (code :int))
(cffi:defcfun ("curl_multi_strerror" %curl-multi-strerror) :string (code :int))
(cffi:defcfun ("curl_share_strerror" %curl-share-strerror) :string (code :int))
(cffi:defcfun ("curl_url_strerror" %curl-url-strerror) :string (code :int))

;;; Version info --------------------------------------------------------------
;;;
;;; struct curl_version_info_data grew a field group per libcurl generation and
;;; the AGE member says how far it is safe to read.  We ask for the newest age
;;; we know about; libcurl answers with its own, which may be older, and every
;;; accessor below is gated on it.  Reading past AGE is reading uninitialised
;;; memory, not merely getting a stale answer.

(cffi:defcenum curl-version
  (:first 0) (:second 1) (:third 2) (:fourth 3) (:fifth 4) (:sixth 5)
  (:seventh 6) (:eighth 7) (:ninth 8) (:tenth 9) (:eleventh 10) (:twelfth 11))

(defconstant +curl-version-now+ 11
  "CURLVERSION_NOW as of libcurl 8.x -- CURLVERSION_TWELFTH.")

(cffi:defcstruct curl-version-info-data
  ;; CURLVERSION_FIRST
  (age :int)
  (version :pointer)
  (version-num :unsigned-int)
  (host :pointer)
  (features :int)
  (ssl-version :pointer)
  (ssl-version-num :long)
  (libz-version :pointer)
  (protocols :pointer)
  ;; CURLVERSION_SECOND
  (ares :pointer)
  (ares-num :int)
  ;; CURLVERSION_THIRD
  (libidn :pointer)
  ;; CURLVERSION_FOURTH
  (iconv-ver-num :int)
  (libssh-version :pointer)
  ;; CURLVERSION_FIFTH
  (brotli-ver-num :unsigned-int)
  (brotli-version :pointer)
  ;; CURLVERSION_SIXTH
  (nghttp2-ver-num :unsigned-int)
  (nghttp2-version :pointer)
  (quic-version :pointer)
  ;; CURLVERSION_SEVENTH
  (cainfo :pointer)
  (capath :pointer)
  ;; CURLVERSION_EIGHTH
  (zstd-ver-num :unsigned-int)
  (zstd-version :pointer)
  ;; CURLVERSION_NINTH
  (hyper-version :pointer)
  ;; CURLVERSION_TENTH
  (gsasl-version :pointer)
  ;; CURLVERSION_ELEVENTH
  (feature-names :pointer)
  ;; CURLVERSION_TWELFTH
  (rtmp-version :pointer))

(cffi:defcfun ("curl_version_info" %curl-version-info) :pointer (age :int))
(cffi:defcfun ("curl_version" %curl-version) :string)

;;; The historical feature bitmask.  Only consulted when libcurl is too old to
;;; supply the open-ended feature_names array; bit 31 is unavailable because
;;; `features' is a signed int and THREADSAFE already claims bit 30.
(defparameter *feature-bits*
  '((:ipv6 . 0) (:kerberos4 . 1) (:ssl . 2) (:libz . 3) (:ntlm . 4)
      (:gssnegotiate . 5) (:debug . 6) (:asynchdns . 7) (:spnego . 8)
      (:largefile . 9) (:idn . 10) (:sspi . 11) (:conv . 12) (:curldebug . 13)
      (:tlsauth-srp . 14) (:ntlm-wb . 15) (:http2 . 16) (:gssapi . 17)
      (:kerberos5 . 18) (:unix-sockets . 19) (:psl . 20) (:https-proxy . 21)
      (:multi-ssl . 22) (:brotli . 23) (:altsvc . 24) (:http3 . 25)
    (:zstd . 26) (:unicode . 27) (:hsts . 28) (:gsasl . 29)
    (:threadsafe . 30)))

(defun %string-array-to-list (pointer)
  "Collect a NULL-terminated char** into a list of strings."
  (unless (cffi:null-pointer-p pointer)
    (loop for i from 0
          for s = (cffi:mem-aref pointer :pointer i)
          until (cffi:null-pointer-p s)
          collect (cffi:foreign-string-to-lisp s))))

(defun %maybe-string (pointer)
  (unless (cffi:null-pointer-p pointer)
    (cffi:foreign-string-to-lisp pointer)))

(defstruct (version-info (:conc-name version-info-))
  "A decoded snapshot of curl_version_info, gated on the struct's AGE field.
Fields libcurl is too old to supply are NIL rather than garbage."
  age version version-num host features feature-names protocols
  ssl-version libz-version libssh-version brotli-version nghttp2-version
  quic-version zstd-version gsasl-version rtmp-version cainfo capath)

(defvar *version-info* nil)

(defun %read-version-info ()
  (let ((p (%curl-version-info +curl-version-now+)))
    (when (cffi:null-pointer-p p)
      (error 'curl-error :message "curl_version_info returned NULL"))
    (macrolet ((slot (name) `(cffi:foreign-slot-value
                              p '(:struct curl-version-info-data) ',name))
               ;; Every field group after the first is only present if libcurl
               ;; reported at least the age that introduced it.
               (when-age (age &body body) `(when (>= age-value ,age) ,@body)))
      (let ((age-value (slot age)))
        (make-version-info
         :age age-value
         :version (%maybe-string (slot version))
         :version-num (slot version-num)
         :host (%maybe-string (slot host))
         :features (loop for (name . bit) in *feature-bits*
                         when (logbitp bit (slot features)) collect name)
         :protocols (%string-array-to-list (slot protocols))
         :ssl-version (%maybe-string (slot ssl-version))
         :libz-version (%maybe-string (slot libz-version))
         :libssh-version (when-age 3 (%maybe-string (slot libssh-version)))
         :brotli-version (when-age 4 (%maybe-string (slot brotli-version)))
         :nghttp2-version (when-age 5 (%maybe-string (slot nghttp2-version)))
         :quic-version (when-age 5 (%maybe-string (slot quic-version)))
         :cainfo (when-age 6 (%maybe-string (slot cainfo)))
         :capath (when-age 6 (%maybe-string (slot capath)))
         :zstd-version (when-age 7 (%maybe-string (slot zstd-version)))
         :gsasl-version (when-age 9 (%maybe-string (slot gsasl-version)))
         ;; feature_names is open-ended where the bitmask has run out of room,
         ;; so prefer it when present: it is what `curl --version' prints.
         :feature-names (when-age 10 (%string-array-to-list (slot feature-names)))
         :rtmp-version (when-age 11 (%maybe-string (slot rtmp-version))))))))

(defun libcurl-version-info (&key refresh)
  "A VERSION-INFO struct describing the loaded libcurl.  Cached; :REFRESH re-reads."
  (when (or refresh (null *version-info*))
    (setf *version-info* (%read-version-info)))
  *version-info*)

(defun libcurl-version ()
  "The version string of the loaded libcurl, e.g. \"8.21.0\"."
  (version-info-version (libcurl-version-info)))

(defun libcurl-version-number ()
  "The loaded libcurl's version as the packed integer 0xMMNNPP."
  (version-info-version-num (libcurl-version-info)))

(defun version-at-least-p (major minor &optional (patch 0))
  "True when the loaded libcurl is at least MAJOR.MINOR.PATCH."
  (>= (libcurl-version-number)
      (logior (ash major 16) (ash minor 8) patch)))

(defun libcurl-features ()
  "Features of the loaded libcurl as a list of keywords.

Taken from the open-ended feature_names array when libcurl is new enough to
supply it, since the historical bitmask has run out of bits."
  (let ((info (libcurl-version-info)))
    (or (mapcar (lambda (name)
                  (intern (substitute #\- #\_ (string-upcase name)) :keyword))
                (version-info-feature-names info))
        (version-info-features info))))

(defun libcurl-protocols ()
  "Protocols the loaded libcurl can speak, as a list of lowercase strings."
  (version-info-protocols (libcurl-version-info)))

(defun feature-supported-p (feature)
  "True when the loaded libcurl was built with FEATURE, e.g. :HTTP2 or :SSL."
  (and (member feature (libcurl-features) :test #'string-equal) t))

(defun protocol-supported-p (protocol)
  "True when the loaded libcurl can speak PROTOCOL, e.g. \"ws\" or :https."
  (and (member (string-downcase (string protocol)) (libcurl-protocols)
               :test #'string-equal)
       t))

;;; Global initialisation -----------------------------------------------------

(defconstant +curl-global-nothing+ 0)
(defconstant +curl-global-ssl+ 1)
(defconstant +curl-global-win32+ 2)
(defconstant +curl-global-all+ 3)
(defconstant +curl-global-default+ +curl-global-all+)
(defconstant +curl-global-ack-eintr+ 4)

(defconstant +curl-error-size+ 256
  "CURL_ERROR_SIZE -- the minimum size of a CURLOPT_ERRORBUFFER buffer.")

(cffi:defcfun ("curl_global_init" %curl-global-init) :int (flags :long))
(cffi:defcfun ("curl_global_cleanup" %curl-global-cleanup) :void)

(defvar *global-initialized-p* nil)

(defun global-init (&optional (flags +curl-global-default+))
  "Run curl_global_init once.  Idempotent; returns T if libcurl is initialised.

libcurl documents this as thread-safe only when the THREADSAFE feature bit is
set, so on a build without it the first call should happen before any threads
are started -- which is what loading this system does."
  (unless *global-initialized-p*
    (let ((code (%curl-global-init flags)))
      (unless (zerop code)
        (error 'easy-error :code code :message (%curl-easy-strerror code)))
      (setf *global-initialized-p* t)))
  t)

(defun global-cleanup ()
  "Run curl_global_cleanup.

Rarely needed: it exists for processes that want to release libcurl's global
state before exiting.  libcurl does not document it as thread-safe, and it must
not be called while any handle is still alive or from a finalizer."
  (when *global-initialized-p*
    (%curl-global-cleanup)
    (setf *global-initialized-p* nil
          *version-info* nil))
  t)

;;; Image save and restore ----------------------------------------------------
;;;
;;; A dumped image comes back with no foreign libraries open and no libcurl
;;; global state, so both have to be redone.  The dump hook drops our cached
;;; state so nothing stale survives into the new image.

(defun %reinitialize ()
  (setf *libcurl-pathname* nil
        *version-info* nil
        *global-initialized-p* nil)
  ;; Close before loading.  CFFI's record of the library as open survives the
  ;; image dump, so USE-FOREIGN-LIBRARY would short-circuit and never dlopen
  ;; anything -- leaving the process bound to whatever libcurl dyld happened to
  ;; pull in at startup.  On macOS that is /usr/lib/libcurl.4.dylib from the
  ;; shared cache, which is a different version with a different TLS backend
  ;; and no websockets, so a dumped executable would silently disagree with the
  ;; same code run from source.
  (ignore-errors (cffi:close-foreign-library 'libcurl))
  (load-libcurl)
  (global-init))

(defun %prepare-for-dump ()
  (setf *version-info* nil
        *global-initialized-p* nil
        *libcurl-pathname* nil)
  ;; Unload before dumping, so the saved image carries no record of libcurl at
  ;; all.  Otherwise SBCL notes the shared object and dyld reopens it at
  ;; startup, before any Lisp runs -- and it reopens it by soname, which on
  ;; macOS resolves through the shared cache to /usr/lib/libcurl.4.dylib.  Two
  ;; libcurls then sit in the process, SBCL's symbol lookup is global and finds
  ;; the first, and a dumped executable silently runs against a different
  ;; version with a different TLS backend and no websockets than the same code
  ;; run from source.  Closing here leaves the restore hook to open exactly one.
  (ignore-errors (cffi:close-foreign-library 'libcurl)))

(uiop:register-image-dump-hook '%prepare-for-dump)
(uiop:register-image-restore-hook '%reinitialize nil)

(load-libcurl)
(global-init)

;;; Global utilities ----------------------------------------------------------

(cffi:defcfun ("curl_getdate" %curl-getdate) :long
  (date :string) (unused :pointer))

(defconstant +unix-epoch-universal-time+
  (encode-universal-time 0 0 0 1 1 1970 0)
  "Universal time of the Unix epoch, for converting time_t results.")

(defun parse-http-date (string)
  "Parse an HTTP or RFC 822/1123/850 date into a universal time, or NIL.

Uses libcurl's own parser, which accepts every format seen in the wild --
including the asctime and RFC 850 forms that Date and Expires headers still
turn up in."
  (let ((seconds (%curl-getdate string (cffi:null-pointer))))
    (unless (minusp seconds)
      (+ seconds +unix-epoch-universal-time+))))

(cffi:defcfun ("curl_global_trace" %curl-global-trace) :int (config :string))

(defun global-trace (configuration)
  "Configure libcurl's internal tracing, e.g. \"all\" or \"http/2,ssl\".

Affects what a debug callback receives.  Best called before any transfer
starts; libcurl documents it as thread-safe only when the THREADSAFE feature
bit is set."
  (let ((code (%curl-global-trace configuration)))
    (unless (zerop code)
      (error 'easy-error :code code :code-name (curlcode-keyword code)
                         :message (%curl-easy-strerror code))))
  t)

(cffi:defcstruct curl-ssl-backend
  (id :int)
  (name :pointer))

(cffi:defcfun ("curl_global_sslset" %curl-global-sslset) :int
  (id :int) (name :string) (available :pointer))

(defparameter *ssl-backends*
  '((:none . 0) (:openssl . 1) (:gnutls . 2) (:nss . 3) (:obsolete4 . 4)
    (:gskit . 5) (:polarssl . 6) (:wolfssl . 7) (:schannel . 8)
    (:securetransport . 9) (:axtls . 10) (:mbedtls . 11) (:mesalink . 12)
    (:bearssl . 13) (:rustls . 14))
  "curl_sslbackend.  Explicit values: the enum is not sequential in meaning and
several members are long deprecated.")

(defun available-ssl-backends ()
  "The TLS backends this libcurl was built with, as a list of (keyword . name).

Only interesting for a multi-SSL build; an ordinary one reports the single
backend it was compiled against."
  (cffi:with-foreign-object (available :pointer)
    (setf (cffi:mem-ref available :pointer) (cffi:null-pointer))
    ;; -1 is CURLSSLBACKEND_NONE used as "just tell me what there is": the call
    ;; reports CURLSSLSET_UNKNOWN_BACKEND but still fills in the list.
    (%curl-global-sslset -1 (cffi:null-pointer) available)
    (let ((array (cffi:mem-ref available :pointer)))
      (unless (cffi:null-pointer-p array)
        (loop for i from 0
              for entry = (cffi:mem-aref array :pointer i)
              until (cffi:null-pointer-p entry)
              collect (let ((id (cffi:foreign-slot-value
                                 entry '(:struct curl-ssl-backend) 'id))
                            (name (cffi:foreign-slot-value
                                   entry '(:struct curl-ssl-backend) 'name)))
                        (cons (or (car (rassoc id *ssl-backends*)) id)
                              (unless (cffi:null-pointer-p name)
                                (cffi:foreign-string-to-lisp name)))))))))

(defun select-ssl-backend (backend)
  "Choose the TLS backend, for a libcurl built with more than one.

Must run before GLOBAL-INIT -- which loading this system already did, so this
is only usable from an image where GLOBAL-CLEANUP has been called.  Signals
UNSUPPORTED-FEATURE if it is too late or the backend is unknown."
  (let* ((id (or (cdr (assoc backend *ssl-backends*))
                 (error 'curl-error
                        :message (format nil "Unknown TLS backend ~S." backend))))
         (code (%curl-global-sslset id (cffi:null-pointer) (cffi:null-pointer))))
    (case code
      (0 t)
      (2 (error 'unsupported-feature
                :name (format nil "selecting the ~S TLS backend" backend)
                :message "curl_global_sslset was called too late; libcurl is
already initialised."))
      (t (error 'unsupported-feature
                :name (format nil "the ~S TLS backend" backend)
                :message "This libcurl was not built with that backend.")))))
