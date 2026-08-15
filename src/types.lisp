;;;; src/types.lisp — foreign types, enumerations and structures.
;;;;
;;;; Layouts are written by hand rather than groveled, so the build needs no C
;;;; toolchain and no curl headers.  The sizes this file assumes are asserted
;;;; by the test suite against the loaded library, which is what keeps a
;;;; hand-written layout honest.
;;;;
;;;; Every enumerator carries an explicit value.  None of libcurl's enums can
;;;; be trusted to number implicitly from zero: CURLMcode starts at -1, CURLcode
;;;; is peppered with CURLE_OBSOLETEnn placeholders whose numbers must be kept,
;;;; and the CURLINFO family encodes its return type in the high bits of the
;;;; value.
;;;;
;;;; Result codes are decoded tolerantly.  A libcurl newer than this binding can
;;;; return a code we have never heard of, and CFFI's enum translation would
;;;; signal on it -- turning "the transfer failed with an unfamiliar code" into
;;;; "the binding is broken".  So the C functions are declared :INT and decoded
;;;; through CURLCODE-KEYWORD, which yields the raw integer when it does not
;;;; recognise it.

(in-package #:libcurl)

;;; Scalar types --------------------------------------------------------------
;;;
;;; curl_off_t is `long' on LP64 and `long long' on 32-bit Apple targets; both
;;; are 64 bits, so :INT64 is right everywhere we support.  Getting this width
;;; wrong is not benign -- CURLOPT_*_LARGE options are passed variadically and a
;;; short write would leave libcurl reading half a value off the stack.
(cffi:defctype curl-off-t :int64)
(cffi:defctype curl-socket-t :int)
(cffi:defctype curl-socklen-t :uint32)

(defconstant +curl-socket-bad+ -1)

;;; Result codes --------------------------------------------------------------

(cffi:defcenum curlcode
  (:ok                          0)
  (:unsupported-protocol        1)
  (:failed-init                 2)
  (:url-malformat               3)
  (:not-built-in                4)
  (:couldnt-resolve-proxy       5)
  (:couldnt-resolve-host        6)
  (:couldnt-connect             7)
  (:weird-server-reply          8)
  (:remote-access-denied        9)
  (:ftp-accept-failed           10)
  (:ftp-weird-pass-reply        11)
  (:ftp-accept-timeout          12)
  (:ftp-weird-pasv-reply        13)
  (:ftp-weird-227-format        14)
  (:ftp-cant-get-host           15)
  (:http2                       16)
  (:ftp-couldnt-set-type        17)
  (:partial-file                18)
  (:ftp-couldnt-retr-file       19)
  (:obsolete20                  20)
  (:quote-error                 21)
  (:http-returned-error         22)
  (:write-error                 23)
  (:obsolete24                  24)
  (:upload-failed               25)
  (:read-error                  26)
  (:out-of-memory               27)
  (:operation-timedout          28)
  (:obsolete29                  29)
  (:ftp-port-failed             30)
  (:ftp-couldnt-use-rest        31)
  (:obsolete32                  32)
  (:range-error                 33)
  (:obsolete34                  34)
  (:ssl-connect-error           35)
  (:bad-download-resume         36)
  (:file-couldnt-read-file      37)
  (:ldap-cannot-bind            38)
  (:ldap-search-failed          39)
  (:obsolete40                  40)
  (:obsolete41                  41)
  (:aborted-by-callback         42)
  (:bad-function-argument       43)
  (:obsolete44                  44)
  (:interface-failed            45)
  (:obsolete46                  46)
  (:too-many-redirects          47)
  (:unknown-option              48)
  (:setopt-option-syntax        49)
  (:obsolete50                  50)
  (:obsolete51                  51)
  (:got-nothing                 52)
  (:ssl-engine-notfound         53)
  (:ssl-engine-setfailed        54)
  (:send-error                  55)
  (:recv-error                  56)
  (:obsolete57                  57)
  (:ssl-certproblem             58)
  (:ssl-cipher                  59)
  (:peer-failed-verification    60)
  (:bad-content-encoding        61)
  (:obsolete62                  62)
  (:filesize-exceeded           63)
  (:use-ssl-failed              64)
  (:send-fail-rewind            65)
  (:ssl-engine-initfailed       66)
  (:login-denied                67)
  (:tftp-notfound               68)
  (:tftp-perm                   69)
  (:remote-disk-full            70)
  (:tftp-illegal                71)
  (:tftp-unknownid              72)
  (:remote-file-exists          73)
  (:tftp-nosuchuser             74)
  (:obsolete75                  75)
  (:obsolete76                  76)
  (:ssl-cacert-badfile          77)
  (:remote-file-not-found       78)
  (:ssh                         79)
  (:ssl-shutdown-failed         80)
  (:again                       81)
  (:ssl-crl-badfile             82)
  (:ssl-issuer-error            83)
  (:ftp-pret-failed             84)
  (:rtsp-cseq-error             85)
  (:rtsp-session-error          86)
  (:ftp-bad-file-list           87)
  (:chunk-failed                88)
  (:no-connection-available     89)
  (:ssl-pinnedpubkeynotmatch    90)
  (:ssl-invalidcertstatus       91)
  (:http2-stream                92)
  (:recursive-api-call          93)
  (:auth-error                  94)
  (:http3                       95)
  (:quic-connect-error          96)
  (:proxy                       97)
  (:ssl-clientcert              98)
  (:unrecoverable-poll          99)
  (:too-large                   100)
  (:ech-required                101))

;;; CURLM_CALL_MULTI_PERFORM is -1 and is NOT an error -- it is a legacy
;;; "call me again immediately" signal.  Any generic "nonzero means failure"
;;; check has to special-case it.
(cffi:defcenum curlmcode
  (:call-multi-perform         -1)
  (:ok                          0)
  (:bad-handle                  1)
  (:bad-easy-handle             2)
  (:out-of-memory               3)
  (:internal-error              4)
  (:bad-socket                  5)
  (:unknown-option              6)
  (:added-already               7)
  (:recursive-api-call          8)
  (:wakeup-failure              9)
  (:bad-function-argument       10)
  (:aborted-by-callback         11)
  (:unrecoverable-poll          12))

(cffi:defcenum curlshcode
  (:ok                          0)
  (:bad-option                  1)
  (:in-use                      2)
  (:invalid                     3)
  (:nomem                       4)
  (:not-built-in                5))

(cffi:defcenum curlucode
  (:ok                          0)
  (:bad-handle                  1)
  (:bad-partpointer             2)
  (:malformed-input             3)
  (:bad-port-number             4)
  (:unsupported-scheme          5)
  (:urldecode                   6)
  (:out-of-memory               7)
  (:user-not-allowed            8)
  (:unknown-part                9)
  (:no-scheme                   10)
  (:no-user                     11)
  (:no-password                 12)
  (:no-options                  13)
  (:no-host                     14)
  (:no-port                     15)
  (:no-query                    16)
  (:no-fragment                 17)
  (:no-zoneid                   18)
  (:bad-file-url                19)
  (:bad-fragment                20)
  (:bad-hostname                21)
  (:bad-ipv6                    22)
  (:bad-login                   23)
  (:bad-password                24)
  (:bad-path                    25)
  (:bad-query                   26)
  (:bad-scheme                  27)
  (:bad-slashes                 28)
  (:bad-user                    29)
  (:lacks-idn                   30)
  (:too-large                   31))

;;; Unlike the others this family has no trailing _LAST sentinel, so
;;; :NOT-BUILT-IN (7) is a real code rather than a bound.
(cffi:defcenum curlhcode
  (:ok                          0)
  (:badindex                    1)
  (:missing                     2)
  (:noheaders                   3)
  (:norequest                   4)
  (:out-of-memory               5)
  (:bad-argument                6)
  (:not-built-in                7))

;;; libcurl provides curl_easy_strerror, curl_multi_strerror,
;;; curl_share_strerror and curl_url_strerror -- but nothing for CURLHcode, so
;;; these messages are ours.
(defparameter *header-code-messages*
  '((:ok . "No error")
    (:badindex . "There is no header with that index")
    (:missing . "No such header exists")
    (:noheaders . "No headers have been received yet")
    (:norequest . "There was no such request number")
    (:out-of-memory . "Out of memory")
    (:bad-argument . "A function argument was not valid")
    (:not-built-in . "The header API was disabled at build time")))

(defun curlcode-keyword (value &optional (type 'curlcode))
  "Decode VALUE into a keyword, or return it unchanged when unrecognised.

Tolerant on purpose: a newer libcurl may report a code this binding predates,
and that should surface as an unfamiliar error rather than a broken binding."
  (or (cffi:foreign-enum-keyword type value :errorp nil) value))

(defun curlcode-value (keyword &optional (type 'curlcode))
  "The integer value of KEYWORD in TYPE, or KEYWORD itself if already an integer."
  (if (integerp keyword)
      keyword
      (cffi:foreign-enum-value type keyword :errorp nil)))

;;; Enumerations used as arguments -------------------------------------------

;;; The debug callback's `type' argument.  Note the unfortunate name collision
;;; with the CURLINFO_* getinfo constants -- these are a completely unrelated
;;; enum that merely shares the prefix in C.
(cffi:defcenum curl-infotype
  (:text 0) (:header-in 1) (:header-out 2) (:data-in 3) (:data-out 4)
  (:ssl-data-in 5) (:ssl-data-out 6))

(cffi:defcenum curl-socktype
  (:ipcxn 0) (:accept 1))

(cffi:defcenum curlioerr
  (:ok 0) (:unknowncmd 1) (:failrestart 2))

(cffi:defcenum curliocmd
  (:nop 0) (:restartread 1))

(cffi:defcenum curl-lock-data
  (:none 0) (:share 1) (:cookie 2) (:dns 3) (:ssl-session 4) (:connect 5)
  (:psl 6) (:hsts 7))

(cffi:defcenum curl-lock-access
  (:none 0) (:shared 1) (:single 2))

(cffi:defcenum curl-shoption
  (:none 0) (:share 1) (:unshare 2) (:lockfunc 3) (:unlockfunc 4) (:userdata 5))

(cffi:defcenum curl-upart
  (:url 0) (:scheme 1) (:user 2) (:password 3) (:options 4) (:host 5)
  (:port 6) (:path 7) (:query 8) (:fragment 9) (:zoneid 10))

(cffi:defcenum curl-msg-type
  (:none 0) (:done 1))

(cffi:defcenum curlsts-code
  (:ok 0) (:done 1) (:fail 2))

(cffi:defcenum curl-khtype
  (:unknown 0) (:rsa1 1) (:rsa 2) (:dss 3) (:ecdsa 4) (:ed25519 5))

(cffi:defcenum curl-khmatch
  (:ok 0) (:mismatch 1) (:missing 2))

(cffi:defcenum curl-khstat
  (:fine-add-to-file 0) (:fine 1) (:reject 2) (:defer 3) (:fine-replace 4))

;;; Structures ----------------------------------------------------------------

(cffi:defcstruct curl-slist
  (data :pointer)
  (next :pointer))

;;; CURL_BLOB_NOCOPY means libcurl keeps the pointer, so the memory has to
;;; outlive the transfer; the binding always allocates blob data it owns.
(defconstant +curl-blob-nocopy+ 0)
(defconstant +curl-blob-copy+ 1)

(cffi:defcstruct curl-blob
  (data :pointer)
  (len :size)
  (flags :unsigned-int))

(cffi:defcstruct curl-waitfd
  (fd curl-socket-t)
  (events :short)
  (revents :short))

(defconstant +curl-wait-pollin+  #x0001)
(defconstant +curl-wait-pollpri+ #x0002)
(defconstant +curl-wait-pollout+ #x0004)

;;; CURLMsg's payload is a union of `void *whatever' and `CURLcode result'.
;;; CFFI lays the union out itself, so RESULT lands at the right offset without
;;; hard-coding 16 -- but a test pins the size and offset anyway, because
;;; reading `result' from the wrong place yields a plausible-looking wrong
;;; CURLcode rather than an obvious crash.
(cffi:defcunion curl-msg-data
  (whatever :pointer)
  (result :int))

(cffi:defcstruct curl-msg
  (msg :int)
  (easy-handle :pointer)
  (data (:union curl-msg-data)))

(cffi:defcstruct curl-header
  (name :pointer)
  (value :pointer)
  (amount :size)
  (index :size)
  (origin :unsigned-int)
  (anchor :pointer))

(defconstant +curlh-header+  (ash 1 0))
(defconstant +curlh-trailer+ (ash 1 1))
(defconstant +curlh-connect+ (ash 1 2))
(defconstant +curlh-1xx+     (ash 1 3))
(defconstant +curlh-pseudo+  (ash 1 4))

;;; includeSubDomains is a C bitfield (`unsigned int x:1'), which CFFI cannot
;;; express at all.  It is declared here as the whole containing word and bit 0
;;; is masked by hand; see the HSTS accessors.
(cffi:defcstruct curl-hstsentry
  (name :pointer)
  (namelen :size)
  (flags :unsigned-int)
  (expire :char :count 18))

(cffi:defcstruct curl-index
  (index :size)
  (total :size))

(cffi:defcstruct curl-khkey
  (key :pointer)
  (len :size)
  (keytype curl-khtype))

(cffi:defcstruct curl-certinfo
  (num-of-certs :int)
  (certinfo :pointer))

(cffi:defcstruct curl-tlssessioninfo
  (backend :int)
  (internals :pointer))

;;; struct curl_sockaddr ends with an embedded `struct sockaddr' by value.  Its
;;; first two bytes differ across platforms -- Darwin and the BSDs have an
;;; sa_len byte ahead of sa_family, Linux does not -- but the struct is 16
;;; bytes either way and the binding only ever hands it back to the caller as
;;; opaque bytes, so it is declared as such rather than wrongly on one of them.
(cffi:defcstruct curl-sockaddr
  (family :int)
  (socktype :int)
  (protocol :int)
  (addrlen :unsigned-int)
  (addr :uint8 :count 16))

(cffi:defcstruct curl-ws-frame
  (age :int)
  (flags :int)
  (offset curl-off-t)
  (bytesleft curl-off-t)
  (len :size))

(defconstant +curlws-text+       (ash 1 0))
(defconstant +curlws-binary+     (ash 1 1))
(defconstant +curlws-cont+       (ash 1 2))
(defconstant +curlws-close+      (ash 1 3))
(defconstant +curlws-ping+       (ash 1 4))
(defconstant +curlws-offset+     (ash 1 5))
(defconstant +curlws-pong+       (ash 1 6))
(defconstant +curlws-raw-mode+   (ash 1 0))
(defconstant +curlws-noautopong+ (ash 1 1))

;;; Callback return sentinels -------------------------------------------------
;;;
;;; These are 32-bit magic numbers returned from functions declared to return
;;; size_t, which is 64 bits here.  They must be returned exactly: widening
;;; CURL_WRITEFUNC_ERROR to a 64-bit all-ones value does not abort the transfer,
;;; it tells libcurl an absurd number of bytes were consumed.
(defconstant +curl-writefunc-pause+      #x10000001)
(defconstant +curl-writefunc-error+      #xFFFFFFFF)
(defconstant +curl-readfunc-abort+       #x10000000)
(defconstant +curl-readfunc-pause+       #x10000001)
(defconstant +curl-progressfunc-continue+ #x10000001)

(defconstant +curl-seekfunc-ok+       0)
(defconstant +curl-seekfunc-fail+     1)
(defconstant +curl-seekfunc-cantseek+ 2)

(defconstant +curl-sockopt-ok+                0)
(defconstant +curl-sockopt-error+             1)
(defconstant +curl-sockopt-already-connected+ 2)

(defconstant +curl-chunk-bgn-func-ok+   0)
(defconstant +curl-chunk-bgn-func-fail+ 1)
(defconstant +curl-chunk-bgn-func-skip+ 2)
(defconstant +curl-chunk-end-func-ok+   0)
(defconstant +curl-chunk-end-func-fail+ 1)

(defconstant +curl-fnmatchfunc-match+   0)
(defconstant +curl-fnmatchfunc-nomatch+ 1)
(defconstant +curl-fnmatchfunc-fail+    2)

(defconstant +curl-prereqfunc-ok+    0)
(defconstant +curl-prereqfunc-abort+ 1)
(defconstant +curl-trailerfunc-ok+    0)
(defconstant +curl-trailerfunc-abort+ 1)

(defconstant +curl-push-ok+       0)
(defconstant +curl-push-deny+     1)
(defconstant +curl-push-errorout+ 2)

(defconstant +curl-poll-none+   0)
(defconstant +curl-poll-in+     1)
(defconstant +curl-poll-out+    2)
(defconstant +curl-poll-inout+  3)
(defconstant +curl-poll-remove+ 4)

(defconstant +curl-cselect-in+  #x01)
(defconstant +curl-cselect-out+ #x02)
(defconstant +curl-cselect-err+ #x04)

(defconstant +curlpause-recv+ (ash 1 0))
(defconstant +curlpause-send+ (ash 1 2))
(defconstant +curlpause-all+  (logior +curlpause-recv+ +curlpause-send+))
(defconstant +curlpause-cont+ 0)
