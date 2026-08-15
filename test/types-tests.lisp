;;;; test/types-tests.lisp — hand-written foreign layouts, against the real ones.
;;;;
;;;; The layouts in src/types.lisp are written by hand rather than groveled, so
;;;; nothing but a test stands between a mistyped slot and silently reading the
;;;; wrong bytes.  Sizes are pinned against values measured from the installed
;;;; curl headers with a C compiler; a mismatch means the Lisp declaration and
;;;; the C struct have diverged.

(in-package #:libcurl/test)

(in-suite types)

(defmacro is-size (expected type)
  `(is (= ,expected (cffi:foreign-type-size ',type))
       "~S should be ~D bytes, got ~D"
       ',type ,expected (cffi:foreign-type-size ',type)))

(test scalar-widths-are-64-bit-where-libcurl-expects-them
  ;; curl_off_t is 64-bit on every platform we support.  A 32-bit declaration
  ;; would half-write every CURLOPT_*_LARGE value passed variadically.
  (is (= 8 (cffi:foreign-type-size 'libcurl::curl-off-t)))
  (is (= 4 (cffi:foreign-type-size 'libcurl::curl-socket-t)))
  ;; C `long' is what CURLINFO_LONG out-parameters are, and it is 8 bytes on
  ;; LP64.  Allocating 4 for one corrupts the neighbouring word.
  (is (= 8 (cffi:foreign-type-size :long))))

(test struct-sizes-match-the-c-layouts
  (is-size 16 (:struct libcurl::curl-slist))
  (is-size 24 (:struct libcurl::curl-blob))
  (is-size 8  (:struct libcurl::curl-waitfd))
  (is-size 24 (:struct libcurl::curl-msg))
  (is-size 48 (:struct libcurl::curl-header))
  (is-size 40 (:struct libcurl::curl-hstsentry))
  (is-size 32 (:struct libcurl::curl-ws-frame))
  (is-size 216 (:struct libcurl::curl-version-info-data)))

(test curl-msg-union-lands-at-the-right-offset
  ;; CURLMsg's payload is a union of `void *whatever' and `CURLcode result'.
  ;; Reading `result' from the wrong offset yields a plausible but wrong
  ;; CURLcode rather than crashing, which is exactly the kind of bug that
  ;; survives casual testing -- so pin the offset.
  (is (= 0 (cffi:foreign-slot-offset '(:struct libcurl::curl-msg) 'libcurl::msg)))
  (is (= 8 (cffi:foreign-slot-offset '(:struct libcurl::curl-msg)
                                     'libcurl::easy-handle)))
  (is (= 16 (cffi:foreign-slot-offset '(:struct libcurl::curl-msg)
                                      'libcurl::data))))

(test curl-header-anchor-is-past-the-padding
  ;; `origin' is an unsigned int followed by a pointer, so there are four bytes
  ;; of padding between them; forgetting it puts `anchor' four bytes early.
  (is (= 32 (cffi:foreign-slot-offset '(:struct libcurl::curl-header)
                                      'libcurl::origin)))
  (is (= 40 (cffi:foreign-slot-offset '(:struct libcurl::curl-header)
                                      'libcurl::anchor))))

(test curlcode-decoding-is-tolerant
  ;; A newer libcurl can return a code this binding predates.  That has to
  ;; surface as an unfamiliar code, not as an error from the decoder.
  (is (eq :ok (curlcode-keyword 0)))
  (is (eq :operation-timedout (curlcode-keyword 28)))
  (is (eq :aborted-by-callback (curlcode-keyword 42)))
  (is (eq :write-error (curlcode-keyword 23)))
  ;; Unknown: returned unchanged rather than signalled.
  (is (= 31337 (curlcode-keyword 31337)))
  (is (= 0 (curlcode-value :ok)))
  (is (= 28 (curlcode-value :operation-timedout)))
  (is (= 99 (curlcode-value 99))))

(test multi-code-zero-is-not-the-first-enumerator
  ;; CURLM_CALL_MULTI_PERFORM is -1, so a defcenum numbering implicitly from
  ;; zero would shift every multi code by one.
  (is (eq :call-multi-perform (curlcode-keyword -1 'libcurl::curlmcode)))
  (is (eq :ok (curlcode-keyword 0 'libcurl::curlmcode)))
  (is (eq :bad-handle (curlcode-keyword 1 'libcurl::curlmcode))))

(test header-code-has-no-sentinel
  ;; CURLHcode is the one family with no trailing _LAST, so 7 is a real code.
  (is (eq :not-built-in (curlcode-keyword 7 'libcurl::curlhcode)))
  (is (= 8 (length libcurl::*header-code-messages*)))
  (dolist (entry libcurl::*header-code-messages*)
    (is (keywordp (car entry)))
    (is (stringp (cdr entry)))))

(test callback-sentinels-are-exactly-32-bit
  ;; These are magic constants returned from size_t-valued callbacks.  Widening
  ;; CURL_WRITEFUNC_ERROR to 64 bits does not abort a transfer, it claims an
  ;; absurd byte count was consumed -- so the value must be exactly 0xFFFFFFFF.
  (is (= #xFFFFFFFF libcurl::+curl-writefunc-error+))
  (is (= #x10000001 libcurl::+curl-writefunc-pause+))
  (is (= #x10000000 libcurl::+curl-readfunc-abort+))
  (is (= #x10000001 libcurl::+curl-readfunc-pause+))
  (is (< libcurl::+curl-writefunc-error+ (expt 2 32))))
