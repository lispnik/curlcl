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

(defun windows-p ()
  (uiop:os-windows-p))

(test scalar-widths-match-the-platform-abi
  ;; curl_off_t is 64-bit on every platform we support.  A 32-bit declaration
  ;; would half-write every CURLOPT_*_LARGE value passed variadically.
  (is (= 8 (cffi:foreign-type-size 'libcurl::curl-off-t)))
  ;; curl_socket_t is an fd on Unix and a Win32 SOCKET -- UINT_PTR -- on
  ;; Windows.  This started as a flat 4, which agreed with the equally flat
  ;; declaration in src/types.lisp and so proved nothing; both were wrong on
  ;; Win64, where CURLINFO_ACTIVESOCKET would have written 8 bytes into 4.
  (is (= (if (windows-p) (cffi:foreign-type-size :uintptr) 4)
         (cffi:foreign-type-size 'libcurl::curl-socket-t)))
  ;; C `long' is 8 bytes on LP64 Unix and 4 on LLP64 Windows.
  (is (= (if (windows-p) 4 8) (cffi:foreign-type-size :long))))

(test the-long-width-is-the-one-libcurl-actually-writes
  ;; The constants above say what the ABI is supposed to be; this asks libcurl.
  ;; A CURLINFO_LONG out-parameter is written as a C `long', so poisoning a
  ;; generous buffer and seeing exactly how much of it changes measures
  ;; sizeof(long) in the loaded library rather than in our assumptions.  If
  ;; CFFI's :LONG and libcurl's `long' ever disagree -- which is what an LP64
  ;; assumption on an LLP64 platform amounts to -- this fails whichever way the
  ;; mismatch goes: too narrow and the guard bytes get clobbered, too wide and
  ;; the tail stays poisoned.
  (let* ((width (cffi:foreign-type-size :long))
         (total (* 4 width))
         (poison #xAA))
    (libcurl::with-raw-easy (h)
      (cffi:with-foreign-object (buffer :uint8 total)
        (dotimes (i total) (setf (cffi:mem-aref buffer :uint8 i) poison))
        ;; CURLINFO_RESPONSE_CODE on a handle that has not performed is a
        ;; defined 0, so the written region is unambiguous.
        (is (eq :ok (curlcode-keyword (libcurl::%getinfo h #x200002 buffer))))
        (is (every (lambda (i) (zerop (cffi:mem-aref buffer :uint8 i)))
                   (alexandria:iota width))
            "libcurl wrote fewer than ~D bytes for a CURLINFO_LONG" width)
        (is (every (lambda (i) (= poison (cffi:mem-aref buffer :uint8 i)))
                   (alexandria:iota (- total width) :start width))
            "libcurl wrote more than ~D bytes for a CURLINFO_LONG -- CFFI's ~
             :LONG is narrower than this libcurl's `long'" width)))))

(test struct-sizes-match-the-c-layouts
  (is-size 16 (:struct libcurl::curl-slist))
  (is-size 24 (:struct libcurl::curl-blob))
  ;; struct curl_waitfd leads with a curl_socket_t, so it is 8 bytes on Unix
  ;; and 16 on Win64 -- 8-byte SOCKET, two shorts, then padding back to the
  ;; 8-byte alignment.  curl_multi_wait reads these as an array, so a wrong
  ;; size means every element after the first is read from the wrong offset.
  (is (= (if (windows-p) 16 8)
         (cffi:foreign-type-size '(:struct libcurl::curl-waitfd))))
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
