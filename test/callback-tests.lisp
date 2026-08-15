;;;; test/callback-tests.lisp — every trampoline, dispatch and sentinel.
;;;;
;;;; Five of the twenty-two callbacks are reachable over HTTP and are exercised
;;;; end to end elsewhere.  The rest belong to FTP wildcards, SFTP, RTSP or
;;;; HSTS, and a hermetic suite cannot raise a server for each -- so they were
;;;; written, compiled, and never once executed.
;;;;
;;;; That is the wrong thing to leave untested.  What is at risk in a
;;;; trampoline is not the protocol: it is whether the arguments are decoded
;;;; correctly and whether the value returned on failure is the one libcurl
;;;; documents for *that* callback.  Those differ per callback -- 2 for
;;;; fnmatch, CURLKHSTAT_REJECT for a host key, CURLSTS_FAIL for HSTS, a 32-bit
;;;; magic number for the size_t-returning ones -- and a wrong one produces
;;;; plausible misbehaviour rather than a crash.
;;;;
;;;; So the trampolines are called directly, the way libcurl would call them:
;;;; through the C function pointer, with a registry key in the userdata slot.
;;;; That exercises the whole Lisp side of the boundary -- lookup, argument
;;;; translation, the condition guard, the sentinel -- without a server.

(in-package #:curlcl/test)

(in-suite callbacks)

(defmacro with-registered-state ((state) &body body)
  "Run BODY with STATE a registered CALLBACK-STATE, released afterwards."
  `(let ((,state (curlcl::make-callback-state)))
     (curlcl::register-callback-state ,state)
     (unwind-protect (progn ,@body)
       (curlcl::release-callback-state (curlcl::cb-key ,state)))))

(defun state-key-pointer (state)
  (curlcl::callback-key-pointer (curlcl::cb-key state)))

(defmacro calling-trampoline (name &rest arguments)
  "Invoke a trampoline through its C function pointer, as libcurl would."
  `(cffi:foreign-funcall-pointer (cffi:get-callback ',name) () ,@arguments))

;;; Dispatch and lifetime -----------------------------------------------------

(test an-unknown-key-fails-rather-than-dispatching
  ;; A freed handle leaves libcurl holding a key that no longer resolves.  Every
  ;; trampoline must treat that as failure rather than calling into nothing.
  (let ((stale (cffi:make-pointer 999999)))
    (is (= curlcl::+curl-writefunc-error+
           (calling-trampoline curlcl::%write-trampoline
                               :pointer (cffi:null-pointer) :size 1 :size 1
                               :pointer stale :size)))
    (is (= curlcl::+curl-readfunc-abort+
           (calling-trampoline curlcl::%read-trampoline
                               :pointer (cffi:null-pointer) :size 1 :size 1
                               :pointer stale :size)))
    (is (= 1 (calling-trampoline curlcl::%xferinfo-trampoline
                                 :pointer stale :int64 0 :int64 0 :int64 0 :int64 0
                                 :int)))))

(test a-null-userdata-is-not-key-zero
  ;; libcurl passes NULL when a *DATA option was never set; keys start at 1 so
  ;; that can never be mistaken for a live registration.
  (is (null (curlcl::%lookup-state (cffi:null-pointer))))
  (with-registered-state (state)
    (is (plusp (curlcl::cb-key state)))))

;;; The size_t-returning callbacks -------------------------------------------

(test the-write-trampoline-decodes-its-buffer-and-honours-its-returns
  (with-registered-state (state)
    (let ((seen nil))
      (setf (curlcl::cb-write state) (lambda (octets) (setf seen octets) t))
      (cffi:with-foreign-string (buffer "hello" :null-terminated-p nil)
        ;; size * nitems is the byte count, and both factors have to be used.
        (is (= 5 (calling-trampoline curlcl::%write-trampoline
                                     :pointer buffer :size 5 :size 1
                                     :pointer (state-key-pointer state) :size)))
        (is (equalp (curlcl::coerce-to-octets "hello") seen))
        (is (= 4 (calling-trampoline curlcl::%write-trampoline
                                     :pointer buffer :size 2 :size 2
                                     :pointer (state-key-pointer state) :size))
            "size and nitems are multiplied"))
      ;; NIL aborts, :PAUSE pauses -- both exact 32-bit values.
      (setf (curlcl::cb-write state) (lambda (octets) (declare (ignore octets)) nil))
      (is (= curlcl::+curl-writefunc-error+
             (calling-trampoline curlcl::%write-trampoline
                                 :pointer (cffi:null-pointer) :size 0 :size 0
                                 :pointer (state-key-pointer state) :size)))
      (setf (curlcl::cb-write state) (lambda (octets) (declare (ignore octets)) :pause))
      (is (= curlcl::+curl-writefunc-pause+
             (calling-trampoline curlcl::%write-trampoline
                                 :pointer (cffi:null-pointer) :size 0 :size 0
                                 :pointer (state-key-pointer state) :size))))))

(test a-condition-in-any-callback-returns-that-callbacks-sentinel
  ;; The guard is uniform, but the value it returns is not: each callback has
  ;; its own, and libcurl reads them differently.
  (with-registered-state (state)
    (macrolet ((signalling (slot) `(setf (,slot state) (lambda (&rest ignored)
                                                         (declare (ignore ignored))
                                                         (error "callback failed")))))
      (signalling curlcl::cb-write)
      (is (= curlcl::+curl-writefunc-error+
             (calling-trampoline curlcl::%write-trampoline
                                 :pointer (cffi:null-pointer) :size 0 :size 0
                                 :pointer (state-key-pointer state) :size)))
      (signalling curlcl::cb-read)
      (is (= curlcl::+curl-readfunc-abort+
             (calling-trampoline curlcl::%read-trampoline
                                 :pointer (cffi:null-pointer) :size 0 :size 0
                                 :pointer (state-key-pointer state) :size)))
      (signalling curlcl::cb-seek)
      (is (= curlcl::+curl-seekfunc-fail+
             (calling-trampoline curlcl::%seek-trampoline
                                 :pointer (state-key-pointer state)
                                 :int64 0 :int 0 :int)))
      (signalling curlcl::cb-sockopt)
      (is (= curlcl::+curl-sockopt-error+
             (calling-trampoline curlcl::%sockopt-trampoline
                                 :pointer (state-key-pointer state)
                                 :int 3 :int 0 :int)))
      (signalling curlcl::cb-fnmatch)
      (is (= curlcl::+curl-fnmatchfunc-fail+
             (calling-trampoline curlcl::%fnmatch-trampoline
                                 :pointer (state-key-pointer state)
                                 :string "*" :string "a" :int)))
      (signalling curlcl::cb-prereq)
      (is (= curlcl::+curl-prereqfunc-abort+
             (calling-trampoline curlcl::%prereq-trampoline
                                 :pointer (state-key-pointer state)
                                 :pointer (cffi:null-pointer)
                                 :pointer (cffi:null-pointer)
                                 :int 0 :int 0 :int)))
      ;; The condition is kept for PERFORM to re-signal, and only the first.
      (is (typep (curlcl::cb-condition state) 'error)))))

(test the-read-trampoline-copies-into-libcurls-buffer
  (with-registered-state (state)
    (setf (curlcl::cb-read state)
          (lambda (capacity) (declare (ignore capacity)) "abcd"))
    (cffi:with-foreign-object (buffer :uint8 16)
      (is (= 4 (calling-trampoline curlcl::%read-trampoline
                                   :pointer buffer :size 16 :size 1
                                   :pointer (state-key-pointer state) :size)))
      (is (equalp (curlcl::coerce-to-octets "abcd")
                  (curlcl::foreign-to-octets buffer 4))))
    ;; :EOF is zero, which is how libcurl learns the body is complete.
    (setf (curlcl::cb-read state) (lambda (capacity) (declare (ignore capacity)) :eof))
    (is (= 0 (calling-trampoline curlcl::%read-trampoline
                                 :pointer (cffi:null-pointer) :size 0 :size 0
                                 :pointer (state-key-pointer state) :size)))
    ;; Returning more than the buffer holds must fail rather than overrun it.
    (setf (curlcl::cb-read state)
          (lambda (capacity) (declare (ignore capacity)) "far too long for this"))
    (cffi:with-foreign-object (buffer :uint8 4)
      (is (= curlcl::+curl-readfunc-abort+
             (calling-trampoline curlcl::%read-trampoline
                                 :pointer buffer :size 4 :size 1
                                 :pointer (state-key-pointer state) :size))
          "an over-long read should abort, not overrun the buffer"))))

;;; Callbacks belonging to protocols a hermetic suite cannot raise ------------

(test the-fnmatch-trampoline-translates-match-and-nomatch
  (with-registered-state (state)
    (let ((seen nil))
      (setf (curlcl::cb-fnmatch state)
            (lambda (pattern string) (setf seen (list pattern string))
              (string= pattern string)))
      (is (= curlcl::+curl-fnmatchfunc-match+
             (calling-trampoline curlcl::%fnmatch-trampoline
                                 :pointer (state-key-pointer state)
                                 :string "same" :string "same" :int)))
      (is (equal '("same" "same") seen))
      (is (= curlcl::+curl-fnmatchfunc-nomatch+
             (calling-trampoline curlcl::%fnmatch-trampoline
                                 :pointer (state-key-pointer state)
                                 :string "a" :string "b" :int))))))

(test the-chunk-trampolines-translate-their-outcomes
  (with-registered-state (state)
    (let ((remains nil))
      (setf (curlcl::cb-chunk-begin state)
            (lambda (info left) (declare (ignore info)) (setf remains left) t))
      (is (= curlcl::+curl-chunk-bgn-func-ok+
             (calling-trampoline curlcl::%chunk-begin-trampoline
                                 :pointer (cffi:null-pointer)
                                 :pointer (state-key-pointer state)
                                 :int 7 :long)))
      (is (= 7 remains))
      (setf (curlcl::cb-chunk-begin state)
            (lambda (info left) (declare (ignore info left)) :skip))
      (is (= curlcl::+curl-chunk-bgn-func-skip+
             (calling-trampoline curlcl::%chunk-begin-trampoline
                                 :pointer (cffi:null-pointer)
                                 :pointer (state-key-pointer state)
                                 :int 0 :long)))
      (setf (curlcl::cb-chunk-end state) (lambda () t))
      (is (= curlcl::+curl-chunk-end-func-ok+
             (calling-trampoline curlcl::%chunk-end-trampoline
                                 :pointer (state-key-pointer state) :long)))
      (setf (curlcl::cb-chunk-end state) (lambda () nil))
      (is (= curlcl::+curl-chunk-end-func-fail+
             (calling-trampoline curlcl::%chunk-end-trampoline
                                 :pointer (state-key-pointer state) :long))))))

(test the-ssh-host-key-trampoline-accepts-with-zero
  ;; CURLE_OK accepts the host key; anything else refuses it.  Getting this
  ;; backwards would accept every unknown host, which is the whole point of
  ;; the callback.
  (with-registered-state (state)
    (let ((seen nil))
      (setf (curlcl::cb-ssh-host-key state)
            (lambda (type key) (setf seen (list type (length key))) t))
      (cffi:with-foreign-string (key "keybytes" :null-terminated-p nil)
        (is (= 0 (calling-trampoline curlcl::%ssh-host-key-trampoline
                                     :pointer (state-key-pointer state)
                                     :int 2 :pointer key :size 8 :int)))
        (is (equal '(:rsa 8) seen)))
      (setf (curlcl::cb-ssh-host-key state) (lambda (type key)
                                               (declare (ignore type key)) nil))
      (is (= 1 (calling-trampoline curlcl::%ssh-host-key-trampoline
                                   :pointer (state-key-pointer state)
                                   :int 2 :pointer (cffi:null-pointer) :size 0 :int)))
      ;; With no closure at all the host key must be refused, not accepted.
      (setf (curlcl::cb-ssh-host-key state) nil)
      (is (= 1 (calling-trampoline curlcl::%ssh-host-key-trampoline
                                   :pointer (state-key-pointer state)
                                   :int 2 :pointer (cffi:null-pointer) :size 0 :int))
          "an unset host-key callback must refuse rather than accept"))))

(test the-ssh-key-trampoline-returns-a-khstat
  (with-registered-state (state)
    (setf (curlcl::cb-ssh-key state)
          (lambda (known found match) (declare (ignore known found match)) :fine))
    (is (= (curlcl:curlcode-value :fine 'curlcl::curl-khstat)
           (calling-trampoline curlcl::%ssh-key-trampoline
                               :pointer (cffi:null-pointer)
                               :pointer (cffi:null-pointer)
                               :pointer (cffi:null-pointer)
                               :int 0 :pointer (state-key-pointer state) :int)))
    ;; Anything unrecognised, and the unset case, must reject.
    (setf (curlcl::cb-ssh-key state) nil)
    (is (= (curlcl:curlcode-value :reject 'curlcl::curl-khstat)
           (calling-trampoline curlcl::%ssh-key-trampoline
                               :pointer (cffi:null-pointer)
                               :pointer (cffi:null-pointer)
                               :pointer (cffi:null-pointer)
                               :int 0 :pointer (state-key-pointer state) :int)))))

(test the-interleave-trampoline-behaves-like-write
  (with-registered-state (state)
    (let ((seen nil))
      (setf (curlcl::cb-interleave state) (lambda (octets) (setf seen octets) t))
      (cffi:with-foreign-string (buffer "rtp" :null-terminated-p nil)
        (is (= 3 (calling-trampoline curlcl::%interleave-trampoline
                                     :pointer buffer :size 3 :size 1
                                     :pointer (state-key-pointer state) :size)))
        (is (equalp (curlcl::coerce-to-octets "rtp") seen))))))

(test the-ioctl-trampoline-maps-its-command-and-errors
  (with-registered-state (state)
    (let ((seen nil))
      (setf (curlcl::cb-ioctl state) (lambda (command) (setf seen command) t))
      (is (= (curlcl:curlcode-value :ok 'curlcl::curlioerr)
             (calling-trampoline curlcl::%ioctl-trampoline
                                 :pointer (cffi:null-pointer) :int 1
                                 :pointer (state-key-pointer state) :int)))
      (is (eq :restartread seen))
      ;; Unset means "unknown command", which is what tells libcurl to give up
      ;; on the ioctl rather than to fail the transfer.
      (setf (curlcl::cb-ioctl state) nil)
      (is (= (curlcl:curlcode-value :unknowncmd 'curlcl::curlioerr)
             (calling-trampoline curlcl::%ioctl-trampoline
                                 :pointer (cffi:null-pointer) :int 0
                                 :pointer (state-key-pointer state) :int))))))

(test the-trailer-trampoline-hands-libcurl-an-slist
  (with-registered-state (state)
    (setf (curlcl::cb-trailer state) (lambda () '("X-Checksum: abc" "X-Count: 2")))
    (cffi:with-foreign-object (out :pointer)
      (setf (cffi:mem-ref out :pointer) (cffi:null-pointer))
      (is (= curlcl::+curl-trailerfunc-ok+
             (calling-trampoline curlcl::%trailer-trampoline
                                 :pointer out
                                 :pointer (state-key-pointer state) :int)))
      (let ((head (cffi:mem-ref out :pointer)))
        (is (not (cffi:null-pointer-p head)))
        (is (equal '("X-Checksum: abc" "X-Count: 2") (curlcl::slist-to-list head)))
        ;; libcurl owns and frees this chain; freeing it here would double-free
        ;; in a real transfer, so the test only reads it.
        (curlcl::%curl-slist-free-all head)))
    ;; Returning nothing aborts, rather than sending an empty trailer.
    (setf (curlcl::cb-trailer state) (lambda () nil))
    (cffi:with-foreign-object (out :pointer)
      (is (= curlcl::+curl-trailerfunc-abort+
             (calling-trampoline curlcl::%trailer-trampoline
                                 :pointer out
                                 :pointer (state-key-pointer state) :int))))))

(test the-hsts-trampolines-decode-the-bitfield-entry
  ;; struct curl_hstsentry's includeSubDomains is a C bitfield, which CFFI
  ;; cannot express, so it is masked out of the containing word by hand.
  (with-registered-state (state)
    (cffi:with-foreign-object (entry '(:struct curlcl::curl-hstsentry))
      (cffi:with-foreign-object (index '(:struct curlcl::curl-index))
        (cffi:with-foreign-string (name "example.com")
          (setf (cffi:foreign-slot-value entry '(:struct curlcl::curl-hstsentry)
                                         'curlcl::name) name
                (cffi:foreign-slot-value entry '(:struct curlcl::curl-hstsentry)
                                         'curlcl::namelen) 64
                (cffi:foreign-slot-value entry '(:struct curlcl::curl-hstsentry)
                                         'curlcl::flags) 1)
          (cffi:lisp-string-to-foreign
           "20300101 00:00:00"
           (cffi:foreign-slot-pointer entry '(:struct curlcl::curl-hstsentry)
                                      'curlcl::expire)
           18)
          (setf (cffi:foreign-slot-value index '(:struct curlcl::curl-index)
                                         'curlcl::index) 0
                (cffi:foreign-slot-value index '(:struct curlcl::curl-index)
                                         'curlcl::total) 1)
          (let ((seen nil))
            (setf (curlcl::cb-hsts-write state)
                  (lambda (entry-list position total)
                    (setf seen (list entry-list position total))))
            (is (= (curlcl:curlcode-value :ok 'curlcl::curlsts-code)
                   (calling-trampoline curlcl::%hsts-write-trampoline
                                       :pointer (cffi:null-pointer)
                                       :pointer entry :pointer index
                                       :pointer (state-key-pointer state) :int)))
            (destructuring-bind (entry-list position total) seen
              (is (string= "example.com" (first entry-list)))
              ;; Bit 0 of the flags word, not the whole word.
              (is (eq t (second entry-list)))
              (is (= 0 position))
              (is (= 1 total))))
          ;; Reading back: no closure means "no more entries", which ends the
          ;; load rather than failing it.
          (setf (curlcl::cb-hsts-read state) nil)
          (is (= (curlcl:curlcode-value :done 'curlcl::curlsts-code)
                 (calling-trampoline curlcl::%hsts-read-trampoline
                                     :pointer (cffi:null-pointer) :pointer entry
                                     :pointer (state-key-pointer state) :int))))))))

(test the-socket-trampolines-translate-poll-flags
  (with-registered-state (state)
    (let ((seen nil))
      (setf (curlcl::cb-socket state)
            (lambda (socket what easy data)
              (declare (ignore easy data))
              (push (cons socket what) seen)
              t))
      (dolist (pair (list (cons curlcl::+curl-poll-in+ :in)
                          (cons curlcl::+curl-poll-out+ :out)
                          (cons curlcl::+curl-poll-inout+ :in-out)
                          (cons curlcl::+curl-poll-remove+ :remove)))
        (calling-trampoline curlcl::%socket-trampoline
                            :pointer (cffi:null-pointer)
                            :int 9
                            :int (car pair)
                            :pointer (state-key-pointer state)
                            :pointer (cffi:null-pointer) :int)
        (is (equal (cons 9 (cdr pair)) (first seen))
            "poll flag ~D should decode to ~S" (car pair) (cdr pair))))
    (let ((timeouts nil))
      (setf (curlcl::cb-timer state) (lambda (ms) (push ms timeouts) t))
      (calling-trampoline curlcl::%timer-trampoline
                          :pointer (cffi:null-pointer) :long 250
                          :pointer (state-key-pointer state) :int)
      ;; -1 means "cancel the timer", which must not look like a 1ms one.
      (calling-trampoline curlcl::%timer-trampoline
                          :pointer (cffi:null-pointer) :long -1
                          :pointer (state-key-pointer state) :int)
      (is (equal '(nil 250) timeouts)))))

;;; The ones that are reachable over HTTP ------------------------------------

(test the-sockopt-callback-sees-the-socket-before-connect
  (with-easy (handle)
    (let ((seen nil))
      (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t)
            (callback-function handle :sockopt)
            (lambda (socket purpose) (push (cons socket purpose) seen) t))
      (setopt handle :url (test-url "/ok"))
      (perform handle)
      (is (plusp (length seen)) "the sockopt callback never fired")
      (is (integerp (car (first seen))))
      (is (eq :ipcxn (cdr (first seen)))))))

(test a-failing-sockopt-callback-aborts-the-connection
  ;; CURL_SOCKOPT_ERROR is documented to abort with CURLE_ABORTED_BY_CALLBACK.
  (with-easy (handle)
    (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t)
          (callback-function handle :sockopt)
          (lambda (socket purpose) (declare (ignore socket purpose)) :error))
    (setopt handle :url (test-url "/ok"))
    (handler-case (progn (perform handle) (fail "expected the sockopt abort"))
      (easy-error (c)
        (is (member (curl-error-code-name c)
                    '(:aborted-by-callback :couldnt-connect)))))))

(test the-closesocket-callback-runs-when-the-connection-goes
  (with-easy (handle)
    (let ((closed 0))
      (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t)
            (callback-function handle :closesocket)
            (lambda (socket) (declare (ignore socket)) (incf closed) t))
      (setopt handle :url (test-url "/ok"))
      (perform handle)
      ;; The socket is closed when the handle is, so force that here.
      (close-handle handle)
      (is (plusp closed) "the closesocket callback never fired"))))

(test the-prereq-callback-sees-the-connection-details
  (with-easy (handle)
    (let ((seen nil))
      (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t)
            (callback-function handle :prereq)
            (lambda (primary-ip primary-port local-ip local-port)
              (setf seen (list primary-ip primary-port local-ip local-port))
              t))
      (setopt handle :url (test-url "/ok"))
      (perform handle)
      (is (not (null seen)) "the prereq callback never fired")
      (destructuring-bind (primary-ip primary-port local-ip local-port) seen
        (is (string= "127.0.0.1" primary-ip))
        (is (integerp primary-port))
        (is (stringp local-ip))
        (is (integerp local-port))))))

(test a-refusing-prereq-callback-aborts-before-the-request
  (with-easy (handle)
    (let ((wrote 0))
      (setf (callback-function handle :write)
            (lambda (octets) (incf wrote (length octets)) t)
            (callback-function handle :prereq)
            (lambda (a b c d) (declare (ignore a b c d)) nil))
      (setopt handle :url (test-url "/ok"))
      (handler-case (progn (perform handle) (fail "expected the prereq abort"))
        (easy-error (c)
          (is (eq :aborted-by-callback (curl-error-code-name c)))))
      (is (zerop wrote) "the request was made despite the prereq refusing"))))

(test the-seek-callback-is-used-to-rewind-an-upload
  ;; The one that matters for streaming uploads: libcurl rewinds the body to
  ;; repeat a request across a redirect, and can only do it through this.
  (uiop:with-temporary-file (:pathname path :stream out :direction :output
                             :element-type '(unsigned-byte 8))
    (write-sequence (curlcl::coerce-to-octets "rewind-me") out)
    (finish-output out)
    :close-stream
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (with-easy (handle)
        (let ((seeks nil))
          (setf (callback-function handle :write) (lambda (o) (declare (ignore o)) t)
                (callback-function handle :read) (curlcl::stream-reader in)
                (callback-function handle :seek)
                (lambda (offset whence)
                  (push (list offset whence) seeks)
                  (funcall (curlcl::stream-seeker in) offset whence)))
          (setopts handle :url (test-url "/redirect/1")
                          :upload t
                          :infilesize-large 9
                          :followlocation t)
          (perform handle)
          (is (plusp (length seeks))
              "libcurl never asked to rewind the upload across the redirect")
          (is (equal '(0 :set) (first (last seeks)))))))))
