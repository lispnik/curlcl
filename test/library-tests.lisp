;;;; test/library-tests.lisp — the library loaded, and what it says about itself.
;;;;
;;;; These are deliberately about the *loaded* library rather than about
;;;; constants baked into the binding.  macOS can supply either the system
;;;; libcurl from the dyld shared cache or a much newer Homebrew build, and the
;;;; two differ in options, protocols and whether websockets exist -- so the
;;;; binding has to interrogate rather than assume, and so do the tests.

(in-package #:libcurl/test)

(in-suite library)

(test library-is-loaded
  ;; Loading the system runs LOAD-LIBCURL at load time, so by the time any test
  ;; runs there is nothing left to do -- if this fails, nothing else can pass.
  (is (libcurl-loaded-p))
  (is (stringp (libcurl-pathname))))

(test version-is-reported
  ;; A parseable MAJOR.MINOR.PATCH, and a packed version number agreeing with it.
  (let ((version (libcurl-version)))
    (is (stringp version))
    (is (<= 3 (length version)))
    (let* ((parts (uiop:split-string version :separator "."))
           (major (parse-integer (first parts) :junk-allowed t))
           (minor (parse-integer (second parts) :junk-allowed t)))
      (is (integerp major))
      (is (integerp minor))
      ;; The packed number is 0xMMNNPP; check it against the string rather than
      ;; against a hard-coded version, so this survives a curl upgrade.
      (is (= major (ldb (byte 8 16) (libcurl-version-number))))
      (is (= minor (ldb (byte 8 8) (libcurl-version-number)))))))

(test version-info-age-gates-fields
  ;; AGE says how far into the struct it is safe to read.  Anything past it
  ;; must come back NIL rather than as uninitialised memory read as a pointer.
  (let* ((info (libcurl-version-info))
         (age (libcurl::version-info-age info)))
    (is (integerp age))
    (is (<= 0 age 11))
    (when (< age 11)
      (is (null (libcurl::version-info-rtmp-version info))))
    (when (< age 10)
      (is (null (libcurl::version-info-feature-names info))))
    (when (< age 7)
      (is (null (libcurl::version-info-zstd-version info))))))

(test version-at-least-p-brackets-the-running-version
  ;; True for its own version, false for one above it -- catches an inverted
  ;; comparison or a mis-packed version number.
  (let* ((n (libcurl-version-number))
         (major (ldb (byte 8 16) n))
         (minor (ldb (byte 8 8) n)))
    (is (version-at-least-p major minor))
    (is (version-at-least-p 7 0))
    (is (not (version-at-least-p (1+ major) 0)))))

(test features-are-reported
  ;; Any libcurl worth binding has SSL and can name some features.
  (let ((features (libcurl-features)))
    (is (listp features))
    (is (plusp (length features)))
    (is (feature-supported-p :ssl))
    (is (not (feature-supported-p :no-such-feature-whatsoever)))))

(test protocols-include-http
  (let ((protocols (libcurl-protocols)))
    (is (member "http" protocols :test #'string-equal))
    (is (protocol-supported-p :http))
    (is (protocol-supported-p "https"))
    (is (not (protocol-supported-p :nosuchproto)))))

(test global-init-is-idempotent
  ;; Called at load time already; calling it again must be a no-op rather than
  ;; a second real initialisation.
  (is (global-init))
  (is (global-init)))

(test strerror-round-trips
  ;; CURLE_OK and a real error both produce text, and they differ.
  (let ((ok (libcurl::%curl-easy-strerror 0))
        (timeout (libcurl::%curl-easy-strerror 28)))
    (is (stringp ok))
    (is (stringp timeout))
    (is (string/= ok timeout))))

(defun definition-kinds (symbol)
  "Every kind of definition SYMBOL names, as a list of keywords.

A symbol can name more than one -- a class and a function, say -- and each is
documented separately, so each has to be checked separately."
  (let ((kinds '()))
    (let ((function (and (fboundp symbol) (ignore-errors (fdefinition symbol)))))
      (cond ((macro-function symbol) (push :macro kinds))
            ((typep function 'generic-function) (push :generic kinds))
            (function (push :function kinds))))
    (when (find-class symbol nil) (push :class kinds))
    (when (boundp symbol)
      (push (if (constantp symbol) :constant :variable) kinds))
    kinds))

(defun definition-documentation (symbol kind)
  (ecase kind
    ((:function :generic :macro) (documentation symbol 'function))
    (:class (documentation (find-class symbol) t))
    ((:variable :constant) (documentation symbol 'variable))))

(test every-exported-definition-is-documented
  ;; The export list is the API, and an undocumented export is a symbol whose
  ;; meaning lives only in the source.  This is a test rather than a habit
  ;; because the gap is invisible: adding a slot with a :READER exports a
  ;; generic function whose documentation is not the slot's :DOCUMENTATION --
  ;; DESCRIBE and every documentation generator look at the function -- so a
  ;; carefully documented class can still leave its accessors bare.
  (let ((undocumented '())
        (checked 0))
    (do-external-symbols (symbol :libcurl)
      (dolist (kind (definition-kinds symbol))
        (incf checked)
        (unless (definition-documentation symbol kind)
          (push (list kind symbol) undocumented))))
    (is (plusp checked))
    (is (null undocumented)
        "~D exported definition~:P without documentation:~{~%  ~{~A ~A~}~}"
        (length undocumented)
        (sort undocumented #'string< :key #'second))))
