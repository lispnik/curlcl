;;;; generator/generate-tables.lisp — emit the option and info tables.
;;;;
;;;; Reads the installed curl headers and writes src/options-table.lisp and
;;;; src/infos-table.lisp.  Those outputs are committed, so building or using
;;;; the library never needs headers, a C compiler, or this system; run it only
;;;; when targeting a newer libcurl, via `make tables'.
;;;;
;;;; Why parse the headers at all, when libcurl can describe its own options at
;;;; runtime through curl_easy_option_next?  Because runtime introspection
;;;; gives the C name and a coarse type but no Lisp keyword, nothing at all for
;;;; CURLINFO, and no answer until the library is loaded -- so it cannot
;;;; provide compile-time-known names.  It is used instead as the *check*: the
;;;; test suite walks the generated table against the loaded library, which
;;;; catches drift between the headers this ran on and the library in use.
;;;;
;;;; Parsing C with regular expressions is normally a mistake.  It is defensible
;;;; here only because the declarations are macro invocations with a rigidly
;;;; fixed shape, and because the result is verified against the real library by
;;;; the tests.  The shape still has teeth:
;;;;
;;;;   - CURLOPTDEPRECATED and CURL_DEPRECATED wrap across lines, and
;;;;     CURL_DEPRECATED sits *between* an enumerator's name and its `=', so
;;;;     newlines have to be normalised before matching.
;;;;   - The CURLINFO_ prefix names two unrelated enums: the getinfo constants
;;;;     and curl_infotype, the debug callback's argument.  Only the former has
;;;;     `= CURLINFO_<TYPE> + n' bodies, which is how they are told apart.
;;;;   - CURLMOPT_ uses the very same CURLOPT() macro, in multi.h.
;;;;   - Nineteen options exist only as #define aliases and are invisible to a
;;;;     regex over CURLOPT(); two of those are plain numbers, not names.

(defpackage #:curlcl/generator
  (:use #:cl)
  (:export #:generate-tables
           #:find-header-directory))

(in-package #:curlcl/generator)

;;; Locating the headers ------------------------------------------------------

(defparameter *header-search-path*
  '("/opt/homebrew/opt/curl/include/curl/"
    "/usr/local/opt/curl/include/curl/"
    "/usr/include/curl/"
    "/usr/local/include/curl/")
  "Where to look for curl.h, in order.  Homebrew first, to match the load
order in src/library.lisp -- generating from one libcurl's headers while the
library loads another is the drift the test suite exists to catch.")

(defun find-header-directory ()
  "The directory holding curl.h, or NIL."
  (or (loop for dir in *header-search-path*
            when (probe-file (merge-pathnames "curl.h" dir))
              return (pathname dir))
      ;; Fall back to asking curl-config where it put them.
      (let ((prefix (ignore-errors
                     (string-trim '(#\Newline #\Space)
                                  (uiop:run-program '("curl-config" "--prefix")
                                                    :output :string)))))
        (when (and prefix (plusp (length prefix)))
          (let ((dir (merge-pathnames "include/curl/"
                                      (uiop:ensure-directory-pathname prefix))))
            (when (probe-file (merge-pathnames "curl.h" dir))
              dir))))))

(defun read-header (directory name)
  (let ((path (merge-pathnames name directory)))
    (unless (probe-file path)
      (error "Cannot read ~A -- no curl headers at ~A." name directory))
    (uiop:read-file-string path)))

;;; Text preparation ----------------------------------------------------------

(defun strip-comments (text)
  "Remove /* ... */ comments.  libcurl's headers have no // comments in the
declarations we parse, and no /* inside the string literals we care about."
  (cl-ppcre:regex-replace-all "(?s)/\\*.*?\\*/" text " "))

(defun collapse-whitespace (text)
  "Fold every run of whitespace to a single space so multi-line macro
invocations match as though they were written on one line."
  (cl-ppcre:regex-replace-all "\\s+" text " "))

(defun prepare (text)
  (collapse-whitespace (strip-comments text)))

;;; Naming --------------------------------------------------------------------

(defun c-name-to-keyword (c-name prefix)
  "CURLOPT_SSL_VERIFYPEER, \"CURLOPT_\" -> :SSL-VERIFYPEER.

Purely mechanical and reversible, so a caller can guess the keyword for any
option from its documented C name: drop the prefix, downcase, underscores
become hyphens.  Nothing is prettified -- CURLOPT_WRITEDATA is :WRITEDATA, not
:WRITE-DATA -- because a rule with exceptions is worse than a blunt one."
  (intern (substitute #\- #\_ (string-upcase (subseq c-name (length prefix))))
          :keyword))

;;; Option type bases ---------------------------------------------------------

(defparameter *option-type-bases*
  '(("CURLOPTTYPE_LONG"          . (0     . :long))
    ("CURLOPTTYPE_VALUES"        . (0     . :values))
    ("CURLOPTTYPE_OBJECTPOINT"   . (10000 . :objectpoint))
    ("CURLOPTTYPE_STRINGPOINT"   . (10000 . :stringpoint))
    ("CURLOPTTYPE_SLISTPOINT"    . (10000 . :slistpoint))
    ("CURLOPTTYPE_CBPOINT"       . (10000 . :cbpoint))
    ("CURLOPTTYPE_FUNCTIONPOINT" . (20000 . :functionpoint))
    ("CURLOPTTYPE_OFF_T"         . (30000 . :off-t))
    ("CURLOPTTYPE_BLOB"          . (40000 . :blob)))
  "Spelled type -> (numeric base . Lisp kind).

Nine names, five numbers.  STRINGPOINT, SLISTPOINT and CBPOINT are all
OBJECTPOINT numerically, and VALUES is LONG; the header spells them apart
precisely so tools like this one can recover the distinction, and this binding
needs it because it decides memory ownership.")

(defun option-type-info (spelled)
  (or (cdr (assoc spelled *option-type-bases* :test #'string=))
      (error "Unknown CURLOPTTYPE_: ~A" spelled)))

;;; Parsing options -----------------------------------------------------------

(defun parse-options (text prefix)
  "Collect CURLOPT()/CURLOPTDEPRECATED() entries whose names start with PREFIX.

Returns a list of (keyword id kind c-name deprecated replacement alias-of)."
  (let ((entries '()))
    ;; Plain declarations.
    (cl-ppcre:do-register-groups (name type ordinal)
        ((format nil "(?<!DEPRECATED)\\bCURLOPT\\( *(~A\\w+) *, *(CURLOPTTYPE_\\w+) *, *(\\d+) *\\)"
                 prefix)
         text)
      (destructuring-bind (base . kind) (option-type-info type)
        (push (list (c-name-to-keyword name prefix)
                    (+ base (parse-integer ordinal))
                    kind name nil nil nil)
              entries)))
    ;; Deprecated declarations carry the version that deprecated them and the
    ;; advice string, both of which are worth surfacing at use.
    (cl-ppcre:do-register-groups (name type ordinal version message)
        ((format nil "CURLOPTDEPRECATED\\( *(~A\\w+) *, *(CURLOPTTYPE_\\w+) *, *(\\d+) *, *([0-9.]+) *, *\"([^\"]*)\" *\\)"
                 prefix)
         text)
      (destructuring-bind (base . kind) (option-type-info type)
        (push (list (c-name-to-keyword name prefix)
                    (+ base (parse-integer ordinal))
                    kind name version message nil)
              entries)))
    (nreverse entries)))

(defun parse-option-aliases (text prefix entries)
  "Collect `#define CURLOPT_OLD CURLOPT_NEW' aliases, resolved against ENTRIES.

These are invisible to a regex over CURLOPT(), but they are real option names
that real code passes, so the table would be incomplete without them."
  (let ((by-name (make-hash-table :test 'equal))
        (aliases '()))
    (dolist (entry entries)
      (setf (gethash (fourth entry) by-name) entry))
    ;; Numeric defines first, and seeded into BY-NAME: CURLOPT_OBSOLETE40 and
    ;; CURLOPT_OBSOLETE72 are `#define ... 9999' rather than aliases of a name,
    ;; and two further aliases -- CURLOPT_WRITEINFO and CURLOPT_CLOSEPOLICY --
    ;; point at them.  Resolving names before numbers would silently drop that
    ;; second pair, since their targets appear in no CURLOPT() declaration.
    (cl-ppcre:do-register-groups (name value)
        ((format nil "#define +(~A\\w+) +(\\d+)\\b" prefix) text)
      (let ((entry (list (c-name-to-keyword name prefix)
                         (parse-integer value)
                         :long name "obsolete" nil nil)))
        (setf (gethash name by-name) entry)
        (push entry aliases)))
    (cl-ppcre:do-register-groups (old new)
        ((format nil "#define +(~A\\w+) +(~A\\w+)\\b" prefix prefix) text)
      (let ((target (gethash new by-name)))
        (when target
          (push (list (c-name-to-keyword old prefix)
                      (second target)      ; same id
                      (third target)       ; same kind
                      old
                      (fifth target)       ; inherit deprecation
                      (sixth target)
                      (c-name-to-keyword new prefix))
                aliases))))
    (nreverse aliases)))

;;; Parsing infos -------------------------------------------------------------

(defparameter *info-type-masks*
  '((#x100000 . :string)
    (#x200000 . :long)
    (#x300000 . :double)
    (#x400000 . :slist)                 ; ...or :POINTER; see below
    (#x500000 . :socket)
    (#x600000 . :off-t)))

(defparameter *info-pointer-overrides*
  '("CURLINFO_CERTINFO"                 ; struct curl_certinfo *
    "CURLINFO_TLS_SESSION"              ; struct curl_tlssessioninfo *
    "CURLINFO_TLS_SSL_PTR")             ; struct curl_tlssessioninfo *
  "Infos sharing the 0x400000 mask that return an opaque pointer rather than a
curl_slist.  CURLINFO_SLIST and CURLINFO_PTR are literally the same number, so
nothing in the value distinguishes them -- but the distinction is ownership:
an slist result must be released with curl_slist_free_all and a pointer result
must not be touched.  Only five infos use the mask, so they are resolved here
by name.")

(defparameter *info-kind-overrides*
  '(("CURLINFO_PRIVATE" . :pointer))
  "Infos whose declared mask lies about their real type.

CURLINFO_PRIVATE is typed CURLINFO_STRING but hands back whatever void* was
stored with CURLOPT_PRIVATE.  Left as :STRING, the binding would call
foreign-string-to-lisp on an arbitrary pointer.")

(defun info-kind (c-name id)
  (let ((base (cdr (assoc (logand id #xf00000) *info-type-masks*))))
    (cond ((cdr (assoc c-name *info-kind-overrides* :test #'string=)))
          ((and (eq base :slist)
                (member c-name *info-pointer-overrides* :test #'string=))
           :pointer)
          (base)
          (t (error "Unknown CURLINFO type mask in ~A (~X)" c-name id)))))

(defun parse-infos (text)
  "Collect the getinfo constants.

Matched on `= CURLINFO_<TYPE> + <n>', which is also what separates them from
curl_infotype -- an unrelated enum sharing the CURLINFO_ prefix whose members
are the debug callback's argument and have no such bodies."
  (let ((entries '())
        (bases '(("STRING" . #x100000) ("LONG" . #x200000) ("DOUBLE" . #x300000)
                 ("SLIST" . #x400000) ("PTR" . #x400000) ("SOCKET" . #x500000)
                 ("OFF_T" . #x600000))))
    (cl-ppcre:do-register-groups (name deprecated-version base ordinal)
        ("\\b(CURLINFO_\\w+) *(?:CURL_DEPRECATED\\( *([0-9.]+) *, *\"[^\"]*\" *\\))? *= *CURLINFO_(STRING|LONG|DOUBLE|SLIST|PTR|SOCKET|OFF_T) *\\+ *(\\d+)"
         text)
      (let* ((id (+ (cdr (assoc base bases :test #'string=))
                    (parse-integer ordinal))))
        (push (list (c-name-to-keyword name "CURLINFO_")
                    id
                    (info-kind name id)
                    name
                    deprecated-version)
              entries)))
    (nreverse entries)))

;;; Emitting ------------------------------------------------------------------

(defun write-banner (stream source-directory version what regenerate)
  (format stream ";;;; ~A — GENERATED FILE, DO NOT EDIT.~%" what)
  (format stream ";;;;~%")
  (format stream ";;;; Produced by generator/generate-tables.lisp from the libcurl ~A~%"
          version)
  (format stream ";;;; headers in ~A~%" source-directory)
  (format stream ";;;;~%")
  (format stream ";;;; Regenerate with `make tables'.  ~A~%" regenerate)
  (format stream ";;;; The committed output is what the library loads, so a build never~%")
  (format stream ";;;; needs curl headers -- and the test suite checks this table against~%")
  (format stream ";;;; whatever libcurl is actually loaded, which is what catches drift.~%~%")
  (format stream "(in-package #:curlcl)~%~%"))

(defun print-entry (stream entry &key hex)
  ;; Written by hand rather than with ~S on the whole list so the columns line
  ;; up and the file stays readable as a reference.  Info ids are emitted in
  ;; hex because their type is the high nibble -- #x200002 shows at a glance
  ;; that CURLINFO_RESPONSE_CODE is a long, where 2097154 shows nothing.
  (format stream "   (~(~S~) ~:[~D~;#x~6,'0X~] ~(~S~) ~S~{ ~S~})~%"
          (first entry) hex (second entry) (third entry) (fourth entry)
          (nthcdr 4 entry)))

(defun emit-table (path banner-args form-name entries &key hex)
  (with-open-file (stream path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
    (apply #'write-banner stream banner-args)
    (format stream "(~A~%  '(~%" form-name)
    (dolist (entry entries)
      (print-entry stream entry :hex hex))
    (format stream "   ))~%"))
  (length entries))

(defun header-version (curl-h-directory)
  (let ((text (read-header curl-h-directory "curlver.h")))
    (or (cl-ppcre:register-groups-bind (version)
            ("#define +LIBCURL_VERSION +\"([^\"]+)\"" text)
          version)
        "unknown")))

(defun generate-tables (&key directory (output (asdf:system-relative-pathname
                                                :curlcl "src/")))
  "Regenerate src/options-table.lisp and src/infos-table.lisp.

DIRECTORY overrides where the curl headers are found."
  (let* ((headers (or directory (find-header-directory)
                      (error "No curl headers found.  Looked in ~{~A~^, ~} and ~
asked curl-config.  Pass :DIRECTORY to override."
                             *header-search-path*)))
         (version (header-version headers))
         (curl-h (prepare (read-header headers "curl.h")))
         (multi-h (prepare (read-header headers "multi.h")))
         (options (parse-options curl-h "CURLOPT_"))
         (aliases (parse-option-aliases curl-h "CURLOPT_" options))
         (multi-options (parse-options multi-h "CURLMOPT_"))
         (infos (parse-infos curl-h)))
    (when (null options)
      (error "Parsed no options from ~A -- the header format has changed." headers))
    (let ((all-options (append options aliases)))
      (emit-table (merge-pathnames "options-table.lisp" output)
                  (list headers version "src/options-table.lisp"
                        (format nil "~D options (~D declared, ~D aliases) and ~D multi options."
                                (length all-options) (length options)
                                (length aliases) (length multi-options)))
                  "register-options" all-options)
      ;; Multi options append to the same file: they share the CURLOPT() macro
      ;; and the same numbering scheme, and splitting them would imply a
      ;; difference that does not exist.
      (with-open-file (stream (merge-pathnames "options-table.lisp" output)
                              :direction :output :if-exists :append)
        (format stream "~%(register-multi-options~%  '(~%")
        (dolist (entry multi-options) (print-entry stream entry))
        (format stream "   ))~%"))
      (emit-table (merge-pathnames "infos-table.lisp" output)
                  (list headers version "src/infos-table.lisp"
                        (format nil "~D info values." (length infos)))
                  "register-infos" infos :hex t)
      (format t "~&Generated from libcurl ~A headers in ~A:~%" version headers)
      (format t "  ~D easy options (~D declared + ~D aliases)~%"
              (length all-options) (length options) (length aliases))
      (format t "  ~D multi options~%" (length multi-options))
      (format t "  ~D info values~%" (length infos))
      (values (length all-options) (length multi-options) (length infos)))))
