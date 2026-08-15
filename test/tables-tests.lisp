;;;; test/tables-tests.lisp — the generated tables, against the loaded libcurl.
;;;;
;;;; src/options-table.lisp is produced by regular expressions over curl.h,
;;;; which is only defensible because of this file.  libcurl can describe its
;;;; own options at runtime through curl_easy_option_by_name, so every entry is
;;;; checked against the library actually loaded: the identifier and the
;;;; argument type both have to agree.
;;;;
;;;; That makes this a drift detector as much as a parser test.  If the headers
;;;; the table was generated from and the library being loaded are different
;;;; versions, this is what says so -- which matters because the two really can
;;;; differ on macOS, where the Homebrew and system libcurls are years apart.

(in-package #:curlcl/test)

(in-suite tables)

;;; Our spelled type -> the type libcurl reports for the same option.  Nine
;;; names collapse onto libcurl's nine CURLOT_ values one-for-one; the mapping
;;; exists because the two vocabularies are spelled differently, not because
;;; either is lossy.
(defparameter *kind-to-easytype*
  '((:long . :long) (:values . :values) (:off-t . :off-t)
    (:objectpoint . :object) (:stringpoint . :string) (:slistpoint . :slist)
    (:cbpoint . :cbptr) (:blob . :blob) (:functionpoint . :function)))

(test tables-are-populated
  ;; Loose lower bounds rather than exact counts: regenerating against a newer
  ;; libcurl legitimately adds entries, and a test that has to be edited every
  ;; time is a test that stops being read.  A parse that silently collapsed
  ;; would fall far below these.
  (is (<= 300 (curlcl::option-count))
      "only ~D options in the table" (curlcl::option-count))
  (is (<= 70 (curlcl::info-count))
      "only ~D infos in the table" (curlcl::info-count))
  (is (<= 16 (hash-table-count curlcl::*multi-options*))))

(test every-table-option-agrees-with-libcurl
  ;; The load-bearing test for the generator.  For every option this binding
  ;; claims to know, ask the loaded libcurl what it thinks that option's
  ;; identifier and argument type are, and require agreement.
  (let ((checked 0)
        (mismatches '()))
    (curlcl::map-options
     (lambda (option)
       (let ((c-name (subseq (curlcl::option-c-name option) (length "CURLOPT_"))))
         (multiple-value-bind (id type) (curlcl::known-option c-name)
           (when id
             (incf checked)
             (unless (= id (curlcl::option-id option))
               (push (format nil "~A: table says ~D, libcurl says ~D"
                             c-name (curlcl::option-id option) id)
                     mismatches))
             (let ((expected (cdr (assoc (curlcl::option-kind option)
                                         *kind-to-easytype*))))
               (unless (eq type expected)
                 (push (format nil "~A: table says ~S (~S), libcurl says ~S"
                               c-name (curlcl::option-kind option) expected type)
                       mismatches))))))))
    ;; Guard against the test passing vacuously by skipping everything.
    (is (<= 250 checked)
        "only ~D options could be checked against libcurl" checked)
    (is (null mismatches)
        "~D option(s) disagree with the loaded curlcl:~%~{  ~A~%~}"
        (length mismatches) mismatches)))

(test libcurl-has-no-options-missing-from-the-table
  ;; The other direction: anything the loaded libcurl offers must be in the
  ;; table, or callers cannot reach it.  A failure here usually means the table
  ;; was generated against older headers than the library in use -- run
  ;; `make tables'.
  (let ((missing '()))
    (curlcl::map-known-options
     (lambda (name id type alias-p)
       (declare (ignore id type alias-p))
       (let ((keyword (intern (substitute #\- #\_ (string-upcase name)) :keyword)))
         (unless (curlcl::find-option keyword)
           (push name missing)))))
    (is (null missing)
        "The loaded libcurl (~A) has ~D option(s) the generated table lacks; ~
run `make tables'.~%  ~{~A~^ ~}"
        (libcurl-version) (length missing) missing)))

(test option-identifiers-encode-their-type
  ;; Spot checks against values read straight from the header, covering one
  ;; option of every base: long, string, function pointer, slist, callback
  ;; data, object pointer, curl_off_t and blob.
  (flet ((check (keyword id kind)
           (let ((option (curlcl::find-option keyword)))
             (is (not (null option)) "~S missing from the table" keyword)
             (when option
               (is (= id (curlcl::option-id option)))
               (is (eq kind (curlcl::option-kind option)))))))
    (check :url 10002 :stringpoint)
    (check :writedata 10001 :cbpoint)
    (check :writefunction 20011 :functionpoint)
    (check :httpheader 10023 :slistpoint)
    (check :ssl-verifypeer 64 :long)
    (check :maxfilesize-large 30117 :off-t)
    (check :cainfo-blob 40309 :blob)
    ;; The one that does not copy its argument, which is why it is an
    ;; OBJECTPOINT rather than a STRINGPOINT and why the binding has to own the
    ;; memory for it.
    (check :postfields 10015 :objectpoint)))

(test string-and-slist-options-are-distinguishable
  ;; STRINGPOINT, SLISTPOINT, CBPOINT and OBJECTPOINT are all 10000 numerically.
  ;; The generator keeps the spelled type precisely because that distinction
  ;; decides who owns the memory -- if it were lost, the table could not say
  ;; whether a value needs curl_slist_free_all or nothing at all.
  (is (eq :stringpoint (curlcl::option-kind (curlcl::find-option :url))))
  (is (eq :slistpoint (curlcl::option-kind (curlcl::find-option :httpheader))))
  (is (eq :cbpoint (curlcl::option-kind (curlcl::find-option :writedata))))
  (is (eq :objectpoint (curlcl::option-kind (curlcl::find-option :postfields))))
  ;; ...yet all four are the same numeric base.
  (dolist (keyword '(:url :httpheader :writedata :postfields))
    (is (= 10000 (* 10000 (floor (curlcl::option-id (curlcl::find-option keyword))
                                 10000))))))

(test argument-classes-cover-every-kind
  ;; Every option in the table must map to one of the three ABI argument
  ;; classes; an unmapped kind would signal at the first setopt using it.
  (curlcl::map-options
   (lambda (option)
     (is (member (curlcl::option-argument-class option) '(:long :off-t :pointer))
         "~S has no argument class" (curlcl::option-keyword option)))))

(test aliases-share-their-target-identifier
  ;; CURLOPT_ENCODING is a #define for CURLOPT_ACCEPT_ENCODING; a regex over
  ;; CURLOPT() alone would miss it, and callers do use the old spelling.
  (let ((alias (curlcl::find-option :encoding))
        (target (curlcl::find-option :accept-encoding)))
    (is (not (null alias)))
    (is (not (null target)))
    (is (= (curlcl::option-id alias) (curlcl::option-id target)))
    (is (eq :accept-encoding (curlcl::option-alias-of alias))))
  ;; These two resolve through a numeric #define rather than a named option,
  ;; which an alias pass that ran before the numeric pass would drop.
  (is (= 9999 (curlcl::option-id (curlcl::find-option :writeinfo))))
  (is (= 9999 (curlcl::option-id (curlcl::find-option :closepolicy)))))

(test deprecated-options-carry-their-replacement
  (let ((option (curlcl::find-option :httppost)))
    (is (not (null option)))
    (is (string= "7.56.0" (curlcl::option-deprecated option)))
    (is (search "MIMEPOST" (curlcl::option-replacement option)))))

(test unknown-options-signal-rather-than-reaching-libcurl
  ;; The point of validating up front is a report that names the option, rather
  ;; than a bare CURLE_UNKNOWN_OPTION from the far side of the ABI.
  (signals unsupported-option (curlcl::ensure-option :no-such-option-at-all))
  (handler-case (curlcl::ensure-option :no-such-option-at-all)
    (unsupported-option (c)
      (is (eq :no-such-option-at-all (unsupported-option-name c)))
      ;; The report must be readable; a condition nobody can print is no better
      ;; than an integer.
      (is (plusp (length (princ-to-string c)))))))

(test option-availability-is-cached-after-the-first-question
  (let ((option (curlcl::find-option :url)))
    (setf (curlcl::option-availability option) :unknown)
    (is (curlcl::option-supported-p option))
    (is (eq t (curlcl::option-availability option)))))

;;; Infos --------------------------------------------------------------------

(test info-identifiers-encode-their-type
  (flet ((check (keyword id kind)
           (let ((info (curlcl::find-info keyword)))
             (is (not (null info)) "~S missing from the info table" keyword)
             (when info
               (is (= id (curlcl::info-id info)))
               (is (eq kind (curlcl::info-kind info)))))))
    (check :effective-url #x100001 :string)
    (check :response-code #x200002 :long)
    (check :total-time #x300003 :double)
    (check :total-time-t #x600032 :off-t)
    ;; The single narrow out-parameter in the whole API: an int, not a long.
    (check :activesocket #x50002C :socket)))

(test info-kinds-match-their-mask-except-where-libcurl-lies
  ;; Every info's kind must follow from its type mask -- with exactly two
  ;; documented families of exception, both of which produce wrong answers
  ;; rather than errors if taken at face value.
  (curlcl::map-infos
   (lambda (info)
     (let ((mask (curlcl::info-type-mask (curlcl::info-id info)))
           (kind (curlcl::info-kind info))
           (name (curlcl::info-c-name info)))
       (cond
         ;; CURLINFO_PRIVATE claims STRING but returns the void* stored with
         ;; CURLOPT_PRIVATE.  Decoding it as a string reads a random pointer.
         ((string= name "CURLINFO_PRIVATE")
          (is (eq :pointer kind))
          (is (= #x100000 mask)))
         ;; CURLINFO_SLIST and CURLINFO_PTR are the same number, so the mask
         ;; cannot say which; resolved by name, and the difference is whether
         ;; the result must be freed with curl_slist_free_all.
         ((= mask #x400000)
          (is (member kind '(:slist :pointer))
              "~A has mask 0x400000 but kind ~S" name kind))
         (t
          (is (eq kind (cdr (assoc mask curlcl::*info-type-masks*)))
              "~A: mask ~X implies ~S, table says ~S"
              name mask (cdr (assoc mask curlcl::*info-type-masks*)) kind)))))))

(test slist-infos-are-separated-from-pointer-infos
  ;; The ownership split behind the shared 0x400000 mask.  Getting this
  ;; backwards either leaks an slist or calls curl_slist_free_all on a
  ;; structure libcurl still owns.
  (is (eq :slist (curlcl::info-kind (curlcl::find-info :ssl-engines))))
  (is (eq :slist (curlcl::info-kind (curlcl::find-info :cookielist))))
  (is (eq :pointer (curlcl::info-kind (curlcl::find-info :certinfo))))
  (is (eq :pointer (curlcl::info-kind (curlcl::find-info :tls-ssl-ptr)))))

(test deprecated-double-infos-have-off-t-successors
  ;; Most of the CURLINFO_DOUBLE family is superseded by _T variants; both are
  ;; kept, because code in the wild still reads the old ones.
  (is (string= "7.55.0" (curlcl::info-deprecated (curlcl::find-info :size-upload))))
  (is (null (curlcl::info-deprecated (curlcl::find-info :size-upload-t))))
  ;; Same ordinal, different type mask -- which is why a value->name map keyed
  ;; on the ordinal alone would collide.  (Not universal: CURLINFO_TOTAL_TIME
  ;; is ordinal 3 while CURLINFO_TOTAL_TIME_T is 50, so the pairing cannot be
  ;; assumed either.)
  (is (= (logand #x0fffff (curlcl::info-id (curlcl::find-info :size-upload)))
         (logand #x0fffff (curlcl::info-id (curlcl::find-info :size-upload-t)))))
  (is (/= (curlcl::info-id (curlcl::find-info :size-upload))
          (curlcl::info-id (curlcl::find-info :size-upload-t)))))
