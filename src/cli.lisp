;;;; src/cli.lisp — curlcl, a curl-compatible command-line driver.
;;;;
;;;; A working program rather than a demo: the option names, the defaults, the
;;;; output destinations and the exit codes follow curl(1), so a curl command
;;;; line usually works unchanged.  That constraint is the point -- it forces
;;;; the library to cover what a real client needs rather than what is
;;;; convenient to expose.
;;;;
;;;; Exit codes are libcurl's own CURLcode values, as curl's are: 6 for an
;;;; unresolved host, 7 for a refused connection, 22 for --fail on a 4xx, 28 for
;;;; a timeout.  Scripts that check curl's exit status keep working.
;;;;
;;;; Two deliberate departures from curl, both noted in --help:
;;;;
;;;;   There is no progress meter unless --progress-bar is given.  curl shows
;;;;   one by default; here the default is quiet, which is what almost every
;;;;   scripted use wants and what -s would otherwise be needed for.
;;;;
;;;;   --write-out understands the variables listed in WRITE-OUT-VALUE rather
;;;;   than curl's full set.

(defpackage #:curlcl/cli
  (:use #:cl)
  (:export #:main))

(in-package #:curlcl/cli)

(defparameter *program-name* "curlcl")
(defparameter *program-version* "0.1.0")

;;; Output --------------------------------------------------------------------

(defun standard-descriptor (direction)
  "The descriptor the running Lisp uses for its own standard stream.

Not simply 0 and 1.  SBCL on Windows keeps an OS handle in an fd-stream's fd
slot rather than a C-runtime descriptor, so passing a literal 1 to
MAKE-FD-STREAM there builds a stream on handle 1 -- which is not standard
output and generally not a handle at all.  That is what `curlcl -s URL' did on
Windows, and it failed with \"The handle is invalid\" on the first byte of
every response body.  Asking the implementation for the descriptor it is
already using gets the right kind of number on both platforms without naming
either."
  (declare (ignorable direction))
  #+sbcl (sb-sys:fd-stream-fd (if (eq direction :input)
                                  sb-sys:*stdin*
                                  sb-sys:*stdout*))
  #-sbcl (if (eq direction :input) 0 1))

(defun fd-byte-stream (fd direction)
  "A byte stream on FD, for standard input and standard output.

Every implementation can do this; none of them spells it the same way, and the
standard offers no way at all -- an existing stream's element type cannot be
changed, and *STANDARD-INPUT* is a character stream whose external format would
mangle arbitrary bytes.  Hence the clauses.

FD comes from STANDARD-DESCRIPTOR rather than being written as 0 or 1, because
what counts as a descriptor is implementation- and platform-specific; see there.
The /dev/fd fallback is Unix-only, so an implementation not named here has no
route on Windows."
  (declare (ignorable direction))
  #+sbcl (sb-sys:make-fd-stream fd :input (eq direction :input)
                                   :output (eq direction :output)
                                   :element-type '(unsigned-byte 8)
                                   :buffering :full)
  #+ecl (ext:make-stream-from-fd fd (if (eq direction :input) :input :output)
                                 :element-type '(unsigned-byte 8))
  #+ccl (ccl:make-fd-stream fd :direction direction
                               :element-type '(unsigned-byte 8)
                               :sharing :lock)
  #+clisp (ext:make-stream fd :direction direction
                              :element-type '(unsigned-byte 8))
  #-(or sbcl ecl ccl clisp)
  (let ((path (format nil "/dev/fd/~D" fd)))
    (unless (probe-file path)
      (error "No way to open file descriptor ~D as a byte stream on this ~
implementation, and ~A does not exist." fd path))
    (open path :direction direction :element-type '(unsigned-byte 8)
               :if-exists :append)))

(defun binary-stdin ()
  "A byte stream on standard input, for `-T -'."
  (fd-byte-stream (standard-descriptor :input) :input))

(defvar *binary-stdout* nil)

(defun binary-stdout ()
  "The byte stream on standard output.

Response bodies are octets and may be anything at all, so they go to the file
descriptor rather than through *STANDARD-OUTPUT*, which would try to encode
them.

Memoised: several buffered streams on one descriptor interleave their flushes,
which under --parallel would shuffle the bodies together."
  (or *binary-stdout*
      (setf *binary-stdout* (fd-byte-stream (standard-descriptor :output) :output))))

(defun message (control &rest arguments)
  "Write a diagnostic to standard error, as curl does."
  (format *error-output* "~&~A: ~?~%" *program-name* control arguments)
  (finish-output *error-output*))

(defun url-filename (url)
  "The last path segment of URL, for --remote-name."
  (let* ((parts (curlcl:parse-url url))
         (path (or (getf parts :path) "/"))
         (slash (position #\/ path :from-end t))
         (name (if slash (subseq path (1+ slash)) path)))
    (if (plusp (length name)) name "index.html")))

;;; --write-out ---------------------------------------------------------------

(defun write-out-value (variable response)
  "The value of a --write-out %{variable}, or NIL if we do not know it."
  (let ((timings (curlcl:response-timings response)))
    (flet ((seconds (key) (/ (or (getf timings key) 0) 1000000.0d0)))
      (cond
        ((string= variable "http_code") (curlcl:response-status response))
        ((string= variable "response_code") (curlcl:response-status response))
        ((string= variable "url_effective") (curlcl:response-url response))
        ((string= variable "content_type")
         (or (curlcl:response-content-type response) ""))
        ((string= variable "num_redirects") (curlcl:response-redirect-count response))
        ;; From getinfo, not the body: with -o the body was streamed to a file
        ;; and never buffered, so its length would be zero.
        ((string= variable "size_download") (curlcl:response-size-download response))
        ((string= variable "size_upload") (curlcl:response-size-upload response))
        ;; curl prints the bare version -- "2", "1.1" -- not the keyword.
        ((string= variable "http_version")
         (case (curlcl:response-version response)
           (:http/1.0 "1.0") (:http/1.1 "1.1") (:http/2 "2") (:http/3 "3")
           (t "0")))
        ((string= variable "time_total") (format nil "~,6F" (seconds :total)))
        ((string= variable "time_namelookup") (format nil "~,6F" (seconds :namelookup)))
        ((string= variable "time_connect") (format nil "~,6F" (seconds :connect)))
        ((string= variable "time_appconnect") (format nil "~,6F" (seconds :appconnect)))
        ((string= variable "time_pretransfer") (format nil "~,6F" (seconds :pretransfer)))
        ((string= variable "time_starttransfer")
         (format nil "~,6F" (seconds :starttransfer)))
        ((string= variable "time_redirect") (format nil "~,6F" (seconds :redirect)))
        (t nil)))))

(defun expand-write-out (format response)
  "Expand a --write-out FORMAT against RESPONSE.

Understands %{variable}, \\n, \\t and \\r, as curl does.  An unknown variable
is left as it was written rather than silently becoming empty, so a typo is
visible."
  (with-output-to-string (out)
    (loop with index = 0
          while (< index (length format))
          for character = (char format index)
          do (cond
               ;; %{name}
               ((and (char= character #\%)
                     (< (1+ index) (length format))
                     (char= (char format (1+ index)) #\{))
                (let ((end (position #\} format :start index)))
                  (cond
                    (end (let* ((name (subseq format (+ index 2) end))
                                (value (write-out-value name response)))
                           (write-string (if value (princ-to-string value)
                                             (format nil "%{~A}" name))
                                         out)
                           (setf index (1+ end))))
                    (t (write-char character out) (incf index)))))
               ;; \n, \t, \r
               ((and (char= character #\\) (< (1+ index) (length format)))
                (let ((next (char format (1+ index))))
                  (write-char (case next (#\n #\Newline) (#\t #\Tab)
                                (#\r #\Return) (t next))
                              out)
                  (incf index 2)))
               (t (write-char character out) (incf index))))))

;;; Turning the command line into request options -----------------------------

(defun split-once (string character)
  "Split STRING at the first CHARACTER.  Returns (values before after)."
  (let ((position (position character string)))
    (if position
        (values (subseq string 0 position) (subseq string (1+ position)))
        (values string nil))))

(defun parse-form-part (specification)
  "Parse a -F argument into a plist for ADD-MIME-PART.

Accepts curl's forms: name=value, name=@path, and a ;type= suffix on either."
  (multiple-value-bind (name rest) (split-once specification #\=)
    (unless rest
      (error "-F needs name=value or name=@file, got ~S" specification))
    (multiple-value-bind (value type-part) (split-once rest #\;)
      (let ((content-type
              (when (and type-part (eql 0 (search "type=" type-part)))
                (subseq type-part 5))))
        (if (and (plusp (length value)) (char= (char value 0) #\@))
            (list* :name name :file (subseq value 1)
                   (when content-type (list :content-type content-type)))
            (list* :name name :data value
                   (when content-type (list :content-type content-type))))))))

(defun data-file-octets (path)
  "Read PATH as octets, or standard input when PATH is \"-\".

Octets rather than a string because --data-binary exists precisely to send a
file that is not text, and decoding one as UTF-8 to re-encode it would corrupt
exactly the case the flag is for."
  (if (string= path "-")
      (let ((stream (binary-stdin))
            (buffer (make-array 65536 :element-type '(unsigned-byte 8)))
            (out (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
        (loop for n = (read-sequence buffer stream)
              while (plusp n)
              do (dotimes (i n) (vector-push-extend (aref buffer i) out)))
        (coerce out '(simple-array (unsigned-byte 8) (*))))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let* ((octets (make-array (file-length in)
                                   :element-type '(unsigned-byte 8)))
               (n (read-sequence octets in)))
          ;; READ-SEQUENCE may stop short of FILE-LENGTH, so trust its count.
          (subseq octets 0 n)))))

(defun strip-line-breaks (octets)
  "Remove CR and LF.  This is what curl's -d does to file content, and what
--data-binary deliberately does not."
  (remove-if (lambda (byte) (or (= byte 10) (= byte 13))) octets))

(defun data-argument-octets (argument &key file strip)
  "The bytes one data argument contributes.

FILE enables curl's @path convention, and @- for standard input; --data-raw is
the form that does not have it, so a body may begin with a literal @.  STRIP
removes line breaks from file content, which -d does and --data-binary does
not."
  (if (and file (plusp (length argument)) (char= (char argument 0) #\@))
      (let ((octets (data-file-octets (subseq argument 1))))
        (if strip (strip-line-breaks octets) octets))
      (curlcl::coerce-to-octets argument)))

(defun form-urlencode (string)
  "Encode STRING the way --data-urlencode does.

libcurl's own encoder does the escaping, so the edge cases agree with curl
rather than with our reading of RFC 3986 -- except for one, which it cannot
do: this is application/x-www-form-urlencoded, where a space is `+' and not
`%20'.  Rewriting the escaper's %20 afterwards is exact rather than a
heuristic, because a literal %20 in the input has already become %2520 by the
time we look."
  (let ((escaped (curlcl:with-easy (handle)
                   (curlcl:url-escape handle string))))
    (with-output-to-string (out)
      (loop with i = 0
            while (< i (length escaped))
            do (if (and (<= (+ i 3) (length escaped))
                        (string= "%20" escaped :start2 i :end2 (+ i 3)))
                   (progn (write-char #\+ out) (incf i 3))
                   (progn (write-char (char escaped i) out) (incf i)))))))

(defun data-urlencode-octets (argument)
  "One --data-urlencode argument, in curl's five forms:

  content        the whole argument, encoded
  =content       likewise, the leading = only marking it as nameless
  name=content   name kept literal, content encoded
  @file          the file's content, encoded
  name@file      name kept literal, the file's content encoded

Only the content is encoded; curl leaves the name alone, which matters because
a name is already a valid key and encoding it would change it."
  (let ((equals (position #\= argument))
        (at (position #\@ argument)))
    (flet ((pair (name content)
             (curlcl::coerce-to-octets
              (if (plusp (length name))
                  (format nil "~A=~A" name (form-urlencode content))
                  (form-urlencode content)))))
      (cond
        ;; @ wins when it comes first, so name@file is a file and not a name
        ;; whose content happens to contain an @.
        ((and at (or (null equals) (< at equals)))
         (pair (subseq argument 0 at)
               (curlcl::octets-to-string
                (data-file-octets (subseq argument (1+ at))))))
        (equals (pair (subseq argument 0 equals) (subseq argument (1+ equals))))
        (t (pair "" argument))))))

(defun join-octets (pieces separator)
  "Concatenate PIECES with a one-byte SEPARATOR between them."
  (let* ((total (+ (reduce #'+ pieces :key #'length)
                   (max 0 (1- (length pieces)))))
         (out (make-array total :element-type '(unsigned-byte 8)))
         (at 0))
    (loop for piece in pieces
          for first = t then nil
          do (unless first
               (setf (aref out at) separator)
               (incf at))
             (replace out piece :start1 at)
             (incf at (length piece)))
    out))

(defun collect-data (command)
  "Every data argument, joined with & as curl does.

Returns octets, not a string: --data-binary can carry a file that is not text.

One difference from curl, and it is in the joining rather than the reading:
curl concatenates these in command-line order, while clingon collects each
option into a list of its own, so the four flags are concatenated in a fixed
order here.  It shows only when the flags are interleaved -- `-d a
--data-binary b -d c' -- and the server cares which piece came first."
  (let ((pieces (append
                 (mapcar (lambda (argument)
                           (data-argument-octets argument :file t :strip t))
                         (clingon:getopt command :data))
                 (mapcar (lambda (argument)
                           (data-argument-octets argument :file t :strip nil))
                         (clingon:getopt command :data-binary))
                 (mapcar (lambda (argument)
                           (data-argument-octets argument :file nil))
                         (clingon:getopt command :data-raw))
                 (mapcar #'data-urlencode-octets
                         (clingon:getopt command :data-urlencode)))))
    (when pieces
      (join-octets pieces (char-code #\&)))))

(defun request-method (command data forms upload)
  "Work out the method the way curl does, unless -X overrides it."
  (let ((explicit (clingon:getopt command :request)))
    (cond (explicit (string-upcase explicit))
          ((clingon:getopt command :head) :head)
          (upload :put)
          ((or data forms) :post)
          (t :get))))

(defun append-query (url data)
  "Append DATA to URL's query string, for -G."
  (curlcl:with-url (parsed url)
    (setf (curlcl:url-part parsed :query :append-query) data)
    (curlcl:url-string parsed)))

(defun retry-codes-for (command)
  "Which transport failures --retry should retry, as curl chooses them.

The library retries a refused connection by default; curl does not, and makes
you ask with --retry-connrefused.  Following curl here rather than the library
default is the whole point of this driver, so the code is removed unless the
flag is given.  --retry-all-errors is curl's sledgehammer: every transport
failure, transient or not."
  (cond ((clingon:getopt command :retry-all-errors) t)
        ((clingon:getopt command :retry-connrefused) curlcl:*retryable-codes*)
        (t (remove :couldnt-connect curlcl:*retryable-codes*))))

(defun retry-specification (command)
  (let ((count (clingon:getopt command :retry)))
    (when (and count (plusp count))
      ;; curl's --retry N means N retries after the first attempt.
      (append
       (list :max-attempts (1+ count)
             :codes (retry-codes-for command)
             ;; curl retries whatever it was asked to, including POST.
             :non-idempotent t)
       ;; --retry-delay in curl is a delay, not a starting point: it waits
       ;; exactly that long each time, and backs off exponentially only when
       ;; you did not say.  Multiplying it would make `--retry-delay 1 --retry 3'
       ;; take seven seconds where curl takes three.  The jitter goes with it
       ;; for the same reason -- an explicit number is an instruction.
       (let ((delay (clingon:getopt command :retry-delay)))
         (if delay
             (list :initial-delay (float delay 1d0) :multiplier 1d0 :jitter 0d0)
             ;; No delay given: curl's own default backoff starts at a second
             ;; and doubles.  The jitter is ours and is kept -- it is invisible
             ;; to a caller and it is what stops a fleet retrying in lockstep.
             (list :initial-delay 1d0)))
       (when (clingon:getopt command :retry-max-time)
         (list :max-total-time (clingon:getopt command :retry-max-time)))))))

(defun parse-rate (text)
  "curl's --limit-rate: a byte count with an optional K, M, G or T suffix.

The suffixes are binary, as curl's are: 1K is 1024 and not 1000."
  (let* ((trimmed (string-trim " " text))
         (last (and (plusp (length trimmed))
                    (char-upcase (char trimmed (1- (length trimmed))))))
         (scale (position last "KMGT")))
    (multiple-value-bind (value end)
        (parse-integer trimmed :junk-allowed t)
      (unless (and value
                   (or (= end (length trimmed))
                       (and scale (= end (1- (length trimmed))))))
        (error "--limit-rate wants a number with an optional K, M, G or T, ~
got ~S" text))
      (if scale (* value (expt 1024 (1+ scale))) value))))

(defun parse-time-condition (text)
  "curl's -z: a date, optionally prefixed with - to invert the sense.

Returns (values curl-timecondition unix-seconds).  The date goes through
libcurl's own parser, which is the same one that reads Last-Modified, so
whatever a server would have written is accepted here."
  (let* ((negated (and (plusp (length text)) (char= (char text 0) #\-)))
         (body (if negated (subseq text 1) text))
         (universal (curlcl:parse-http-date body)))
    (unless universal
      (error "-z wants a date libcurl can parse, got ~S" text))
    (values (if negated 2 1)            ; CURL_TIMECOND_IFUNMODSINCE / IFMODSINCE
            ;; libcurl wants a Unix time; Lisp counts from 1900.
            (- universal (encode-universal-time 0 0 0 1 1 1970 0)))))

(defun connection-setopts (command)
  "The libcurl options behind the connection and transfer flags.

These reach libcurl through REQUEST's :SETOPTS rather than through a keyword
of its own for each, because they are settings of the transfer rather than
parts of the request, and there are a great many of them."
  (let ((plist '()))
    (flet ((add (option value) (setf plist (list* option value plist))))
      (let ((rate (clingon:getopt command :limit-rate)))
        (when rate
          ;; curl limits both directions with the one flag.
          (let ((bytes (parse-rate rate)))
            (add :max-recv-speed-large bytes)
            (add :max-send-speed-large bytes))))
      (when (clingon:getopt command :continue-at)
        (add :resume-from-large (clingon:getopt command :continue-at)))
      (when (clingon:getopt command :max-filesize)
        (add :maxfilesize-large (clingon:getopt command :max-filesize)))
      ;; CURL_IPRESOLVE_V4 is 1 and V6 is 2.  -6 wins if both are given, as in
      ;; curl, where the last one parsed decides and ours are ordered.
      (when (clingon:getopt command :ipv4) (add :ipresolve 1))
      (when (clingon:getopt command :ipv6) (add :ipresolve 2))
      (when (clingon:getopt command :interface)
        (add :interface (clingon:getopt command :interface)))
      (when (clingon:getopt command :resolve)
        (add :resolve (clingon:getopt command :resolve)))
      (when (clingon:getopt command :unix-socket)
        (add :unix-socket-path (clingon:getopt command :unix-socket)))
      (let ((cert (clingon:getopt command :cert)))
        (when cert
          ;; curl spells the passphrase cert:password, and a Windows path has
          ;; a colon in it, so only a colon after the second character counts.
          (let ((colon (position #\: cert :start 2)))
            (add :sslcert (if colon (subseq cert 0 colon) cert))
            (when colon (add :keypasswd (subseq cert (1+ colon)))))))
      (when (clingon:getopt command :key)
        (add :sslkey (clingon:getopt command :key)))
      (let ((condition (clingon:getopt command :time-cond)))
        (when condition
          (multiple-value-bind (sense seconds) (parse-time-condition condition)
            (add :timecondition sense)
            (add :timevalue-large seconds))))
      (when (clingon:getopt command :proxy-user)
        (add :proxyuserpwd (clingon:getopt command :proxy-user)))
      (when (clingon:getopt command :noproxy)
        (add :noproxy (clingon:getopt command :noproxy)))
      (when (clingon:getopt command :proxytunnel)
        (add :httpproxytunnel t)))
    plist))

(defun http-version-keyword (command)
  (cond ((clingon:getopt command :http1.0) :http/1.0)
        ((clingon:getopt command :http1.1) :http/1.1)
        ((clingon:getopt command :http2) :http/2)
        ((clingon:getopt command :http3) :http/3)
        (t nil)))

(defun request-options (command)
  "Every REQUEST option implied by the command line."
  (let* ((data (collect-data command))
         (forms (mapcar #'parse-form-part (clingon:getopt command :form)))
         (upload (clingon:getopt command :upload-file))
         (method (request-method command data forms upload))
         (get-style (clingon:getopt command :get)))
    (append
     (list :method method
           :follow-redirects (and (clingon:getopt command :location) t)
           :verbose (and (clingon:getopt command :verbose) t))
     (when (clingon:getopt command :header)
       (list :headers (clingon:getopt command :header)))
     ;; -G moves the data into the query string instead of the body.
     (when (and data (not get-style))
       (list :content data))
     (when forms (list :multipart forms))
     ;; Streamed, not buffered: -T on a large file must not need it in memory,
     ;; and "-" means standard input, which has no length to buffer by.
     (when upload
       (list :input (if (string= upload "-") (binary-stdin) upload)))
     (when (clingon:getopt command :max-redirs)
       (list :max-redirects (clingon:getopt command :max-redirs)))
     (when (clingon:getopt command :max-time)
       (list :timeout (clingon:getopt command :max-time)))
     (when (clingon:getopt command :connect-timeout)
       (list :connect-timeout (clingon:getopt command :connect-timeout)))
     (when (clingon:getopt command :user)
       (multiple-value-bind (user password) (split-once (clingon:getopt command :user) #\:)
         (list :basic-auth (cons user (or password "")))))
     (when (clingon:getopt command :oauth2-bearer)
       (list :bearer-auth (clingon:getopt command :oauth2-bearer)))
     (when (clingon:getopt command :user-agent)
       (list :user-agent (clingon:getopt command :user-agent)))
     (when (clingon:getopt command :referer)
       (list :referer (clingon:getopt command :referer)))
     (when (clingon:getopt command :cookie)
       (list :cookies (clingon:getopt command :cookie)))
     (when (clingon:getopt command :cookie-jar)
       (list :cookie-jar (clingon:getopt command :cookie-jar)))
     (when (clingon:getopt command :proxy)
       (list :proxy (clingon:getopt command :proxy)))
     (when (clingon:getopt command :range)
       (list :range (clingon:getopt command :range)))
     (when (clingon:getopt command :cacert)
       (list :ca-file (clingon:getopt command :cacert)))
     (when (clingon:getopt command :insecure) (list :verify-ssl :none))
     ;; curl sends no Accept-Encoding unless asked, so neither do we: an
     ;; explicit NIL suppresses the header the library would otherwise send.
     ;; Without this the sizes reported by --write-out would be compressed
     ;; ones, and would not match curl's.
     (list :accept-encoding (when (clingon:getopt command :compressed) ""))
     (when (http-version-keyword command)
       (list :http-version (http-version-keyword command)))
     (let ((retry (retry-specification command)))
       (when retry (list :retry retry)))
     (let ((setopts (connection-setopts command)))
       (when setopts (list :setopts setopts)))
     ;; The body is written as it arrives rather than buffered, so a large
     ;; download does not have to fit in memory.
     (list :force-binary t))))

;;; Running -------------------------------------------------------------------

(defun output-destination (command url index)
  "Where URL's body should go: a pathname, or NIL for standard output."
  (let ((outputs (clingon:getopt command :output)))
    (cond ((clingon:getopt command :remote-name) (url-filename url))
          ;; curl matches -o arguments to URLs positionally.
          ((nth index outputs) (nth index outputs))
          (t nil))))

(defun exit-code-for (condition)
  "The CURLcode CONDITION carries, or 2 for anything else -- as curl does."
  (if (and (typep condition 'curlcl:curl-error)
           (integerp (curlcl:curl-error-code condition)))
      (curlcl:curl-error-code condition)
      2))

(defun report-response (command response)
  "Do the after-the-fact parts: --write-out, then --fail.  Returns an exit code.

--write-out runs first and runs regardless, because curl expands it whatever
the transfer did -- `curl -f -w %{http_code}' prints 404 and exits 22, and a
script relying on that would otherwise get silence."
  (let ((format (clingon:getopt command :write-out)))
    (when format
      (write-string (expand-write-out format response) *standard-output*)
      (finish-output *standard-output*)))
  (let ((status (curlcl:response-status response)))
    (cond ((and (or (clingon:getopt command :fail)
                    ;; Same exit code, same message; the difference is only
                    ;; that MAKE-SINKS lets the body through.
                    (clingon:getopt command :fail-with-body))
                (<= 400 status))
           (unless (clingon:getopt command :silent)
             (message "The requested URL returned error: ~D" status))
           ;; CURLE_HTTP_RETURNED_ERROR, which is what curl --fail exits with.
           22)
          (t 0))))

;;; The output sink -----------------------------------------------------------
;;;
;;; A retried transfer delivers its body from the beginning, so whatever has
;;; already been written has to go.  The driver owns its destination outright --
;;; it opened the file, or it is standard output -- which is what lets it say
;;; :RETRY-STREAMED and mean it: on each retry it truncates the file by
;;; reopening it, and the sinks pick up the new stream because they ask the
;;; sink for it rather than closing over it.
;;;
;;; Standard output cannot be unwritten, and neither can curl's, so a retry
;;; there repeats whatever the failed attempt printed.  That is curl's
;;; behaviour and matching it is the point.

(defstruct (output-sink (:conc-name sink-))
  "Where one transfer's headers and body go, and how to start it over."
  stream
  ;; NIL when the destination is standard output, which must not be closed.
  destination)

(defun open-sink (destination)
  (make-output-sink
   :destination destination
   :stream (if destination
               (open destination :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
               (binary-stdout))))

(defun reset-sink (sink)
  "Discard what a failed attempt wrote, so the retry starts from nothing."
  (when (sink-destination sink)
    (close (sink-stream sink))
    (setf (sink-stream sink)
          (open (sink-destination sink) :direction :output
                                        :element-type '(unsigned-byte 8)
                                        :if-exists :supersede
                                        :if-does-not-exist :create)))
  sink)

(defun close-sink (sink)
  "Close a file sink, or flush standard output.  Errors propagate: a failure to
flush loses data, and the exit code has to say so."
  (if (sink-destination sink)
      (close (sink-stream sink))
      (finish-output (sink-stream sink))))

(defmacro with-output ((sink command url index) &body body)
  "Bind SINK to where this URL's output should go, closing it if it is a file."
  `(let ((,sink (open-sink (output-destination ,command ,url ,index))))
     (unwind-protect (progn ,@body)
       (close-sink ,sink))))

(defun parse-status-line (line)
  "The status code from an HTTP status line, or NIL if that is not one."
  (when (eql 0 (search "HTTP/" line))
    (let ((space (position #\Space line)))
      (when space (parse-integer line :start (1+ space) :junk-allowed t)))))

(defun make-sinks (command sink &optional dump)
  "Header and body callbacks for one transfer.  Returns (values header data status).

DUMP, when given, is the byte stream -D writes the response headers to.

STATUS is a cons whose car tracks the most recent response code, so --fail can
suppress output from the status line onward.

Doing the suppression here rather than with CURLOPT_FAILONERROR is deliberate.
Letting libcurl abort does keep the body out of the output, but it destroys
everything else --fail is expected to coexist with: there is no response left
to expand --write-out from, and no HTTP status for --retry to judge, so
`-f -w' printed nothing and `-f --retry' stopped retrying.  libcurl's own
documentation also warns that FAILONERROR is not fail-safe -- 401 and 407 slip
through when authentication is involved -- and it has nothing to say about
non-HTTP URLs.  Watching the status line covers all of those the same way."
  (let ((status (cons 0 nil))
        (failing (clingon:getopt command :fail))
        (include (or (clingon:getopt command :include)
                     (clingon:getopt command :head))))
    (flet ((suppressed-p () (and failing (<= 400 (car status)))))
      (values
       (lambda (line)
         (let ((code (parse-status-line line)))
           (when code (setf (car status) code)))
         ;; -D captures what arrived, whether or not --fail is hiding it from
         ;; the output: the file is there to be read afterwards, and a header
         ;; dump that silently empties itself on a 404 would be a poor way to
         ;; find out why.
         (when dump
           (write-sequence (curlcl::coerce-to-octets
                            (format nil "~A~C~C" line #\Return #\Newline))
                           dump))
         (when (and include (not (suppressed-p)))
           (write-sequence (curlcl::coerce-to-octets
                            (format nil "~A~C~C" line #\Return #\Newline))
                           ;; Asked for per write rather than closed over, so a
                           ;; reset between attempts is picked up here.
                           (sink-stream sink))))
       (lambda (octets)
         (unless (suppressed-p)
           (write-sequence octets (sink-stream sink))))
       status))))

(defmacro with-dump-header ((variable command index) &body body)
  "Bind VARIABLE to the stream -D dumps headers to, or NIL when it was not given.

Truncated for the first URL and appended to for the rest, so `-D h a b' ends
with both sets of headers in the order they arrived rather than only the last
-- which is what curl does, and the reason the index has to reach this far."
  (alexandria:once-only (command index)
    `(let ((path (clingon:getopt ,command :dump-header)))
       (if path
           (with-open-file (,variable path :direction :output
                                           :element-type '(unsigned-byte 8)
                                           :if-does-not-exist :create
                                           :if-exists (if (zerop ,index)
                                                          :supersede
                                                          :append))
             ,@body)
           (let ((,variable nil))
             ,@body)))))

(defun progress-reporter (command)
  "A progress callback for --progress-bar, or NIL."
  (when (and (clingon:getopt command :progress-bar)
             (not (clingon:getopt command :silent)))
    (let ((last -1))
      (lambda (download-total downloaded upload-total uploaded)
        (declare (ignore upload-total uploaded))
        (when (plusp download-total)
          (let ((percent (floor (* 100 downloaded) download-total)))
            (when (/= percent last)
              (setf last percent)
              (format *error-output* "~C[~vA~vA] ~3D%" #\Return
                      (floor percent 2) (make-string (floor percent 2)
                                                     :initial-element #\#)
                      (- 50 (floor percent 2)) ""
                      percent)
              (finish-output *error-output*))))
        t))))

(defun perform-one (command url index)
  "Fetch one URL.  Returns an exit code."
  (handler-case
      (with-output (sink command url index)
        (let* ((options (request-options command))
               ;; -G moves the body into the query string, which is text, so
               ;; this is the one place the octets have to become a string.
               (query (and (clingon:getopt command :get) (collect-data command)))
               (effective-url (if query
                                  (append-query url (curlcl::octets-to-string query))
                                  url))
               (progress (progress-reporter command)))
          (with-dump-header (dump command index)
           (multiple-value-bind (on-header on-data) (make-sinks command sink dump)
           (let ((response (apply #'curlcl:request effective-url
                                  :on-data on-data
                                  :on-header on-header
                                  ;; The body is streamed, so the library
                                  ;; refuses to retry it unless told the
                                  ;; redelivery is handled.  It is: ON-RETRY
                                  ;; throws away what the failed attempt wrote.
                                  :retry-streamed t
                                  :on-retry (lambda (attempt delay reason)
                                              (declare (ignore attempt delay reason))
                                              (reset-sink sink))
                                  (append
                                   (when progress (list :on-progress progress))
                                   options))))
             (when progress (format *error-output* "~%"))
             (report-response command response))))))
    (curlcl:curl-error (condition)
      (unless (and (clingon:getopt command :silent)
                   (not (clingon:getopt command :show-error)))
        (message "~A" condition))
      (exit-code-for condition))))

;;; Websockets ----------------------------------------------------------------
;;;
;;; This is the one place the driver goes beyond curl rather than following it.
;;; curl accepts a ws:// URL but has no interactive mode for it; the library
;;; underneath has the whole API, so the driver exposes it -- for the same
;;; reason -V reports which libcurl was loaded, which curl has no need to do.
;;;
;;; An easy handle must not be used from two threads at once, so the obvious
;;; shape -- a thread sending and a thread receiving -- is not available.  The
;;; reader thread therefore touches nothing but standard input and a queue,
;;; and the main loop owns the handle and does both halves.

(defconstant +ws-drain-seconds+ 2
  "How long to keep reading after standard input ends.

A reply is in flight when the last line is sent, so closing the moment the
queue empties throws it away.  The window ends early -- almost always
immediately -- when the server answers the close frame, so this is a bound
rather than a delay that is actually waited out.")

(defun websocket-url-p (url)
  "True for the schemes that mean a websocket."
  (let ((scheme (string-downcase (or (getf (curlcl:parse-url url) :scheme) ""))))
    (or (string= scheme "ws") (string= scheme "wss"))))

(defstruct (stdin-queue (:conc-name queue-))
  "Chunks read from standard input, waiting to be sent."
  (lock (bt:make-lock "curlcl ws stdin queue"))
  (items '())
  (eof nil))

(defun queue-push (queue item)
  (bt:with-lock-held ((queue-lock queue))
    (setf (queue-items queue) (append (queue-items queue) (list item)))))

(defun queue-pop (queue)
  (bt:with-lock-held ((queue-lock queue))
    (pop (queue-items queue))))

(defun queue-finished-p (queue)
  (bt:with-lock-held ((queue-lock queue))
    (and (queue-eof queue) (null (queue-items queue)))))

(defun start-stdin-reader (queue binary)
  "Read standard input into QUEUE until it ends.  Returns the thread.

Lines in text mode, because a line is what an interactive user means by a
message and the newline is a terminator rather than part of it; raw blocks in
binary mode, where there is no such convention to appeal to."
  (bt:make-thread
   (lambda ()
     (handler-case
         (if binary
             (let ((stream (binary-stdin))
                   (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
               (loop for n = (read-sequence buffer stream)
                     while (plusp n)
                     do (queue-push queue (subseq buffer 0 n))))
             (loop for line = (read-line *standard-input* nil nil)
                   while line
                   do (queue-push queue line)))
       ;; The queue still has to be closed, or the main loop waits forever for
       ;; input that can no longer arrive.
       (error () nil))
     (bt:with-lock-held ((queue-lock queue))
       (setf (queue-eof queue) t)))
   :name "curlcl ws stdin reader"))

(defun perform-websocket (command url)
  "Talk to a websocket, stdin to frames and frames to stdout.  Returns an exit code."
  (unless (curlcl:websockets-supported-p)
    (message "the loaded libcurl (~A) was built without websocket support"
             (curlcl:libcurl-version))
    ;; CURLE_UNSUPPORTED_PROTOCOL, which is what curl says for a scheme it
    ;; cannot speak.
    (return-from perform-websocket 1))
  (let* ((binary (clingon:getopt command :ws-binary))
         (queue (make-stdin-queue))
         (out (binary-stdout))
         (reader nil))
    (handler-case
        (unwind-protect
             (curlcl:with-websocket (handle url)
               (setf reader (start-stdin-reader queue binary))
               (let ((closing nil) (deadline nil))
                 (loop
                   (let ((item (queue-pop queue)))
                     (when item
                       (if binary
                           (curlcl:ws-send handle item :type :binary)
                           (curlcl:ws-send-text handle item))))
                   (multiple-value-bind (octets frame) (curlcl:ws-receive handle)
                     (cond
                       ((and frame (member :close (curlcl:frame-flags frame)))
                        (return 0))
                       (octets
                        (write-sequence octets out)
                        ;; A text frame carries no terminator, so one is added
                        ;; here or everything the server says runs together.
                        (when (and (not binary)
                                   (member :text (curlcl:frame-flags frame)))
                          (write-sequence (curlcl::coerce-to-octets
                                           (string #\Newline))
                                          out))
                        (finish-output out)
                        ;; Something arrived, so anything still coming is
                        ;; worth waiting for: restart the drain window.
                        (when closing
                          (setf deadline (+ (get-internal-real-time)
                                            (* +ws-drain-seconds+
                                               internal-time-units-per-second)))))
                       ;; Standard input has ended and everything queued has
                       ;; gone.  Not the same as being finished: the replies to
                       ;; what was just sent are still in flight, and closing
                       ;; here loses them -- which it did, silently, dropping
                       ;; every echo but the first.  Say goodbye, then keep
                       ;; reading until the server says it back or the window
                       ;; runs out.
                       ((and (queue-finished-p queue) (not closing))
                        (setf closing t
                              deadline (+ (get-internal-real-time)
                                          (* +ws-drain-seconds+
                                             internal-time-units-per-second)))
                        (curlcl:ws-close handle))
                       ((and closing (> (get-internal-real-time) deadline))
                        (return 0))
                       ;; Nothing either way; wait rather than spin on
                       ;; CURLE_AGAIN.
                       (t (sleep 0.02)))))))
          (when (and reader (bt:thread-alive-p reader))
            ;; It is blocked reading a descriptor that is not ours to close.
            (ignore-errors (bt:destroy-thread reader))))
      (curlcl:curl-error (condition)
        (unless (and (clingon:getopt command :silent)
                     (not (clingon:getopt command :show-error)))
          (message "~A" condition))
        (exit-code-for condition)))))

(defun perform-parallel (command urls)
  "Fetch every URL at once, for --parallel.  Returns an exit code."
  (let ((sinks (make-array (length urls) :initial-element nil))
        (open-p t)
        (worst 0))
    (unwind-protect
         ;; One dump file for the whole batch, opened once at index 0: the
         ;; transfers interleave, so per-transfer truncation would leave
         ;; whichever finished last and lose the rest.  Their headers arrive
         ;; mixed together, which is inherent to -Z and true of curl too.
         (with-dump-header (dump command 0)
         (let ((requests
                 (loop for url in urls
                       for index from 0
                       collect (let ((sink (open-sink (output-destination
                                                       command url index))))
                                 (setf (aref sinks index) sink)
                                 (multiple-value-bind (on-header on-data)
                                     (make-sinks command sink dump)
                                   (list* url :on-data on-data
                                          :on-header on-header
                                          :retry-streamed t
                                          (request-options command)))))))
           (loop for outcome in (curlcl:request-many
                                 requests
                                 ;; The batch hook rather than a per-request
                                 ;; one, because REQUEST-MANY reports retries
                                 ;; batch-wide -- and it passes the index, which
                                 ;; is exactly what says whose body is starting
                                 ;; over while several are interleaved.
                                 :on-retry (lambda (index attempt delay reason)
                                             (declare (ignore attempt delay reason))
                                             (reset-sink (aref sinks index)))
                                 :max-connections
                                 (or (clingon:getopt command :parallel-max) 8))
                 for url in urls
                 do (let ((code (if (typep outcome 'curlcl:response)
                                    (report-response command outcome)
                                    (progn
                                      (unless (clingon:getopt command :silent)
                                        (message "~A: ~A" url outcome))
                                      (exit-code-for outcome)))))
                      (when (plusp code) (setf worst code))))
           ;; Flushing and closing can fail -- a full disk, a quota, EPIPE --
           ;; and that failure loses data, so it has to reach the exit code
           ;; rather than vanish into IGNORE-ERRORS.  The single-transfer path
           ;; gets this for free from WITH-OPEN-FILE, whose close error
           ;; propagates; this one has to do it by hand.
           (loop for sink across sinks
                 when sink
                   do (handler-case (close-sink sink)
                        (error (condition)
                          (unless (clingon:getopt command :silent)
                            (message "~A" condition))
                          ;; CURLE_WRITE_ERROR, as curl reports for a failed
                          ;; write.
                          (setf worst 23))))
           (setf open-p nil)
           worst))
      ;; Only reached on a non-local exit; the normal path closed them above.
      (when open-p
        (loop for sink across sinks
              when sink do (ignore-errors (close-sink sink)))))))

;;; The command ---------------------------------------------------------------

(defun print-curl-version ()
  "Print a version banner in curl -V's shape."
  (format t "~A ~A (~A) libcurl/~A~@[ ~A~]~%"
          *program-name* *program-version*
          (or (curlcl::version-info-host (curlcl:libcurl-version-info)) "unknown")
          (curlcl:libcurl-version)
          (curlcl::version-info-ssl-version (curlcl:libcurl-version-info)))
  (format t "Release-Date: ~A~%" "unreleased")
  ;; Not something curl reports, but this binding can load any of several
  ;; libcurls -- and on macOS the one in the dyld shared cache differs from
  ;; Homebrew's in version, TLS backend and protocol support -- so saying which
  ;; one is in use turns a confusing class of bug into an obvious one.
  (format t "Library: ~A~%" (curlcl:libcurl-pathname))
  (format t "Protocols: ~{~A~^ ~}~%" (curlcl:libcurl-protocols))
  (format t "Features: ~{~A~^ ~}~%"
          (sort (mapcar #'string-downcase
                        (mapcar #'symbol-name (curlcl:libcurl-features)))
                #'string<)))

(defun options ()
  (list
   ;; Short name only: clingon installs its own --version, which prints the
   ;; bare version string.  -V prints the full banner, as curl's does.
   (clingon:make-option :flag :short-name #\V
                        :description "show libcurl version information and exit"
                        :key :show-version)
   (clingon:make-option :string :short-name #\X :long-name "request"
                        :description "specify request method to use"
                        :key :request)
   (clingon:make-option :list :short-name #\H :long-name "header"
                        :description "pass custom header(s) to server"
                        :key :header)
   (clingon:make-option :list :short-name #\d :long-name "data"
                        :description "HTTP POST data" :key :data)
   (clingon:make-option :list :long-name "data-binary"
                        :description "HTTP POST binary data" :key :data-binary)
   (clingon:make-option :list :long-name "data-raw"
                        :description "HTTP POST data, '@' allowed"
                        :key :data-raw)
   (clingon:make-option :list :long-name "data-urlencode"
                        :description "HTTP POST data URL encoded"
                        :key :data-urlencode)
   (clingon:make-option :list :short-name #\F :long-name "form"
                        :description "specify multipart form data" :key :form)
   (clingon:make-option :flag :short-name #\G :long-name "get"
                        :description "put the post data in the URL and use GET"
                        :key :get)
   (clingon:make-option :list :short-name #\o :long-name "output"
                        :description "write to file instead of stdout" :key :output)
   (clingon:make-option :flag :short-name #\O :long-name "remote-name"
                        :description "write output to a file named as the remote file"
                        :key :remote-name)
   (clingon:make-option :string :short-name #\T :long-name "upload-file"
                        :description "transfer local FILE to destination (- for stdin)"
                        :key :upload-file)
   (clingon:make-option :string :short-name #\u :long-name "user"
                        :description "server user and password" :key :user)
   (clingon:make-option :string :short-name #\A :long-name "user-agent"
                        :description "send User-Agent to server" :key :user-agent)
   (clingon:make-option :string :short-name #\e :long-name "referer"
                        :description "referrer URL" :key :referer)
   (clingon:make-option :string :short-name #\b :long-name "cookie"
                        :description "send cookies from string" :key :cookie)
   (clingon:make-option :string :short-name #\c :long-name "cookie-jar"
                        :description "write cookies to FILE after operation"
                        :key :cookie-jar)
   (clingon:make-option :flag :short-name #\L :long-name "location"
                        :description "follow redirects" :key :location)
   (clingon:make-option :integer :long-name "max-redirs"
                        :description "maximum number of redirects allowed"
                        :key :max-redirs)
   (clingon:make-option :integer :short-name #\m :long-name "max-time"
                        :description "maximum time allowed for the transfer"
                        :key :max-time)
   (clingon:make-option :integer :long-name "connect-timeout"
                        :description "maximum time allowed for connection"
                        :key :connect-timeout)
   (clingon:make-option :flag :short-name #\k :long-name "insecure"
                        :description "allow insecure server connections"
                        :key :insecure)
   (clingon:make-option :string :long-name "cacert"
                        :description "CA certificate to verify peer against"
                        :key :cacert)
   (clingon:make-option :string :short-name #\E :long-name "cert"
                        :description "client certificate file (CERT[:PASSWORD])"
                        :key :cert)
   (clingon:make-option :string :long-name "key"
                        :description "private key file name" :key :key)
   (clingon:make-option :string :long-name "limit-rate"
                        :description "limit transfer speed to RATE (e.g. 200K)"
                        :key :limit-rate)
   (clingon:make-option :integer :short-name #\C :long-name "continue-at"
                        :description "resume transfer at byte OFFSET"
                        :key :continue-at)
   (clingon:make-option :integer :long-name "max-filesize"
                        :description "maximum file size to download"
                        :key :max-filesize)
   (clingon:make-option :flag :short-name #\4 :long-name "ipv4"
                        :description "resolve names to IPv4 addresses"
                        :key :ipv4)
   (clingon:make-option :flag :short-name #\6 :long-name "ipv6"
                        :description "resolve names to IPv6 addresses"
                        :key :ipv6)
   (clingon:make-option :string :long-name "interface"
                        :description "use network INTERFACE" :key :interface)
   (clingon:make-option :list :long-name "resolve"
                        :description "resolve HOST:PORT to ADDRESS"
                        :key :resolve)
   (clingon:make-option :string :long-name "unix-socket"
                        :description "connect through this Unix domain socket"
                        :key :unix-socket)
   (clingon:make-option :flag :long-name "ws-binary"
                        :description "send websocket frames as binary, not text"
                        :key :ws-binary)
   (clingon:make-option :string :short-name #\z :long-name "time-cond"
                        :description "transfer only if changed since TIME"
                        :key :time-cond)
   (clingon:make-option :string :short-name #\U :long-name "proxy-user"
                        :description "proxy user and password" :key :proxy-user)
   (clingon:make-option :string :long-name "noproxy"
                        :description "hosts which do not use a proxy"
                        :key :noproxy)
   (clingon:make-option :flag :short-name #\p :long-name "proxytunnel"
                        :description "tunnel through the HTTP proxy"
                        :key :proxytunnel)
   (clingon:make-option :string :short-name #\x :long-name "proxy"
                        :description "use this proxy" :key :proxy)
   (clingon:make-option :string :short-name #\r :long-name "range"
                        :description "retrieve only bytes within RANGE" :key :range)
   (clingon:make-option :flag :short-name #\I :long-name "head"
                        :description "show document info only" :key :head)
   (clingon:make-option :flag :short-name #\i :long-name "include"
                        :description "include protocol response headers in the output"
                        :key :include)
   (clingon:make-option :flag :short-name #\s :long-name "silent"
                        :description "silent mode" :key :silent)
   (clingon:make-option :flag :short-name #\S :long-name "show-error"
                        :description "show an error even when -s is used"
                        :key :show-error)
   (clingon:make-option :flag :short-name #\v :long-name "verbose"
                        :description "make the operation more talkative" :key :verbose)
   (clingon:make-option :flag :short-name #\f :long-name "fail"
                        :description "fail fast with no output on HTTP errors"
                        :key :fail)
   (clingon:make-option :string :short-name #\w :long-name "write-out"
                        :description "use output FORMAT after completion"
                        :key :write-out)
   (clingon:make-option :flag :long-name "compressed"
                        :description "request compressed response" :key :compressed)
   (clingon:make-option :flag :short-name #\# :long-name "progress-bar"
                        :description "display transfer progress as a bar"
                        :key :progress-bar)
   (clingon:make-option :integer :long-name "retry"
                        :description "retry request if transient problems occur"
                        :key :retry)
   (clingon:make-option :integer :long-name "retry-delay"
                        :description "wait time between retries" :key :retry-delay)
   (clingon:make-option :integer :long-name "retry-max-time"
                        :description "retry only within this many seconds"
                        :key :retry-max-time)
   (clingon:make-option :flag :long-name "retry-all-errors"
                        :description "retry all errors, not just transient ones"
                        :key :retry-all-errors)
   (clingon:make-option :flag :long-name "retry-connrefused"
                        :description "retry on connection refused"
                        :key :retry-connrefused)
   (clingon:make-option :string :short-name #\D :long-name "dump-header"
                        :description "write response headers to FILE"
                        :key :dump-header)
   (clingon:make-option :string :long-name "oauth2-bearer"
                        :description "OAuth 2 Bearer Token" :key :oauth2-bearer)
   (clingon:make-option :flag :long-name "fail-with-body"
                        :description "fail on HTTP errors but save the body"
                        :key :fail-with-body)
   (clingon:make-option :flag :short-name #\Z :long-name "parallel"
                        :description "perform transfers in parallel" :key :parallel)
   (clingon:make-option :integer :long-name "parallel-max"
                        :description "maximum concurrency for parallel transfers"
                        :key :parallel-max)
   (clingon:make-option :flag :long-name "http1.0" :description "use HTTP 1.0"
                        :key :http1.0)
   (clingon:make-option :flag :long-name "http1.1" :description "use HTTP 1.1"
                        :key :http1.1)
   (clingon:make-option :flag :long-name "http2" :description "use HTTP 2"
                        :key :http2)
   (clingon:make-option :flag :long-name "http3" :description "use HTTP 3"
                        :key :http3)))

(defun handler (command)
  (when (clingon:getopt command :show-version)
    (print-curl-version)
    (clingon:exit 0))
  (let ((urls (clingon:command-arguments command)))
    (when (null urls)
      (message "no URL specified")
      (message "try '~A --help' for more information" *program-name*)
      (clingon:exit 2))
    (clingon:exit
     (cond
       ;; A websocket is a conversation rather than a transfer, so it does not
       ;; go through the -Z batch or the sink machinery.  One URL only: two
       ;; interactive sessions sharing a standard input has no useful meaning.
       ((some #'websocket-url-p urls)
        (cond ((rest urls)
               (message "only one websocket URL at a time")
               2)
              ((clingon:getopt command :parallel)
               (message "--parallel does not apply to a websocket")
               2)
              (t (perform-websocket command (first urls)))))
       ((clingon:getopt command :parallel)
        (perform-parallel command urls))
       (t
        (loop with worst = 0
              for url in urls
              for index from 0
              for code = (perform-one command url index)
              do (when (plusp code) (setf worst code))
              finally (return worst)))))))

(defun command ()
  (clingon:make-command
   :name *program-name*
   :description "transfer a URL, in the manner of curl(1)"
   :long-description
   (format nil "A curl-compatible client built on the libcurl Common Lisp ~
binding.  Option names, defaults and exit codes follow curl(1), so most curl ~
command lines work unchanged; exit codes are libcurl's own CURLcode values.~%~%~
One deliberate difference: there is no progress meter unless --progress-bar ~
is given.")
   :authors '("Matthew Kennedy <burnsidemk@gmail.com>")
   :license "MIT"
   :version *program-version*
   :usage "[options...] <url>..."
   :options (options)
   :handler #'handler))

(defun main ()
  "Entry point for the bin/curlcl executable."
  (clingon:run (command)))
