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
;;;;   --upload-file reads the file into memory rather than streaming it.  The
;;;;   library supports streaming uploads through a read callback; the CLI does
;;;;   not yet wire it up.

(defpackage #:libcurl/cli
  (:use #:cl)
  (:export #:main))

(in-package #:libcurl/cli)

(defparameter *program-name* "curlcl")
(defparameter *program-version* "0.1.0")

;;; Output --------------------------------------------------------------------

(defun binary-stdout ()
  "A byte stream on file descriptor 1.

Response bodies are octets and may be anything at all, so they go to the file
descriptor rather than through *STANDARD-OUTPUT*, which would try to encode
them."
  #+sbcl (sb-sys:make-fd-stream 1 :output t :element-type '(unsigned-byte 8)
                                  :buffering :full)
  #-sbcl (error "No binary standard output on this implementation."))

(defun message (control &rest arguments)
  "Write a diagnostic to standard error, as curl does."
  (format *error-output* "~&~A: ~?~%" *program-name* control arguments)
  (finish-output *error-output*))

(defun url-filename (url)
  "The last path segment of URL, for --remote-name."
  (let* ((parts (libcurl:parse-url url))
         (path (or (getf parts :path) "/"))
         (slash (position #\/ path :from-end t))
         (name (if slash (subseq path (1+ slash)) path)))
    (if (plusp (length name)) name "index.html")))

;;; --write-out ---------------------------------------------------------------

(defun write-out-value (variable response)
  "The value of a --write-out %{variable}, or NIL if we do not know it."
  (let ((timings (libcurl:response-timings response)))
    (flet ((seconds (key) (/ (or (getf timings key) 0) 1000000.0d0)))
      (cond
        ((string= variable "http_code") (libcurl:response-status response))
        ((string= variable "response_code") (libcurl:response-status response))
        ((string= variable "url_effective") (libcurl:response-url response))
        ((string= variable "content_type")
         (or (libcurl:response-content-type response) ""))
        ((string= variable "num_redirects") (libcurl:response-redirect-count response))
        ;; From getinfo, not the body: with -o the body was streamed to a file
        ;; and never buffered, so its length would be zero.
        ((string= variable "size_download") (libcurl:response-size-download response))
        ((string= variable "size_upload") (libcurl:response-size-upload response))
        ;; curl prints the bare version -- "2", "1.1" -- not the keyword.
        ((string= variable "http_version")
         (case (libcurl:response-version response)
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

(defun collect-data (command)
  "Join every -d and --data-binary argument with & as curl does."
  (let ((pieces (append (clingon:getopt command :data)
                        (clingon:getopt command :data-binary))))
    (when pieces
      (format nil "~{~A~^&~}" pieces))))

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
  (libcurl:with-url (parsed url)
    (setf (libcurl:url-part parsed :query :append-query) data)
    (libcurl:url-string parsed)))

(defun retry-specification (command)
  (let ((count (clingon:getopt command :retry)))
    (when (and count (plusp count))
      ;; curl's --retry N means N retries after the first attempt.
      (list :max-attempts (1+ count)
            :initial-delay (float (or (clingon:getopt command :retry-delay) 1) 1d0)
            ;; curl retries whatever it was asked to, including POST.
            :non-idempotent t))))

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
     (when upload
       (list :content (with-open-file (in upload :element-type '(unsigned-byte 8))
                        (let ((octets (make-array (file-length in)
                                                  :element-type '(unsigned-byte 8))))
                          (read-sequence octets in)
                          octets))))
     (when (clingon:getopt command :max-redirs)
       (list :max-redirects (clingon:getopt command :max-redirs)))
     (when (clingon:getopt command :max-time)
       (list :timeout (clingon:getopt command :max-time)))
     (when (clingon:getopt command :connect-timeout)
       (list :connect-timeout (clingon:getopt command :connect-timeout)))
     (when (clingon:getopt command :user)
       (multiple-value-bind (user password) (split-once (clingon:getopt command :user) #\:)
         (list :basic-auth (cons user (or password "")))))
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
  (if (and (typep condition 'libcurl:curl-error)
           (integerp (libcurl:curl-error-code condition)))
      (libcurl:curl-error-code condition)
      2))

(defun report-response (command response)
  "Do the after-the-fact parts: --fail, --write-out.  Returns an exit code."
  (let ((status (libcurl:response-status response)))
    (cond
      ((and (clingon:getopt command :fail) (<= 400 status))
       (unless (clingon:getopt command :silent)
         (message "The requested URL returned error: ~D" status))
       ;; CURLE_HTTP_RETURNED_ERROR, which is what curl --fail exits with.
       22)
      (t
       (let ((format (clingon:getopt command :write-out)))
         (when format
           (write-string (expand-write-out format response) *standard-output*)
           (finish-output *standard-output*)))
       0))))

(defmacro with-output ((stream command url index) &body body)
  "Bind STREAM to where this URL's body should go, closing it if it is a file."
  (alexandria:with-gensyms (destination file)
    `(let* ((,destination (output-destination ,command ,url ,index))
            (,file ,destination))
       (if ,file
           (with-open-file (,stream ,file :direction :output
                                          :element-type '(unsigned-byte 8)
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
             ,@body)
           (let ((,stream (binary-stdout)))
             (unwind-protect (progn ,@body)
               (finish-output ,stream)))))))

(defun header-writer (command stream)
  "A header callback that writes header lines to STREAM, for --include."
  (when (or (clingon:getopt command :include) (clingon:getopt command :head))
    (lambda (line)
      (write-sequence (libcurl::coerce-to-octets (format nil "~A~C~C" line
                                                         #\Return #\Newline))
                      stream))))

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
      (with-output (stream command url index)
        (let* ((options (request-options command))
               (effective-url (if (and (clingon:getopt command :get)
                                       (collect-data command))
                                  (append-query url (collect-data command))
                                  url))
               (progress (progress-reporter command))
               (response (apply #'libcurl:request effective-url
                                :output stream
                                :on-header (header-writer command stream)
                                (append
                                 (when progress (list :on-progress progress))
                                 options))))
          (when progress (format *error-output* "~%"))
          (report-response command response)))
    (libcurl:curl-error (condition)
      (unless (and (clingon:getopt command :silent)
                   (not (clingon:getopt command :show-error)))
        (message "~A" condition))
      (exit-code-for condition))))

(defun perform-parallel (command urls)
  "Fetch every URL at once, for --parallel.  Returns an exit code."
  (let ((streams '())
        (worst 0))
    (unwind-protect
         (let ((requests
                 (loop for url in urls
                       for index from 0
                       collect (let* ((destination (output-destination command url index))
                                      (stream (if destination
                                                  (open destination
                                                        :direction :output
                                                        :element-type '(unsigned-byte 8)
                                                        :if-exists :supersede
                                                        :if-does-not-exist :create)
                                                  (binary-stdout))))
                                 (push stream streams)
                                 (list* url :output stream
                                        :on-header (header-writer command stream)
                                        (request-options command))))))
           (loop for outcome in (libcurl:request-many
                                 requests
                                 :max-connections
                                 (or (clingon:getopt command :parallel-max) 8))
                 for url in urls
                 do (let ((code (if (typep outcome 'libcurl:response)
                                    (report-response command outcome)
                                    (progn
                                      (unless (clingon:getopt command :silent)
                                        (message "~A: ~A" url outcome))
                                      (exit-code-for outcome)))))
                      (when (plusp code) (setf worst code))))
           worst)
      (dolist (stream streams) (ignore-errors (finish-output stream))))))

;;; The command ---------------------------------------------------------------

(defun print-curl-version ()
  "Print a version banner in curl -V's shape."
  (format t "~A ~A (~A) libcurl/~A~@[ ~A~]~%"
          *program-name* *program-version*
          (or (libcurl::version-info-host (libcurl:libcurl-version-info)) "unknown")
          (libcurl:libcurl-version)
          (libcurl::version-info-ssl-version (libcurl:libcurl-version-info)))
  (format t "Release-Date: ~A~%" "unreleased")
  ;; Not something curl reports, but this binding can load any of several
  ;; libcurls -- and on macOS the one in the dyld shared cache differs from
  ;; Homebrew's in version, TLS backend and protocol support -- so saying which
  ;; one is in use turns a confusing class of bug into an obvious one.
  (format t "Library: ~A~%" (libcurl:libcurl-pathname))
  (format t "Protocols: ~{~A~^ ~}~%" (libcurl:libcurl-protocols))
  (format t "Features: ~{~A~^ ~}~%"
          (sort (mapcar #'string-downcase
                        (mapcar #'symbol-name (libcurl:libcurl-features)))
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
                        :description "transfer local FILE to destination"
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
   (clingon:make-option :flag :long-name "progress-bar"
                        :description "display transfer progress as a bar"
                        :key :progress-bar)
   (clingon:make-option :integer :long-name "retry"
                        :description "retry request if transient problems occur"
                        :key :retry)
   (clingon:make-option :integer :long-name "retry-delay"
                        :description "wait time between retries" :key :retry-delay)
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
     (if (clingon:getopt command :parallel)
         (perform-parallel command urls)
         (loop with worst = 0
               for url in urls
               for index from 0
               for code = (perform-one command url index)
               do (when (plusp code) (setf worst code))
               finally (return worst))))))

(defun command ()
  (clingon:make-command
   :name *program-name*
   :description "transfer a URL, in the manner of curl(1)"
   :long-description
   (format nil "A curl-compatible client built on the libcurl Common Lisp ~
binding.  Option names, defaults and exit codes follow curl(1), so most curl ~
command lines work unchanged; exit codes are libcurl's own CURLcode values.~%~%~
Two deliberate differences: there is no progress meter unless --progress-bar ~
is given, and --upload-file reads the file into memory rather than streaming ~
it.")
   :authors '("Matthew Kennedy <burnsidemk@gmail.com>")
   :license "MIT"
   :version *program-version*
   :usage "[options...] <url>..."
   :options (options)
   :handler #'handler))

(defun main ()
  "Entry point for the bin/curlcl executable."
  (clingon:run (command)))
