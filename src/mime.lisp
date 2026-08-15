;;;; src/mime.lisp — multipart bodies.
;;;;
;;;; Only curl_mime_* is bound.  The older curl_formadd interface is deprecated
;;;; on every one of its enumerators as of 7.56.0, and it is variadic with a
;;;; sentinel-terminated option list -- which would mean a second, worse
;;;; variadic call path for an API libcurl itself tells you not to use.
;;;;
;;;; Lifetime is the thing to get right.  A curl_mime belongs to the easy
;;;; handle it was created from, must outlive the transfer that posts it, and
;;;; must be freed with curl_mime_free after curl_easy_cleanup -- so it is
;;;; recorded against the handle like any other borrowed resource.  Parts are
;;;; owned by their mime and are never freed individually.

(in-package #:curlcl)

;;; curl_mime_free is declared in easy-raw.lisp, where the resource sweep that
;;; calls it can see it.
(cffi:defcfun ("curl_mime_init" %curl-mime-init) :pointer (handle :pointer))
(cffi:defcfun ("curl_mime_addpart" %curl-mime-addpart) :pointer (mime :pointer))
(cffi:defcfun ("curl_mime_name" %curl-mime-name) :int
  (part :pointer) (name :string))
(cffi:defcfun ("curl_mime_filename" %curl-mime-filename) :int
  (part :pointer) (filename :string))
(cffi:defcfun ("curl_mime_type" %curl-mime-type) :int
  (part :pointer) (type :string))
(cffi:defcfun ("curl_mime_encoder" %curl-mime-encoder) :int
  (part :pointer) (encoding :string))
(cffi:defcfun ("curl_mime_data" %curl-mime-data) :int
  (part :pointer) (data :pointer) (size :size))
(cffi:defcfun ("curl_mime_filedata" %curl-mime-filedata) :int
  (part :pointer) (filename :string))
(cffi:defcfun ("curl_mime_subparts" %curl-mime-subparts) :int
  (part :pointer) (subparts :pointer))
(cffi:defcfun ("curl_mime_headers" %curl-mime-headers) :int
  (part :pointer) (headers :pointer) (take-ownership :int))

(defgeneric mime-pointer (mime)
  (:documentation "The raw curl_mime* this object wraps."))

(defgeneric mime-freed-p (mime)
  (:documentation
   "True once the mime structure has been released.

A mime belongs to the easy handle it was created from and is freed with it, so
this can become true without FREE-MIME having been called directly."))

(defclass mime ()
  ((pointer :initarg :pointer :reader mime-pointer)
   (handle :initarg :handle :reader mime-handle
           :documentation "The easy handle this was created from; a mime cannot
outlive it.")
   (freed-p :accessor mime-freed-p :initform nil))
  (:documentation "A multipart body under construction."))

(defmethod print-object ((mime mime) stream)
  (print-unreadable-object (mime stream :type t :identity t)
    (when (mime-freed-p mime) (write-string "freed" stream))))

(defun make-mime (handle)
  "Start a multipart body for HANDLE.

The result is registered with HANDLE and released when it is closed, so there
is nothing to free by hand in the common case where the mime is posted and the
handle then goes away."
  (check-open handle)
  (let ((pointer (%curl-mime-init (handle-pointer handle))))
    (when (cffi:null-pointer-p pointer)
      (error 'easy-error :message "curl_mime_init returned NULL"))
    (let ((mime (make-instance 'mime :pointer pointer :handle handle)))
      ;; Freed after curl_easy_cleanup, in the handle's resource sweep.
      (own-resource (handle-resources handle) :mime pointer)
      mime)))

(defun add-mime-part (mime &key name filename content-type encoding
                                data file headers subparts)
  "Add one part to MIME.  Returns the part pointer, which MIME owns.

  NAME          the form field name
  FILENAME      the filename to report, for a file part
  CONTENT-TYPE  overrides libcurl's guess
  ENCODING      a transfer encoding such as \"base64\"
  DATA          octets or a string, sent inline
  FILE          a pathname, streamed from disk at transfer time
  HEADERS       extra headers for this part, as a list of strings
  SUBPARTS      another MIME, nested as multipart/mixed

DATA is copied by libcurl, so the Lisp object need not outlive this call."
  (let ((part (%curl-mime-addpart (mime-pointer mime))))
    (when (cffi:null-pointer-p part)
      (error 'easy-error :message "curl_mime_addpart returned NULL"))
    (flet ((check (code) (%check-easy code)))
      (when name (check (%curl-mime-name part name)))
      (when filename (check (%curl-mime-filename part filename)))
      (when content-type (check (%curl-mime-type part content-type)))
      (when encoding (check (%curl-mime-encoder part encoding)))
      (when data
        (let ((octets (coerce-to-octets data)))
          ;; curl_mime_data copies, so a pointer into the Lisp heap is safe for
          ;; the duration of the call.
          (cffi:with-pointer-to-vector-data (pointer
                                             (if (plusp (length octets))
                                                 octets
                                                 (make-array 1 :element-type
                                                             '(unsigned-byte 8))))
            (check (%curl-mime-data part pointer (length octets))))))
      (when file
        (check (%curl-mime-filedata part (uiop:native-namestring file))))
      (when headers
        ;; take-ownership 0 means libcurl copies rather than adopting, so the
        ;; slist stays ours and is released with the handle.
        (check (%curl-mime-headers
                part
                (own-slist (handle-resources (mime-handle mime))
                           (mapcar #'string headers))
                0)))
      (when subparts
        (check (%curl-mime-subparts part (mime-pointer subparts)))
        ;; libcurl takes ownership of a mime attached as subparts, so it must
        ;; not also be freed by the handle's resource sweep.
        (disown-mime subparts)))
    part))

(defun disown-mime (mime)
  "Stop tracking MIME for release, because libcurl has taken ownership."
  (let ((resources (handle-resources (mime-handle mime))))
    (setf (resources-items resources)
          (remove-if (lambda (item)
                       (and (eq :mime (car item))
                            (cffi:pointer-eq (cdr item) (mime-pointer mime))))
                     (resources-items resources))))
  (setf (mime-freed-p mime) t)
  mime)

(defun attach-mime (handle mime)
  "Post MIME as HANDLE's request body."
  (setopt handle :mimepost (mime-pointer mime))
  handle)

(defun set-mime-body (handle parts)
  "Build a multipart body from PARTS and attach it to HANDLE.

Each part is a plist of the keywords ADD-MIME-PART accepts:

  (set-mime-body h '((:name \"field\" :data \"value\")
                     (:name \"upload\" :file #p\"/tmp/x.png\"
                      :content-type \"image/png\")))"
  (let ((mime (make-mime handle)))
    (dolist (part parts)
      (apply #'add-mime-part mime part))
    (attach-mime handle mime)
    mime))

;;; Streaming parts -----------------------------------------------------------
;;;
;;; curl_mime_data_cb takes a read callback, an optional seek callback, and one
;;; userdata pointer shared between them.  A per-handle registry key will not do
;;; here: a message can have several streaming parts, each needing its own
;;; reader.  So each part gets a registry key of its own, holding a
;;; CALLBACK-STATE whose read slot is that part's function -- which lets the
;;; existing read trampoline serve it unchanged.  The key is recorded against
;;; the handle and released with it.

(cffi:defcfun ("curl_mime_data_cb" %curl-mime-data-cb) :int
  (part :pointer) (datasize curl-off-t)
  (read-function :pointer) (seek-function :pointer) (free-function :pointer)
  (argument :pointer))

(defun add-streaming-mime-part (mime &key name filename content-type encoding
                                          headers size reader seeker)
  "Add a part whose data comes from READER, called during the transfer.

READER takes a maximum byte count and returns octets, a string, or :EOF, with
the same contract as a read callback.  SIZE is the total size of the part; -1
means unknown, which forces chunked encoding.  SEEKER, if given, takes an
offset and a whence keyword and lets libcurl rewind the part for a redirect or
an authentication retry.

Streaming rather than buffering matters for a part that does not fit in memory
or is not available yet; a part that is already in hand should use :DATA."
  (let* ((state (make-callback-state))
         (part (%curl-mime-addpart (mime-pointer mime))))
    (when (cffi:null-pointer-p part)
      (error 'easy-error :message "curl_mime_addpart returned NULL"))
    (setf (cb-read state) reader
          (cb-seek state) seeker
          (cb-handle state) (mime-handle mime))
    (register-callback-state state)
    ;; Released when the handle is, alongside the mime itself.
    (own-resource (handle-resources (mime-handle mime)) :callback-key
                  (callback-key-pointer (cb-key state)))
    (flet ((check (code) (%check-easy code)))
      (when name (check (%curl-mime-name part name)))
      (when filename (check (%curl-mime-filename part filename)))
      (when content-type (check (%curl-mime-type part content-type)))
      (when encoding (check (%curl-mime-encoder part encoding)))
      (when headers
        (check (%curl-mime-headers
                part
                (own-slist (handle-resources (mime-handle mime))
                           (mapcar #'string headers))
                0)))
      (check (%curl-mime-data-cb
              part
              (or size -1)
              (cffi:get-callback '%read-trampoline)
              (if seeker (cffi:get-callback '%seek-trampoline) (cffi:null-pointer))
              ;; No free callback: the state is owned by the handle, not by
              ;; libcurl, and releasing it here would free it twice.
              (cffi:null-pointer)
              (callback-key-pointer (cb-key state)))))
    part))
