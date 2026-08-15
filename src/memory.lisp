;;;; src/memory.lisp — foreign memory a handle owns, and octet conversion.
;;;;
;;;; libcurl's copying rules are not uniform, and the exceptions are the ones
;;;; that bite:
;;;;
;;;;   CURLOPT_* string options are copied, so a temporary is fine.
;;;;   CURLOPT_POSTFIELDS is NOT -- libcurl keeps the pointer and reads from it
;;;;     during the transfer.  That is the entire reason CURLOPT_COPYPOSTFIELDS
;;;;     exists.  Handing it a WITH-FOREIGN-STRING pointer posts freed memory.
;;;;   CURLOPT_ERRORBUFFER is a buffer the caller owns and must keep alive for
;;;;     the whole transfer, and it must be at least CURL_ERROR_SIZE.
;;;;   struct curl_slist chains and CURL_BLOB_NOCOPY blobs are likewise
;;;;     borrowed, not copied.
;;;;
;;;; So anything in that second group is allocated here, recorded against the
;;;; handle that borrowed it, and released only after curl_easy_cleanup has
;;;; returned.  Ordering matters: freeing before cleanup frees memory libcurl
;;;; is still reading.

(in-package #:libcurl)

;;; Octets <-> foreign buffers ------------------------------------------------

(defun foreign-to-octets (pointer length)
  "Copy LENGTH bytes at POINTER into a fresh octet vector.

The bulk copy on SBCL is not premature optimisation: this runs once per
received chunk, so a per-byte loop would show up on any real download."
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    #+sbcl
    (when (plusp length)
      (sb-sys:with-pinned-objects (octets)
        (sb-kernel:system-area-ub8-copy
         (sb-sys:int-sap (cffi:pointer-address pointer)) 0
         (sb-sys:vector-sap octets) 0 length)))
    #-sbcl
    (dotimes (i length)
      (setf (aref octets i) (cffi:mem-aref pointer :uint8 i)))
    octets))

(defun octets-to-foreign (octets pointer &key (start 0) (end (length octets)))
  "Copy OCTETS[START:END] to POINTER.  Returns the number of bytes written."
  (let ((length (- end start)))
    #+sbcl
    (when (plusp length)
      (sb-sys:with-pinned-objects (octets)
        (sb-kernel:system-area-ub8-copy
         (sb-sys:vector-sap octets) start
         (sb-sys:int-sap (cffi:pointer-address pointer)) 0 length)))
    #-sbcl
    (loop for i from 0 below length
          do (setf (cffi:mem-aref pointer :uint8 i) (aref octets (+ start i))))
    length))

(defun octets-to-string (octets &key (encoding :utf-8) (start 0)
                                     (end (length octets)))
  "Decode OCTETS[START:END] as text.

Goes through a foreign buffer because CFFI's decoders work on foreign memory;
the buffer is stack-allocated and released on the way out, which the obvious
FOREIGN-ALLOC spelling of this would not be."
  (let ((length (- end start)))
    (if (zerop length)
        ""
        (cffi:with-foreign-object (buffer :uint8 length)
          (octets-to-foreign octets buffer :start start :end end)
          (cffi:foreign-string-to-lisp buffer :count length :encoding encoding)))))

(defun coerce-to-octets (data &key (encoding :utf-8))
  "Octets for DATA, which may already be octets, or a string, or a character."
  (etypecase data
    ((array (unsigned-byte 8) (*)) data)
    (string (cffi:with-foreign-string ((pointer length) data
                                       :encoding encoding :null-terminated-p nil)
              (foreign-to-octets pointer length)))))

;;; Owned resources -----------------------------------------------------------
;;;
;;; A flat list of (kind . pointer), released newest first.  A list rather than
;;; anything cleverer because the counts are tiny -- a handle owns a handful of
;;; strings and slists -- and because release order is the only property that
;;; matters.

(defstruct (foreign-resources (:conc-name resources-))
  (items '() :type list))

(defun own-resource (resources kind pointer)
  "Record POINTER as owned, and return it."
  (push (cons kind pointer) (resources-items resources))
  pointer)

(defun release-resources (resources)
  "Free everything RESOURCES owns.  Safe to call twice.

Must not run until curl_easy_cleanup has returned: until then libcurl may
still be reading the very buffers being freed here."
  (dolist (item (resources-items resources))
    (destructuring-bind (kind . pointer) item
      (unless (cffi:null-pointer-p pointer)
        (ecase kind
          ((:foreign) (cffi:foreign-free pointer))
          ;; An slist is a chain libcurl allocated one node at a time; only its
          ;; own free walks it.
          ((:slist) (%curl-slist-free-all pointer))
          ;; A blob's payload is a separate allocation from the struct.
          ((:blob) (let ((data (cffi:foreign-slot-value
                                pointer '(:struct curl-blob) 'data)))
                     (unless (cffi:null-pointer-p data)
                       (cffi:foreign-free data)))
                   (cffi:foreign-free pointer))
          ;; A curl_mime must be freed after curl_easy_cleanup, which is
          ;; exactly when this sweep runs.
          ((:mime) (%curl-mime-free pointer))))))
  (setf (resources-items resources) '())
  (values))

(defun own-foreign-string (resources string &key (encoding :utf-8))
  "Allocate a NUL-terminated copy of STRING that outlives the call."
  (own-resource resources :foreign
                (cffi:foreign-string-alloc string :encoding encoding)))

(defun own-octets (resources octets)
  "Allocate a copy of OCTETS, returning (values pointer length).

Used for CURLOPT_POSTFIELDS, which borrows rather than copies."
  (let* ((length (length octets))
         ;; Allocate at least one byte: a zero-length foreign-alloc is not
         ;; portable, and libcurl is handed the length separately anyway.
         (pointer (cffi:foreign-alloc :uint8 :count (max 1 length))))
    (own-resource resources :foreign pointer)
    (octets-to-foreign octets pointer)
    (values pointer length)))

(defun own-slist (resources strings &key (encoding :utf-8))
  "Build a struct curl_slist from STRINGS and record it for release.

Returns a null pointer for an empty list, which is what libcurl expects for
\"no items\".  Note curl_slist_append returns NULL on failure and does not free
what it was given, so the partial chain is released before signalling."
  (if (null strings)
      (cffi:null-pointer)
      (let ((head (cffi:null-pointer)))
        (dolist (string strings)
          (let ((next (cffi:with-foreign-string (c-string string :encoding encoding)
                        (cffi:foreign-funcall "curl_slist_append"
                                              :pointer head :pointer c-string
                                              :pointer))))
            (when (cffi:null-pointer-p next)
              (unless (cffi:null-pointer-p head)
                (%curl-slist-free-all head))
              (error 'curl-error
                     :message (format nil "curl_slist_append failed for ~S" string)))
            (setf head next)))
        (own-resource resources :slist head))))

(defun slist-to-list (pointer)
  "Walk a struct curl_slist into a list of strings.  Does not free it."
  (loop for node = pointer then (cffi:foreign-slot-value
                                 node '(:struct curl-slist) 'next)
        until (cffi:null-pointer-p node)
        for data = (cffi:foreign-slot-value node '(:struct curl-slist) 'data)
        collect (if (cffi:null-pointer-p data)
                    ""
                    (cffi:foreign-string-to-lisp data))))

(defun own-blob (resources data &key (copy t))
  "Build a struct curl_blob over DATA (octets or a string).

COPY sets CURL_BLOB_COPY, telling libcurl to take its own copy; with it NIL
libcurl borrows the buffer, which is safe here only because the buffer is
owned by the handle and outlives the transfer."
  (let* ((octets (coerce-to-octets data))
         (length (length octets))
         (payload (cffi:foreign-alloc :uint8 :count (max 1 length)))
         (blob (cffi:foreign-alloc '(:struct curl-blob))))
    (octets-to-foreign octets payload)
    ;; Zero the struct first: only bit 0 of `flags' is defined and the rest are
    ;; reserved, so they must not be whatever was on the heap.
    (dotimes (i (cffi:foreign-type-size '(:struct curl-blob)))
      (setf (cffi:mem-aref blob :uint8 i) 0))
    (setf (cffi:foreign-slot-value blob '(:struct curl-blob) 'data) payload
          (cffi:foreign-slot-value blob '(:struct curl-blob) 'len) length
          (cffi:foreign-slot-value blob '(:struct curl-blob) 'flags)
          (if copy +curl-blob-copy+ +curl-blob-nocopy+))
    (own-resource resources :blob blob)))

;;; Error buffer --------------------------------------------------------------

(defun allocate-error-buffer (resources)
  "Allocate a CURL_ERROR_SIZE buffer for CURLOPT_ERRORBUFFER.

libcurl writes a human-readable explanation here that is very often more
specific than curl_easy_strerror -- it names the host, file or certificate at
fault -- which is why conditions prefer it."
  (let ((buffer (cffi:foreign-alloc :char :count +curl-error-size+)))
    (setf (cffi:mem-aref buffer :char 0) 0)
    (own-resource resources :foreign buffer)))

(defun error-buffer-text (buffer)
  "The message libcurl left in BUFFER, or NIL if it left none.

libcurl only writes on failure and never clears, so an empty first byte is the
signal that this transfer contributed nothing."
  (when (and buffer (not (cffi:null-pointer-p buffer))
             (plusp (cffi:mem-aref buffer :unsigned-char 0)))
    (cffi:foreign-string-to-lisp buffer :max-chars (1- +curl-error-size+))))

(defun clear-error-buffer (buffer)
  (when (and buffer (not (cffi:null-pointer-p buffer)))
    (setf (cffi:mem-aref buffer :char 0) 0)))
