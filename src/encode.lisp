(in-package :conspack)

 ;; Utility

(defstruct pointer
  (value 0)
  (bit-width nil))

(defun pointer (value &optional bit-width)
  (make-pointer :value value :bit-width bit-width))

(defmethod print-object ((o pointer) s)
  (print-unreadable-object (o s :type t)
    (format s "#x~8,'0X:~D" (pointer-value o) (pointer-bit-width o))))

 ;; Encoding

(defun encode-header (header-byte buffer &optional fixed-header
                      size-value size-type)
  "Combine `VALUES` with LOGIOR and write."
  (let ((size-type (cond
                     (fixed-header (size-type fixed-header))
                     (size-type size-type)
                     (size-value (len-size-type size-value))
                     (t 0))))
    (unless fixed-header
      (writeu8 (logior size-type header-byte) buffer))
    (when size-value
      (encode-length size-type size-value buffer))))

(defun encode-length (size-type len buffer)
  (declare (type (unsigned-byte 8) size-type))
  (ecase size-type
    (#b00 (writeu8 len buffer))
    (#b01 (writeu16-be len buffer))
    (#b10 (writeu32-be len buffer))))

(defun encode-boolean (value buffer &optional fixed-header)
  (when fixed-header
    (error "FIXED-HEADER specified for boolean."))
  (if value
      (writeu8 #b1 buffer)
      (writeu8 #b0 buffer)))

(defun encode-number (value buffer &optional fixed-header)
  (let ((type (if fixed-header
                  (decode-number-header fixed-header)
                  (number-type value))))
    (encode-header (number-header type) buffer fixed-header)
    (ecase type
      (:int8 (write8 value buffer))
      (:uint8  (writeu8 value buffer))
      (:int16 (write16-be value buffer))
      (:uint16 (writeu16-be value buffer))
      (:int32 (write32-be value buffer))
      (:uint32 (writeu32-be value buffer))
      (:int64 (write64-be value buffer))
      (:uint64 (writeu64-be value buffer))
      (:int128 (write128-be value buffer))
      (:uint128 (writeu128-be value buffer))
      (:single-float (writeu32-be (ieee-floats:encode-float32 value) buffer))
      (:double-float (writeu64-be (ieee-floats:encode-float64 value) buffer))
      (:complex
       (%encode (realpart value) buffer)
       (%encode (imagpart value) buffer))
      (:rational
       (%encode (numerator value) buffer)
       (%encode (denominator value) buffer)))))

(defun encode-string (value buffer &optional fixed-header)
  (let ((octets (trivial-utf-8:string-to-utf-8-bytes value)))
    (encode-header +string-header+ buffer fixed-header (length octets))
    (fast-write-sequence octets buffer)))

(defun encode-list (value buffer &optional fixed-header fixed-type)
  (let ((len (list-length-with-refs value)))
    (if (or (consp (cdr value)) fixed-header)
        (progn
          (encode-header (container-header :list fixed-type) buffer
                         fixed-header len)
          (when fixed-type (encode-header fixed-type buffer))
          (loop for i on value
                for x from 0 below len
                do (encode-ref-or-value (car i) buffer fixed-type)
                until (or (null (cdr i))
                          (get-ref-id (cdr i)))
                finally
                   (cond
                     ((and (not fixed-type) (listp i))
                      (encode-ref-or-value (cdr i) buffer fixed-type))
                     ((not fixed-type)
                      (encode-ref-or-value i buffer fixed-type)))))
        (encode-cons value buffer))))

(defun encode-vector (value buffer &optional fixed-header fixed-type)
  (let ((fixed-type (or fixed-type
                        (number-header (find-fixed-type (array-element-type value))))))
    (encode-header (container-header :vector fixed-type) buffer
                   fixed-header (length value))
    (when fixed-type
      (encode-header fixed-type buffer))
    (loop for i across value do
      (encode-ref-or-value i buffer fixed-type))))

(defun encode-hash (value buffer &optional fixed-header fixed-type)
  (encode-header (container-header :map fixed-type) buffer
                 fixed-header (hash-table-count value))
  (loop for k being each hash-key in value using (hash-value v) do
    (encode-ref-or-value k buffer)
    (encode-ref-or-value v buffer)))

(defun encode-tmap (value buffer &optional fixed-header)
  (let* ((encoded-alist (if *ref-context*
                            (gethash value (ref-context-encoded-objects *ref-context*))
                            (encode-object value)))
         (len (length encoded-alist)))
    (encode-header (container-header :tmap nil) buffer fixed-header len)
    (encode-ref-or-value (object-class-identifier value) buffer)
    (loop for i in encoded-alist do
          (encode-ref-or-value (car i) buffer)
          (encode-ref-or-value (cdr i) buffer))))

(defun encode-sequence (value buffer &optional fixed-header fixed-type)
  (etypecase value
    (list (encode-list value buffer fixed-header fixed-type))
    (vector (encode-vector value buffer fixed-header fixed-type))
    (hash-table (encode-hash value buffer fixed-header fixed-type))))

(defun encode-ref (value buffer &optional fixed-header)
  (if (and (not fixed-header) (typep value '(unsigned-byte 4)))
      (writeu8 (logior +ref-header+ +reftag-inline+
                             (logand value #xF))
              buffer)
      (encode-header +ref-header+ buffer fixed-header value)))

(defun encode-r-ref (value buffer &optional fixed-header)
  (encode-header +r-ref-header+ buffer fixed-header)
  (%encode value buffer))

(defun encode-pointer (value buffer &optional fixed-header)
  (encode-header +pointer-header+ buffer fixed-header
                 (pointer-value value)
                 (when-let (width (pointer-bit-width value))
                   (bits-size-type width))))

(defun encode-tag (value buffer &optional fixed-header)
  (if (and (not fixed-header) (typep value '(unsigned-byte 4)))
      (writeu8 (logior +tag-header+ +reftag-inline+
                      (logand value #xF))
              buffer)
      (encode-header +tag-header+ buffer fixed-header value)))

(defun encode-cons (value buffer &optional fixed-header)
  (encode-header +cons-header+ buffer fixed-header)
  (encode-ref-or-value (car value) buffer)
  (encode-ref-or-value (cdr value) buffer))

(defun encode-package (value buffer &optional fixed-header)
  (encode-header +package-header+ buffer fixed-header)
  (encode-ref-or-value (package-name value) buffer))

(defun encode-symbol (value buffer &optional fixed-header)
  (encode-header (symbol-header value) buffer fixed-header)
  (encode-ref-or-value (symbol-name value) buffer)
  (unless (keywordp value)
    (encode-ref-or-value (symbol-package value) buffer)))

(defun encode-character (value buffer &optional fixed-header)
  (let ((bytes (trivial-utf-8:string-to-utf-8-bytes (string value))))
    (encode-header (logior +character-header+ (length bytes))
                   buffer fixed-header)
    (fast-write-sequence bytes buffer)))

(defun encode-properties (value buffer)
  (encode-header +properties-header+ buffer)
  (encode-ref-or-value value buffer))

(defun encode-index (value buffer &optional fixed-header)
  (if (and (not fixed-header) (typep value '(unsigned-byte 4)))
      (writeu8 (logior +index-header+ +reftag-inline+
                      (logand value #xF))
              buffer)
      (encode-header +index-header+ buffer fixed-header value)))

(defun %properties-encode (value buffer)
  ;; FIXME: This should all be in a separate function or method
  (typecase value
    (hash-table
     (when (eq 'equal (hash-table-test value))
       (setf (property value :test) :equal))))
  (when-let ((p (properties value)))
    (encode-properties p buffer)))

(defun %encode (value buffer &optional fixed-header)
  (unless (or fixed-header
              (not (tracking-refs-p))
              (written-p value))
    (let ((id (get-ref-id value)))
      (when id
        (encode-tag id buffer)
        (wrote value))))
  (etypecase value
    (boolean (encode-boolean value buffer fixed-header))
    (number (encode-number value buffer fixed-header))
    (string (encode-string value buffer fixed-header))
    ((or sequence hash-table) (encode-sequence value buffer fixed-header))
    (package (encode-package value buffer fixed-header))
    (symbol
     (let ((id (find-in-index value)))
       (if id
           (encode-index id buffer fixed-header)
           (encode-symbol value buffer fixed-header))))
    (character (encode-character value buffer fixed-header))
    (r-ref (encode-r-ref (r-ref-value value) buffer fixed-header))
    (pointer (encode-pointer value buffer fixed-header))
    (t (encode-tmap value buffer fixed-header))))

(defun encode-ref-or-value (value buffer &optional fixed-header)
  (%properties-encode value buffer)
  (if (and (tracking-refs-p)
           (or (not fixed-header) (ref-p fixed-header))
           (written-p value))
      (encode-ref (get-ref-id value) buffer fixed-header)
      (%encode value buffer fixed-header)))

(defun encode-to-buffer (value fast-io-buffer)
  (with-properties ()
    (when (tracking-refs-p)
      (notice-recursively value))
    (encode-ref-or-value value fast-io-buffer)))

(defun encode-to-file (value filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
    (encode value :stream stream)))

(defun encode (value &key stream)
  (with-fast-output (buffer stream)
    (encode-to-buffer value buffer)))
