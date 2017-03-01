(in-package :conspack)

 ;; Decode Properties

(defvar *current-properties* nil)

 ;; Reading

(defun decode-boolean (header)
  (use-bytes +platform-bytes+)
  (logbitp 0 header))

(defun decode-number (header buffer)
  (ecase (decode-number-header header)
    (:int8 (use-bytes 1) (read8 buffer))
    (:int16 (use-bytes 2) (read16-be buffer))
    (:int32 (use-bytes 4) (read32-be buffer))
    (:int64 (use-bytes 8) (read64-be buffer))
    (:uint8 (use-bytes 1) (readu8 buffer))
    (:uint16 (use-bytes 2) (readu16-be buffer))
    (:uint32 (use-bytes 4) (readu32-be buffer))
    (:uint64 (use-bytes 8) (readu64-be buffer))
    (:single-float
     (use-bytes 4)
     (ieee-floats:decode-float32 (readu32-be buffer)))
    (:double-float
     (use-bytes 8)
     (ieee-floats:decode-float64 (readu64-be buffer)))
    (:int128 (use-bytes 16) (read128-be buffer))
    (:uint128 (use-bytes 16) (readu128-be buffer))
    (:complex
     (use-bytes +platform-bytes+)
     (let ((realpart (decode-value buffer))
           (imagpart (decode-value buffer)))
       (complex realpart imagpart)))
    (:rational
     (use-bytes +platform-bytes+)
     (let ((num (decode-value buffer))
           (den (decode-value buffer)))
       (rationalize (/ num den))))))

(defun decode-size (bytes buffer)
  (ecase bytes
    (1 (readu8 buffer))
    (2 (readu16-be buffer))
    (4 (readu32-be buffer))))

(defun decode-string (header buffer &optional len)
  (let ((len (or len (decode-size (size-bytes header) buffer))))
    (use-bytes len)
    (let ((octets (make-octet-vector len)))
      (fast-read-sequence octets buffer)
      (trivial-utf-8:utf-8-bytes-to-string octets))))

(defun decode-list (header buffer &optional len)
  (let ((len (or len (decode-size (size-bytes header) buffer)))
        (fixed-header (when (container-fixed-p header)
                        (fast-read-byte buffer))))
    (container-precheck-bytes len fixed-header)
    (when (< len 2)
      (error 'invalid-size :value len :reason "Length of LIST must be >= 2" ))
    (loop with first = (cons nil nil)
          for i from 0 below (1- len)
          as cons = first then (setf (cdr cons) (cons nil nil))
          do (setf (car cons)
                   (decode-value-or-fref buffer :car cons nil fixed-header))
          finally
             (if fixed-header
                 (setf (cdr cons)
                       (list (decode-value-or-fref buffer :car cons nil fixed-header)))
                 (setf (cdr cons) (decode-value-or-fref buffer :cdr cons nil)))
             (return first))))

(defun decode-vector (header buffer &optional len)
  (let* ((len (or len (decode-size (size-bytes header) buffer)))
         (fixed-header (when (container-fixed-p header)
                         (fast-read-byte buffer)))
         (fixed-type
           (when fixed-header
             (number-type-to-lisp
              (decode-number-header fixed-header)))))
    (container-precheck-bytes len fixed-header)
    (let ((v (make-array len :element-type (or fixed-type t))))
      (loop for i below len do
        (setf (aref v i)
              (decode-value-or-fref buffer :aref v i fixed-header)))
      v)))

(defun decode-map (header buffer &optional len)
  (let ((len (or len (decode-size (size-bytes header) buffer)))
        (fixed-header (when (container-fixed-p header)
                        (fast-read-byte buffer)))
        (hash (make-hash-table :test (if (eq :equal (getf *current-properties* :test))
                                         'equal 'eql))))
    (container-precheck-bytes (* 2 len) fixed-header)
    (loop for i from 0 below len do
      (let* ((key (decode-value-or-fref buffer :map-key hash fixed-header))
             (value (decode-value-or-fref buffer :map-value hash key)))
        (setf (gethash key hash) value)))
    hash))

(defun decode-tmap (header buffer &optional len)
  (let* ((len (or len (decode-size (size-bytes header) buffer)))
         (fixed-header (when (container-fixed-p header)
                         (fast-read-byte buffer)))
         (class (decode-value buffer)))
    (container-precheck-bytes (* 2 len) fixed-header)
    (unless (symbolp class)
      (error 'invalid-tmap-type :value class :reason "Not a symbol"))
    (loop for i from 0 below len
          collect
          (let ((cons (cons nil nil)))
            (setf (car cons) (decode-value-or-fref buffer :car cons fixed-header))
            (setf (cdr cons) (decode-value-or-fref buffer :cdr cons nil))
            cons) into alist
          finally
             (return (decode-object class alist)))))

(defun decode-container (header buffer &optional len)
  (let ((type (decode-container-type header)))
    (ecase type
      (:list (decode-list header buffer len))
      (:vector (decode-vector header buffer len))
      (:map (decode-map header buffer len))
      (:tmap (decode-tmap header buffer len)))))

(defun decode-ref-id (header buffer)
  (if (logbitp 4 header)
      (logand header #xF)
      (decode-size (size-bytes header) buffer)))

(defun decode-ref (header buffer)
  (let ((id (decode-ref-id header buffer)))
    (use-bytes +platform-bytes+)
    (multiple-value-bind (obj exists-p) (get-ref id)
      (if exists-p
          obj
          (progn
            (unless *conspack-forward-refs*
              (error 'invalid-forward-ref
                     :value id
                     :reason "Forward refs restricted."))
            (add-forward-ref id))))))

(defun decode-r-ref (header buffer)
  (declare (ignore header))
  (use-bytes +platform-bytes+)
  (let ((object (decode-value buffer)))
    (funcall *remote-ref-fun* object)))

(defun decode-pointer (header buffer &optional len)
  (let ((len (or len (size-bytes header))))
    (use-bytes +platform-bytes+ 2)
    (pointer (decode-size len buffer) (* 8 len))))

(defun decode-tag (header buffer)
  (let* ((id (decode-ref-id header buffer))
         (object (decode-value buffer)))
    (add-ref id object)
    (replace-forward-refs id object)
    object))

(defun decode-cons (header buffer)
  (declare (ignore header))
  (use-bytes +platform-bytes+ 2)
  (cons (decode-value buffer)
        (decode-value buffer)))

(defun decode-package (header buffer)
  (declare (ignore header))
  (use-bytes +platform-bytes+)
  (let ((package-name (decode-value buffer)))
    (unless (stringp package-name)
      (error 'invalid-package-name :value package-name :reason "Not a string."))
    (let ((package (find-package package-name)))
      (unless package
        (error 'invalid-package-name :value package-name
                                     :reason "Package does not exist."))
      package)))

(defvar *intern-symbols* nil)

(defun decode-symbol (header buffer)
  (let ((symbol-name (decode-value buffer)))
    (use-bytes +platform-bytes+)
    (unless (stringp symbol-name)
      (error 'invalid-symbol-name :value symbol-name :reason "Not a string."))
    (let ((package (if (keyword-p header)
                       (find-package :keyword)
                       (decode-value buffer))))
      (if package
          (if *intern-symbols*
              (intern symbol-name package)
              (multiple-value-bind (symbol status)
                  (find-symbol symbol-name package)
                (if status
                    symbol
                    (make-symbol symbol-name))))
          (make-symbol symbol-name)))))

(defmacro with-interning (&body body)
  `(let ((*intern-symbols* t)) ,@body))

(defun decode-character (header buffer)
  (let ((len (logand header #b11)))
    (use-bytes len)
    (let ((octets (make-octet-vector len)))
      (fast-read-sequence octets buffer)
      (aref (trivial-utf-8:utf-8-bytes-to-string octets) 0))))

(defun decode-index (header buffer)
  (let* ((id (decode-ref-id header buffer))
         (sym (find-in-index id)))
    (unless sym
      (error 'invalid-index :value id :reason "ID not found in index"))
    (use-bytes +platform-bytes+)
    sym))

(defun decode-properties (header buffer)
  (declare (ignore header))
  (let* ((*current-properties* (decode-value buffer))
         (next-object (decode-value buffer)))
    (unless (or (characterp next-object)
                (numberp next-object))
      (setf (gethash next-object *properties*)
            *current-properties*))
    next-object))

(defun decode-value (buffer &optional header)
  (let ((header (or header (fast-read-byte buffer))))
    (ecase (decode-header header)
      (:boolean (decode-boolean header))
      (:number (decode-number header buffer))
      (:string (decode-string header buffer))
      (:container (decode-container header buffer))
      (:tag (decode-tag header buffer))
      (:ref (decode-ref header buffer))
      (:r-ref (decode-r-ref header buffer))
      (:pointer (decode-pointer header buffer))
      (:cons (decode-cons header buffer))
      (:package (decode-package header buffer))
      (:symbol (decode-symbol header buffer))
      (:character (decode-character header buffer))
      (:properties (decode-properties header buffer))
      (:index (decode-index header buffer)))))

(defun decode-value-or-fref (buffer type ref datum &optional header)
  (let ((value (decode-value buffer header)))
    (when (forward-ref-p value)
      (setf (forward-ref-type value) type)
      (setf (forward-ref-ref value) ref)
      (setf (forward-ref-datum value) datum))
    value))

(defun decode (byte-vector &optional (offset 0))
  (with-fast-input (buffer byte-vector nil offset)
    (values (with-properties ()
              (tracking-refs ()
                (decode-value buffer)))
            (fast-io:buffer-position buffer))))

(defun decode-stream (stream &optional (offset 0))
  (with-fast-input (buffer nil stream offset)
    (values (with-properties ()
                (tracking-refs ()
                  (decode-value buffer)))
            (fast-io:buffer-position buffer))))

(defun decode-file (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let (eof)
      (tracking-refs ()
        (loop as object = (handler-case
                              (decode-stream stream)
                            (end-of-file () (setf eof t) (values)))
              until eof
              collect object)))))

