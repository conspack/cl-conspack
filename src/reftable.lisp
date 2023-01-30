(in-package :conspack)

(defvar *ref-context* nil)

(declaim (inline tracking-refs-p))
(defun tracking-refs-p () *ref-context*)

(defstruct ref-context
  (max-id 0 :type (unsigned-byte 32))
  (obj-to-id (make-hash-table))
  (id-to-obj (make-hash-table))
  (noticed-objects (make-hash-table))
  (written-objects (make-hash-table))
  (encoded-objects (make-hash-table)))

(defun clear-refs (&optional (context *ref-context*))
  (setf (ref-context-max-id context) 0)
  (clrhash (ref-context-obj-to-id context))
  (clrhash (ref-context-id-to-obj context))
  (clrhash (ref-context-noticed-objects context))
  (clrhash (ref-context-written-objects context)))

(defun add-ref (id object &optional (context *ref-context*))
  (when (nth-value 1 (gethash id (ref-context-id-to-obj context)))
    (error 'duplicate-id
           :value id
           :existing-value (gethash id (ref-context-id-to-obj context))
           :new-value object))
  (setf (gethash id (ref-context-id-to-obj context)) object
        (gethash object (ref-context-obj-to-id context)) id
        (ref-context-max-id context) (1+ (max id (ref-context-max-id context))))
  (values))

(defun get-ref (id &optional (context *ref-context*))
  (when context
    (gethash id (ref-context-id-to-obj context))))

(defun get-ref-id (object &optional (context *ref-context*))
  (when context
    (gethash object (ref-context-obj-to-id context))))

(defun tag-object (object &optional (context *ref-context*))
  (unless (get-ref-id object context)
    (add-ref (ref-context-max-id context)
             object context)))

(defun written-p (object &optional (context *ref-context*))
  (gethash object (ref-context-written-objects context)))

(defun wrote (object &optional (context *ref-context*))
  (setf (gethash object (ref-context-written-objects context)) t))

(defun trivial-p (object)
  (or (typep object 'character)
      (typep object 'number)
      (typep object 'boolean)))

(defun noticed-p (object &optional (context *ref-context*))
  (nth-value 1 (gethash object (ref-context-noticed-objects context))))

(defun notice-object (object &optional (context *ref-context*))
  (setf (gethash object (ref-context-noticed-objects context)) t))

(defun list-length-with-refs (list &optional (context *ref-context*))
  (if context
      (loop for i on list
            summing 1 into count
            until (and (consp (cdr i))
                       (get-ref-id (cdr i) context))
            finally (return (1+ count)))
      (loop for i on list
            summing 1 into count
            while (consp (cdr i))
            finally (return (1+ count)))))

(defun notice-recursively (object &optional (context *ref-context*))
  (unless (or (trivial-p object) (null context))
    (if (noticed-p object context)
        (tag-object object context)
        (progn
          (notice-object object context)
          (typecase object
            (list
             (notice-recursively (car object) context)
             (notice-recursively (cdr object) context))
            (vector
             (unless (subtypep (array-element-type object) 'number)
               (loop for i across object do (notice-recursively i context))))
            (hash-table
             (loop for k being each hash-key in object
                     using (hash-value v) do
                       (notice-recursively k context)
                       (notice-recursively v context)))
            ((or string package symbol r-ref pointer))
            (t (let ((encoded-alist (encode-object object)))
                 (notice-recursively encoded-alist context)
                 (setf (gethash object (ref-context-encoded-objects context))
                       encoded-alist))))))))

(defun referrable-p (object)
  (or (typep object 'sequence)
      (typep object 'hash-table)
      (typep object 'symbol)
      (typep object 'package)))

(defmacro tracking-refs (ref-context &body body)
  `(let ((*ref-context* (or ,ref-context
                            *ref-context*
                            (make-ref-context))))
     ,@body))
