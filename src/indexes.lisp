(in-package :conspack)

(defstruct (symbol-index (:constructor %make-symbol-index))
  (sym->id nil :type hash-table)
  (id->sym nil :type vector))

(defmethod make-load-form ((object symbol-index) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defun make-symbol-index (len)
  (%make-symbol-index :sym->id (make-hash-table :size len)
                      :id->sym (make-array (the fixnum len))))

(defvar *index* nil)
(defvar *indexes* (make-hash-table))

(defun make-index (values &optional name)
  (let ((table (make-symbol-index (length values))))
    (loop for value in values
          as i from 0
          do (setf (gethash value (symbol-index-sym->id table)) i)
             (setf (aref (symbol-index-id->sym table) i) value))
    (when name
      (setf (gethash name *indexes*) table))
    table))

(defun find-index (name)
  (gethash name *indexes*))

(defmacro with-index ((&rest values) &body body)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (let ((table (make-index values)))
      `(let ((*index* ,table))
         ,@body))))

(defmacro with-named-index (name &body body)
  `(let ((*index* (find-index ,name)))
     ,@body))

(defmacro define-index (name &rest values)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (make-index ',values ',name)))

(defun delete-index (name)
  (remhash name *indexes*))

(defun find-in-index (symbol-or-number &optional (index *index*))
  (when index
    (etypecase symbol-or-number
      (symbol (gethash symbol-or-number (symbol-index-sym->id index)))
      (number (aref (symbol-index-id->sym index) symbol-or-number)))))
