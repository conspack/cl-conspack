(in-package :conspack)

(defvar *properties* nil
  "Object => PLIST association for Properties.  Setting a property and
encoding will encode that property.  Decoded properties will be
included here as well.")

(defmacro with-properties (nil &body body)
  `(let ((*properties* (or *properties*
                           (tg:make-weak-hash-table :weakness :key))))
     ,@body))

(defun property (object tag &optional default)
  (getf (gethash object *properties*) tag default))

(defun (setf property) (v object tag)
  (setf (getf (gethash object *properties*) tag) v))

(defun remove-property (object tag)
  (remf (gethash object *properties*) tag))

(defun remove-properties (object)
  (remhash object *properties*))

(defun properties (object)
  (unless (trivial-p object)
    (gethash object *properties*)))
