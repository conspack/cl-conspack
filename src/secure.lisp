(in-package :conspack)

 ;; External

(defvar *conspack-security* nil)
(defvar *conspack-max-bytes* nil)
(defvar *conspack-forward-refs* t)

 ;; Internal

(defvar *bytes-alloc* nil)

;;; FIXME: There's more to life than x86_64 .. but trivial-features
;;; doesn't seem to implement much and I don't have platforms to test
;;; on ...
(defconstant +platform-bytes+
  #+(not x86-64)
  4

  #+(or x86-64)
  8)

 ;; Config

(defmacro with-conspack-security ((&key (max-bytes nil) (forward-refs t))
                                  &body body)
  `(let ((*conspack-security* t)
         (*conspack-max-bytes* ,max-bytes)
         (*conspack-forward-refs* ,forward-refs)
         (*bytes-alloc* (or *bytes-alloc* 0)))
     ,@body))

 ;; Utility

(declaim (ftype (function (fixnum &optional fixnum) null)
                use-bytes precheck-bytes))
(declaim (inline use-bytes))
(defun use-bytes (n &optional (times 1))
  (when *conspack-max-bytes*
    (incf *bytes-alloc* (* n times))
    (when (> *bytes-alloc* *conspack-max-bytes*)
      (error 'max-size-exceeded
             :value *bytes-alloc*
             :reason "Size restricted."))))

(defun precheck-bytes (n &optional (times 1))
  (unless *conspack-max-bytes* (return-from precheck-bytes))
  (when (> (+ (* n times) *bytes-alloc*)
           *conspack-max-bytes*)
    (error 'max-size-exceeded
           :value (+ (* n times) *bytes-alloc*)
           :reason "Size restricted.")))

(declaim (ftype (function (fixnum (or null (unsigned-byte 8))) null) container-precheck-bytes))
(defun container-precheck-bytes (len fixed-header)
  (when *conspack-max-bytes*
    (use-bytes +platform-bytes+)
    (if (and fixed-header (number-p fixed-header))
        (precheck-bytes (number-size fixed-header) len)
        (precheck-bytes +platform-bytes+ len))))
