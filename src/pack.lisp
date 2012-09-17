(in-package :conspack.pack)

 ;; Vector buffer

;;; This is a bit overkill, but in SBCL at least, MAKE-ARRAY with
;;; :adjustable or :fill-pointer performs terribly.  This works
;;; relatively snappily in both SBCL and CCL.

(defstruct output-buffer
  (buffer (make-vector8 16))
  (fill 0 :type fixnum)
  (queue nil :type list)
  (last nil :type list)
  (stream nil))

(defvar *write-buffer* nil)

(defun make-vector8 (len)
  (make-array (the fixnum len) :element-type '(unsigned-byte 8)))

(defun concat-vec8 (arrays &optional last-array fill-pointer)
  (let* ((len (+ (reduce (lambda (n a) (+ n (length a)))
                         arrays :initial-value 0)
                 (or fill-pointer 0)))
         (array (make-vector8 len)))
    (loop as i = 0 then (+ i (length a))
          for a in arrays do
            (replace array a :start1 i)
          finally
             (when last-array
               (replace array last-array :start1 i :end2 fill-pointer)))
    array))

(defun flush-buffer ()
  (when (> (output-buffer-fill *write-buffer*) 0)
    (write-sequence (output-buffer-buffer *write-buffer*)
                    (output-buffer-stream *write-buffer*))
    (setf (output-buffer-fill *write-buffer*) 0)))

(defun extend-buffer (&optional (min 1))
  (let ((current-buffer (output-buffer-buffer *write-buffer*)))
    (setf (output-buffer-last *write-buffer*)
          (nconc (output-buffer-last *write-buffer*)
                 (cons current-buffer nil))
          (output-buffer-buffer *write-buffer*)
          (make-vector8 (max min (1+ (length current-buffer))))
          (output-buffer-fill *write-buffer*) 0)
    (unless (output-buffer-queue *write-buffer*)
      (setf (output-buffer-queue *write-buffer*)
            (output-buffer-last *write-buffer*)))))

(defun buffer-byte (byte)
  (when (= (output-buffer-fill *write-buffer*)
           (array-dimension (output-buffer-buffer *write-buffer*) 0))
    (if (output-buffer-stream *write-buffer*)
        (flush-buffer)
        (extend-buffer)))
  (prog1
      (setf (aref (output-buffer-buffer *write-buffer*)
                  (output-buffer-fill *write-buffer*))
            byte)
    (incf (output-buffer-fill *write-buffer*))))

(defun buffer-sequence (sequence)
  (if (output-buffer-stream *write-buffer*)
      (progn
        (flush-buffer)
        (write-sequence sequence (output-buffer-stream *write-buffer*)))
      (progn
        (let* ((start2 0)
               (len (length sequence))
               (buffer-remaining
                 (- (length (output-buffer-buffer *write-buffer*))
                    (output-buffer-fill *write-buffer*))))
          (when (> buffer-remaining 0)
            (replace (output-buffer-buffer *write-buffer*) sequence
                     :start1 (output-buffer-fill *write-buffer*))
            (setf start2 buffer-remaining)
            (incf (output-buffer-fill *write-buffer*)
                  (min buffer-remaining len)))
          (when (< start2 len)
            (extend-buffer (- len start2))
            (replace (output-buffer-buffer *write-buffer*)
                     sequence :start2 start2)
            (incf (output-buffer-fill *write-buffer*)
                  (- len start2)))))))

(defun finish-output-buffer ()
  (concat-vec8 (output-buffer-queue *write-buffer*)
               (output-buffer-buffer *write-buffer*)
               (output-buffer-fill *write-buffer*)))

(defmacro with-buffered-output ((&optional stream) &body body)
  (with-gensyms (fn)
    `(flet ((,fn () ,@body))
       (if *write-buffer*
           (,fn)
           (let ((*write-buffer* (make-output-buffer :stream ,stream)))
             (,fn)
             (if (output-buffer-stream *write-buffer*)
                 (flush-buffer)
                 (finish-output-buffer)))))))

 ;; READx and WRITEx
;;; WRITE-UNSIGNED-BE, READ-UNSIGNED-BE, and a couple others were
;;; taken from PACK, which is in the public domain.

(defmacro write-unsigned-be (value size)
  (once-only (value)
    `(progn
       ,@(loop for i from (* (1- size) 8) downto 0 by 8
               collect `(buffer-byte (ldb (byte 8 ,i) ,value))))))

(defmacro read-unsigned-be (size stream)
  (with-gensyms (value)
    (once-only (stream)
      `(let ((,value 0))
         ,@(loop for i from (* (1- size) 8) downto 0 by 8
                 collect `(setf (ldb (byte 8 ,i) ,value) (read-byte ,stream)))
         ,value))))

(defmacro make-readers (&rest bitlens)
  (let ((names (mapcar (lambda (n) (symbolicate "READ" (write-to-string n)))
                       bitlens))
        (unames (mapcar (lambda (n) (symbolicate "READU" (write-to-string n)))
                        bitlens)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (inline ,@names ,@unames))
       ,@(loop for name in names
               for uname in unames
               for bits in bitlens
               as bytes = (truncate bits 8)
               collect
               `(progn
                  (defun ,name (stream)
                    (unsigned-to-signed (read-unsigned-be ,bytes stream) ,bytes))
                  (defun ,uname (stream)
                    (read-unsigned-be ,bytes stream)))))))

(defmacro make-writers (&rest bitlens)
  (let ((names (mapcar (lambda (n) (symbolicate "WRITE" (write-to-string n)))
                       bitlens)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (notinline ,@names))
       ,@(loop for name in names
               for bits in bitlens
               as bytes = (truncate bits 8)
               collect
               `(progn
                  (defun ,name (value)
                    (write-unsigned-be value ,bytes)))))))

(make-writers 8 16 32 64 128)
(make-readers 8 16 32 64 128)

(declaim (inline unsigned-to-signed))
(defun unsigned-to-signed (value size)
  (let ((max-signed (expt 2 (1- (* 8 size))))
        (to-subtract (expt 2 (* 8 size))))
    (if (>= value max-signed)
        (- value to-subtract)
        value)))

(defmacro writen (bits stream &rest values)
  (let ((fn (symbolicate "WRITE" (write-to-string bits))))
    (once-only (stream)
      `(progn
         ,@(loop for val in values
                 collect `(,fn ,val ,stream))))))

