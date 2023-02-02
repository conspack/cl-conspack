(in-package :conspack)

 ;; Debugging

(defvar *explain-eof* nil)

(defmacro explaining-errors (&body body)
  `(handler-case
       (unless *explain-eof*
         (progn ,@body))
     (invalid-header (c) (list 'invalid-header
                               (conspack-error-value c)
                               (conspack-error-reason c)))
     (end-of-file ()
       (setf *explain-eof* t)
       'end-of-file)))

(defun explain-container (buffer header)
  (let* ((item)
         (size (decode-size (size-bytes header) buffer))
         (container-type (decode-container-type header))
         (nobjects (ecase container-type
                     ((:vector :list) size)
                     ((:map) (* 2 size))
                     ((:tmap) (1+ (* 2 size)))))) ; 1+ for class
    (push container-type item)
    (push size item)
    (if (container-fixed-p header)
        (progn
          (push :fixed item)
          (let ((fixed-header (readu8 buffer)))
            (if (eq :number (decode-header fixed-header))
                (push (decode-number-header fixed-header) item)
                (push (decode-header fixed-header) item))
            (push (explain-buffer buffer nobjects fixed-header)
                  item)))
        (push (explain-buffer buffer nobjects) item))
    item))

(defun explain-buffer (buffer &optional n fixed-header)
  "This will try and find and interpret as many bytes as possible,
up to `N` objects, or until end-of-file, if `N` is `nil`."
  (let (header output (i 0))
    (tagbody
     :top
       (handler-case
           (loop do
             (setf header (or fixed-header (readu8 buffer)))
             (let (item)
               (push (decode-header header) item)
               (cond
                 ((string-p header)
                  (let ((size (decode-size (size-bytes header) buffer)))
                    (push (decode-string header buffer size)
                          item)))
                 ((container-p header)
                  (setf item (explain-container buffer header)))
                 ((number-p header)
                  (push (decode-number-header header) item)
                  (push (decode-number header buffer) item))
                 ((tag-p header)
                  (push (decode-ref-id header buffer) item)
                  (push (car (explain-buffer buffer 1)) item))
                 ((ref-p header)
                  (push (decode-ref-id header buffer) item))
                 ((remote-ref-p header)
                  (push (car (explain-buffer buffer 1)) item))
                 ((index-p header)
                  (push (decode-ref-id header buffer) item))
                 ((cons-p header)
                  (push (car (explain-buffer buffer 1)) item)
                  (push (car (explain-buffer buffer 1)) item))
                 ((properties-p header)
                  (push (car (explain-buffer buffer 1)) item)
                  (push (car (explain-buffer buffer 1)) item))
                 (t (push (decode-value buffer header) item)))
               (push (nreverse item) output))
             (unless (tag-p header) (incf i))
             (when (and n (>= i n)) (loop-finish)))
         (end-of-file () (push 'end-of-file output))
         (invalid-header ()
           (push (list :invalid-byte header) output)
           (go :top))))
    (nreverse output)))

(defun explain (vector)
  (with-fast-input (buffer vector)
    (explain-buffer buffer)))
