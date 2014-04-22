(in-package :conspack)

(defstruct r-ref (value))

(defvar *remote-ref-fun*
  (lambda (value)
    (error 'unhandled-remote-reference
           :value value
           :reason "No remote reference handler provided.")))

(defmacro with-remote-refs (fun &body body)
  `(let ((*remote-ref-fun* ,fun))
     ,@body))

(declaim (inline r-ref))
(defun r-ref (value)
  (make-r-ref :value value))
