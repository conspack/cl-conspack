(in-package #:conspack)

;;;; This defines an encoding for general arrays via TMaps.
;;;; This will only be used for non-vectors; vectors are encoded more
;;;; directly as described in SPEC.

(defmethod object-class-identifier ((object array) &key &allow-other-keys)
  ;; There are various subclasses of CL:ARRAY, some of which may be
  ;; implementation-specific; we don't need any of them.
  'array)

(defmethod encode-object ((object array) &key &allow-other-keys)
  (let ((aet (array-element-type object)))
    `((:dimensions . ,(array-dimensions object))
      (:element-type . ,aet)
      ;; Cheap maneuever: This displaced array is a SEQUENCE, so it can be
      ;; encoded through conspack's normal and efficient vector encoding.
      (:content . ,(make-array (array-total-size object)
                               :element-type aet
                               :displaced-to object)))))

(defmethod decode-object ((class (eql 'array)) alist &key &allow-other-keys)
  (let* ((cdimensions (cdr (assoc :dimensions alist)))
         (caet (cdr (assoc :element-type alist)))
         (ccontent (cdr (assoc :content alist)))
         (array (make-array cdimensions :element-type caet)))
    (loop for i below (length ccontent)
          do (setf (row-major-aref array i) (aref ccontent i)))
    array))
