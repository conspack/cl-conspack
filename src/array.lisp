(in-package #:conspack)

;;;; This defines an encoding for general arrays via TMaps.
;;;; This will only be used for non-vectors; vectors are encoded more
;;;; directly as described in SPEC.

(defmethod object-class-identifier ((object array) &key &allow-other-keys)
  ;; There are various subclasses of CL:ARRAY, some of which may be
  ;; implementation-specific; we don't need any of them.
  'array)

(defmethod encode-object append ((object array) &key &allow-other-keys)
  (let ((aet (array-element-type object)))
    `((:dimensions . ,(array-dimensions object))
      (:element-type . ,aet)
      ;; Cheap maneuever: This displaced array is a SEQUENCE, so it can be
      ;; encoded through conspack's normal and efficient vector encoding.
      (:content . ,(make-array (array-total-size object)
                               :element-type aet
                               :displaced-to object)))))

(defmethod decode-object-allocate ((class (eql 'array)) alist
                                   &key &allow-other-keys)
  (let* ((cdimensions (cdr (assoc :dimensions alist)))
         (caet (cdr (assoc :element-type alist))))
    (make-array cdimensions :element-type caet)))

(defmethod decode-object-initialize progn ((array array) class alist
                                           &key &allow-other-keys)
  (declare (ignore class))
  (let ((ccontent (cdr (assoc :content alist))))
    (loop for i below (length ccontent)
          do (setf (row-major-aref array i) (aref ccontent i)))
    array))
