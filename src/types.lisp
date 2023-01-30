(in-package :conspack)

 ;; Types

(deftype header () '(unsigned-byte 8))
(deftype header-test-fun ()
  '(function (header) boolean))
(deftype header-decode-fun ()
  '(function (header) keyword))

(define-condition conspack-error (error)
  ((value :initarg :value :reader conspack-error-value)
   (reason :initarg :reason :reader conspack-error-reason))
  (:report (lambda (c s)
             (with-slots (value reason) c
               (format s "Conspack: ~A: ~A~%Reason: ~A"
                       (class-name (class-of c))
                       value reason)))))

(define-condition invalid-header (conspack-error) ())
(define-condition invalid-size (conspack-error) ())

(define-condition duplicate-id (conspack-error)
  ((existing-value :initarg :existing-value :reader duplicate-ref-existing-value)
   (new-value :initarg :new-value :reader duplicate-ref-new-value))
  (:report (lambda (c s)
             (with-slots (value existing-value new-value) c
               (format s "Duplicate object ID: ~A~%Existing value: ~A~%New value: ~A"
                       value existing-value new-value)))))


(define-condition invalid-package-name (conspack-error) ())
(define-condition invalid-symbol-name (conspack-error) ())
(define-condition invalid-symbol-package (conspack-error) ())

(define-condition invalid-index (conspack-error) ())
(define-condition vacuous-ref (conspack-error) ())
(define-condition vacuous-properties (conspack-error) ())
(define-condition forward-referenced-properties (conspack-error) ())
(define-condition toplevel-forward-ref (conspack-error) ())

(define-condition unhandled-remote-reference (conspack-error) ())

(define-condition invalid-tmap-type (conspack-error) ())

(define-condition security-error (conspack-error) ())

(define-condition invalid-forward-ref (security-error) ())
(define-condition max-size-exceeded (security-error) ())
