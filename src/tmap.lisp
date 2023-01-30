(in-package :conspack)

(defgeneric encode-object (object &key &allow-other-keys)
  (:documentation "Return an alist for `OBJECT` which will be used for
key-value pairs for a Typed Map (tmap).  The class of `OBJECT` will
be encoded along with these and used by the decoder to recreate
the object.")
  ;; To support inheritance, the results of multiple applicable methods
  ;; are appended together to form the final alist.
  (:method-combination append))

(defgeneric object-class-identifier (object &key &allow-other-keys)
  (:documentation "Return a value for `OBJECT` which will be used as
the class for a Typed Map (tmap). This object will be encoded along
with the key-value pairs returned by `ENCODE-OBJECT` and used by
the decoder to recreate the object.")
  (:method (object &key &allow-other-keys)
    (class-name (class-of object))))

(defgeneric decode-object-allocate (class alist &key &allow-other-keys)
  (:documentation "Create an empty object of the given `CLASS`
based on the values in the `ALIST`. Note that any values in the TMap
that are or contain forward references may not appear in the alist,
and containers may be uninitialized. The alist will only be complete
when passed to `DECODE-OBJECT-INITIALIZE`.
Methods should specialize on `CLASS (EQL symbol)`.")
  (:method (class alist &key &allow-other-keys)
    (declare (ignore alist))
    ;; Assume it's a standard class.
    (allocate-instance (find-class class))))

(defgeneric decode-object-initialize (object class alist &key &allow-other-keys)
  (:documentation "Initialize an empty OBJECT of the given `CLASS`
based on the values in the `ALIST`. Methods should specialize on `OBJECT`,
but can use the `CLASS` if they wish. Return value is ignored.")
  ;; Method combo in place to make it easier to initialize subclasses.
  ;; Methods need only initialize their particular slots (or whatever),
  ;; and can rely on the superclass initializations having completed.
  (:method-combination progn :most-specific-last))

(defmacro slots-to-alist ((instance) &body slot-names)
  "Produce an `ALIST` of slot-names-to-slot-values, suitable for
`ENCODE-OBJECT`."
  (if slot-names
      (alexandria:once-only (instance)
        (alexandria:with-gensyms (alist)
          `(let ((,alist nil))
             ,@(loop for slot-name in slot-names
                     collect `(when (slot-boundp ,instance ',slot-name)
                                (push (cons ',slot-name
                                            (slot-value ,instance ',slot-name))
                                      ,alist)))
             ,alist)))
      ()))

(defmacro alist-to-slots ((alist instance) &body slot-names)
  "Set slots via `(SETF (SLOT-VALUE ...))` based on the values of the
slots specified.

Slots are set on the provided `INSTANCE`."
  (alexandria:once-only (alist)
    (alexandria:with-gensyms (object alist% pair)
      `(let ((,object ,instance) (,alist% ,alist))
         (declare (ignorable ,alist%)) ; if no slots
         (prog1 ,object
           ,@(loop for slot-name in slot-names
                   collect `(let ((,pair (assoc ',slot-name ,alist%)))
                              (when ,pair
                                (setf (slot-value ,object ',slot-name)
                                      (cdr ,pair))))))))))

(defmacro defencoding (class-name &body slot-names)
  "Trivially define `ENCODE-OBJECT` and `DECODE-OBJECT-INITIALIZE`
to store and load the given slots."
  `(eval-when (:load-toplevel :execute)
     (defmethod encode-object append
         ((object ,class-name) &key &allow-other-keys)
       (slots-to-alist (object) ,@slot-names))
     (defmethod decode-object-initialize progn
         ((object ,class-name) class alist &key &allow-other-keys)
       (declare (ignore class))
       (alist-to-slots (alist object)
         ,@slot-names))))
