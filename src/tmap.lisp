(in-package :conspack)

(defgeneric encode-object (object &key &allow-other-keys)
  (:documentation "Return an alist for `OBJECT` which will be used for
key-value pairs for a Typed Map (tmap).  The class of `OBJECT` will
be encoded along with these and used by the decoder to recreate
the object."))

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
Methods should specialize on `CLASS (EQL symbol)`."))

(defgeneric decode-object-initialize (class object alist &key &allow-other-keys)
  (:documentation "Initialize an empty object of the given `CLASS`
based on the values in the `ALIST`. Methods should specialize on
`CLASS (EQL symbol)`."))
  

(defmacro slots-to-alist ((instance) &body slot-names)
  "Produce an `ALIST` of slot-names-to-slot-values, suitable for
`ENCODE-OBJECT`."
  (alexandria:once-only (instance)
    `(list
      ,@(loop for slot-name in slot-names
              collect `(cons ',slot-name (slot-value ,instance ',slot-name))))))

(defmacro alist-to-slots ((alist instance) &body slot-names)
  "Set slots via `(SETF (SLOT-VALUE ...))` based on the values of the
slots specified.

Slots are set on the provided `INSTANCE`."
  (alexandria:once-only (alist)
    (alexandria:with-gensyms (object)
      `(let ((,object ,instance))
         (prog1 ,object
           (setf
            ,@(loop for slot-name in slot-names
                    collect `(slot-value ,object ',slot-name)
                    collect `(aval ',slot-name ,alist))))))))

(defmacro defencoding (class-name &body slot-names)
  "Trivially define `ENCODE-OBJECT`. `DECODE-OBJECT-ALLOCATE`, and
`DECODE-OBJECT-INITIALIZE` to store and load the given slots."
  `(eval-when (:load-toplevel :execute)
     (defmethod encode-object ((object ,class-name) &key &allow-other-keys)
       (slots-to-alist (object) ,@slot-names))
     (defmethod decode-object-allocate ((class (eql ',class-name)) alist
                                        &key &allow-other-keys)
       (declare (ignore alist))
       (allocate-instance (find-class ',class-name)))
     (defmethod decode-object-initialize ((class (eql ',class-name))
                                          object alist
                                          &key &allow-other-keys)
       (alist-to-slots (alist object)
         ,@slot-names))))

