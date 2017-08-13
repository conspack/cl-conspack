(in-package :conspack)

(defgeneric encode-object (object &key &allow-other-keys)
  (:documentation "Return an alist for `OBJECT` which will be used for
key-value pairs for a Typed Map (tmap).  The class of `OBJECT` will
be encoded along with these and used by `DECODE-OBJECT` to recreate
the object."))

(defgeneric decode-object (class alist &key &allow-other-keys)
  (:documentation "Take the values in `ALIST` and recreate the object
of the given `CLASS`.  Methods should specialize on `CLASS (EQL symbol)`."))

(defmacro slots-to-alist ((instance) &body slot-names)
  "Produce an `ALIST` of slot-names-to-slot-values, suitable for
`ENCODE-OBJECT`."
  (if slot-names
    (alexandria:once-only (instance)
      `(list
         ,@(loop for slot-name in slot-names
                 collect `(cons ',slot-name (slot-value ,instance ',slot-name)))))
    nil))

(defmacro alist-to-slots ((alist &key instance class) &body slot-names)
  "Set slots via `(SETF (SLOT-VALUE ...))` based on the values of the
slots specified.

If `INSTANCE` is specified, slots are set on the provided value.

If `CLASS` is specified, trivially create an instance
via `(MAKE-INSTANCE 'CLASS)`, quoted.  If you need more complex
initialization, specify `:INSTANCE` with your own `MAKE-INSTANCE`.

You may not use both `:INSTANCE` and `:CLASS`."
  (when (and instance class)
    (error "Please specify only one of `INSTANCE` or `CLASS`."))
  (unless (or instance class)
    (error "You must specify one of `INSTANCE` or `CLASS`."))
  (alexandria:with-gensyms (object alist%)
    `(let ((,object ,(cond (class `(make-instance ',class))
                           (instance instance)))
           (,alist% ,alist))
       (declare (ignorable ,alist%))
       (prog1 ,object
         (setf
           ,@(loop for slot-name in slot-names
                   collect `(slot-value ,object ',slot-name)
                   collect `(aval ',slot-name ,alist%)))))))

(defmacro defencoding (class-name &body slot-names)
  "Trivially define `ENCODE-OBJECT` and `DECODE-OBJECT` to store and
load the given slots."
  `(eval-when (:load-toplevel :execute)
     (defmethod encode-object ((object ,class-name) &key &allow-other-keys)
       (slots-to-alist (object) ,@slot-names))
     (defmethod decode-object ((class (eql ',class-name)) alist
                               &key &allow-other-keys)
       (alist-to-slots (alist :class ,class-name)
         ,@slot-names))))

