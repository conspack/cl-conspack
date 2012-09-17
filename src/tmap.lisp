(in-package :conspack)

(defgeneric encode-object (object &key &allow-other-keys)
  (:documentation "Return an alist for `OBJECT` which will be used for
key-value pairs for a Typed Map (tmap).  The class of `OBJECT` will
be encoded along with these and used by `DECODE-OBJECT` to recreate
the object."))

(defgeneric decode-object (class alist &key &allow-other-keys)
  (:documentation "Take the values in `ALIST` and recreate the object
of the given `CLASS`.  Methods should specialize on `CLASS (EQL symbol)`."))
