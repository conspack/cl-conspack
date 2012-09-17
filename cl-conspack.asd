(defsystem :cl-conspack
  :description "CONSPACK implementation for Common Lisp"
  :author "Ryan Pavlik"
  :license "NewBSD"

  :depends-on (:closer-mop :alexandria :ieee-floats :trivial-utf-8
               :fast-io)

  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "types")
   (:file "secure")
   (:file "reftable")
   (:file "r-ref")
   (:file "headers")
   (:file "indexes")
   (:file "tmap")
   (:file "encode")
   (:file "decode")
   (:file "explain")))
