(cl:eval-when (:load-toplevel :execute)
  (asdf:load-system :fiveam)
  (asdf:load-system :checkl))

(defsystem :cl-conspack-test
  :description "Tests for cl-conspack"

  :depends-on (:cl-conspack :checkl)

  :pathname "t"
  :serial t

  :components
  ((:file "package")

   (checkl:tests "encode-decode")
   (checkl:test-values "test-values"
                       :package :conspack.test)))

(checkl:define-test-op :cl-conspack :cl-conspack-test)
(checkl:define-test-op :cl-conspack-test)
