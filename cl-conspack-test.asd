(defsystem :cl-conspack-test
  :description "Tests for cl-conspack"

  :depends-on (:cl-conspack :fiveam)

  :pathname "t"
  :serial t

  :perform (test-op (o s)
             (uiop:symbol-call :fiveam :run!
                               (find-symbol "CONSPACK" "CONSPACK.TEST")))

  :components
  ((:file "package")
   (:file "encode-decode")))
