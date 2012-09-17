(defpackage :conspack
  (:nicknames #:cpk)
  (:use #:closer-common-lisp #:alexandria #:fast-io)
  (:export #:conspack-error #:conspack-error-value #:conspack-error-reason
           #:invalid-header #:duplicate-id #:invalid-package-name
           #:invalid-symbol-name #:invalid-symbol-package
           #:invalid-index #:unhandled-remote-reference
           #:invalid-tmap-type

           #:encode #:encode-to-stream
           #:make-r-ref #:with-remote-refs
           #:tracking-refs #:clear-refs #:make-ref-context
           #:with-index #:with-named-index
           #:make-index #:define-index #:delete-index
           #:find-in-index

           #:encode-object #:decode-object

           #:decode-value #:decode

           #:explain

           #:*conspack-interning* #:*conspack-max-bytes*
           #:*conspack-forward-refs*

           #:with-conspack-security))
