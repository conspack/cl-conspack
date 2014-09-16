;; -*- lisp -*-
;;
;; Re-run this command to regenerate this file.  It'll overwrite
;; whatever is there, so make sure it's going to the right place:
#+(or)
(make-util:make-util '(:cl-conspack "util") :package "CONSPACK" :symbols
                     '(laconic:aval) :exportp nil)

;; ===================================================================
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CONSPACK")
    (make-package "CONSPACK" :use '(#:cl))))
(in-package "CONSPACK")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'aval)
    (defun aval (akey alist &key (key 'identity) (test 'eq))
      "Get the value for key `KEY` in `ALIST`"
      (cdr (assoc akey alist :key key :test test)))))
