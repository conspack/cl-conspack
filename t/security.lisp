(in-package :conspack.test)

(check (:category :security :name :refs :output-p t)
  (let ((*print-circle* t)
        (circle (cons 42 nil)))
    (setf (cdr circle) circle)
    (with-conspack-security (:forward-refs nil)
      (results
       (write-to-string (cycle-refs circle))))))

(check (:category :security :name :size :output-p t)
  (results
   (with-conspack-security (:max-bytes 1024)
     (decode (fast-io:octets-from #(40 255))))))
