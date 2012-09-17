(in-package :conspack.test)

(defun cycle (value)
  (let ((output (decode (encode value))))
    (values output (equalp value output))))

(defun cycle-refs (value)
  (tracking-refs ()
    (let ((encoded (encode value)))
      (clear-refs)
      (decode encoded))))

(check (:name 'numbers :output-p t)
  (results
   (cycle 42)
   (cycle -42)
   (cycle 128)
   (cycle 256)
   (cycle (expt 2 31))
   (cycle (expt 2 63))
   (cycle (expt 2 127))
   (cycle 5s0)
   (cycle 10d0)
   (cycle #C(0 1))
   (cycle 1/2)
   (cycle 5999999999999/71111111111111)
   (cycle #C(1/2 1/2))))

(check (:name :strings :output-p t)
  (results
   (cycle "hi")
   ;; Taken from the utf-8 test file
   (cycle "Οὐχὶ ταὐτὰ παρίσταταί μοι γιγνώσκειν, ὦ ἄνδρες ᾿Αθηναῖοι")
   (cycle "ሰማይ አይታረስ ንጉሥ አይከሰስ።")
   (cycle "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ")))

(check (:name :containers :output-p t)
  (let ((hash (make-hash-table)))
    (setf (gethash 5 hash) 50
          (gethash 6 hash) 60)
    (results
     (cycle '(1 2 3))
     (cycle '(50000 60000 70000))
     (cycle #(1 2 3))
     (cycle #(500 600 700))
     (cycle '((1 2) (3 4)))
     (cycle #(#(1 2) (3 4)))
     (nth-value 1 (cycle '(1 2 . 3)))
     (encode hash))))

(check (:name :refs :output-p t)
  (let* ((*print-circle* t)
         (ref0 (list 1 2 3))
         (l0 (list 0 ref0 ref0))
         (l1 (cycle-refs l0))
         (ref1 (list 42))
         (vec0 (map 'vector #'identity (list ref0 ref0)))
         (hash (make-hash-table)))
    (setf (cdr ref1) ref1)
    (setf (gethash hash hash) hash)
    (results
     (tracking-refs () (encode l0))
     (cycle-refs l0)
     (eq (cadr l1) (caddr l1))
     (tracking-refs () (encode ref1))
     (write-to-string (cycle-refs ref1))
     (cycle-refs vec0)
     (tracking-refs () (encode vec0))
     (tracking-refs () (encode hash))
     (let ((hash2 (cycle-refs hash)))
       (eq hash2 (gethash hash2 hash2))))))

(check (:name :packages-and-symbols :output-p t)
  (results
   (encode (find-package :common-lisp))
   (coerce (package-name (cycle (find-package :common-lisp)))
           '(simple-array character (*)))
   (cycle 'foo)
   (cycle :foo)
   (let ((sym0 (decode (encode '#:|foo|))))
     (and (null (symbol-package sym0))
          (string= "foo" (symbol-name sym0))))))

(check (:name :fixed-containers :output-p t)
  (let ((fixed-vector (make-array 3 :element-type '(unsigned-byte 16)
                                    :initial-contents '(200 300 400))))
    (results
     (encode fixed-vector)
     (decode (encode fixed-vector)))))

(check (:name :indexes :output-p t)
  (with-index (:a :b)
    (results
     (encode :a)
     (cycle :a)
     (cycle '(:a :b))
     (encode #(:a :b))
     (encode '(:a :b :c)))))

(check (:name :remote-refs :output-p t)
  (with-remote-refs (lambda (value) (list :r-ref value))
    (results
     (encode (make-r-ref :value 42))
     (decode (encode (make-r-ref :value 42)))
     (decode (encode (list 1 2 (make-r-ref :value 3)))))))

;;; Note this is for simple testing, and a terrible example, since
;;; there is no value checking.  You should always make sure values
;;; are correct and assume the remote end is trying to break your
;;; code.

(defstruct point x y)

(defmethod encode-object ((v point) &key &allow-other-keys)
  `((:x . ,(point-x v))
    (:y . ,(point-y v))))

(defmethod decode-object ((c (eql 'point)) value &key &allow-other-keys)
  (make-point :x (cdr (assoc :x value))
              :y (cdr (assoc :y value))))

;;;;;;

(check (:name :tmaps :output-p t)
  (results
   (encode (make-instance 'point :x 0 :y 0))
   (cycle (make-instance 'point :x 1 :y 2))))

(check (:name :tmaps-and-indexes :output-p t)
  (with-index (point :x :y)
    (results
     (encode (make-instance 'point :x 0 :y 0))
     (cycle (make-instance 'point :x 1 :y 2)))))

#+-
(with-index (point :x :y)
  (let ((vec (encode (make-point :x 0 :y 1))))
    (tracking-refs ()
      (:bench (600000)
        (decode vec)
        (clear-refs)))))

#+-
(let ((str (json:encode-json-to-string (make-point :x 0 :y 1))))
  (:bench (6000)
    (json:decode-json-from-string str)))

;;; (checkl-store)
