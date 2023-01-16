(in-package :conspack.test)

(5am:def-suite conspack)
(5am:in-suite conspack)

(defmacro cycle (value)
  (alexandria:once-only (value)
    `(5am:is (equalp ,value (decode (encode ,value))))))

(defun cycle/refs (value)
  (tracking-refs () (decode (prog1 (encode value) (clear-refs)))))

(5am:test numbers
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
          (cycle #C(1/2 1/2)))

(5am:test strings
          (cycle "hi")
          ;; Taken from the utf-8 test file
          (cycle "Οὐχὶ ταὐτὰ παρίσταταί μοι γιγνώσκειν, ὦ ἄνδρες ᾿Αθηναῖοι")
          (cycle "ሰማይ አይታረስ ንጉሥ አይከሰስ።")
          (cycle "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ"))

(5am:test containers
          (let ((hash (make-hash-table)))
            (setf (gethash 5 hash) 50
                  (gethash 6 hash) 60)
            (cycle hash))
          (cycle '(1 2 3))
          (cycle '(50000 60000 70000))
          (cycle #(1 2 3))
          (cycle #(500 600 700))
          (cycle '((1 2) (3 4)))
          (cycle #(#(1 2) (3 4)))
          (cycle '(1 2 . 3)))

(5am:test refs
          (let* ((ref0 (list 1 2 3))
                 (l0 (list 0 ref0 ref0))
                 (cl0 (cycle/refs l0))
                 (ref1 (list 42))
                 (vec0 (map 'vector #'identity (list ref0 ref0)))
                 (cvec0 (cycle/refs vec0))
                 (hash (make-hash-table)))
            (setf (cdr ref1) ref1)
            (setf (gethash hash hash) hash)
            (5am:is (equalp cl0 l0))
            (5am:is (eq (second cl0) (third cl0)))
            (let ((cref1 (cycle/refs ref1)))
              (5am:is (= 42 (car cref1)))
              (5am:is (eq cref1 (cdr cref1))))
            (5am:is (equalp vec0 cvec0))
            (5am:is (eq (aref cvec0 0) (aref cvec0 1)))
            (let ((chash (cycle/refs hash)))
              (eq chash (gethash chash chash)))))

(5am:test packages-and-symbols
          (cycle (find-package :common-lisp))
          (5am:is (string= (package-name (decode
                                          (encode
                                           (find-package :common-lisp))))
                           "COMMON-LISP"))
          (cycle 'foo)
          (cycle :foo)
          (let ((sym0 (decode (encode '#:|foo|))))
            (5am:is (null (symbol-package sym0)))
            (5am:is (string= "foo" (symbol-name sym0)))))

(5am:test fixed-containers
          (cycle (make-array 3 :element-type '(unsigned-byte 16)
                             :initial-contents '(200 300 400))))

(5am:test indices
          (with-index (:a :b)
            (cycle :a)
            (cycle '(:a :b))
            (cycle #(:a :b))
            (cycle '(:a :b :c))))

(5am:test remote-refs
          (with-remote-refs (lambda (value) (list :r-ref value))
            (5am:is (equal '(:r-ref 42)
                           (decode (encode (make-r-ref :value 42)))))
            (5am:is (equal '(1 2 (:r-ref 3))
                           (decode
                            (encode (list 1 2 (make-r-ref :value 3))))))))

;;; Note this is for simple testing, and a terrible example, since
;;; there is no value checking.  You should always make sure values
;;; are correct and assume the remote end is trying to break your
;;; code.

(defstruct point x y)

(defmethod encode-object append ((v point) &key &allow-other-keys)
  `((:x . ,(point-x v))
    (:y . ,(point-y v))))

(defmethod decode-object-allocate ((c (eql 'point)) alist
                                   &key &allow-other-keys)
  (declare (ignore alist))
  (make-point))

(defmethod decode-object-initialize progn ((o point) class alist
                                           &key &allow-other-keys)
  (declare (ignore class))
  (setf (point-x o) (cdr (assoc :x alist))
        (point-y o) (cdr (assoc :y alist))))

;;;;;;

(5am:test tmaps
          (cycle (make-point :x 1 :y 2)))

(5am:test tmaps-and-indices
          (with-index (point :x :y)
            (cycle (make-point :x 1 :y 2))))

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
