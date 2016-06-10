(in-package :conspack)

 ;; Constants

(defconstant +boolean-header+      #b00000000) ; #b0000000n
(defconstant +number-header+       #b00010000) ; #b0001nnnn
(defconstant +container-header+    #b00100000) ; #b001xxfnn
(defconstant +string-header+       #b01000000) ; #b010000nn
(defconstant +ref-header+          #b01100000) ; #b011fdddd
(defconstant +r-ref-header+        #b01100100) ; #b01100100
(defconstant +pointer-header+      #b01101000) ; #b011010nn
(defconstant +tag-header+          #b11100000) ; #b111fdddd
(defconstant +cons-header+         #b10000000) ; #b10000000
(defconstant +package-header+      #b10000001) ; #b10000001
(defconstant +symbol-header+       #b10000010) ; #b1000001f
(defconstant +character-header+    #b10000100) ; #b100001nn
(defconstant +properties-header+   #b10001000) ; #b10001000
(defconstant +index-header+        #b10100000) ; #b101fdddd

(defconstant +int8+ #x0)
(defconstant +int16+ #x1)
(defconstant +int32+ #x2)
(defconstant +int64+ #x3)
(defconstant +uint8+ #x4)
(defconstant +uint16+ #x5)
(defconstant +uint32+ #x6)
(defconstant +uint64+ #x7)
(defconstant +single-float+ #x8)
(defconstant +double-float+ #x9)
(defconstant +int128+ #xA)
(defconstant +uint128+ #xB)
(defconstant +complex+ #xC)
(defconstant +rational+ #xF)

(defconstant +container-vector+ #b00100000)
(defconstant +container-list+   #b00101000)
(defconstant +container-map+    #b00110000)
(defconstant +container-tmap+   #b00111000)
(defconstant +container-fixed+  #b00000100)

(defconstant +reftag-inline+  #b00010000)
(defconstant +symbol-keyword+ #b00000001)

 ;; Header testing

(declaim (ftype header-test-fun
                boolean-p number-p container-p string-p ref-p
                cons-p package-p symbol-p index-p)
         (inline boolean-p number-p container-p string-p ref-p
                 cons-p package-p symbol-p index-p))

(defun boolean-p (n)
  (= 0 (ash n -1)))

(defun number-p (n)
  (= #b00010000 (logand n #b11110000)))

(defun container-p (n)
  (= #b00100000 (logand n #b11100000)))

(defun container-fixed-p (n)
  (logbitp 2 n))

(defun string-p (n)
  (= #b01000000 (logand n #b11111100)))

(defun ref-p (n)
  (or
   ;; id-follows
   (= #b01100000 (logand n #b11111100))
   ;; id-inline
   (= #b01110000 (logand n #b11110000))))

(defun remote-ref-p (n)
  (= +r-ref-header+ n))

(defun pointer-header-p (n)
  (= +pointer-header+ (logand n #b111111100)))

(defun tag-p (n)
  (or
   ;; id-follows
   (= #b11100000 (logand n #b11111100))
   ;; id-inline
   (= #b11110000 (logand n #b11110000))))

(defun tag-inline-p (n)
  (logbitp 4 n))

(defun cons-p (n)
  (= n #b10000000))

(defun package-p (n)
  (= n #b10000001))

(defun symbol-p (n)
  (= #b10000010 (logand n #b11111110)))

(defun character-p (n)
  (= +character-header+ (logand n #b11111100)))

(defun properties-p (n)
  (= +properties-header+ n))

(defun keyword-p (n)
  (and (symbol-p n)
       (logbitp 0 n)))

(defun index-p (n)
  (= +index-header+
     (logand +index-header+ n)))

 ;; Making headers

(defun bits-size-type (bits)
  (ecase bits
    (8 #b00)
    (16 #b01)
    (32 #b10)))

(defun len-size-type (len)
  (etypecase len
    ((unsigned-byte 8)  #b00)
    ((unsigned-byte 16) #b01)
    ((unsigned-byte 32) #b10)))

(defun number-type (number)
  "Find the closest/smallest encoding type for NUMBER."
  (etypecase number
    ((signed-byte 8)     :int8)
    ((unsigned-byte 8)   :uint8)
    ((signed-byte 16)    :int16)
    ((unsigned-byte 16)  :uint16)
    ((signed-byte 32)    :int32)
    ((unsigned-byte 32)  :uint32)
    ((signed-byte 64)    :int64)
    ((unsigned-byte 64)  :uint64)
    ((signed-byte 128)   :int128)
    ((unsigned-byte 128) :uint128)
    (single-float        :single-float)
    (double-float        :double-float)
    (complex             :complex)
    (ratio               :rational)))

(defun number-type-to-lisp (type)
  "Find the lisp type (e.g., SINGLE-FLOAT) for a specified number
type (e.g., :SINGLE-FLOAT).  The inverse of NUMBER-TYPE."
  (case type
    (:int8           '(signed-byte 8))
    (:uint8          '(unsigned-byte 8))
    (:int16          '(signed-byte 16))
    (:uint16         '(unsigned-byte 16))
    (:int32          '(signed-byte 32))
    (:uint32         '(unsigned-byte 32))
    (:int64          '(signed-byte 64))
    (:uint64         '(unsigned-byte 64))
    (:int128         '(signed-byte 128))
    (:uint128        '(unsigned-byte 128))
    (:single-float   'single-float)
    (:double-float   'double-float)
    (:complex        'complex)
    (:rational       'ratio)))

(defun number-header (type)
  (when type
    (logior +number-header+
            (ecase type
              (:int8 #x0) (:int16 #x1) (:int32 #x2) (:int64 #x3)
              (:uint8 #x4) (:uint16 #x5) (:uint32 #x6) (:uint64 #x7)
              (:single-float #x8) (:double-float #x9)
              (:int128 #xA) (:uint128 #xB)
              (:complex #xC) (:rational #xF)))))

(defun container-type (type)
  (ecase type
    (:vector +container-vector+)
    (:list   +container-list+)
    (:map    +container-map+)
    (:tmap   +container-tmap+)))

(defun container-header (type fixed-p)
  (let ((type-bits (container-type type))
        (fixed-bits (if fixed-p +container-fixed+ 0)))
    (logior type-bits fixed-bits)))

;;; Yes, this does assume the lisp calls types as ([UN]SIGNED-BYTE n)
;;; and not (INTEGER a b).  SBCL, CCL, and CLISP do.  This is
;;; considerably faster and less consing than consecutive SUBTYPEP.
(defun find-fixed-type (type)
  (unless (eq type t)
    (cond
      ((and (consp type) (eq 'signed-byte (car type)))
       (case (cadr type)
         (8 :int8) (16 :int16) (32 :int32) (64 :int64) (128 :int128)))
      ((and (consp type) (eq 'unsigned-byte (car type)))
       (case (cadr type)
         (8 :uint8) (16 :uint16) (32 :uint32) (64 :uint64) (128 :uint128)))
      ((eq 'single-float type) :single-float)
      ((eq 'double-float type) :double-float))))

(defun type-header (type)
  (ecase type
    ((:int8 :int16 :int32 :int64 :int128
      :uint8 :uint16 :uint32 :uint64 :uint128
      :single-float :double-float :complex :rational)
     (number-header type))
    ((:string))))

(defun symbol-header (value)
  (if (keywordp value)
      (logior +symbol-header+ +symbol-keyword+)
      +symbol-header+))

(defun character-header (c)
  (logior +character-header+
          (length (trivial-utf-8:string-to-utf-8-bytes (string c)))))

 ;; Decode

(declaim (ftype header-decode-fun
                decode-header decode-number-header))

(defun decode-header (h)
  (cond
    ((boolean-p h) :boolean)
    ((number-p h) :number)
    ((container-p h) :container)
    ((string-p h) :string)
    ((ref-p h) :ref)
    ((remote-ref-p h) :r-ref)
    ((pointer-header-p h) :pointer)
    ((tag-p h) :tag)
    ((cons-p h) :cons)
    ((package-p h) :package)
    ((symbol-p h) :symbol)
    ((character-p h) :character)
    ((properties-p h) :properties)
    ((index-p h) :index)
    (t (error 'invalid-header :value h :reason "unknown type"))))

(defun decode-number-header (h)
  (case (logand #x0F h)
    (#x0 :int8) (#x1 :int16) (#x2 :int32) (#x3 :int64)
    (#x4 :uint8) (#x5 :uint16) (#x6 :uint32) (#x7 :uint64)
    (#x8 :single-float) (#x9 :double-float)
    (#xA :int128) (#xB :uint128)
    (#xC :complex) (#xF :rational)
    (t (error 'invalid-header :value h :reason "reserved number type"))))

(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 8))
                size-type size-bytes)
         (inline size-type size-bytes))
(defun size-type (header)
  (logand #b11 header))

(defun size-bytes (header)
  (case (size-type header)
    (#b00 1)
    (#b01 2)
    (#b10 4)
    (t (error 'invalid-header :value header :reason "invalid size-bytes"))))

(defun number-size (h)
  (case (logand #x0F h)
    (#x0 1) (#x1 2) (#x2 4) (#x3 8)
    (#x4 1) (#x5 2) (#x6 4) (#x7 8)
    (#x8 4) (#x9 8) (#xA 16) (#xB 16)
    (#xC 16) (#xF 16) ;; more of an estimate, really
    (t (error 'invalid-header :value h :reason "reserved number type"))))

(defun decode-container-type (header)
  (case (ldb (byte 2 3) header)
    (#b00 :vector)
    (#b01 :list)
    (#b10 :map)
    (#b11 :tmap)))
