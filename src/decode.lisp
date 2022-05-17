(in-package :conspack)

 ;; Decode Properties

(defvar *current-properties* nil)

 ;; Reading

;;; We have to do some extra work to handle forward references (that is,
;;; references to a container within that container). DECODE-VALUE, the
;;; main function here, will return (values value T) in the absence of
;;; forward references. If there are forward references, it instead
;;; returns (values incomplete nil), where INCOMPLETE is one of these
;;; structures. The INCOMPLETE stores an uninitialized VALUE (unless we're
;;; dealing with a forward reference directly, in which case this is not
;;; possible), and DATA to be used to complete initialization of the value.
;;; DECODE-TAG will try to build an object containing references to itself,
;;; and get an INCOMPLETE. It will resolve the reference to the INCOMPLETE's
;;; VALUE, and then try completing initialization. Since only a TAG can
;;; introduce a forward reference, this should ensure everything is
;;; eventually resolved.
;;; We use this second value instead of just checking if the result is an
;;; INCOMPLETE in order to allow serializing INCOMPLETE values themselves;
;;; unlikely, but possible, and some kind of out-of-band signal is needed
;;; to deal with it. We could alternately wrap complete results in a
;;; different structure, but that would mean extra consing in the
;;; much more common case.
;;; Possible TODO:
;;; * If INCOMPLETEs kept track of which references they were waiting on,
;;;   the decoder could skip completion attempts that are doomed to fail.
;;;   Would let complex circular references be decoded more efficiently,
;;;   and maybe make the code simpler?
;;; * The alists passed to DECODE-OBJECT-ALLOCATE vary based on what are
;;;   forward references. This is because we attempt to get as complete
;;;   an alist as possible, meaning we can't construct tagged TMaps
;;;   before recursing into their keys and value. It might be possible to
;;;   include a mechanism for specifying which alist entries are required
;;;   for allocation to avoid this.

(defstruct (incomplete (:constructor make-incomplete (type value data)))
  type value data)

(defun decode-boolean (header)
  (use-bytes +platform-bytes+)
  (values (logbitp 0 header) t))

(defun decode-number (header buffer)
  (values
   (ecase (decode-number-header header)
     (:int8 (use-bytes 1) (read8 buffer))
     (:int16 (use-bytes 2) (read16-be buffer))
     (:int32 (use-bytes 4) (read32-be buffer))
     (:int64 (use-bytes 8) (read64-be buffer))
     (:uint8 (use-bytes 1) (readu8 buffer))
     (:uint16 (use-bytes 2) (readu16-be buffer))
     (:uint32 (use-bytes 4) (readu32-be buffer))
     (:uint64 (use-bytes 8) (readu64-be buffer))
     (:single-float
      (use-bytes 4)
      (ieee-floats:decode-float32 (readu32-be buffer)))
     (:double-float
      (use-bytes 8)
      (ieee-floats:decode-float64 (readu64-be buffer)))
     (:int128 (use-bytes 16) (read128-be buffer))
     (:uint128 (use-bytes 16) (readu128-be buffer))
     (:complex
      (use-bytes +platform-bytes+)
      (let ((realpart (decode-value buffer))
            (imagpart (decode-value buffer)))
        (complex realpart imagpart)))
     (:rational
      (use-bytes +platform-bytes+)
      (let ((num (decode-value buffer))
            (den (decode-value buffer)))
        (rationalize (/ num den)))))
   t))

(defun decode-size (bytes buffer)
  (ecase bytes
    (1 (readu8 buffer))
    (2 (readu16-be buffer))
    (4 (readu32-be buffer))))

(defun decode-string (header buffer &optional len)
  (let ((len (or len (decode-size (size-bytes header) buffer))))
    (use-bytes len)
    (let ((octets (make-octet-vector len)))
      (fast-read-sequence octets buffer)
      (values (trivial-utf-8:utf-8-bytes-to-string octets) t))))

(defun decode-list (header buffer &optional len)
  (let ((len (or len (decode-size (size-bytes header) buffer)))
        (fixed-header (when (container-fixed-p header)
                        (fast-read-byte buffer))))
    (container-precheck-bytes len fixed-header)
    (when (< len 2)
      (error 'invalid-size :value len :reader "Length of LIST must be >= 2"))
    (loop with first = (cons nil nil)
          with incomplete = nil
          for i below (1- len)
          for cons = first then (setf (cdr cons) (cons nil nil))
          do (setf (car cons)
                   (multiple-value-bind (value complete-p)
                       (decode-value buffer fixed-header)
                     (unless complete-p (push cons incomplete))
                     value))
          finally (return
                    (multiple-value-bind (last complete-p)
                        (decode-value buffer fixed-header)
                      ;; We use the vector as a special mark indicating
                      ;; the CDR needs to be completed.
                      (unless complete-p (push (vector cons) incomplete))
                      (setf (cdr cons) last)
                      (if incomplete
                          (values (make-incomplete :list first incomplete) nil)
                          (values first t)))))))

(defun try-list-completion (incomplete)
  (let ((conses (incomplete-data incomplete))
        ;; A bit inefficient in that we cons up a new list when we don't
        ;; strictly need to, but this is a rare case anyway.
        new-conses)
    (loop for cons in conses
          do (if (vectorp cons)
                 ;; CDR
                 (let* ((vec cons)
                        (cons (aref vec 0)))
                   (multiple-value-bind (new complete-p)
                       (try-completion (cdr cons))
                     (if complete-p
                         (setf (cdr cons) new)
                         (push vec new-conses))))
                 ;; CAR
                 (multiple-value-bind (new complete-p)
                     (try-completion (car cons))
                   (if complete-p
                       (setf (car cons) new)
                       (push cons new-conses)))))
    (cond (new-conses
           (setf (incomplete-data incomplete) new-conses)
           (values incomplete nil))
          (t (values (incomplete-value incomplete) t)))))

(defun decode-vector (header buffer &optional len)
  (let* ((len (or len (decode-size (size-bytes header) buffer)))
         (fixed-header (when (container-fixed-p header)
                         (fast-read-byte buffer)))
         (fixed-type
           (when fixed-header
             (number-type-to-lisp
              (decode-number-header fixed-header)))))
    (container-precheck-bytes len fixed-header)
    (let ((v (make-array len :element-type (or fixed-type t))))
      (loop with incomplete = nil
            for i below len
            do (setf (aref v i)
                     (multiple-value-bind (value complete-p)
                         (decode-value buffer fixed-header)
                       (unless complete-p (push i incomplete))
                       value))
            finally (return
                      (if incomplete
                          (values (make-incomplete :vector v incomplete) nil)
                          (values v t)))))))

(defun try-vector-completion (incomplete)
  (let ((vec (incomplete-value incomplete))
        (indices (incomplete-data incomplete))
        new-indices)
    (loop for index in indices
          for subincomplete = (aref vec index)
          do (multiple-value-bind (new complete-p)
                 (try-completion subincomplete)
               (if complete-p
                   (setf (aref vec index) new)
                   (push index new-indices))))
    (cond (new-indices
           (setf (incomplete-data incomplete) new-indices)
           (values incomplete nil))
          (t (values vec t)))))

(defun decode-map (header buffer &optional len)
  (let* ((len (or len (decode-size (size-bytes header) buffer)))
         (fixed-header (when (container-fixed-p header)
                         (fast-read-byte buffer)))
         (hash (make-hash-table :test (if (eq :equal (getf *current-properties* :test))
                                          'equal 'eql)
                                :size len)))
    (container-precheck-bytes (* 2 len) fixed-header)
    (loop with incomplete = nil
          for i below len
          do (multiple-value-bind (key key-complete-p)
                 (decode-value buffer fixed-header)
               (multiple-value-bind (value value-complete-p)
                   (decode-value buffer)
                 (setf (gethash key hash) value)
                 (cond ((and key-complete-p value-complete-p))
                       (key-complete-p
                        (push (list :value key value) incomplete))
                       (value-complete-p
                        (push (list :key key) incomplete))
                       (t (push (list :key-value key value) incomplete)))))
          finally (return
                    (if incomplete
                        (values (make-incomplete :map hash incomplete) nil)
                        (values hash t))))))

(defun try-map-completion (incomplete)
  (let ((hash (incomplete-value incomplete))
        (data (incomplete-data incomplete))
        new-data)
    (loop for datum in data
          for which = (car datum)
          do (ecase which
               (:key (let ((key (second datum)))
                       (multiple-value-bind (new complete-p)
                           (try-completion key)
                         (if complete-p
                             (let ((value (gethash key hash)))
                               ;; Delete the incomplete key first in an
                               ;; effort to avoid expanding the table.
                               (remhash key hash)
                               (setf (gethash new hash) value))
                             (push datum new-data)))))
               (:value (let ((key (second datum)) (val (third datum)))
                         (multiple-value-bind (new complete-p)
                             (try-completion val)
                           (if complete-p
                               (setf (gethash key hash) new)
                               (push datum new-data)))))
               (:key-value
                (let ((key (second datum)) (val (third datum)))
                  (multiple-value-bind (new-val val-complete-p)
                      (try-completion val)
                    (multiple-value-bind (new-key key-complete-p)
                        (try-completion key)
                      (when key-complete-p (remhash key hash))
                      (setf (gethash new-key hash) new-val)
                      (cond ((and key-complete-p val-complete-p))
                            (key-complete-p
                             (push (list :value new-key new-val) new-data))
                            (val-complete-p
                             (push (list :key new-key) new-data))
                            (t (push datum new-data)))))))))
    (cond (new-data
           (setf (incomplete-data incomplete) data)
           (values incomplete nil))
          (t (values hash t)))))

(defun decode-tmap (header buffer &optional len)
  (let* ((len (or len (decode-size (size-bytes header) buffer)))
         (fixed-header (when (container-fixed-p header)
                         (fast-read-byte buffer)))
         (class (decode-value buffer)))
    (container-precheck-bytes (* 2 len) fixed-header)
    (unless (symbolp class)
      ;; This additionally means that the class cannot be an incomplete.
      (error 'invalid-tmap-type :value class :reason "Not a symbol"))
    (loop with incomplete = nil
          with alloc-pairs = nil
          for i below len
          collect (multiple-value-bind (key key-complete-p)
                      (decode-value buffer fixed-header)
                    (multiple-value-bind (value value-complete-p)
                        (decode-value buffer)
                      (let ((pair (cons key value))
                            (akey key) (avalue value) (complete-enough-p t))
                        ;; Push any incompleteness fixups.
                        ;; Push the pair to the alist for -allocate
                        ;; unless either is a totally unresolved ref.
                        (unless key-complete-p
                          (push (cons :key pair) incomplete)
                          (if (eq (incomplete-type key) :ref)
                              (setf complete-enough-p nil)
                              (setf akey (incomplete-value key))))
                        (unless value-complete-p
                          (push (cons :value pair) incomplete)
                          (if (eq (incomplete-type value) :ref)
                              (setf complete-enough-p nil)
                              (setf avalue (incomplete-value value))))
                        (when complete-enough-p
                          (push (if (and (eq key akey) (eq value avalue))
                                    pair
                                    (cons akey avalue))
                                alloc-pairs))
                        pair)))
            into pairs
          finally (return
                    (let ((obj (decode-object-allocate class alloc-pairs)))
                      (cond
                        (incomplete
                         (values (make-incomplete
                                  :tmap obj (list* pairs class incomplete))
                                 nil))
                        (t
                         (decode-object-initialize obj class pairs)
                         (values obj t))))))))

(defun try-tmap-completion (incomplete)
  (declare (optimize debug))
  (let* ((obj (incomplete-value incomplete))
         (data (incomplete-data incomplete))
         (pairs (first data)) (class (second data)) (work (cddr data))
         new-data)
    (assert work)
    (loop for datum in work
          for (which . pair) = datum
          for inc = (ecase which (:key (car pair)) (:value (cdr pair)))
          do (multiple-value-bind (new complete-p)
                 (try-completion inc)
               (if complete-p
                   (ecase which
                     (:key (setf (car pair) new))
                     (:value (setf (cdr pair) new)))
                   (push datum new-data))))
    (cond (new-data
           (setf (incomplete-data incomplete) (list* pairs class new-data))
           (values incomplete nil))
          (t
           (decode-object-initialize obj class pairs)
           (values obj t)))))

(defun decode-container (header buffer &optional len)
  (let ((type (decode-container-type header)))
    (ecase type
      (:list (decode-list header buffer len))
      (:vector (decode-vector header buffer len))
      (:map (decode-map header buffer len))
      (:tmap (decode-tmap header buffer len)))))

(defun decode-ref-id (header buffer)
  (if (logbitp 4 header)
      (logand header #xF)
      (decode-size (size-bytes header) buffer)))

(defun decode-ref (header buffer)
  (let ((id (decode-ref-id header buffer)))
    (use-bytes +platform-bytes+)
    (multiple-value-bind (obj exists-p) (get-ref id)
      (cond (exists-p (values obj t))
            (t
             (unless *conspack-forward-refs*
               (error 'invalid-forward-ref
                      :value id
                      :reason "Forward refs restricted."))
             (values (make-incomplete :ref nil id) nil))))))

(defun try-ref-completion (incomplete)
  (multiple-value-bind (obj exists-p) (get-ref (incomplete-data incomplete))
    (if exists-p (values obj t) (values incomplete nil))))

(defun decode-r-ref (header buffer)
  (declare (ignore header))
  (use-bytes +platform-bytes+)
  (let ((object (decode-value buffer)))
    (values (funcall *remote-ref-fun* object) t)))

(defun decode-pointer (header buffer &optional len)
  (let ((len (or len (size-bytes header))))
    (use-bytes +platform-bytes+ 2)
    (values (pointer (decode-size len buffer) (* 8 len)) t)))

(defun decode-tag (header buffer)
  (let ((id (decode-ref-id header buffer)))
    (multiple-value-bind (obj complete-p) (decode-value buffer)
      (cond (complete-p
             ;; Store the complete value in the ref table for later refs.
             (add-ref id obj)
             (values obj t))
            (t
             ;; Make sure we don't have a #1=#1# situation.
             (when (eq (incomplete-type obj) :ref)
               (error 'vacuous-ref
                      :value id
                      :reason "Tag's value cannot be a forward ref."))
             ;; Store the allocated value in the ref table and try completion.
             ;; (try-completion returns the same values decode-value should.)
             (add-ref id (incomplete-value obj))
             (try-completion obj))))))

(defun decode-cons (header buffer)
  (declare (ignore header))
  (use-bytes +platform-bytes+ 2)
  (multiple-value-bind (car car-complete-p) (decode-value buffer)
    (multiple-value-bind (cdr cdr-complete-p) (decode-value buffer)
      (let ((cons (cons car cdr)))
        (cond ((and car-complete-p cdr-complete-p) (values cons t))
              (car-complete-p
               (values (make-incomplete :cons cons (cons t nil)) nil))
              (cdr-complete-p
               (values (make-incomplete :cons cons (cons nil t)) nil))
              (t (values (make-incomplete :cons cons (cons nil nil)) nil)))))))

(defun try-cons-completion (incomplete)
  (let ((cons (incomplete-value incomplete))
        (which (incomplete-data incomplete)))
    (unless (car which)
      (multiple-value-bind (car car-complete-p) (try-completion (car cons))
        (when car-complete-p
          (setf (car cons) car (car which) car-complete-p))))
    (unless (cdr which)
      (multiple-value-bind (cdr cdr-complete-p) (try-completion (cdr cons))
        (when cdr-complete-p
          (setf (cdr cons) cdr (cdr which) cdr-complete-p))))
    (if (and (car which) (cdr which))
        (values cons t)
        (values incomplete nil))))

(defun decode-package (header buffer)
  (declare (ignore header))
  (use-bytes +platform-bytes+)
  (let ((package-name (decode-value buffer)))
    (unless (stringp package-name)
      (error 'invalid-package-name :value package-name :reason "Not a string."))
    (let ((package (find-package package-name)))
      (unless package
        (error 'invalid-package-name :value package-name
                                     :reason "Package does not exist."))
      (values package t))))

(defvar *intern-symbols* nil)

(defun decode-symbol (header buffer)
  (let ((symbol-name (decode-value buffer)))
    (use-bytes +platform-bytes+)
    (unless (stringp symbol-name)
      (error 'invalid-symbol-name :value symbol-name :reason "Not a string."))
    (let ((package (if (keyword-p header)
                       (find-package :keyword)
                       (decode-value buffer))))
      (values (if package
                  (if *intern-symbols*
                      (intern symbol-name package)
                      (multiple-value-bind (symbol status)
                          (find-symbol symbol-name package)
                        (if status
                            symbol
                            (make-symbol symbol-name))))
                  (make-symbol symbol-name))
              t))))

(defmacro with-interning (nil &body body)
  `(let ((*intern-symbols* t)) ,@body))

(defun decode-character (header buffer)
  (let ((len (logand header #b11)))
    (use-bytes len)
    (let ((octets (make-octet-vector len)))
      (fast-read-sequence octets buffer)
      (values (aref (trivial-utf-8:utf-8-bytes-to-string octets) 0) t))))

(defun decode-index (header buffer)
  (let* ((id (decode-ref-id header buffer))
         (sym (find-in-index id)))
    (unless sym
      (error 'invalid-index :value id :reason "ID not found in index"))
    (use-bytes +platform-bytes+)
    (values sym t)))

(defun mark-properties (next-object)
  (unless (or (characterp next-object)
              (numberp next-object))
    (setf (gethash next-object *properties*)
          *current-properties*)))

(defun decode-properties (header buffer)
  (declare (ignore header))
  (multiple-value-bind (*current-properties* cpp) (decode-value buffer)
    (unless cpp
      (error 'forward-referenced-properties
             :value *current-properties*
             :reason "Properties cannot be a forward ref."))
    (multiple-value-bind (next-object complete-p) (decode-value buffer)
      (cond (complete-p (mark-properties next-object))
            ((not (eq (incomplete-type next-object) :ref))
             (mark-properties (incomplete-value next-object)))
            (t (error 'vacuous-properties
                      :value (incomplete-data next-object)
                      :reason "Cannot set properties of forward ref.")))
      (values next-object complete-p))))

(defun decode-value (buffer &optional header)
  (let ((header (or header (fast-read-byte buffer))))
    (ecase (decode-header header)
      (:boolean (decode-boolean header))
      (:number (decode-number header buffer))
      (:string (decode-string header buffer))
      (:container (decode-container header buffer))
      (:tag (decode-tag header buffer))
      (:ref (decode-ref header buffer))
      (:r-ref (decode-r-ref header buffer))
      (:pointer (decode-pointer header buffer))
      (:cons (decode-cons header buffer))
      (:package (decode-package header buffer))
      (:symbol (decode-symbol header buffer))
      (:character (decode-character header buffer))
      (:properties (decode-properties header buffer))
      (:index (decode-index header buffer)))))

(defun try-completion (incomplete)
  (ecase (incomplete-type incomplete)
    (:list (try-list-completion incomplete))
    (:vector (try-vector-completion incomplete))
    (:map (try-map-completion incomplete))
    (:tmap (try-tmap-completion incomplete))
    (:cons (try-cons-completion incomplete))
    (:ref (try-ref-completion incomplete))))

(defun %decode (buffer)
  (with-properties ()
    (tracking-refs ()
      (multiple-value-bind (value complete-p) (decode-value buffer)
        (if complete-p
            value
            (error 'toplevel-forward-ref :value value
                   :reason "There are unresolved forward reference(s)."))))))

(defun decode (byte-vector &optional (offset 0))
  (with-fast-input (buffer byte-vector nil offset)
    (values (%decode buffer) (fast-io:buffer-position buffer))))

(defun decode-stream (stream &optional (offset 0))
  (with-fast-input (buffer nil stream offset)
    (values (%decode buffer) (fast-io:buffer-position buffer))))

(defun decode-file (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let (eof)
      (tracking-refs ()
        (loop as object = (handler-case
                              (decode-stream stream)
                            (end-of-file () (setf eof t) (values)))
              until eof
              collect object)))))

