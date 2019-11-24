;;
;;  ANSI COMMON LISP: 18. Hash Tables
;;
(deftest make-hash-table.1
  (typep (make-hash-table) 'hash-table)
  t)

(deftest hash-table-p.1
  (hash-table-p
    (make-hash-table))
  t)

(deftest hash-table-p.2
  (hash-table-p 100)
  nil)

(deftest hash-table-count.1
  (let ((inst (make-hash-table)))
    (hash-table-count inst))
  0)

(deftest hash-table-count.2
  (let ((inst (make-hash-table)))
    (setf (gethash :hello inst) :value)
    (hash-table-count inst))
  1)

(deftest hash-table-count.3
  (let ((inst (make-hash-table)))
    (setf (gethash :hello inst) :value)
    (setf (gethash :aaa inst) :bbb)
    (setf (gethash :ccc inst) :value)
    (setf (gethash :hello inst) :value)
    (hash-table-count inst))
  3)

(deftest hash-table-rehash-size.1
  (realp
    (hash-table-rehash-size
      (make-hash-table)))
  t)

(deftest hash-table-rehash-size.2
  (hash-table-rehash-size
    (make-hash-table :rehash-size 10))
  10)

(deftest hash-table-rehash-size.3
  (= (hash-table-rehash-size
       (make-hash-table :rehash-size 2.0))
     2.0)
  t)

(deftest hash-table-rehash-threshold.1
  (realp
    (hash-table-rehash-threshold
      (make-hash-table)))
  t)

(deftest hash-table-rehash-threshold.2
  (= (hash-table-rehash-threshold
       (make-hash-table :rehash-threshold 0.25))
     0.25)
  t)

(deftest hash-table-size.1
  (integerp
    (hash-table-size
      (make-hash-table)))
  t)

(deftest hash-table-size.2
  (hash-table-size
    (make-hash-table :size 100))
  100)

(deftest hash-table-test.1
  (hash-table-test
    (make-hash-table))
  eql)

(deftest hash-table-test.2
  (hash-table-test
    (make-hash-table :test 'eq))
  eq)

(deftest hash-table-test.3
  (hash-table-p
    (make-hash-table :test #'equal))
  t)

(deftest gethash.1
  (let ((inst (make-hash-table :test 'eq)))
    (gethash :hello inst))
  nil nil)

(deftest gethash.2
  (let ((inst (make-hash-table :test 'eq)))
    (gethash :hello inst :aaa))
  :aaa nil)

(deftest gethash.3
  (let ((inst (make-hash-table :test 'eq)))
    (setf (gethash :hello inst) :value)
    (gethash :hello inst))
  :value t)

(deftest gethash.4
  (let ((inst (make-hash-table :test 'eq)))
    (setf (gethash :hello inst) :value)
    (gethash :hello inst :aaa))
  :value t)

(deftest gethash.5
  (let ((inst (make-hash-table :test 'eq)))
    (setf (gethash :hello inst :zzz) :value)
    (gethash :hello inst :aaa))
  :value t)

(deftest gethash.6
  (let ((inst (make-hash-table :test 'eq))
        result)
    (setf (gethash :hello inst (setq result 100)) :value)
    (gethash :hello inst :aaa)
    result)
  100)

(deftest gethash.7
  (let ((inst (make-hash-table :test 'eq)))
    (setf (gethash :aaa inst) 100)
    (setf (gethash :bbb inst) 200)
    (setf (gethash :ccc inst) 300)
    (setf (gethash :ddd inst) 400)
    (gethash :ccc inst))
  300 t)

(deftest gethash.8
  (let ((inst (make-hash-table :test 'equal)))
    (setf (gethash "aaa" inst) 100)
    (setf (gethash "bbb" inst) 200)
    (setf (gethash "ccc" inst) 300)
    (setf (gethash "ddd" inst) 400)
    (gethash "ccc" inst))
  300 t)

(deftest remhash.1
  (let ((inst (make-hash-table :test 'eq)))
    (setf (gethash :aaa inst) 100)
    (setf (gethash :bbb inst) 200)
    (setf (gethash :ccc inst) 300)
    (remhash :bbb inst))
  t)

(deftest remhash.2
  (let ((inst (make-hash-table :test 'eq)))
    (setf (gethash :aaa inst) 100)
    (setf (gethash :bbb inst) 200)
    (setf (gethash :ccc inst) 300)
    (remhash :bbb inst)
    (gethash :bbb inst))
  nil nil)

(deftest remhash.3
  (let ((inst (make-hash-table :test 'eq)))
    (setf (gethash :aaa inst) 100)
    (setf (gethash :bbb inst) 200)
    (setf (gethash :ccc inst) 300)
    (remhash :zzz inst))
  nil)

(deftest maphash.1
  (let (result)
    (maphash
      (lambda (x y)
        (declare (ignore x y))
        (setq result 100))
      (make-hash-table))
    result)
  nil)

(deftest maphash.2
  (let ((inst (make-hash-table :test 'eql))
        (a 0) (b 0))
    (setf (gethash 10 inst) 100)
    (setf (gethash 20 inst) 999)
    (setf (gethash 20 inst) 200)
    (setf (gethash 30 inst) 300)
    (maphash
      (lambda (x y)
        (setq a (+ a x))
        (setq b (+ b y)))
      inst)
    (values a b))
  60 600)

(deftest maphash.3
  (let ((inst (make-hash-table :test 'eql)))
    (setf (gethash 10 inst) 100)
    (maphash
      (lambda (x y)
        (declare (ignore x y))
        100)
      inst))
  nil)

(deftest with-hash-table-iterator.1
  (let ((inst (make-hash-table)))
    (with-hash-table-iterator
      (call inst)
      (call)))
  nil nil nil)

(deftest with-hash-table-iterator.2
  (let ((inst (make-hash-table)))
    (setf (gethash :aaa inst) :bbb)
    (with-hash-table-iterator
      (call inst)
      (call)))
  t :aaa :bbb)

(deftest with-hash-table-iterator.3
  (let ((inst (make-hash-table))
        (a 0) (b 0))
    (setf (gethash 10 inst) 100)
    (setf (gethash 20 inst) 200)
    (setf (gethash 30 inst) 300)
    (with-hash-table-iterator
      (call inst)
      (multiple-value-bind (x y z) (call) (when x (setq a (+ y a) b (+ z b))))
      (multiple-value-bind (x y z) (call) (when x (setq a (+ y a) b (+ z b))))
      (multiple-value-bind (x y z) (call) (when x (setq a (+ y a) b (+ z b))))
      (multiple-value-bind (x y z) (call) (when x (setq a (+ y a) b (+ z b))))
      (multiple-value-bind (x y z) (call) (when x (setq a (+ y a) b (+ z b)))))
    (values a b))
  60 600)

(deftest clrhash.1
  (let ((inst (make-hash-table)))
    (setf (gethash :aaa inst) :value)
    (setf (gethash :bbb inst) :value)
    (setf (gethash :ccc inst) :value)
    (hash-table-p
      (clrhash inst)))
  t)

(deftest clrhash.2
  (let ((inst (make-hash-table)))
    (setf (gethash :aaa inst) :value)
    (setf (gethash :bbb inst) :value)
    (setf (gethash :ccc inst) :value)
    (clrhash inst)
    (hash-table-count inst))
  0)

(deftest sxhash.1
  (integerp
    (sxhash "Hello"))
  t)


;;
;;  equalp
;;
(deftest sxhash-equalp.1
  (let ((a (make-hash-table :test 'equal))
        (b (make-hash-table :test 'equalp)))
    (setf (gethash 1 a) 'aaa)
    (setf (gethash 1.0f0 a) 'bbb)
    (setf (gethash 1.0d0 a) 'ccc)
    (setf (gethash 1 b) 'ddd)
    (setf (gethash 1.0f0 b) 'eee)
    (setf (gethash 1.0d0 b) 'fff)
    (values
      (gethash 1 a)
      (gethash 1.0f0 a)
      (gethash 1.0d0 a)
      (gethash 1 b)
      (gethash 1.0f0 b)
      (gethash 1.0d0 b)))
  aaa bbb ccc fff fff fff)


;;
;;  do-tests
;;
(do-tests :test t)

