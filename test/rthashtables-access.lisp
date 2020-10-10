;;
;;  ANSI COMMON LISP: 18. Hash Tables
;;

;;
;;  Accessor GETHASH
;;
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

(deftest gethash.9
  (let ((inst (make-hash-table)))
    (incf (gethash :aaa inst 10))
    (gethash :aaa inst))
  11 t)

(deftest-error gethash-error.1
  (eval '(gethash 10 20))
  type-error)

(deftest-error! gethash-error.2
  (eval '(gethash 10)))

(deftest-error! gethash-error.3
  (eval '(gethash 10 (make-hash-table) 20 30)))

(deftest-error gethash-error.4
  (eval '(setf (gethash 10 20) #\a))
  type-error)

(deftest-error! gethash-error.5
  (eval '(setf (gethash 10) #\a)))

(deftest-error! gethash-error.6
  (eval '(setf (gethash 10 (make-hash-table) 20 30) #\a)))

;;  ANSI Common Lisp
(defvar *gethash-table*)
(deftest gethash-test.1
  (progn
    (setq *gethash-table* (make-hash-table))
    (gethash 1 *gethash-table*))
  nil nil)

(deftest gethash-test.2
  (gethash 1 *gethash-table* 2)
  2 nil)

(deftest gethash-test.3
  (setf (gethash 1 *gethash-table*) "one")
  "one")

(deftest gethash-test.4
  (setf (gethash 2 *gethash-table* "two") "two")
  "two")

(deftest gethash-test.5
  (gethash 1 *gethash-table*)
  "one" t)

(deftest gethash-test.6
  (gethash 2 *gethash-table*)
  "two" t)

(deftest gethash-test.7
  (gethash nil *gethash-table*)
  nil nil)

(deftest gethash-test.8
  (setf (gethash nil *gethash-table*) nil)
  nil)

(deftest gethash-test.9
  (gethash nil *gethash-table*)
  nil t)

(defvar *gethash-counters*)

(deftest gethash-test.10
  (progn
    (setq *gethash-counters* (make-hash-table))
    (gethash 'foo *gethash-counters*))
  nil nil)

(deftest gethash-test.11
  (gethash 'foo *gethash-counters* 0)
  0 nil)

(defmacro gethash-how-many (obj)
  `(values (gethash ,obj *gethash-counters* 0)))

(defun gethash-count-it (obj)
  (incf (gethash-how-many obj)))

(deftest gethash-test.12
  (progn
    (dolist (x '(bar foo foo bar bar baz))
      (gethash-count-it x))
    (gethash-how-many 'foo))
  2)

(deftest gethash-test.13
  (gethash-how-many 'bar)
  3)

(deftest gethash-test.14
  (gethash-how-many 'quux)
  0)


;;
;;  Function REMHASH
;;
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

(deftest-error remhash-error.1
  (eval '(remhash 10 20))
  type-error)

(deftest-error! remhash-error.2
  (eval '(remhash 10)))

(deftest-error! remhash-error.3
  (eval '(remhash 10 (make-hash-table 30))))

;;  ANSI Common Lisp
(defvar *remhash-table*)

(deftest remhash-test.1
  (progn
    (setq *remhash-table* (make-hash-table))
    (setf (gethash 100 *remhash-table*) "C"))
  "C")

(deftest remhash-test.2
  (gethash 100 *remhash-table*)
  "C" t)

(deftest remhash-test.3
  (remhash 100 *remhash-table*)
  t)

(deftest remhash-test.4
  (gethash 100 *remhash-table*)
  nil nil)

(deftest remhash-test.5
  (remhash 100 *remhash-table*)
  nil)


;;
;;  Function MAPHASH
;;
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

(deftest maphash.4
  (maphash
    'equal
    (make-hash-table))
  nil)

(deftest-error maphash-error.1
  (eval '(maphash 10 (make-hash-table)))
  type-error)

(deftest-error maphash-error.2
  (eval '(maphash #'equal 20))
  type-error)

(deftest-error! maphash-error.3
  (eval '(maphash #'equal)))

(deftest-error! maphash-error.4
  (eval '(maphash #'equal (make-hash-table) 30)))

(deftest-error maphash-error.5
  (let ((x (lambda (x y z) (list x y z)))
        (y (make-hash-table)))
    (setf (gethash 10 y) 20)
    (maphash x y)))

(deftest-error maphash-error.6
  (let ((x (lambda (x) (list x)))
        (y (make-hash-table)))
    (setf (gethash 10 y) 20)
    (maphash x y)))

;;  ANSI Common Lisp
(defvar *maphash-table*)

(deftest maphash-test.1
  (progn
    (setq *maphash-table* (make-hash-table))
    (dotimes (i 10)
      (setf (gethash i *maphash-table*) i))
    (let ((sum-of-squares 0))
      (maphash #'(lambda (key val)
                   (let ((square (* val val)))
                     (incf sum-of-squares square)
                     (setf (gethash key *maphash-table*) square)))
               *maphash-table*)
      sum-of-squares))
  285)

(deftest maphash-test.2
  (hash-table-count *maphash-table*)
  10)

(deftest maphash-test.3
  (maphash #'(lambda (key val)
               (when (oddp val) (remhash key *maphash-table*)))
           *maphash-table*)
  nil)

(deftest maphash-test.4
  (hash-table-count *maphash-table*)
  5)

(deftest maphash-test.5
  (let (list)
    (maphash #'(lambda (k v) (push (list k v) list)) *maphash-table*)
    (values-list
      (sort (nreverse list) #'< :key #'car)))
  (0 0) (2 4) (4 16) (6 36) (8 64))


;;
;;  Macro WITH-HASH-TABLE-ITERATOR
;;
(deftest with-hash-table-iterator.1
  (let ((inst (make-hash-table)))
    (with-hash-table-iterator
      (call inst)
      (call)))
  nil)

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

(deftest with-hash-table-iterator.4
  (with-hash-table-iterator
    (x (make-hash-table))
    (declare (ignore x))
    :hello)
  :hello)

(deftest with-hash-table-iterator.5
  (let (list x y)
    (setq x (make-hash-table))
    (setq y (make-hash-table))
    (setf (gethash 10 x) :aa)
    (setf (gethash 20 x) :bb)
    (setf (gethash 30 x) :cc)
    (setf (gethash 40 y) :dd)
    (setf (gethash 50 y) :ee)
    (setf (gethash 60 y) :ff)
    (with-hash-table-iterator
      (call x)
      (push (multiple-value-list (call)) list)
      (with-hash-table-iterator
        (call y)
        (push (multiple-value-list (call)) list))
      (push (multiple-value-list (call)) list))
    (destructuring-bind (x y z) (nreverse list)
      (and (<= 10 (cadr x) 30)
           (<= 40 (cadr y) 60)
           (<= 10 (cadr z) 30))))
  t)

(deftest with-hash-table-iterator.6
  (with-hash-table-iterator (x (make-hash-table)))
  nil)

(deftest with-hash-table-iterator.7
  (with-hash-table-iterator (x (make-hash-table)) :hello)
  :hello)

(deftest-error with-hash-table-iterator-error.1
  (eval '(with-hash-table-iterator (10 (make-hash-table)))))

(deftest-error with-hash-table-iterator-error.2
  (eval '(with-hash-table-iterator (x 20))))

(deftest-error with-hash-table-iterator-error.3
  (eval '(with-hash-table-iterator (x))))

(deftest-error with-hash-table-iterator-error.4
  (eval '(with-hash-table-iterator (x (make-hash-table) 10))))


;;
;;  Function CLRHASH
;;
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

(deftest-error clrhash-error.1
  (eval '(clrhash 10))
  type-error)

(deftest-error! clrhash-error.2
  (eval '(clrhash)))

(deftest-error! clrhash-error.3
  (eval '(clrhash (make-hash-table) 20)))

;;  ANSI Common Lisp
(defvar *clrhash-table*)

(deftest clrhash-test.1
  (progn
    (setq *clrhash-table* (make-hash-table))
    (dotimes (i 100)
      (setf (gethash i *clrhash-table*) (format nil "~R" i)))
    (hash-table-count *clrhash-table*))
  100)

(deftest clrhash-test.2
  (gethash 57 *clrhash-table*)
  "fifty-seven" t)

(deftest clrhash-test.3
  (hash-table-p
    (clrhash *clrhash-table*))
  t)

(deftest clrhash-test.4
  (hash-table-count *clrhash-table*)
  0)

(deftest clrhash-test.5
  (gethash 57 *clrhash-table*)
  nil nil)


;;
;;  Function SXHASH
;;
(deftest sxhash.1
  (integerp
    (sxhash "Hello"))
  t)

(deftest sxhash.2
  (typep (sxhash "Hello") 'fixnum)
  t)

(deftest sxhash.3
  (= (sxhash (list 'a 'b 10 20 "Hello"))
     (sxhash (list 'a 'b 10 20 "Hello")))
  t)

(deftest-error! sxhash-error.1
  (eval '(sxhash)))

(deftest-error! sxhash-error.2
  (eval '(sxhash 10 20)))

;;  ANSI Common Lisp
(deftest sxhash-test.1
  (= (sxhash (list 'list "ab"))
     (sxhash (list 'list "ab")))
  t)

(deftest sxhash-test.2
  (= (sxhash "a")
     (sxhash (make-string 1 :initial-element #\a)))
  t)

(deftest sxhash-test.3
  (let ((r (make-random-state)))
    (= (sxhash r)
       (sxhash (make-random-state r))))
  nil)


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

