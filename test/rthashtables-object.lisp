;;
;;  ANSI COMMON LISP: 18. Hash Tables
;;

;;
;;  System Class HASH-TABLE
;;
(deftest hash-table-type.1
  (lisp-system:closp
    (find-class 'hash-table))
  t)

(deftest hash-table-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'hash-table)))
  (hash-table t))

(deftest hash-table-type.3
  (typep (make-hash-table) 'hash-table)
  t)

(deftest hash-table-type.4
  (typep 'hello 'hash-table)
  nil)


;;
;;  Function MAKE-HASH-TABLE
;;
(deftest make-hash-table.1
  (typep (make-hash-table) 'hash-table)
  t)

(deftest make-hash-table-test.1
  (let ((x (make-hash-table :test 'eq)))
    (setf (gethash :hello x) 10)
    (gethash :hello x))
  10 t)

(deftest make-hash-table-test.2
  (let ((x (make-hash-table :test 'eq)))
    (setf (gethash (lisp-system:make-fixnum 10) x) 20)
    (gethash (lisp-system:make-fixnum 10) x))
  nil nil)

(deftest make-hash-table-test.3
  (let ((x (make-hash-table :test #'eql)))
    (setf (gethash (lisp-system:make-fixnum 10) x) 20)
    (gethash (lisp-system:make-fixnum 10) x))
  20 t)

(deftest make-hash-table-test.4
  (let ((x (make-hash-table :test #'eql)))
    (setf (gethash (list 10) x) 20)
    (gethash (list 10) x))
  nil nil)

(deftest make-hash-table-test.5
  (let ((x (make-hash-table :test 'equal)))
    (setf (gethash (list 10) x) 20)
    (gethash (list 10) x))
  20 t)

(deftest make-hash-table-test.6
  (let ((x (make-hash-table :test 'equal)))
    (setf (gethash "Hello" x) 20)
    (gethash "HELLO" x))
  nil nil)

(deftest make-hash-table-test.7
  (let ((x (make-hash-table :test #'equalp)))
    (setf (gethash "Hello" x) 20)
    (gethash "HELLO" x))
  20 t)

(deftest-error make-hash-table-test.8
  (make-hash-table :test 'car))

(deftest-error make-hash-table-test.9
  (make-hash-table :test #'car))

(deftest-error make-hash-table-test.10
  (make-hash-table :test (lambda (x y) (equal x y))))

(deftest-error make-hash-table-test.11
  (make-hash-table :test 10))

(deftest make-hash-table-size.1
  (hash-table-p
    (make-hash-table :size 100))
  t)

(deftest make-hash-table-size.2
  (hash-table-p
    (make-hash-table :size 0))
  t)

(deftest make-hash-table-size.3
  (let ((x (make-hash-table :size 0)))
    (setf (gethash 'a x) 10)
    (setf (gethash 'b x) 20)
    (setf (gethash 'c x) 30)
    (gethash 'b x))
  20 t)

(deftest-error make-hash-table-size.4
  (eval '(make-hash-table :size -1)))

(deftest make-hash-table-rehash-size.1
  (hash-table-p
    (make-hash-table :rehash-size 1))
  t)

(deftest-error make-hash-table-rehash-size.2
  (make-hash-table :rehash-size 0))

(deftest make-hash-table-rehash-size.3
  (hash-table-p
    (make-hash-table :rehash-size 100))
  t)

(deftest make-hash-table-rehash-size.4
  (hash-table-p
    (make-hash-table :rehash-size 1.001))
  t)

(deftest-error make-hash-table-rehash-size.5
  (make-hash-table :rehash-size 1.0))

(deftest-error make-hash-table-rehash-size.6
  (eval '(make-hash-table :rehash-size -2.0)))

(deftest make-hash-table-rehash-size.7
  (hash-table-p
    (make-hash-table :rehash-size 5.0d0))
  t)

(deftest-error make-hash-table-rehash-size.8
  (make-hash-table :rehash-size :hello))

(deftest make-hash-table-rehash-threshold.1
  (hash-table-p
    (make-hash-table :rehash-threshold 0))
  t)

(deftest make-hash-table-rehash-threshold.2
  (hash-table-p
    (make-hash-table :rehash-threshold 1.0d0))
  t)

(deftest make-hash-table-rehash-threshold.3
  (hash-table-p
    (make-hash-table :rehash-threshold 0.5))
  t)

(deftest-error make-hash-table-rehash-threshold.4
  (eval '(make-hash-table :rehash-threshold -0.1)))

(deftest-error make-hash-table-rehash-threshold.5
  (make-hash-table :rehash-threshold 1.1))

(deftest-error make-hash-table-error.1
  (eval '(make-hash-table nil)))

(deftest-error make-hash-table-error.2
  (eval '(make-hash-table :test)))

(deftest-error make-hash-table-error.3
  (eval '(make-hash-table :test 10)))

(deftest-error make-hash-table-error.4
  (eval '(make-hash-table :hello 10)))

;;  ANSI Common Lisp
(deftest make-hash-table-common.1
  (progn
    (setq *make-hash-table* (make-hash-table))
    (setf (gethash "one" *make-hash-table*) 1)
    (gethash "one" *make-hash-table*))
  nil nil)

(deftest make-hash-table-common.2
  (progn
    (setq *make-hash-table* (make-hash-table :test 'equal))
    (setf (gethash "one" *make-hash-table*) 1)
    (gethash "one" *make-hash-table*))
  1 t)

(deftest make-hash-table-common.3
  (hash-table-p
    (make-hash-table :rehash-size 1.5 :rehash-threshold 0.7) )
  t)


;;
;;  Function HASH-TABLE-P
;;
(deftest hash-table-p.1
  (hash-table-p
    (make-hash-table))
  t)

(deftest hash-table-p.2
  (hash-table-p 100)
  nil)

(deftest-error! hash-table-p-error.1
  (eval '(hash-table-p)))

(deftest-error! hash-table-p-error.2
  (eval '(hash-table-p 10 20)))

;;  ANSI Common Lisp
(deftest hash-table-p-test.1
  (hash-table-p
    (make-hash-table))
  t)

(deftest hash-table-p-test.2
  (hash-table-p 37)
  nil)

(deftest hash-table-p-test.3
  (hash-table-p '((a . 1) (b . 2)))
  nil)


;;
;;  Function HASH-TABLE-COUNT
;;
(deftest hash-table-count.1
  (let ((inst (make-hash-table)))
    (hash-table-count inst))
  0)

(deftest hash-table-count.2
  (let ((inst (make-hash-table :test #'equalp)))
    (setf (gethash :hello inst) :value)
    (hash-table-count inst))
  1)

(deftest hash-table-count.3
  (let ((inst (make-hash-table :size 0 :rehash-size 1 :rehash-threshold 0.8)))
    (setf (gethash :hello inst) :value)
    (setf (gethash :aaa inst) :bbb)
    (setf (gethash :ccc inst) :value)
    (setf (gethash :hello inst) :value)
    (hash-table-count inst))
  3)

(deftest hash-table-count.4
  (let ((inst (make-hash-table :size 10)))
    (setf (gethash :hello inst) :value)
    (hash-table-count inst))
  1)

(deftest-error hash-table-count-error.1
  (eval '(hash-table-count 10))
  type-error)

(deftest-error! hash-table-count-error.2
  (eval '(hash-table-count)))

(deftest-error! hash-table-count-error.3
  (eval '(hash-table-count
           (make-hash-table)
           20)))

;;  ANSI Common Lisp
(defvar *hash-table-count-table*)

(deftest hash-table-count-test.1
  (progn
    (setq *hash-table-count-table* (make-hash-table))
    (hash-table-count *hash-table-count-table*))
  0)

(deftest hash-table-count-test.2
  (progn
    (setf (gethash 17 *hash-table-count-table*) "fifty-seven")
    (hash-table-count *hash-table-count-table*))
  1)

(deftest hash-table-count-test.3
  (progn
    (dotimes (i 20)
      (setf (gethash i *hash-table-count-table*) i))
    (hash-table-count *hash-table-count-table*))
  20)


;;
;;  Function HASH-TABLE-REHASH-SIZE
;;
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

(deftest hash-table-rehash-size.4
  (= (hash-table-rehash-size
       (make-hash-table :size 100 :rehash-size 1.4))
     1.4)
  t)

(deftest-error hash-table-rehash-size-error.1
  (eval '(hash-table-rehash-size 10))
  type-error)

(deftest-error! hash-table-rehash-size-error.2
  (eval '(hash-table-rehash-size)))

(deftest-error! hash-table-rehash-size-error.3
  (eval '(hash-table-rehash-size
           (make-hash-talbe)
           20)))


;;
;;  Function HASH-TABLE-REHASH-THRESHOLD
;;
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

(deftest hash-table-rehash-threshold.3
  (= (hash-table-rehash-threshold
       (make-hash-table :size 100 :rehash-threshold 0.5))
     0.5)
  t)

(deftest-error hash-table-rehash-threshold-error.1
  (eval '(hash-table-rehash-threshold 10))
  type-error)

(deftest-error! hash-table-rehash-threshold-error.2
  (eval '(hash-table-rehash-threshold)))

(deftest-error! hash-table-rehash-threshold-error.3
  (eval '(hash-table-rehash-threshold
           (make-hash-talbe)
           20)))


;;
;;  Function HASH-TABLE-SIZE
;;
(deftest hash-table-size.1
  (integerp
    (hash-table-size
      (make-hash-table)))
  t)

(deftest hash-table-size.2
  (hash-table-size
    (make-hash-table :size 100))
  100)

(deftest-error hash-table-size-error.1
  (eval '(hash-table-size 10))
  type-error)

(deftest-error! hash-table-size-error.2
  (eval '(hash-table-size)))

(deftest-error! hash-table-size-error.3
  (eval '(hash-table-size
           (make-hash-talbe)
           20)))


;;
;;  Function HASH-TABLE-TEST
;;
(deftest hash-table-test.1
  (hash-table-test
    (make-hash-table))
  eql)

(deftest hash-table-test.2
  (hash-table-test
    (make-hash-table :test 'eq))
  eq)

(deftest hash-table-test.3
  (hash-table-test
    (make-hash-table :test #'eql))
  eql)

(deftest hash-table-test.4
  (hash-table-test
    (make-hash-table :test 'equal))
  equal)

(deftest hash-table-test.5
  (hash-table-test
    (make-hash-table :test #'equalp))
  equalp)

(deftest-error hash-table-test-error.1
  (eval '(hash-table-test 10))
  type-error)

(deftest-error! hash-table-test-error.2
  (eval '(hash-table-test)))

(deftest-error! hash-table-test-error.3
  (eval '(hash-table-test
           (make-hash-talbe)
           20)))

