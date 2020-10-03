;;
;;  compile-value
;;
(deftest compile-value-nil.1
  (value-compile nil)
  nil)

(deftest compile-value-t.1
  (value-compile t)
  t)

(deftest compile-value-cons.1
  (value-compile '(10 20 30))
  (10 20 30))

(deftest compile-value-cons.2
  (value-compile '(10 (#\A #\b (40 50 "Hello")) . 70))
  (10 (#\A #\b (40 50 "Hello")) . 70))

(deftest compile-value-vector.1
  (value-compile #())
  #())

(deftest compile-value-vector.2
  (value-compile #(10 20 30))
  #(10 20 30))

(deftest compile-value-vector.3
  (value-compile #(10 (#\A #\b (40 50 "Hello". 80)) 70))
  #(10 (#\A #\b (40 50 "Hello" . 80)) 70))

(deftest compile-value-character.1
  (value-compile #\B)
  #\B)

(deftest compile-value-character.2
  (let ((x (value-compile #\B)))
    (values
      (typep x 'standard-char)
      (typep x 'base-char)))
  t t)

(deftest compile-value-character.3
  (let ((x (value-compile #\u3000)))
    (values
      (typep x 'standard-char)
      (typep x 'base-char)))
  nil t)

(deftest compile-value-string.1
  (value-compile "Hello")
  "Hello")

(deftest compile-value-string.2
  (let ((x (value-compile "Hello")))
    (values
      (typep x 'string)
      (typep x 'simple-string)
      (typep x 'base-string)
      (typep x 'simple-base-string)))
  t t t t)

(deftest compile-value-symbol.1
  (value-compile 'hello)
  hello)

(deftest compile-value-symbol.2
  (value-compile :aaa)
  :aaa)

(deftest compile-value-fixnum.1
  (value-compile 111)
  111)

(deftest compile-value-bignum.1
  (value-compile
    #xFFFFFFFFFFFFAAAAAAAAAAAAAA1111111111222222222222)
  #xFFFFFFFFFFFFAAAAAAAAAAAAAA1111111111222222222222)

(deftest compile-value-bignum.2
  (value-compile
    #x-FFFFFFFFFFFFAAAAAAAAAAAAAA1111111111222222222222)
  #x-FFFFFFFFFFFFAAAAAAAAAAAAAA1111111111222222222222)

(deftest compile-value-ratio.1
  (value-compile 111/7)
  111/7)

(deftest compile-value-ratio.2
  (value-compile -111/7)
  -111/7)

(deftest compile-value-single-float.1
  (value-compile 10.4f0)
  10.4f0)

(deftest compile-value-double-float.1
  (value-compile -10.4d0)
  -10.4d0)

(deftest compile-value-long-float.1
  (value-compile 10.4L0)
  10.4L0)

(deftest compile-value-complex.1
  (value-compile #c(-10 20))
  #c(-10 20))

(deftest compile-value-complex.2
  (value-compile #c(1.23 -4.56))
  #c(1.23 -4.56))

(deftest compile-value-random-state.1
  (let ((x (exec-compile (make-random-state t))))
    (integerp (random 100 x)))
  t)

(deftest compile-value-pathname.1
  (value-compile #p"/usr/local/bin/aaa.hello")
  #p"/usr/local/bin/aaa.hello")

(deftest compile-value-bitvector.1
  (value-compile #*11011)
  #*11011)

(deftest compile-value-bitvector.2
  (value-compile #*110101100010111011010000110100001001110001001000110101011011101101101010000011000110011110111110101010001111000101111111000110100101011101000000010101111101)
  #*110101100010111011010000110100001001110001001000110101011011101101101010000011000110011110111110101010001111000101111111000110100101011101000000010101111101)

(deftest compile-value-hashtable.1
  (hash-table-p
    (expr-compile
      (make-hash-table :test 'eq)))
  t)

(deftest compile-value-hashtable.2
  (values
    (hash-table-test
      (expr-compile (make-hash-table :test 'eq)))
    (hash-table-test
      (expr-compile (make-hash-table :test 'eql)))
    (hash-table-test
      (expr-compile (make-hash-table :test 'equal)))
    (hash-table-test
      (expr-compile (make-hash-table :test 'equalp))))
  eq eql equal equalp)

(deftest compile-value-hashtable.3
  (values
    (hash-table-count
      (expr-compile (make-hash-table :test 'eql)))
    (hash-table-count
      (expr-compile
        (let ((x (make-hash-table :test 'eql)))
          (setf (gethash 10 x) :aaa)
          (setf (gethash 20 x) :bbb)
          x))))
  0 2)

(deftest compile-value-hashtable.4
  (let ((y (expr-compile
             (let ((x (make-hash-table :test 'eql)))
               (setf (gethash 10 x) :aaa)
               (setf (gethash 20 x) :bbb)
               (setf (gethash #\a x) :ccc)
               x))))
    (values
      (hash-table-test y)
      (hash-table-count y)
      (gethash 10 y)
      (gethash 20 y)
      (gethash #\a y)
      (gethash #\b y)))
  eql 3 :aaa :bbb :ccc nil)

(deftest compile-value-hashtable.5
  (hash-table-rehash-size
    (expr-compile
      (make-hash-table :rehash-size 2.5d0)))
  2.5d0)

(deftest compile-value-hashtable.6
  (hash-table-rehash-threshold
    (expr-compile
      (make-hash-table :rehash-threshold 0.5d0)))
  0.5d0)


;;
;;  lambda
;;
(deftest compile-lambda.1
  (value-compile
    (locally
      (declare (optimize (speed 3)))
      (flet ((aaa () (lambda () :hello)))
        (eq (aaa) (aaa)))))
  t)

(deftest compile-lambda.2
  (value-compile
    (locally
      (declare (optimize (speed 0)))
      (flet ((aaa () (lambda () :hello)))
        (eq (aaa) (aaa)))))
  nil)

(deftest compile-lambda.3
  (value-compile
    (locally
      (declare (optimize (speed 3)))
      (flet ((aaa (x) (lambda () x)))
        (eq (aaa 10) (aaa 10)))))
  nil)

(deftest compile-lambda.4
  (value-compile
    (locally
      (declare (optimize (speed 0)))
      (flet ((aaa (x) (lambda () x)))
        (eq (aaa 10) (aaa 10)))))
  nil)

