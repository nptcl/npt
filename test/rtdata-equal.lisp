;;
;;  ANSI COMMON LISP: 5. Data and Control Flow
;;

;;
;;  Function EQ
;;
(deftest eq.1
  (eq 'hello 'hello)
  t)

(deftest eq.2
  (eq (list nil) (list nil))
  nil)

(deftest eq.3
  (eq nil nil)
  t)

(deftest eq.4
  (eq 10 10)
  t)

(deftest eq.5
  (eq 10 (lisp-system:make-fixnum 10))
  nil)

(deftest eq.6
  (eq #\A #\A)
  t)

(deftest eq.7
  (eq #\A (lisp-system:make-character #\A))
  nil)

(deftest-error! eq-error.1
  (eval '(eq 10)))

(deftest-error! eq-error.2
  (eval '(eq nil nil nil)))

;;  ANSI Common Lisp
(deftest eq-test.1
  (eq 'a 'b)
  nil)

(deftest eq-test.2
  (eq 'a 'a)
  t)

(deftest eq-test.3
  (eq 3 3.0)
  nil)

(deftest eq-test.4
  (eq #c(3 -4.0) #c(3 -4))
  nil)

(deftest eq-test.5
  (eq (cons 'a 'b) (cons 'a 'c))
  nil)

(deftest eq-test.6
  (eq (cons 'a 'b) (cons 'a 'b))
  nil)

(deftest eq-test.7
  (progn
    (setq x (cons 'a 'b))
    (eq x x))
  t)

(deftest eq-test.8
  (progn
    (setq x '(a . b))
    (eq x x))
  t)

(deftest eq-test.9
  (let ((x "Foo"))
    (eq x x))
  t)

(deftest eq-test.10
  (eq "Foo" (copy-seq "Foo"))
  nil)

(deftest eq-test.11
  (eq "FOO" "foo")
  nil)

(deftest eq-test.12
  (eq "string-seq" (copy-seq "string-seq"))
  nil)

(deftest eq-test.13
  (let ((x 5)) (eq x x))
  t)


;;
;;  Function EQL
;;
(deftest eql.1
  (eql 10 10)
  t)

(deftest eql.2
  (eql nil nil)
  t)

(deftest eql.3
  (eql (list nil) (list nil))
  nil)

(deftest eql.4
  (eql 10 10)
  t)

(deftest eql.5
  (eql 10 (lisp-system:make-fixnum 10))
  t)

(deftest eql.6
  (eql #\a #\a)
  t)

(deftest eql.7
  (eql #\a (lisp-system:make-character #\a))
  t)

(deftest eql.8
  (eql #\a #\A)
  nil)

(deftest eql.9
  (eql #c(4 5) #c(4 5))
  t)

(deftest eql.10
  (eql #c(4 5) #c(4.0 5.0))
  nil)

(deftest eql.11
  (eql #c(5.0 0.0) 5.0)
  nil)

(deftest eql.12
  (eql #c(5 0) 5)
  t)

(deftest-error! eql-error.1
  (eval '(eql 10)))

(deftest-error! eql-error.2
  (eval '(eql nil nil nil)))

;;  ANSI Common Lisp
(deftest eql-test.1
  (eql 'a 'b)
  nil)

(deftest eql-test.2
  (eql 'a 'a)
  t)

(deftest eql-test.3
  (eql 3 3)
  t)

(deftest eql-test.4
  (eql 3 3.0)
  nil)

(deftest eql-test.5
  (eql 3.0 3.0)
  t)

(deftest eql-test.6
  (eql #c(3 -4) #c(3 -4))
  t)

(deftest eql-test.7
  (eql #c(3 -4.0) #c(3 -4))
  nil)

(deftest eql-test.8
  (eql (cons 'a 'b) (cons 'a 'c))
  nil)

(deftest eql-test.9
  (eql (cons 'a 'b) (cons 'a 'b))
  nil)

(deftest eql-test.10
  (progn
    (setq x (cons 'a 'b))
    (eql x x))
  t)

(deftest eql-test.11
  (progn
    (setq x '(a . b))
    (eql x x))
  t)

(deftest eql-test.12
  (eql #\A #\A)
  t)

(deftest eql-test.13
  (eql "Foo" (copy-seq "Foo"))
  nil)

(deftest eql-test.14
  (eql "FOO" "foo")
  nil)


;;
;;  Function EQUAL
;;
(deftest equal.1
  (equal nil nil)
  t)

(deftest equal.2
  (equal 'hello 'hello)
  t)

(deftest equal.3
  (equal 'hello 'abc)
  nil)

(deftest equal.4
  (equal 10 10)
  t)

(deftest equal.5
  (equal 10 999)
  nil)

(deftest equal.6
  (equal (list nil) (list nil))
  t)

(deftest equal.7
  (equal (list t) (list nil))
  nil)

(deftest equal.8
  (let ((x #(a b c)))
    (equal x x))
  t)

(deftest equal.9
  (equal #(a b c) (copy-seq #(a b c)))
  nil)

(deftest equal.10
  (let ((x #1a(a b c)))
    (equal x x))
  t)

(deftest equal.11
  (equal #1a(a b c) (copy-seq #1a(a b c)))
  nil)

(deftest equal.12
  (equal "Hello" (copy-seq "Hello"))
  t)

(deftest equal.13
  (equal "Hello" "ABC")
  nil)

(deftest equal.14
  (equal #*10111 (copy-seq #*10111))
  t)

(deftest equal.15
  (equal #*10111 #*1011)
  nil)

(deftest equal.16
  (equal #p"/tmp/hello.txt" #p"/tmp/hello.txt")
  t)

(deftest equal.17
  (equal #p"/tmp/hello.txt" #p"/tmp/hello.exe")
  nil)

(deftest equal-array.1
  (equal (make-array 5 :element-type t :initial-contents "Hello")
         (make-array 5 :element-type t :initial-contents "Hello"))
  nil)

(deftest equal-array.2
  (equal (make-array 5 :element-type 'character :initial-contents "Hello")
         (make-array 5 :element-type 'character :initial-contents "Hello"))
  t)

(deftest equal-array.3
  (equal "Hello"
         (make-array 5 :element-type 'character :initial-contents "Hello"))
  t)

(deftest equal-array.4
  (equal (make-array 5 :element-type 'character :initial-contents "Hello")
         "Hello")
  t)

(deftest equal-array.5
  (equal (make-array 5 :element-type 'character :initial-contents "hello")
         (make-array 5 :element-type 'character :initial-contents "Hello"))
  nil)

(deftest-error! equal-error.1
  (eval '(equal 10)))

(deftest-error! equal-error.2
  (eval '(equal nil nil nil)))

;;  ANSI Common Lisp
(deftest equal-test.1
  (equal 'a 'b)
  nil)

(deftest equal-test.2
  (equal 'a 'a)
  t)

(deftest equal-test.3
  (equal 3 3)
  t)

(deftest equal-test.4
  (equal 3 3.0)
  nil)

(deftest equal-test.5
  (equal 3.0 3.0)
  t)

(deftest equal-test.6
  (equal #c(3 -4) #c(3 -4))
  t)

(deftest equal-test.7
  (equal #c(3 -4.0) #c(3 -4))
  nil)

(deftest equal-test.8
  (equal (cons 'a 'b) (cons 'a 'c))
  nil)

(deftest equal-test.9
  (equal (cons 'a 'b) (cons 'a 'b))
  t)

(deftest equal-test.10
  (equal #\A #\A)
  t)

(deftest equal-test.11
  (equal #\A #\a)
  nil)

(deftest equal-test.12
  (equal "Foo" "Foo")
  t)

(deftest equal-test.13
  (equal "Foo" (copy-seq "Foo"))
  t)

(deftest equal-test.14
  (equal "FOO" "foo")
  nil)

(deftest equal-test.15
  (equal "This-string" "This-string")
  t)

(deftest equal-test.16
  (equal "This-string" "this-string")
  nil)


;;
;;  Function EQUALP
;;
(deftest equalp.1
  (equalp nil nil)
  t)

(deftest equalp.2
  (equalp 'hello 'hello)
  t)

(deftest equalp.3
  (equalp #\A #\A)
  t)

(deftest equalp.4
  (equalp #\A #\a)
  t)

(deftest equalp.5
  (equalp #\A #\B)
  nil)

(deftest equalp.6
  (equalp 10 10)
  t)

(deftest equalp.7
  (equalp 10 10.0)
  t)

(deftest equalp.8
  (equalp 10 10.1)
  nil)

(deftest equalp.9
  (equalp (list #\a) (list #\a))
  t)

(deftest equalp.10
  (equalp (list #\A) (list #\a))
  t)

(deftest equalp.11
  (equalp (list #\z) (list #\a))
  nil)

(deftest equalp-array-string.1
  (equalp #1a(#\a #\b #\c) #(#\A #\b #\c))
  t)

(deftest equalp-array-string.2
  (equalp #(#\a #\b #\c) #1a(#\A #\b #\c))
  t)

(deftest equalp-array-string.3
  (equalp #1a(#\a #\b #\c) #(#\a #\Z #\c))
  nil)

(deftest equalp-array-string.4
  (equalp #(#\a #\b #\c) #1a(#\a 10 #\c))
  nil)

(deftest equalp-array-string.5
  (equalp #(#\a 20 #\c) #1a(#\a #\b #\c))
  nil)

(deftest equalp-array-string.6
  (equalp #1a(#\a #\b #\c) "abc")
  t)

(deftest equalp-array-string.7
  (equalp "abc" #1a(#\a #\b #\c))
  t)

(deftest equalp-array-string.8
  (equalp #1a(#\a #\b #\c) "aZc")
  nil)

(deftest equalp-array-string.9
  (equalp "abc" #1a(#\a 10 #\c))
  nil)

(deftest equalp-array-bit.1
  (equalp #1a(1 0 1 1 0) #*10110)
  t)

(deftest equalp-array-bit.2
  (equalp #*10110 #1a(1 0 1 1 0))
  t)

(deftest equalp-array-bit.3
  (equalp #1a(1 0 1 1 0) #(1 0 1 1 0))
  t)

(deftest equalp-array-bit.4
  (equalp #(1 0 1 1 0) #1a(1 0 1 1 0))
  t)

(deftest equalp-array-specialized.1
  (equalp (make-array 5 :element-type '(unsigned-byte 8)
                      :initial-contents #(1 0 1 1 0))
          #*10110)
  t)

(deftest equalp-array-specialized.2
  (equalp (make-array 5 :element-type '(unsigned-byte 8)
                      :initial-contents #(1 0 1 1 0))
          #*10100)
  nil)

(deftest equalp-array-specialized.3
  (equalp #*10110
          (make-array 5 :element-type '(unsigned-byte 8)
                      :initial-contents #(1 0 1 1 0)))
  t)

(deftest equalp-array-specialized.4
  (equalp #*10100
          (make-array 5 :element-type '(unsigned-byte 8)
                      :initial-contents #(1 0 1 1 0)))
  nil)

(deftest equalp-array-specialized.5
  (equalp (make-array 5 :element-type '(unsigned-byte 16)
                      :initial-contents #(1 0 1 1 0))
          (make-array 5 :element-type '(unsigned-byte 8)
                      :initial-contents #(1 0 1 1 0)))
  t)

(deftest equalp-array-specialized.6
  (equalp (make-array 5 :element-type '(unsigned-byte 16)
                      :initial-contents #(1 0 1 0 0))
          (make-array 5 :element-type '(unsigned-byte 8)
                      :initial-contents #(1 0 1 1 0)))
  nil)

(defstruct equalp-structure-test-1 aaa bbb)
(defstruct equalp-structure-test-2 aaa bbb)
(defstruct equalp-structure-test-3 aaa ccc)
(defstruct equalp-structure-test-4 aaa bbb ccc)
(defclass equalp-structure-test-5 () (aaa bbb))

(deftest equalp-structure.1
  (let ((x (make-equalp-structure-test-1 :aaa 10 :bbb 20))
        (y (make-equalp-structure-test-1 :aaa 10 :bbb 20)))
    (values (eq x y) (equalp x y)))
  nil t)

(deftest equalp-structure.2
  (let ((x (make-equalp-structure-test-1 :aaa 10 :bbb 20))
        (y (make-equalp-structure-test-1 :aaa 10 :bbb 30)))
    (equalp x y))
  nil)

(deftest equalp-structure.3
  (let ((x (make-equalp-structure-test-1 :aaa 10 :bbb 20))
        (y (make-equalp-structure-test-2 :aaa 10 :bbb 20)))
    (equalp x y))
  nil)

(deftest equalp-structure.4
  (let ((x (make-equalp-structure-test-1 :aaa 10 :bbb 20))
        (y (make-equalp-structure-test-3 :aaa 10 :ccc 20)))
    (equalp x y))
  nil)

(deftest equalp-structure.5
  (let ((x (make-equalp-structure-test-1 :aaa 10 :bbb 20))
        (y (make-equalp-structure-test-4 :aaa 10 :bbb 20)))
    (equalp x y))
  nil)

(deftest equalp-structure.6
  (let ((x (make-instance 'equalp-structure-test-5))
        (y (make-instance 'equalp-structure-test-5)))
    (setf (slot-value x 'aaa) 10)
    (setf (slot-value y 'aaa) 10)
    (setf (slot-value x 'bbb) 20)
    (setf (slot-value y 'bbb) 20)
    (equalp x y))
  nil)

(deftest equalp-structure.7
  (let ((x (make-equalp-structure-test-1 :aaa 10 :bbb 20)))
    (values (equalp x 10) (equalp 10 x)))
  nil nil)

(deftest equalp-structure.8
  (let ((x (make-equalp-structure-test-1 :aaa #\a :bbb 20))
        (y (make-equalp-structure-test-1 :aaa #\A :bbb 20)))
    (equalp x y))
  t)

(deftest equalp-hash-table.1
  (let ((x (make-hash-table))
        (y (make-hash-table)))
    (equalp x y))
  t)

(deftest equalp-hash-table.2
  (let ((x (make-hash-table :test 'equal))
        (y (make-hash-table :test 'eq)))
    (equalp x y))
  nil)

(deftest equalp-hash-table.3
  (let ((x (make-hash-table :test 'eq))
        (y (make-hash-table :test 'eq)))
    (setf (gethash :hello x) 10)
    (setf (gethash :hello y) 10)
    (setf (gethash :aaa x) 20)
    (setf (gethash :aaa y) 20)
    (equalp x y))
  t)

(deftest equalp-hash-table.4
  (let ((x (make-hash-table :test 'eq))
        (y (make-hash-table :test 'eq)))
    (setf (gethash :hello x) 10)
    (setf (gethash :hello y) 20)
    (setf (gethash :aaa x) 20)
    (setf (gethash :aaa y) 20)
    (equalp x y))
  nil)

(deftest equalp-hash-table.5
  (let ((x (make-hash-table :test 'eq))
        (y (make-hash-table :test 'eq)))
    (setf (gethash :hello x) 10)
    (setf (gethash :bbb y) 10)
    (setf (gethash :aaa x) 20)
    (setf (gethash :aaa y) 20)
    (equalp x y))
  nil)

(deftest equalp-hash-table.6
  (let ((x (make-hash-table :test 'eq))
        (y (make-hash-table :test 'eq)))
    (setf (gethash :hello x) 10)
    (setf (gethash :hello y) 10)
    (setf (gethash :aaa x) 20)
    (setf (gethash :aaa y) 20)
    (setf (gethash :bbb x) 30)
    (values (equalp x y) (equalp y x)))
  nil nil)

(deftest equalp-hash-table.7
  (let ((x (make-hash-table :test 'equalp))
        (y (make-hash-table :test 'equalp)))
    (setf (gethash "HELLO" x) #\A)
    (setf (gethash "hello" y) #\a)
    (setf (gethash :aaa x) '(10 20 30))
    (setf (gethash :aaa y) '(10.0F0 20.0D0 30.0L0))
    (values (equalp x y) (equalp y x)))
  t t)

(deftest-error! equalp-error.1
  (eval '(equalp 10)))

(deftest-error! equalp-error.2
  (eval '(equalp nil nil nil)))

;;  ANSI Common Lisp
(deftest equalp-test.1
  (equalp 'a 'b)
  nil)

(deftest equalp-test.2
  (equalp 'a 'a)
  t)

(deftest equalp-test.3
  (equalp 3 3)
  t)

(deftest equalp-test.4
  (equalp 3 3.0)
  t)

(deftest equalp-test.5
  (equalp 3.0 3.0)
  t)

(deftest equalp-test.6
  (equalp #c(3 -4) #c(3 -4))
  t)

(deftest equalp-test.7
  (equalp #c(3 -4.0) #c(3 -4))
  t)

(deftest equalp-test.8
  (equalp (cons 'a 'b) (cons 'a 'c))
  nil)

(deftest equalp-test.9
  (equalp (cons 'a 'b) (cons 'a 'b))
  t)

(deftest equalp-test.10
  (equalp #\A #\A)
  t)

(deftest equalp-test.11
  (equalp #\A #\a)
  t)

(deftest equalp-test.12
  (equalp "Foo" "Foo")
  t)

(deftest equalp-test.13
  (equalp "Foo" (copy-seq "Foo"))
  t)

(deftest equalp-test.14
  (equalp "FOO" "foo")
  t)

(deftest equalp-test.15
  (let ((equalp-test-array1
          (make-array 6 :element-type 'integer :initial-contents '(1 1 1 3 5 7)))
        (equalp-test-array2
          (make-array 8 :element-type 'integer
                      :initial-contents '(1 1 1 3 5 7 2 6)
                      :fill-pointer 6))
        (equalp-test-vector1
          (vector 1 1 1 3 5 7)))
    (values
      (equalp equalp-test-array1 equalp-test-array2)
      (equalp equalp-test-array1 equalp-test-vector1)))
  t t)

