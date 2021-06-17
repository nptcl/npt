;;
;;  ANSI COMMON LISP: 4. Types and Classes
;;

;;
;;  Macro DEFTYPE
;;
(deftest deftype.1
  (deftype deftype-test-1 (a)
    `(integer ,a))
  deftype-test-1)

(deftype deftype-test-2 (a)
  `(integer ,a))

(deftest deftype.2
  (values
    (null (lisp-system::symbol-deftype 'deftype-test-2))
    (null (lisp-system::symbol-deftype 'no-such-deftype-symbol)))
  nil t)

(deftype deftest-test-3 (a)
  `(integer ,a))

(deftest deftype.3
  (values
    (null (lisp-system::symbol-deftype 'deftest-test-3))
    (progn
      (lisp-system::delete-deftype 'deftest-test-3)
      (null (lisp-system::symbol-deftype 'deftest-test-3))))
  nil t)

(deftype deftype-test-4 (x)
  (declare (ignore x))
  "Hello"
  'integer)

(deftest deftype.4
  (documentation 'deftype-test-4 'type)
  "Hello")

(deftype deftype-test-5 (x)
  `(cons ,x ,x))

(deftest deftype.5
  (typep (cons 10 20) '(deftype-test-5 integer))
  t)

(deftest deftype.6
  (typep (cons 10 20) '(deftype-test-5 string))
  nil)

(deftest deftype.7
  (typep (cons 10 20) '(deftype-test-5 *))
  t)

(deftype deftype-test-6 (&optional x)
  `(cons ,x ,x))

(deftest deftype.8
  (typep (cons 10 20) 'deftype-test-6)
  t)

(deftest deftype.9
  (typep (cons 10 20) '(deftype-test-6))
  t)

(deftest deftype.10
  (typep (cons 10 20) '(deftype-test-6 integer))
  t)

(deftest deftype.11
  (typep (cons 10 20) '(deftype-test-6 string))
  nil)

(deftype deftype-test-7 ()
  (return-from deftype-test-7 'string))

(deftest deftype.12
  (typep "Hello" 'deftype-test-7)
  t)

(deftest-error deftype-error.1
  (eval '(deftype name)))

(deftest-error deftype-error.2
  (eval '(deftype name 10)))


;;
;;  Function TYPE-OF
;;
(deftest type-of.1
  (type-of 10)
  (integer 10 10))

(deftest type-of.2
  (type-of 'hello)
  symbol)

(deftest type-of.3
  (type-of *readtable*)
  readtable)

(deftest type-of.4
  (let ((x 10))
    (subtypep (type-of x) (class-of x)))
  t t)

(deftest type-of.5
  (typep 10 (type-of 10))
  t)

(defclass type-of-test-1 () ())

(deftest type-of.6
  (type-of (make-instance 'type-of-test-1))
  type-of-test-1)

(defstruct type-of-test-2)

(deftest type-of.7
  (type-of (make-type-of-test-2))
  type-of-test-2)

(define-condition type-of-test-3 () ())

(deftest type-of.8
  (type-of (make-condition 'type-of-test-3))
  type-of-test-3)

(deftest-error! type-of-error.1
  (eval '(type-of)))

(deftest-error! type-of-error.2
  (eval '(type-of 10 20)))

;;  ANSI Common Lisp
(deftest type-of-test.1
  (type-of 'a)
  symbol)

(deftest type-of-test.2
  (type-of '(1 . 2))
  cons)

(deftest type-of-test.3
  (type-of #c(0 1))
  (complex (or (integer 0 0) (integer 1 1))))

(defstruct type-of-test-temp-struct x y z)

(deftest type-of-test.4
  (type-of (make-type-of-test-temp-struct))
  type-of-test-temp-struct)

(deftest type-of-test.5
  (subtypep (type-of "abc") 'string)
  t t)

(deftest type-of-test.6
  (subtypep (type-of (expt 2 40)) 'integer)
  t t)

(deftest type-of-test.7
  (subtypep (type-of 112312) 'integer)
  t t)

(defvar *type-of-test-foo* (make-array 5 :element-type t))

(deftest type-of-test.8
  (subtypep
    (class-name
      (class-of *type-of-test-foo*))
    'vector)
  t t)

(deftest type-of-test.9
  (subtypep
    (type-of *type-of-test-foo*)
    'vector)
  t t)


;;
;;  Function TYPEP
;;
(deftest typep.1
  (typep 10 'integer)
  t)

(deftest typep.2
  (typep "Hello" 'integer)
  nil)

(deftest typep.3
  (typep 10 '(real 10 20))
  t)

(deftest typep.4
  (typep 10 '(single-float 10.0f0 20.0f0))
  nil)

(deftest typep.5
  (typep 10 'integer nil)
  t)

(deftype typep-test-1 (&environment env)
  (macroexpand 'typep-test-hello env))

(deftest typep.6
  (symbol-macrolet ((typep-test-hello integer))
    (macrolet ((aaa (&environment env)
                    (typep 10 'typep-test-1 env)))
      (aaa)))
  t)

(deftest-error typep.7
  (eval '(macrolet ((aaa (&environment env)
                         (typep 10 'typep-test-1 env)))
           (aaa))))

(deftest typep.8
  (typep #'car 'function)
  t)

(deftest-error! typep-error.1
  (eval '(typep 10)))

(deftest-error! typep-error.2
  (eval '(typep 10 'integer nil nil)))

(deftest-error typep-error.3
  (eval '(typep 10 20))
  type-error)

(deftest-error typep-error.4
  (eval '(typep 10 values)))

(deftest-error typep-error.5
  (eval '(typep 10 (values integer))))

(deftest-error typep-error.6
  (eval '(typep 10 (function))))

;;  ANSI Common Lisp
(deftest typep-test.1
  (typep 12 'integer)
  t)

(deftest typep-test.2
  (typep (1+ most-positive-fixnum) 'fixnum)
  nil)

(deftest typep-test.3
  (typep nil t)
  t)

(deftest typep-test.4
  (typep nil nil)
  nil)

(deftest typep-test.5
  (typep 1 '(mod 2))
  t)

(deftest typep-test.6
  (typep #c(1 1) '(complex (eql 1)))
  t)

(deftest typep-test.7
  (typep #c(0 0) '(complex (eql 0)))
  nil)


;;
;;  subtypep
;;
(deftest subtypep.1
  (subtypep 'integer 'integer)
  t t)

(deftest subtypep.2
  (subtypep 'fixnum 'integer)
  t t)

(deftest subtypep.3
  (subtypep 'integer 'fixnum)
  nil t)

(deftest subtypep.4
  (subtypep 'string 'fixnum)
  nil t)

(deftest subtypep.5
  (subtypep 'string '(satisfies hello))
  nil nil)

(deftype test-subtypep (x)
  `(integer ,x))

(deftest subtypep.6
  (subtypep 'fixnum '(test-subtypep *))
  t t)

(deftest subtypep.7
  (subtypep '(integer 10 20) '(integer 0 100))
  t t)

(deftest subtypep.8
  (subtypep '(integer 10 20) '(integer (20) 100))
  nil t)

(deftype subtypep-test-1 (&environment env)
  (macroexpand 'subtypep-test-left env))

(deftype subtypep-test-2 (&environment env)
  (macroexpand 'subtypep-test-right env))

(deftest subtypep.9
  (symbol-macrolet ((subtypep-test-left integer)
                    (subtypep-test-right real))
    (macrolet ((bbb (&environment env)
                    (subtypep 'subtypep-test-1 'subtypep-test-2 env)))
      (bbb)))
  t)

(deftest subtypep.10
  (symbol-macrolet ((subtypep-test-left real)
                    (subtypep-test-right integer))
    (macrolet ((bbb (&environment env)
                    (subtypep 'subtypep-test-1 'subtypep-test-2 env)))
      (bbb)))
  nil)

(deftest-error! subtypep-error.1
  (eval '(subtypep 'integer)))

(deftest-error! subtypep-error.2
  (eval '(subtypep 'integer 'real nil nil)))

(deftest-error! subtypep-error.3
  (eval '(subtypep 10 20))
  type-error)

;;  ANSI Common Lisp
(deftest subtypep-test.1
  (subtypep 'compiled-function 'function)
  t t)

(deftest subtypep-test.2
  (subtypep 'null 'list)
  t t)

(deftest subtypep-test.3
  (subtypep 'null 'symbol)
  t t)

(deftest subtypep-test.4
  (subtypep 'integer 'string)
  nil t)

(deftest subtypep-test.5
  (subtypep '(satisfies dummy) nil)
  nil t)

(deftest subtypep-test.6
  (subtypep '(integer 1 3) '(integer 1 4))
  t t)

(deftest subtypep-test.7
  (subtypep '(integer (0) (0)) 'nil)
  t t)

(deftest subtypep-test.8
  (subtypep 'nil '(integer (0) (0)))
  t t)

(deftest subtypep-test.9
  (subtypep '(integer (0) (0)) '(member))
  t t)

(deftest subtypep-test.10
  (subtypep '(member) 'nil)
  t t)

(deftest subtypep-test.11
  (subtypep 'nil '(member))
  t t)


;;
;;  error
;;
(deftest-error parse-type-error.1
  (eval '(the parse-type-error-1 100)))

(deftest parse-type-error.2
  (functionp
    (eval '(lambda ()
             (the parse-type-error-1 100))))
  t)

