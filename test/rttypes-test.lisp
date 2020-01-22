;;
;;  ANSI COMMON LISP: 4. Types and Classes
;;

;;  deftest
(deftest deftype.1
  (deftype test-type.1 (a)
    `(integer ,a))
  test-type.1)

(deftype test-type.2 (a)
  `(integer ,a))

(deftest deftype.2
  (values
    (null (lisp-system::symbol-deftype 'test-type.2))
    (null (lisp-system::symbol-deftype 'no-such-deftype-symbol)))
  nil t)

(deftype test-type.3 (a)
  `(integer ,a))

(deftest deftype.3
  (values
    (null (lisp-system::symbol-deftype 'test-type.3))
    (progn
      (lisp-system::delete-deftype 'test-type.3)
      (null (lisp-system::symbol-deftype 'test-type.3))))
  nil t)


;;  subtypep
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


;;  type-of
(deftest type-of.1
  (type-of 10)
  (integer 10 10))

(deftest type-of.2
  (type-of 'hello)
  symbol)

(deftest type-of.3
  (type-of *readtable*)
  readtable)


;;  typep
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


;;
;;  subtypep  and/or
;;
(import 'lisp-system::subtypep-result)
(import 'lisp-system::include)
(import 'lisp-system::exclude)
(import 'lisp-system::invalid)
(import 'lisp-system::false)

(deftest subtypep-and-right.1
  (subtypep-result 'fixnum '(and real rational integer))
  include)

(deftest subtypep-and-right.2
  (subtypep-result 'fixnum '(and real string integer))
  exclude)

(deftest subtypep-and-right.3
  (subtypep-result 'fixnum '(and real (integer 0 *) integer))
  false)

(deftest subtypep-and-right.4
  (subtypep-result 'fixnum '(and (satisfies hello) cons fixnum))
  exclude)

(deftest subtypep-and-right.5
  (subtypep-result 'fixnum '(and (satisfies hello) (integer 0 *)))
  invalid)

(deftest subtypep-or-right.1
  (subtypep-result 'integer '(or real string cons))
  include)

(deftest subtypep-or-right.2
  (subtypep-result 'integer '(or pathname string cons))
  exclude)

(deftest subtypep-or-right.3
  (subtypep-result 'integer '(or pathname (integer * 0) cons))
  false)

(deftest subtypep-or-right.4
  (subtypep-result 'integer '(or (satisfies hello) real cons))
  include)

(deftest subtypep-or-right.5
  (subtypep-result 'integer '(or (satisfies hello) string cons))
  invalid)

(deftest subtypep-and-left.1
  (subtypep-result '(and integer rational float) 'real)
  include)

(deftest subtypep-and-left.2
  (subtypep-result '(and string rational float) 'real)
  include)

(deftest subtypep-and-left.3
  (subtypep-result '(and (integer 0 *) rational float) 'real)
  include)

(deftest subtypep-and-left.4
  (subtypep-result '(and real rational number) 'integer)
  false)

(deftest subtypep-and-left.5
  (subtypep-result '(and string rational number) 'integer)
  include)

(deftest subtypep-or-left.1
  (subtypep-result '(or integer ratio float) 'real)
  include)

(deftest subtypep-or-left.2
  (subtypep-result '(or integer ratio string) 'real)
  false)

(deftest subtypep-or-left.3
  (subtypep-result '(or pathname package string) 'real)
  exclude)

(deftest subtypep-or-left.4
  (subtypep-result '(or number package string) 'real)
  false)

(deftest subtypep-or-left.5
  (subtypep-result '(or (satisfies hello) number) 'real)
  false)

(deftest subtypep-or-or.1
  (subtypep-result '(or integer symbol) '(or real symbol))
  include)

