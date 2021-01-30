;;
;;  ANSI COMMON LISP: 4. Types and Classes
;;

;;
;;  Type Specifier SATISFIES
;;
(deftest satisfies-specifier.1
  (typep 10 '(satisfies oddp))
  nil)

(deftest satisfies-specifier.2
  (typep 11 '(satisfies oddp))
  t)

(deftest satisfies-specifier.3
  (subtypep 'integer '(satisfies evenp))
  nil nil)

(deftest satisfies-specifier.4
  (subtypep '(satisfies evenp) 'real)
  nil nil)

(deftest satisfies-specifier.5
  (typep 10 '(satisfies *)) ;; global function *
  t)

(deftest-error satisfies-specifier-error.1
  (typep 10 'satisfies))

(deftest-error satisfies-specifier-error.2
  (typep 10 '(satisfies)))

(deftest-error satisfies-specifier-error.3
  (typep 10 '(satisfies evenp nil)))

(deftest-error satisfies-specifier-error.4
  (typep 10 '(satisfies 10)))

(deftest-error satisfies-specifier-error.6
  (typep 'hello '(satisfies *)) ;; global function *
  type-error)


;;
;;  Type Specifier MEMBER
;;
(deftest member-specifier.1
  (typep 20 '(member 10 20 30 40))
  t)

(deftest member-specifier.2
  (typep 21 '(member 10 20 30 40))
  nil)

(deftest member-specifier.3
  (typep (list 20) '(member 10 (20) 30 40))
  nil)

(deftest member-specifier.4
  (subtypep '(member 10 20 30) 'integer)
  t t)

(deftest member-specifier.5
  (subtypep '(member 10 #\a 30) 'integer)
  nil t)

(deftest member-specifier.6
  (subtypep '(member 10 20) '(member 1 2 3 10 20 30))
  t t)

(deftest member-specifier.7
  (subtypep '(member 10 20 0) '(member 1 2 3 10 20 30))
  nil t)

(deftest member-specifier.8
  (typep 10 '(member))
  nil)

(deftest member-specifier.9
  (typep 10 '(member *))
  nil)

(deftest member-specifier.10
  (typep '* '(member *))
  t)

(deftest-error member-specifier-error.1
  (typep 10 'member))


;;
;;  Type Specifier EQL
;;
(deftest eql-specifier.1
  (typep 10 '(eql 10))
  t)

(deftest eql-specifier.2
  (typep 20 '(eql 10))
  nil)

(deftest eql-specifier.3
  (typep (list 10) '(eql (10)))
  nil)

(deftest eql-specifier.4
  (subtypep '(eql 10) 'integer)
  t t)

(deftest eql-specifier.5
  (subtypep '(member 10) '(eql 10))
  t t)

(deftest eql-specifier.6
  (subtypep 'integer '(eql 10))
  nil t)

(deftest eql-specifier.7
  (typep 10 '(eql *))
  nil)

(deftest eql-specifier.8
  (typep '* '(eql *))
  t)

(deftest-error eql-specifier-error.1
  (typep 10 '(eql)))

(deftest-error eql-specifier-error.2
  (typep 10 '(eql nil nil)))

(deftest-error eql-specifier-error.3
  (typep 10 '(eql)))


;;
;;  Type Specifier NOT
;;
(deftest not-specifier.1
  (typep 10 '(not integer))
  nil)

(deftest not-specifier.2
  (typep 10 '(not function))
  t)

(deftest not-specifier.3
  (subtypep 'cons '(not list))
  nil t)

(deftest not-specifier.4
  (subtypep 'list '(not cons))
  nil t)

(deftest not-specifier.5
  (subtypep 'real '(not cons))
  t t)

(deftest not-specifier.6
  (subtypep '(not real) 'cons)
  nil t)

(deftest-error not-specifier-error.1
  (typep 10 '(not *)))

(deftest-error not-specifier-error.2
  (typep 10 '(not)))

(deftest-error not-specifier-error.3
  (typep 10 '(not null null)))

(deftest-error not-specifier-error.4
  (typep 10 'not))


;;
;;  Type Specifier AND
;;
(deftest and-specifier.1
  (typep 10 '(and integer real))
  t)

(deftest and-specifier.2
  (typep 10 '(and cons string))
  nil)

(deftest and-specifier.3
  (subtypep 'integer '(and real rational))
  t t)

(deftest and-specifier.4
  (subtypep 'integer '(and real string))
  nil t)

(deftest and-specifier.5
  (typep 10 '(and))
  t)

(deftest-error and-specifier-error.1
  (typep 10 '(and *)))

(deftest-error and-specifier-error.2
  (typep 10 'and))


;;
;;  Type Specifier OR
;;
(deftest or-specifier.1
  (typep 10 '(or integer string))
  t)

(deftest or-specifier.2
  (typep 10 '(or cons string))
  nil)

(deftest or-specifier.3
  (subtypep 'integer '(or real string))
  t t)

(deftest or-specifier.4
  (subtypep 'integer '(or cons string))
  nil t)

(deftest or-specifier.5
  (typep 10 '(or))
  nil)

(deftest-error or-specifier-error.1
  (typep 10 '(or *)))

(deftest-error or-specifier-error.2
  (typep 10 'or))


;;
;;  Type Specifier VALUES
;;
(deftest values-specifier.1
  (the (values) 10)
  10)

(deftest values-specifier.2
  (the (values &rest t) (values 10 "Hello"))
  10 "Hello")

(deftest values-specifier.3
  (the (values integer) 10)
  10)

(deftest-error values-specifier.4
  (the (values integer) #\A)
  type-error)

(deftest values-specifier.5
  (the (values integer &optional character) 10)
  10)

(deftest values-specifier.6
  (the (values integer &optional character) (values 10 #\A))
  10 #\A)

(deftest-error values-specifier.7
  (the (values integer &optional character) (values 10 20))
  type-error)

(deftest-error values-specifier-error.1
  (eval '(the values 10)))

(deftest-error values-specifier-error.2
  (eval '(the values *)))

(deftest-error values-specifier-error.3
  (eval '(the (values t &allow-other-keys) 10)))

