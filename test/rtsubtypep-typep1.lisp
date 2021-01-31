;;
;;  typep / subtypep
;;

;;
;;  clos
;;
(defclass type-clos-test-1 () ())
(defclass type-clos-test-2 (type-clos-test-1) ())

(deftest typep-clos.1
  (typep 10 'class)
  nil)

(deftest typep-clos.2
  (typep (find-class 'type-clos-test-2) 'class)
  t)

(deftest typep-clos.3
  (typep (find-class 'type-clos-test-2) (find-class 'standard-class))
  t)

(deftest typep-clos.4
  (typep (find-class 'type-clos-test-2) 'standard-class)
  t)

(deftest typep-clos.5
  (typep 10 (find-class 'integer))
  t)

(deftest typep-clos.6
  (typep #\A (find-class 'integer))
  nil)

(deftest typep-clos.7
  (typep (make-instance 'type-clos-test-1) 'type-clos-test-1)
  t)

(deftest typep-clos.8
  (typep (make-instance 'type-clos-test-2) 'type-clos-test-1)
  t)

(deftest typep-clos.9
  (typep (make-instance 'type-clos-test-1) 'type-clos-test-2)
  nil)


;;
;;  asterisk
;;
(deftest-error typep-asterisk.1
  (typep 10 '*))


;;
;;  and
;;
(deftest typep-and.1
  (typep 10 '(and))
  t)

(deftest typep-and.2
  (typep 10 '(and integer real fixnum))
  t)

(deftest typep-and.3
  (typep 10 '(and string real fixnum))
  nil)

(deftest typep-and.4
  (typep 10 '(and (integer 0 100) (integer 8 12)))
  t)


;;
;;  or
;;
(deftest typep-or.1
  (typep "aaa" '(or))
  nil)

(deftest typep-or.2
  (typep "aaa" '(or integer string character))
  t)

(deftest typep-or.3
  (typep "aaa" '(or integer (array bit) character))
  nil)

(deftest typep-or.4
  (typep 10 '(or (integer 0 9) (real 11 100)))
  nil)

(deftest typep-or.5
  (typep 10 '(or (integer 0 14) (real 11 100)))
  t)


;;
;;  eql
;;
(deftest typep-eql.1
  (typep 10 '(eql 10))
  t)

(deftest typep-eql.2
  (typep 10 '(eql #\A))
  nil)

(deftest typep-eql.3
  (typep (list 10) '(eql (10)))
  nil)


;;
;;  member
;;
(deftest typep-member.1
  (typep 10 '(member))
  nil)

(deftest typep-member.2
  (typep 10 '(member 10))
  t)

(deftest typep-member.3
  (typep 20 '(member 10 20 30))
  t)

(deftest typep-member.4
  (typep 40 '(member 10 20 30))
  nil)


;;
;;  mod
;;
(deftest typep-mod.1
  (typep 10 '(mod 100))
  t)

(deftest typep-mod.2
  (typep -1 '(mod 100))
  nil)

(deftest typep-mod.3
  (typep 99 '(mod 100))
  t)

(deftest typep-mod.4
  (typep 100 '(mod 100))
  nil)

(deftest typep-mod.5
  (typep #\A '(mod 100))
  nil)

(deftest typep-mod.6
  (typep 0 '(mod 1))
  t)

(deftest typep-mod.7
  (typep 1 '(mod 1))
  nil)


;;
;;  not
;;
(deftest typep-not.1
  (typep 10 '(not integer))
  nil)

(deftest typep-not.2
  (typep #\A '(not integer))
  t)

(deftest typep-not.3
  (typep #\A '(not (integer 0 10)))
  t)

(deftest typep-not.4
  (typep 5 '(not (integer 0 10)))
  nil)

(deftest typep-not.5
  (typep 20 '(not (integer 0 10)))
  t)

(deftest typep-not.6
  (typep 5.0 '(not (integer 0 10)))
  t)


;;
;;  satisfies
;;
(deftest typep-satisfies.1
  (typep 10 '(satisfies evenp))
  t)

(deftest typep-satisfies.2
  (typep 11 '(satisfies evenp))
  nil)

(deftest typep-satisfies.3
  (typep 10 '(and integer (satisfies evenp)))
  t)

(deftest typep-satisfies.4
  (typep #\A '(and integer (satisfies evenp)))
  nil)


;;
;;  values
;;
(deftest-error typep-values.1
  (typep 10 '(values)))


;;
;;  atom
;;
(deftest typep-atom.1
  (typep 10 'atom)
  t)

(deftest typep-atom.2
  (typep nil 'atom)
  t)

(deftest typep-atom.3
  (typep '(1 2 3) 'atom)
  nil)


;;
;;  list
;;
(deftest typep-list.1
  (typep nil 'list)
  t)

(deftest typep-list.2
  (typep '(1 . 2) 'list)
  t)

(deftest typep-list.3
  (typep 100 'list)
  nil)


;;
;;  boolean
;;
(deftest typep-boolean.1
  (typep nil 'boolean)
  t)

(deftest typep-boolean.2
  (typep t 'boolean)
  t)

(deftest typep-boolean.3
  (typep 10 'boolean)
  nil)


;;
;;  exntended-char
;;
(deftest typep-extended-char.1
  (typep (lisp-system:make-character #x80000000) 'extended-char)
  t)

(deftest typep-extended-char.2
  (typep (lisp-system:make-character #\A) 'extended-char)
  nil)

(deftest typep-extended-char.3
  (typep #\A 'extended-char)
  nil)

(deftest typep-extended-char.4
  (typep 10 'extended-char)
  nil)

