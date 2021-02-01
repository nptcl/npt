;;
;;  typep
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


;;
;;  signed-byte
;;
(deftest typep-signed-byte.1
  (typep 10 '(signed-byte 8))
  t)

(deftest typep-signed-byte.2
  (typep #\a '(signed-byte 8))
  nil)

(deftest typep-signed-byte.3
  (typep #x-81 '(signed-byte 8))
  nil)

(deftest typep-signed-byte.4
  (typep #x-80 '(signed-byte 8))
  t)

(deftest typep-signed-byte.5
  (typep #x7F '(signed-byte 8))
  t)

(deftest typep-signed-byte.6
  (typep #x80 '(signed-byte 8))
  nil)

(deftest typep-signed-byte.7
  (typep #x-80000000000000000000000000000001 '(signed-byte 128))
  nil)

(deftest typep-signed-byte.8
  (typep #x-80000000000000000000000000000000 '(signed-byte 128))
  t)

(deftest typep-signed-byte.9
  (typep #x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF '(signed-byte 128))
  t)

(deftest typep-signed-byte.10
  (typep #x80000000000000000000000000000000 '(signed-byte 128))
  nil)

(deftest typep-signed-byte.11
  (typep 10 '(signed-byte *))
  t)

(deftest typep-signed-byte.12
  (typep -10 '(signed-byte *))
  t)

(deftest typep-signed-byte.13
  (typep #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF '(signed-byte *))
  t)


;;
;;  signed-byte
;;
(deftest typep-unsigned-byte.1
  (typep 10 '(unsigned-byte 8))
  t)

(deftest typep-unsigned-byte.2
  (typep #\a '(unsigned-byte 8))
  nil)

(deftest typep-unsigned-byte.3
  (typep -1 '(unsigned-byte 8))
  nil)

(deftest typep-unsigned-byte.4
  (typep 0 '(unsigned-byte 8))
  t)

(deftest typep-unsigned-byte.5
  (typep #xFF '(unsigned-byte 8))
  t)

(deftest typep-unsigned-byte.6
  (typep #x0100 '(unsigned-byte 8))
  nil)

(deftest typep-unsigned-byte.7
  (typep 0 '(unsigned-byte 128))
  t)

(deftest typep-unsigned-byte.8
  (typep #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF '(unsigned-byte 128))
  t)

(deftest typep-unsigned-byte.9
  (typep 10 '(unsigned-byte *))
  t)

(deftest typep-unsigned-byte.10
  (typep -10 '(unsigned-byte *))
  nil)

(deftest typep-unsigned-byte.11
  (typep #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF '(unsigned-byte *))
  t)

(deftest typep-unsigned-byte.12
  (typep #x-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF '(unsigned-byte *))
  nil)


;;
;;  bit
;;
(deftest typep-bit.1
  (typep 0 'bit)
  t)

(deftest typep-bit.2
  (typep 1 'bit)
  t)

(deftest typep-bit.3
  (typep -1 'bit)
  nil)

(deftest typep-bit.4
  (typep "Hello" 'bit)
  nil)


;;
;;  fixnum
;;
(deftest typep-fixnum.1
  (typep 0 'fixnum)
  t)

(deftest typep-fixnum.2
  (typep most-negative-fixnum 'fixnum)
  t)

(deftest typep-fixnum.3
  (typep (1- most-negative-fixnum) 'fixnum)
  nil)

(deftest typep-fixnum.4
  (typep most-positive-fixnum 'fixnum)
  t)

(deftest typep-fixnum.5
  (typep (1+ most-positive-fixnum) 'fixnum)
  nil)

(deftest typep-fixnum.6
  (typep #\A 'fixnum)
  nil)

(deftest typep-fixnum.7
  (typep 4/5 'fixnum)
  nil)


;;
;;  bignum
;;
(deftest typep-bignum.1
  (typep 0 'bignum)
  nil)

(deftest typep-bignum.2
  (typep most-negative-fixnum 'bignum)
  nil)

(deftest typep-bignum.3
  (typep (1- most-negative-fixnum) 'bignum)
  t)

(deftest typep-bignum.4
  (typep most-positive-fixnum 'bignum)
  nil)

(deftest typep-bignum.5
  (typep (1+ most-positive-fixnum) 'bignum)
  t)

(deftest typep-bignum.6
  (typep #\A 'bignum)
  nil)

(deftest typep-bignum.7
  (typep 4/5 'bignum)
  nil)

