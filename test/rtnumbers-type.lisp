;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  System Class NUMBER
;;
(deftest number-type.1
  (lisp-system:closp
    (find-class 'number))
  t)

(deftest number-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'number)))
  (number t))

(deftest number-type.3
  (typep 100 'number)
  t)

(deftest number-type.4
  (typep #c(10 20) 'number)
  t)

(deftest number-type.5
  (typep "Hello" 'number)
  nil)

(deftest number-type.6
  (subtypep 'real 'number)
  t t)

(deftest number-type.7
  (subtypep 'complex 'number)
  t t)

(deftest number-type.8
  (= 3 3.0)
  t)

(deftest number-type.9
  (eql 3 3.0)
  nil)

(deftest-error number-type.10
  (typep 10 '(number)))


;;
;;  System Class COMPLEX
;;
(deftest complex-type.1
  (lisp-system:closp
    (find-class 'complex))
  t)

(deftest complex-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'complex)))
  (complex number t))

(deftest complex-type.3
  (typep 10 'complex)
  nil)

(deftest complex-type.4
  (typep #c(10 0) 'complex)
  nil)

(deftest complex-type.5
  (typep #c(2 3) 'complex)
  t)

(deftest complex-type.6
  (typep 10.0 'complex)
  nil)

(deftest complex-type.7
  (typep #c(10.0 0) 'complex)
  t)

(deftest complex-type.8
  (typep #c(10.0 20.0) 'complex)
  t)

(deftest complex-type.9
  (typep #c(10.0 20.0) '(complex *))
  t)

(deftest complex-type.10
  (typep #c(10.0 20.0) '(complex real))
  t)

(deftest complex-type.11
  (typep #c(10.0 20.0) '(complex rational))
  nil)

(deftest complex-type.12
  (typep #c(10 20) '(complex rational))
  t)

(deftest complex-type.13
  (coerce 10 'complex)
  10)

(deftest complex-type.14
  (coerce 10.0 'complex)
  #c(10.0 0.0))

(deftest-error complex-type.15
  (typep #c(10 20) '(complex integer integer)))


;;
;;  System Class REAL
;;
(deftest real-type.1
  (lisp-system:closp
    (find-class 'real))
  t)

(deftest real-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'real)))
  (real number t))

(deftest real-type.3
  (typep 10 'real)
  t)

(deftest real-type.4
  (typep 12.3L0 'real)
  t)

(deftest real-type.5
  (typep #c(2 3) 'real)
  nil)

(deftest real-type.6
  (typep "Hello" 'real)
  nil)

(deftest real-type.7
  (typep 15 '(real))
  t)

(deftest real-type.8
  (typep 15 '(real *))
  t)

(deftest real-type.9
  (typep 15 '(real * *))
  t)

(deftest-error real-type.10
  (typep 15 '(real * * *)))

(deftest real-type-range.1
  (typep 10 '(real 10.0))
  t)

(deftest real-type-range.2
  (typep 10 '(real (10.0)))
  nil)

(deftest real-type-range.3
  (typep 5 '(real 10.0 *))
  nil)

(deftest real-type-range.4
  (typep 20 '(real (10.0) *))
  t)

(deftest real-type-range.5
  (typep 20 '(real * 20.0))
  t)

(deftest real-type-range.6
  (typep 20 '(real * (20.0)))
  nil)

(deftest real-type-range.7
  (typep 15 '(real * 20.0))
  t)

(deftest real-type-range.8
  (typep 30 '(real * (20.0)))
  nil)

(deftest real-type-range.9
  (typep 15 '(real 10 20))
  t)


;;
;;  System Class FLOAT
;;
(deftest float-type.1
  (lisp-system:closp
    (find-class 'float))
  t)

(deftest float-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'float)))
  (float real number t))

(deftest float-type.3
  (typep 12.3E4 'float)
  t)

(deftest float-type.4
  (typep 12.3S4 'float)
  t)

(deftest float-type.5
  (typep 12.3F4 'float)
  t)

(deftest float-type.6
  (typep 12.3D4 'float)
  t)

(deftest float-type.7
  (typep 12.3L4 'float)
  t)

(deftest float-type.8
  (typep 15.5 '(float))
  t)

(deftest float-type.9
  (typep 15.5 '(float *))
  t)

(deftest float-type.10
  (typep 15.5 '(float * *))
  t)

(deftest-error float-type.11
  (typep 15.5 '(float * * *)))

(deftest float-type-range.1
  (typep 10.0 '(float 10.0))
  t)

(deftest float-type-range.2
  (typep 10.0 '(float (10.0)))
  nil)

(deftest float-type-range.3
  (typep 5.0 '(float 10.0 *))
  nil)

(deftest float-type-range.4
  (typep 20.0 '(float (10.0) *))
  t)

(deftest float-type-range.5
  (typep 20.0 '(float * 20.0))
  t)

(deftest float-type-range.6
  (typep 20.0 '(float * (20.0)))
  nil)

(deftest float-type-range.7
  (typep 15.0 '(float * 20.0))
  t)

(deftest float-type-range.8
  (typep 30.0 '(float * (20.0)))
  nil)

(deftest float-type-range.9
  (typep 15.0 '(float 10.0 20.0))
  t)


;;
;;  Type SHORT-FLOAT
;;
(deftest short-float-type.1
  (lisp-system:closp
    (find-class 'short-float))
  t)

(deftest short-float-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'short-float)))
  (short-float float real number t))

(deftest short-float-type.3
  (typep 12.3s0 'short-float)
  t)

(deftest short-float-type.4
  (typep 12.3s0 '(short-float 10.0s0 20.0s0))
  t)

(deftest short-float-type.5
  (typep 12.3L0 '(short-float 10.0s0 20.0s0))
  nil)


;;
;;  Type SINGLE-FLOAT
;;
(deftest single-float-type.1
  (lisp-system:closp
    (find-class 'single-float))
  t)

(deftest single-float-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'single-float)))
  (single-float float real number t))

(deftest single-float-type.3
  (typep 12.3f0 'single-float)
  t)

(deftest single-float-type.4
  (typep 12.3f0 '(single-float 10.0f0 20.0f0))
  t)

(deftest single-float-type.5
  (typep 12.3L0 '(single-float 10.0f0 20.0f0))
  nil)


;;
;;  Type DOUBLE-FLOAT
;;
(deftest double-float-type.1
  (lisp-system:closp
    (find-class 'double-float))
  t)

(deftest double-float-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'double-float)))
  (double-float float real number t))

(deftest double-float-type.3
  (typep 12.3d0 'double-float)
  t)

(deftest double-float-type.4
  (typep 12.3d0 '(double-float 10.0d0 20.0d0))
  t)

(deftest double-float-type.5
  (typep 12.3L0 '(double-float 10.0d0 20.0d0))
  nil)


;;
;;  Type LONG-FLOAT
;;
(deftest long-float-type.1
  (lisp-system:closp
    (find-class 'long-float))
  t)

(deftest long-float-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'long-float)))
  (long-float float real number t))

(deftest long-float-type.3
  (typep 12.3L0 'long-float)
  t)

(deftest long-float-type.4
  (typep 12.3L0 '(long-float 10.0L0 20.0L0))
  t)

(deftest long-float-type.5
  (typep 12.3d0 '(long-float 10.0L0 20.0L0))
  nil)



;;
;;  System Class RATIONAL
;;
(deftest rational-type.1
  (lisp-system:closp
    (find-class 'rational))
  t)

(deftest rational-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'rational)))
  (rational real number t))

(deftest rational-type.3
  (typep 10/7 'rational)
  t)

(deftest rational-type.4
  (typep 12 'rational)
  t)

(deftest rational-type.5
  (typep #c(2 3) 'rational)
  nil)

(deftest rational-type.6
  (typep "Hello" 'rational)
  nil)

(deftest rational-type.7
  (typep 15/4 '(rational))
  t)

(deftest rational-type.8
  (typep 15/4 '(rational *))
  t)

(deftest rational-type.9
  (typep 15/4 '(rational * *))
  t)

(deftest-error rational-type.10
  (typep 15/4 '(rational * * *)))

(deftest rational-type.11
  (subtypep 'integer 'rational)
  t t)

(deftest rational-type.12
  (subtypep 'ratio 'rational)
  t t)

(deftest rational-type-range.1
  (typep 10 '(rational 10))
  t)

(deftest rational-type-range.2
  (typep 10 '(rational (10)))
  nil)

(deftest rational-type-range.3
  (typep 5 '(rational 10 *))
  nil)

(deftest rational-type-range.4
  (typep 20 '(rational (10) *))
  t)

(deftest rational-type-range.5
  (typep 20 '(rational * 20))
  t)

(deftest rational-type-range.6
  (typep 20 '(rational * (20)))
  nil)

(deftest rational-type-range.7
  (typep 15 '(rational * 20))
  t)

(deftest rational-type-range.8
  (typep 30 '(rational * (20)))
  nil)

(deftest rational-type-range.9
  (typep 15/7 '(rational 10/7 20/7))
  t)


;;
;;  System Class RATIO
;;
(deftest ratio-type.1
  (lisp-system:closp
    (find-class 'ratio))
  t)

(deftest ratio-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'ratio)))
  (ratio rational real number t))

(deftest ratio-type.3
  (typep 10 'ratio)
  nil)

(deftest ratio-type.4
  (typep 10/2 'ratio)
  nil)

(deftest ratio-type.5
  (typep 10/7 'ratio)
  t)

(deftest ratio-type.6
  (typep 10.3 'ratio)
  nil)

(deftest ratio-type.7
  (typep #c(3/4 5/6) 'ratio)
  nil)

(deftest-error ratio-type.8
  (typep 4/5 '(ration)))


;;
;;  System Class INTEGER
;;
(deftest integer-type.1
  (lisp-system:closp
    (find-class 'integer))
  t)

(deftest integer-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'integer)))
  (integer rational real number t))

(deftest integer-type.3
  (typep 10 'integer)
  t)

(deftest integer-type.4
  (typep 12/7 'integer)
  nil)

(deftest integer-type.5
  (typep #c(2 3) 'integer)
  nil)

(deftest integer-type.6
  (typep "Hello" 'integer)
  nil)

(deftest integer-type.7
  (typep 15 '(integer))
  t)

(deftest integer-type.8
  (typep 15 '(integer *))
  t)

(deftest integer-type.9
  (typep 15 '(integer * *))
  t)

(deftest-error integer-type.10
  (typep 15 '(integer * * *)))

(deftest integer-type.11
  (subtypep 'fixnum 'integer)
  t t)

(deftest integer-type.12
  (subtypep 'bignum 'integer)
  t t)

(deftest integer-type-range.1
  (typep 10 '(integer 10))
  t)

(deftest integer-type-range.2
  (typep 10 '(integer (10)))
  nil)

(deftest integer-type-range.3
  (typep 5 '(integer 10 *))
  nil)

(deftest integer-type-range.4
  (typep 20 '(integer (10) *))
  t)

(deftest integer-type-range.5
  (typep 20 '(integer * 20))
  t)

(deftest integer-type-range.6
  (typep 20 '(integer * (20)))
  nil)

(deftest integer-type-range.7
  (typep 15 '(integer * 20))
  t)

(deftest integer-type-range.8
  (typep 30 '(integer * (20)))
  nil)

(deftest integer-type-range.9
  (typep 15 '(integer 10 20))
  t)

(deftest integer-type-test.1
  (subtypep 'fixnum `(integer ,most-negative-fixnum ,most-positive-fixnum))
  t t)

(deftest integer-type-test.2
  (subtypep 'bit '(integer 0 1))
  t t)

(deftest integer-type-test.3
  (subtypep 'unsigned-byte '(integer 0 *))
  t t)


;;
;;  Type SIGNED-BYTE
;;
(deftest signed-byte-type.1
  (lisp-system:closp
    (find-class 'signed-byte))
  t)

(deftest signed-byte-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'signed-byte)))
  (signed-byte integer rational real number t))

(deftest signed-byte-type.3
  (typep 10 'signed-byte)
  t)

(deftest signed-byte-type.4
  (typep -20 'signed-byte)
  t)

(deftest signed-byte-type.5
  (typep 10/7 'signed-byte)
  nil)

(deftest signed-byte-type.6
  (typep 12 '(signed-byte))
  t)

(deftest signed-byte-type.7
  (typep -24 '(signed-byte *))
  t)

(deftest-error signed-byte-type.8
  (typep -24 '(signed-byte * *))
  t)

(deftest-error signed-byte-type.9
  (typep 24 '(signed-byte 0)))

(deftest signed-byte-type.10
  (typep 24 '(signed-byte 1))
  nil)

(deftest signed-byte-type.11
  (typep 0 '(signed-byte 1))
  t)

(deftest signed-byte-type-range.1
  (typep -128 '(signed-byte 8))
  t)

(deftest signed-byte-type-range.2
  (typep -129 '(signed-byte 8))
  nil)

(deftest signed-byte-type-range.3
  (typep 127 '(signed-byte 8))
  t)

(deftest signed-byte-type-range.4
  (typep 128 '(signed-byte 8))
  nil)

(deftest signed-byte-type-test.1
  (subtypep 'integer '(signed-byte *))
  t t)

(deftest signed-byte-type-test.2
  (subtypep '(signed-byte *) 'integer)
  t t)


;;
;;  Type UNSIGNED-BYTE
;;
(deftest unsigned-byte-type.1
  (lisp-system:closp
    (find-class 'unsigned-byte))
  t)

(deftest unsigned-byte-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'unsigned-byte)))
  (unsigned-byte signed-byte integer rational real number t))

(deftest unsigned-byte-type.3
  (typep 10 'unsigned-byte)
  t)

(deftest unsigned-byte-type.4
  (typep 0 'unsigned-byte)
  t)

(deftest unsigned-byte-type.5
  (typep -1 'unsigned-byte)
  nil)

(deftest unsigned-byte-type.6
  (typep 5/6 'unsigned-byte)
  nil)

(deftest unsigned-byte-type.7
  (typep 10 '(unsigned-byte))
  t)

(deftest unsigned-byte-type.8
  (typep 10 '(unsigned-byte *))
  t)

(deftest-error unsigned-byte-type.9
  (typep 10 '(unsigned-byte * *)))

(deftest-error unsigned-byte-type.10
  (typep 24 '(unsigned-byte 0)))

(deftest unsigned-byte-type.11
  (typep 24 '(unsigned-byte 1))
  nil)

(deftest unsigned-byte-type.12
  (typep 0 '(unsigned-byte 1))
  t)

(deftest unsigned-byte-type-range.1
  (typep 0 '(unsigned-byte 8))
  t)

(deftest unsigned-byte-type-range.2
  (typep -1 '(unsigned-byte 8))
  nil)

(deftest unsigned-byte-type-range.3
  (typep 255 '(unsigned-byte 8))
  t)

(deftest unsigned-byte-type-range.4
  (typep 256 '(unsigned-byte 8))
  nil)

(deftest unsigned-byte-type-test.1
  (subtypep '(integer 0 *) '(unsigned-byte *))
  t t)

(deftest unsigned-byte-type-test.2
  (subtypep '(unsigned-byte *) '(integer 0 *))
  t t)

(deftest unsigned-byte-type-test.3
  (subtypep 'bit '(unsigned-byte 1))
  t t)

(deftest unsigned-byte-type-test.4
  (subtypep '(unsigned-byte 1) 'bit)
  t t)


;;
;;  Type Specifier MOD
;;
(deftest mod-type.1
  (typep 10 '(mod 100))
  t)

(deftest mod-type.2
  (typep 99 '(mod 100))
  t)

(deftest mod-type.3
  (typep 100 '(mod 100))
  nil)

(deftest-error mod-type.4
  (typep 100 'mod))

(deftest-error mod-type.5
  (typep 100 '(mod)))

(deftest-error mod-type.6
  (typep 100 '(mod *)))

(deftest-error mod-type.7
  (typep 100 '(mod 0)))

(deftest-error mod-type.8
  (typep 100 '(mod 4 4)))

(deftest mod-type.9
  (subtypep '(mod 5) '(integer 0 4))
  t t)

(deftest mod-type.10
  (subtypep '(integer 0 4) '(mod 5))
  t t)


;;
;;  Type BIT
;;
(deftest bit-type.1
  (lisp-system:closp
    (find-class 'bit))
  t)

(deftest bit-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'bit)))
  (bit unsigned-byte signed-byte integer rational real number t))

(deftest bit-type.3
  (typep 0 'bit)
  t)

(deftest bit-type.4
  (typep 1 'bit)
  t)

(deftest bit-type.5
  (typep -1 'bit)
  nil)

(deftest bit-type.6
  (typep 2 'bit)
  nil)

(deftest-error bit-type.7
  (typep 1 '(bit)))


;;
;;  Type FIXNUM
;;
(deftest fixnum-type.1
  (lisp-system:closp
    (find-class 'fixnum))
  t)

(deftest fixnum-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'fixnum)))
  (fixnum integer rational real number t))

(deftest fixnum-type.3
  (typep 10 'fixnum)
  t)

(deftest fixnum-type.4
  (typep -20 'fixnum)
  t)

(deftest fixnum-type.5
  (typep 5/6 'fixnum)
  nil)

(deftest fixnum-type.6
  (typep most-negative-fixnum 'fixnum)
  t)

(deftest fixnum-type.7
  (typep most-positive-fixnum 'fixnum)
  t)

(deftest fixnum-type.8
  (typep most-positive-fixnum 'fixnum)
  t)

(deftest fixnum-type.9
  (subtypep '(signed-byte 16) 'fixnum)
  t t)

(deftest-error fixnum-type.10
  (typep 10 '(fixnum)))


;;
;;  Type BIGNUM
;;
(deftest bignum-type.1
  (lisp-system:closp
    (find-class 'bignum))
  t)

(deftest bignum-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'bignum)))
  (bignum integer rational real number t))

(deftest bignum-type.3
  (typep (ash 1 100) 'bignum)
  t)

(deftest bignum-type.4
  (typep 10 'bignum)
  nil)

(deftest bignum-type.5
  (typep "Hello" 'bignum)
  nil)

(deftest-error bignum-type.6
  (typep 10 '(bignum)))


;;
;;  System Class RANDOM-STATE
;;
(deftest random-state-type.1
  (lisp-system:closp
    (find-class 'random-state))
  t)

(deftest random-state-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'random-state)))
  (random-state t))

(deftest random-state-type.3
  (typep *random-state* 'random-state)
  t)

(deftest random-state-type.4
  (typep 100 'random-state)
  nil)

(deftest-error random-state-type.5
  (typep *random-state* '(random-state)))

