;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Function COMPLEX
;;
(deftest complex-real.1
  (complex 10)
  10)

(deftest complex-real.2
  (complex -200000000000000000000000000000000000)
  -200000000000000000000000000000000000)

(deftest complex-real.3
  (complex 2/3)
  2/3)

(deftest complex-real.4
  (complex 12.3f0)
  #c(12.3f0 0.0f0))

(deftest complex-real.5
  (complex -1.0d0)
  #c(-1.0d0 0.0d0))

(deftest complex-real.6
  (complex 11.0l2)
  #c(11.0l2 0.0l0))

(deftest complex-imag.1
  (complex 10 20)
  #c(10 20))

(deftest complex-imag.2
  (complex 10 0)
  10)

(deftest complex-imag.3
  (complex 999999999999999999999999999999999999999 20)
  #c(999999999999999999999999999999999999999 20))

(deftest complex-imag.4
  (complex 999999999999999999999999999999999999999 0)
  999999999999999999999999999999999999999)

(deftest complex-imag.5
  (complex 2/3 4/5)
  #c(2/3 4/5))

(deftest complex-imag.6
  (complex 2/3 0)
  2/3)

(deftest complex-imag.7
  (complex 1.0f0 2.0f0)
  #c(1.0f0 2.0f0))

(deftest complex-imag.8
  (complex 1.0f0 0)
  #c(1.0f0 0.0f0))

(deftest complex-imag.9
  (complex 1.0d0 2.0d0)
  #c(1.0d0 2.0d0))

(deftest complex-imag.10
  (complex 1.0d0 0)
  #c(1.0d0 0.0d0))

(deftest complex-imag.11
  (complex 1.0L0 2.0L0)
  #c(1.0L0 2.0L0))

(deftest complex-imag.12
  (complex 1.0L0 0)
  #c(1.0L0 0.0L0))

(deftest complex-imag.13
  (complex 1 0.0d0)
  #c(1.0d0 0.0d0))

(deftest complex-imag.14
  (complex 1 -0.0)
  #c(1.0 -0.0))

(deftest-error! complex-error.1
  (eval '(complex)))

(deftest-error! complex-error.2
  (eval '(complex "AAA"))
  type-error)

(deftest-error! complex-error.3
  (eval '(complex 10 "AAA"))
  type-error)

(deftest-error! complex-error.4
  (eval '(complex 10 20 30)))

;;  ANSI Common Lisp
(deftest complex-test.1
  (complex 0)
  0)

(deftest complex-test.2
  (complex 0.0)
  #c(0.0 0.0))

(deftest complex-test.3
  (complex 1 1/2)
  #c(1 1/2))

(deftest complex-test.4
  (complex 1 .99)
  #c(1.0 0.99))

(deftest complex-test.5
  (complex 3/2 0.0)
  #c(1.5 0.0))


;;
;;  Function CONJUGATE
;;
(deftest conjugate.1
  (conjugate 10)
  10)

(deftest conjugate.2
  (conjugate 1.2)
  1.2)

(deftest conjugate.3
  (conjugate #c(10 -20))
  #c(10 20))

(deftest conjugate.4
  (conjugate #c(1.2 3.4))
  #c(1.2 -3.4))

(deftest conjugate.5
  (conjugate #c(1.0d0 0.0d0))
  #c(1.0d0 -0.0d0))

(deftest-error! conjugate-error.1
  (eval '(conjugate)))

(deftest-error! conjugate-error.2
  (eval '(conjugate "Hello"))
  type-error)

(deftest-error! conjugate-error.3
  (eval '(conjugate 10 20)))

;;  ANSI Common Lisp
(deftest conjugate-test.1
  (conjugate #c(0 -1))
  #c(0 1))

(deftest conjugate-test.2
  (conjugate #c(1 1))
  #c(1 -1))

(deftest conjugate-test.3
  (conjugate 1.5)
  1.5)

(deftest conjugate-test.4
  (conjugate #c(3/5 4/5))
  #c(3/5 -4/5))

(deftest conjugate-test.5
  (conjugate #c(0.0D0 -1.0D0))
  #c(0.0D0 1.0D0))

(deftest conjugate-test.6
  (conjugate 3.7)
  3.7)


;;
;;  Function PHASE
;;
(deftest phase-rational.1
  (phase 0)
  0.0f0)

(deftest phase-rational.2
  (phase -0)
  0.0f0)

(deftest phase-rational.3
  (phase 3/4)
  0.0f0)

(deftest phase-single.1
  (phase 1.2f0)
  0.0f0)

(deftest phase-single.2
  (phase -3.4f0)
  3.14159265358979323844f0)

(deftest phase-single.3
  (phase +0.0f0)
  0.0f0)

(deftest phase-single.4
  (phase -0.0f0)
  3.14159265358979323844f0)

(deftest phase-double.1
  (phase 1.2d0)
  0.0d0)

(deftest phase-double.2
  (phase -3.4d0)
  3.14159265358979323844d0)

(deftest phase-double.3
  (phase +0.0d0)
  0.0d0)

(deftest phase-double.4
  (phase -0.0d0)
  3.14159265358979323844d0)

(deftest phase-long.1
  (phase 1.2L0)
  0.0L0)

(deftest phase-long.2
  (phase -3.4L0)
  3.14159265358979323844L0)

(deftest phase-long.3
  (phase +0.0L0)
  0.0L0)

(deftest phase-long.4
  (phase -0.0L0)
  3.14159265358979323844L0)

(deftest-single phase-complex.1
  (phase (cis 30))
  -1.4159266f0)

(deftest-single phase-complex.2
  (phase #c(0 1))
  1.5707964f0)

(deftest phase-complex.3
  (phase #c(+0.0d0 +0.0d0))
  0.0d0)

(deftest phase-complex.4
  (phase #c(+0.0d0 -0.0d0))
  -0.0D0)

(deftest phase-complex.5
  (phase #c(-0.0d0 +0.0d0))
  3.141592653589793D0)

(deftest phase-complex.6
  (phase #c(-0.0d0 -0.0d0))
  -3.141592653589793D0)

(deftest-error! phase-error.1
  (eval '(phase)))

(deftest-error! phase-error.2
  (eval '(phase "Hello"))
  type-error)

(deftest-error! phase-error.3
  (eval '(phase 10 20)))


;;
;;  Function REALPART
;;
(deftest realpart.1
  (realpart 0)
  0)

(deftest realpart.2
  (realpart 0.0f0)
  0.0f0)

(deftest realpart.3
  (realpart 0.0L0)
  0.0L0)

(deftest realpart.4
  (realpart -10)
  -10)

(deftest realpart.5
  (realpart 23.4)
  23.4)

(deftest realpart.6
  (realpart #c(12 34))
  12)

(deftest realpart.7
  (realpart #c(-2.3 4.5))
  -2.3)

(deftest-error! realpart-error.1
  (eval '(realpart)))

(deftest-error! realpart-error.2
  (eval '(realpart "Hello"))
  type-error)

(deftest-error! realpart-error.3
  (eval '(realpart 10 20)))


;;
;;  Function IMAGPART
;;
(deftest imagpart-rational.1
  (imagpart 0)
  0)

(deftest imagpart-rational.2
  (imagpart -0)
  0)

(deftest imagpart-rational.3
  (imagpart 10)
  0)

(deftest imagpart-rational.4
  (imagpart -10)
  0)

(deftest imagpart-single.1
  (imagpart 0.0f0)
  0.0f0)

(deftest imagpart-single.2
  (imagpart -0.0f0)
  -0.0f0)

(deftest imagpart-single.3
  (imagpart 1.23f0)
  0.0f0)

(deftest imagpart-single.4
  (imagpart -1.23f0)
  0.0f0)

(deftest imagpart-double.1
  (imagpart 0.0d0)
  0.0d0)

(deftest imagpart-double.2
  (imagpart -0.0d0)
  -0.0d0)

(deftest imagpart-double.3
  (imagpart 1.23d0)
  0.0d0)

(deftest imagpart-double.4
  (imagpart -1.23d0)
  0.0d0)

(deftest imagpart-long.1
  (imagpart 0.0L0)
  0.0L0)

(deftest imagpart-long.2
  (imagpart -0.0L0)
  -0.0L0)

(deftest imagpart-long.3
  (imagpart 1.23L0)
  0.0L0)

(deftest imagpart-long.4
  (imagpart -1.23L0)
  0.0L0)

(deftest imagpart-complex.1
  (imagpart #c(12 34))
  34)

(deftest imagpart-complex.2
  (imagpart #c(3.4 -1.2))
  -1.2)

(deftest-error! imagpart-error.1
  (eval '(imagpart)))

(deftest-error! imagpart-error.2
  (eval '(imagpart "Hello"))
  type-error)

(deftest-error! imagpart-error.3
  (eval '(imagpart 10 20)))

;;  ANSI Common Lisp
(deftest realpart-test.1
  (realpart #c(23 41))
  23)

(deftest realpart-test.2
  (imagpart #c(23 41.0))
  41.0)

(deftest realpart-test.3
  (realpart #c(23 41.0))
  23.0)

(deftest realpart-test.4
  (imagpart 23.0)
  0.0)


;;
;;  Function UPGRADED-COMPLEX-PART-TYPE
;;
(deftest upgraded-complex-part-type.1
  (upgraded-complex-part-type 'integer)
  integer)

(deftest upgraded-complex-part-type.2
  (upgraded-complex-part-type '(rational 2/3 4/5))
  rational)

(deftest upgraded-complex-part-type.3
  (upgraded-complex-part-type 'float)
  single-float)

(deftest upgraded-complex-part-type.4
  (upgraded-complex-part-type 'double-float nil)
  double-float)

(deftest upgraded-complex-part-type.5
  (macrolet ((aaa (&environment env)
                  `',(upgraded-complex-part-type 'long-float env)))
    (aaa))
  long-float)

(deftype upgraded-complex-part-type-1 (&environment env)
  (macroexpand 'bbb env))

(deftest upgraded-complex-part-type.6
  (macrolet ((aaa (&environment env)
                  `',(upgraded-complex-part-type
                       'upgraded-complex-part-type-1 env)))
    (symbol-macrolet ((bbb long-float))
      (aaa)))
  long-float)

(deftest-error! upgraded-complex-part-type-error.1
  (eval '(upgraded-complex-part-type)))

(deftest-error! upgraded-complex-part-type-error.2
  (eval '(upgraded-complex-part-type 100))
  type-error)

(deftest-error! upgraded-complex-part-type-error.3
  (eval '(upgraded-complex-part-type 'float nil nil)))

