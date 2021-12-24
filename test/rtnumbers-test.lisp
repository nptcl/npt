;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Function MAX
;;
(deftest max.1
  (max 10)
  10)

(deftest max.2
  (max 7 2 3 9 1 2 999 3 8 9)
  999)

(deftest-error! max-error.1
  (eval '(max)))

(deftest-error! max-error.2
  (eval '(max #c(10 20)))
  type-error)


;;
;;  Function MIN
;;
(deftest min.1
  (min 10)
  10)

(deftest min.2
  (min 7 2 3 9 1 2 999 3 8 9)
  1)

(deftest-error! min-error.1
  (eval '(min)))

(deftest-error! min-error.2
  (eval '(min #c(10 20)))
  type-error)

;;  ANSI Common Lisp
(deftest max-min-test.1
  (max 3)
  3)

(deftest max-min-test.2
  (min 3)
  3)

(deftest max-min-test.3
  (max 6 12)
  12)

(deftest max-min-test.4
  (min 6 12)
  6)

(deftest max-min-test.5
  (max -6 -12)
  -6)

(deftest max-min-test.6
  (min -6 -12)
  -12)

(deftest max-min-test.7
  (max 1 3 2 -7)
  3)

(deftest max-min-test.8
  (min 1 3 2 -7)
  -7)

(deftest max-min-test.9
  (max -2 3 0 7)
  7)

(deftest max-min-test.10
  (min -2 3 0 7)
  -2)

(deftest max-min-test.11
  (max 5.0 2)
  5.0)

(deftest max-min-test.12
  (min 5.0 2)
  2)

(deftest max-min-test.13
  (max 3.0 7 1)
  7)

(deftest max-min-test.14
  (min 3.0 7 1)
  1)

(deftest max-min-test.15
  (max 1.0s0 7.0d0)
  7.0d0)

(deftest max-min-test.16
  (min 1.0s0 7.0d0)
  1.0s0)

(deftest max-min-test.17
  (max 3 1 1.0s0 1.0d0)
  3)

(deftest max-min-test.18
  (= (min 3 1 1.0s0 1.0d0) 1)
  t)


;;
;;  Function MINUSP
;;
(deftest minusp.1
  (values (minusp 10) (minusp 0) (minusp -10))
  nil nil t)

(deftest minusp.2
  (values (minusp (make-bignum 10))
          (minusp (make-bignum 0))
          (minusp (make-bignum -20)))
  nil nil t)

(deftest minusp.3
  (values (minusp 1/2)
          (minusp (make-ratio 0 10))
          (minusp -34/555))
  nil nil t)

(deftest minusp.4
  (values
    (minusp 10.0f0) (minusp 0.0f0) (minusp -12.3f0)
    (minusp 10.0d0) (minusp 0.0d0) (minusp -12.3d0)
    (minusp 10.0l0) (minusp 0.0l0) (minusp -12.3l0))
  nil nil t
  nil nil t
  nil nil t)

(deftest-error! minusp-error.1
  (eval '(minusp)))

(deftest-error! minusp-error.2
  (eval '(minusp 'hello))
  type-error)

(deftest-error! minusp-error.3
  (eval '(minusp 10 20)))

;;  ANSI Common Lisp
(deftest minusp-test.1
  (minusp -1)
  t)

(deftest minusp-test.2
  (minusp -0.0)
  nil)


;;
;;  Function PLUSP
;;
(deftest plusp.1
  (values (plusp 10) (plusp 0) (plusp -10))
  t nil nil)

(deftest plusp.2
  (values (plusp (make-bignum 10))
          (plusp (make-bignum 0))
          (plusp (make-bignum -20)))
  t nil nil)

(deftest plusp.3
  (values (plusp 1/2)
          (plusp (make-ratio 0 10))
          (plusp -34/555))
  t nil nil)

(deftest plusp.4
  (values
    (plusp 10.0f0) (plusp 0.0f0) (plusp -12.3f0)
    (plusp 10.0d0) (plusp 0.0d0) (plusp -12.3d0)
    (plusp 10.0l0) (plusp 0.0l0) (plusp -12.3l0))
  t nil nil
  t nil nil
  t nil nil)

(deftest-error! plusp-error.1
  (eval '(plusp)))

(deftest-error! plusp-error.2
  (eval '(plusp 'hello))
  type-error)

(deftest-error! plusp-error.3
  (eval '(plusp 10 20)))

;;  ANSI Common Lisp
(deftest plusp-test.1
  (plusp 0)
  nil)

(deftest plusp-test.2
  (plusp least-positive-single-float)
  t)


;;
;;  Function ZEROP
;;
(deftest zerop.1
  (values (zerop 10) (zerop 0) (zerop -10))
  nil t nil)

(deftest zerop.2
  (values (zerop (make-bignum 10))
          (zerop (make-bignum 0))
          (zerop (make-bignum -20)))
  nil t nil)

(deftest zerop.3
  (values (zerop 1/2)
          (zerop (make-ratio 0 10))
          (zerop -34/555))
  nil t nil)

(deftest zerop.4
  (values
    (zerop 10.0f0) (zerop 0.0f0) (zerop -12.3f0)
    (zerop 10.0d0) (zerop 0.0d0) (zerop -12.3d0)
    (zerop 10.0l0) (zerop 0.0l0) (zerop -12.3l0))
  nil t nil
  nil t nil
  nil t nil)

(deftest zerop.5
  (values
    (zerop #c(0 0))
    (zerop #c(10 0))
    (zerop #c(10 20))
    (zerop #c(0 10)))
  t nil nil nil)

(deftest-error! zerop-error.1
  (eval '(zerop)))

(deftest-error! zerop-error.2
  (eval '(zerop 'hello))
  type-error)

(deftest-error! zerop-error.3
  (eval '(zerop 10 20)))

;;  ANSI Common Lisp
(deftest zerop-test.1
  (zerop 0)
  t)

(deftest zerop-test.2
  (zerop 1)
  nil)

(deftest zerop-test.3
  (zerop -0.0)
  t)

(deftest zerop-test.4
  (zerop 0/100)
  t)

(deftest zerop-test.5
  (zerop #c(0 0.0))
  t)



;;
;;
;;
(deftest float.1
  (float 10)
  10f0)

(deftest float.2
  (float 10/2)
  5f0)

(deftest float.4
  (float 10d4)
  10d4)

(deftest float.5
  (float 10d4 10f5)
  10f4)

(deftest float.6
  (float 10 10l5)
  10l0)

(deftest floatp.1
  (values
    (floatp 10)
    (floatp 3/4)
    (floatp 1.2)
    (floatp 2.3s1)
    (floatp 2.3f1)
    (floatp -2.3d-1)
    (floatp 2.5L-1)
    (floatp #c(1.2 3.4)))
  nil nil t t t t t nil)

(deftest abs.1
  (abs 0)
  0)

(deftest abs.2
  (abs 10)
  10)

(deftest abs.3
  (abs -20)
  20)

(deftest abs.4
  (abs (make-bignum 10))
  10)

(deftest abs.5
  (abs -888888888888888888888888888888888)
  888888888888888888888888888888888)

(deftest abs.6
  (abs 4/5)
  4/5)

(deftest abs.7
  (abs -789/1111)
  789/1111)

(deftest abs.8
  (abs 1.2f3)
  1.2f3)

(deftest abs.9
  (abs -1.2f3)
  1.2f3)

(deftest abs.10
  (abs 1.2d3)
  1.2d3)

(deftest abs.11
  (abs -1.2d3)
  1.2d3)

(deftest abs.12
  (abs 1.2l3)
  1.2l3)

(deftest abs.13
  (abs -1.2l3)
  1.2l3)

(deftest-single abs.14
  (abs #c(1 2))
  2.236068)

(deftest-double abs.15
  (abs #c(1.2d0 3.4d0))
  3.605551275463989d0)

(deftest evenp.1
  (values (evenp 0) (evenp (make-bignum 0)))
  t t)

(deftest evenp.2
  (values
    (evenp 4) (evenp 5)
    (evenp -4) (evenp -5)
    (evenp (make-bignum 4)) (evenp (make-bignum 5))
    (evenp (make-bignum -4)) (evenp (make-bignum -5)))
  t nil t nil t nil t nil)

(deftest oddp.1
  (values (oddp 0) (oddp (make-bignum 0)))
  nil nil)

(deftest oddp.2
  (values
    (oddp 4) (oddp 5)
    (oddp -4) (oddp -5)
    (oddp (make-bignum 4)) (oddp (make-bignum 5))
    (oddp (make-bignum -4)) (oddp (make-bignum -5)))
  nil t nil t nil t nil t)

(deftest gcd.1
  (values
    (gcd)
    (gcd 60 42)
    (gcd 3333 -33 101)
    (gcd 3333 -33 1002001)
    (gcd 91 -49)
    (gcd 63 -42 35)
    (gcd 5)
    (gcd -4))
  0 6 1 11 7 7 5 4)

(deftest lcm.1
  (values
    (lcm 10)
    (lcm 25 30)
    (lcm -24 18 10)
    (lcm 14 35)
    (lcm 0 5)
    (lcm 1 2 3 4 5 6))
  10 150 360 70 0 60)

(deftest incf.1
  (let ((a 10))
    (incf a)
    a)
  11)

(deftest incf.2
  (let ((a (list 10 20)))
    (incf (car a) 888)
    a)
  (898 20))

(deftest decf.1
  (let ((a 10))
    (decf a)
    a)
  9)

(deftest decf.2
  (let ((a (list 10 20)))
    (decf (cadr a) 888)
    a)
  (10 -868))

(deftest signumf.1
  (signum 0)
  0)

(deftest signumf.2
  (signum -10)
  -1)

(deftest signumf.3
  (signum 333)
  1)

(deftest signumb.1
  (signum (make-bignum 0))
  0)

(deftest signumb.2
  (signum (make-bignum -10))
  -1)

(deftest signumb.3
  (signum (make-bignum 333))
  1)

(deftest signumr.1
  (signum (make-ratio 0 1))
  0)

(deftest signumr.2
  (signum -10/7)
  -1)

(deftest signumr.3
  (signum 333/321)
  1)

(deftest signums.1
  (signum 0.0f0)
  0.0f0)

(deftest signums.2
  (signum -99.9f0)
  -1.0f0)

(deftest signums.3
  (signum 0.3f0)
  1.0f0)

(deftest signumd.1
  (signum 0.0d0)
  0.0d0)

(deftest signumd.2
  (signum -99.9d0)
  -1.0d0)

(deftest signumd.3
  (signum 0.3d0)
  1.0d0)

(deftest signuml.1
  (signum 0.0L0)
  0.0L0)

(deftest signuml.2
  (signum -99.9L0)
  -1.0L0)

(deftest signuml.3
  (signum 0.3L0)
  1.0L0)

(deftest signumc.1
  (let ((c (make-complex 0.0 0.0)))
    (values
      (complexp c)
      (signum c)))
  t 0.0)

(deftest-single signumc.2
  (signum #c(3 4))
  0.6 0.8)

(deftest-double signumc.3
  (signum #c(-1.2d0 3.4f0))
  -0.3328201094338546d0 0.942990336512754d0)

(deftest-long signumc.4
  (signum #c(1 2.0L0))
  0.4472135954999579d0 0.8944271909999159d0)

(deftest sqrt.1
  (sqrt 0)
  0.0f0)

(deftest sqrt.2
  (sqrt (make-bignum 36))
  6.0f0)

(deftest-single sqrt.3
  (sqrt -22)
  0.0f0 4.690416f0)

(deftest-single sqrt.4
  (sqrt 16/25)
  0.8f0)

(deftest-single sqrt.5
  (sqrt 12.3f0)
  3.5071356f0)

(deftest-double sqrt.6
  (sqrt -65.43d0)
  0.0d0 8.088881257627659d0)

(deftest-long sqrt.7
  (sqrt 81.0L0)
  9.0L0)

(deftest-single sqrt.8
  (sqrt #c(5 -6))
  2.530835f0 -1.1853796f0)

(deftest-double sqrt.9
  (sqrt #c(1.2d0 3.4d3))
  41.23833296682044d0 41.22378082954486d0 1.0e-10)

(deftest make-random-state.1
  (typep (make-random-state) 'random-state)
  t)

(deftest make-random-state.2
  (let ((a (make-random-state))
        (b (make-random-state)))
    (values (eq a b) (equal-random-state a b)))
  nil t)

(deftest make-random-state.3
  (let ((a (make-random-state nil))
        (b (make-random-state nil)))
    (values (eq a b) (equal-random-state a b)))
  nil t)

(deftest make-random-state.4
  (let ((a (make-random-state t))
        (b (make-random-state t)))
    (values (eq a b)
            (equal-random-state a b)
            (equal-random-state a *random-state*)))
  nil nil nil)

(deftest make-random-state.5
  (let ((a (make-random-state nil))
        (b (make-random-state t)))
    (values
      (eq *random-state* a)
      (eq *random-state* b)
      (equal-random-state *random-state* a)
      (equal-random-state *random-state* b)))
  nil nil t nil)

(deftest random.1
  (and
    (<= 0 (random 3) 2)
    (<= 0 (random 3) 2)
    (<= 0 (random 3) 2)
    (<= 0 (random 3) 2)
    (<= 0 (random 3) 2)
    (<= 0 (random 3) 2)
    (<= 0 (random 3) 2))
  t)

(deftest random.2
  (and
    (<= 0.0 (random 0.5) 0.5)
    (<= 0.0 (random 0.5) 0.5)
    (<= 0.0 (random 0.5) 0.5)
    (<= 0.0 (random 0.5) 0.5)
    (<= 0.0 (random 0.5) 0.5)
    (<= 0.0 (random 0.5) 0.5))
  t)

(deftest random.3
  (values
    (integerp (random 10))
    (floatp (random 2.0)))
  t t)

(deftest random.4
  (let* ((state1 (make-random-state t))
         (state2 (make-random-state state1)))
    (values
      (equal (random 9999999999999999999999999999999999999999999999 state1)
             (random 9999999999999999999999999999999999999999999999 state2))
      (equal (random 1.0d5 state1)
             (random 1.0d5 state2))))
  t t)

(deftest random-state-p.1
  (values
    (random-state-p *random-state*)
    (random-state-p (make-random-state))
    (random-state-p 'hello))
  t t nil)

(deftest numberp.1
  (values
    (numberp 10)
    (numberp 9999999999999999999999999999999999999)
    (numberp 2/3)
    (numberp 2.3s0)
    (numberp 2.3f0)
    (numberp -1.2d0)
    (numberp 3.4L0)
    (numberp #c(1 2))
    (numberp 'hello))
  t t t t t t t t nil)

(deftest complex1.1
  (complex 10)
  10)

(deftest complex1.2
  (complex -20000000000000000000000000)
  -20000000000000000000000000)

(deftest complex1.3
  (complex 2/3)
  2/3)

(deftest complex1.4
  (complex 12.3f0)
  #c(12.3f0 0.0f0))

(deftest complex1.5
  (complex -1.0d0)
  #c(-1.0d0 0.0d0))

(deftest complex1.6
  (complex 11.0l2)
  #c(11.0l2 0.0l0))

(deftest complex2.1
  (complex 10 20)
  #c(10 20))

(deftest complex2.2
  (complex 10 0)
  10)

(deftest complex2.3
  (complex 999999999999999999999999999999999999999 20)
  #c(999999999999999999999999999999999999999 20))

(deftest complex2.4
  (complex 999999999999999999999999999999999999999 0)
  999999999999999999999999999999999999999)

(deftest complex2.5
  (complex 2/3 4/5)
  #c(2/3 4/5))

(deftest complex2.6
  (complex 2/3 0)
  2/3)

(deftest complex2.7
  (complex 1.0f0 2.0f0)
  #c(1.0f0 2.0f0))

(deftest complex2.8
  (complex 1.0f0 0)
  #c(1.0f0 0.0f0))

(deftest complex2.9
  (complex 1.0d0 2.0d0)
  #c(1.0d0 2.0d0))

(deftest complex2.10
  (complex 1.0d0 0)
  #c(1.0d0 0.0d0))

(deftest complex2.11
  (complex 1.0L0 2.0L0)
  #c(1.0L0 2.0L0))

(deftest complex2.12
  (complex 1.0L0 0)
  #c(1.0L0 0.0L0))

(deftest complexp.1
  (values
    (complexp 10)
    (complexp 'hello)
    (complexp #c(10 20))
    (complexp #c(10 0))
    (complexp #c(0 20))
    (complexp #c(12.3 45))
    (complexp #c(12.3 0.0))
    (complexp #c(0.0d0 12.3d0))
    (complexp #c(12.3 -0.0)))
  nil nil t nil t t t t t)

(deftest conjugate.1
  (conjugate 10)
  10)

(deftest conjugate.2
  (conjugate #c(10 -20))
  #c(10 20))

(deftest conjugate.3
  (conjugate #c(1.2 3.4))
  #c(1.2 -3.4))

(deftest phase.1
  (phase 0)
  0.0f0)

(deftest phase.2
  (phase 3/4)
  0.0f0)

(deftest phase.3
  (phase 1.2f0)
  0.0f0)

(deftest phase.4
  (phase -34.5d4)
  0.0d0)

(deftest-single phase.5
  (phase (cis 30))
  -1.4159266f0)

(deftest-single phase.6
  (phase #c(0 1))
  1.5707964f0)

(deftest realpart.1
  (realpart 0)
  0)

(deftest realpart.2
  (realpart -10)
  -10)

(deftest realpart.3
  (realpart 23.4)
  23.4)

(deftest realpart.4
  (realpart #c(12 34))
  12)

(deftest realpart.5
  (realpart #c(0 34))
  0)

(deftest imagpart.1
  (imagpart 0)
  0)

(deftest imagpart.2
  (imagpart -10)
  0)

(deftest imagpart.3
  (imagpart 23.4)
  0)

(deftest imagpart.4
  (imagpart #c(12 34))
  34)

(deftest imagpart.5
  (imagpart #c(0 -1.2))
  -1.2)

(deftest upgraded-complex-part-type.1
  (upgraded-complex-part-type 'integer)
  integer)

(deftest upgraded-complex-part-type.2
  (upgraded-complex-part-type '(rational 2/3 4/5))
  rational)

(deftest upgraded-complex-part-type.3
  (upgraded-complex-part-type 'float)
  single-float)

(deftest realp.1
  (values
    (realp 10)
    (realp 3/4)
    (realp 4.5)
    (realp #c(10 20))
    (realp 'hello))
  t t t nil nil)

(deftest numerator.1
  (numerator 0)
  0)

(deftest numerator.2
  (numerator 10)
  10)

(deftest numerator.3
  (numerator -20)
  -20)

(deftest numerator.4
  (numerator 4/6)
  2)

(deftest numerator.5
  (numerator 123/541)
  123)

(deftest denominator.1
  (denominator 0)
  1)

(deftest denominator.2
  (denominator 10)
  1)

(deftest denominator.3
  (denominator -20)
  1)

(deftest denominator.4
  (denominator 4/6)
  3)

(deftest denominator.5
  (denominator 123/541)
  541)

(deftest rationalp.1
  (values
    (rationalp 10)
    (rationalp 200000000000000000000000000000000)
    (rationalp 4/5)
    (rationalp 34.5))
  t t t nil)

(deftest ash.1
  (ash 0 0)
  0)

(deftest ash.2
  (ash 10 0)
  10)

(deftest ash.3
  (ash 1 200)
  1606938044258990275541962092341162602522202993782792835301376)

(deftest ash.4
  (ash #xABCD 150)
  #x2AF340000000000000000000000000000000000000)

(deftest ash.5
  (ash #xABCD 151)
  #x55E680000000000000000000000000000000000000)

(deftest ash.6
  (ash #xABCD 152)
  #xABCD00000000000000000000000000000000000000)

(deftest ash.7
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD 63)
  #x7F6E5D4C3B2A1908091A2B3C055E6F7D55DE66ED55DE66E8000000000000000)

(deftest ash.8
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD 64)
  #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD0000000000000000)

(deftest ash.9
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD 65)
  #x1FDB97530ECA864202468ACF01579BDF557799BB557799BA0000000000000000)

(deftest ash.10
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD 66)
  #x3FB72EA61D950C84048D159E02AF37BEAAEF3376AAEF33740000000000000000)

(deftest ash.11
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDDABCDEFABCDEF1122334456789012345 66)
  #x3FB72EA61D950C84048D159E02AF37BEAAEF3376AAEF3376AF37BEAF37BC4488CD1159E24048D140000000000000000)

(deftest ash.12
  (ash #xABCD -100)
  0)

(deftest ash.13
  (ash -4 -1)
  -2)

(deftest ash.14
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD -63)
  #x1FDB97530ECA864202468ACF01579BDF)

(deftest ash.15
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD -64)
  #xFEDCBA9876543210123456780ABCDEF)

(deftest ash.16
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD -65)
  #x7F6E5D4C3B2A1908091A2B3C055E6F7)

(deftest ash.17
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD -66)
  #x3FB72EA61D950C84048D159E02AF37B)

(deftest ash.18
  (ash #x-FEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDDABCDEFABCDEF1122334456789012345 -66)
  #x-3FB72EA61D950C84048D159E02AF37BEAAEF3376AAEF3376AF37BEAF37BC44)

(deftest integer-length.1
  (values
    (integer-length 0)
    (integer-length 1)
    (integer-length 2)
    (integer-length 3)
    (integer-length 4)
    (integer-length 7)
    (integer-length 8)
    (integer-length 15)
    (integer-length 16))
  0 1 2 2 3 3 4 4 5)

(deftest integer-length.2
  (values
    (integer-length -1)
    (integer-length -2)
    (integer-length -3)
    (integer-length -4)
    (integer-length -5)
    (integer-length -8)
    (integer-length -9)
    (integer-length -16)
    (integer-length -17))
  0 1 2 2 3 3 4 4 5)

(deftest integer-length.3
  (values
    (integer-length #xFFFFFFFFFFFFFFFF)
    (integer-length #x-FFFFFFFFFFFFFFFF))
  64 64)

(deftest integer-length.4
  (values
    (integer-length #x10000000000000000)
    (integer-length #x-10000000000000000))
  65 64)

(deftest integer-length.5
  (values
    (integer-length #x10000000000000001)
    (integer-length #x-10000000000000001))
  65 65)

(deftest integer-length.6
  (values
    (integer-length #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
    (integer-length #x-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF))
  128 128)

(deftest integer-length.7
  (values
    (integer-length #x100000000000000000000000000000000)
    (integer-length #x-100000000000000000000000000000000))
  129 128)

(deftest integer-length.8
  (values
    (integer-length #x100000000000000000000000000000001)
    (integer-length #x-100000000000000000000000000000001))
  129 129)

(deftest integerp.1
  (values
    (integerp 0)
    (integerp 10)
    (integerp -210)
    (integerp -22222222222222222222222222222222222222)
    (integerp 3/4)
    (integerp 'hello))
  t t t t nil nil)

(deftest parse-integer.1
  (parse-integer "10")
  10 2)

(deftest parse-integer.2
  (parse-integer "   10")
  10 5)

(deftest parse-integer.3
  (parse-integer "   10   ")
  10 8)

(deftest parse-integer.4
  (parse-integer "   10   " :junk-allowed t)
  10 5)

(deftest parse-integer.5
  (parse-integer "123456789" :start 3 :end 5)
  45 5)

(deftest-error parse-integer.6
  (parse-integer "ABCD"))

(deftest parse-integer.7
  (parse-integer "ABCD" :radix 16)
  #xABCD 4)

(deftest parse-integer.8
  (parse-integer "efghi" :radix 16 :junk-allowed :hello)
  #xEF 2)

(deftest parse-integer.9
  (parse-integer " +10 ")
  10 5)

(deftest parse-integer.10
  (parse-integer " -10 " :junk-allowed t)
  -10 4)

(deftest-error parse-integer.11
  (parse-integer "  HHHH" :junk-allowed nil))

(deftest parse-integer.12
  (parse-integer "  HHHH" :junk-allowed t)
  nil 2)


;;
;;  Degrade
;;
(deftest numbers-test.1
  (values
    (= +0.0 -0.0)
    (eql +0.0 -0.0))
  t nil)

(deftest numbers-test.2
  (values
    (= +0.0s0 -0.0s0)
    (eql +0.0s0 -0.0s0))
  t nil)

(deftest numbers-test.3
  (values
    (= +0.0f0 -0.0f0)
    (eql +0.0f0 -0.0f0))
  t nil)

(deftest numbers-test.4
  (values
    (= +0.0d0 -0.0d0)
    (eql +0.0d0 -0.0d0))
  t nil)

(deftest numbers-test.5
  (values
    (= +0.0L0 -0.0L0)
    (eql +0.0L0 -0.0L0))
  t nil)

