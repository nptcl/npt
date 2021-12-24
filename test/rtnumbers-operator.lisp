
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
;;  Function ABS
;;
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

(deftest-single abs.15
  (abs #c(11 22))
  24.596748)

(deftest-double abs.16
  (abs #c(1.2d0 3.4d0))
  3.605551275463989d0)

(deftest-error! abs-error.1
  (eval '(abs)))

(deftest-error! abs-error.2
  (eval '(abs 'hello))
  type-error)

(deftest-error! abs-error.3
  (eval '(abs 10 20)))

;;  ANSI Common Lisp
(deftest abs-test.1
  (abs 0)
  0)

(deftest abs-test.2
  (abs 12/13)
  12/13)

(deftest abs-test.3
  (abs -1.09)
  1.09)

(deftest abs-test.4
  (abs #c(5.0 -5.0))
  7.071068)

(deftest abs-test.5
  (abs #c(5 5))
  7.071068)

(deftest abs-test.6
  (abs #c(3/5 4/5))
  1.0)

(deftest abs-test.7
  (eql (abs -0.0) -0.0)
  t)

(deftest abs-test.8
  (abs #c(3 4))
  5.0)



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

