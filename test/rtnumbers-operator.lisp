
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
;;  Macro INCF
;;
(deftest incf.1
  (let ((a 10))
    (incf a))
  11)

(deftest incf.2
  (let ((a 10))
    (incf a)
    a)
  11)

(deftest incf.3
  (let ((a (list 10 20)))
    (incf (car a) 888))
  898)

(deftest incf.4
  (let ((a (list 10 20)))
    (incf (car a) 888)
    a)
  (898 20))

(deftest-error! incf-error.1
  (eval '(incf)))

(deftest-error! incf-error.2
  (eval '(incf 100)))

(deftest-error! incf-error.3
  (eval '(incf incf-error-x "AAA")))

(deftest-error! incf-error.4
  (eval '(incf incf-error-x 10 nil)))


;;
;;  Macro DECF
;;
(deftest decf.1
  (let ((a 10))
    (decf a))
  9)

(deftest decf.2
  (let ((a 10))
    (decf a)
    a)
  9)

(deftest decf.3
  (let ((a (list 10 20)))
    (decf (cadr a) 888))
  -868)

(deftest decf.4
  (let ((a (list 10 20)))
    (decf (cadr a) 888)
    a)
  (10 -868))

(deftest-error! decf-error.1
  (eval '(decf)))

(deftest-error! decf-error.2
  (eval '(decf 100)))

(deftest-error! defc-error.3
  (eval '(defc defc-error-x "AAA")))

(deftest-error! decf-error.4
  (eval '(decf decf-error-x 10 nil)))

;;  ANSI Common Lisp
(defvar incf-test-n)

(deftest incf-test.1
  (progn
    (setq incf-test-n 0)
    (incf incf-test-n))
  1)

(deftest incf-test.2
  incf-test-n
  1)

(deftest incf-test.3
  (decf incf-test-n 3)
  -2)

(deftest incf-test.4
  incf-test-n
  -2)

(deftest incf-test.5
  (decf incf-test-n -5)
  3)

(deftest incf-test.6
  (decf incf-test-n)
  2)

(deftest incf-test.7
  (incf incf-test-n 0.5)
  2.5)

(deftest incf-test.8
  (decf incf-test-n)
  1.5)

(deftest incf-test.9
  incf-test-n
  1.5)


;;
;;  Function SIGNUM
;;
(deftest signum-fixnum.1
  (signum 0)
  0)

(deftest signum-fixnum.2
  (signum -10)
  -1)

(deftest signum-fixnum.3
  (signum 333)
  1)

(deftest signum-bignum.1
  (signum (make-bignum 0))
  0)

(deftest signum-bignum.2
  (signum (make-bignum -10))
  -1)

(deftest signum-bignum.3
  (signum (make-bignum 333))
  1)

(deftest signum-ratio.1
  (signum (make-ratio 0 1))
  0)

(deftest signum-ratio.2
  (signum -10/7)
  -1)

(deftest signum-ratio.3
  (signum 333/321)
  1)

(deftest signum-single.1
  (signum 0.0f0)
  0.0f0)

(deftest signum-single.2
  (signum -99.9f0)
  -1.0f0)

(deftest signum-single.3
  (signum 0.3f0)
  1.0f0)

(deftest signum-single.4
  (signum -0.0f0)
  -0.0f0)

(deftest signum-double.1
  (signum 0.0d0)
  0.0d0)

(deftest signum-double.2
  (signum -99.9d0)
  -1.0d0)

(deftest signum-double.3
  (signum 0.3d0)
  1.0d0)

(deftest signum-double.4
  (signum -0.0d0)
  -0.0d0)

(deftest signum-long.1
  (signum 0.0L0)
  0.0L0)

(deftest signum-long.2
  (signum -99.9L0)
  -1.0L0)

(deftest signum-long.3
  (signum 0.3L0)
  1.0L0)

(deftest signum-long.4
  (signum -0.0L0)
  -0.0L0)

(deftest signum-complex.1
  (signum #c(+0.0 -0.0))
  #c(+0.0 -0.0))

(deftest-single signum-complex.2
  (signum #c(3 4))
  0.6 0.8)

(deftest-double signum-complex.3
  (signum #c(-1.2d0 3.4f0))
  -0.3328201094338546d0 0.942990336512754d0)

(deftest-long signum-complex.4
  (signum #c(1 2.0L0))
  0.4472135954999579d0 0.8944271909999159d0)

(deftest-error! signum-error.1
  (eval '(signum)))

(deftest-error! signum-error.2
  (eval '(signum "AAA"))
  type-error)

(deftest-error! signum-error.3
  (eval '(signum 10 20)))

;;  ANSI Common Lisp
(deftest signum-test.1
  (signum 0)
  0)

(deftest signum-test.2
  (signum 99)
  1)

(deftest signum-test.3
  (signum 4/5)
  1)

(deftest signum-test.4
  (signum -99/100)
  -1)

(deftest signum-test.5
  (signum 0.0)
  0.0)

(deftest signum-test.6
  (signum #c(0 33))
  #c(0.0 1.0))

(deftest signum-test.7
  (signum #c(7.5 10.0))
  #c(0.6 0.8))

(deftest signum-test.8
  (signum #c(0.0 -14.7))
  #c(0.0 -1.0))

(deftest signum-test.9
  (eql (signum -0.0) -0.0)
  t)


;;
;;  Function ASH
;;
(deftest ash.1
  (ash 0 0)
  0)

(deftest ash.2
  (ash 10 0)
  10)

(deftest ash.3
  (loop for x from -8 to 8
        collect
        (loop for y from -8 to 8
              collect (ash y x)))
  ((-1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 0 0 0 0 0)
   (-1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 0 0 0 0 0)
   (-1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 0 0 0 0 0)
   (-1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 0 0 0 0 0)
   (-1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 0 0 0 0 0)
   (-1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 0 0 0 0 1)
   (-2 -2 -2 -2 -1 -1 -1 -1 0 0 0 0 1 1 1 1 2)
   (-4 -4 -3 -3 -2 -2 -1 -1 0 0 1 1 2 2 3 3 4)
   (-8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8)
   (-16 -14 -12 -10 -8 -6 -4 -2 0 2 4 6 8 10 12 14 16)
   (-32 -28 -24 -20 -16 -12 -8 -4 0 4 8 12 16 20 24 28 32)
   (-64 -56 -48 -40 -32 -24 -16 -8 0 8 16 24 32 40 48 56 64)
   (-128 -112 -96 -80 -64 -48 -32 -16 0 16 32 48 64 80 96 112 128)
   (-256 -224 -192 -160 -128 -96 -64 -32 0 32 64 96 128 160 192 224 256)
   (-512 -448 -384 -320 -256 -192 -128 -64 0 64 128 192 256 320 384 448 512)
   (-1024 -896 -768 -640 -512 -384 -256 -128 0 128 256 384 512 640 768 896 1024)
   (-2048 -1792 -1536 -1280 -1024 -768 -512 -256 0 256 512 768 1024 1280 1536 1792 2048)))

(deftest ash-plus.1
  (ash 1 200)
  1606938044258990275541962092341162602522202993782792835301376)

(deftest ash-plus.2
  (ash #xABCD 150)
  #x2AF340000000000000000000000000000000000000)

(deftest ash-plus.3
  (ash #xABCD 151)
  #x55E680000000000000000000000000000000000000)

(deftest ash-plus.4
  (ash #xABCD 152)
  #xABCD00000000000000000000000000000000000000)

(deftest ash-plus.5
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD 63)
  #x7F6E5D4C3B2A1908091A2B3C055E6F7D55DE66ED55DE66E8000000000000000)

(deftest ash-plus.6
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD 64)
  #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD0000000000000000)

(deftest ash-plus.7
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD 65)
  #x1FDB97530ECA864202468ACF01579BDF557799BB557799BA0000000000000000)

(deftest ash-plus.8
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD 66)
  #x3FB72EA61D950C84048D159E02AF37BEAAEF3376AAEF33740000000000000000)

(deftest ash-plus.9
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDDABCDEFABCDEF1122334456789012345 66)
  #x3FB72EA61D950C84048D159E02AF37BEAAEF3376AAEF3376AF37BEAF37BC4488CD1159E24048D140000000000000000)

(deftest ash-plus.10
  (ash #xABCD -100)
  0)

(deftest ash-plus.11
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD -63)
  #x1FDB97530ECA864202468ACF01579BDF)

(deftest ash-plus.12
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD -64)
  #xFEDCBA9876543210123456780ABCDEF)

(deftest ash-plus.13
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD -65)
  #x7F6E5D4C3B2A1908091A2B3C055E6F7)

(deftest ash-plus.14
  (ash #xFEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD -66)
  #x3FB72EA61D950C84048D159E02AF37B)

(deftest ash-minus.1
  (ash -16 1)
  -32)

(deftest ash-minus.2
  (ash -16 0)
  -16)

(deftest ash-minus.3
  (ash -16 -1)
  -8)

(deftest ash-minus.4
  (ash -4 -1)
  -2)

(deftest ash-minus.5
  (ash -10 0)
  -10)

(deftest ash-minus.6
  (ash -1 200)
  -1606938044258990275541962092341162602522202993782792835301376)

(deftest ash-minus.7
  (ash #x-ABCD 150)
  #x-2AF340000000000000000000000000000000000000)

(deftest ash-minus.8
  (ash #x-FEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDDABCDEFABCDEF1122334456789012345 -66)
  #x-3FB72EA61D950C84048D159E02AF37BEAAEF3376AAEF3376AF37BEAF37BC45)

(deftest ash-minus.9
  (ash #x-FEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD 63)
  #x-7F6E5D4C3B2A1908091A2B3C055E6F7D55DE66ED55DE66E8000000000000000)

(deftest ash-minus.10
  (ash #x-FEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD 64)
  #x-FEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD0000000000000000)

(deftest ash-minus.11
  (ash #x-FEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD 65)
  #x-1FDB97530ECA864202468ACF01579BDF557799BB557799BA0000000000000000)

(deftest ash-minus.12
  (ash #x-FEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD 66)
  #x-3FB72EA61D950C84048D159E02AF37BEAAEF3376AAEF33740000000000000000)

(deftest ash-minus.13
  (ash #x-FEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD -63)
  #x-1FDB97530ECA864202468ACF01579BE0)

(deftest ash-minus.14
  (ash #x-FEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD -64)
  #x-FEDCBA9876543210123456780ABCDF0)

(deftest ash-minus.15
  (ash #x-FEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD -65)
  #x-7F6E5D4C3B2A1908091A2B3C055E6F8)

(deftest ash-minus.16
  (ash #x-FEDCBA9876543210123456780ABCDEFAABBCCDDAABBCCDD -66)
  #x-3FB72EA61D950C84048D159E02AF37C)

(deftest-error! ash-error.1
  (eval '(ash)))

(deftest-error! ash-error.2
  (eval '(ash 2/3))
  type-error)

(deftest-error! ash-error.3
  (eval '(ash 10 4.5))
  type-error)

(deftest-error! ash-error.4
  (eval '(ash 10 20 30)))

(deftest ash-test.1
  (ash 16 1)
  32)

(deftest ash-test.2
  (ash 16 0)
  16)

(deftest ash-test.3
  (ash 16 -1)
  8)

(deftest ash-test.4
  (ash -100000000000000000000000000000000 -100)
  -79)

(deftest ash-test.5
  (ash 100000000000000000000000000000000 -100)
  78)



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

