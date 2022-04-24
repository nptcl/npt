;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Function SIN
;;
(deftest-single sin.1
  (sin 0)
  0.0)

(deftest-float sin.2
  (sin (/ pi 2))
  1.0)

(deftest-single sin.3
  (sin 30)
  -0.9880316)

(deftest-single sin.4
  (sin (make-bignum 30))
  -0.9880316)

(deftest-single sin.5
  (sin 3/4)
  0.6816388)

(deftest-single sin.6
  (sin 1.2f-3)
  0.0011999998)

(deftest-double sin.7
  (sin -4.5d1)
  -0.8509035245341184d0)

(deftest-long sin.8
  (sin 6.7l0)
  0.40484992061659816147L0)

(deftest-single sin.9
  (sin #c(1.2 3.4))
  13.97941 5.4228153)

(deftest-double sin.10
  (sin #c(5.6d-1 7.8d0))
  648.2071513890561d0 1033.906076170876d0
  1.0e-6)

(deftest-single sin.11
  (sin #c(1 2))
  3.1657784 1.959601)

(deftest-double sin.12
  (sin #c(2 3.4d0))
  13.638312649372535d0 -6.227788631993562d0)

(deftest-error! sin-error.1
  (eval '(sin)))

(deftest-error! sin-error.2
  (eval '(sin "hello"))
  type-error)

(deftest-error! sin-error.3
  (eval '(sin 10 20)))

;; ANSI Common Lisp
(deftest sin-test.1
  (sin 0)
  0.0)

(deftest-single sin-test.2
  (cos 0.7853982)
  0.707107)

(deftest-single sin-test.3
  (tan #c(0 1))
  0.0 0.761594)


;;
;;  Function COS
;;
(deftest-single cos.1
  (cos 0)
  1.0)

(deftest-float cos.2
  (cos (/ pi 2))
  0.0)

(deftest-single cos.3
  (cos 30)
  0.15425146)

(deftest-single cos.4
  (cos (make-bignum 30))
  0.15425146)

(deftest-single cos.5
  (cos 3/4)
  0.73168886)

(deftest-single cos.6
  (cos 1.2f-3)
  0.9999993)

(deftest-double cos.7
  (cos -4.5d1)
  0.5253219888177297d0)

(deftest-long cos.8
  (cos 6.7l0)
  0.9143831482353194412L0)

(deftest-single cos.9
  (cos #c(1.2 3.4))
  5.4349084 -13.948305)

(deftest-double cos.10
  (cos #c(5.6d-1 7.8d0))
  1033.9064233209313d0 -648.2069337434905d0
  1.0e-6)

(deftest-single cos.11
  (cos #c(1 2))
  2.032723 -3.0518978)

(deftest-double cos.12
  (cos #c(2 3.4d0))
  -6.241676812712784d0 -13.607966420872854d0)

(deftest-error! cos-error.1
  (eval '(cos)))

(deftest-error! cos-error.2
  (eval '(cos "hello"))
  type-error)

(deftest-error! cos-error.3
  (eval '(cos 10 20)))


;;
;;  Function TAN
;;
(deftest-single tan.1
  (tan 0)
  0.0)

(deftest-single tan.2
  (tan -1.0)
  -1.5574077)

(deftest-single tan.3
  (tan 30)
  -6.405331)

(deftest-single tan.4
  (tan (make-bignum 30))
  -6.405331)

(deftest-single tan.5
  (tan 3/4)
  0.93159646)

(deftest-single tan.6
  (tan 1.2f-3)
  0.0012000006)

(deftest-double tan.7
  (tan -4.5d1)
  -1.6197751905438615d0)

(deftest-long tan.8
  (tan 6.7l0)
  0.44275741673271600319L0)

(deftest-single tan.9
  (tan #c(1.2 3.4))
  0.0015071015 1.0016428)

(deftest-double tan.10
  (tan #c(5.6d-1 7.8d0))
  3.022226362061947d-7 0.999999853712828d0)

(deftest-single tan.11
  (tan #c(1 2))
  0.033812825 1.0147936)

(deftest-double tan.12
  (tan #c(2 3.4d0))
  -0.001688271692080371d0 1.001455660722037d0)

(deftest-error! tan-error.1
  (eval '(tan)))

(deftest-error! tan-error.2
  (eval '(tan "hello"))
  type-error)

(deftest-error! tan-error.3
  (eval '(tan 10 20)))


;;
;;  Function ASIN
;;
(deftest-single asin.1
  (asin 0)
  0.0)

(deftest-single asin.2
  (asin 1.0)
  1.5707964)

(deftest-single asin.3
  (asin 1.1)
  1.5707964 0.44356826)

(deftest-single asin.4
  (asin -1.0)
  -1.5707964)

(deftest-single asin.5
  (asin -1.1)
  -1.5707964 0.44356826)

(deftest-single asin.6
  (asin 0.5)
  0.52359873)

(deftest-single asin.7
  (asin 30)
  1.5707964 4.0940666 #+math-inaccuracy 1e-2)

(deftest-single asin.8
  (asin (make-bignum 30))
  1.5707964 4.0940666 #+math-inaccuracy 1e-2)

(deftest-single asin.9
  (asin 3/4)
  0.84806216)

(deftest-single asin.10
  (asin 1.2f-3)
  0.0012000003)

(deftest-double asin.11
  (asin -4.5d1)
  -1.5707963267948966d0 4.499686190671499d0 #+math-inaccuracy 1e-10)

(deftest-long asin.12
  (asin 6.7l0)
  1.5707963267948966193L0 2.5896384300847235507L0)

(deftest-single asin.13
  (asin #c(1.2 3.4))
  0.32774293 1.9904647 #+math-inaccuracy 1e-3)

(deftest-double asin.14
  (asin #c(5.6d-1 7.8d0))
  0.07109489712702859d0 2.7538638452334756d0)

(deftest-single asin.15
  (asin #c(1 2))
  0.42707857 1.5285708)

(deftest-double asin.16
  (asin #c(2 3.4d0))
  0.5180101152330411d0 2.073496058489464d0)

(deftest-error! asin-error.1
  (eval '(asin)))

(deftest-error! asin-error.2
  (eval '(asin "hello"))
  type-error)

(deftest-error! asin-error.3
  (eval '(asin 10 20)))

;;  ANSI Common Lisp
(deftest asin-test.1
  (asin 0)
  0.0)

(deftest-single asin-test.2
  (acos #c(0 1))
  1.5707963267948966 -0.8813735870195432)


;;
;;  Function ACOS
;;
(deftest-single acos.1
  (acos 0)
  1.5707964)

(deftest-single acos.2
  (acos 1.0)
  0.0)

(deftest-single acos.3
  (acos -1.0)
  3.1415927)

(deftest-single acos.4
  (acos 1.1)
  0 -0.44356826)

#-math-inaccuracy
(deftest-single acos.5
  (acos -1.1)
  3.1415927 -0.44356826)
#+math-inaccuracy
(deftest acos.5
  (values))

(deftest-single acos.6
  (acos 30)
  0 -4.0940666)

(deftest-single acos.7
  (acos (make-bignum 30))
  0 -4.0940666)

(deftest-single acos.8
  (acos 3/4)
  0.7227342)

(deftest-single acos.9
  (acos 1.2f-3)
  1.5695964)

(deftest-double acos.10
  (acos -4.5d1)
  3.141592653589793d0 -4.499686190671499d0 #+math-inaccuracy 1e-10)

(deftest-long acos.11
  (acos 6.7l0)
  0 -2.5896384300847235507L0)

(deftest-single acos.12
  (acos #c(1.2 3.4))
  1.2430534 -1.9904647)

(deftest-double acos.13
  (acos #c(5.6d-1 7.8d0))
  1.499701429667868d0 -2.7538638452334756d0)

(deftest-single acos.14
  (acos #c(1 2))
  1.1437178 -1.5285708)

(deftest-double acos.15
  (acos #c(2 3.4d0))
  1.0527862115618554d0 -2.073496058489464d0)

(deftest-error! acos-error.1
  (eval '(acos)))

(deftest-error! acos-error.2
  (eval '(acos "hello"))
  type-error)

(deftest-error! acos-error.3
  (eval '(acos 10 20)))


;;
;;  Function ATAN
;;
(deftest atan2-range.1
  (atan +0.0 1.0)
  +0.0)

(deftest atan2-range.2
  (atan +0.0 0.1)
  +0.0)

(deftest atan2-range.3
  (< +0.0 (atan 1.0 1.0) (/ pi 2.0))
  t)

(deftest atan2-range.4
  (< +0.0 (atan 1.0 0.1) (/ pi 2.0))
  t)

(deftest atan2-range.5
  (< +0.0 (atan 0.1 1.0) (/ pi 2.0))
  t)

(deftest atan2-range.6
  (< +0.0 (atan 0.1 0.1) (/ pi 2.0))
  t)

(deftest-float atan2-range.7
  (atan 1.0 +0.0)
  #.(/ pi 2.0))

(deftest-float atan2-range.8
  (atan 1.0 -0.0)
  #.(/ pi 2.0))

(deftest-float atan2-range.9
  (atan 0.1 +0.0)
  #.(/ pi 2.0))

(deftest-float atan2-range.10
  (atan 0.1 -0.0)
  #.(/ pi 2.0))

(deftest-float atan2-range.11
  (atan +0.0 -1.0)
  #.pi)

(deftest-float atan2-range.12
  (atan +0.0 -0.1)
  #.pi)

(deftest-float atan2-range.13
  (atan -0.0 -1.0)
  #.(- pi))

(deftest-float atan2-range.14
  (atan -0.0 -0.1)
  #.(- pi))

(deftest atan2-range.15
  (< (- pi) (atan -1.0 -1.0) (- (/ pi 2.0)))
  t)

(deftest atan2-range.16
  (< (- pi) (atan -1.0 -0.1) (- (/ pi 2.0)))
  t)

(deftest atan2-range.17
  (< (- pi) (atan -0.1 -1.0) (- (/ pi 2.0)))
  t)

(deftest atan2-range.18
  (< (- pi) (atan -0.1 -0.1) (- (/ pi 2.0)))
  t)

(deftest-float atan2-range.19
  (atan -1.0 +0.0)
  #.(- (/ pi 2.0)))

(deftest-float atan2-range.20
  (atan -0.1 +0.0)
  #.(- (/ pi 2.0)))

(deftest-float atan2-range.21
  (atan -1.0 -0.0)
  #.(- (/ pi 2.0)))

(deftest-float atan2-range.22
  (atan -0.1 -0.0)
  #.(- (/ pi 2.0)))

(deftest atan2-range.23
  (< (- (/ pi 2.0)) (atan -1.0 +1.0) -0.0)
  t)

(deftest atan2-range.24
  (< (- (/ pi 2.0)) (atan -1.0 +0.1) -0.0)
  t)

(deftest atan2-range.25
  (< (- (/ pi 2.0)) (atan -0.1 +1.0) -0.0)
  t)

(deftest atan2-range.26
  (< (- (/ pi 2.0)) (atan -0.1 +0.1) -0.0)
  t)

(deftest atan2-range.27
  (atan +0.0 +0.0)
  +0.0)

(deftest atan2-range.28
  (atan -0.0 +0.0)
  -0.0)

(deftest-float atan2-range.29
  (atan +0.0 -0.0)
  #.pi)

(deftest-float atan2-range.30
  (atan -0.0 -0.0)
  #.(- pi))


;;  atan1
(deftest-single atan.1
  (atan 0)
  0.0)

(deftest-single atan.2
  (atan -1.0)
  -0.7853981)

(deftest-single atan.3
  (atan 30)
  1.5374753)

(deftest-single atan.4
  (atan (make-bignum 30))
  1.5374753)

(deftest-single atan.5
  (atan 3/4)
  0.64350116)

(deftest-single atan.6
  (atan 1.2f-3)
  0.0011999995)

(deftest-double atan.7
  (atan -4.5d1)
  -1.5485777614681775d0)

(deftest-long atan.8
  (atan 6.7l0)
  1.4226363060630652408L0)

(deftest-single atan.9
  (atan #c(1.2 3.4))
  1.4720986 0.265218)

(deftest-double atan.10
  (atan #c(5.6d-1 7.8d0))
  1.5614879013267753d0 0.12823512882695196d0)

(deftest-single atan.11
  (atan #c(1 2))
  1.3389726 0.4023595)

(deftest-double atan.12
  (atan #c(2 3.4d0))
  1.4367409352599831d0 0.2181831893837563d0)


;;  atan2
(deftest-single atan2.1
  (atan 1 2)
  0.4636476)

(deftest-single atan2.2
  (atan 4 1)
  1.3258177)

(deftest-single atan2.3
  (atan -2 3)
  -0.58800256)

(deftest-single atan2.4
  (atan 2 -3)
  2.5535903)

(deftest-single atan2.5
  (atan -2 -3)
  -2.5535903)

(deftest-single atan2.6
  (atan 0 1)
  0.0)

(deftest-single atan2.7
  (atan 1 0)
  1.5707964)

(deftest-single atan2f.1
  (atan 5 6)
  0.6947382)

(deftest-single atan2f.2
  (atan 5 (make-bignum 6))
  0.6947382)

(deftest-single atan2f.3
  (atan -5 3/4)
  -1.421906)

(deftest-single atan2f.4
  (atan 5 -3.4f0)
  2.167973)

(deftest-double atan2f.5
  (atan -5 5.6d1)
  -0.0890495826344978d0)

(deftest-long atan2f.6
  (atan -5 2.3l-1)
  -1.5248287309977921294L0)

(deftest-single atan2b.1
  (atan (make-bignum 5) 6)
  0.6947382)

(deftest-single atan2b.2
  (atan (make-bignum 5) (make-bignum 6))
  0.6947382)

(deftest-single atan2b.3
  (atan (make-bignum -5) 3/4)
  -1.421906)

(deftest-single atan2b.4
  (atan (make-bignum 5) -3.4f0)
  2.167973)

(deftest-double atan2b.5
  (atan (make-bignum -5) 5.6d1)
  -0.0890495826344978d0)

(deftest-long atan2b.6
  (atan (make-bignum -5) 2.3l-1)
  -1.5248287309977921294L0)

(deftest-single atan2r.1
  (atan 5/6 7)
  0.11848996)

(deftest-single atan2r.2
  (atan 5/6 (make-bignum 7))
  0.11848996)

(deftest-single atan2r.3
  (atan -5/6 3/4)
  -0.8379812)

(deftest-single atan2r.4
  (atan 5/6 -3.4f0)
  2.901233)

(deftest-double atan2r.5
  (atan -5/6 5.6d1)
  -0.014879854100564067d0)

(deftest-long atan2r.6
  (atan -5/6 2.3l-1)
  -1.3015006617463758469L0)

(deftest-single atan2s.1
  (atan 5.0f0 6)
  0.6947382)

(deftest-single atan2s.2
  (atan 5.0f0 (make-bignum 6))
  0.6947382)

(deftest-single atan2s.3
  (atan -5.0f0 3/4)
  -1.421906)

(deftest-single atan2s.4
  (atan 5.0f0 -3.4f0)
  2.167973)

(deftest-double atan2s.5
  (atan -5.0f0 5.6d1)
  -0.0890495826344978d0)

(deftest-long atan2s.6
  (atan -5.0f0 2.3l-11)
  -1.5707963267902967d0)

(deftest-double atan2d.1
  (atan 5.6d0 6)
  0.7509290623979404d0)

(deftest-double atan2d.2
  (atan 5.6d0 (make-bignum 6))
  0.7509290623979404d0)

(deftest-double atan2d.3
  (atan -5.6d0 3/4)
  -1.437659999243249d0)

(deftest-double atan2d.4
  (atan 5.6d0 -3.4f0)
  2.116451393764983d0)

(deftest-long atan2d.5
  (atan -5.6d0 5.6l1)
  -0.09966865249116202d0)

(deftest-long atan2d.6
  (atan -5.6d0 2.3l-1)
  -1.5297479688252098d0)

(deftest-long atan2l.1
  (atan 5.6l0 6)
  0.7509290623979404d0)

(deftest-long atan2l.2
  (atan 5.6l0 (make-bignum 6))
  0.7509290623979404d0)

(deftest-long atan2l.3
  (atan -5.6l0 3/4)
  -1.437659999243249d0)

(deftest-long atan2l.4
  (atan 5.6l0 -3.4f0)
  2.116451393764983d0)

(deftest-long atan2l.5
  (atan -5.6l0 5.6l1)
  -0.09966865249116202d0)

(deftest-long atan2l.6
  (atan -5.6l0 2.3l-1)
  -1.5297479688252098d0)

(deftest-error! atan-error.1
  (eval '(atan)))

(deftest-error! atan-error.2
  (eval '(atan "hello"))
  type-error)

(deftest-error! atan-error.3
  (eval '(atan 10 20 30)))

(deftest-error! atan-error.4
  (eval '(atan #c(10 20) #c(3.4 5.6)))
  type-error)

(deftest-error! atan-error.5
  (eval '(atan 1.0 #c(3.4 5.6)))
  type-error)

(deftest-error! atan-error.6
  (eval '(atan #c(10 20) 2.0))
  type-error)

;;  ANSI Common Lisp
(deftest-single atan-test.1
  (/ (atan 1 (sqrt 3)) 6)
  0.087266)

(deftest-single atan-test.2
  (atan #c(0 2))
  1.5707964 0.54930615)   ;; #c(-1.5707964 0.54930615) ??


;;
;;  Function SINH
;;
(deftest-single sinh.1
  (sinh 0)
  0.0)

(deftest-single sinh.2
  (sinh 3)
  10.017875)

(deftest-single sinh.3
  (sinh -2)
  -3.6268604)

(deftest-single sinh.4
  (sinh (make-bignum 3))
  10.017875)

(deftest-single sinh.5
  (sinh 3/4)
  0.8223167)

(deftest-single sinh.6
  (sinh 1.2f-3)
  0.0012000003)

(deftest-double sinh.7
  (sinh -0.12d1)
  -1.5094613554121725d0)

(deftest-long sinh.8
  (sinh 6.7l-1)
  0.7212643714246986d0)

(deftest-single sinh.9
  (sinh #c(1.2 3.4))
  -1.4593445 -0.46269712)

(deftest-double sinh.10
  (sinh #c(5.6d-1 7.8d0))
  0.03181922287589073d0 1.1592496923124425d0)

(deftest-single sinh.11
  (sinh #c(1 2))
  -0.48905626 1.4031193)

(deftest-double sinh.12
  (sinh #c(2 3.4d0))
  -3.506442087044505d0 -0.9613956329401071d0)

(deftest-error! sinh-error.1
  (eval '(sinh)))

(deftest-error! sinh-error.2
  (eval '(sinh "hello"))
  type-error)

(deftest-error! sinh-error.3
  (eval '(sinh 10 20)))

;;  ANSI Common Lisp
(deftest sinh-test.1
  (sinh 0)
  0.0)


;;
;;  Function COSH
;;
(deftest-single cosh.1
  (cosh 0)
  1.0)

(deftest-float cosh.2
  (cosh -3)
  10.067662)

(deftest-single cosh.3
  (cosh 3)
  10.067662)

(deftest-single cosh.4
  (cosh (make-bignum 3))
  10.067662)

(deftest-single cosh.5
  (cosh 3/4)
  1.2946833)

(deftest-single cosh.6
  (cosh 1.2f-1)
  1.0072086)

(deftest-double cosh.7
  (cosh -0.19d1)
  3.417731530750952d0)

(deftest-long cosh.8
  (cosh -1.7l0)
  2.8283154578899672056L0)

(deftest-single cosh.9
  (cosh #c(1.2 3.4))
  -1.7505386 -0.38572958)

(deftest-double cosh.10
  (cosh #c(5.6d-1 7.8d0))
  0.06263904814505099d0 0.5888726827885506d0)

(deftest-single cosh.11
  (cosh #c(1 2))
  -0.64214814 1.0686073)

(deftest-double cosh.12
  (cosh #c(2 3.4d0))
  -3.6372839942698914d0 -0.9268119055187098d0)

(deftest-error! cosh-error.1
  (eval '(cosh)))

(deftest-error! cosh-error.2
  (eval '(cosh "hello"))
  type-error)

(deftest-error! cosh-error.3
  (eval '(cosh 10 20)))

;;  ANSI Common Lisp
(deftest-float cosh-test.1
  (cosh (complex 0 -1))
  0.540302 -0.0)


;;
;;  Function TANH
;;
(deftest-single tanh.1
  (tanh 0)
  0.0)

(deftest-single tanh.2
  (tanh -30)
  -1.0)

(deftest-single tanh.3
  (tanh -2)
  -0.9640276)

(deftest-single tanh.4
  (tanh (make-bignum 2))
  0.9640276)

(deftest-single tanh.5
  (tanh 3/4)
  0.63514894)

(deftest-single tanh.6
  (tanh 1.2f-3)
  0.0011999995)

(deftest-double tanh.7
  (tanh -4.5d-1)
  -0.4218990052500079d0)

(deftest-long tanh.8
  (tanh 6.7l-1)
  0.5849798828807289d0)

(deftest-single tanh.9
  (tanh #c(1.2 3.4))
  0.85059696 0.07688873)

(deftest-double tanh.10
  (tanh #c(5.6d-1 7.8d0))
  1.9522497139738364d0 0.15362886340809115d0)

(deftest-single tanh.11
  (tanh #c(1 2))
  1.1667362 -0.2434582)

(deftest-double tanh.12
  (tanh #c(2 3.4d0))
  0.9684958203029618d0 0.017535660217539918d0)

(deftest-error! tanh-error.1
  (eval '(tanh)))

(deftest-error! tanh-error.2
  (eval '(tanh "hello"))
  type-error)

(deftest-error! tanh-error.3
  (eval '(tanh 10 20)))


;;
;;  Function ASINH
;;
(deftest-single asinh.1
  (asinh 0)
  0.0)

(deftest-single asinh.2
  (asinh 1.0)
  0.8813736)

(deftest-single asinh.3
  (asinh -1.1)
  -0.95034695)

(deftest-single asinh.4
  (asinh 30)
  4.094622)

(deftest-single asinh.5
  (asinh (make-bignum 30))
  4.094622)

(deftest-single asinh.6
  (asinh 3/4)
  0.6931472)

(deftest-single asinh.7
  (asinh 1.2f-3)
  0.0011999998);

(deftest-double asinh.8
  (asinh -4.5d1)
  -4.49993310426429d0)

(deftest-long asinh.9
  (asinh 6.7l0)
  2.6007779200572094d0)

(deftest-single asinh.10
  (asinh #c(1.2 3.4))
  1.9605457 1.2188689)

(deftest-double asinh.11
  (asinh #c(5.6d-1 7.8d0))
  2.745771153233351d0 1.4985332512262457d0 #+math-inaccuracy 1e-10)

(deftest-single asinh.12
  (asinh #c(1 2))
  1.4693518 1.0634401)

(deftest-double asinh.13
  (asinh #c(2 3.4d0))
  2.057911139343063d0 1.0247016139057237d0)

(deftest-error! asinh-error.1
  (eval '(asinh)))

(deftest-error! asinh-error.2
  (eval '(asinh "hello"))
  type-error)

(deftest-error! asinh-error.3
  (eval '(asinh 10 20)))


;;
;;  Function ACOSH
;;
(deftest-single acosh.1
  (acosh 1.1)
  0.44356832)

(deftest-single acosh.2
  (acosh 1.0)
  0.0)

(deftest-single acosh.3
  (acosh 0.9)
  0.0 0.45102686)

(deftest-single acosh.4
  (acosh 0)
  0.0 1.5707964)

(deftest-single acosh.5
  (acosh 30)
  4.0940666)

(deftest-single acosh.7
  (acosh (make-bignum 30))
  4.0940666)

(deftest-single acosh.8
  (acosh 3/4)
  0.0 0.7227343)

(deftest-single acosh.9
  (acosh 1.2f-3)
  0.0 1.5695964)

(deftest-double acosh.10
  (acosh -4.5d1)
  4.499686190671499d0 3.141592653589793d0 #+math-inaccuracy 1e-10)

(deftest-long acosh.11
  (acosh 6.7l0)
  2.5896384300847237d0)

(deftest-single acosh.12
  (acosh #c(1.2 3.4))
  1.9904652 1.2430533)

(deftest-double acosh.13
  (acosh #c(5.6d-1 7.8d0))
  2.7538638452334747d0 1.4997014296678677d0)

(deftest-single acosh.14
  (acosh #c(1 2))
  1.5285709 1.1437178)

(deftest-double acosh.15
  (acosh #c(2 3.4d0))
  2.0734960584894635d0 1.0527862115618554d0)

(deftest-error! acosh-error.1
  (eval '(acosh)))

(deftest-error! acosh-error.2
  (eval '(acosh "hello"))
  type-error)

(deftest-error! acosh-error.3
  (eval '(acosh 10 20)))


;;
;;  Function ATANH
;;
(deftest-single atanh.1
  (atanh 0)
  0.0)

(deftest-single atanh.2
  (atanh 0.5)
  0.54930615)

(deftest-error atanh.3
  (atanh 1.0))

(deftest-error atanh.4
  (atanh -1.0))

(deftest-single atanh.5
  (atanh -2.0)
  -0.54930615 1.5707964)

(deftest-single atanh.6
  (atanh 30)
  0.033345688 1.5707964)

(deftest-single atanh.7
  (atanh (make-bignum 30))
  0.033345688 1.5707964)

(deftest-single atanh.8
  (atanh 3/4)
  0.97295505)

(deftest-single atanh.9
  (atanh 1.2f-3)
  0.0012000006)

(deftest-double atanh.10
  (atanh -4.5d1)
  -0.02222588128541692d0 1.5707963267948966d0)

(deftest-long atanh.11
  (atanh 6.7l0)
  0.15037707700956685d0 1.5707963267948966d0)

(deftest-single atanh.12
  (atanh #c(1.2 3.4))
  0.086569056 1.3130218)

(deftest-double atanh.13
  (atanh #c(5.6d-1 7.8d0))
  0.009010912086100742d0 1.4439232790136352d0)

(deftest-single atanh.14
  (atanh #c(1 2))
  0.1732868 1.1780972)

(deftest-double atanh.15
  (atanh #c(2 3.4d0))
  0.12320756988672794d0 1.3523323709528103d0)

(deftest-error! atanh-error.4
  (eval '(atanh #c(10 20) #c(3.4 5.6)))
  type-error)

(deftest-error! atanh-error.5
  (eval '(atanh 1.0 #c(3.4 5.6)))
  type-error)

(deftest-error! atanh-error.6
  (eval '(atanh #c(10 20) 2.0))
  type-error)


;;
;;  Function CIS
;;
(deftest-float cis.1
  (cis pi)
  -1.0 0.0)

(deftest-float cis.2
  (cis (* 0.5 pi))
  0.0 1.0)

(deftest-single cis.3
  (cis 20)
  0.40808207 0.9129453)

(deftest-single cis.4
  (cis (make-bignum 20))
  0.40808207 0.9129453)

(deftest-single cis.5
  (cis 3/4)
  0.73168886 0.6816388)

(deftest-single cis.6
  (cis 1.2f0)
  0.3623577544766736f0 0.9320390859672263f0)

(deftest-double cis.7
  (cis -3.4d0)
  -0.96679819257946101425d0 0.25554110202683131935d0)

(deftest-long cis.8
  (cis 5.6l0)
  0.7755658785102497976L0 -0.6312666378723213115L0)

(deftest-error! cis-error.1
  (eval '(cis)))

(deftest-error! cis-error.2
  (eval '(cis #c(10 20)))
  type-error)

(deftest-error! cis-error.3
  (eval '(cis 10 20)))

;;  ANSI Common Lisp
(deftest cis-test.1
  (cis 0)
  #c(1.0 0.0))

;;  degrade
(deftest cis-degrade.1
  (cis 0.0f0)
  #c(1.0f0 0.0f0))

(deftest cis-degrade.2
  (cis 0.0d0)
  #c(1.0d0 0.0d0))

(deftest cis-degrade.3
  (cis 0.0L0)
  #c(1.0L0 0.0L0))

