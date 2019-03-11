;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;  single-float
;;    0x1.FFFFP-126, 2.350970f-38, normal
;;    0x1.FFFFP-127, 1.175485f-38, sub-normal
;;  double-float
;;    0x1.FFFFP-1022, 4.450113d-308, normal
;;    0x1.FFFFP-1023, 2.225056d-308, sub-normal
;;  long-double 80bit
;;    0x1.FFFFP-16382, 6.724154L-4932, normal
;;    0x1.FFFFP-16383, 3.362077L-4932, sub-normal
;;

;;  decode-float
(defun decode-single-float (v x y z &optional (eps 1.0e-6))
  (multiple-value-bind (a b c) (decode-float v)
    (and (typep x 'single-float)
         (typep y 'integer)
         (typep z 'single-float)
         (equal-eps x a :eps eps)
         (= y b)
         (= z c))))

(defun decode-double-float (v x y z &optional (eps 1.0e-14))
  (multiple-value-bind (a b c) (decode-float v)
    (and (typep x 'double-float)
         (typep y 'integer)
         (typep z 'double-float)
         (equal-eps x a :eps eps)
         (= y b)
         (= z c))))

(defun decode-long-float (v x y z &optional (eps 1.0e-14))
  (multiple-value-bind (a b c) (decode-float v)
    (and (typep x 'long-float)
         (typep y 'integer)
         (typep z 'long-float)
         (equal-eps x a :eps eps)
         (= y b)
         (= z c))))

(deftest decode-float.s1
  (decode-single-float 0.5f0   0.5f0 0 1.0f0)
  t)

(deftest decode-float.s2
  (decode-single-float -1.0f0  0.5f0 1 -1.0f0)
  t)

(deftest decode-float.s3
  (decode-single-float -1.0f0   0.5f0 1 -1.0f0)
  t)

(deftest decode-float.s4
  (decode-single-float -1.234f8   0.91940165f0 27 -1.0f0)
  t)

(deftest decode-float.s5
  (decode-single-float 1.234f-12   0.67839867 -39 1.0f0)
  t)

(deftest decode-float.d1
  (decode-double-float 0.5d0   0.5d0 0 1.0d0)
  t)

(deftest decode-float.d2
  (decode-double-float -1.0d0  0.5d0 1 -1.0d0)
  t)

(deftest decode-float.d3
  (decode-double-float -1.0d0   0.5d0 1 -1.0d0)
  t)

(deftest decode-float.d4
  (decode-double-float -1.234d8   0.9194016456604004d0 27 -1.0d0)
  t)

(deftest decode-float.d5
  (decode-double-float 1.234d-12   0.678398674337792d0 -39 1.0d0)
  t)

(deftest decode-float.L1
  (decode-long-float 0.5L0   0.5L0 0 1.0L0)
  t)

(deftest decode-float.L2
  (decode-long-float -1.0L0  0.5L0 1 -1.0L0)
  t)

(deftest decode-float.L3
  (decode-long-float -1.0L0   0.5L0 1 -1.0L0)
  t)

(deftest decode-float.L4
  (decode-long-float -1.234L8   0.9194016456604004L0 27 -1.0L0)
  t)

(deftest decode-float.L5
  (decode-long-float 1.234L-12   0.678398674337792L0 -39 1.0L0)
  t)


;;  scale-float
(deftest-single scale-float.f1
  (scale-float 0.0f0 0)
  0.0f0)

(deftest-single scale-float.f2
  (scale-float 0.0f0 2)
  0.0f0)

(deftest-single scale-float.f3
  (scale-float 0.0f0 -2)
  0.0f0)

(deftest-single scale-float.f4
  (scale-float 0.25f0 1)
  0.5f0)

(deftest-single scale-float.f5
  (scale-float -0.25f0 2)
  -1.0f0)

(deftest-single scale-float.f6
  (scale-float -0.25f0 -5)
  -0.0078125f0)

(deftest-single scale-float.f7
  (scale-float 12.345f0 12)
  50565.12f0)

(deftest-double scale-float.d1
  (scale-float 0.0d0 0)
  0.0d0)

(deftest-double scale-float.d2
  (scale-float 0.0d0 2)
  0.0d0)

(deftest-double scale-float.d3
  (scale-float 0.0d0 -2)
  0.0d0)

(deftest-double scale-float.d4
  (scale-float 0.25d0 1)
  0.5d0)

(deftest-double scale-float.d5
  (scale-float -0.25d0 2)
  -1.0d0)

(deftest-double scale-float.d6
  (scale-float -0.25d0 -5)
  -0.0078125d0)

(deftest-double scale-float.d7
  (scale-float 12.345d0 12)
  50565.12d0)

(deftest-long scale-float.L1
  (scale-float 0.0L0 0)
  0.0L0)

(deftest-long scale-float.L2
  (scale-float 0.0L0 2)
  0.0L0)

(deftest-long scale-float.L3
  (scale-float 0.0L0 -2)
  0.0L0)

(deftest-long scale-float.L4
  (scale-float 0.25L0 1)
  0.5L0)

(deftest-long scale-float.L5
  (scale-float -0.25L0 2)
  -1.0L0)

(deftest-long scale-float.L6
  (scale-float -0.25L0 -5)
  -0.0078125L0)

(deftest-long scale-float.L7
  (scale-float 12.345L0 12)
  50565.12L0)


;;  float-radix
(deftest float-radix.1
  (values
    (float-radix -1.23s0)
    (float-radix -1.23f0)
    (float-radix 0.0d0)
    (float-radix 1.0L0))
  2 2 2 2)


;;  float-sign
(deftest float-sign1f.1
  (float-sign 12.3f0)
  1.0f0)

(deftest float-sign1f.2
  (float-sign -0.5f0)
  -1.0f0)

(deftest float-sign1f.3
  (float-sign 0.0f0)
  1.0f0)

(deftest float-sign1f.4
  (float-sign -0.0f0)
  -1.0f0)

(deftest float-sign1d.1
  (float-sign 12.3d0)
  1.0d0)

(deftest float-sign1d.2
  (float-sign -0.5d0)
  -1.0d0)

(deftest float-sign1d.3
  (float-sign 0.0d0)
  1.0d0)

(deftest float-sign1d.4
  (float-sign -0.0d0)
  -1.0d0)

(deftest float-sign1L.1
  (float-sign 12.3L0)
  1.0L0)

(deftest float-sign1L.2
  (float-sign -0.5L0)
  -1.0L0)

(deftest float-sign1L.3
  (float-sign 0.0L0)
  1.0L0)

(deftest float-sign1L.4
  (float-sign -0.0L0)
  -1.0L0)

(deftest-single float-signf.1
  (float-sign 12.3f0 45.6f0)
  45.6f0)

(deftest-single float-signf.2
  (float-sign -12.3f0 45.6f0)
  -45.6f0)

(deftest-single float-signf.3
  (float-sign -0.0f0 45.6f0)
  -45.6f0)

(deftest-double float-signf.4
  (float-sign 12.3f0 45.6d0)
  45.6d0)

(deftest-long float-signf.5
  (float-sign -12.3f0 45.6L0)
  -45.6L0)

(deftest-single float-signd.1
  (float-sign 12.3d0 45.6f0)
  45.6f0)

(deftest-double float-signd.2
  (float-sign -12.3d0 45.6d0)
  -45.6d0)

(deftest-double float-signd.3
  (float-sign -0.0d0 45.6d0)
  -45.6d0)

(deftest-double float-signd.4
  (float-sign 12.3d0 45.6d0)
  45.6d0)

(deftest-long float-signd.5
  (float-sign -12.3d0 45.6L0)
  -45.6L0)

(deftest-single float-signl.1
  (float-sign 12.3L0 45.6f0)
  45.6f0)

(deftest-double float-signl.2
  (float-sign -12.3L0 45.6d0)
  -45.6d0)

(deftest-long float-signl.3
  (float-sign -0.0L0 45.6L0)
  -45.6L0)

(deftest-double float-signl.4
  (float-sign 12.3L0 45.6d0)
  45.6d0)

(deftest-long float-signl.5
  (float-sign -12.3L0 45.6L0)
  -45.6L0)


;;  float-digits
(deftest float-digits.1
  (float-digits 1.2f0)
  24)

(deftest float-digits.2
  (float-digits -1.0d0)
  53)

#+long-float-64
(deftest float-digits.3
  (float-digits 1.0L0)
  53)

#+long-float-80
(deftest float-digits.3
  (float-digits 1.0L0)
  64)

#+long-float-128
(deftest float-digits.3
  (float-digits 1.0L0)
  113)

(deftest float-digits.4
  (values
    (float-digits 2.350970f-38)
    (float-digits 1.175485f-38)
    (float-digits 4.450113d-308)
    (float-digits 2.225056d-308))
  24 24 53 53)

#+long-float-64
(deftest float-digits.4a
  (values
    (float-digits 4.450113L-308)
    (float-digits 2.225056L-308))
  53 53)

#+long-float-80
(deftest float-digits.4a
  (values
    (float-digits 6.724154L-4932)
    (float-digits 3.362077L-4932))
  64 64)

(deftest float-digits.5
  (values
    (float-digits 0.0f0)
    (float-digits -0.0f0)
    (float-digits 0.0d0)
    (float-digits -0.0d0))
  24 24 53 53)

#+long-float-64
(deftest float-digits.5a
  (values
    (float-digits 0.0L0)
    (float-digits -0.0L0))
  53 53)

#+long-float-80
(deftest float-digits.5a
  (values
    (float-digits 0.0L0)
    (float-digits -0.0L0))
  64 64)

#+long-float-128
(deftest float-digits.5a
  (values
    (float-digits 0.0L0)
    (float-digits -0.0L0))
  113 113)


;;  float-precision
(deftest float-precision.1
  (float-precision 1.2f0)
  24)

(deftest float-precision.2
  (float-precision -1.0d0)
  53)

#+long-float-64
(deftest float-precision.3
  (float-precision 1.0L0)
  53)

#+long-float-80
(deftest float-precision.3
  (float-precision 1.0L0)
  64)

#+long-float-128
(deftest float-precision.3
  (float-precision 1.0L0)
  113)

(deftest float-precision.4
  (values
    (float-precision 2.350970f-38)
    (float-precision 1.175485f-38)
    (float-precision 4.450113d-308)
    (float-precision 2.225056d-308))
  24 23 53 52)

#+long-float-64
(deftest float-precision.4a
  (values
    (float-precision 4.450113L-308)
    (float-precision 2.225056L-308))
  53 52)

#+long-float-80
(deftest float-precision.4a
  (values
    (float-precision 6.724154L-4932)
    (float-precision 3.362077L-4932))
  64 63)

(deftest float-precision.5
  (values
    (float-precision 0.0f0)
    (float-precision -0.0f0)
    (float-precision 0.0d0)
    (float-precision -0.0d0)
    (float-precision 0.0L0)
    (float-precision -0.0L0))
  0 0 0 0 0 0)


;;  integer-decode-float
(deftest integer-decode-float.1
  (integer-decode-float 0.5f0)
  8388608 -24 1)

(deftest integer-decode-float.2
  (integer-decode-float -1.2f0)
  10066330 -23 -1)

(deftest integer-decode-float.3
  (integer-decode-float 1.23f10)
  12011719 10 1)

(deftest integer-decode-float.4
  (integer-decode-float -9.87f-11)
  14224169 -57 -1)

(deftest integer-decode-float.5
  (integer-decode-float 0.5d0)
  4503599627370496 -53 1)

(deftest integer-decode-float.6
  (integer-decode-float -1.2d0)
  5404319552844595 -52 -1)

(deftest integer-decode-float.7
  (integer-decode-float 1.23d10)
  6448742400000000 -19 1)

(deftest integer-decode-float.8
  (integer-decode-float -9.87d-11)
  7636542617341690 -86 -1)

#+long-float-64
(deftest integer-decode-float.9
  (integer-decode-float 0.5L0)
  4503599627370496 -53 1)

#+long-float-64
(deftest integer-decode-float.10
  (integer-decode-float -1.2L0)
  5404319552844595 -52 -1)

#+long-float-64
(deftest integer-decode-float.11
  (integer-decode-float 1.23L10)
  6448742400000000 -19 1)

#+long-float-64
(deftest integer-decode-float.12
  (integer-decode-float -9.87L-11)
  7636542617341690 -86 -1)

#+long-float-80
(deftest integer-decode-float.9
  (integer-decode-float 0.5L0)
  9223372036854775808 -64 1)

#+long-float-80
(deftest integer-decode-float.10
  (integer-decode-float -1.2L0)
  11068046444225730970 -63 -1)

#+long-float-80
(deftest integer-decode-float.11
  (integer-decode-float 1.23L10)
  13207024435200000000 -30 1)

#+long-float-80
(deftest integer-decode-float.12
  (integer-decode-float -9.87L-11)
  15639639280315780241 -97 -1)


;;
;;  rational
;;
(deftest rational.1
  (rational 0)
  0)

(deftest rational.2
  (rational -12)
  -12)

(deftest rational.3
  (rational 3/4)
  3/4)

(deftest rationalf.1
  (rational 1.0f0)
  1)

(deftest rationalf.2
  (rational 0.5f0)
  1/2)

(deftest rationalf.3
  (rational -0.125f0)
  -1/8)

(deftest rationalf.4
  (rational 0.1f0)
  13421773/134217728)

(deftest rationalf.5
  (rational -1.23f5)
  -123000)

(deftest rationalf.6
  (rational 4.56f-2)
  12240657/268435456)

(deftest rationald.1
  (rational 1.0d0)
  1)

(deftest rationald.2
  (rational 0.5d0)
  1/2)

(deftest rationald.3
  (rational -0.125d0)
  -1/8)

(deftest rationald.4
  (rational 0.1d0)
  3602879701896397/36028797018963968)

(deftest rationald.5
  (rational -1.23d5)
  -123000)

(deftest rationald.6
  (rational 4.57d-2)
  6586064095066613/144115188075855872)

(deftest rationalL.1
  (rational 1.0d0)
  1)

(deftest rationalL.2
  (rational 0.5L0)
  1/2)

(deftest rationalL.3
  (rational -0.125L0)
  -1/8)

#+long-float-64
(deftest rationalL.4
  (rational 0.1L0)
  3602879701896397/36028797018963968)

#+long-float-80
(deftest rationalL.4
  (rational 0.1L0)
  14757395258967641293/147573952589676412928)

(deftest rationalL.5
  (rational -1.23L5)
  -123000)

#+long-float-64
(deftest rationalL.6
  (rational 4.56L-2)
  1642913144064757/36028797018963968)

#+long-float-80
(deftest rationalL.6
  (rational 4.56L-2)
  13458744476178488859/295147905179352825856)


;;
;;  rationalize
;;
(deftest rationalize.1
  (rationalize 0)
  0)

(deftest rationalize.2
  (rationalize -12)
  -12)

(deftest rationalize.3
  (rationalize 3/4)
  3/4)

(deftest rationalizef.1
  (rationalize 1.0f0)
  1)

(deftest rationalizef.2
  (rationalize 0.5f0)
  1/2)

(deftest rationalizef.3
  (rationalize -0.125f0)
  -1/8)

(deftest rationalizef.4
  (rationalize 0.1f0)
  1/10)

(deftest rationalizef.5
  (rationalize -1.23f5)
  -123000)

(deftest rationalizef.6
  (rationalize 4.56f-2)
  57/1250)

(deftest rationalized.1
  (rationalize 1.0d0)
  1)

(deftest rationalized.2
  (rationalize 0.5d0)
  1/2)

(deftest rationalized.3
  (rationalize -0.125d0)
  -1/8)

(deftest rationalized.4
  (rationalize 0.1d0)
  1/10)

(deftest rationalized.5
  (rationalize -1.23d5)
  -123000)

(deftest rationalized.6
  (rationalize 4.57d-2)
  457/10000)

(deftest rationalizeL.1
  (rationalize 1.0d0)
  1)

(deftest rationalizeL.2
  (rationalize 0.5L0)
  1/2)

(deftest rationalizeL.3
  (rationalize -0.125L0)
  -1/8)

(deftest rationalizeL.4
  (rationalize 0.1L0)
  1/10)

(deftest rationalizeL.5
  (rationalize -1.23L5)
  -123000)

(deftest rationalizeL.6
  (rationalize 4.56L-2)
  57/1250)

