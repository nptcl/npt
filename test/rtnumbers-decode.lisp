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

;;
;;  Function DECODE-FLOAT
;;
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

(deftest-error! decode-float-error.1
  (eval '(decode-float)))

(deftest-error! decode-float-error.2
  (eval '(decode-float 10))
  type-error)

(deftest-error! decode-float-error.3
  (eval '(decode-float 1.0f0 nil)))


;;
;;  Function SCALE-FLOAT
;;
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

(deftest-error! scale-float-error.1
  (eval '(scale-float 1.2)))

(deftest-error! scale-float-error.2
  (eval '(scale-float 1.2 2/3))
  type-error)

(deftest-error! scale-float-error.3
  (eval '(scale-float 1.2 10 20)))


;;
;;  Function FLOAT-RADIX
;;
(deftest float-radix.1
  (float-radix -1.23s0)
  2)

(deftest float-radix.2
  (float-radix -1.23f0)
  2)

(deftest float-radix.3
  (float-radix 0.0d0)
  2)

(deftest float-radix.4
  (float-radix 1.0L0)
  2)

(deftest-error! float-radix-error.1
  (eval '(float-radix)))

(deftest-error! float-radix-error.2
  (eval '(float-radix 10))
  type-error)

(deftest-error! float-radix-error.3
  (eval '(float-radix 1.0 1.0)))


;;  Function FLOAT-SIGN
(deftest float-sign1.f1
  (float-sign 12.3f0)
  1.0f0)

(deftest float-sign1.f2
  (float-sign -0.5f0)
  -1.0f0)

(deftest float-sign1.f3
  (float-sign 0.0f0)
  1.0f0)

(deftest float-sign1.f4
  (float-sign -0.0f0)
  -1.0f0)

(deftest float-sign1.d1
  (float-sign 12.3d0)
  1.0d0)

(deftest float-sign1.d2
  (float-sign -0.5d0)
  -1.0d0)

(deftest float-sign1.d3
  (float-sign 0.0d0)
  1.0d0)

(deftest float-sign1.d4
  (float-sign -0.0d0)
  -1.0d0)

(deftest float-sign1.L1
  (float-sign 12.3L0)
  1.0L0)

(deftest float-sign1.L2
  (float-sign -0.5L0)
  -1.0L0)

(deftest float-sign1.L3
  (float-sign 0.0L0)
  1.0L0)

(deftest float-sign1.L4
  (float-sign -0.0L0)
  -1.0L0)

(deftest-single float-sign.f1
  (float-sign 12.3f0 45.6f0)
  45.6f0)

(deftest-single float-sign.f2
  (float-sign -12.3f0 45.6f0)
  -45.6f0)

(deftest-single float-sign.f3
  (float-sign -0.0f0 45.6f0)
  -45.6f0)

(deftest-double float-sign.f4
  (float-sign 12.3f0 45.6d0)
  45.6d0)

(deftest-long float-sign.f5
  (float-sign -12.3f0 45.6L0)
  -45.6L0)

(deftest-single float-sign.d1
  (float-sign 12.3d0 45.6f0)
  45.6f0)

(deftest-double float-sign.d2
  (float-sign -12.3d0 45.6d0)
  -45.6d0)

(deftest-double float-sign.d3
  (float-sign -0.0d0 45.6d0)
  -45.6d0)

(deftest-double float-sign.d4
  (float-sign 12.3d0 45.6d0)
  45.6d0)

(deftest-long float-sign.d5
  (float-sign -12.3d0 45.6L0)
  -45.6L0)

(deftest-single float-sign.L1
  (float-sign 12.3L0 45.6f0)
  45.6f0)

(deftest-double float-sign.L2
  (float-sign -12.3L0 45.6d0)
  -45.6d0)

(deftest-long float-sign.L3
  (float-sign -0.0L0 45.6L0)
  -45.6L0)

(deftest-double float-sign.L4
  (float-sign 12.3L0 45.6d0)
  45.6d0)

(deftest-long float-sign.L5
  (float-sign -12.3L0 45.6L0)
  -45.6L0)

(deftest-error! float-sign-error.1
  (eval '(float-sign)))

(deftest-error! float-sign-error.2
  (eval '(float-sign 1.0 3/4))
  type-error)

(deftest-error! float-sign-error.3
  (eval '(float-sign 1.0 3.0 4.0)))


;;
;;  Function FLOAT-DIGITS
;;
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

(deftest-error! float-digits-error.1
  (eval '(float-digits)))

(deftest-error! float-digits-error.2
  (eval '(float-digits 10))
  type-error)

(deftest-error! float-digits-error.3
  (eval '(float-digits 2.0 nil)))


;;
;;  Function FLOAT-PRECISION
;;
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

(deftest-error! float-precision-error.1
  (eval '(float-precision)))

(deftest-error! float-precision-error.2
  (eval '(float-precision 10))
  type-error)

(deftest-error! float-precision-error.3
  (eval '(float-precision 1.0 nil)))


;;
;;  Function INTEGER-DECODE-FLOAT
;;
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

(deftest-error! integer-decode-float-error.1
  (eval '(integer-decode-float)))

(deftest-error! integer-decode-float-error.2
  (eval '(integer-decode-float 10))
  type-error)

(deftest-error! integer-decode-float-error.3
  (eval '(integer-decode-float 1.0 nil)))


;;  ANSI Common Lisp
(deftest float-decode-test.1
  (decode-float .5)
  0.5 0 1.00)

(deftest float-decode-test.2
  (decode-float 1.0)
  0.5 1 1.0)

(deftest float-decode-test.3
  (scale-float 1.0 1)
  2.0)

(deftest float-decode-test.4
  (scale-float 10.01 -2)
  2.5025)

(deftest float-decode-test.5
  (scale-float 23.0 0)
  23.0)

(deftest float-decode-test.6
  (float-radix 1.0)
  2)

(deftest float-decode-test.7
  (float-sign 5.0)
  1.0)

(deftest float-decode-test.8
  (float-sign -5.0)
  -1.0)

(deftest float-decode-test.9
  (float-sign 0.0)
  1.0)

(deftest float-decode-test.10
  (float-sign -0.0)
  -1.0)

(deftest float-decode-test.11
  (float-sign 1.0 0.0)
  0.0)

(deftest float-decode-test.12
  (float-sign 1.0 -10.0)
  10.0)

(deftest float-decode-test.13
  (float-sign -1.0 10.0)
  -10.0)

(deftest float-decode-test.14
  (float-digits 1.0)
  24)

(deftest float-decode-test.15
  (float-precision 1.0)
  24)

(deftest float-decode-test.16
  (float-precision least-positive-single-float)
  1)

(deftest float-decode-test.17
  (integer-decode-float 1.0)
  8388608 -23 1)

