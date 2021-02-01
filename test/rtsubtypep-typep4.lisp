;;
;;  typep
;;

;;
;;  number
;;
(deftest typep-number.1
  (typep 10 'number)
  t)

(deftest typep-number.2
  (typep #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF 'number)
  t)

(deftest typep-number.3
  (typep -5/6 'number)
  t)

(deftest typep-number.4
  (typep 0.25e0 'number)
  t)

(deftest typep-number.5
  (typep 0.25s0 'number)
  t)

(deftest typep-number.6
  (typep 0.25f0 'number)
  t)

(deftest typep-number.7
  (typep 0.25d0 'number)
  t)

(deftest typep-number.8
  (typep 0.25L0 'number)
  t)

(deftest typep-number.9
  (typep #c(1.0 2.0) 'number)
  t)

(deftest typep-number.10
  (typep #\A 'number)
  nil)


;;
;;  ratio
;;
(deftest typep-ratio.1
  (typep 3/4 'ratio)
  t)

(deftest typep-ratio.2
  (typep 40 'ratio)
  nil)


;;
;;  complex
;;
(deftest typep-complex.1
  (typep #c(2 3) 'complex)
  t)

(deftest typep-complex.2
  (typep #c(2.3 4.5) '(complex))
  t)

(deftest typep-complex.3
  (typep #c(2.3 4.5) '(complex *))
  t)

(deftest typep-complex.4
  (typep #c(2.3 4.5) '(complex integer))
  nil)

(deftest typep-complex.5
  (typep #c(2 4) '(complex long-float))
  nil)

(deftest typep-complex.6
  (typep #c(2 4) '(complex rational))
  t)

(deftest typep-complex.7
  (typep 100 '(complex integer))
  nil)


;;
;;  real
;;
(deftest typep-real.1
  (typep 10 'real)
  t)

(deftest typep-real.2
  (typep #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF '(real))
  t)

(deftest typep-real.3
  (typep -5/6 '(real *))
  t)

(deftest typep-real.4
  (typep 0.25f0 '(real * *))
  t)

(deftest typep-real.5
  (typep 0.25d0 'real)
  t)

(deftest typep-real.6
  (typep 0.25L0 'real)
  t)

(deftest typep-real.7
  (typep #c(1.0 2.0) 'real)
  nil)

(deftest typep-real.8
  (typep #\A 'real)
  nil)

(deftest typep-real-range.1
  (typep 10.0 '(real 9.0))
  t)

(deftest typep-real-range.2
  (typep 10.0 '(real 10))
  t)

(deftest typep-real-range.3
  (typep 10.0 '(real (10)))
  nil)

(deftest typep-real-range.4
  (typep 10.0 '(real 11.0))
  nil)

(deftest typep-real-range.5
  (typep 10.0 '(real 5 9))
  nil)

(deftest typep-real-range.6
  (typep 10.0 '(real 5 10.0))
  t)

(deftest typep-real-range.7
  (typep 10.0 '(real 5 (10.0)))
  nil)

(deftest typep-real-range.8
  (typep 10.0 '(real 5 11))
  t)


;;
;;  rational
;;
(deftest typep-rational.1
  (typep 10 'rational)
  t)

(deftest typep-rational.2
  (typep #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF '(rational))
  t)

(deftest typep-rational.3
  (typep -5/6 '(rational *))
  t)

(deftest typep-rational.4
  (typep 0.25f0 '(rational * *))
  nil)

(deftest typep-rational.5
  (typep 0.25d0 'rational)
  nil)

(deftest typep-rational.6
  (typep 0.25L0 'rational)
  nil)

(deftest typep-rational.7
  (typep #c(1.0 2.0) 'rational)
  nil)

(deftest typep-rational.8
  (typep #\A 'rational)
  nil)

(deftest typep-rational-range.1
  (typep 10 '(rational 9))
  t)

(deftest typep-rational-range.2
  (typep 10 '(rational 10))
  t)

(deftest typep-rational-range.3
  (typep 10 '(rational (10)))
  nil)

(deftest typep-rational-range.4
  (typep 10 '(rational 11))
  nil)

(deftest typep-rational-range.5
  (typep 10 '(rational 5 9))
  nil)

(deftest typep-rational-range.6
  (typep 10 '(rational 5 10))
  t)

(deftest typep-rational-range.7
  (typep 10 '(rational 5 (10)))
  nil)

(deftest typep-rational-range.8
  (typep 10 '(rational 5 11))
  t)

(deftest typep-rational-range.9
  (typep 10 '(rational 5/6))
  t)


;;
;;  integer
;;
(deftest typep-integer.1
  (typep 10 'integer)
  t)

(deftest typep-integer.2
  (typep #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF '(integer))
  t)

(deftest typep-integer.3
  (typep -5/6 '(integer *))
  nil)

(deftest typep-integer.4
  (typep 0.25f0 '(integer * *))
  nil)

(deftest typep-integer.5
  (typep 0.25d0 'integer)
  nil)

(deftest typep-integer.6
  (typep 0.25L0 'integer)
  nil)

(deftest typep-integer.7
  (typep #c(1.0 2.0) 'integer)
  nil)

(deftest typep-integer.8
  (typep #\A 'integer)
  nil)

(deftest typep-integer-range.1
  (typep 10 '(integer 9))
  t)

(deftest typep-integer-range.2
  (typep 10 '(integer 10))
  t)

(deftest typep-integer-range.3
  (typep 10 '(integer (10)))
  nil)

(deftest typep-integer-range.4
  (typep 10 '(integer 11))
  nil)

(deftest typep-integer-range.5
  (typep 10 '(integer 5 9))
  nil)

(deftest typep-integer-range.6
  (typep 10 '(integer 5 10))
  t)

(deftest typep-integer-range.7
  (typep 10 '(integer 5 (10)))
  nil)

(deftest typep-integer-range.8
  (typep 10 '(integer 5 11))
  t)


;;
;;  float
;;
(deftest typep-float.1
  (typep 10 'float)
  nil)

(deftest typep-float.2
  (typep #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF '(float))
  nil)

(deftest typep-float.3
  (typep -5/6 '(float *))
  nil)

(deftest typep-float.4
  (typep 0.25f0 '(float * *))
  t)

(deftest typep-float.5
  (typep 0.25d0 'float)
  t)

(deftest typep-float.6
  (typep 0.25L0 'float)
  t)

(deftest typep-float.7
  (typep #c(1.0 2.0) 'float)
  nil)

(deftest typep-float.8
  (typep #\A 'float)
  nil)

(deftest typep-float-range.1
  (typep 10.0 '(float 9.0))
  t)

(deftest typep-float-range.2
  (typep 10.0f0 '(float 10.0d0))
  t)

(deftest typep-float-range.3
  (typep 10.0d0 '(float (10.0L0)))
  nil)

(deftest typep-float-range.4
  (typep 10.0L0 '(float 11.0e0))
  nil)

(deftest typep-float-range.5
  (typep 10.0L0 '(float 5.0f0 9.0d0))
  nil)

(deftest typep-float-range.6
  (typep 10.0d0 '(float 5.0L0 10.0e0))
  t)

(deftest typep-float-range.7
  (typep 10.0e0 '(float 5.0f0 (10.0d0)))
  nil)

(deftest typep-float-range.8
  (typep 10.0 '(float 5.0L0 11.0e0))
  t)


;;
;;  single-float
;;
(deftest typep-single-float.1
  (typep 10 'single-float)
  nil)

(deftest typep-single-float.2
  (typep #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF '(single-float))
  nil)

(deftest typep-single-float.3
  (typep -5/6 '(single-float *))
  nil)

(deftest typep-single-float.4
  (typep 0.25f0 '(single-float * *))
  t)

(deftest typep-single-float.5
  (typep 0.25d0 'single-float)
  nil)

(deftest typep-single-float.6
  (typep 0.25L0 'single-float)
  nil)

(deftest typep-single-float.7
  (typep #c(1.0 2.0) 'single-float)
  nil)

(deftest typep-single-float.8
  (typep #\A 'single-float)
  nil)

(deftest typep-single-float-range.1
  (typep 10.0f0 '(single-float 9.0f0))
  t)

(deftest typep-single-float-range.2
  (typep 10.0f0 '(single-float 10.0f0))
  t)

(deftest typep-single-float-range.3
  (typep 10.0f0 '(single-float (10.0f0)))
  nil)

(deftest typep-single-float-range.4
  (typep 10.0f0 '(single-float 11.0f0))
  nil)

(deftest typep-single-float-range.5
  (typep 10.0f0 '(single-float 5.0f0 9.0f0))
  nil)

(deftest typep-single-float-range.6
  (typep 10.0f0 '(single-float 5.0f0 10.0f0))
  t)

(deftest typep-single-float-range.7
  (typep 10.0f0 '(single-float 5.0f0 (10.0f0)))
  nil)

(deftest typep-single-float-range.8
  (typep 10.0f0 '(single-float 5.0f0 11.0f0))
  t)


;;
;;  double-float
;;
(deftest typep-double-float.1
  (typep 10 'double-float)
  nil)

(deftest typep-double-float.2
  (typep #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF '(double-float))
  nil)

(deftest typep-double-float.3
  (typep -5/6 '(double-float *))
  nil)

(deftest typep-double-float.4
  (typep 0.25f0 '(double-float * *))
  nil)

(deftest typep-double-float.5
  (typep 0.25d0 'double-float)
  t)

(deftest typep-double-float.6
  (typep 0.25L0 'double-float)
  nil)

(deftest typep-double-float.7
  (typep #c(1.0 2.0) 'double-float)
  nil)

(deftest typep-double-float.8
  (typep #\A 'double-float)
  nil)

(deftest typep-double-float-range.1
  (typep 10.0d0 '(double-float 9.0d0))
  t)

(deftest typep-double-float-range.2
  (typep 10.0d0 '(double-float 10.0d0))
  t)

(deftest typep-double-float-range.3
  (typep 10.0d0 '(double-float (10.0d0)))
  nil)

(deftest typep-double-float-range.4
  (typep 10.0d0 '(double-float 11.0d0))
  nil)

(deftest typep-double-float-range.5
  (typep 10.0d0 '(double-float 5.0d0 9.0d0))
  nil)

(deftest typep-double-float-range.6
  (typep 10.0d0 '(double-float 5.0d0 10.0d0))
  t)

(deftest typep-double-float-range.7
  (typep 10.0d0 '(double-float 5.0d0 (10.0d0)))
  nil)

(deftest typep-double-float-range.8
  (typep 10.0d0 '(double-float 5.0d0 11.0d0))
  t)


;;
;;  long-float
;;
(deftest typep-long-float.1
  (typep 10 'long-float)
  nil)

(deftest typep-long-float.2
  (typep #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF '(long-float))
  nil)

(deftest typep-long-float.3
  (typep -5/6 '(long-float *))
  nil)

(deftest typep-long-float.4
  (typep 0.25f0 '(long-float * *))
  nil)

(deftest typep-long-float.5
  (typep 0.25d0 'long-float)
  nil)

(deftest typep-long-float.6
  (typep 0.25L0 'long-float)
  t)

(deftest typep-long-float.7
  (typep #c(1.0 2.0) 'long-float)
  nil)

(deftest typep-long-float.8
  (typep #\A 'long-float)
  nil)

(deftest typep-long-float-range.1
  (typep 10.0L0 '(long-float 9.0L0))
  t)

(deftest typep-long-float-range.2
  (typep 10.0L0 '(long-float 10.0L0))
  t)

(deftest typep-long-float-range.3
  (typep 10.0L0 '(long-float (10.0L0)))
  nil)

(deftest typep-long-float-range.4
  (typep 10.0L0 '(long-float 11.0L0))
  nil)

(deftest typep-long-float-range.5
  (typep 10.0L0 '(long-float 5.0L0 9.0L0))
  nil)

(deftest typep-long-float-range.6
  (typep 10.0L0 '(long-float 5.0L0 10.0L0))
  t)

(deftest typep-long-float-range.7
  (typep 10.0L0 '(long-float 5.0L0 (10.0L0)))
  nil)

(deftest typep-long-float-range.8
  (typep 10.0L0 '(long-float 5.0L0 11.0L0))
  t)

