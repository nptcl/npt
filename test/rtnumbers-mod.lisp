;;
;;  ANSI COMMON LISP: 12. Numbers
;;


;;
;;  Function MOD
;;
(deftest mod.1
  (mod -1 5)
  4)

(deftest mod.2
  (mod 13 4)
  1)

(deftest mod.3
  (mod -13 4)
  3)

(deftest mod.4
  (mod 13 -4)
  -3)

(deftest mod.5
  (mod -13 -4)
  -1)

(deftest-single mod.6
  (mod 13.4 1)
  0.4)

(deftest-single mod.7
  (mod -13.4 1)
  0.6)

(deftest mod-fixnum.1
  (mod 77 12)
  5)

(deftest mod-fixnum.2
  (mod -77 12)
  7)

(deftest mod-fixnum.3
  (mod 77 -12)
  -7)

(deftest mod-fixnum.4
  (mod -77 -12)
  -5)

(deftest-error mod-fixnum.5
  (mod 10 0)
  division-by-zero)

(deftest mod-fixnum.6
  (mod 0 100)
  0)

(deftest mod-fixnum.7
  (mod 3 100)
  3)

(deftest mod-fixnum.8
  (mod 77 (make-bignum 12))
  5)

(deftest mod-fixnum.9
  (mod 77 3/4)
  1/2)

(deftest-single mod-fixnum.10
  (mod 77 23.3f0)
  7.1 0.0 1e-4)

(deftest-double mod-fixnum.11
  (mod 77 1.23d0)
  0.74d0)

(deftest-long mod-fixnum.12
  (mod 77 3.21L0)
  3.17L0)

(deftest mod-bignum.1
  (mod (make-bignum 77) 12)
  5)

(deftest mod-bignum.2
  (mod (make-bignum 77) -12)
  -7)

(deftest-error mod-bignum.3
  (mod (make-bignum 10) 0)
  division-by-zero)

(deftest mod-bignum.4
  (mod (make-bignum 77) (make-bignum 12))
  5)

(deftest mod-bignum.5
  (mod (make-bignum 77) 3/4)
  1/2)

(deftest-single mod-bignum.6
  (mod (make-bignum 77) 23.3f0)
  7.1 0.0 1e-4)

(deftest-double mod-bignum.7
  (mod (make-bignum 77) 1.23d0)
  0.74d0)

(deftest-long mod-bignum.8
  (mod (make-bignum 77) 3.21L0)
  3.17L0)

(deftest mod-ratio.1
  (mod 97/4 11/7)
  19/28)

(deftest mod-ratio.2
  (mod 97/4 11/7)
  19/28)

(deftest mod-ratio.3
  (mod -97/4 11/7)
  25/28)

(deftest mod-ratio.4
  (mod 97/4 -11/7)
  -25/28)

(deftest mod-ratio.5
  (mod -97/4 -11/7)
  -19/28)

(deftest mod-ratio.6
  (mod (make-ratio 0 1) 11/7)
  0)

(deftest-error mod-ratio.7
  (mod 10/11 0)
  division-by-zero)

(deftest mod-ratio.8
  (mod -123/7 5)
  17/7)

(deftest mod-ratio.9
  (mod 123/7 (make-bignum 5))
  18/7)

(deftest-single mod-ratio.10
  (mod 123/7 12.3f0)
  5.2714276)

(deftest-double mod-ratio.11
  (mod 123/7 98.7d0)
  17.571428571428573d0)

(deftest-long mod-ratio.12
  (mod 123/7 -98.7L0)
  -81.128571428571428566L0)

(deftest-single mod-single.1
  (mod 12.3f0 5)
  2.3000002)

(deftest-single mod-single.2
  (mod -12.3f0 (make-bignum 5))
  2.6999998)

(deftest-single mod-single.3
  (mod -12.3f0 7/13)
  0.084616296)

(deftest-single mod-single.4
  (mod 12.3f0 5.32f0)
  1.6600001)

(deftest-double mod-single.5
  (mod 12.3f0 -3.32d0)
  -0.9799998092651356d0)

(deftest-long mod-single.6
  (mod 12.3f0 3.32L0)
  2.340000190734864d0)

(deftest-double mod-double.1
  (mod 12.3d0 5)
  2.3000000000000007d0)

(deftest-double mod-double.2
  (mod -12.3d0 (make-bignum 5))
  2.6999999999999993d0)

(deftest-double mod-double.3
  (mod -12.3d0 7/13)
  0.08461538461538265d0)

(deftest-double mod-double.4
  (mod 12.3d0 5.32f0)
  1.6599996566772468d0)

(deftest-double mod-double.5
  (mod 12.3d0 -3.32d0)
  -0.9799999999999982d0)

(deftest-long mod-double.6
  (mod 12.3d0 3.32L0)
  2.3400000000000016d0)

(deftest-long mod-long.1
  (mod 12.3L0 5)
  2.3000000000000007L0)

(deftest-long mod-long.2
  (mod -12.3L0 (make-bignum 5))
  2.6999999999999993L0)

(deftest-long mod-long.3
  (mod -12.3L0 7/13)
  0.08461538461538265L0)

(deftest-long mod-long.4
  (mod 12.3L0 5.32f0)
  1.6599996566772468L0)

(deftest-long mod-long.5
  (mod 12.3L0 -3.32L0)
  -0.9799999999999982L0)

(deftest-long mod-long.6
  (mod 12.3L0 3.32L0)
  2.3400000000000016L0)

(deftest-error mod-division-by-zero.1
  (mod 10 0)
  division-by-zero)

(deftest-error mod-division-by-zero.2
  (mod 10.0 0.0)
  division-by-zero)

(deftest-error mod-division-by-zero.3
  (mod 10.0d0 0.0d0)
  division-by-zero)

(deftest-error! mod-error.1
  (eval '(mod 10)))

(deftest-error! mod-error.2
  (eval '(mod #c(10 20) 20))
  type-error)

(deftest-error! mod-error.3
  (eval '(mod 10.0 #c(10.0 20.0)))
  type-error)

(deftest-error! mod-error.4
  (eval '(mod 10 20 30)))


;;
;;  Function REM
;;
(deftest rem.1
  (rem -1 5)
  -1)

(deftest rem.2
  (rem 13 4)
  1)

(deftest rem.3
  (rem -13 4)
  -1)

(deftest rem.4
  (rem 13 -4)
  1)

(deftest rem.5
  (rem -13 -4)
  -1)

(deftest-single rem.6
  (rem 13.4 1)
  0.4)

(deftest-single rem.7
  (rem -13.4 1)
  -0.4)

(deftest rem-fixnum.1
  (rem 77 12)
  5)

(deftest rem-fixnum.2
  (rem -77 12)
  -5)

(deftest rem-fixnum.3
  (rem 77 -12)
  5)

(deftest rem-fixnum.4
  (rem -77 -12)
  -5)

(deftest-error rem-fixnum.5
  (rem 10 0)
  division-by-zero)

(deftest rem-fixnum.6
  (rem 0 100)
  0)

(deftest rem-fixnum.7
  (rem 3 100)
  3)

(deftest rem-fixnum.8
  (rem 77 (make-bignum 12))
  5)

(deftest rem-fixnum.9
  (rem 77 3/4)
  1/2)

(deftest-single rem-fixnum.10
  (rem 77 23.3f0)
  7.1 0.0 1e-4)

(deftest-double rem-fixnum.11
  (rem 77 -1.23d0)
  0.74d0)

(deftest-long rem-fixnum.12
  (rem -77 3.21L0)
  -3.17L0)

(deftest rem-bignum.1
  (rem (make-bignum 77) 12)
  5)

(deftest rem-bignum.2
  (rem (make-bignum 77) -12)
  5)

(deftest-error rem-bignum.3
  (rem (make-bignum 10) 0)
  division-by-zero)

(deftest rem-bignum.4
  (rem (make-bignum 77) (make-bignum 12))
  5)

(deftest rem-bignum.5
  (rem (make-bignum 77) 3/4)
  1/2)

(deftest-single rem-bignum.6
  (rem (make-bignum 77) 23.3f0)
  7.1 0.0 1e-4)

(deftest-double rem-bignum.7
  (rem (make-bignum -77) 1.23d0)
  -0.74d0)

(deftest-long rem-bignum.8
  (rem (make-bignum 77) -3.21L0)
  3.17L0)

(deftest rem-ratio.1
  (rem 97/4 11/7)
  19/28)

(deftest rem-ratio.2
  (rem 97/4 11/7)
  19/28)

(deftest rem-ratio.3
  (rem -97/4 11/7)
  -19/28)

(deftest rem-ratio.4
  (rem 97/4 -11/7)
  19/28)

(deftest rem-ratio.5
  (rem -97/4 -11/7)
  -19/28)

(deftest rem-ratio.6
  (rem (make-ratio 0 1) 11/7)
  0)

(deftest-error rem-ratio.7
  (rem 10/11 0)
  division-by-zero)

(deftest rem-ratio.8
  (rem -123/7 5)
  -18/7)

(deftest rem-ratio.9
  (rem 123/7 (make-bignum 5))
  18/7)

(deftest-single rem-ratio.10
  (rem 123/7 12.3f0)
  5.2714276)

(deftest-double rem-ratio.11
  (rem -123/7 98.7d0)
  -17.571428571428573d0)

(deftest-long rem-ratio.12
  (rem 123/7 -98.7L0)
  17.571428571428573d0)

(deftest-single rem-single.1
  (rem 12.3f0 5)
  2.3000002)

(deftest-single rem-single.2
  (rem -12.3f0 (make-bignum 5))
  -2.3000002)

(deftest-single rem-single.3
  (rem -12.3f0 7/13)
  -0.45384598)

(deftest-single rem-single.4
  (rem 12.3f0 5.32f0)
  1.6600001)

(deftest-double rem-single.5
  (rem 12.3f0 -3.32d0)
  2.340000190734864d0)

(deftest-long rem-single.6
  (rem -12.3f0 3.32L0)
  -2.340000190734864d0)

(deftest-double rem-double.1
  (rem 12.3d0 5)
  2.3000000000000007d0)

(deftest-double rem-double.2
  (rem -12.3d0 (make-bignum 5))
  -2.3000000000000007d0)

(deftest-double rem-double.3
  (rem -12.3d0 7/13)
  -0.4538461538461558d0)

(deftest-double rem-double.4
  (rem 12.3d0 5.32f0)
  1.6599996566772468d0)

(deftest-double rem-double.5
  (rem 12.3d0 -3.32d0)
  2.3400000000000016d0)

(deftest-long rem-double.6
  (rem -12.3d0 3.32L0)
  -2.3400000000000016d0)

(deftest-long rem-long.1
  (rem 12.3L0 5)
  2.3000000000000007L0)

(deftest-long rem-long.2
  (rem -12.3L0 (make-bignum 5))
  -2.3000000000000007L0)

(deftest-long rem-long.3
  (rem -12.3L0 7/13)
  -0.4538461538461558d0)

(deftest-long rem-long.4
  (rem 12.3L0 5.32f0)
  1.6599996566772468L0)

(deftest-long rem-long.5
  (rem 12.3L0 -3.32L0)
  2.3400000000000016L0)

(deftest-long rem-long.6
  (rem -12.3L0 3.32L0)
  -2.3400000000000016L0)

(deftest-error rem-division-by-zero.1
  (rem 10 0)
  division-by-zero)

(deftest-error rem-division-by-zero.2
  (rem 10.0 0.0)
  division-by-zero)

(deftest-error rem-division-by-zero.3
  (rem 10.0d0 0.0d0)
  division-by-zero)

(deftest-error! rem-error.1
  (eval '(rem 10)))

(deftest-error! rem-error.2
  (eval '(rem #c(10 20) 20))
  type-error)

(deftest-error! rem-error.3
  (eval '(rem 10.0 #c(10.0 20.0)))
  type-error)

(deftest-error! rem-error.4
  (eval '(rem 10 20 30)))

;;  ANSI Common Lisp
(deftest mod-test.1
  (rem -1 5)
  -1)

(deftest mod-test.2
  (mod -1 5)
  4)

(deftest mod-test.3
  (mod 13 4)
  1)

(deftest mod-test.4
  (rem 13 4)
  1)

(deftest mod-test.5
  (mod -13 4)
  3)

(deftest mod-test.6
  (rem -13 4)
  -1)

(deftest mod-test.7
  (mod 13 -4)
  -3)

(deftest mod-test.8
  (rem 13 -4)
  1)

(deftest mod-test.9
  (mod -13 -4)
  -1)

(deftest mod-test.10
  (rem -13 -4)
  -1)

(deftest-single mod-test.11
  (mod 13.4 1)
  0.4)

(deftest-single mod-test.12
  (rem 13.4 1)
  0.4)

(deftest-single mod-test.13
  (mod -13.4 1)
  0.6)

(deftest-single mod-test.14
  (rem -13.4 1)
  -0.4)

