;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;  mod
(deftest mod.1
  (values
    (mod -1 5)
    (mod 13 4)
    (mod -13 4)
    (mod 13 -4)
    (mod -13 -4))
  4 1 3 -3 -1)

(deftest-single mod.2
  (mod 13.4 1)
  0.4)

(deftest-single mod.3
  (mod -13.4 1)
  0.6)

(deftest modf.1
  (mod 77 12)
  5)

(deftest modf.2
  (mod -77 12)
  7)

(deftest modf.3
  (mod 77 -12)
  -7)

(deftest modf.4
  (mod -77 -12)
  -5)

(deftest-error modf.5
  (mod 10 0)
  division-by-zero)

(deftest modf.6
  (mod 0 100)
  0)

(deftest modf.7
  (mod 3 100)
  3)

(deftest modf.8
  (mod 77 (make-bignum 12))
  5)

(deftest modf.9
  (mod 77 3/4)
  1/2)

(deftest-single modf.10
  (mod 77 23.3f0)
  7.1 0.0 1e-4)

(deftest-double modf.11
  (mod 77 1.23d0)
  0.74d0)

(deftest-long modf.12
  (mod 77 3.21L0)
  3.17L0)

(deftest modb.1
  (mod (make-bignum 77) 12)
  5)

(deftest modb.2
  (mod (make-bignum 77) -12)
  -7)

(deftest-error modb.3
  (mod (make-bignum 10) 0)
  division-by-zero)

(deftest modb.4
  (mod (make-bignum 77) (make-bignum 12))
  5)

(deftest modb.5
  (mod (make-bignum 77) 3/4)
  1/2)

(deftest-single modb.6
  (mod (make-bignum 77) 23.3f0)
  7.1 0.0 1e-4)

(deftest-double modb.7
  (mod (make-bignum 77) 1.23d0)
  0.74d0)

(deftest-long modb.8
  (mod (make-bignum 77) 3.21L0)
  3.17L0)

(deftest modr.1
  (mod 97/4 11/7)
  19/28)

(deftest modr.2
  (mod 97/4 11/7)
  19/28)

(deftest modr.3
  (mod -97/4 11/7)
  25/28)

(deftest modr.4
  (mod 97/4 -11/7)
  -25/28)

(deftest modr.5
  (mod -97/4 -11/7)
  -19/28)

(deftest modr.6
  (mod (make-ratio 0 1) 11/7)
  0)

(deftest-error modr.7
  (mod 10/11 0)
  arithmetic-error)

(deftest modr.8
  (mod -123/7 5)
  17/7)

(deftest modr.9
  (mod 123/7 (make-bignum 5))
  18/7)

(deftest-single modr.10
  (mod 123/7 12.3f0)
  5.2714276)

(deftest-double modr.11
  (mod 123/7 98.7d0)
  17.571428571428573d0)

(deftest-long modr.12
  (mod 123/7 -98.7L0)
  -81.128571428571428566L0)

(deftest-single mods.1
  (mod 12.3f0 5)
  2.3000002)

(deftest-single mods.2
  (mod -12.3f0 (make-bignum 5))
  2.6999998)

(deftest-single mods.3
  (mod -12.3f0 7/13)
  0.084616296)

(deftest-single mods.4
  (mod 12.3f0 5.32f0)
  1.6600001)

(deftest-double mods.5
  (mod 12.3f0 -3.32d0)
  -0.9799998092651356d0)

(deftest-long mods.6
  (mod 12.3f0 3.32L0)
  2.340000190734864d0)

(deftest-double modd.1
  (mod 12.3d0 5)
  2.3000000000000007d0)

(deftest-double modd.2
  (mod -12.3d0 (make-bignum 5))
  2.6999999999999993d0)

(deftest-double modd.3
  (mod -12.3d0 7/13)
  0.08461538461538265d0)

(deftest-double modd.4
  (mod 12.3d0 5.32f0)
  1.6599996566772468d0)

(deftest-double modd.5
  (mod 12.3d0 -3.32d0)
  -0.9799999999999982d0)

(deftest-long modd.6
  (mod 12.3d0 3.32L0)
  2.3400000000000016d0)

(deftest-long modl.1
  (mod 12.3L0 5)
  2.3000000000000007L0)

(deftest-long modl.2
  (mod -12.3L0 (make-bignum 5))
  2.6999999999999993L0)

(deftest-long modl.3
  (mod -12.3L0 7/13)
  0.08461538461538265L0)

(deftest-long modl.4
  (mod 12.3L0 5.32f0)
  1.6599996566772468L0)

(deftest-long modl.5
  (mod 12.3L0 -3.32L0)
  -0.9799999999999982L0)

(deftest-long modl.6
  (mod 12.3L0 3.32L0)
  2.3400000000000016L0)

;;  rem
(deftest rem.1
  (values
    (rem -1 5)
    (rem 13 4)
    (rem -13 4)
    (rem 13 -4)
    (rem -13 -4))
  -1 1 -1 1 -1)

(deftest-single rem.2
  (rem 13.4 1)
  0.4)

(deftest-single rem.3
  (rem -13.4 1)
  -0.4)

(deftest remf.1
  (rem 77 12)
  5)

(deftest remf.2
  (rem -77 12)
  -5)

(deftest remf.3
  (rem 77 -12)
  5)

(deftest remf.4
  (rem -77 -12)
  -5)

(deftest-error remf.5
  (rem 10 0)
  division-by-zero)

(deftest remf.6
  (rem 0 100)
  0)

(deftest remf.7
  (rem 3 100)
  3)

(deftest remf.8
  (rem 77 (make-bignum 12))
  5)

(deftest remf.9
  (rem 77 3/4)
  1/2)

(deftest-single remf.10
  (rem 77 23.3f0)
  7.1 0.0 1e-4)

(deftest-double remf.11
  (rem 77 -1.23d0)
  0.74d0)

(deftest-long remf.12
  (rem -77 3.21L0)
  -3.17L0)

(deftest remb.1
  (rem (make-bignum 77) 12)
  5)

(deftest remb.2
  (rem (make-bignum 77) -12)
  5)

(deftest-error remb.3
  (rem (make-bignum 10) 0)
  division-by-zero)

(deftest remb.4
  (rem (make-bignum 77) (make-bignum 12))
  5)

(deftest remb.5
  (rem (make-bignum 77) 3/4)
  1/2)

(deftest-single remb.6
  (rem (make-bignum 77) 23.3f0)
  7.1 0.0 1e-4)

(deftest-double remb.7
  (rem (make-bignum -77) 1.23d0)
  -0.74d0)

(deftest-long remb.8
  (rem (make-bignum 77) -3.21L0)
  3.17L0)

(deftest remr.1
  (rem 97/4 11/7)
  19/28)

(deftest remr.2
  (rem 97/4 11/7)
  19/28)

(deftest remr.3
  (rem -97/4 11/7)
  -19/28)

(deftest remr.4
  (rem 97/4 -11/7)
  19/28)

(deftest remr.5
  (rem -97/4 -11/7)
  -19/28)

(deftest remr.6
  (rem (make-ratio 0 1) 11/7)
  0)

(deftest-error remr.7
  (rem 10/11 0)
  arithmetic-error)

(deftest remr.8
  (rem -123/7 5)
  -18/7)

(deftest remr.9
  (rem 123/7 (make-bignum 5))
  18/7)

(deftest-single remr.10
  (rem 123/7 12.3f0)
  5.2714276)

(deftest-double remr.11
  (rem -123/7 98.7d0)
  -17.571428571428573d0)

(deftest-long remr.12
  (rem 123/7 -98.7L0)
  17.571428571428573d0)

(deftest-single rems.1
  (rem 12.3f0 5)
  2.3000002)

(deftest-single rems.2
  (rem -12.3f0 (make-bignum 5))
  -2.3000002)

(deftest-single rems.3
  (rem -12.3f0 7/13)
  -0.45384598)

(deftest-single rems.4
  (rem 12.3f0 5.32f0)
  1.6600001)

(deftest-double rems.5
  (rem 12.3f0 -3.32d0)
  2.340000190734864d0)

(deftest-long rems.6
  (rem -12.3f0 3.32L0)
  -2.340000190734864d0)

(deftest-double remd.1
  (rem 12.3d0 5)
  2.3000000000000007d0)

(deftest-double remd.2
  (rem -12.3d0 (make-bignum 5))
  -2.3000000000000007d0)

(deftest-double remd.3
  (rem -12.3d0 7/13)
  -0.4538461538461558d0)

(deftest-double remd.4
  (rem 12.3d0 5.32f0)
  1.6599996566772468d0)

(deftest-double remd.5
  (rem 12.3d0 -3.32d0)
  2.3400000000000016d0)

(deftest-long remd.6
  (rem -12.3d0 3.32L0)
  -2.3400000000000016d0)

(deftest-long reml.1
  (rem 12.3L0 5)
  2.3000000000000007L0)

(deftest-long reml.2
  (rem -12.3L0 (make-bignum 5))
  -2.3000000000000007L0)

(deftest-long reml.3
  (rem -12.3L0 7/13)
  -0.4538461538461558d0)

(deftest-long reml.4
  (rem 12.3L0 5.32f0)
  1.6599996566772468L0)

(deftest-long reml.5
  (rem 12.3L0 -3.32L0)
  2.3400000000000016L0)

(deftest-long reml.6
  (rem -12.3L0 3.32L0)
  -2.3400000000000016L0)

