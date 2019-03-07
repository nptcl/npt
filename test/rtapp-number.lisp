;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(deftest max.1
  (max 10)
  10)

(deftest max.2
  (max 7 2 3 9 1 2 999 3 8 9)
  999)

(deftest min.1
  (min 10)
  10)

(deftest min.2
  (min 7 2 3 9 1 2 999 3 8 9)
  1)

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

(deftest \+.1
  (+)
  0)

(deftest \+.2
  (+ 10)
  10)

(deftest \+.3
  (+ 10 20)
  30)

(deftest \+.4
  (+ 10 20 30 40)
  100)

(deftest \-.1
  (- 10)
  -10)

(deftest \-.2
  (- -20)
  20)

(deftest \-.3
  (- 10 4)
  6)

(deftest \-.4
  (- 10 1 2 3)
  4)

(deftest 1+.1
  (1+ 10)
  11)

(deftest 1+.2
  (1+ -1)
  0)

(deftest 1+.3
  (1+ 13.4)
  14.4)

(deftest 1-.1
  (1- 10)
  9)

(deftest 1-.2
  (1- 1)
  0)

(deftest 1-.3
  (1- 13.4)
  12.4)

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

(deftest numberp.1
  (numberp 10)
  t)

(deftest numberp.2
  (numberp 99999999999999999999999999999999)
  t)

(deftest numberp.3
  (numberp -3/4)
  t)

(deftest numberp.4
  (numberp 12.34)
  t)

(deftest numberp.5
  (numberp #c(10 20))
  t)

(deftest numberp.6
  (numberp "Hello")
  nil)

(deftest complexp.1
  (complexp #c(10 20))
  t)

(deftest complexp.2
  (complexp #c(10 0))
  nil)

(deftest complexp.3
  (complexp #c(12.3 45))
  t)

(deftest complexp.4
  (complexp #c(12.3 0.0))
  nil)

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


;;
;;  constant
;;
(deftest pi.1
  (floatp pi)
  t)

(deftest pi.2
  (< 3.0 pi 4.0)
  t)

(deftest boole-1.1
  (integerp boole-1)
  t)

(deftest boole-2.1
  (integerp boole-2)
  t)

(deftest boole-and.1
  (integerp boole-and)
  t)

(deftest boole-andc1.1
  (integerp boole-andc1)
  t)

(deftest boole-andc2.1
  (integerp boole-andc2)
  t)

(deftest boole-c1.1
  (integerp boole-c1)
  t)

(deftest boole-c2.1
  (integerp boole-c2)
  t)

(deftest boole-clr.1
  (integerp boole-clr)
  t)

(deftest boole-eqv.1
  (integerp boole-eqv)
  t)

(deftest boole-ior.1
  (integerp boole-ior)
  t)

(deftest boole-nand.1
  (integerp boole-nand)
  t)

(deftest boole-nor.1
  (integerp boole-nor)
  t)

(deftest boole-orc1.1
  (integerp boole-orc1)
  t)

(deftest boole-orc2.1
  (integerp boole-orc2)
  t)

(deftest boole-set.1
  (integerp boole-set)
  t)

(deftest boole-xor.1
  (integerp boole-xor)
  t)

(deftest most-positive-fixnum.1
  (values
    (typep most-positive-fixnum 'fixnum)
    (= most-positive-fixnum #+64-bit #x7FFFFFFFFFFFFFFF #-64-bit #x7FFFFFFF))
  t t)

(deftest most-negative-fixnum.1
  (values
    (typep most-negative-fixnum 'fixnum)
    (= most-negative-fixnum #+64-bit #x-8000000000000000 #-64-bit #x-80000000))
  t t)

(deftest most-positive-short-float.1
  (values
    (typep most-positive-short-float 'single-float)
    (plusp most-positive-short-float)
    (zerop most-positive-short-float)
    (< 10s0 most-positive-short-float))
  t t nil t)

(deftest most-positive-single-float.1
  (values
    (typep most-positive-single-float 'single-float)
    (plusp most-positive-single-float)
    (zerop most-positive-single-float)
    (< 10f0 most-positive-single-float))
  t t nil t)

(deftest most-positive-double-float.1
  (values
    (typep most-positive-double-float 'double-float)
    (plusp most-positive-double-float)
    (zerop most-positive-double-float)
    (< 10d0 most-positive-double-float))
  t t nil t)

(deftest most-positive-long-float.1
  (values
    (typep most-positive-long-float 'long-float)
    (plusp most-positive-long-float)
    (zerop most-positive-long-float)
    (< 10l0 most-positive-long-float))
  t t nil t)

(deftest most-negative-short-float.1
  (values
    (typep most-negative-short-float 'single-float)
    (minusp most-negative-short-float)
    (zerop most-negative-short-float)
    (> 10s0 most-negative-short-float))
  t t nil t)

(deftest most-negative-single-float.1
  (values
    (typep most-negative-single-float 'single-float)
    (minusp most-negative-single-float)
    (zerop most-negative-single-float)
    (> 10f0 most-negative-single-float))
  t t nil t)

(deftest most-negative-double-float.1
  (values
    (typep most-negative-double-float 'double-float)
    (minusp most-negative-double-float)
    (zerop most-negative-double-float)
    (> 10d0 most-negative-double-float))
  t t nil t)

(deftest most-negative-long-float.1
  (values
    (typep most-negative-long-float 'long-float)
    (minusp most-negative-long-float)
    (zerop most-negative-long-float)
    (> 10l0 most-negative-long-float))
  t t nil t)

(deftest least-positive-short-float.1
  (values
    (typep least-positive-short-float 'single-float)
    (plusp least-positive-short-float)
    (zerop least-positive-short-float))
  t t nil)

(deftest least-positive-normalized-short-float.1
  (values
    (typep least-positive-normalized-short-float 'single-float)
    (plusp least-positive-normalized-short-float)
    (zerop least-positive-normalized-short-float)
    (<= least-positive-short-float least-positive-normalized-short-float))
  t t nil t)

(deftest least-positive-single-float.1
  (values
    (typep least-positive-single-float 'single-float)
    (plusp least-positive-single-float)
    (zerop least-positive-single-float))
  t t nil)

(deftest least-positive-normalized-single-float.1
  (values
    (typep least-positive-normalized-single-float 'single-float)
    (plusp least-positive-normalized-single-float)
    (zerop least-positive-normalized-single-float)
    (<= least-positive-single-float least-positive-normalized-single-float))
  t t nil t)

(deftest least-positive-double-float.1
  (values
    (typep least-positive-double-float 'double-float)
    (plusp least-positive-double-float)
    (zerop least-positive-double-float))
  t t nil)

(deftest least-positive-normalized-double-float.1
  (values
    (typep least-positive-normalized-double-float 'double-float)
    (plusp least-positive-normalized-double-float)
    (zerop least-positive-normalized-double-float)
    (<= least-positive-double-float least-positive-normalized-double-float))
  t t nil t)

(deftest least-positive-long-float.1
  (values
    (typep least-positive-long-float 'long-float)
    (plusp least-positive-long-float)
    (zerop least-positive-long-float))
  t t nil)

(deftest least-positive-normalized-long-float.1
  (values
    (typep least-positive-normalized-long-float 'long-float)
    (plusp least-positive-normalized-long-float)
    (zerop least-positive-normalized-long-float)
    (<= least-positive-long-float least-positive-normalized-long-float))
  t t nil t)

(deftest least-negative-short-float.1
  (values
    (typep least-negative-short-float 'single-float)
    (minusp least-negative-short-float)
    (zerop least-negative-short-float))
  t t nil)

(deftest least-negative-normalized-short-float.1
  (values
    (typep least-negative-normalized-short-float 'single-float)
    (minusp least-negative-normalized-short-float)
    (zerop least-negative-normalized-short-float)
    (>= least-negative-short-float least-negative-normalized-short-float))
  t t nil t)

(deftest least-negative-single-float.1
  (values
    (typep least-negative-single-float 'single-float)
    (minusp least-negative-single-float)
    (zerop least-negative-single-float))
  t t nil)

(deftest least-negative-normalized-single-float.1
  (values
    (typep least-negative-normalized-single-float 'single-float)
    (minusp least-negative-normalized-single-float)
    (zerop least-negative-normalized-single-float)
    (>= least-negative-single-float least-negative-normalized-single-float))
  t t nil t)

(deftest least-negative-double-float.1
  (values
    (typep least-negative-double-float 'double-float)
    (minusp least-negative-double-float)
    (zerop least-negative-double-float))
  t t nil)

(deftest least-negative-normalized-double-float.1
  (values
    (typep least-negative-normalized-double-float 'double-float)
    (minusp least-negative-normalized-double-float)
    (zerop least-negative-normalized-double-float)
    (>= least-negative-double-float least-negative-normalized-double-float))
  t t nil t)

(deftest least-negative-long-float.1
  (values
    (typep least-negative-long-float 'long-float)
    (minusp least-negative-long-float)
    (zerop least-negative-long-float))
  t t nil)

(deftest least-negative-normalized-long-float.1
  (values
    (typep least-negative-normalized-long-float 'long-float)
    (minusp least-negative-normalized-long-float)
    (zerop least-negative-normalized-long-float)
    (>= least-negative-long-float least-negative-normalized-long-float))
  t t nil t)

(deftest short-float-epsilon.1
  (values
    (typep short-float-epsilon 'single-float)
    (plusp short-float-epsilon)
    (zerop short-float-epsilon)
    (let ((v short-float-epsilon))
      (= (float 1 v) (+ (float 1 v) v))))
  t t nil t)

(deftest short-float-negative-epsilon.1
  (values
    (typep short-float-negative-epsilon 'single-float)
    (plusp short-float-negative-epsilon)
    (zerop short-float-negative-epsilon)
    (let ((v short-float-negative-epsilon))
      (= (float 1 v) (- (float 1 v) v))))
  t t nil t)

(deftest single-float-epsilon.1
  (values
    (typep single-float-epsilon 'single-float)
    (plusp single-float-epsilon)
    (zerop single-float-epsilon)
    (let ((v single-float-epsilon))
      (= (float 1 v) (+ (float 1 v) v))))
  t t nil t)

(deftest single-float-negative-epsilon.1
  (values
    (typep single-float-negative-epsilon 'single-float)
    (plusp single-float-negative-epsilon)
    (zerop single-float-negative-epsilon)
    (let ((v single-float-negative-epsilon))
      (= (float 1 v) (- (float 1 v) v))))
  t t nil t)

(deftest double-float-epsilon.1
  (values
    (typep double-float-epsilon 'double-float)
    (plusp double-float-epsilon)
    (zerop double-float-epsilon)
    (let ((v double-float-epsilon))
      (= (float 1 v) (+ (float 1 v) v))))
  t t nil t)

(deftest double-float-negative-epsilon.1
  (values
    (typep double-float-negative-epsilon 'double-float)
    (plusp double-float-negative-epsilon)
    (zerop double-float-negative-epsilon)
    (let ((v double-float-negative-epsilon))
      (= (float 1 v) (- (float 1 v) v))))
  t t nil t)

(deftest long-float-epsilon.1
  (values
    (typep long-float-epsilon 'long-float)
    (plusp long-float-epsilon)
    (zerop long-float-epsilon)
    (let ((v long-float-epsilon))
      (= (float 1 v) (+ (float 1 v) v))))
  t t nil t)

(deftest long-float-negative-epsilon.1
  (values
    (typep long-float-negative-epsilon 'long-float)
    (plusp long-float-negative-epsilon)
    (zerop long-float-negative-epsilon)
    (let ((v long-float-negative-epsilon))
      (= (float 1 v) (- (float 1 v) v))))
  t t nil t)

