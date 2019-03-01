;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(deftest =.1
  (= 10)
  t)

(deftest =.2
  (= 10.1 10.1)
  t)

(deftest =.3
  (= 12 34)
  nil)

(deftest =.4
  (= 1 1 1)
  t)

(deftest =.5
  (= 1 2 2)
  nil)

(deftest =.6
  (= 1 1 10)
  nil)

(deftest =.7
  (= #c(10 20) #c(10 20))
  t)

(deftest =.8
  (= #c(10 20) #c(10 21.2))
  nil)

(deftest /=.1
  (/= 10)
  t)

(deftest /=.2
  (/= 10 20)
  t)

(deftest /=.3
  (/= 10 10)
  nil)

(deftest /=.4
  (/= 10 20 30)
  t)

(deftest /=.5
  (/= 10 20 10)
  nil)

(deftest /=.6
  (/= 10 10 20)
  nil)

(deftest /=.7
  (/= 10 10 10)
  nil)

(deftest <.1
  (< 10)
  t)

(deftest <.2
  (< 10 20)
  t)

(deftest <.3
  (< 20 10)
  nil)

(deftest <.4
  (< 10 10)
  nil)

(deftest <.5
  (< 10 20 30)
  t)

(deftest <.6
  (< 30 20 10)
  nil)

(deftest <.7
  (< 10 20 15)
  nil)

(deftest <.8
  (< 20 10 100)
  nil)

(deftest >.1
  (> 10)
  t)

(deftest >.2
  (> 20 10)
  t)

(deftest >.3
  (> 10 20)
  nil)

(deftest >.4
  (> 10 10)
  nil)

(deftest >.5
  (> 30 20 10)
  t)

(deftest >.6
  (> 10 20 30)
  nil)

(deftest >.7
  (> 30 20 25)
  nil)

(deftest >.8
  (> 30 40 10)
  nil)

(deftest <=.1
  (<= 10)
  t)

(deftest <=.2
  (<= 10 20)
  t)

(deftest <=.3
  (<= 20 10)
  nil)

(deftest <=.4
  (<= 10 10)
  t)

(deftest <=.5
  (<= 10 20 30)
  t)

(deftest <=.6
  (<= 30 20 10)
  nil)

(deftest <=.7
  (<= 10 20 15)
  nil)

(deftest <=.8
  (<= 20 10 100)
  nil)

(deftest >=.1
  (>= 10)
  t)

(deftest >=.2
  (>= 20 10)
  t)

(deftest >=.3
  (>= 10 20)
  nil)

(deftest >=.4
  (>= 10 10)
  t)

(deftest >=.5
  (>= 30 20 10)
  t)

(deftest >=.6
  (>= 10 20 30)
  nil)

(deftest >=.7
  (>= 30 20 25)
  nil)

(deftest >=.8
  (>= 30 40 10)
  nil)

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
  (minusp -10)
  t)

(deftest minusp.2
  (minusp -10.2)
  t)

(deftest minusp.3
  (minusp 10)
  nil)

(deftest minusp.4
  (minusp 0)
  nil)

(deftest minusp.5
  (minusp 0.0d0)
  nil)

(deftest plusp.1
  (plusp 10)
  t)

(deftest plusp.2
  (plusp 10.2)
  t)

(deftest plusp.3
  (plusp -10)
  nil)

(deftest plusp.4
  (plusp 0)
  nil)

(deftest plusp.5
  (plusp 0.0d0)
  nil)

(deftest zerop.1
  (zerop 0)
  t)

(deftest zerop.2
  (zerop 0.0d0)
  t)

(deftest zerop.3
  (zerop 10)
  nil)

(deftest zerop.4
  (zerop -20.2)
  nil)

(deftest zerop.5
  (zerop #c(10 20))
  nil)

(deftest zerop.6
  (zerop #c(0 0))
  t)

(deftest zerop.7
  (zerop #c(0 0.0d0))
  t)

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

