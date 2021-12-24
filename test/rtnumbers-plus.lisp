;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Function 1+
;;
(deftest 1+.1
  (1+ 10)
  11)

(deftest 1+.2
  (1+ -1)
  0)

(deftest 1+.3
  (1+ -12)
  -11)

(deftest 1+.4
  (1+ 5/4)
  9/4)

(deftest 1+.5
  (1+ 13.4f0)
  14.4f0)

(deftest 1+.6
  (1+ -22.5d0)
  -21.5d0)

(deftest 1+.7
  (1+ 100L0)
  101L0)

(deftest 1+.8
  (1+ #c(2 3))
  #c(3 3))

(deftest 1+.9
  (1+ #c(-4.5d0 -6.25d0))
  #c(-3.5d0 -6.25d0))

(deftest 1+.10
  (values
    (fixnump most-positive-fixnum)
    (bignump (1+ most-positive-fixnum)))
  t t)

(deftest-error! 1+-error.1
  (eval '(1+)))

(deftest-error! 1+-error.2
  (eval '(1+ 'aaa))
  type-error)

(deftest-error! 1+-error.3
  (eval '(1+ 10 20)))

;;  ANSI Common Lisp
(deftest 1+-test.1
  (1+ 99)
  100)

(deftest 1+-test.2
  (1+ (complex 0.0))
  #c(1.0 0.0))


;;
;;  Function 1-
;;
(deftest 1-.1
  (1- 10)
  9)

(deftest 1-.2
  (1- 1)
  0)

(deftest 1-.3
  (1- -12)
  -13)

(deftest 1-.4
  (1- 5/4)
  1/4)

(deftest 1-.5
  (1- 13.4f0)
  12.4f0)

(deftest 1-.6
  (1- -22.5d0)
  -23.5d0)

(deftest 1-.7
  (1- 100L0)
  99L0)

(deftest 1-.8
  (1- #c(2 3))
  #c(1 3))

(deftest 1-.9
  (1- #c(-4.5d0 -6.25d0))
  #c(-5.5d0 -6.25d0))

(deftest 1-.10
  (values
    (fixnump most-negative-fixnum)
    (bignump (1- most-negative-fixnum)))
  t t)

(deftest-error! 1--error.1
  (eval '(1-)))

(deftest-error! 1--error.2
  (eval '(1- 'aaa))
  type-error)

(deftest-error! 1--error.3
  (eval '(1- 10 20)))

;;  ANSI Common Lisp
(deftest 1--test.1
  (1- 100)
  99)

(deftest 1--test.2
  (1- 5/3)
  2/3)


;;
;;  Function +
;;
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

(deftest \+f.1
  (+ -20 30)
  10)

(deftest \+f.2
  (+ 20 (make-bignum -100))
  -80)

(deftest \+f.3
  (+ 111 3/4)
  447/4)

(deftest-single \+f.4
  (+ 4 -0.4f0)
  3.6f0)

(deftest-double \+f.5
  (+ -4 0.4d0)
  -3.6d0)

(deftest-long \+f.6
  (+ -4 100L0)
  96.0L0)

(deftest \+f.7
  (+ 10 #c(33 44))
  #c(43 44))

(deftest-double \+f.8
  (+ 10 #c(-12.3d0 4.5d-2))
  -2.3d0 4.5d-2)

(deftest \+b.1
  (+ (make-bignum -20) 30)
  10)

(deftest \+b.2
  (+ (make-bignum 20) (make-bignum -100))
  -80)

(deftest \+b.3
  (+ (make-bignum 111) 3/4)
  447/4)

(deftest-single \+b.4
  (+ (make-bignum 4) -0.4f0)
  3.6f0)

(deftest-double \+b.5
  (+ (make-bignum -4) 0.4d0)
  -3.6d0)

(deftest-long \+b.6
  (+ (make-bignum -4) 100L0)
  96.0L0)

(deftest \+b.7
  (+ (make-bignum 10) #c(33 44))
  #c(43 44))

(deftest-double \+b.8
  (+ (make-bignum 10) #c(-12.3d0 4.5d-2))
  -2.3d0 4.5d-2)

(deftest \+r.1
  (+ 41/189 30)
  5711/189)

(deftest \+r.2
  (+ 41/189 (make-bignum -30))
  -5629/189)

(deftest \+r.3
  (+ 41/189 3/4)
  731/756)

(deftest-single \+r.4
  (+ 41/189 -0.4f0)
  -0.18306878f0)

(deftest-double \+r.5
  (+ -41/189 0.4d0)
  0.1830687830687831d0)

(deftest-long \+r.6
  (+ -567/891 100L0)
  99.36363636363636364L0)

(deftest \+r.7
  (+ 4/5 #c(33 44))
  #c(169/5 44))

(deftest-double \+r.8
  (+ 4/5 #c(-12.3d0 4.5d-2))
  -11.5d0 0.045d0)

(deftest-single \+s.1
  (+ -20.0f0 30)
  10.0f0)

(deftest-single \+s.2
  (+ 20.0f0 (make-bignum -100))
  -80.0f0)

(deftest-single \+s.3
  (+ 111.0f0 3/4)
  111.75f0)

(deftest-single \+s.4
  (+ 4.0f0 -0.4f0)
  3.6f0)

(deftest-double \+s.5
  (+ -4.0f0 0.4d0)
  -3.6d0)

(deftest-long \+s.6
  (+ -4.0f0 100L0)
  96.0L0)

(deftest \+s.7
  (+ 10.0f0 #c(33 44))
  #c(43.0f0 44.0f0))

(deftest-double \+s.8
  (+ 10.0f0 #c(-12.3d0 4.5d-2))
  -2.3d0 4.5d-2)

(deftest-double \+d.1
  (+ -20.0d0 30)
  10.0d0)

(deftest-double \+d.2
  (+ 20.0d0 (make-bignum -100))
  -80.0d0)

(deftest-double \+d.3
  (+ 111.0d0 3/4)
  111.75d0)

(deftest-double \+d.4
  (+ 4.0d0 -0.4f0)
  3.6d0 0.0d0 1e-6)

(deftest-double \+d.5
  (+ -4.0d0 0.4d0)
  -3.6d0)

(deftest-long \+d.6
  (+ -4.0d0 100L0)
  96.0L0)

(deftest \+d.7
  (+ 10.0d0 #c(33 44))
  #c(43.0d0 44.0d0))

(deftest-double \+d.8
  (+ 10.0d0 #c(-12.3d0 4.5d-2))
  -2.3d0 4.5d-2)

(deftest-long \+l.1
  (+ -20.0L0 30)
  10.0L0)

(deftest-long \+l.2
  (+ 20.0L0 (make-bignum -100))
  -80.0L0)

(deftest-long \+l.3
  (+ 111.0L0 3/4)
  111.75L0)

(deftest-long \+l.4
  (+ 4.0L0 -0.4f0)
  3.6L0 0.0L0 1e-6)

(deftest-long \+l.5
  (+ -4.0L0 0.4d0)
  -3.6L0)

(deftest-long \+l.6
  (+ -4.0L0 100L0)
  96.0L0)

(deftest \+l.7
  (+ 10.0L0 #c(33 44))
  #c(43.0L0 44.0L0))

(deftest-long \+l.8
  (+ 10.0L0 #c(-12.3d0 4.5d-2))
  -2.3L0 4.5L-2)

(deftest \+c.1
  (+ #c(10 20) 30)
  #c(40 20))

(deftest \+c.2
  (+ #c(12.3f0 45.6f0) 30)
  #c(42.3f0 45.6f0))

(deftest \+c.3
  (+ #c(10 20) (make-bignum -100))
  #c(-90 20))

(deftest \+c.4
  (+ #c(1.23f0 9.87f0) (make-bignum -20))
  #c(-18.77 9.87))

(deftest \+c.5
  (+ #c(10 20) 3/4)
  #c(43/4 20))

(deftest-long \+c.6
  (+ #c(12.3L0 23.4L0) 3/4)
  13.05L0 23.4L0)

(deftest-single \+c.7
  (+ #c(10 20) -0.4f0)
  9.6f0 20.0f0)

(deftest-double \+c.8
  (+ #c(10d0 20d0) 0.4f0)
  10.4d0 20.0d0 1e-6)

(deftest-double \+c.9
  (+ #c(-4 -5) 0.4d0)
  -3.6d0 -5.0d0)

(deftest-double \+c.10
  (+ #c(-4.0f0 -5.0f0) 0.4d0)
  -3.6d0 -5.0d0 1e-6)

(deftest-long \+c.11
  (+ #c(1 2) 100.0L0)
  101.0L0 2.0L0)

(deftest-long \+c.12
  (+ #c(10.0f0 -20.0f0) -100.0L0)
  -90.0L0 -20.0L0 1e-6)

(deftest \+c.13
  (+ #c(-9 8) #c(33 44))
  #c(24 52))

(deftest-double \+c.14
  (+ #c(-9 8) #c(3.3d0 4.4d0))
  -5.7d0 12.4d0)

(deftest-double \+c.15
  (+ #c(-9.0d0 8.0d0) #c(-12 4))
  -21.0d0 12.0d0)

(deftest-double \+c.16
  (+ #c(-9.0d0 8.0d0) #c(-12.3d0 4.5d0))
  -21.3d0 12.5d0)

(deftest-error! \+-error.1
  (eval '(+ 'aaa))
  type-error)

(deftest-error \+-error.2
  (+ most-positive-single-float most-positive-single-float)
  floating-point-overflow)

(deftest-error \+-error.3
  (+ most-positive-double-float most-positive-double-float)
  floating-point-overflow)

(deftest-error \+-error.4
  (+ most-positive-long-float most-positive-long-float)
  floating-point-overflow)

;;  ANSI Common Lisp
(deftest \+-test.1
  (+)
  0)

(deftest \+-test.2
  (+ 1)
  1)

(deftest \+-test.3
  (+ 31/100 69/100)
  1)

(deftest \+-test.4
  (+ 1/5 0.8)
  1.0)


;;
;;  Function -
;;
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

(deftest \-f.1
  (- -20 30)
  -50)

(deftest \-f.2
  (- 20 (make-bignum -100))
  120)

(deftest \-f.3
  (- 111 3/4)
  441/4)

(deftest-single \-f.4
  (- 4 -0.4f0)
  4.4f0)

(deftest-double \-f.5
  (- -4 0.4d0)
  -4.4d0)

(deftest-long \-f.6
  (- -4 100L0)
  -104.0L0)

(deftest \-f.7
  (- 10 #c(33 44))
  #c(-23 -44))

(deftest-double \-f.8
  (- 10 #c(-12.3d0 4.5d-2))
  22.3d0 -0.045d0)

(deftest \-b.1
  (- (make-bignum -20) 30)
  -50)

(deftest \-b.2
  (- (make-bignum 20) (make-bignum -100))
  120)

(deftest \-b.3
  (- (make-bignum 111) 3/4)
  441/4)

(deftest-single \-b.4
  (- (make-bignum 4) -0.4f0)
  4.4f0)

(deftest-double \-b.5
  (- (make-bignum -4) 0.4d0)
  -4.4d0)

(deftest-long \-b.6
  (- (make-bignum -4) 100L0)
  -104.0L0)

(deftest \-b.7
  (- (make-bignum 10) #c(33 44))
  #c(-23 -44))

(deftest-double \-b.8
  (- (make-bignum 10) #c(-12.3d0 4.5d-2))
  22.3d0 -0.045d0)

(deftest \-r.1
  (- 41/189 30)
  -5629/189)

(deftest \-r.2
  (- 41/189 (make-bignum -30))
  5711/189)

(deftest \-r.3
  (- 41/189 3/4)
  -403/756)

(deftest-single \-r.4
  (- 41/189 -0.4f0)
  0.6169312)

(deftest-double \-r.5
  (- -41/189 0.4d0)
  -0.6169312169312169d0)

(deftest-long \-r.6
  (- -567/891 100L0)
  -100.63636363636363636L0)

(deftest \-r.7
  (- 4/5 #c(33 44))
  #c(-161/5 -44))

(deftest-double \-r.8
  (- 4/5 #c(-12.3d0 4.5d-2))
  13.1d0 -0.045d0)

(deftest-single \-s.1
  (- -20.0f0 30)
  -50.0f0)

(deftest-single \-s.2
  (- 20.0f0 (make-bignum -100))
  120.0f0)

(deftest-single \-s.3
  (- 111.0f0 3/4)
  110.25f0)

(deftest-single \-s.4
  (- 4.0f0 -0.4f0)
  4.4f0)

(deftest-double \-s.5
  (- -4.0f0 0.4d0)
  -4.4d0)

(deftest-long \-s.6
  (- -4.0f0 100L0)
  -104.0L0)

(deftest \-s.7
  (- 10.0f0 #c(33 44))
  #c(-23.0f0 -44.0f0))

(deftest-double \-s.8
  (- 10.0f0 #c(-12.3d0 4.5d-2))
  22.3d0 -0.045d0)

(deftest-double \-d.1
  (- -20.0d0 30)
  -50.0d0)

(deftest-double \-d.2
  (- 20.0d0 (make-bignum -100))
  120.0d0)

(deftest-double \-d.3
  (- 111.0d0 3/4)
  110.25d0)

(deftest-double \-d.4
  (- 4.0d0 -0.4f0)
  4.4d0 0.0d0 1e-6)

(deftest-double \-d.5
  (- -4.0d0 0.4d0)
  -4.4d0)

(deftest-long \-d.6
  (- -4.0d0 100L0)
  -104.0L0)

(deftest \-d.7
  (- 10.0d0 #c(33 44))
  #c(-23.0d0 -44.0d0))

(deftest-double \-d.8
  (- 10.0d0 #c(-12.3d0 4.5d-2))
  22.3d0 -0.045d0)

(deftest-long \-l.1
  (- -20.0L0 30)
  -50.0L0)

(deftest-long \-l.2
  (- 20.0L0 (make-bignum -100))
  120.0L0)

(deftest-long \-l.3
  (- 111.0L0 3/4)
  110.25L0)

(deftest-long \-l.4
  (- 4.0L0 -0.4f0)
  4.4L0 0.0L0 1e-6)

(deftest-long \-l.5
  (- -4.0L0 0.4d0)
  -4.4L0)

(deftest-long \-l.6
  (- -4.0L0 100L0)
  -104.0L0)

(deftest \-l.7
  (- 10.0L0 #c(33 44))
  #c(-23.0L0 -44.0L0))

(deftest-long \-l.8
  (- 10.0L0 #c(-12.3d0 4.5d-2))
  22.3L0 -4.5L-2)

(deftest \-c.1
  (- #c(10 20) 30)
  #c(-20 20))

(deftest \-c.2
  (- #c(12.3f0 45.6f0) 30)
  #c(-17.7f0 45.6f0))

(deftest \-c.3
  (- #c(10 20) (make-bignum -100))
  #c(110 20))

(deftest \-c.4
  (- #c(1.23f0 9.87f0) (make-bignum -20))
  #c(21.23 9.87))

(deftest \-c.5
  (- #c(10 20) 3/4)
  #c(37/4 20))

(deftest-long \-c.6
  (- #c(12.3L0 23.4L0) 3/4)
  11.55L0 23.4L0)

(deftest-single \-c.7
  (- #c(10 20) -0.4f0)
  10.4f0 20.0f0)

(deftest-double \-c.8
  (- #c(10d0 20d0) 0.4f0)
  9.6d0 20.0d0 1e-6)

(deftest-double \-c.9
  (- #c(-4 -5) 0.4d0)
  -4.4d0 -5.0d0)

(deftest-double \-c.10
  (- #c(-4.0f0 -5.0f0) 0.4d0)
  -4.4d0 -5.0d0 1e-6)

(deftest-long \-c.11
  (- #c(1 2) 100.0L0)
  -99.0L0 2.0L0)

(deftest-long \-c.12
  (- #c(10.0f0 -20.0f0) -100.0L0)
  110.0L0 -20.0L0 1e-6)

(deftest \-c.13
  (- #c(-9 8) #c(33 44))
  #c(-42 -36))

(deftest-double \-c.14
  (- #c(-9 8) #c(3.3d0 4.4d0))
  -12.3d0 3.6d0)

(deftest-double \-c.15
  (- #c(-9.0d0 8.0d0) #c(-12 4))
  3.0d0 4.0d0)

(deftest-double \-c.16
  (- #c(-9.0d0 8.0d0) #c(-12.3d0 4.5d0))
  3.3d0 3.5d0)

(deftest \--sign.1
  (values
    (- +0)
    (- -0)
    (- 10)
    (- -10))
  0 0 -10 10)

(deftest \--sign.2
  (values
    (zerop (- (make-bignum 0)))
    (- 100000000000000000000000000000000000000000000000000000)
    (- -100000000000000000000000000000000000000000000000000000))
  t
  -100000000000000000000000000000000000000000000000000000
  100000000000000000000000000000000000000000000000000000)

(deftest \--sign.3
  (values
    (zerop (- (make-ratio 0 10)))
    (- 3/4)
    (- -3/4))
  t -3/4 3/4)

(deftest \--sign.4
  (values
    (- +0.0s0)
    (- -0.0s0)
    (- 10.0s0)
    (- -10.0s0))
  -0.0s0 +0.0s0 -10.0s0 10.0s0)

(deftest \--sign.5
  (values
    (- +0.0f0)
    (- -0.0f0)
    (- 10.0f0)
    (- -10.0f0))
  -0.0f0 +0.0f0 -10.0f0 10.0f0)

(deftest \--sign.6
  (values
    (- +0.0d0)
    (- -0.0d0)
    (- 10.0d0)
    (- -10.0d0))
  -0.0d0 +0.0d0 -10.0d0 10.0d0)

(deftest \--sign.7
  (values
    (- +0.0L0)
    (- -0.0L0)
    (- 10.0L0)
    (- -10.0L0))
  -0.0L0 +0.0L0 -10.0L0 10.0L0)

(deftest \--sign.8
  (values
    (- #c(+0 -0))
    (- #c(-0 +0))
    (- #c(10 -20))
    (- #c(-10 20)))
  #c(-0 +0) #c(+0 -0) #c(-10 20) #c(10 -20))

(deftest \--sign.9
  (values
    (- #c(+0.0 -0.0))
    (- #c(-0.0 +0.0))
    (- #c(10.0 -20.0))
    (- #c(-10.0 20.0)))
  #c(-0.0 +0.0) #c(+0.0 -0.0) #c(-10.0 20.0) #c(10.0 -20.0))

(deftest-error! \--error.1
  (eval '(-)))

(deftest-error! \--error.2
  (eval '(- 'aaa))
  type-error)

(deftest-error! \--error.3
  (eval '(- 10 'aaa))
  type-error)

(deftest-error \--error.4
  (- most-positive-single-float most-negative-single-float)
  floating-point-overflow)

(deftest-error \--error.5
  (- most-positive-double-float most-negative-double-float)
  floating-point-overflow)

(deftest-error \--error.6
  (- most-positive-long-float most-negative-long-float)
  floating-point-overflow)

;;  ANSI Common Lisp
(deftest \--test.1
  (- 55.55)
  -55.55)

(deftest \--test.2
  (- #c(3 -5))
  #c(-3 5))

(deftest \--test.3
  (- 0)
  0)

(deftest \--test.4
  (eql (- 0.0) -0.0)
  t)

(deftest \--test.5
  (- #c(100 45) #c(0 45))
  100)

(deftest \--test.6
  (- 10 1 2 3 4)
  0)

