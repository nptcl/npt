;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Function MINUSP
;;
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

(deftest-error! minusp-error.1
  (eval '(minusp)))

(deftest-error! minusp-error.2
  (eval '(minusp 'hello))
  type-error)

(deftest-error! minusp-error.3
  (eval '(minusp 10 20)))

;;  ANSI Common Lisp
(deftest minusp-test.1
  (minusp -1)
  t)

(deftest minusp-test.2
  (minusp -0.0)
  nil)


;;
;;  Function PLUSP
;;
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

(deftest-error! plusp-error.1
  (eval '(plusp)))

(deftest-error! plusp-error.2
  (eval '(plusp 'hello))
  type-error)

(deftest-error! plusp-error.3
  (eval '(plusp 10 20)))

;;  ANSI Common Lisp
(deftest plusp-test.1
  (plusp 0)
  nil)

(deftest plusp-test.2
  (plusp least-positive-single-float)
  t)


;;
;;  Function ZEROP
;;
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

(deftest-error! zerop-error.1
  (eval '(zerop)))

(deftest-error! zerop-error.2
  (eval '(zerop 'hello))
  type-error)

(deftest-error! zerop-error.3
  (eval '(zerop 10 20)))

;;  ANSI Common Lisp
(deftest zerop-test.1
  (zerop 0)
  t)

(deftest zerop-test.2
  (zerop 1)
  nil)

(deftest zerop-test.3
  (zerop -0.0)
  t)

(deftest zerop-test.4
  (zerop 0/100)
  t)

(deftest zerop-test.5
  (zerop #c(0 0.0))
  t)


;;
;;  Function EVENP
;;
(deftest evenp.1
  (evenp 0)
  t)

(deftest evenp.2
  (evenp (make-bignum 0))
  t)

(deftest evenp.3
  (evenp 4)
  t)

(deftest evenp.4
  (evenp 5)
  nil)

(deftest evenp.5
  (evenp -4)
  t)

(deftest evenp.6
  (evenp -5)
  nil)

(deftest evenp.7
  (evenp (make-bignum 4))
  t)

(deftest evenp.8
  (evenp (make-bignum 5))
  nil)

(deftest evenp.9
  (evenp (make-bignum -4))
  t)

(deftest evenp.10
  (evenp (make-bignum -5))
  nil)

(deftest-error! evenp-error.1
  (eval '(evenp)))

(deftest-error! evenp-error.2
  (eval '(evenp 4/5))
  type-error)

(deftest-error! evenp-error.3
  (eval '(evenp 10 20)))

;;  ANSI Common Lisp
(deftest evenp-test.1
  (evenp 0)
  t)


;;
;;  Function ODDP
;;
(deftest oddp.1
  (oddp 0)
  nil)

(deftest oddp.2
  (oddp (make-bignum 0))
  nil)

(deftest oddp.3
  (oddp 4)
  nil)

(deftest oddp.4
  (oddp 5)
  t)

(deftest oddp.5
  (oddp -4)
  nil)

(deftest oddp.6
  (oddp -5)
  t)

(deftest oddp.7
  (oddp (make-bignum 4))
  nil)

(deftest oddp.8
  (oddp (make-bignum 5))
  t)

(deftest oddp.9
  (oddp (make-bignum -4))
  nil)

(deftest oddp.10
  (oddp (make-bignum -5))
  t)

(deftest-error! oddp-error.1
  (eval '(oddp)))

(deftest-error! oddp-error.2
  (eval '(oddp 4/5))
  type-error)

(deftest-error! oddp-error.3
  (eval '(oddp 10 20)))

;;  ANSI Common Lisp
(deftest oddp-test.1
  (oddp 10000000000000000000000)
  nil)

(deftest oddp-test.2
  (oddp -1)
  t)


;;
;;  Function RANDOM-STATE-P
;;
(deftest random-state-p.1
  (random-state-p *random-state*)
  t)

(deftest random-state-p.2
  (random-state-p (make-random-state))
  t)

(deftest random-state-p.3
  (random-state-p 'test-function)
  nil)

(deftest-error! random-state-p-error.1
  (eval '(random-state)))

(deftest-error! random-state-p-error.2
  (eval '(random-state nil nil)))


;;
;;  Function NUMBERP
;;
(deftest numberp.1
  (numberp 10)
  t)

(deftest numberp.2
  (numberp 9999999999999999999999999999999999999)
  t)

(deftest numberp.3
  (numberp 2/3)
  t)

(deftest numberp.4
  (numberp 2.3s0)
  t)

(deftest numberp.5
  (numberp 2.3f0)
  t)

(deftest numberp.6
  (numberp -1.2d0)
  t)

(deftest numberp.7
  (numberp 3.4L0)
  t)

(deftest numberp.8
  (numberp #c(1 2))
  t)

(deftest numberp.9
  (numberp #c(1.0 2.0))
  t)

(deftest numberp.10
  (numberp 'hello)
  nil)

(deftest-error! numberp-error.1
  (eval '(numberp)))

(deftest-error! numberp-error.2
  (eval '(numberp nil nil)))

;;  ANSI Common Lisp
(deftest numberp-test.1
  (numberp 12)
  t)

(deftest numberp-test.2
  (numberp (expt 2 130))
  t)

(deftest numberp-test.3
  (numberp #c(5/3 7.2))
  t)

(deftest numberp-test.4
  (numberp nil)
  nil)

(deftest numberp-test.5
  (numberp (cons 1 2))
  nil)


;;
;;  Function NUMBERP
;;
(deftest complexp.1
  (complexp 10)
  nil)

(deftest complexp.2
  (complexp 'hello)
  nil)

(deftest complexp.3
  (complexp #c(10 20))
  t)

(deftest complexp.4
  (complexp #c(10 0))
  nil)

(deftest complexp.5
  (complexp #c(0 20))
  t)

(deftest complexp.6
  (complexp #c(12.3 45))
  t)

(deftest complexp.7
  (complexp #c(12.3 0.0))
  t)

(deftest complexp.8
  (complexp #c(0.0d0 12.3d0))
  t)

(deftest complexp.9
  (complexp #c(12.3 -0.0))
  t)

(deftest-error! complexp-error.1
  (eval '(complexp)))

(deftest-error! complexp-error.2
  (eval '(complexp nil nil)))

;;  ANSI Common Lisp
(deftest complexp-test.1
  (complexp 1.2d2)
  nil)

(deftest complexp-test.2
  (complexp #c(5/3 7.2))
  t)


;;
;;  Function REALP
;;
(deftest realp.1
  (realp 10)
  t)

(deftest realp.2
  (realp 3/4)
  t)

(deftest realp.3
  (realp 4.5)
  t)

(deftest realp.4
  (realp 4.5d0)
  t)

(deftest realp.5
  (realp 4.5L0)
  t)

(deftest realp.6
  (realp #c(10 20))
  nil)

(deftest realp.7
  (realp #c(10.0 20.0))
  nil)

(deftest realp.8
  (realp 'hello)
  nil)

(deftest-error! realp-error.1
  (eval '(realp)))

(deftest-error! realp-error.2
  (eval '(realp nil nil)))

;;  ANSI Common Lisp
(deftest realp-test.1
  (realp 12)
  t)

(deftest realp-test.2
  (realp #c(5/3 7.2))
  nil)

(deftest realp-test.3
  (realp nil)
  nil)

(deftest realp-test.4
  (realp (cons 1 2))
  nil)


;;
;;  Function RATIONALP
;;
(deftest rationalp.1
  (rationalp 10)
  t)

(deftest rationalp.2
  (rationalp 200000000000000000000000000000000)
  t)

(deftest rationalp.3
  (rationalp 4/5)
  t)

(deftest rationalp.4
  (rationalp 34.5)
  nil)

(deftest-error! rationalp-error.1
  (eval '(rationalp)))

(deftest-error! rationalp-error.2
  (eval '(rationalp nil nil)))

;;  ANSI Comon Lisp
(deftest rationalp-test.1
  (rationalp 12)
  t)

(deftest rationalp-test.2
  (rationalp 6/5)
  t)

(deftest rationalp-test.3
  (rationalp 1.212)
  nil)


;;
;;  Function INTEGERP
;;
(deftest integerp.1
  (integerp 0)
  t)

(deftest integerp.2
  (integerp 10)
  t)

(deftest integerp.3
  (integerp -210)
  t)

(deftest integerp.4
  (integerp -22222222222222222222222222222222222222)
  t)

(deftest integerp.5
  (integerp 3/4)
  nil)

(deftest integerp.6
  (integerp 'hello)
  nil)

(deftest-error! integerp-error.1
  (eval '(integerp)))

(deftest-error! integerp-error.2
  (eval '(integerp nil nil)))

;;  ANSI Common Lisp
(deftest integerp-test.1
  (integerp 1)
  t)

(deftest integerp-test.2
  (integerp (expt 2 130))
  t)

(deftest integerp-test.3
  (integerp 6/5)
  nil)

(deftest integerp-test.4
  (integerp nil)
  nil)


;;
;;  Function FLOATP
;;
(deftest floatp.1
  (floatp 10)
  nil)

(deftest floatp.2
  (floatp 3/4)
  nil)

(deftest floatp.3
  (floatp 1.2)
  t)

(deftest floatp.4
  (floatp 2.3s1)
  t)

(deftest floatp.5
  (floatp 2.3f1)
  t)

(deftest floatp.6
  (floatp -2.3d-1)
  t)

(deftest floatp.7
  (floatp 2.5L-1)
  t)

(deftest floatp.8
  (floatp #c(1 3))
  nil)

(deftest floatp.9
  (floatp #c(1.2 3.4))
  nil)

(deftest-error! floatp-error.1
  (eval '(floatp)))

(deftest-error! floatp-error.2
  (eval '(floatp nil nil)))

;;  ANSI Common Lisp
(deftest floatp-test.1
  (floatp 1.2d2)
  t)

(deftest floatp-test.2
  (floatp 1.212)
  t)

(deftest floatp-test.3
  (floatp 1.2s2)
  t)

(deftest floatp-test.4
  (floatp (expt 2 130))
  nil)


;;
;;  Degrade
;;
(deftest numbers-test.1
  (values
    (= +0.0 -0.0)
    (eql +0.0 -0.0))
  t nil)

(deftest numbers-test.2
  (values
    (= +0.0s0 -0.0s0)
    (eql +0.0s0 -0.0s0))
  t nil)

(deftest numbers-test.3
  (values
    (= +0.0f0 -0.0f0)
    (eql +0.0f0 -0.0f0))
  t nil)

(deftest numbers-test.4
  (values
    (= +0.0d0 -0.0d0)
    (eql +0.0d0 -0.0d0))
  t nil)

(deftest numbers-test.5
  (values
    (= +0.0L0 -0.0L0)
    (eql +0.0L0 -0.0L0))
  t nil)

