;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Function =
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

(deftest =.9
  (= 12 (make-bignum 12) (make-ratio 12 1) 12.0f0 12.0l0 #c(12 0) #c(12.0 0.0))
  t)

(deftest =f.1
  (values
    (fixnump 10)
    (= 10 10)
    (= 10 (make-bignum 10))
    (= 10 (make-ratio 10 1)))
  t t t t)

(deftest =f.2
  (values
    (= 10 20)
    (= 10 (make-bignum 20))
    (= 10 (make-ratio 10 2)))
  nil nil nil)

(deftest =f.3
  (values (= 10 10.0s0) (= 10 10.0f0) (= 10 10.0d0) (= 10 10.0l0))
  t t t t)

(deftest =f.4
  (values (= 10 10.3s0) (= 10 10.3f0) (= 10 10.3d0) (= 10 10.3l0))
  nil nil nil nil)

(deftest =f.5
  (values (= 10 #c(10 0)) (= 10 #c(10 10)) (= 10 #c(0 10)))
  t nil nil)

(deftest =f.6
  (values (= 10 #c(10.0 0)) (= 10 #c(10.0 10.0)) (= 10 #c(0 10.0)))
  t nil nil)

(defvar *=b* (make-bignum 10))
(deftest ==b.1
  (values
    (bignump *=b*)
    (= *=b* 10)
    (= *=b* (make-bignum 10))
    (= *=b* (make-ratio 10 1)))
  t t t t)

(deftest =b.2
  (values
    (= *=b* 20)
    (= *=b* (make-bignum 20))
    (= *=b* (make-ratio 10 2)))
  nil nil nil)

(deftest =b.3
  (values (= *=b* 10.0s0) (= *=b* 10.0f0) (= *=b* 10.0d0) (= *=b* 10.0l0))
  t t t t)

(deftest =b.4
  (values (= *=b* 10.3s0) (= *=b* 10.3f0) (= *=b* 10.3d0) (= *=b* 10.3l0))
  nil nil nil nil)

(deftest =b.5
  (values (= *=b* #c(10 0)) (= *=b* #c(10 10)) (= *=b* #c(0 10)))
  t nil nil)

(deftest =b.6
  (values (= *=b* #c(10.0 0)) (= *=b* #c(10 10.0)) (= *=b* #c(0 10.0)))
  t nil nil)

(deftest =b.7
  (values
    (= 77777777777777777778888888888888888899999999999999999
       77777777777777777778888888888888888899999999999999999)
    (= 47777777777777777778888888888888888899999999999999999
       77777777777777777778888888888888888899999999999999999)
    (= 77777777777777777778888888888888888899999999999999999
       77777777777777777778888888888888888899999999999999994))
  t nil nil)

(defvar *=r* (make-ratio 10 1))
(deftest =r.1
  (values
    (ratiop *=r*)
    (= *=r* 10)
    (= *=r* (make-bignum 10))
    (= *=r* (make-ratio 10 1)))
  t t t t)

(deftest =r.2
  (values
    (= *=r* 20)
    (= *=r* (make-bignum 20))
    (= *=r* (make-ratio 10 2)))
  nil nil nil)

(deftest =r.3
  (values (= *=r* 10.0s0) (= *=r* 10.0f0) (= *=r* 10.0d0) (= *=r* 10.0l0))
  t t t t)

(deftest =r.4
  (values (= *=r* 10.3s0) (= *=r* 10.3f0) (= *=r* 10.3d0) (= *=r* 10.3l0))
  nil nil nil nil)

(deftest =r.5
  (values (= *=r* #c(10 0)) (= *=r* #c(10 10)) (= *=r* #c(0 10)))
  t nil nil)

(deftest =r.6
  (values (= *=r* #c(10.0 0.0)) (= *=r* #c(10.0 10.0)) (= *=r* #c(0.0 10.0)))
  t nil nil)

(deftest =r.7
  (values
    (= 2/3 2/3) (= 1/3 2/3) (= 2/3 1/4)
    (= 1111111111111111222222222222223333333333333/4444444444444455555555555556666
       1111111111111111222222222222223333333333333/4444444444444455555555555556666)
    (= 2111111111111111222222222222223333333333333/4444444444444455555555555556666
       1111111111111111222222222222223333333333333/4444444444444455555555555556666)
    (= 1111111111111111222222222222223333333333333/4444444444444455555555555556666
       1111111111111111222222222222223333333333333/4444444444444455555555555556662))
  t nil nil
  t nil nil)

(deftest =s.1
  (values
    (= 10.0f0 10)
    (= 10.0f0 (make-bignum 10))
    (= 10.0f0 (make-ratio 10 1)))
  t t t)

(deftest =s.2
  (values
    (= 10.0f0 20)
    (= 10.0f0 (make-bignum 20))
    (= 10.0f0 (make-ratio 10 2)))
  nil nil nil)

(deftest =s.3
  (values (= 10.0f0 10.0s0) (= 10.0f0 10.0f0) (= 10.0f0 10.0d0) (= 10.0f0 10.0l0))
  t t t t)

(deftest =s.4
  (values (= 10.0f0 10.3s0) (= 10.0f0 10.3f0) (= 10.0f0 10.3d0) (= 10.0f0 10.3l0))
  nil nil nil nil)

(deftest =s.5
  (values (= 10.0f0 #c(10 0)) (= 10.0f0 #c(10 10)) (= 10.0f0 #c(0 10)))
  t nil nil)

(deftest =s.6
  (values (= 10.0f0 #c(10.0 0)) (= 10.0f0 #c(10.0 10)) (= 10.0f0 #c(0 10.0)))
  t nil nil)

(deftest =d.1
  (values
    (= 10.0d0 10)
    (= 10.0d0 (make-bignum 10))
    (= 10.0d0 (make-ratio 10 1)))
  t t t)

(deftest =d.2
  (values
    (= 10.0d0 20)
    (= 10.0d0 (make-bignum 20))
    (= 10.0d0 (make-ratio 10 2)))
  nil nil nil)

(deftest =d.3
  (values (= 10.0d0 10.0s0) (= 10.0d0 10.0f0) (= 10.0d0 10.0d0) (= 10.0d0 10.0l0))
  t t t t)

(deftest =d.4
  (values (= 10.0d0 10.3s0) (= 10.0d0 10.3f0) (= 10.0d0 10.3d0) (= 10.0d0 10.3l0))
  nil nil nil nil)

(deftest =d.5
  (values (= 10.0d0 #c(10 0)) (= 10.0d0 #c(10 10)) (= 10.0d0 #c(0 10)))
  t nil nil)

(deftest =d.6
  (values (= 10.0d0 #c(10.0 0)) (= 10.0d0 #c(10.0 10)) (= 10.0d0 #c(0 10.0)))
  t nil nil)

(deftest =l.1
  (values
    (= 10.0l0 10)
    (= 10.0l0 (make-bignum 10))
    (= 10.0l0 (make-ratio 10 1)))
  t t t)

(deftest =l.2
  (values
    (= 10.0l0 20)
    (= 10.0l0 (make-bignum 20))
    (= 10.0l0 (make-ratio 10 2)))
  nil nil nil)

(deftest =l.3
  (values (= 10.0l0 10.0s0) (= 10.0l0 10.0f0) (= 10.0l0 10.0d0) (= 10.0l0 10.0l0))
  t t t t)

(deftest =l.4
  (values (= 10.0l0 10.3s0) (= 10.0l0 10.3f0) (= 10.0l0 10.3d0) (= 10.0l0 10.3l0))
  nil nil nil nil)

(deftest =l.5
  (values (= 10.0l0 #c(10 0)) (= 10.0l0 #c(10 10)) (= 10.0l0 #c(0 10)))
  t nil nil)

(deftest =l.6
  (values (= 10.0l0 #c(10.0 0)) (= 10.0l0 #c(10.0 10)) (= 10.0l0 #c(0 10.0)))
  t nil nil)

(deftest-error! =-error.1
  (eval '(=)))

(deftest-error! =-error.2
  (eval '(= "Hello"))
  type-error)


;;
;;  Function /=
;;
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

(deftest /=.8
  (/= 12 (make-bignum 34) (make-ratio 3 4) 12.3)
  t)

(deftest /=f.1
  (values
    (/= 10 10)
    (/= 10 (make-bignum 10))
    (/= 10 (make-ratio 10 1)))
  nil nil nil)

(deftest /=f.2
  (values
    (/= 10 20)
    (/= 10 (make-bignum 20))
    (/= 10 (make-ratio 10 2)))
  t t t)

(deftest /=f.3
  (values (/= 10 10.0s0) (/= 10 10.0f0) (/= 10 10.0d0) (/= 10 10.0l0))
  nil nil nil nil)

(deftest /=f.4
  (values (/= 10 10.3s0) (/= 10 10.3f0) (/= 10 10.3d0) (/= 10 10.3l0))
  t t t t)

(deftest /=f.5
  (values (/= 10 #c(10 0)) (/= 10 #c(10 10)) (/= 10 #c(0 10)))
  nil t t)

(deftest /=f.6
  (values (/= 10 #c(10.0 0)) (/= 10 #c(10.0 10.0)) (/= 10 #c(0 10.0)))
  nil t t)

(defvar */=b* (make-bignum 10))
(deftest /=b.1
  (values
    (/= */=b* 10)
    (/= */=b* (make-bignum 10))
    (/= */=b* (make-ratio 10 1)))
  nil nil nil)

(deftest /=b.2
  (values
    (/= */=b* 20)
    (/= */=b* (make-bignum 20))
    (/= */=b* (make-ratio 10 2)))
  t t t)

(deftest /=b.3
  (values (/= */=b* 10.0s0) (/= */=b* 10.0f0) (/= */=b* 10.0d0) (/= */=b* 10.0l0))
  nil nil nil nil)

(deftest /=b.4
  (values (/= */=b* 10.3s0) (/= */=b* 10.3f0) (/= */=b* 10.3d0) (/= */=b* 10.3l0))
  t t t t)

(deftest /=b.5
  (values (/= */=b* #c(10 0)) (/= */=b* #c(10 10)) (/= */=b* #c(0 10)))
  nil t t)

(deftest /=b.6
  (values (/= */=b* #c(10.0 0)) (/= */=b* #c(10 10.0)) (/= */=b* #c(0 10.0)))
  nil t t)

(deftest /=b.7
  (values
    (/= 77777777777777777778888888888888888899999999999999999
        77777777777777777778888888888888888899999999999999999)
    (/= 47777777777777777778888888888888888899999999999999999
        77777777777777777778888888888888888899999999999999999)
    (/= 77777777777777777778888888888888888899999999999999999
        77777777777777777778888888888888888899999999999999994))
  nil t t)

(defvar */=r* (make-ratio 10 1))
(deftest /=r.1
  (values
    (/= */=r* 10)
    (/= */=r* (make-bignum 10))
    (/= */=r* (make-ratio 10 1)))
  nil nil nil)

(deftest /=r.2
  (values
    (/= */=r* 20)
    (/= */=r* (make-bignum 20))
    (/= */=r* (make-ratio 10 2)))
  t t t)

(deftest /=r.3
  (values (/= */=r* 10.0s0) (/= */=r* 10.0f0) (/= */=r* 10.0d0) (/= */=r* 10.0l0))
  nil nil nil nil)

(deftest /=r.4
  (values (/= */=r* 10.3s0) (/= */=r* 10.3f0) (/= */=r* 10.3d0) (/= */=r* 10.3l0))
  t t t t)

(deftest /=r.5
  (values (/= */=r* #c(10 0)) (/= */=r* #c(10 10)) (/= */=r* #c(0 10)))
  nil t t)

(deftest /=r.6
  (values (/= */=r* #c(10.0 0.0)) (/= */=r* #c(10.0 10.0)) (/= */=r* #c(0.0 10.0)))
  nil t t)

(deftest /=r.7
  (values
    (/= 2/3 2/3) (/= 1/3 2/3) (/= 2/3 1/4)
    (/= 1111111111111111222222222222223333333333333/4444444444444455555555555556666
        1111111111111111222222222222223333333333333/4444444444444455555555555556666)
    (/= 2111111111111111222222222222223333333333333/4444444444444455555555555556666
        1111111111111111222222222222223333333333333/4444444444444455555555555556666)
    (/= 1111111111111111222222222222223333333333333/4444444444444455555555555556666
        1111111111111111222222222222223333333333333/4444444444444455555555555556662))
  nil t t
  nil t t)

(deftest /=s.1
  (values
    (/= 10.0f0 10)
    (/= 10.0f0 (make-bignum 10))
    (/= 10.0f0 (make-ratio 10 1)))
  nil nil nil)

(deftest /=s.2
  (values
    (/= 10.0f0 20)
    (/= 10.0f0 (make-bignum 20))
    (/= 10.0f0 (make-ratio 10 2)))
  t t t)

(deftest /=s.3
  (values (/= 10.0f0 10.0s0) (/= 10.0f0 10.0f0) (/= 10.0f0 10.0d0) (/= 10.0f0 10.0l0))
  nil nil nil nil)

(deftest /=s.4
  (values (/= 10.0f0 10.3s0) (/= 10.0f0 10.3f0) (/= 10.0f0 10.3d0) (/= 10.0f0 10.3l0))
  t t t t)

(deftest /=s.5
  (values (/= 10.0f0 #c(10 0)) (/= 10.0f0 #c(10 10)) (/= 10.0f0 #c(0 10)))
  nil t t)

(deftest /=s.6
  (values (/= 10.0f0 #c(10.0f0 0)) (/= 10.0f0 #c(10.0f0 10)) (/= 10.0f0 #c(0 10.0f0)))
  nil t t)

(deftest /=d.1
  (values
    (/= 10.0d0 10)
    (/= 10.0d0 (make-bignum 10))
    (/= 10.0d0 (make-ratio 10 1)))
  nil nil nil)

(deftest /=d.2
  (values
    (/= 10.0d0 20)
    (/= 10.0d0 (make-bignum 20))
    (/= 10.0d0 (make-ratio 10 2)))
  t t t)

(deftest /=d.3
  (values (/= 10.0d0 10.0s0) (/= 10.0d0 10.0f0) (/= 10.0d0 10.0d0) (/= 10.0d0 10.0l0))
  nil nil nil nil)

(deftest /=d.4
  (values (/= 10.0d0 10.3s0) (/= 10.0d0 10.3f0) (/= 10.0d0 10.3d0) (/= 10.0d0 10.3l0))
  t t t t)

(deftest /=d.5
  (values (/= 10.0d0 #c(10 0)) (/= 10.0d0 #c(10 10)) (/= 10.0d0 #c(0 10)))
  nil t t)

(deftest /=d.6
  (values (/= 10.0d0 #c(10.0 0)) (/= 10.0d0 #c(10.0 10)) (/= 10.0d0 #c(0 10.0)))
  nil t t)

(deftest /=l.1
  (values
    (/= 10.0l0 10)
    (/= 10.0l0 (make-bignum 10))
    (/= 10.0l0 (make-ratio 10 1)))
  nil nil nil)

(deftest /=l.2
  (values
    (/= 10.0l0 20)
    (/= 10.0l0 (make-bignum 20))
    (/= 10.0l0 (make-ratio 10 2)))
  t t t)

(deftest /=l.3
  (values (/= 10.0l0 10.0s0) (/= 10.0l0 10.0f0) (/= 10.0l0 10.0d0) (/= 10.0l0 10.0l0))
  nil nil nil nil)

(deftest /=l.4
  (values (/= 10.0l0 10.3s0) (/= 10.0l0 10.3f0) (/= 10.0l0 10.3d0) (/= 10.0l0 10.3l0))
  t t t t)

(deftest /=l.5
  (values (/= 10.0l0 #c(10 0)) (/= 10.0l0 #c(10 10)) (/= 10.0l0 #c(0 10)))
  nil t t)

(deftest /=l.6
  (values (/= 10.0l0 #c(10.0 0)) (/= 10.0l0 #c(10.0 10)) (/= 10.0l0 #c(0 10.0)))
  nil t t)

(deftest-error! /=-error.1
  (eval '(/=)))

(deftest-error! /=-error.2
  (eval '(/= "Hello"))
  type-error)


;;
;;  ANSI Common Lisp
;;
(deftest number-compare-test.1
  (= 3 3)
  t)

(deftest number-compare-test.2
  (/= 3 3)
  nil)

(deftest number-compare-test.3
  (= 3 5)
  nil)

(deftest number-compare-test.4
  (/= 3 5)
  t)

(deftest number-compare-test.5
  (= 3 3 3 3)
  t)

(deftest number-compare-test.6
  (/= 3 3 3 3)
  nil)

(deftest number-compare-test.7
  (= 3 3 5 3)
  nil)

(deftest number-compare-test.8
  (/= 3 3 5 3)
  nil)

(deftest number-compare-test.9
  (= 3 6 5 2)
  nil)

(deftest number-compare-test.10
  (/= 3 6 5 2)
  t)

(deftest number-compare-test.11
  (= 3 2 3)
  nil)

(deftest number-compare-test.12
  (/= 3 2 3)
  nil)

(deftest number-compare-test.13
  (< 3 5)
  t)

(deftest number-compare-test.14
  (<= 3 5)
  t)

(deftest number-compare-test.15
  (< 3 -5)
  nil)

(deftest number-compare-test.16
  (<= 3 -5)
  nil)

(deftest number-compare-test.17
  (< 3 3)
  nil)

(deftest number-compare-test.18
  (<= 3 3)
  t)

(deftest number-compare-test.19
  (< 0 3 4 6 7)
  t)

(deftest number-compare-test.20
  (<= 0 3 4 6 7)
  t)

(deftest number-compare-test.21
  (< 0 3 4 4 6)
  nil)

(deftest number-compare-test.22
  (<= 0 3 4 4 6)
  t)

(deftest number-compare-test.23
  (> 4 3)
  t)

(deftest number-compare-test.24
  (>= 4 3)
  t)

(deftest number-compare-test.25
  (> 4 3 2 1 0)
  t)

(deftest number-compare-test.26
  (>= 4 3 2 1 0)
  t)

(deftest number-compare-test.27
  (> 4 3 3 2 0)
  nil)

(deftest number-compare-test.28
  (>= 4 3 3 2 0)
  t)

(deftest number-compare-test.29
  (> 4 3 1 2 0)
  nil)

(deftest number-compare-test.30
  (>= 4 3 1 2 0)
  nil)

(deftest number-compare-test.31
  (= 3)
  t)

(deftest number-compare-test.32
  (/= 3)
  t)

(deftest number-compare-test.33
  (< 3)
  t)

(deftest number-compare-test.34
  (<= 3)
  t)

(deftest number-compare-test.35
  (= 3.0 #c(3.0 0.0))
  t)

(deftest number-compare-test.36
  (/= 3.0 #c(3.0 1.0))
  t)

(deftest number-compare-test.37
  (= 3 3.0)
  t)

(deftest number-compare-test.38
  (= 3.0s0 3.0d0)
  t)

(deftest number-compare-test.39
  (= 0.0 -0.0)
  t)

(deftest number-compare-test.40
  (= 5/2 2.5)
  t)

(deftest number-compare-test.41
  (> 0.0 -0.0)
  nil)

(deftest number-compare-test.42
  (= 0 -0.0)
  t)

(deftest number-compare-test.43
  (flet ((r (x) (<= 0 x 9)))
    (values
      (and (r 0) (r 1) (r 3) (r 8) (r 9))
      (or (r -1) (r 10))))
  t nil)

(deftest number-compare-test.44
  (flet ((r (x) (< 0.0 x 1.0)))
    (values
      (and (r 0.01) (r 0.5) (r 0.99))
      (or (r -1.0) (r 0.0) (r 1.0) (r 1.1))))
  t nil)

(deftest number-compare-test.45
  (let ((v #(1 2 3 4 5)))
    (flet ((r (j) (< -1 j (length v))))
      (values
        (and (r 0) (r 1) (r 4))
        (or (r -2) (r -1) (r 5) (r 6)))))
  t nil)

