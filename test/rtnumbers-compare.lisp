;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Function <
;;
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

(deftest <f.1
  (values
    (< 10 20)
    (< 10 (make-bignum 20))
    (< 10 (make-ratio 777 11)))
  t t t)

(deftest <f.2
  (values
    (< 10 10)
    (< 10 (make-bignum 10))
    (< 10 (make-ratio 10 1))
    (< 10 5)
    (< 10 (make-bignum 5))
    (< 10 (make-ratio 20 3)))
  nil nil nil nil nil nil)

(deftest <f.3
  (values (< 10 20.0s0) (< 10 20.0f0) (< 10 20.0d0) (< 10 20.0l0))
  t t t t)

(deftest <f.4
  (values (< 10 9.3s0) (< 10 9.3f0) (< 10 9.3d0) (< 10 9.3l0))
  nil nil nil nil)

(deftest <f.5
  (values (< 10 #c(20 0)) (< 10 #c(5 0)))
  t nil)

(defvar *<b* (make-bignum 10))
(deftest <b.1
  (values
    (< *<b* 20)
    (< *<b* (make-bignum 20))
    (< *<b* (make-ratio 777 11)))
  t t t)

(deftest <b.2
  (values
    (< *<b* 10)
    (< *<b* (make-bignum 10))
    (< *<b* (make-ratio 10 1))
    (< *<b* 5)
    (< *<b* (make-bignum 5))
    (< *<b* (make-ratio 20 3)))
  nil nil nil nil nil nil)

(deftest <b.3
  (values (< *<b* 20.0s0) (< *<b* 20.0f0) (< *<b* 20.0d0) (< *<b* 20.0l0))
  t t t t)

(deftest <b.4
  (values (< *<b* 9.3s0) (< *<b* 9.3f0) (< *<b* 9.3d0) (< *<b* 9.3l0))
  nil nil nil nil)

(deftest <b.5
  (values (< *<b* #c(20 0)) (< *<b* #c(5 0)))
  t nil)

(deftest <b.6
  (values
    (< 77777777777777777778888888888888888899999999999999999
       77777777777777777778888888888888888899999999999999999)
    (< 47777777777777777778888888888888888899999999999999999
       77777777777777777778888888888888888899999999999999999)
    (< 77777777777777777778888888888888888899999999999999999
       77777777777777777778888888888888888899999999999999994))
  nil t nil)

(defvar *<r* (make-ratio 10 1))
(deftest <r.1
  (values
    (< *<r* 20)
    (< *<r* (make-bignum 20))
    (< *<r* (make-ratio 777 11)))
  t t t)

(deftest <r.2
  (values
    (< *<r* 10)
    (< *<r* (make-bignum 10))
    (< *<r* (make-ratio 10 1))
    (< *<r* 5)
    (< *<r* (make-bignum 5))
    (< *<r* (make-ratio 20 3)))
  nil nil nil nil nil nil)

(deftest <r.3
  (values (< *<r* 20.0s0) (< *<r* 20.0f0) (< *<r* 20.0d0) (< *<r* 20.0l0))
  t t t t)

(deftest <r.4
  (values (< *<r* 9.3s0) (< *<r* 9.3f0) (< *<r* 9.3d0) (< *<r* 9.3l0))
  nil nil nil nil)

(deftest <r.5
  (values (< *<r* #c(20 0)) (< *<r* #c(5 0)))
  t nil)

(deftest <r.6
  (values
    (< 5/6 7/8)
    (< 11111111111111111111111111111111111/222222222222222222222222222222222
       55555555555555555555555555555555555/777777777777777777777777777777777))
  t t)

(deftest <s.1
  (values
    (< 10.0f0 20)
    (< 10.0f0 (make-bignum 20))
    (< 10.0f0 (make-ratio 20 1)))
  t t t)

(deftest <s.2
  (values
    (< 10.0f0 10)
    (< 10.0f0 (make-bignum 10))
    (< 10.0f0 (make-ratio 10 1))
    (< 10.0f0 5)
    (< 10.0f0 (make-bignum 5))
    (< 10.0f0 (make-ratio 20 3)))
  nil nil nil nil nil nil)

(deftest <s.3
  (values (< 10.0f0 20.0s0) (< 10.0f0 20.0f0) (< 10.0f0 20.0d0) (< 10.0f0 20.0l0))
  t t t t)

(deftest <s.4
  (values (< 10.0f0 9.3s0) (< 10.0f0 9.3f0) (< 10.0f0 9.3d0) (< 10.0f0 9.3l0))
  nil nil nil nil)

(deftest <s.5
  (values
    (< 10.0f0 #c(20 0))
    (< 10.0f0 #c(10 0)))
  t nil)

(deftest <d.1
  (values
    (< 10.0d0 20)
    (< 10.0d0 (make-bignum 20))
    (< 10.0d0 (make-ratio 20 1)))
  t t t)

(deftest <d.2
  (values
    (< 10.0d0 10)
    (< 10.0d0 (make-bignum 10))
    (< 10.0d0 (make-ratio 10 1))
    (< 10.0d0 5)
    (< 10.0d0 (make-bignum 5))
    (< 10.0d0 (make-ratio 20 3)))
  nil nil nil nil nil nil)

(deftest <d.3
  (values (< 10.0d0 20.0s0) (< 10.0d0 20.0f0) (< 10.0d0 20.0d0) (< 10.0d0 20.0l0))
  t t t t)

(deftest <d.4
  (values (< 10.0d0 9.3s0) (< 10.0d0 9.3f0) (< 10.0d0 9.3d0) (< 10.0d0 9.3l0))
  nil nil nil nil)

(deftest <d.5
  (values
    (< 10.0l0 #c(20 0))
    (< 10.0l0 #c(10 0)))
  t nil)

(deftest <l.1
  (values
    (< 10.0l0 20)
    (< 10.0l0 (make-bignum 20))
    (< 10.0l0 (make-ratio 20 1)))
  t t t)

(deftest <l.2
  (values
    (< 10.0l0 10)
    (< 10.0l0 (make-bignum 10))
    (< 10.0l0 (make-ratio 10 1))
    (< 10.0l0 5)
    (< 10.0l0 (make-bignum 5))
    (< 10.0l0 (make-ratio 20 3)))
  nil nil nil nil nil nil)

(deftest <l.3
  (values (< 10.0l0 20.0s0) (< 10.0l0 20.0f0) (< 10.0l0 20.0d0) (< 10.0l0 20.0l0))
  t t t t)

(deftest <l.4
  (values (< 10.0l0 9.3s0) (< 10.0l0 9.3f0) (< 10.0l0 9.3d0) (< 10.0l0 9.3l0))
  nil nil nil nil)

(deftest <l.5
  (values
    (< 10.0l0 #c(20 0))
    (< 10.0l0 #c(10 0)))
  t nil)

(deftest-error! <-error.1
  (eval '(<)))

(deftest-error! <-error.2
  (eval '(< "Hello"))
  type-error)


;;
;;  Function >
;;
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

(deftest >f.1
  (values
    (> 10 5)
    (> 10 (make-bignum 5))
    (> 10 (make-ratio 77 11)))
  t t t)

(deftest >f.2
  (values
    (> 10 10)
    (> 10 (make-bignum 10))
    (> 10 (make-ratio 10 1))
    (> 10 20)
    (> 10 (make-bignum 20))
    (> 10 (make-ratio 777 11)))
  nil nil nil nil nil nil)

(deftest >f.3
  (values (> 10 5.0s0) (> 10 5.0f0) (> 10 5.0d0) (> 10 5.0l0))
  t t t t)

(deftest >f.4
  (values (> 10 10.3s0) (> 10 10.3f0) (> 10 10.3d0) (> 10 10.3l0))
  nil nil nil nil)

(deftest >f.5
  (values (> 10 #c(5 0)) (> 10 #c(20 0)))
  t nil)

(defvar *>b* (make-bignum 10))
(deftest >b.1
  (values
    (> *>b* 5)
    (> *>b* (make-bignum 5))
    (> *>b* (make-ratio 77 11)))
  t t t)

(deftest >b.2
  (values
    (> *>b* 10)
    (> *>b* (make-bignum 10))
    (> *>b* (make-ratio 10 1))
    (> *>b* 20)
    (> *>b* (make-bignum 20))
    (> *>b* (make-ratio 777 11)))
  nil nil nil nil nil nil)

(deftest >b.3
  (values (> *>b* 5.0s0) (> *>b* 5.0f0) (> *>b* 5.0d0) (> *>b* 5.0l0))
  t t t t)

(deftest >b.4
  (values (> *>b* 10.3s0) (> *>b* 10.3f0) (> *>b* 10.3d0) (> *>b* 10.3l0))
  nil nil nil nil)

(deftest >b.5
  (values (> *>b* #c(5 0)) (> *>b* #c(10 0)))
  t nil)

(deftest >b.6
  (values
    (> 77777777777777777778888888888888888899999999999999999
       77777777777777777778888888888888888899999999999999999)
    (> 47777777777777777778888888888888888899999999999999999
       77777777777777777778888888888888888899999999999999999)
    (> 77777777777777777778888888888888888899999999999999999
       77777777777777777778888888888888888899999999999999994))
  nil nil t)

(defvar *>r* (make-ratio 10 1))
(deftest >r.1
  (values
    (> *>r* 5)
    (> *>r* (make-bignum 5))
    (> *>r* (make-ratio 77 11)))
  t t t)

(deftest >r.2
  (values
    (> *>r* 10)
    (> *>r* (make-bignum 10))
    (> *>r* (make-ratio 10 1))
    (> *>r* 20)
    (> *>r* (make-bignum 20))
    (> *>r* (make-ratio 777 11)))
  nil nil nil nil nil nil)

(deftest >r.3
  (values (> *>r* 5.0s0) (> *>r* 5.0f0) (> *>r* 5.0d0) (> *>r* 5.0l0))
  t t t t)

(deftest >r.4
  (values (> *>r* 10.3s0) (> *>r* 10.3f0) (> *>r* 10.3d0) (> *>r* 10.3l0))
  nil nil nil nil)

(deftest >r.5
  (values (> *>r* #c(5 0)) (> *>r* #c(10 0)))
  t nil)

(deftest >r.6
  (values
    (> 5/6 7/8)
    (> 11111111111111111111111111111111111/222222222222222222222222222222222
       55555555555555555555555555555555555/777777777777777777777777777777777))
  nil nil)

(deftest >s.1
  (values
    (> 10.0f0 5)
    (> 10.0f0 (make-bignum 5))
    (> 10.0f0 (make-ratio 5 1)))
  t t t)

(deftest >s.2
  (values
    (> 10.0f0 10)
    (> 10.0f0 (make-bignum 10))
    (> 10.0f0 (make-ratio 10 1))
    (> 10.0f0 20)
    (> 10.0f0 (make-bignum 20))
    (> 10.0f0 (make-ratio 777 11)))
  nil nil nil nil nil nil)

(deftest >s.3
  (values (> 10.0f0 5.0s0) (> 10.0f0 5.0f0) (> 10.0f0 5.0d0) (> 10.0f0 5.0l0))
  t t t t)

(deftest >s.4
  (values (> 10.0f0 10.3s0) (> 10.0f0 10.3f0) (> 10.0f0 10.3d0) (> 10.0f0 10.3l0))
  nil nil nil nil)

(deftest >s.5
  (values
    (> 10.0f0 #c(6 0))
    (> 10.0f0 #c(10 0)))
  t nil)

(deftest >d.1
  (values
    (> 10.0d0 5)
    (> 10.0d0 (make-bignum 5))
    (> 10.0d0 (make-ratio 5 1)))
  t t t)

(deftest >d.2
  (values
    (> 10.0d0 10)
    (> 10.0d0 (make-bignum 10))
    (> 10.0d0 (make-ratio 10 1))
    (> 10.0d0 20)
    (> 10.0d0 (make-bignum 20))
    (> 10.0d0 (make-ratio 777 11)))
  nil nil nil nil nil nil)

(deftest >d.3
  (values (> 10.0d0 5.0s0) (> 10.0d0 5.0f0) (> 10.0d0 5.0d0) (> 10.0d0 5.0l0))
  t t t t)

(deftest >d.4
  (values (> 10.0d0 10.3s0) (> 10.0d0 10.3f0) (> 10.0d0 10.3d0) (> 10.0d0 10.3l0))
  nil nil nil nil)

(deftest >d.5
  (values
    (> 10.0l0 #c(5 0))
    (> 10.0l0 #c(10 0)))
  t nil)

(deftest >l.1
  (values
    (> 10.0l0 5)
    (> 10.0l0 (make-bignum 5))
    (> 10.0l0 (make-ratio 5 1)))
  t t t)

(deftest >l.2
  (values
    (> 10.0l0 10)
    (> 10.0l0 (make-bignum 10))
    (> 10.0l0 (make-ratio 10 1))
    (> 10.0l0 20)
    (> 10.0l0 (make-bignum 20))
    (> 10.0l0 (make-ratio 777 11)))
  nil nil nil nil nil nil)

(deftest >l.3
  (values (> 10.0l0 5.0s0) (> 10.0l0 5.0f0) (> 10.0l0 5.0d0) (> 10.0l0 5.0l0))
  t t t t)

(deftest >l.4
  (values (> 10.0l0 10.3s0) (> 10.0l0 10.3f0) (> 10.0l0 10.3d0) (> 10.0l0 10.3l0))
  nil nil nil nil)

(deftest >l.5
  (values
    (> 10.0l0 #c(5 0))
    (> 10.0l0 #c(10 0)))
  t nil)

(deftest-error! >-error.1
  (eval '(>)))

(deftest-error! >-error.2
  (eval '(> "Hello"))
  type-error)


;;
;;  Function <=
;;
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

(deftest <=f.1
  (values
    (<= 10 20)
    (<= 10 (make-bignum 20))
    (<= 10 (make-ratio 777 11)))
  t t t)

(deftest <=f.2
  (values
    (<= 10 10)
    (<= 10 (make-bignum 10))
    (<= 10 (make-ratio 10 1))
    (<= 10 5)
    (<= 10 (make-bignum 5))
    (<= 10 (make-ratio 20 3)))
  t t t nil nil nil)

(deftest <=f.3
  (values (<= 10 20.0s0) (<= 10 20.0f0) (<= 10 20.0d0) (<= 10 20.0l0))
  t t t t)

(deftest <=f.4
  (values (<= 10 9.3s0) (<= 10 9.3f0) (<= 10 9.3d0) (<= 10 9.3l0))
  nil nil nil nil)

(deftest <=f.5
  (values (<= 10 #c(20 0)) (<= 10 #c(5 0)))
  t nil)

(defvar *<=b* (make-bignum 10))
(deftest <=b.1
  (values
    (<= *<=b* 20)
    (<= *<=b* (make-bignum 20))
    (<= *<=b* (make-ratio 777 11)))
  t t t)

(deftest <=b.2
  (values
    (<= *<=b* 10)
    (<= *<=b* (make-bignum 10))
    (<= *<=b* (make-ratio 10 1))
    (<= *<=b* 5)
    (<= *<=b* (make-bignum 5))
    (<= *<=b* (make-ratio 20 3)))
  t t t nil nil nil)

(deftest <=b.3
  (values (<= *<=b* 20.0s0) (<= *<=b* 20.0f0) (<= *<=b* 20.0d0) (<= *<=b* 20.0l0))
  t t t t)

(deftest <=b.4
  (values (<= *<=b* 9.3s0) (<= *<=b* 9.3f0) (<= *<=b* 9.3d0) (<= *<=b* 9.3l0))
  nil nil nil nil)

(deftest <=b.5
  (values (<= *<=b* #c(20 0)) (<= *<=b* #c(5 0)))
  t nil)

(deftest <=b.6
  (values
    (<= 77777777777777777778888888888888888899999999999999999
        77777777777777777778888888888888888899999999999999999)
    (<= 47777777777777777778888888888888888899999999999999999
        77777777777777777778888888888888888899999999999999999)
    (<= 77777777777777777778888888888888888899999999999999999
        77777777777777777778888888888888888899999999999999994))
  t t nil)

(defvar *<=r* (make-ratio 10 1))
(deftest <=r.1
  (values
    (<= *<=r* 20)
    (<= *<=r* (make-bignum 20))
    (<= *<=r* (make-ratio 777 11)))
  t t t)

(deftest <=r.2
  (values
    (<= *<=r* 10)
    (<= *<=r* (make-bignum 10))
    (<= *<=r* (make-ratio 10 1))
    (<= *<=r* 5)
    (<= *<=r* (make-bignum 5))
    (<= *<=r* (make-ratio 20 3)))
  t t t nil nil nil)

(deftest <=r.3
  (values (<= *<=r* 20.0s0) (<= *<r* 20.0f0) (<= *<=r* 20.0d0) (<= *<=r* 20.0l0))
  t t t t)

(deftest <=r.4
  (values (<= *<=r* 9.3s0) (<= *<=r* 9.3f0) (<= *<=r* 9.3d0) (<= *<=r* 9.3l0))
  nil nil nil nil)

(deftest <=r.5
  (values (<= *<=r* #c(20 0)) (<= *<=r* #c(5 0)))
  t nil)

(deftest <=r.6
  (values
    (<= 5/6 7/8)
    (<= 11111111111111111111111111111111111/222222222222222222222222222222222
        55555555555555555555555555555555555/777777777777777777777777777777777))
  t t)

(deftest <=s.1
  (values
    (<= 10.0f0 20)
    (<= 10.0f0 (make-bignum 20))
    (<= 10.0f0 (make-ratio 20 1)))
  t t t)

(deftest <=s.2
  (values
    (<= 10.0f0 10)
    (<= 10.0f0 (make-bignum 10))
    (<= 10.0f0 (make-ratio 10 1))
    (<= 10.0f0 5)
    (<= 10.0f0 (make-bignum 5))
    (<= 10.0f0 (make-ratio 20 3)))
  t t t nil nil nil)

(deftest <=s.3
  (values (<= 10.0f0 20.0s0) (<= 10.0f0 20.0f0) (<= 10.0f0 20.0d0) (<= 10.0f0 20.0l0))
  t t t t)

(deftest <=s.4
  (values (<= 10.0f0 9.3s0) (<= 10.0f0 9.3f0) (<= 10.0f0 9.3d0) (<= 10.0f0 9.3l0))
  nil nil nil nil)

(deftest <=s.5
  (values
    (<= 10.0f0 #c(20 0))
    (<= 10.0f0 #c(10 0)))
  t t)

(deftest <=d.1
  (values
    (<= 10.0d0 20)
    (<= 10.0d0 (make-bignum 20))
    (<= 10.0d0 (make-ratio 20 1)))
  t t t)

(deftest <=d.2
  (values
    (<= 10.0d0 10)
    (<= 10.0d0 (make-bignum 10))
    (<= 10.0d0 (make-ratio 10 1))
    (<= 10.0d0 5)
    (<= 10.0d0 (make-bignum 5))
    (<= 10.0d0 (make-ratio 20 3)))
  t t t nil nil nil)

(deftest <=d.3
  (values (<= 10.0d0 20.0s0) (<= 10.0d0 20.0f0) (<= 10.0d0 20.0d0) (<= 10.0d0 20.0l0))
  t t t t)

(deftest <=d.4
  (values (<= 10.0d0 9.3s0) (<= 10.0d0 9.3f0) (<= 10.0d0 9.3d0) (<= 10.0d0 9.3l0))
  nil nil nil nil)

(deftest <=d.5
  (values
    (<= 10.0l0 #c(20 0))
    (<= 10.0l0 #c(10 0)))
  t t)

(deftest <=l.1
  (values
    (<= 10.0l0 20)
    (<= 10.0l0 (make-bignum 20))
    (<= 10.0l0 (make-ratio 20 1)))
  t t t)

(deftest <=l.2
  (values
    (<= 10.0l0 10)
    (<= 10.0l0 (make-bignum 10))
    (<= 10.0l0 (make-ratio 10 1))
    (<= 10.0l0 5)
    (<= 10.0l0 (make-bignum 5))
    (<= 10.0l0 (make-ratio 20 3)))
  t t t nil nil nil)

(deftest <=l.3
  (values (<= 10.0l0 20.0s0) (<= 10.0l0 20.0f0) (<= 10.0l0 20.0d0) (<= 10.0l0 20.0l0))
  t t t t)

(deftest <=l.4
  (values (<= 10.0l0 9.3s0) (<= 10.0l0 9.3f0) (<= 10.0l0 9.3d0) (<= 10.0l0 9.3l0))
  nil nil nil nil)

(deftest <=l.5
  (values
    (<= 10.0l0 #c(20 0))
    (<= 10.0l0 #c(10 0)))
  t t)

(deftest-error! <=-error.1
  (eval '(<=)))

(deftest-error! <=-error.2
  (eval '(<= "Hello"))
  type-error)


;;
;;  Function >=
;;
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

(deftest >=f.1
  (values
    (>= 10 5)
    (>= 10 (make-bignum 5))
    (>= 10 (make-ratio 77 11)))
  t t t)

(deftest >=f.2
  (values
    (>= 10 10)
    (>= 10 (make-bignum 10))
    (>= 10 (make-ratio 10 1))
    (>= 10 20)
    (>= 10 (make-bignum 20))
    (>= 10 (make-ratio 777 11)))
  t t t nil nil nil)

(deftest >=f.3
  (values (>= 10 5.0s0) (>= 10 5.0f0) (>= 10 5.0d0) (>= 10 5.0l0))
  t t t t)

(deftest >=f.4
  (values (>= 10 10.3s0) (>= 10 10.3f0) (>= 10 10.3d0) (>= 10 10.3l0))
  nil nil nil nil)

(deftest >=f.5
  (values (>= 10 #c(5 0)) (>= 10 #c(20 0)))
  t nil)

(defvar *>=b* (make-bignum 10))
(deftest >=b.1
  (values
    (>= *>=b* 5)
    (>= *>=b* (make-bignum 5))
    (>= *>=b* (make-ratio 77 11)))
  t t t)

(deftest >=b.2
  (values
    (>= *>=b* 10)
    (>= *>=b* (make-bignum 10))
    (>= *>=b* (make-ratio 10 1))
    (>= *>=b* 20)
    (>= *>=b* (make-bignum 20))
    (>= *>=b* (make-ratio 777 11)))
  t t t nil nil nil)

(deftest >=b.3
  (values (>= *>=b* 5.0s0) (>= *>=b* 5.0f0) (>= *>=b* 5.0d0) (>= *>=b* 5.0l0))
  t t t t)

(deftest >=b.4
  (values (>= *>=b* 10.3s0) (>= *>=b* 10.3f0) (>= *>=b* 10.3d0) (>= *>=b* 10.3l0))
  nil nil nil nil)

(deftest >=b.5
  (values (>= *>=b* #c(5 0)) (>= *>=b* #c(10 0)))
  t t)

(deftest >=b.6
  (values
    (>= 77777777777777777778888888888888888899999999999999999
        77777777777777777778888888888888888899999999999999999)
    (>= 47777777777777777778888888888888888899999999999999999
        77777777777777777778888888888888888899999999999999999)
    (>= 77777777777777777778888888888888888899999999999999999
        77777777777777777778888888888888888899999999999999994))
  t nil t)

(defvar *>=r* (make-ratio 10 1))
(deftest >=r.1
  (values
    (>= *>=r* 5)
    (>= *>=r* (make-bignum 5))
    (>= *>=r* (make-ratio 77 11)))
  t t t)

(deftest >=r.2
  (values
    (>= *>=r* 10)
    (>= *>=r* (make-bignum 10))
    (>= *>=r* (make-ratio 10 1))
    (>= *>=r* 20)
    (>= *>=r* (make-bignum 20))
    (>= *>=r* (make-ratio 777 11)))
  t t t nil nil nil)

(deftest >=r.3
  (values (>= *>=r* 5.0s0) (>= *>=r* 5.0f0) (>= *>=r* 5.0d0) (>= *>=r* 5.0l0))
  t t t t)

(deftest >=r.4
  (values (>= *>=r* 10.3s0) (>= *>=r* 10.3f0) (>= *>=r* 10.3d0) (>= *>=r* 10.3l0))
  nil nil nil nil)

(deftest >=r.5
  (values (>= *>=r* #c(5 0)) (>= *>=r* #c(10 0)))
  t t)

(deftest >=r.6
  (values
    (>= 5/6 7/8)
    (>= 11111111111111111111111111111111111/222222222222222222222222222222222
        55555555555555555555555555555555555/777777777777777777777777777777777))
  nil nil)

(deftest >=s.1
  (values
    (>= 10.0f0 5)
    (>= 10.0f0 (make-bignum 5))
    (>= 10.0f0 (make-ratio 5 1)))
  t t t)

(deftest >=s.2
  (values
    (>= 10.0f0 10)
    (>= 10.0f0 (make-bignum 10))
    (>= 10.0f0 (make-ratio 10 1))
    (>= 10.0f0 20)
    (>= 10.0f0 (make-bignum 20))
    (>= 10.0f0 (make-ratio 777 11)))
  t t t nil nil nil)

(deftest >=s.3
  (values (>= 10.0f0 5.0s0) (>= 10.0f0 5.0f0) (>= 10.0f0 5.0d0) (>= 10.0f0 5.0l0))
  t t t t)

(deftest >=s.4
  (values (>= 10.0f0 10.3s0) (>= 10.0f0 10.3f0) (>= 10.0f0 10.3d0) (>= 10.0f0 10.3l0))
  nil nil nil nil)

(deftest >=s.5
  (values
    (>= 10.0f0 #c(6 0))
    (>= 10.0f0 #c(10 0)))
  t t)

(deftest >=d.1
  (values
    (>= 10.0d0 5)
    (>= 10.0d0 (make-bignum 5))
    (>= 10.0d0 (make-ratio 5 1)))
  t t t)

(deftest >=d.2
  (values
    (>= 10.0d0 10)
    (>= 10.0d0 (make-bignum 10))
    (>= 10.0d0 (make-ratio 10 1))
    (>= 10.0d0 20)
    (>= 10.0d0 (make-bignum 20))
    (>= 10.0d0 (make-ratio 777 11)))
  t t t nil nil nil)

(deftest >=d.3
  (values (>= 10.0d0 5.0s0) (>= 10.0d0 5.0f0) (>= 10.0d0 5.0d0) (>= 10.0d0 5.0l0))
  t t t t)

(deftest >=d.4
  (values (>= 10.0d0 10.3s0) (>= 10.0d0 10.3f0) (>= 10.0d0 10.3d0) (>= 10.0d0 10.3l0))
  nil nil nil nil)

(deftest >=d.5
  (values
    (>= 10.0l0 #c(5 0))
    (>= 10.0l0 #c(10 0)))
  t t)

(deftest >=l.1
  (values
    (>= 10.0l0 5)
    (>= 10.0l0 (make-bignum 5))
    (>= 10.0l0 (make-ratio 5 1)))
  t t t)

(deftest >=l.2
  (values
    (>= 10.0l0 10)
    (>= 10.0l0 (make-bignum 10))
    (>= 10.0l0 (make-ratio 10 1))
    (>= 10.0l0 20)
    (>= 10.0l0 (make-bignum 20))
    (>= 10.0l0 (make-ratio 777 11)))
  t t t nil nil nil)

(deftest >=l.3
  (values (>= 10.0l0 5.0s0) (>= 10.0l0 5.0f0) (>= 10.0l0 5.0d0) (>= 10.0l0 5.0l0))
  t t t t)

(deftest >=l.4
  (values (>= 10.0l0 10.3s0) (>= 10.0l0 10.3f0) (>= 10.0l0 10.3d0) (>= 10.0l0 10.3l0))
  nil nil nil nil)

(deftest >=l.5
  (values
    (>= 10.0l0 #c(5 0))
    (>= 10.0l0 #c(10 0)))
  t t)

(deftest-error! >=-error.1
  (eval '(>=)))

(deftest-error! >=-error.2
  (eval '(>= "Hello"))
  type-error)

