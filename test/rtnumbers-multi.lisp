;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  *
;;
(deftest *.1
  (*)
  1)

(deftest *.2
  (* 10)
  10)

(deftest *.3
  (* 10 20 30)
  6000)

(deftest *.4
  (* -10 20)
  -200)

(deftest *.5
  (* 0.1 -2.3)
  -0.23)

(deftest *.6
  (* 1 2 3 4 555555 6 0 7 9)
  0)

(deftest *f.1
  (* 11 22)
  242)

(deftest *f.2
  (* 11 (make-bignum -22))
  -242)

(deftest *f.3
  (* 6 7/12)
  7/2)

(deftest-single *f.4
  (* -7 2.3f0)
  -16.1f0)

(deftest-double *f.5
  (* -10 -2.1d0)
  21.0d0)

(deftest-long *f.6
  (* 11 1.2L-2)
  0.132L0)

(deftest *f.7
  (* 12 #c(4 5))
  #c(48 60))

(deftest-double *f.8
  (* 12 #c(1.2d0 3.4d0))
  14.4d0 40.8d0)

(deftest *b.1
  (* (make-bignum 11) 22)
  242)

(deftest *b.2
  (* (make-bignum 11) (make-bignum -22))
  -242)

(deftest *b.3
  (* (make-bignum 6) 7/12)
  7/2)

(deftest-single *b.4
  (* (make-bignum -7) 2.3f0)
  -16.1f0)

(deftest-double *b.5
  (* (make-bignum -10) -2.1d0)
  21.0d0)

(deftest-long *b.6
  (* (make-bignum 11) 1.2L-2)
  0.132L0)

(deftest *b.7
  (* (make-bignum 12) #c(4 5))
  #c(48 60))

(deftest-double *b.8
  (* (make-bignum 12) #c(1.2d0 3.4d0))
  14.4d0 40.8d0)

(deftest *r.1
  (* -4/5 22)
  -88/5)

(deftest *r.2
  (* 4/5 (make-bignum 22))
  88/5)

(deftest *r.3
  (* 6/2222 7/12)
  7/4444)

(deftest-single *r.4
  (* -7/8 2.3f0)
  -2.0125f0)

(deftest-double *r.5
  (* -10/301 -2.1d0)
  0.06976744186046513d0)

(deftest-long *r.6
  (* 11/13 1.2L-2)
  0.010153846153846153846L0)

(deftest *r.7
  (* 12/25 #c(4 5))
  #c(48/25 12/5))

(deftest-double *r.8
  (* 12/15 #c(1.2d0 3.4d0))
  0.96d0 2.72d0)

(deftest-single *s.1
  (* -4.5f0 22)
  -99.0f0)

(deftest-single *s.2
  (* 4.5f0 (make-bignum 22))
  99.0f0)

(deftest-single *s.3
  (* 6.2222f0 7/12)
  3.6296165f0)

(deftest-single *s.4
  (* -7.8f0 2.3f0)
  -17.94f0)

(deftest-double *s.5
  (* -10.301f0 -2.1d0)
  21.63209924697876d0)

(deftest-long *s.6
  (* 11.13f0 1.2L-2)
  0.13356000137329102d0)

(deftest-single *s.7
  (* -12.25f0 #c(4 5))
  -49.0f0 -61.25f0)

(deftest-double *s.8
  (* 12.15f0 #c(1.2d0 3.4d0))
  14.579999542236328d0 41.309998703002925d0)

(deftest-double *d.1
  (* -4.5d0 22)
  -99.0d0)

(deftest-double *d.2
  (* 4.5d0 (make-bignum 22))
  99.0d0)

(deftest-double *d.3
  (* 6.2222d0 7/12)
  3.6296166666666667d0)

(deftest-double *d.4
  (* -7.8d0 2.3f0)
  -17.939999628067017d0)

(deftest-double *d.5
  (* -10.301d0 -2.1d0)
  21.6321d0)

(deftest-long *d.6
  (* 11.13d0 1.2L-2)
  0.13356d0)

(deftest-double *d.7
  (* -12.25d0 #c(4 5))
  -49.0d0 -61.25d0)

(deftest-double *d.8
  (* 12.15d0 #c(1.2d0 3.4d0))
  14.58d0 41.31d0)

(deftest-long *l.1
  (* -4.5L0 22)
  -99.0L0)

(deftest-long *l.2
  (* 4.5L0 (make-bignum 22))
  99.0L0)

(deftest-long *l.3
  (* 6.2222L0 7/12)
  3.6296166666666667L0)

(deftest-long *l.4
  (* -7.8L0 2.3f0)
  -17.939999628067017L0)

(deftest-long *l.5
  (* -10.301L0 -2.1d0)
  21.6321L0)

(deftest-long *l.6
  (* 11.13L0 1.2L-2)
  0.13356L0)

(deftest-long *l.7
  (* -12.25L0 #c(4 5))
  -49.0L0 -61.25L0)

(deftest-long *l.8
  (* 12.15L0 #c(1.2d0 3.4d0))
  14.58L0 41.31L0)

(deftest *c.1
  (* #c(2 -3) 22)
  #c(44 -66))

(deftest-single *c.2
  (* #c(2.2f0 -3.9f1) 22)
  48.4f0 -858.0f0)

(deftest *c.3
  (* #c(2 -3) (make-bignum -22))
  #c(-44 66))

(deftest-single *c.4
  (* #c(2.2f0 3.9f1) (make-bignum 22))
  48.4f0 858.0f0)

(deftest *c.5
  (* #c(4 5) 31/51)
  #c(124/51 155/51))

(deftest-double *c.6
  (* #c(-4.3d0 5.0d0) 31/51)
  -2.613725490196078d0 3.0392156862745097d0)

(deftest-single *c.7
  (* #c(11 -7/3) 12.3f0)
  135.3f0 -28.699999f0)

(deftest-double *c.8
  (* #c(-12.3d0 3.4d0) 12.3f0)
  -151.29000234603882d0 41.820000648498535d0)

(deftest-double *c.9
  (* #c(2 3) 4.5d-1)
  0.9d0 1.35d0)

(deftest-double *c.10
  (* #c(12.3f0 3.4f0) 4.5d0)
  55.35d0 15.3d0 1e-6)

(deftest-long *c.11
  (* #c(2/7 3) 4.5L0)
  1.2857142857142856d0 13.5d0)

(deftest-long *c.12
  (* #c(2.7d0 3.0d0) 4.5L0)
  12.15d0 13.5d0)

(deftest *c.13
  (* #c(2 3) #c(-4 5))
  #c(-23 -2))

(deftest *c.14
  (* #c(5/6 2/101) #c(45/67 111/222))
  #c(7441/13534 34915/81204))

(deftest-double *c.15
  (* #c(2 3) #c(-4.5d0 2.3d1))
  -78.0d0 32.5d0)

(deftest-single *c.16
  (* #c(5/6 2/101) #c(45.67f0 111.222f0))
  35.855915 93.589355)

(deftest-single *c.17
  (* #c(2.2f0 3.4f0) #c(-4 5))
  -25.8f0 -2.6f0 1e-4)

(deftest-double *c.18
  (* #c(5.6d0 2.101d0) #c(45/67 111/222))
  2.7106940298507465d0 4.211119402985075d0)

(deftest-double *c.19
  (* #c(-2.3d0 -3.4d0) #c(-4.5d0 2.3d1))
  88.55d0 -37.6d0)

(deftest-long *c.20
  (* #c(5.6L0 2.101L0) #c(45.67d0 111.222d0))
  22.074578000000002d0 718.7958699999999d0 1e-10)


;;
;;  /
;;
(deftest /.1
  (/ 1)
  1)

(deftest /.2
  (/ 10)
  1/10)

(deftest /.3
  (/ 10 20 30)
  1/60)

(deftest /.4
  (/ -10 20)
  -1/2)

(deftest-single /.5
  (/ 0.1f0 -2.3f0)
  -0.04347826f0)

(deftest /.6
  (/ 1 2 3 4 555555 6 7 9)
  1/5039994960)

(deftest-error /.7
  (/ 0)
  division-by-zero)

(deftest /.8
  (/ 0 2)
  0)

(deftest-error /.9
  (/ 1 0)
  division-by-zero)

(deftest-error /.10
  (/ 1 4 5 0 2 3)
  division-by-zero)

(deftest /.11
  (/ -10 -24)
  5/12)

(deftest /f.1
  (/ 11 22)
  11/22)

(deftest /f.2
  (/ 11 (make-bignum -22))
  -11/22)

(deftest /f.3
  (/ 14 7/12)
  24)

(deftest-single /f.4
  (/ -7 2.3f0)
  -3.0434783f0)

(deftest-double /f.5
  (/ -10 -2.1d0)
  4.761904761904762d0)

(deftest-long /f.6
  (/ 11 1.2L-2)
  916.6666666666666667L0 0.0L0 1e-10)

(deftest /f.7
  (/ 12 #c(4 5))
  #c(48/41 -60/41))

(deftest-double /f.8
  (/ 12 #c(1.2d0 3.4d0))
  1.1076923076923078d0 -3.138461538461539d0)

(deftest /b.1
  (/ (make-bignum 11) 22)
  11/22)

(deftest /b.2
  (/ (make-bignum 11) (make-bignum -22))
  -11/22)

(deftest /b.3
  (/ (make-bignum 6) 7/12)
  72/7)

(deftest-single /b.4
  (/ (make-bignum -7) 2.3f0)
  -3.0434783f0)

(deftest-double /b.5
  (/ (make-bignum -10) -2.1d0)
  4.761904761904762d0)

(deftest-long /b.6
  (/ (make-bignum 11) 1.2L-2)
  916.6666666666666667L0 0.0L0 1e-10)

(deftest /b.7
  (/ (make-bignum 12) #c(4 5))
  #c(48/41 -60/41))

(deftest-double /b.8
  (/ (make-bignum 12) #c(1.2d0 3.4d0))
  1.1076923076923078d0 -3.138461538461539d0)

(deftest /r.1
  (/ -4/5 22)
  -2/55)

(deftest /r.2
  (/ 4/5 (make-bignum 22))
  2/55)

(deftest /r.3
  (/ 6/2222 7/12)
  36/7777)

(deftest-single /r.4
  (/ -7/8 2.3f0)
  -0.38043478f0)

(deftest-double /r.5
  (/ -10/301 -2.1d0)
  0.0158202816010125d0)

(deftest-long /r.6
  (/ 11/13 1.2L-2)
  70.512820512820512824L0)

(deftest /r.7
  (/ 12/25 #c(4 5))
  #C(48/1025 -12/205))

(deftest-double /r.8
  (/ 12/15 #c(1.2d0 3.4d0))
  0.07384615384615385d0 -0.20923076923076925d0)

(deftest-single /s.1
  (/ -4.5f0 22)
  -0.20454545f0)

(deftest-single /s.2
  (/ 4.5f0 (make-bignum 22))
  0.20454545f0)

(deftest-single /s.3
  (/ 6.2222f0 7/12)
  10.666629f0)

(deftest-single /s.4
  (/ -7.8f0 2.3f0)
  -3.3913045f0)

(deftest-double /s.5
  (/ -10.301f0 -2.1d0)
  4.905238f0 0.0f0 1e-6)

(deftest-long /s.6
  (/ 11.13f0 1.2L-2)
  927.5L0 0.0L0 1e-5)

(deftest-single /s.7
  (/ -12.25f0 #c(4 5))
  -1.1951219 1.4939024)

(deftest-double /s.8
  (/ 12.15f0 #c(1.2d0 3.4d0))
  1.1215384 -3.1776922 1e-6)

(deftest-double /d.1
  (/ -4.5d0 22)
  -0.20454545454545456d0)

(deftest-double /d.2
  (/ 4.5d0 (make-bignum 22))
  0.20454545454545456d0)

(deftest-double /d.3
  (/ 6.2222d0 7/12)
  10.666628571428571d0)

(deftest-double /d.4
  (/ -7.8d0 2.3f0)
  -3.391304418134781d0)

(deftest-double /d.5
  (/ -10.301d0 -2.1d0)
  4.905238095238095d0)

(deftest-long /d.6
  (/ 11.13d0 1.2L-2)
  927.5d0 0.0d0 1e-10)

(deftest-double /d.7
  (/ -12.25d0 #c(4 5))
  -1.1951219512195124d0 1.4939024390243905d0)

(deftest-double /d.8
  (/ 12.15d0 #c(1.2d0 3.4d0))
  1.1215384615384616d0 -3.1776923076923076d0)

(deftest-long /l.1
  (/ -4.5L0 22)
  -0.20454545454545456L0)

(deftest-long /l.2
  (/ 4.5L0 (make-bignum 22))
  0.20454545454545456L0)

(deftest-long /l.3
  (/ 6.2222L0 7/12)
  10.666628571428571L0)

(deftest-long /l.4
  (/ -7.8L0 2.3f0)
  -3.391304418134781L0)

(deftest-long /l.5
  (/ -10.301L0 -2.1d0)
  4.905238095238095L0)

(deftest-long /l.6
  (/ 11.13L0 1.2L-2)
  927.5L0 0.0L0 1e-10)

(deftest-long /l.7
  (/ -12.25L0 #c(4 5))
  -1.1951219512195124L0 1.4939024390243905L0)

(deftest-long /l.8
  (/ 12.15L0 #c(1.2d0 3.4d0))
  1.1215384615384616L0 -3.1776923076923076L0)

(deftest /c.1
  (/ #c(2 -3) 22)
  #c(1/11 -3/22))

(deftest-single /c.2
  (/ #c(2.2f0 -3.9f1) 22)
  0.1f0 -1.7727273f0)

(deftest /c.3
  (/ #c(2 -3) (make-bignum -22))
  #c(-1/11 3/22))

(deftest-single /c.4
  (/ #c(2.2f0 3.9f1) (make-bignum 22))
  0.1 1.7727273)

(deftest /c.5
  (/ #c(4 5) 31/51)
  #c(204/31 255/31))

(deftest-double /c.6
  (/ #c(-4.3d0 5.0d0) 31/51)
  -7.074193548387097d0 8.225806451612904d0)

(deftest-single /c.7
  (/ #c(11 -7/3) 12.3f0)
  0.8943089f0 -0.18970188f0)

(deftest-double /c.8
  (/ #c(-12.3d0 3.4d0) 12.3f0)
  -0.9999999844931008d0 0.2764227599411823d0)

(deftest-double /c.9
  (/ #c(2 3) 4.5d-1)
  4.444444444444445d0 6.666666666666666d0)

(deftest-double /c.10
  (/ #c(12.3f0 3.4f0) 4.5d0)
  2.7333333757188587d0 0.7555555767483182d0)

(deftest-long /c.11
  (/ #c(2/7 3) 4.5L0)
  0.06349206349206349d0 0.6666666666666666d0)

(deftest-long /c.12
  (/ #c(2.7d0 3.0d0) 4.5L0)
  0.6d0 0.6666666666666666d0)

(deftest /c.13
  (/ #c(2 3) #c(-4 5))
  #c(7/41 -22/41))

(deftest /c.14
  (/ #c(5/6 2/101) #c(45/67 111/222))
  #c(1033006/1271489 -2194585/3814467))

(deftest-double /c.15
  (/ #c(2 3) #c(-4.5d0 2.3d1))
  0.10923987255348203d0 -0.108329540282203d0)

(deftest-single /c.16
  (/ #c(5/6 2/101) #c(45.67f0 111.222f0))
  0.0027850384f0 -0.0063489294f0)

(deftest-single /c.17
  (/ #c(2.2f0 3.4f0) #c(-4 5))
  0.2f0 -0.6f0)

(deftest-double /c.18
  (/ #c(5.6d0 2.101d0) #c(45/67 111/222))
  6.863037413615061d0 -1.980994519024545d0)

(deftest-double /c.19
  (/ #c(-2.3d0 -3.4d0) #c(-4.5d0 2.3d1))
  -0.12353208921256258d0 0.12416932180245789d0)

(deftest-long /c.20
  (/ #c(5.6L0 2.101L0) #c(45.67d0 111.222d0))
  0.033856297700195756d0 -0.03644767118045045d0)

