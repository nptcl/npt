;;
;;  ANSI COMMON LISP: 12. Numbers
;;

(deftest complex1.1
  (complex 10)
  10)

(deftest complex1.2
  (complex -20000000000000000000000000)
  -20000000000000000000000000)

(deftest complex1.3
  (complex 2/3)
  2/3)

(deftest complex1.4
  (complex 12.3f0)
  #c(12.3f0 0.0f0))

(deftest complex1.5
  (complex -1.0d0)
  #c(-1.0d0 0.0d0))

(deftest complex1.6
  (complex 11.0l2)
  #c(11.0l2 0.0l0))

(deftest complex2.1
  (complex 10 20)
  #c(10 20))

(deftest complex2.2
  (complex 10 0)
  10)

(deftest complex2.3
  (complex 999999999999999999999999999999999999999 20)
  #c(999999999999999999999999999999999999999 20))

(deftest complex2.4
  (complex 999999999999999999999999999999999999999 0)
  999999999999999999999999999999999999999)

(deftest complex2.5
  (complex 2/3 4/5)
  #c(2/3 4/5))

(deftest complex2.6
  (complex 2/3 0)
  2/3)

(deftest complex2.7
  (complex 1.0f0 2.0f0)
  #c(1.0f0 2.0f0))

(deftest complex2.8
  (complex 1.0f0 0)
  #c(1.0f0 0.0f0))

(deftest complex2.9
  (complex 1.0d0 2.0d0)
  #c(1.0d0 2.0d0))

(deftest complex2.10
  (complex 1.0d0 0)
  #c(1.0d0 0.0d0))

(deftest complex2.11
  (complex 1.0L0 2.0L0)
  #c(1.0L0 2.0L0))

(deftest complex2.12
  (complex 1.0L0 0)
  #c(1.0L0 0.0L0))

(deftest conjugate.1
  (conjugate 10)
  10)

(deftest conjugate.2
  (conjugate #c(10 -20))
  #c(10 20))

(deftest conjugate.3
  (conjugate #c(1.2 3.4))
  #c(1.2 -3.4))

(deftest phase.1
  (phase 0)
  0.0f0)

(deftest phase.2
  (phase 3/4)
  0.0f0)

(deftest phase.3
  (phase 1.2f0)
  0.0f0)

(deftest phase.4
  (phase -34.5d4)
  0.0d0)

(deftest-single phase.5
  (phase (cis 30))
  -1.4159266f0)

(deftest-single phase.6
  (phase #c(0 1))
  1.5707964f0)

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

(deftest upgraded-complex-part-type.1
  (upgraded-complex-part-type 'integer)
  integer)

(deftest upgraded-complex-part-type.2
  (upgraded-complex-part-type '(rational 2/3 4/5))
  rational)

(deftest upgraded-complex-part-type.3
  (upgraded-complex-part-type 'float)
  single-float)

