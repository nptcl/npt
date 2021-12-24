;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Function FLOOR
;;
(deftest floor.1
  (floor 3.25)
  3 0.25)

(deftest floor.2
  (floor 3.0 1.75)
  1 1.25)

(deftest floor.3
  (floor 0 100)
  0 0)

(deftest-error! floor-error.1
  (eval '(floor)))

(deftest-error! floor-error.2
  (eval '(floor #(2.0 0.0))))

(deftest-error! floor-error.3
  (eval '(floor 4 3 2)))

(deftest-error floor-error.4
  (floor 100 0)
  division-by-zero)

(deftest-error floor-error.5
  (floor (make-bignum 100) (make-bignum 0))
  division-by-zero)

(deftest-error floor-error.6
  (floor 2/3 (make-ratio 0 20))
  division-by-zero)

(deftest-error floor-error.7
  (floor 100.0f0 0.0f0)
  division-by-zero)

(deftest-error floor-error.8
  (floor 100.0d0 0.0d0)
  division-by-zero)

(deftest-error floor-error.9
  (floor 100.0L0 0.0L0)
  division-by-zero)


;;
;;  Function FFLOOR
;;
(deftest ffloor.1
  (ffloor 3.25)
  3.0 0.25)

(deftest ffloor.2
  (ffloor 3.0 1.75)
  1.0 1.25)

(deftest ffloor.3
  (ffloor 0 100)
  0.0 0)

(deftest-error! ffloor-error.1
  (eval '(ffloor)))

(deftest-error! ffloor-error.2
  (eval '(ffloor #(2.0 0.0))))

(deftest-error! ffloor-error.3
  (eval '(ffloor 4 3 2)))

(deftest-error ffloor-error.4
  (ffloor 100 0)
  division-by-zero)

(deftest-error ffloor-error.5
  (ffloor (make-bignum 100) (make-bignum 0))
  division-by-zero)

(deftest-error ffloor-error.6
  (ffloor 2/3 (make-ratio 0 20))
  division-by-zero)

(deftest-error ffloor-error.7
  (ffloor 100.0f0 0.0f0)
  division-by-zero)

(deftest-error ffloor-error.8
  (ffloor 100.0d0 0.0d0)
  division-by-zero)

(deftest-error ffloor-error.9
  (ffloor 100.0L0 0.0L0)
  division-by-zero)



;;
;;  Function CEILING
;;
(deftest ceiling.1
  (ceiling 3.25)
  4 -0.75)

(deftest ceiling.2
  (ceiling 3.0 1.75)
  2 -0.5)

(deftest ceiling.3
  (ceiling 0 100)
  0 0)

(deftest-error! ceiling-error.1
  (eval '(ceiling)))

(deftest-error! ceiling-error.2
  (eval '(ceiling #(2.0 0.0))))

(deftest-error! ceiling-error.3
  (eval '(ceiling 4 3 2)))

(deftest-error ceiling-error.4
  (ceiling 100 0)
  division-by-zero)

(deftest-error ceiling-error.5
  (ceiling (make-bignum 100) (make-bignum 0))
  division-by-zero)

(deftest-error ceiling-error.6
  (ceiling 2/3 (make-ratio 0 20))
  division-by-zero)

(deftest-error ceiling-error.7
  (ceiling 100.0f0 0.0f0)
  division-by-zero)

(deftest-error ceiling-error.8
  (ceiling 100.0d0 0.0d0)
  division-by-zero)

(deftest-error ceiling-error.9
  (ceiling 100.0L0 0.0L0)
  division-by-zero)


;;
;;  Function FCEILING
;;
(deftest fceiling.1
  (fceiling 3.25)
  4.0 -0.75)

(deftest fceiling.2
  (fceiling 3.0 1.75)
  2.0 -0.5)

(deftest fceiling.3
  (fceiling 0 100)
  0.0 0)

(deftest-error! fceiling-error.1
  (eval '(fceiling)))

(deftest-error! fceiling-error.2
  (eval '(fceiling #(2.0 0.0))))

(deftest-error! fceiling-error.3
  (eval '(fceiling 4 3 2)))

(deftest-error fceiling-error.4
  (fceiling 100 0)
  division-by-zero)

(deftest-error fceiling-error.5
  (fceiling (make-bignum 100) (make-bignum 0))
  division-by-zero)

(deftest-error fceiling-error.6
  (fceiling 2/3 (make-ratio 0 20))
  division-by-zero)

(deftest-error fceiling-error.7
  (fceiling 100.0f0 0.0f0)
  division-by-zero)

(deftest-error fceiling-error.8
  (fceiling 100.0d0 0.0d0)
  division-by-zero)

(deftest-error fceiling-error.9
  (fceiling 100.0L0 0.0L0)
  division-by-zero)


;;
;;  Function TRUNCATE
;;
(deftest truncate.1
  (truncate 3.25)
  3 0.25)

(deftest truncate.2
  (truncate 3.0 1.75)
  1 1.25)

(deftest truncate.3
  (truncate 0 100)
  0 0)

(deftest-error! truncate-error.1
  (eval '(truncate)))

(deftest-error! truncate-error.2
  (eval '(truncate #(2.0 0.0))))

(deftest-error! truncate-error.3
  (eval '(truncate 4 3 2)))

(deftest-error truncate-error.4
  (truncate 100 0)
  division-by-zero)

(deftest-error truncate-error.5
  (truncate (make-bignum 100) (make-bignum 0))
  division-by-zero)

(deftest-error truncate-error.6
  (truncate 2/3 (make-ratio 0 20))
  division-by-zero)

(deftest-error truncate-error.7
  (truncate 100.0f0 0.0f0)
  division-by-zero)

(deftest-error truncate-error.8
  (truncate 100.0d0 0.0d0)
  division-by-zero)

(deftest-error truncate-error.9
  (truncate 100.0L0 0.0L0)
  division-by-zero)


;;
;;  Function FTRUNCATE
;;
(deftest ftruncate.1
  (ftruncate 3.25)
  3.0 0.25)

(deftest ftruncate.2
  (ftruncate 3.0 1.75)
  1.0 1.25)

(deftest ftruncate.3
  (ftruncate 0 100)
  0.0 0)

(deftest-error! ftruncate-error.1
  (eval '(ftruncate)))

(deftest-error! ftruncate-error.2
  (eval '(ftruncate #(2.0 0.0))))

(deftest-error! ftruncate-error.3
  (eval '(ftruncate 4 3 2)))

(deftest-error ftruncate-error.4
  (ftruncate 100 0)
  division-by-zero)

(deftest-error ftruncate-error.5
  (ftruncate (make-bignum 100) (make-bignum 0))
  division-by-zero)

(deftest-error ftruncate-error.6
  (ftruncate 2/3 (make-ratio 0 20))
  division-by-zero)

(deftest-error ftruncate-error.7
  (ftruncate 100.0f0 0.0f0)
  division-by-zero)

(deftest-error ftruncate-error.8
  (ftruncate 100.0d0 0.0d0)
  division-by-zero)

(deftest-error ftruncate-error.9
  (ftruncate 100.0L0 0.0L0)
  division-by-zero)


;;
;;  Function ROUND
;;
(deftest round.1
  (round 3.25)
  3 0.25)

(deftest round.2
  (round 3.0 1.75)
  2 -0.5)

(deftest round.3
  (round 0 100)
  0 0)

(deftest-error! round-error.1
  (eval '(round)))

(deftest-error! round-error.2
  (eval '(round #(2.0 0.0))))

(deftest-error! round-error.3
  (eval '(round 4 3 2)))

(deftest-error round-error.4
  (round 100 0)
  division-by-zero)

(deftest-error round-error.5
  (round (make-bignum 100) (make-bignum 0))
  division-by-zero)

(deftest-error round-error.6
  (round 2/3 (make-ratio 0 20))
  division-by-zero)

(deftest-error round-error.7
  (round 100.0f0 0.0f0)
  division-by-zero)

(deftest-error round-error.8
  (round 100.0d0 0.0d0)
  division-by-zero)

(deftest-error round-error.9
  (round 100.0L0 0.0L0)
  division-by-zero)


;;
;;  Function FROUND
;;
(deftest fround.1
  (fround 3.25)
  3.0 0.25)

(deftest fround.2
  (fround 3.0 1.75)
  2.0 -0.5)

(deftest fround.3
  (fround 0 100)
  0.0 0)

(deftest-error! fround-error.1
  (eval '(fround)))

(deftest-error! fround-error.2
  (eval '(fround #(2.0 0.0))))

(deftest-error! fround-error.3
  (eval '(fround 4 3 2)))

(deftest-error fround-error.4
  (fround 100 0)
  division-by-zero)

(deftest-error fround-error.5
  (fround (make-bignum 100) (make-bignum 0))
  division-by-zero)

(deftest-error fround-error.6
  (fround 2/3 (make-ratio 0 20))
  division-by-zero)

(deftest-error fround-error.7
  (fround 100.0f0 0.0f0)
  division-by-zero)

(deftest-error fround-error.8
  (fround 100.0d0 0.0d0)
  division-by-zero)

(deftest-error fround-error.9
  (fround 100.0L0 0.0L0)
  division-by-zero)


;;
;;  ANSI Common Lisp
;;
(deftest division-test.1
  (floor 3/2)
  1 1/2)

(deftest division-test.2
  (ceiling 3 2)
  2 -1)

(deftest-float2 division-test.3
  (ffloor 3 2)
  1.0 1)

(deftest-float2 division-test.4
  (ffloor -4.7)
  -5.0 0.3)

(deftest-double2 division-test.5
  (ffloor 3.5d0)
  3.0d0 0.5d0)

(deftest division-test.6
  (fceiling 3/2)
  2.0 -1/2)

(deftest division-test.7
  (truncate 1)
  1 0)

(deftest division-test.8
  (truncate .5)
  0 0.5)

(deftest division-test.9
  (round .5)
  0 0.5)

(deftest division-test.10
  (ftruncate -7 2)
  -3.0 -1)

(deftest division-test.11
  (fround -7 2)
  -4.0 1)

(deftest division-test.12
  (let (list)
    (dolist (n '(2.6 2.5 2.4 0.7 0.3 -0.3 -0.7 -2.4 -2.5 -2.6))
      (push (format nil "~4,1@F ~2,' D ~2,' D ~2,' D ~2,' D"
                    n (floor n) (ceiling n) (truncate n) (round n))
            list))
    (nreverse list))
  ("+2.6  2  3  2  3"
   "+2.5  2  3  2  2"
   "+2.4  2  3  2  2"
   "+0.7  0  1  0  1"
   "+0.3  0  1  0  0"
   "-0.3 -1  0  0  0"
   "-0.7 -1  0  0 -1"
   "-2.4 -3 -2 -2 -2"
   "-2.5 -3 -2 -2 -2"
   "-2.6 -3 -2 -2 -3"))

(deftest division-test.13
  (floor 5 2)
  2 1)

(deftest division-test.14
  (floor (/ 5 2))
  2 1/2)

