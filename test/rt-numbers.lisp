;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(import 'lisp-system::make-bignum)
(import 'lisp-system::make-ratio)
(import 'lisp-system::fixnump)
(import 'lisp-system::bignump)
(import 'lisp-system::ratiop)

(defun equal-float2 (a1 b1 a2 b2 eps)
  (and (= a1 a2)
       (or (= b1 b2)
           (and (or (and (plusp b1) (plusp b2))
                    (and (minusp b1) (minusp b2)))
                (< (abs (- (abs b1) (abs b2))) eps)))))

(load #p"test/rtapp-number.lisp")
(load #p"test/rtapp-floor.lisp")
(load #p"test/rtapp-ffloor.lisp")
(load #p"test/rtapp-ceiling.lisp")
(load #p"test/rtapp-fceiling.lisp")
(load #p"test/rtapp-truncate.lisp")
(load #p"test/rtapp-ftruncate.lisp")
(load #p"test/rtapp-round.lisp")
(load #p"test/rtapp-fround.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

