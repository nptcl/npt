;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(load #p"test/rtapp-float.lisp")

;;
;;  testcase
;;
(load #p"test/rtapp-number-equal.lisp")
(load #p"test/rtapp-number.lisp")
(load #p"test/rtapp-floor.lisp")
(load #p"test/rtapp-ffloor.lisp")
(load #p"test/rtapp-ceiling.lisp")
(load #p"test/rtapp-fceiling.lisp")
(load #p"test/rtapp-truncate.lisp")
(load #p"test/rtapp-ftruncate.lisp")
(load #p"test/rtapp-round.lisp")
(load #p"test/rtapp-fround.lisp")
(load #p"test/rtapp-mod.lisp")
(load #p"test/rtapp-sin.lisp")
(load #p"test/rtapp-exp.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

