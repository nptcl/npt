;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(load #p"test/rtnumbers.lisp")

;;
;;  testcase
;;
(load #p"test/rtnumbers-test.lisp")
(load #p"test/rtnumbers-equal.lisp")
(load #p"test/rtnumbers-plus.lisp")
(load #p"test/rtnumbers-multi.lisp")
(load #p"test/rtnumbers-floor.lisp")
(load #p"test/rtnumbers-ffloor.lisp")
(load #p"test/rtnumbers-ceiling.lisp")
(load #p"test/rtnumbers-fceiling.lisp")
(load #p"test/rtnumbers-truncate.lisp")
(load #p"test/rtnumbers-ftruncate.lisp")
(load #p"test/rtnumbers-round1.lisp")
(load #p"test/rtnumbers-roundf.lisp")
(load #p"test/rtnumbers-roundb.lisp")
(load #p"test/rtnumbers-roundr.lisp")
(load #p"test/rtnumbers-rounds.lisp")
(load #p"test/rtnumbers-fround1.lisp")
(load #p"test/rtnumbers-froundf.lisp")
(load #p"test/rtnumbers-froundb.lisp")
(load #p"test/rtnumbers-froundr.lisp")
(load #p"test/rtnumbers-frounds.lisp")
(load #p"test/rtnumbers-mod.lisp")
(load #p"test/rtnumbers-sin.lisp")
(load #p"test/rtnumbers-exp.lisp")
(load #p"test/rtnumbers-decode.lisp")
(load #p"test/rtnumbers-bit.lisp")
(load #p"test/rtnumbers-byte.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

