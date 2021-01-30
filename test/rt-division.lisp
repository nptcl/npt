;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(load #p"test/rtnumbers.lisp")

(load #p"test/rtdivision-floor.lisp")
(load #p"test/rtdivision-ffloor.lisp")
(load #p"test/rtdivision-ceiling.lisp")
(load #p"test/rtdivision-fceiling.lisp")
(load #p"test/rtdivision-truncate.lisp")
(load #p"test/rtdivision-ftruncate.lisp")
(load #p"test/rtdivision-round1.lisp")
(load #p"test/rtdivision-roundf.lisp")
(load #p"test/rtdivision-roundb.lisp")
(load #p"test/rtdivision-roundr.lisp")
(load #p"test/rtdivision-rounds.lisp")
(load #p"test/rtdivision-fround1.lisp")
(load #p"test/rtdivision-froundf.lisp")
(load #p"test/rtdivision-froundb.lisp")
(load #p"test/rtdivision-froundr.lisp")
(load #p"test/rtdivision-frounds.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

