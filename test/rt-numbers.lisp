;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(load #p"test/rtnumbers-include.lisp")

;;
;;  testcase
;;
(load #p"test/rtnumbers-type.lisp")
(load #p"test/rtnumbers-condition.lisp")
(load #p"test/rtnumbers-constant.lisp")
(load #p"test/rtnumbers-test.lisp")
(load #p"test/rtnumbers-equal.lisp")
(load #p"test/rtnumbers-plus.lisp")
(load #p"test/rtnumbers-multi.lisp")
(load #p"test/rtnumbers-mod.lisp")
(load #p"test/rtnumbers-sin.lisp")
(load #p"test/rtnumbers-exp.lisp")
(load #p"test/rtnumbers-decode.lisp")
(load #p"test/rtnumbers-bit.lisp")
(load #p"test/rtnumbers-byte.lisp")
(load #p"test/rtnumbers-isqrt.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

