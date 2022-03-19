;;
;;  ANSI COMMON LISP: 4. Types and Classes
;;
(import 'lisp-system::array-general-p)
(import 'lisp-system::array-specialized-p)

(load #p"test/rtnumbers-include.lisp")
(load #p"test/rtmop-require.lisp")
(load #p"test/rttypes-type.lisp")
(load #p"test/rttypes-specifier.lisp")
(load #p"test/rttypes-test.lisp")
(load #p"test/rttypes-coerce.lisp")
(load #p"test/rttypes-array.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

