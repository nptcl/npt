;;
;;  ANSI COMMON LISP: 7. Objects
;;
(import 'lisp-system::closp)
(import 'lisp-clos::referenced-class)
(use-package 'lisp-clos)

(load #p"test/rtobjects-reader.lisp")
(load #p"test/rtobjects-class.lisp")
(load #p"test/rtobjects-generic.lisp")
(load #p"test/rtobjects-combination.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

