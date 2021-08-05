;;
;;  ANSI COMMON LISP: 7. Objects
;;
(import 'lisp-system::closp)
(import 'lisp-system:sysctl)
(import 'lisp-clos::referenced-class)
(import 'lisp-clos:find-method-combination)
(use-package 'lisp-clos)

(load #p"test/rtobjects-slot.lisp")
(load #p"test/rtobjects-instance.lisp")
(load #p"test/rtobjects-defclass.lisp")
(load #p"test/rtobjects-class.lisp")
(load #p"test/rtobjects-defgeneric.lisp")
(load #p"test/rtobjects-generic.lisp")
(load #p"test/rtobjects-method.lisp")
(load #p"test/rtobjects-combination.lisp")
(load #p"test/rtobjects-redefine.lisp")

(load #p"test/rtmop-readers.lisp")
(load #p"test/rtmop-protocols.lisp")
(load #p"test/rtmop-misc.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

