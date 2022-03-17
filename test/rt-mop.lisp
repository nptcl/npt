;;
;;  MetaObject Protocol
;;
(import 'lisp-system::closp)
(import 'lisp-system:sysctl)
(import 'lisp-clos::referenced-class)
(import 'lisp-clos:find-method-combination)
(import 'lisp-clos::method-combination-instance)
(use-package 'lisp-clos)

(load #p"test/rtmop-class.lisp")
(load #p"test/rtmop-readers.lisp")
(load #p"test/rtmop-protocols.lisp")
(load #p"test/rtmop-misc.lisp")


;;
;;  do-tests
;;
(do-tests :test t)


