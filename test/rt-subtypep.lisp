;;
;;  typep / subtypep
;;
(unintern 'lisp-system::subtypep-result)
(unintern 'include)
(unintern 'exclude)
(unintern 'invalid)
(unintern 'false)
(import 'lisp-system::subtypep-result)
(import 'lisp-system::include)
(import 'lisp-system::exclude)
(import 'lisp-system::invalid)
(import 'lisp-system::false)

(load #p"test/rtsubtypep-typep.lisp")
(load #p"test/rtsubtypep-test.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

