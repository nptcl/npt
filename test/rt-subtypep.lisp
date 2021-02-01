;;
;;  typep / subtypep
;;
(unintern 'subtypep!)
(unintern 'include)
(unintern 'exclude)
(unintern 'invalid)
(unintern 'false)
(import 'lisp-system::subtypep!)
(import 'lisp-system::include)
(import 'lisp-system::exclude)
(import 'lisp-system::invalid)
(import 'lisp-system::false)

(load #p"test/rtsubtypep-typep1.lisp")
(load #p"test/rtsubtypep-typep2.lisp")
(load #p"test/rtsubtypep-typep3.lisp")
(load #p"test/rtsubtypep-typep4.lisp")
(load #p"test/rtsubtypep-subtypep.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

