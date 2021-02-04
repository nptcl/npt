;;
;;  typep / subtypep
;;
(unintern 'subtypep!)
(unintern 'include)
(unintern 'exclude)
(unintern 'invalid)
(unintern 'false)
(import 'lisp-system::subtypep!)
(import 'lisp-system::*subtypep!*)
(import 'lisp-system::subtypep-atomic)
(import 'lisp-system::subtypep-atomic-not)
(import 'lisp-system::subtypep-compound)
(import 'lisp-system::subtypep-force-number)
(import 'lisp-system::subtypep-normal)
(import 'lisp-system::include)
(import 'lisp-system::exclude)
(import 'lisp-system::invalid)
(import 'lisp-system::false)

(load #p"test/rtsubtypep-typep1.lisp")
(load #p"test/rtsubtypep-typep2.lisp")
(load #p"test/rtsubtypep-typep3.lisp")
(load #p"test/rtsubtypep-typep4.lisp")
(load #p"test/rtsubtypep-subtypep1.lisp")
(load #p"test/rtsubtypep-subtypep2.lisp")
(load #p"test/rtsubtypep-subtypep3.lisp")
(load #p"test/rtsubtypep-subtypep4.lisp")
(load #p"test/rtsubtypep-subtypep5.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

