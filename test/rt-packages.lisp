;;
;;  ANSI COMMON LISP: 11. Packages
;;
(load #p"test/rtpackages.lisp")
(load #p"test/rtpackages-object.lisp")
(load #p"test/rtpackages-package.lisp")
(load #p"test/rtpackages-defpackage.lisp")
(load #p"test/rtpackages-find.lisp")
(load #p"test/rtpackages-symbol.lisp")
(load #p"test/rtpackages-export.lisp")
(load #p"test/rtpackages-use.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

