;;
;;  ANSI COMMON LISP: 11. Packages
;;
(make-package 'test1)
(make-package 'test2)
(make-package 'test3)

(load #p"test/rtpackages-object.lisp")
(load #p"test/rtpackages-package.lisp")
(load #p"test/rtpackages-defpackage.lisp")
(load #p"test/rtpackages-symbol.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

