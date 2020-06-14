;;
;;  compile-file
;;
(load #p"test/rtcompile-file.lisp")
(load #p"test/rtcompile-value.lisp")
(load #p"test/rtcompile-array.lisp")
(load #p"test/rtcompile-type.lisp")
(load #p"test/rtcompile-code.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

