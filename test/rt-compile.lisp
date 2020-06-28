;;
;;  compile-file
;;
(load #p"test/rtcompile-file.lisp")
(load #p"test/rtcompile-value.lisp")
(load #p"test/rtcompile-array.lisp")
(load #p"test/rtcompile-type.lisp")
(load #p"test/rtcompile-clos.lisp")
(load #p"test/rtcompile-code.lisp")
(load #p"test/rtcompile-toplevel.lisp")
(load #p"test/rtcompile-define.lisp")


;;
;;  delete-file
;;
(deftest compile-eval-delete.1
  (progn
    (test-compile-delete)
    (values)))


;;
;;  do-tests
;;
(do-tests :test t)

