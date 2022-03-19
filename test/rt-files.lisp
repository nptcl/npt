;;
;;  ANSI COMMON LISP: 20. Files
;;
(load #p"test/rtmop-require.lisp")
(load #p"test/rtfiles-type.lisp")
#+ansi-c (load #p"test/rtfiles-ansi.lisp")
#-ansi-c (load #p"test/rtfiles-test.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

