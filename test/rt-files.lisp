;;
;;  ANSI COMMON LISP: 20. Files
;;
#+ansi-c
(load #p"test/rtfiles-ansi.lisp")
#-ansi-c
(load #p"test/rtfiles-test.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

