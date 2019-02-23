;;
;;  ANSI COMMON LISP: 22. Printer
;;
(deftest format.1
  (equal (format nil "~S" (copy-readtable))
         (format nil "~S" (copy-readtable)))
  nil)


;;
;;  load
;;
(load #p"test/rtapp-english.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

