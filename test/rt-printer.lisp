;;
;;  ANSI COMMON LISP: 22. Printer
;;
(defvar *default-print-dispatch* *print-pprint-dispatch*)
(defmacro with-default-print (&body body)
  `(let ((*print-array* t)
         (*print-base* 10)
         (*print-radix* nil)
         (*print-case* :upcase)
         (*print-circle* nil)
         (*print-escape* t)
         (*print-gensym* t)
         (*print-level* nil)
         (*print-length* nil)
         (*print-pprint-dispatch* *default-print-dispatch*)
         (*print-pretty* t)
         (*print-readably* nil)
         (*print-right-margin* nil)
         (*read-default-float-format* 'single-float))
     ,@body))

(load #p"test/rtprinter-format.lisp")
(load #p"test/rtprinter-function.lisp")
(load #p"test/rtprinter-write.lisp")
(load #p"test/rtprinter-sequence.lisp")
(load #p"test/rtprinter-object.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

