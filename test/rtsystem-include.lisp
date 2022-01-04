;;
;;  ANSI COMMON LISP: 24. System Construction
;;
(defconstant +compile-file1+ #p"test/rtsystem-compile1.lisp")
(defconstant +compile-file2+ #p"test/rtsystem-compile2.lisp")

(defconstant +load-file1+ #p"test/rtsystem-load1.lisp")
(defconstant +load-file2+ #p"test/rtsystem-load2.lisp")

(defvar *compile-value* nil)
(defvar *load-value* nil)

