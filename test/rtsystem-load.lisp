;;
;;  ANSI COMMON LISP: 24. System Construction
;;
(defvar *load-print-test*)
(defvar *load-verbose-test*)

(deftest *load-print*.1
  (let ((*load-print-test* 'hello)
        (*load-verbose-test* 'hello))
    (declare (ignorable *load-print-test*))
    (declare (ignorable *load-verbose-test*))
    (with-output-to-string (*standard-output*)
      (load #p"test/rtsystem-file4.lisp"))
    *load-print-test*)
  nil)

(deftest *load-print*.2
  (let ((*load-print-test* 'hello)
        (*load-verbose-test* 'hello))
    (declare (ignorable *load-print-test*))
    (declare (ignorable *load-verbose-test*))
    (with-output-to-string (*standard-output*)
      (load #p"test/rtsystem-file4.lisp" :print t))
    *load-print-test*)
  t)

(deftest *load-verbose*.1
  (let ((*load-print-test* 'hello)
        (*load-verbose-test* 'hello))
    (declare (ignorable *load-print-test*))
    (declare (ignorable *load-verbose-test*))
    (with-output-to-string (*standard-output*)
      (load #p"test/rtsystem-file4.lisp"))
    *load-verbose-test*)
  nil)

(deftest *load-verbose*.2
  (let ((*load-print-test* 'hello)
        (*load-verbose-test* 'hello))
    (declare (ignorable *load-print-test*))
    (declare (ignorable *load-verbose-test*))
    (with-output-to-string (*standard-output*)
      (load #p"test/rtsystem-file4.lisp" :verbose t))
    *load-verbose-test*)
  t)

