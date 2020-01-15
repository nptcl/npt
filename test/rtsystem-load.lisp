;;
;;  ANSI COMMON LISP: 24. System Construction
;;
(deftest *load-print*.1
  (let ((*load-print-test* 'hello)
        (*load-verbose-test* 'hello))
    (declare (ignorable *load-print-test*))
    (declare (ignorable *load-verbose-test*))
    (load #p"test/rtsystem-file3.lisp")
    *load-print-test*)
  nil)

(deftest *load-print*.2
  (let ((*load-print-test* 'hello)
        (*load-verbose-test* 'hello))
    (declare (ignorable *load-print-test*))
    (declare (ignorable *load-verbose-test*))
    (load #p"test/rtsystem-file3.lisp" :print t)
    *load-print-test*)
  t)

(deftest *load-verbose*.1
  (let ((*load-print-test* 'hello)
        (*load-verbose-test* 'hello))
    (declare (ignorable *load-print-test*))
    (declare (ignorable *load-verbose-test*))
    (load #p"test/rtsystem-file3.lisp")
    *load-verbose-test*)
  nil)

(deftest *load-verbose*.2
  (let ((*load-print-test* 'hello)
        (*load-verbose-test* 'hello))
    (declare (ignorable *load-print-test*))
    (declare (ignorable *load-verbose-test*))
    (load #p"test/rtsystem-file3.lisp" :verbose t)
    *load-verbose-test*)
  t)

