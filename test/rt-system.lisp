;;
;;  ANSI COMMON LISP: 24. System Construction
;;
#+lisp-degrade
(deftest *load-pathname*.1
  #.*load-pathname*
  nil)

#+lisp-degrade
(deftest *load-truename*.1
  #.*load-truename*
  nil)

#-lisp-degrade
(deftest *load-pathname*.1
  (let ((x #.*load-pathname*))
    (values
      (pathname-name x)
      (pathname-type x)))
  "rt-system" "lisp")

#-lisp-degrade
(deftest *load-truename*.1
  (let ((x #.*load-truename*))
    (values
      (pathnamep x)
      (pathname-name x)
      (pathname-type x)))
  t "rt-system" "lisp")

(load #p"test/rtsystem-load1.lisp")

(progn
  (setf (logical-pathname-translations "hello") '(("aaa;bbb;*.*" "test/")))
  (load #p"hello:aaa;bbb;rtsystem-load2.lisp"))


;;
;;  do-tests
;;
(do-tests :test t)

