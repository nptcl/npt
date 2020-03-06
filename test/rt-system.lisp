;;
;;  ANSI COMMON LISP: 24. System Construction
;;
(load #p"test/rtsystem-fasl.lisp")
(load #p"test/rtsystem-compile.lisp")
(load #p"test/rtsystem-require.lisp")
(load #p"test/rtsystem-load.lisp")


;;
;;  load
;;
#+rt-degrade
(deftest *load-pathname*.1
  #.*load-pathname*
  nil)

#+rt-degrade
(deftest *load-truename*.1
  #.*load-truename*
  nil)

#-rt-degrade
(deftest *load-pathname*.1
  (let ((x #.*load-pathname*))
    (values
      (pathname-name x)
      (pathname-type x)))
  "rt-system" "lisp")

#-rt-degrade
(deftest *load-truename*.1
  (let ((x #.*load-truename*))
    (values
      (pathnamep x)
      (pathname-name x)
      (pathname-type x)))
  t "rt-system" "lisp")

(load #p"test/rtsystem-load1.lisp")

(setf (logical-pathname-translations "hello") '(("aaa;bbb;*.*" "test/")))
(load (parse-namestring "hello:aaa;bbb;rtsystem-load2.lisp"))


;;
;;  do-tests
;;
(do-tests :test t)

