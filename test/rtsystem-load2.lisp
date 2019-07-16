;;
;;  ANSI COMMON LISP: 24. System Construction
;;
(deftest *load-pathname*.3
  (let ((x #.*load-pathname*))
    (values
      (last (pathname-directory x))
      (pathname-name x)
      (pathname-type x)))
  ("bbb") "rtsystem-load2" "lisp")

(deftest *load-truename*.3
  (let ((x #.*load-truename*))
    (values
      (pathnamep x)
      (last (pathname-directory x))
      (pathname-name x)
      (pathname-type x)))
  t ("test") "rtsystem-load2" "lisp")

