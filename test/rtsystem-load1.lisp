;;
;;  ANSI COMMON LISP: 24. System Construction
;;
(deftest *load-pathname*.2
  (let ((x #.*load-pathname*))
    (values
      (last (pathname-directory x))
      (pathname-name x)
      (pathname-type x)))
  ("test") "rtsystem-load1" "lisp")

(deftest *load-truename*.2
  (let ((x #.*load-truename*))
    (values
      (pathnamep x)
      (last (pathname-directory x))
      (pathname-name x)
      (pathname-type x)))
  t ("test") "rtsystem-load1" "lisp")

