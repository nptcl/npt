;;
;;  ANSI COMMON LISP: 24. System Construction
;;
(deftest *load-pathname*.5
  (let ((x #.*load-pathname*))
    (values
      (last (pathname-directory x))
      (pathname-name x)
      (pathname-type x)))
  ("bbb") "rtsystem-load2" "lisp")

(deftest *load-truename*.6
  (let ((x #.*load-truename*))
    (values
      (pathnamep x)
      (last (pathname-directory x))
      (pathname-name x)
      (pathname-type x)))
  t ("test") "rtsystem-load2" "lisp")

#+(or unix windows)
(deftest *load-truename*.7
  (let ((x #.*load-truename*))
    (car (pathname-directory x)))
  :absolute)

