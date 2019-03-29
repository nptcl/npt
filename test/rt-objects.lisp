;;
;;  ANSI COMMON LISP: 7. Objects
;;
(deftest find-class.1
  (null
    (find-class 'standard-class nil))
  nil)

(deftest find-class.2
  (find-class 'no-such-class-name nil)
  nil)

(deftest class-name.1
  (class-name
    (find-class 'standard-class))
  standard-class)

(deftest class-name.2
  (class-name
    (find-class 'string))
  string)


;;
;;  do-tests
;;
(do-tests :test t)

