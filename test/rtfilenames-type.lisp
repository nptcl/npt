;;
;;  ANSI COMMON LISP: 19. Filenames
;;

;;
;;  System Class PATHNAME
;;
(deftest pathname-class.1
  (lisp-system:closp
    (find-class 'pathname))
  t)

(deftest pathname-class.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'pathname)))
  (pathname t))

(deftest pathname-class.3
  (typep #p"Hello.txt" 'pathname)
  t)

(deftest pathname-class.4
  (typep
    (logical-pathname "test:aaa;bbb;Hello.txt")
    'pathname)
  t)

(deftest pathname-class.5
  (typep 100 'pathname)
  nil)


;;
;;  System Class LOGICAL-PATHNAME
;;
(deftest logical-pathname-class.1
  (lisp-system:closp
    (find-class 'logical-pathname))
  t)

(deftest logical-pathname-class.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'logical-pathname)))
  (logical-pathname pathname t))

(deftest logical-pathname-class.3
  (typep #p"Hello.txt" 'logical-pathname)
  nil)

(deftest logical-pathname-class.4
  (typep
    (logical-pathname "test:aaa;bbb;Hello.txt")
    'logical-pathname)
  t)

(deftest logical-pathname-class.5
  (typep 100 'logical-pathname)
  nil)


;;
;;  Function PATHNAMEP
;;
(deftest pathnamep.1
  (pathnamep #p"hello.txt")
  t)

(deftest pathnamep.2
  (pathnamep
    (logical-pathname "test:aaa;bbb;Hello.txt"))
  t)

(deftest pathnamep.3
  (pathnamep t)
  nil)

(deftest pathnamep.4
  (pathnamep "hello.txt")
  nil)

(deftest-error! pathnamep-error.1
  (eval '(pathname)))

(deftest-error! pathnamep-error.2
  (eval '(pathname t t)))

;;  ANSI Common Lisp
(deftest pathnamep-test.1
  (pathnamep "test")
  nil)

(deftest pathnamep-test.2
  (pathnamep (pathname "test"))
  t)

(deftest pathnamep-test.3
  (pathnamep
    (logical-pathname "test:aaa;bbb;Hello.txt"))
  t)

