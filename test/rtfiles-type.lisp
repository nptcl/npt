;;
;;  ANSI COMMON LISP: 20. Files
;;

;;
;;  Condition Type END-OF-FILE
;;
(deftest file-error-type.1
  (lisp-system:closp
    (find-class 'file-error))
  t)

(deftest file-error-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'file-error)))
  (file-error error serious-condition condition standard-object t))


;;
;;  Function FILE-ERROR-PATHNAME
;;
(deftest file-error-pathname.1
  (handler-case
    (error (make-condition 'file-error :pathname #p"hello.txt"))
    (file-error (c) (file-error-pathname c)))
  #p"hello.txt")

(deftest-error file-error-pathname-error.1
  (eval '(file-error-pathname 10))
  type-error)

(deftest-error! file-error-pathname-error.2
  (eval '(file-error-pathname)))

(deftest-error! file-error-pathname-error.3
  (eval '(file-error-pathname
           (make-condition 'file-error :pathname #p"hello.txt")
           nil)))

