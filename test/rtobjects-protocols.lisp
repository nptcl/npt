;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  make-method-lambda
;;
(defmacro method-lambda (args &body body)
  `(make-method-lambda
     (class-prototype (find-class 'standard-generic-function))
     (class-prototype (find-class 'standard-method))
     '(lambda ,args ,@body)
     nil))

(deftest make-method-lambda.1
  (listp
    (method-lambda (a) (1+ a)))
  t)

(deftest make-method-lambda.2
  (functionp
    (eval
      (method-lambda (a) (1+ a))))
  t)

(deftest make-method-lambda.3
  (funcall
    (eval
      (method-lambda (a) (1+ a)))
    nil nil 100)
  101)

