;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  Macro DEFMETHOD
;;

;;
;;  defmethod
;;
(defgeneric defmethod-1 ())
(defmethod defmethod-1 ()
  :hello)

(deftest defmethod.1
  (defmethod-1)
  :hello)

(defgeneric defmethod-2 ())
(deftest defmethod.2
  (closp
    (defmethod defmethod-2 ()))
  t)

(defgeneric defmethod-3 (a))
(defmethod defmethod-3 (a)
  (+ a 10))

(deftest defmethod.3
  (defmethod-3 111)
  121)

(defgeneric defmethod-4 (a))
(defmethod defmethod-4 (a)
  (+ a 10))

(defmethod defmethod-4 ((a string))
  (concatenate 'string "abc" a))

(deftest defmethod.4
  (defmethod-4 100)
  110)

(deftest defmethod.5
  (defmethod-4 "def")
  "abcdef")

(defgeneric defmethod-6 (a))
(defmethod defmethod-6 (a)
  (+ a 10))

(defmethod defmethod-6 ((a string))
  (+ (length a) (call-next-method 1000)))

(deftest defmethod.6
  (defmethod-6 30)
  40)

(deftest defmethod.7
  (defmethod-6 "Hello")
  1015)

(defgeneric defmethod-8 (a))
(defmethod defmethod-8 (a)
  (declare (ignore a))
  (next-method-p))

(defmethod defmethod-8 ((a string))
  (declare (ignore a))
  (next-method-p))

(deftest defmethod.8
  (defmethod-8 30)
  nil)

(deftest defmethod.9
  (defmethod-8 "Hello")
  t)

(deftest defmethod.10
  (progn
    (defgeneric defmethod-10 ())
    (typep
      (defmethod defmethod-10 ())
      'standard-method))
  t)

(deftest defmethod-setf.1
  (progn
    (defgeneric (setf defmethod-setf-1) ())
    (typep
      (defmethod (setf defmethod-setf-1) ())
      'standard-method))
  t)

(defvar *defmethod-qualifier*)
(deftest defmethod-qualifier.1
  (progn
    (defgeneric defmethod-qualifier-1 ())
    (defmethod defmethod-qualifier-1 :before ()
      (push :aaa *defmethod-qualifier*)
      10)
    (defmethod defmethod-qualifier-1 ()
      (push :bbb *defmethod-qualifier*)
      20)
    (let (*defmethod-qualifier*)
      (values
        (defmethod-qualifier-1)
        (nreverse *defmethod-qualifier*))))
  20 (:aaa :bbb))

(deftest defmethod-declaration.1
  (progn
    (defgeneric defmethod-declaration-1 (x))
    (defmethod defmethod-declaration-1 (x)
      (declare (special x))
      x)
    (defmethod-declaration-1 100))
  100)

(deftest defmethod-documentation.1
  (progn
    (defgeneric defmethod-documentation-1 (x))
    (documentation
      (defmethod defmethod-documentation-1 (x)
        (declare (ignore x))
        "Hello"
        (+ 10 20 30))
      't))
  "Hello")


;;
;;  new generic
;;
(deftest defmethod-new.1
  (progn
    (fmakunbound 'defmethod-new-1)
    (defmethod defmethod-new-1 (x y)
      (+ 100 x y))
    (defmethod-new-1 11 22))
  133)

(deftest defmethod-new.2
  (typep
    #'defmethod-new-1
    'standard-generic-function)
  t)

(deftest defmethod-new.3
  (class-name
    (generic-function-method-class
      #'defmethod-new-1))
  standard-method)

(deftest defmethod-new.4
  (slot-value
    (generic-function-method-combination #'defmethod-new-1)
    'lisp-clos::name)
  standard)

(defun defmethod-new-5 ())
(deftest-error defmethod-new.5
  (defmethod defmethod-new-5 ()))

(defmacro defmethod-new-6 ())
(deftest-error defmethod-new.6
  (defmethod defmethod-new-6 ()))

(deftest-error defmethod-new.7
  (defmethod load-time-value ()))

(defun (setf defmethod-new-8) ())
(deftest-error defmethod-new.8
  (defmethod (setf defmethod-new-8) ()))


;;
;;  overwrite
;;
(defgeneric defmethod-overwrite-1 (x))
(defmethod defmethod-overwrite-1 ((x integer))
  (declare (ignore x))
  :aaa)

(deftest defmethod-overwrite.1
  (closp
    (defmethod defmethod-overwrite-1 ((x integer))
      (declare (ignore x))
      :bbb))
  t)

(deftest defmethod-overwrite.2
  (defmethod-overwrite-1 100)
  :bbb)


;;
;;  lambda-list
;;
(deftest-error defmethod-lambda.1
  (progn
    (defgeneric defmethod-lambda-1 (x))
    (defmethod defmethod-lambda-1 ()
      :hello)))

(deftest-error defmethod-lambda.2
  (progn
    (defgeneric defmethod-lambda-2 (x))
    (defmethod defmethod-lambda-2 (x y)
      (declare (ignore x y))
      :hello)))

(deftest-error defmethod-lambda.3
  (progn
    (defgeneric defmethod-lambda-3 (x &optional y))
    (defmethod defmethod-lambda-3 (x y)
      (declare (ignore x y))
      :hello)))

(deftest-error defmethod-lambda.4
  (progn
    (defgeneric defmethod-lambda-4 (x y))
    (defmethod defmethod-lambda-4 (x &optional y)
      (declare (ignore x y))
      :hello)))

(deftest defmethod-lambda.5
  (progn
    (defgeneric defmethod-lambda-5 (x &rest args))
    (defmethod defmethod-lambda-5 (x &key)
      (declare (ignore x))
      :hello)
    (defmethod-lambda-5 10))
  :hello)

(deftest-error defmethod-lambda.6
  (progn
    (defgeneric defmethod-lambda-6 (x &key))
    (defmethod defmethod-lambda-6 (x)
      (declare (ignore x y))
      :hello)))

(deftest defmethod-block.1
  (progn
    (defgeneric defmethod-block-1 (x))
    (defmethod defmethod-block-1 (x)
      (return-from defmethod-block-1 (+ x 100)))
    (defmethod-block-1 200))
  300)

(deftest defmethod-block.2
  (progn
    (defgeneric (setf defmethod-block-2) (x))
    (defmethod (setf defmethod-block-2) (x)
      (return-from defmethod-block-2 (+ x 500)))
    (funcall #'(setf defmethod-block-2) 200))
  700)

(deftest defmethod-class.1
  (progn
    (defclass defmethod-method-class-1 (standard-method) ())
    (make-instance 'defmethod-method-class-1)
    (defgeneric defmethod-class-1 () (:method-class defmethod-method-class-1))
    (let ((x (defmethod defmethod-class-1 ())))
      (class-name
        (class-of x))))
  defmethod-method-class-1)


;;
;;  eql-specializer
;;
(defgeneric defmethod-eql-specializer-1 (value))

(defmethod defmethod-eql-specializer-1 (value)
  value)

(deftest defmethod-eql-specializer.1
  (typep
    (defmethod defmethod-eql-specializer-1 ((value (eql 100)))
      (format nil "Hello: ~A" value))
    'standard-method)
  t)

(deftest defmethod-eql-specializer.2
  (defmethod-eql-specializer-1 10)
  10)

(deftest defmethod-eql-specializer.3
  (defmethod-eql-specializer-1 100)
  "Hello: 100")


;;
;;  Local Function NEXT-METHOD-P
;;
(deftest next-method-p.1
  (progn
    (defgeneric next-method-p-1 ())
    (defmethod next-method-p-1 ()
      (next-method-p))
    (next-method-p-1))
  nil)

(deftest next-method-p.2
  (progn
    (defgeneric next-method-p-2 (x))
    (defmethod next-method-p-2 ((x real))
      (declare (ignore x))
      :real)
    (defmethod next-method-p-2 ((x integer))
      (declare (ignore x))
      (next-method-p))
    (values
      (next-method-p-2 1.23)
      (next-method-p-2 100)))
  :real t)

(deftest-error next-method-p.3
  (eval '(progn
           (defgeneric next-method-p-3 ())
           (defmethod next-method-p-3 ()
             (next-method-p 100))
           (next-method-p-3))))

(deftest next-method-p.4
  (flet ((next-method-p () :hello))
    (declare (ignorable #'next-method-p))
    (defgeneric next-method-p-4 ())
    (defmethod next-method-p-4 ()
      (next-method-p))
    (next-method-p-4))
  nil)


;;
;;  Local Function CALL-NEXT-METHOD
;;
(deftest call-next-method.1
  (progn
    (defgeneric call-next-method-1 (x))
    (defmethod call-next-method-1 ((x integer))
      (declare (ignore x))
      (call-next-method))
    (defmethod call-next-method-1 ((x real))
      (declare (ignore x))
      :hello)
    (call-next-method-1 100))
  :hello)

(deftest call-next-method.2
  (progn
    (defgeneric call-next-method-2 (x))
    (defmethod call-next-method-2 ((x integer))
      (declare (ignore x))
      (call-next-method))
    (defmethod call-next-method-2 ((x real))
      (declare (ignore x))
      (values 10 20 30))
    (call-next-method-2 100))
  10 20 30)

(deftest call-next-method.3
  (progn
    (defgeneric call-next-method-3 (x))
    (defmethod call-next-method-3 ((x integer))
      (list* :integer x (call-next-method)))
    (defmethod call-next-method-3 ((x real))
      (list :real x))
    (call-next-method-3 100))
  (:integer 100 :real 100))

(deftest-error call-next-method.4
  (progn
    (defgeneric call-next-method-4 ())
    (defmethod call-next-method-4 ()
      (call-next-method))
    (call-next-method-4)))

(deftest call-next-method.5
  (progn
    (defgeneric call-next-method-5 ())
    (defmethod call-next-method-5 ()
      (if (next-method-p)
        (call-next-method)
        :error))
    (call-next-method-5))
  :error)

(deftest call-next-method.6
  (progn
    (sysctl 'recovery 'no-next-method)
    (defmethod no-next-method
      ((x standard-generic-function) (y standard-method) &rest args)
      (declare (ignore x y args))
      :hello)
    (defgeneric call-next-method-6 ())
    (defmethod call-next-method-6 ()
      (call-next-method))
    (prog1 (call-next-method-6)
      (sysctl 'recovery 'no-next-method)))
  :hello)

(deftest call-next-method-standard.1
  (progn
    (defgeneric call-next-method-standard-1 (x))
    (defmethod call-next-method-standard-1 :around ((x integer))
      (declare (ignore x))
      (+ 10 (call-next-method)))
    (defmethod call-next-method-standard-1 :around ((x real))
      (declare (ignore x))
      (+ 20 (call-next-method)))
    (defmethod call-next-method-standard-1 ((x real))
      (+ 30 x))
    (call-next-method-standard-1 100))
  160)

(deftest-error call-next-method-standard.2
  (progn
    (defgeneric call-next-method-standard-2 (x))
    (defmethod call-next-method-standard-2 :before ((x integer))
      (declare (ignore x))
      (call-next-method))
    (defmethod call-next-method-standard-2 :before ((x real))
      (declare (ignore x))
      111)
    (defmethod call-next-method-standard-2 ((x real))
      x)
    (call-next-method-standard-2 100)))

(deftest-error call-next-method-standard.3
  (progn
    (defgeneric call-next-method-standard-3 (x))
    (defmethod call-next-method-standard-3 :after ((x integer))
      (declare (ignore x))
      222)
    (defmethod call-next-method-standard-3 :after ((x real))
      (declare (ignore x))
      (call-next-method))
    (defmethod call-next-method-standard-3 ((x real))
      x)
    (call-next-method-standard-3 100)))

(deftest call-next-method-rest.1
  (progn
    (defgeneric call-next-method-rest-1 (x))
    (defmethod call-next-method-rest-1 ((x integer))
      (declare (ignore x))
      (call-next-method 10))
    (defmethod call-next-method-rest-1 ((x real))
      (1+ x))
    (call-next-method-rest-1 999))
  11)

(deftest-error call-next-method-rest.2
  (progn
    (defgeneric call-next-method-rest-2 (x))
    (defmethod call-next-method-rest-2 ((x integer))
      (declare (ignore x))
      (call-next-method 10 20 30))
    (defmethod call-next-method-rest-2 ((x real))
      (1+ x))
    (call-next-method-rest-2 999)))

(deftest call-next-method-rest.3
  (progn
    (defgeneric call-next-method-rest-3 (x y &optional z))
    (defmethod call-next-method-rest-3 ((x integer) y &optional z)
      (declare (ignore x y z))
      (call-next-method 10 20 30))
    (defmethod call-next-method-rest-3 ((x real) y &optional (z 999))
      (values x y z))
    (call-next-method-rest-3 888 777 666))
  10 20 30)

(deftest call-next-method-rest.4
  (progn
    (defgeneric call-next-method-rest-4 (x y &optional z))
    (defmethod call-next-method-rest-4 ((x integer) y &optional z)
      (declare (ignore x y z))
      (call-next-method 10 20))
    (defmethod call-next-method-rest-4 ((x real) y &optional (z 999))
      (values x y z))
    (call-next-method-rest-4 888 777 666))
  10 20 999)
(deftest-error call-next-method-error.1
  (eval '(call-next-method)))

(deftest-error call-next-method-error.2
  (progn
    (defgeneric call-next-method-error-2 (x y))
    (defmethod call-next-method-error-2 ((x integer) y)
      (declare (ignore x y z))
      (call-next-method 10 20 30))
    (defmethod call-next-method-error-2 ((x real) y)
      (values x y))
    (call-next-method-error-2 888 777 666)))

