;;
;;  ANSI COMMON LISP: 7. Objects
;;
(import 'lisp-clos:find-method-combination)

;;
;;  Macro DEFGENERIC
;;
(deftest defgeneric.1
  (typep
    (defgeneric defgeneric-1 ())
    'standard-generic-function)
  t)

(deftest defgeneric.2
  (functionp #'defgeneric-1)
  t)

(deftest defgeneric.3
  (closp #'defgeneric-1)
  t)

(deftest defgeneric.4
  (functionp
    (defgeneric defgeneric-4 (a b c)))
  t)

(deftest defgeneric.5
  (functionp
    (defgeneric defgeneric-5 (a b &optional c d) (:argument-precedence-order b a)))
  t)

(deftest defgeneric.6
  (functionp
    (defgeneric defgeneric-6 (c d &optional &rest e) (:argument-precedence-order c d)))
  t)

(deftest defgeneric.7
  (functionp
    (defgeneric defgeneric-7 (a) (:documentation "Hello")))
  t)

(deftest defgeneric.8
  (functionp
    (defgeneric defgeneric-8 (a) (:method-combination standard)))
  t)

(deftest defgeneric.9
  (functionp
    (defgeneric defgeneric-9 (a &optional b)
                (:generic-function-class standard-generic-function)))
  t)

(deftest defgeneric.10
  (functionp
    (defgeneric defgeneric-10 (a b) (:method-class standard-method)))
  t)

(deftest defgeneric.11
  (functionp
    (defgeneric defgeneric-11 (a b) (declare (optimize speed safety))))
  t)


;;
;;  function-call
;;
(deftest defgeneric-call.1
  (progn
    (defgeneric defgeneric-call-1 (a))
    (values)))

(deftest-error defgeneric-call.2
  (defgeneric-call-1 10))

(deftest defgeneric-call.3
  (progn
    (defmethod defgeneric-call-1 ((a integer))
      (+ a 100))
    (defgeneric-call-1 10))
  110)

(deftest-error defgeneric-call.4
  (defgeneric-call-1 "Hello"))

(deftest defgeneric-call.5
  (progn
    (defmethod defgeneric-call-1 ((a string))
      (format nil "<<~A>>" a))
    (values
      (defgeneric-call-1 10)
      (defgeneric-call-1 "ABC")))
  110 "<<ABC>>")

(deftest-error defgeneric-call.6
  (defgeneric-call-1 10 20))

(deftest defgeneric-call.7
  (values
    (funcall #'defgeneric-call-1 10)
    (apply #'defgeneric-call-1 (list "ABC")))
  110 "<<ABC>>")


;;
;;  name
;;
(deftest defgeneric-name.1
  (functionp
    (defgeneric defgeneric-name-1 ()))
  t)

(deftest defgeneric-name.2
  (functionp
    (defgeneric (setf defgeneric-name-2) ()))
  t)

(deftest-error defgeneric-name.3
  (eval '(defgeneric 100 ())))

(defun defgeneric-name-4 () :hello)
(deftest-error defgeneric-name.4
  (eval '(defgeneric defgeneric-name-4 ()))
  program-error)

(defmacro defgeneric-name-5 () :hello)
(deftest-error defgeneric-name.5
  (eval '(defgeneric defgeneric-name-5 ()))
  program-error)

(deftest-error defgeneric-name.6
  (eval '(defgeneric load-time-value ()))
  program-error)


;;
;;  argument-precedence-order
;;
(deftest defgeneric-order.1
  (progn
    (defgeneric defgeneric-order-1 (x y) (:argument-precedence-order x y))
    (values)))

(deftest defgeneric-order.2
  (progn
    (defgeneric defgeneric-order-2 (x y) (:argument-precedence-order y x))
    (values)))

(deftest-error defgeneric-order.3
  (defgeneric defgeneric-order-3 (x y z) (:argument-precedence-order y))
  program-error)

(deftest-error defgeneric-order.4
  (defgeneric defgeneric-order-4 (x y z) (:argument-precedence-order x y z a))
  program-error)

(deftest-error defgeneric-order.5
  (defgeneric defgeneric-order-5 (x y z) (:argument-precedence-order x x x))
  program-error)

(deftest-error defgeneric-order.6
  (defgeneric defgeneric-order-6 (x y &optional z)
              (:argument-precedence-order x y z))
  program-error)

(deftest defgeneric-order.7
  (functionp
    (defgeneric defgeneric-order-7 (x y &optional z)
                (:argument-precedence-order x y)))
  t)

(defvar *defgeneric-order*)
(deftest defgeneric-order.8
  (progn
    (defmethod defgeneric-order-1 ((x integer) (y real))
      (push :first *defgeneric-order*)
      (push (list x y) *defgeneric-order*)
      (when (next-method-p)
        (call-next-method)))
    (defmethod defgeneric-order-1 ((x real) (y integer))
      (push :second *defgeneric-order*)
      (push (list x y) *defgeneric-order*)
      (when (next-method-p)
        (call-next-method)))
    (defmethod defgeneric-order-2 ((x integer) (y real))
      (push :first *defgeneric-order*)
      (push (list x y) *defgeneric-order*)
      (when (next-method-p)
        (call-next-method)))
    (defmethod defgeneric-order-2 ((x real) (y integer))
      (push :second *defgeneric-order*)
      (push (list x y) *defgeneric-order*)
      (when (next-method-p)
        (call-next-method)))
    (values)))

(deftest defgeneric-order.9
  (let (*defgeneric-order*)
    (defgeneric-order-1 10 20)
    (nreverse *defgeneric-order*))
  (:first (10 20) :second (10 20)))

(deftest defgeneric-order.10
  (let (*defgeneric-order*)
    (defgeneric-order-2 10 20)
    (nreverse *defgeneric-order*))
  (:second (10 20) :first (10 20)))


;;
;;  declare
;;
(deftest defgeneric-declare.1
  (functionp
    (defgeneric defgeneric-declare-1 (a b) (declare)))
  t)

(deftest-error defgeneric-declare.2
  (eval '(defgeneric defgeneric-declare-2 (a) (declare (special *hello*)))))

(deftest defgeneric-declare.3
  (functionp
    (defgeneric defgeneric-declare-3 (a b) (declare (optimize speed))))
  t)

(deftest-error defgeneric-declare.4
  (eval '(defgeneric defgeneric-declare-2 (a) (declare (ftype)))))

(deftest-error defgeneric-declare.5
  (eval '(defgeneric defgeneric-declare-2 (a) (declare (inline)))))

(deftest-error defgeneric-declare.6
  (eval '(defgeneric defgeneric-declare-2 (a) (declare (notinline)))))

(deftest-error defgeneric-declare.7
  (eval '(defgeneric defgeneric-declare-2 (a) (declare (declaration aaa)))))


;;
;;  documentation
;;
(deftest defgeneric-documentation.1
  (functionp
    (defgeneric defgeneric-documentation-1 () (:documentation "Hello")))
  t)

(deftest-error defgeneric-documentation.2
  (eval '(defgeneric defgeneric-documentation-2 () (:documentation))))

(deftest-error defgeneric-documentation.3
  (eval '(defgeneric defgeneric-documentation-3 () (:documentation 10))))

(deftest-error defgeneric-documentation.4
  (eval '(defgeneric defgeneric-documentation-4 () (:documentation "Hello" nil))))

(deftest defgeneric-documentation.5
  (documentation 'defgeneric-documentation-1 'function)
  "Hello")


;;
;;  method-combination
;;
(deftest defgeneric-method-combination.1
  (progn
    (defun defgeneric-method-combination-1 (&rest args)
      (+ 111 (apply #'+ args)))
    (define-method-combination defgeneric-method-combination-1)
    (defgeneric defgeneric-method-combination-2 (a)
                (:method-combination defgeneric-method-combination-1))
    (values)))

(deftest defgeneric-method-combination.2
  (progn
    (defmethod defgeneric-method-combination-2
      defgeneric-method-combination-1 ((a integer))
      (declare (ignore a))
      10)
    (defmethod defgeneric-method-combination-2
      defgeneric-method-combination-1 ((a rational))
      (declare (ignore a))
      20)
    (defmethod defgeneric-method-combination-2
      defgeneric-method-combination-1 ((a real))
      (declare (ignore a))
      30)
    (defgeneric-method-combination-2 10))
  171)

(deftest-error defgeneric-method-combination.3
  (eval '(defgeneric defgeneric-method-combination-3 (a)
                     (:method-combination))))


;;
;;  generic-function-class
;;
(deftest defgeneric-generic-function-class.1
  (progn
    (defgeneric defgeneric-generic-function-class-1 (x)
                (:generic-function-class standard-generic-function))
    (values)))

(deftest-error defgeneric-generic-function-class.2
  (eval '(defgeneric defgeneric-generic-function-class-2 (x)
                     (:generic-function-class))))

(deftest-error defgeneric-generic-function-class.3
  (eval '(defgeneric defgeneric-generic-function-class-3 (x)
                     (:generic-function-class standard-generic-function nil))))

(deftest-error defgeneric-generic-function-class.4
  (eval '(defgeneric defgeneric-generic-function-class-4 (x)
                     (:generic-function-class nil))))

(deftest-error defgeneric-generic-function-class.5
  (eval '(defgeneric defgeneric-generic-function-class-5 (x)
                     (:generic-function-class 100))))


;;
;;  method-class
;;
(deftest defgeneric-method-class.1
  (progn
    (defgeneric defgeneric-method-class-1 (a) (:method-class standard-method))
    (values)))

(deftest-error defgeneric-method-class.2
  (eval '(defgeneric defgeneric-method-class-2 (a) (:method-class))))

(deftest-error defgeneric-method-class.3
  (eval '(defgeneric defgeneric-method-class-3 (a)
                     (:method-class standard-method nil))))

(deftest-error defgeneric-method-class.4
  (eval '(defgeneric defgeneric-method-class-4 (a)
                     (:method-class nil))))


;;
;;  method
;;
(deftest defgeneric-method.1
  (typep
    (defgeneric defgeneric-method-1 () (:method () :hello))
    'generic-function)
  t)

(deftest defgeneric-method.2
  (progn
    (defgeneric defgeneric-method-2 () (:method () :hello))
    (defgeneric-method-2))
  :hello)

(deftest defgeneric-method.3
  (progn
    (defgeneric defgeneric-method-3 (x)
                (:method ((x integer)) x :aaa)
                (:method ((y string)) y :bbb))
    (values
      (defgeneric-method-3 100)
      (defgeneric-method-3 "ABC")))
  :aaa :bbb)


;;
;;  redefine
;;
(deftest defgeneric-redefine.1
  (progn
    (defgeneric defgeneric-redefine-1 ())
    (functionp
      (defgeneric defgeneric-redefine-1 (x))))
  t)

(deftest defgeneric-redefine.2
  (progn
    (defgeneric defgeneric-redefine-2 ())
    (defmethod defgeneric-redefine-2 ())
    (functionp
      (defgeneric defgeneric-redefine-2 ())))
  t)

(deftest-error defgeneric-redefine.3
  (progn
    (defgeneric defgeneric-redefine-3 (x))
    (defmethod defgeneric-redefine-3 (x) x)
    (defgeneric defgeneric-redefine-3 ())))

(deftest defgeneric-redefine.4
  (progn
    (defgeneric defgeneric-redefine-4 (x))
    (defmethod defgeneric-redefine-4 (x) x)
    (defgeneric defgeneric-redefine-4 (x))
    (defmethod defgeneric-redefine-4 (x) (1+ x))
    (defgeneric-redefine-4 100))
  101)

(deftest defgeneric-redefine-method.1
  (progn
    (defgeneric defgeneric-redefine-method-1 (x) (:method (x) (1+ x)))
    (defgeneric-redefine-method-1 100))
  101)

(deftest-error defgeneric-redefine-method.2
  (progn
    (defgeneric defgeneric-redefine-method-2 (x) (:method (x) (1+ x)))
    (defgeneric defgeneric-redefine-method-2 (x))
    (defgeneric-redefine-method-2 100)))

(deftest defgeneric-redefine-method.3
  (progn
    (defgeneric defgeneric-redefine-method-3 (x))
    (defmethod defgeneric-redefine-method-3 (x) (1+ x))
    (defgeneric defgeneric-redefine-method-3 (x))
    (defgeneric-redefine-method-3 100))
  101)


;;
;;  error
;;
(deftest-error defgeneric-error.1
  (eval '(defgeneric defgeneric-error1 (a b &optional c d)
                     (:argument-precedence-order :most-specific-first))))

(deftest-error defgeneric-error.2
  (eval '(defgeneric defgeneric-error2 (&optional c d &rest e)
                     (:argument-precedence-order :most-specific-last))))

(deftest-error defgeneric-error.3
  (eval '(defgeneric defgeneric-error2 (a b)
                     (:argument-precedence-order c d))))

(defgeneric defgeneric-error4 (x y z) (:argument-precedence-order z y x))

(defmethod defgeneric-error4 ((x integer) y (z t))
  (declare (ignore x y z))
  "integer-t")

(defmethod defgeneric-error4 ((x t) y (z string))
  (declare (ignore x y z))
  "t-string")

(deftest defgeneric-error.4
  (defgeneric-error4 10 20 "Hello")
  "t-string")

(defgeneric defgeneric-error5 (x y z) (:argument-precedence-order x y z))

(defmethod defgeneric-error5 ((x integer) y (z t))
  (declare (ignore x y z))
  "integer-t")

(defmethod defgeneric-error5 ((x t) y (z string))
  (declare (ignore x y z))
  "t-string")

(deftest defgeneric-error.5
  (defgeneric-error5 10 20 "Hello")
  "integer-t")

(deftest-error defgeneric-error.6
  (eval '(defgeneric defgeneric-error-6 (x) (:hello 10)))
  program-error)


;;
;;  Function ENSURE-GENERIC-FUNCTION
;;
(deftest ensure-generic-function.1
  (typep
    (ensure-generic-function 'ensure-generic-function-1)
    'standard-generic-function)
  t)

(deftest ensure-generic-function.2
  (functionp
    #'ensure-generic-function-1)
  t)

(deftest ensure-generic-function.3
  (closp
    #'ensure-generic-function-1)
  t)

(deftest ensure-generic-function.4
  (functionp
    (ensure-generic-function
      'ensure-generic-function-4
      :lambda-list '(a b c)))
  t)

(deftest ensure-generic-function.5
  (functionp
    (ensure-generic-function
      'ensure-generic-function-5
      :lambda-list '(a b &optional c d)
      :argument-precedence-order '(b a)))
  t)

(deftest ensure-generic-function.6
  (functionp
    (ensure-generic-function
      'ensure-generic-function-6
      :lambda-list '(c d &optional &rest e)
      :argument-precedence-order '(c d)))
  t)

(deftest ensure-generic-function.7
  (functionp
    (ensure-generic-function
      'ensure-generic-function-7
      :lambda-list '(a)
      :documentation "Hello"))
  t)

(deftest ensure-generic-function.8
  (functionp
    (ensure-generic-function
      'ensure-generic-function-8
      :lambda-list '(a)
      :method-combination (find-method-combination
                            (defgeneric ensure-generic-function-8-1 ())
                            'standard nil)))
  t)

(deftest ensure-generic-function.9
  (functionp
    (ensure-generic-function
      'ensure-generic-function-9
      :lambda-list '(a &optional b)
      :generic-function-class (find-class 'standard-generic-function)))
  t)

(deftest ensure-generic-function.10
  (functionp
    (ensure-generic-function
      'ensure-generic-function-10
      :lambda-list '(a b)
      :method-class (find-class 'standard-method)))
  t)

(deftest ensure-generic-function.11
  (functionp
    (ensure-generic-function
      'ensure-generic-function-11
      :lambda-list '(a b)
      :declare '(optimize speed safety)))
  t)


;;
;;  function-call
;;
(deftest ensure-generic-function-call.1
  (progn
    (ensure-generic-function
      'ensure-generic-function-call-1
      :lambda-list '(a))
    (values)))

(deftest-error ensure-generic-function-call.2
  (ensure-generic-function-call-1 10))

(deftest ensure-generic-function-call.3
  (progn
    (defmethod ensure-generic-function-call-1 ((a integer))
      (+ a 100))
    (ensure-generic-function-call-1 10))
  110)

(deftest-error ensure-generic-function-call.4
  (ensure-generic-function-call-1 "Hello"))

(deftest ensure-generic-function-call.5
  (progn
    (defmethod ensure-generic-function-call-1 ((a string))
      (format nil "<<~A>>" a))
    (values
      (ensure-generic-function-call-1 10)
      (ensure-generic-function-call-1 "ABC")))
  110 "<<ABC>>")

(deftest-error ensure-generic-function-call.6
  (ensure-generic-function-call-1 10 20))

(deftest ensure-generic-function-call.7
  (values
    (funcall #'ensure-generic-function-call-1 10)
    (apply #'ensure-generic-function-call-1 (list "ABC")))
  110 "<<ABC>>")

(deftest ensure-generic-function-call.8
  (progn
    (ensure-generic-function
      'ensure-generic-function-call-8)
    (defmethod ensure-generic-function-call-8 ()
      :hello)
    (ensure-generic-function-call-8))
  :hello)


;;
;;  name
;;
(deftest ensure-generic-function-name.1
  (functionp
    (ensure-generic-function
      'ensure-generic-function-name-1))
  t)

(deftest ensure-generic-function-name.2
  (functionp
    (ensure-generic-function
      '(setf ensure-generic-function-name-2)))
  t)

(deftest-error ensure-generic-function-name.3
  (eval '(ensure-generic-function 100)))

(defun ensure-generic-function-name-4 () :hello)
(deftest-error ensure-generic-function-name.4
  (eval '(ensure-generic-function 'ensure-generic-function-name-4))
  type-error)

(defmacro ensure-generic-function-name-5 () :hello)
(deftest-error ensure-generic-function-name.5
  (eval '(ensure-generic-function 'ensure-generic-function-name-5))
  type-error)

(deftest-error ensure-generic-function-name.6
  (eval '(ensure-generic-function 'load-time-value))
  type-error)


;;
;;  argument-precedence-order
;;
(deftest ensure-generic-function-order.1
  (progn
    (ensure-generic-function'
      ensure-generic-function-order-1
      :lambda-list '(x y)
      :argument-precedence-order '(x y))
    (values)))

(deftest ensure-generic-function-order.2
  (progn
    (ensure-generic-function
      'ensure-generic-function-order-2
      :lambda-list '(x y)
      :argument-precedence-order '(y x))
    (values)))

(deftest-error ensure-generic-function-order.3
  (ensure-generic-function
    'ensure-generic-function-order-3
    :lambda-list '(x y z)
    :argument-precedence-order '(y)))

(deftest-error ensure-generic-function-order.4
  (ensure-generic-function
    'ensure-generic-function-order-4
    :lambda-list '(x y z)
    :argument-precedence-order '(x y z a)))

(deftest-error ensure-generic-function-order.5
  (ensure-generic-function
    'ensure-generic-function-order-5
    :lambda-list '(x y z)
    :argument-precedence-order '(x x x)))

(deftest-error ensure-generic-function-order.6
  (ensure-generic-function
    'ensure-generic-function-order-6
    :lambda-list '(x y &optional z)
    :argument-precedence-order '(x y z)))

(deftest ensure-generic-function-order.7
  (functionp
    (ensure-generic-function
      'ensure-generic-function-order-7
      :lambda-list '(x y &optional z)
      :argument-precedence-order '(x y)))
  t)

(defvar *ensure-generic-function-order*)
(deftest ensure-generic-function-order.8
  (progn
    (defmethod ensure-generic-function-order-1 ((x integer) (y real))
      (push :first *ensure-generic-function-order*)
      (push (list x y) *ensure-generic-function-order*)
      (when (next-method-p)
        (call-next-method)))
    (defmethod ensure-generic-function-order-1 ((x real) (y integer))
      (push :second *ensure-generic-function-order*)
      (push (list x y) *ensure-generic-function-order*)
      (when (next-method-p)
        (call-next-method)))
    (defmethod ensure-generic-function-order-2 ((x integer) (y real))
      (push :first *ensure-generic-function-order*)
      (push (list x y) *ensure-generic-function-order*)
      (when (next-method-p)
        (call-next-method)))
    (defmethod ensure-generic-function-order-2 ((x real) (y integer))
      (push :second *ensure-generic-function-order*)
      (push (list x y) *ensure-generic-function-order*)
      (when (next-method-p)
        (call-next-method)))
    (values)))

(deftest ensure-generic-function-order.9
  (let (*ensure-generic-function-order*)
    (ensure-generic-function-order-1 10 20)
    (nreverse *ensure-generic-function-order*))
  (:first (10 20) :second (10 20)))

(deftest ensure-generic-function-order.10
  (let (*ensure-generic-function-order*)
    (ensure-generic-function-order-2 10 20)
    (nreverse *ensure-generic-function-order*))
  (:second (10 20) :first (10 20)))


;;
;;  declare
;;
(deftest ensure-generic-function-declare.1
  (functionp
    (ensure-generic-function
      'ensure-generic-function-declare-1
      :lambda-list '(a b)
      :declare nil))
  t)

(deftest ensure-generic-function-declare.2
  (functionp
    (ensure-generic-function
      'ensure-generic-function-declare-2
      :lambda-list '(a)
      :declare '((special *hello*))))
  t)

(deftest ensure-generic-function-declare.3
  (functionp
    (ensure-generic-function
      'ensure-generic-function-declare-3
      :lambda-list '(a b)
      :declare '((optimize speed))))
  t)

(deftest ensure-generic-function-declare.4
  (functionp
    (ensure-generic-function
      'ensure-generic-function-declare-2
      :lambda-list '(a)
      :declare '((ftype))))
  t)

(deftest ensure-generic-function-declare.5
  (functionp
    (ensure-generic-function
      'ensure-generic-function-declare-2
      :lambda-list '(a)
      :declare '((inline))))
  t)

(deftest ensure-generic-function-declare.6
  (functionp
    (ensure-generic-function
      'ensure-generic-function-declare-2
      :lambda-list '(a)
      :declare '((notinline))))
  t)

(deftest ensure-generic-function-declare.7
  (functionp
    (ensure-generic-function
      'ensure-generic-function-declare-2
      :lambda-list '(a)
      :declare '((declaration aaa))))
  t)


;;
;;  documentation
;;
(deftest ensure-generic-function-documentation.1
  (functionp
    (ensure-generic-function
      'ensure-generic-function-documentation-1
      :documentation "Hello"))
  t)

(deftest-error ensure-generic-function-documentation.2
  (eval '(ensure-generic-function
           'ensure-generic-function-documentation-2
           :documentation 10)))

(deftest ensure-generic-function-documentation.3
  (documentation 'ensure-generic-function-documentation-1 'function)
  "Hello")


;;
;;  method-combination
;;
(deftest ensure-generic-function-method-combination.1
  (progn
    (defun ensure-generic-function-method-combination-1 (&rest args)
      (+ 111 (apply #'+ args)))
    (define-method-combination ensure-generic-function-method-combination-1)
    (ensure-generic-function
      'ensure-generic-function-method-combination-2
      :lambda-list '(a)
      :method-combination
      (find-method-combination
        (defgeneric ensure-generic-function-method-combination-1-dummy ())
        'ensure-generic-function-method-combination-1 nil))
    (values)))

(deftest ensure-generic-function-method-combination.2
  (progn
    (defmethod ensure-generic-function-method-combination-2
      ensure-generic-function-method-combination-1 ((a integer))
      (declare (ignore a))
      10)
    (defmethod ensure-generic-function-method-combination-2
      ensure-generic-function-method-combination-1 ((a rational))
      (declare (ignore a))
      20)
    (defmethod ensure-generic-function-method-combination-2
      ensure-generic-function-method-combination-1 ((a real))
      (declare (ignore a))
      30)
    (ensure-generic-function-method-combination-2 10))
  171)

(deftest-error ensure-generic-function-method-combination.3
  (ensure-generic-function
    'ensure-generic-function-method-combination-3
    :method-combination nil))


;;
;;  generic-function-class
;;
(deftest ensure-generic-function-generic-function-class.1
  (progn
    (ensure-generic-function'
      ensure-generic-function-generic-function-class-1
      :lambda-list '(x)
      :generic-function-class (find-class 'standard-generic-function))
    (values)))

(deftest-error ensure-generic-function-generic-function-class.2
  (eval '(ensure-generic-function
           'ensure-generic-function-generic-function-class-2
           :lambda-list '(x)
           :generic-function-class nil)))

(deftest-error ensure-generic-function-generic-function-class.3
  (eval '(ensure-generic-function
           'ensure-generic-function-generic-function-class-3
           :lambda-list '(x)
           :generic-function-class nil)))


;;
;;  method-class
;;
(deftest ensure-generic-function-method-class.1
  (progn
    (ensure-generic-function
      'ensure-generic-function-method-class-1
      :lambda-list '(a)
      :method-class (find-class 'standard-method))
    (values)))

(deftest-error ensure-generic-function-method-class.2
  (eval '(ensure-generic-function
           'ensure-generic-function-method-class-2
           :lambda-list '(a)
           :method-class nil)))


;;
;;  redefine
;;
(deftest ensure-generic-function-redefine.1
  (progn
    (ensure-generic-function
      'ensure-generic-function-redefine-1
      :lambda-list nil)
    (functionp
      (ensure-generic-function
        'ensure-generic-function-redefine-1
        :lambda-list '(x))))
  t)

(deftest ensure-generic-function-redefine.2
  (progn
    (ensure-generic-function
      'ensure-generic-function-redefine-2)
    (defmethod ensure-generic-function-redefine-2 ())
    (functionp
      (ensure-generic-function
        'ensure-generic-function-redefine-2)))
  t)

(deftest-error ensure-generic-function-redefine.3
  (progn
    (ensure-generic-function
      'ensure-generic-function-redefine-3
      :lambda-list '(x))
    (defmethod ensure-generic-function-redefine-3 (x) x)
    (ensure-generic-function
      'ensure-generic-function-redefine-3
      :lambda-list nil)))

(deftest ensure-generic-function-redefine.4
  (progn
    (ensure-generic-function
      'ensure-generic-function-redefine-4
      :lambda-list '(x))
    (defmethod ensure-generic-function-redefine-4 (x) x)
    (functionp
      (ensure-generic-function
        'ensure-generic-function-redefine-4)))
  t)

(deftest ensure-generic-function-redefine.5
  (progn
    (ensure-generic-function
      'ensure-generic-function-redefine-5
      :lambda-list '(x))
    (defmethod ensure-generic-function-redefine-5 (x) x)
    (ensure-generic-function
      'ensure-generic-function-redefine-5
      :lambda-list '(x))
    (defmethod ensure-generic-function-redefine-5 (x) (1+ x))
    (ensure-generic-function-redefine-5 100))
  101)

(deftest ensure-generic-function-redefine-method.1
  (progn
    (defgeneric ensure-generic-function-redefine-method-1 (x) (:method (x) (1+ x)))
    (ensure-generic-function-redefine-method-1 100))
  101)

(deftest ensure-generic-function-redefine-method.2
  (progn
    (defgeneric ensure-generic-function-redefine-method-2 (x) (:method (x) (1+ x)))
    (ensure-generic-function
      'ensure-generic-function-redefine-method-2
      :lambda-list '(x))
    (ensure-generic-function-redefine-method-2 100))
  101)

(deftest ensure-generic-function-redefine-method.3
  (progn
    (ensure-generic-function
      'ensure-generic-function-redefine-method-3
      :lambda-list '(x))
    (defmethod ensure-generic-function-redefine-method-3 (x) (1+ x))
    (ensure-generic-function
      'ensure-generic-function-redefine-method-3
      :lambda-list '(x))
    (ensure-generic-function-redefine-method-3 100))
  101)


;;
;;  environment
;;
(deftest ensure-generic-function-environment.1
  (functionp
    (ensure-generic-function
      'ensure-generic-function-environment-1
      :environment nil))
  t)

(deftest ensure-generic-function-environment.2
  (functionp
    (eval '(macrolet ((call (&environment env)
                            (ensure-generic-function
                              'ensure-generic-function-environment-1
                              :environment env)))
             (call))))
  t)

(deftest-error ensure-generic-function-environment.3
  (eval '(ensure-generic-function
           'ensure-generic-function-environment-1
           :environment 100)))


;;
;;  error
;;
(deftest-error! ensure-generic-function-error.1
  (eval '(ensure-generic-function)))

(deftest-error ensure-generic-function-error.2
  (eval '(ensure-generic-function 100))
  type-error)

(deftest-error ensure-generic-function-error.3
  (eval '(ensure-generic-function
           'ensure-generic-function-error-3 10)))

