;;
;;  ANSI COMMON LISP: 7. Objects
;;

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
    (defgeneric defgeneric-3 (a b c)))
  t)

(deftest defgeneric.5
  (functionp
    (defgeneric defgeneric-4 (a b &optional c d) (:argument-precedence-order b a)))
  t)

(deftest defgeneric.6
  (functionp
    (defgeneric defgeneric-5 (c d &optional &rest e) (:argument-precedence-order c d)))
  t)

(deftest defgeneric.7
  (functionp
    (defgeneric defgeneric-6 (a) (:documentation "Hello")))
  t)

(deftest defgeneric.8
  (functionp
    (defgeneric defgeneric-7 (a) (:method-combination standard)))
  t)

(deftest defgeneric.9
  (functionp
    (defgeneric defgeneric-8 (a &optional b)
                (:generic-function-class standard-generic-function)))
  t)

(deftest defgeneric.10
  (functionp
    (defgeneric defgeneric-9 (a b) (:method-class standard-method)))
  t)

(deftest defgeneric.11
  (functionp
    (defgeneric defgeneric-10 (a b) (declare (optimize speed safety))))
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
  (defgeneric defgeneric-order-3 (x y z) (:argument-precedence-order y)))

(deftest-error defgeneric-order.4
  (defgeneric defgeneric-order-4 (x y z) (:argument-precedence-order x y z a)))

(deftest-error defgeneric-order.5
  (defgeneric defgeneric-order-5 (x y z) (:argument-precedence-order x x x)))

(deftest-error defgeneric-order.6
  (defgeneric defgeneric-order-6 (x y &optional z)
              (:argument-precedence-order x y z)))

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
  (eval '(defgeneric defgeneric-generic-function-class-1 (x)
                     (:generic-function-class))))

(deftest-error defgeneric-generic-function-class.3
  (eval '(defgeneric defgeneric-generic-function-class-1 (x)
                     (:generic-function-class standard-generic-function nil))))


;;
;;  method-class
;;
(deftest defgeneric-method-class.1
  (progn
    (defgeneric defgeneric-method-class-1 (a) (:method-class standard-method))
    (values)))

(deftest-error defgeneric-method-class.2
  (eval '(defgeneric defgeneric-method-class-1 (a) (:method-class))))

(deftest-error defgeneric-method-class.3
  (eval '(defgeneric defgeneric-method-class-1 (a)
                     (:method-class standard-method nil))))


;;
;;  redefine
;;
'(deftest defgeneric-redefine.1
   (progn
     (defgeneric defgeneric-redefine-1 ())
     (functionp
       (defgeneric defgeneric-redefine-1 (x))))
   t)


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


;;
;;  Function ENSURE-GENERIC-FUNCTION
;;

