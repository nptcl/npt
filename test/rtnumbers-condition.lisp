;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Condition Type ARITHMETIC-ERROR
;;
(deftest arithmetic-error-condition.1
  (lisp-system:closp
    (make-condition 'arithmetic-error))
  t)

(deftest arithmetic-error-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'arithmetic-error)))
  (arithmetic-error error serious-condition condition standard-object t))

(deftest arithmetic-error-condition.3
  (let ((inst (make-condition 'arithmetic-error :operation 'aa :operands '(bb))))
    (values
      (arithmetic-error-operation inst)
      (arithmetic-error-operands inst)))
  aa (bb))

(deftest arithmetic-error-condition.4
  (handler-case
    (eval '(/ 10 0))
    (arithmetic-error () 'hit))
  hit)


;;
;;  Function ARITHMETIC-ERROR-OPERATION
;;
(deftest arithmetic-error-operation.1
  (let ((x (make-condition 'arithmetic-error :operation 'aa :operands '(bb))))
    (arithmetic-error-operation x))
  aa)

(deftest-error! arithmetic-error-operation.2
  (eval '(arithmetic-error-operation)))

(deftest-error! arithmetic-error-operation.3
  (eval '(arithmetic-error-operation 10)))

(deftest-error! arithmetic-error-operation.4
  (eval '(arithmetic-error-operation
           (make-condition 'arithmetic-error :operation 'aa :operands '(bb))
           nil)))


;;
;;  Function ARITHMETIC-ERROR-OPERANDS
;;
(deftest arithmetic-error-operands.1
  (let ((x (make-condition 'arithmetic-error :operation 'aa :operands '(bb))))
    (arithmetic-error-operands x))
  (bb))

(deftest-error! arithmetic-error-operands.2
  (eval '(arithmetic-error-operands)))

(deftest-error! arithmetic-error-operands.3
  (eval '(arithmetic-error-operands 10)))

(deftest-error! arithmetic-error-operands.4
  (eval '(arithmetic-error-operands
           (make-condition 'arithmetic-error :operation 'aa :operands '(bb))
           nil)))


;;
;;  Condition Type DIVISION-BY-ZERO
;;
(deftest division-by-zero-condition.1
  (lisp-system:closp
    (make-condition 'division-by-zero))
  t)

(deftest division-by-zero-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'division-by-zero)))
  (division-by-zero
    arithmetic-error error serious-condition condition standard-object t))

(deftest division-by-zero-condition.3
  (let ((inst (make-condition 'division-by-zero :operation 'aa :operands '(bb))))
    (values
      (arithmetic-error-operation inst)
      (arithmetic-error-operands inst)))
  aa (bb))

(deftest division-by-zero-condition.4
  (handler-case
    (eval '(floor 10 0))
    (division-by-zero () 'hit))
  hit)


;;
;;  Condition Type FLOATING-POINT-INVALID-OPERATION
;;
(deftest floating-point-invalid-operation-condition.1
  (lisp-system:closp
    (make-condition 'floating-point-invalid-operation))
  t)

(deftest floating-point-invalid-operation-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'floating-point-invalid-operation)))
  (floating-point-invalid-operation
    arithmetic-error error serious-condition condition standard-object t))

(deftest floating-point-invalid-operation-condition.3
  (let ((inst (make-condition
                'floating-point-invalid-operation
                :operation 'aa :operands '(bb))))
    (values
      (arithmetic-error-operation inst)
      (arithmetic-error-operands inst)))
  aa (bb))


;;
;;  Condition Type FLOATING-POINT-INEXACT
;;
(deftest floating-point-inexact-condition.1
  (lisp-system:closp
    (make-condition 'floating-point-inexact))
  t)

(deftest floating-point-inexact-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'floating-point-inexact)))
  (floating-point-inexact
    arithmetic-error error serious-condition condition standard-object t))

(deftest floating-point-inexact-condition.3
  (let ((inst (make-condition
                'floating-point-inexact
                :operation 'aa :operands '(bb))))
    (values
      (arithmetic-error-operation inst)
      (arithmetic-error-operands inst)))
  aa (bb))


;;
;;  Condition Type FLOATING-POINT-OVERFLOW
;;
(deftest floating-point-overflow-condition.1
  (lisp-system:closp
    (make-condition 'floating-point-overflow))
  t)

(deftest floating-point-overflow-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'floating-point-overflow)))
  (floating-point-overflow
    arithmetic-error error serious-condition condition standard-object t))

(deftest floating-point-overflow-condition.3
  (let ((inst (make-condition
                'floating-point-overflow
                :operation 'aa :operands '(bb))))
    (values
      (arithmetic-error-operation inst)
      (arithmetic-error-operands inst)))
  aa (bb))

(deftest-error floating-point-overflow-condition.4
  (* 1.0f20 1.0f20 1.0f20 1.0f20)
  floating-point-overflow)


;;
;;  Condition Type FLOATING-POINT-UNDERFLOW
;;
(deftest floating-point-underflow-condition.1
  (lisp-system:closp
    (make-condition 'floating-point-underflow))
  t)

(deftest floating-point-underflow-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'floating-point-underflow)))
  (floating-point-underflow
    arithmetic-error error serious-condition condition standard-object t))

(deftest floating-point-underflow-condition.3
  (let ((inst (make-condition
                'floating-point-underflow
                :operation 'aa :operands '(bb))))
    (values
      (arithmetic-error-operation inst)
      (arithmetic-error-operands inst)))
  aa (bb))

