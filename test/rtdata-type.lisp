;;
;;  ANSI COMMON LISP: 5. Data and Control Flow
;;

;;
;;  Function FUNCTIONP
;;
(deftest functionp.1
  (functionp #'car)
  t)

(deftest functionp.2
  (functionp #'(setf car))
  t)

(deftest functionp.3
  (functionp #'(lambda () :hello))
  t)

(deftest functionp.4
  (functionp '(lambda () :hello))
  nil)

(deftest functionp.5
  (functionp 'car)
  nil)

(deftest functionp.6
  (functionp 10)
  nil)

(deftest functionp.7
  (flet ((functionp-test-6 () :hello))
    (functionp #'functionp-test-6))
  t)

(deftest-error! functionp-error.1
  (eval '(functionp)))

(deftest-error! functionp-error.2
  (eval '(functionp nil nil)))

;;  ANSI Common Lisp
(deftest functionp-test.1
  (functionp 'append)
  nil)

(deftest functionp-test.2
  (functionp #'append)
  t)

(deftest functionp-test.3
  (functionp (symbol-function 'append))
  t)

(deftest functionp-test.4
  (flet ((f () 1)) (functionp #'f))
  t)

(deftest functionp-test.5
  (handler-bind ((warning #'muffle-warning))
    (functionp (compile nil '(lambda () 259))))
  t)

(deftest functionp-test.6
  (functionp nil)
  nil)

(deftest functionp-test.7
  (functionp 12)
  nil)

(deftest functionp-test.8
  (functionp '(lambda (x) (* x x)))
  nil)

(deftest functionp-test.9
  (functionp #'(lambda (x) (* x x)))
  t)


;;
;;  Function COMPILED-FUNCTION-P
;;
(deftest compiled-function-p.1
  (compiled-function-p #'car)
  t)

(deftest compiled-function-p.2
  (compiled-function-p nil)
  nil)

(deftest compiled-function-p.3
  (compiled-function-p (lambda () :hello))
  nil)

(deftest-error! compiled-function-p-error.1
  (eval '(compiled-function-p)))

(deftest-error! compiled-function-p-error.2
  (eval '(compiled-function-p nil nil)))

;;  ANSI Common Lisp
(deftest compiled-function-p-test.1
  (progn
    (defun compiled-function-p-test-1 (x) x)
    (values
      (functionp #'compiled-function-p-test-1)
      (compiled-function-p #'compiled-function-p-test-1)))
  t nil)

(deftest compiled-function-p-test.2
  (compiled-function-p 'compiled-function-p-test-1)
  nil)

(deftest compiled-function-p-test.3
  (compiled-function-p 'car)
  nil)

(deftest compiled-function-p-test.4
  (handler-bind ((warning #'muffle-warning))
    (compile 'compiled-function-p-test-1)
    (compiled-function-p #'compiled-function-p-test-1))
  nil)  ;; may be true.

(deftest compiled-function-p-test.5
  (compiled-function-p 'compiled-function-p-test-1)
  nil)

(deftest compiled-function-p-test.6
  (handler-bind ((warning #'muffle-warning))
    (compiled-function-p (compile nil '(lambda (x) x))))
  nil)  ;; may be true

(deftest compiled-function-p-test.7
  (compiled-function-p #'(lambda (x) x))
  nil)

(deftest compiled-function-p-test.8
  (compiled-function-p '(lambda (x) x))
  nil)


;;
;;  Condition Type CONTROL-ERROR
;;
(deftest control-error.1
  (lisp-system:closp
    (find-class 'control-error))
  t)

(deftest control-error.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'control-error)))
  (control-error error serious-condition condition standard-object t))


;;
;;  Condition Type PROGRAM-ERROR
;;
(deftest program-error.1
  (lisp-system:closp
    (find-class 'program-error))
  t)

(deftest program-error.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'program-error)))
  (program-error error serious-condition condition standard-object t))


;;
;;  Condition Type UNDEFINED-FUNCTION
;;
(deftest undefined-function.1
  (lisp-system:closp
    (find-class 'undefined-function))
  t)

(deftest undefined-function.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'undefined-function)))
  (undefined-function cell-error error serious-condition
                      condition standard-object t))

(deftest undefined-function.3
  (handler-case
    (eval '(no-such-function-name 100 200 300))
    (undefined-function (c) (cell-error-name c)))
  no-such-function-name)

