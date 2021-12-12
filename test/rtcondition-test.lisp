;;
;;  ANSI COMMON LISP: 9. Conditions
;;

;;
;;  Function CELL-ERROR-NAME
;;
(deftest cell-error-name.1
  (let ((x (make-condition 'cell-error :name 'hello)))
    (cell-error-name x))
  hello)

(deftest cell-error-name.2
  (handler-case
    (eval 'no-such-variable)
    (unbound-variable (c) (cell-error-name c)))
  no-such-variable)

(deftest cell-error-name.3
  (handler-case
    (eval #'no-such-function)
    (undefined-function (c) (cell-error-name c)))
  no-such-function)

(deftest cell-error-name.4
  (handler-case
    (eval #'(setf no-such-function))
    (undefined-function (c) (cell-error-name c)))
  (setf no-such-function))

(deftest cell-error-name.5
  (progn
    (defclass cell-error-name-1 () (aaa))
    (handler-case
      (eval '(slot-value (make-instance 'cell-error-name-1) 'aaa))
      (unbound-slot (c) (cell-error-name c))))
  aaa)

(deftest-error! cell-error-name-error.1
  (eval '(cell-error-name)))

(deftest-error! cell-error-name-error.2
  (eval '(cell-error-name 100)))

(deftest-error! cell-error-name-error.3
  (eval '(cell-error-name
           (make-condition 'cell-error)
           200)))


;;
;;  Function SIMPLE-CONDITION-FORMAT-CONTROL
;;
(deftest simple-condition-format-control.1
  (let ((x (make-condition
             'simple-condition
             :format-control "AAA"
             :format-arguments '("BBB"))))
    (simple-condition-format-control x))
  "AAA")

(deftest simple-condition-format-control.2
  (handler-case
    (error "Hello~A" 10 20 30)
    (simple-error (c)
      (simple-condition-format-control c)))
  "Hello~A")

(deftest-error! simple-condition-format-control-error.1
  (eval '(simple-condition-format-control)))

(deftest-error! simple-condition-format-control-error.2
  (eval '(simple-condition-format-control 100)))

(deftest-error! simple-condition-format-control-error.3
  (eval '(simple-condition-format-control
           (make-condition 'simple-condition)
           100)))


;;
;;  Function SIMPLE-CONDITION-FORMAT-ARGUMENTS
;;
(deftest simple-condition-format-arguments.1
  (let ((x (make-condition
             'simple-condition
             :format-control "AAA"
             :format-arguments '("BBB"))))
    (simple-condition-format-arguments x))
  ("BBB"))

(deftest simple-condition-format-arguments.2
  (handler-case
    (error "Hello~A" 10 20 30)
    (simple-error (c)
      (simple-condition-format-arguments c)))
  (10 20 30))

(deftest-error! simple-condition-format-arguments-error.1
  (eval '(simple-condition-format-arguments)))

(deftest-error! simple-condition-format-arguments-error.2
  (eval '(simple-condition-format-arguments 100)))

(deftest-error! simple-condition-format-arguments-error.3
  (eval '(simple-condition-format-arguments
           (make-condition 'simple-condition)
           100)))

;;  ANSI Common Lisp
(deftest simple-condition-format-control-test.1
  (let ((foo (make-condition 'simple-condition
                             :format-control "Hi ~S"
                             :format-arguments '(ho))))
    (apply #'format nil
           (simple-condition-format-control foo)
           (simple-condition-format-arguments foo)))
  "Hi HO")


;;
;;  Macro ASSERT
;;
(deftest assert.1
  (assert t)
  nil)

(deftest-error assert.2
  (assert nil))

(deftest assert.3
  (handler-case
    (assert nil)
    (error () 'hit))
  hit)

(deftest assert.4
  (let ((x 100) (n 0))
    (handler-bind
      ((error
         (lambda (c)
           (if (< n 3)
             (incf n 1)
             (setq x 200))
           (continue c))))
      (assert (eql x 200)))
    (values x n))
  200 3)

(deftest assert.5
  (let ((x 200))
    (assert (eql x 200) (x)))
  nil)

(deftest assert.6
  (let ((x 200))
    (assert (eql x 200) ()))
  nil)

(deftest assert.7
  (let ((x 200) y z)
    (assert (eql x 200) (x y z)))
  nil)

(deftest assert.8
  (handler-case
    (let ((x 100))
      (assert (eql x 200) (x)))
    (error () 'hit))
  hit)

(deftest assert.9
  (handler-case
    (let ((x 100))
      (assert (eql x 200) (x) "Hello" 10 20 30))
    (error (c)
      (values
        (simple-condition-format-control c)
        (simple-condition-format-arguments c))))
  "Hello" (10 20 30))

(deftest-error assert-error.1
  (eval '(assert)))

(deftest-error assert-error.2
  (eval '(assert nil 10)))


;;
;;  Macro CHECK-TYPE
;;
(deftest check-type.1
  (let ((x 10))
    (check-type x integer))
  nil)

(deftest-error check-type.2
  (let ((x 10))
    (check-type x string))
  type-error)

(deftest-error check-type.3
  (let ((x 10))
    (check-type x string))
  type-error)

(deftest check-type.4
  (let ((x 10))
    (handler-bind ((type-error
                     (lambda (c)
                       (store-value "Hello" c))))
      (check-type x string)))
  nil)

(deftest check-type.5
  (let ((x 10))
    (handler-bind ((type-error
                     (lambda (c)
                       (store-value "Hello" c))))
      (check-type x string))
    x)
  "Hello")

(deftest check-type.6
  (let ((x 10))
    (check-type x integer "Hello"))
  nil)

(deftest-error check-type.7
  (let ((x 10))
    (check-type x string (concatenate 'string "ABC" "DEF")))
  type-error)

(deftest check-type.8
  (let ((list '(200 300 "abc" #\a))
        (x 100))
    (handler-bind ((type-error
                     (lambda (c)
                       (store-value (pop list) c))))
      (check-type x string "HELLO"))
    x)
  "abc")

(deftest-error! check-type-error.1
  (eval '(let (x) (check-type x))))

(deftest-error! check-type-error.2
  (eval '(let ((x 100)) (check-type x no-such-type-specifier))))

(deftest-error! check-type-error.3
  (eval '(let ((x 100)) (check-type x integer "string" 10))))

(deftest check-type-degrade.1
  (let ((x 'hello))
    (handler-bind
      ((type-error
         (lambda (c)
           (store-value 999 c))))
      (check-type x integer))
    x)
  999)

(deftest check-type-degrade.2
  (let ((x '(a b c d)))
    (handler-bind
      ((type-error
         (lambda (c)
           (store-value 999 c))))
      (check-type (car x) integer))
    x)
  (999 b c d))

;;  ANSI Common Lisp
(defvar check-type-aardvarks)

(deftest-error check-type-test.1
  (progn
    (setq check-type-aardvarks '(sam harry fred))
    (check-type check-type-aardvarks (array * (3))))
  type-error)

(deftest check-type-test.2
  (handler-bind ((type-error
                   (lambda (c)
                     (store-value #(sam fred harry) c))))
    (setq check-type-aardvarks '(sam harry fred))
    (check-type check-type-aardvarks (array * (3))))
  nil)

(deftest check-type-test.3
  check-type-aardvarks
  #(sam fred harry))

(deftest check-type-test.4
  (map 'list #'identity check-type-aardvarks)
  (sam fred harry))

(deftest-error check-type-test.5
  (let ((aardvark-count 'foo))
    (check-type aardvark-count (integer 0 *) "A positive integer"))
  type-error)

(defmacro check-type-define-adder (name amount)
  (check-type name (and symbol (not null)) "a name for an adder function")
  (check-type amount integer)
  `(defun ,name (x) (+ x ,amount)))

(deftest check-type-test.6
  (values
    (macroexpand-1 '(check-type-define-adder add3 3)))
  (defun add3 (x) (+ x 3)))

(deftest check-type-test.7
  (handler-bind ((type-error
                   (lambda (c)
                     (store-value 'add7 c))))
    (values
      (macroexpand-1 '(check-type-define-adder 7 7))))
  (defun add7 (x) (+ x 7)))

(deftest check-type-test.8
  (handler-bind ((type-error
                   (lambda (c)
                     (store-value 5 c))))
    (values
      (macroexpand-1 '(check-type-define-adder add5 something))))
  (defun add5 (x) (+ x 5)))


;;
;;  Function BREAK
;;
(deftest-error break-error.1
  (eval '(break 10))
  type-error)


;;
;;  Variable *DEBUGGER-HOOK*
;;
(deftest debugger-hook.1
  *debugger-hook*
  nil)

(deftest debugger-hook.2
  (lisp-system:specialp '*debugger-hook*)
  t)

(deftest debugger-hook.3
  (let ((*debugger-hook*
          (lambda (condition hook)
            (declare (ignore condition hook))
            nil)))
    (functionp
      *debugger-hook*))
  t)


;;
;;  Variable *BREAK-ON-SIGNALS*
;;
(deftest break-on-signals.1
  *break-on-signals*
  nil)

(deftest break-on-signals.2
  (lisp-system:specialp '*break-on-signals*)
  t)

