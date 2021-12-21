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

(deftest simple-condition-format-arguments.3
  (let ((x (make-condition 'simple-condition :format-control "Hello")))
    (simple-condition-format-arguments x))
  nil)

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


;;
;;  Function ABORT
;;
(deftest abort.1
  (restart-case
    (abort)
    (abort () 'hello))
  hello)

(deftest abort.2
  (restart-case
    (abort nil)
    (abort () 'hello))
  hello)

(deftest abort.3
  (let ((x (make-condition 'program-error))
        (y (make-condition 'program-error)))
    (restart-case
      (with-condition-restarts
        x
        (list (find-restart 'abort))
        (restart-case
          (with-condition-restarts
            y
            (list (find-restart 'abort))
            (abort x))
          (abort () 100)))
      (abort () 200)))
  200)

(deftest-error! abort-error.1
  (eval '(abort 10)))

(deftest-error! abort-error.2
  (eval '(abort nil nil)))


;;
;;  Function CONTINUE
;;
(define-condition testcond () ((aaa :initarg :aaa)))
(deftest continue.1
  (continue)
  nil)

(deftest continue.2
  (continue nil)
  nil)

(deftest continue.3
  (restart-case
    (continue)
    (continue () 'hello))
  hello)

(deftest continue.4
  (restart-case
    (continue nil)
    (continue () 'hello))
  hello)

(deftest continue.5
  (let ((x (make-condition 'program-error))
        (y (make-condition 'program-error)))
    (restart-case
      (with-condition-restarts
        x
        (list (find-restart 'continue))
        (restart-case
          (with-condition-restarts
            y
            (list (find-restart 'continue))
            (continue x))
          (continue () 100)))
      (continue () 200)))
  200)

(deftest-error! continue-error.1
  (eval '(continue 10)))

(deftest-error! continue-error.2
  (eval '(continue nil nil)))


;;
;;  Function MUFFLE-WARNING
;;
(deftest-error muffle-warning.1
  (muffle-warning)
  control-error)

(deftest-error muffle-warning.2
  (muffle-warning nil)
  control-error)

(deftest-error muffle-warning.3
  (muffle-warning
    (make-condition 'testcond :aaa 10))
  control-error)

(deftest muffle-warning.4
  (let ((x (make-condition 'program-error))
        (y (make-condition 'program-error)))
    (restart-case
      (with-condition-restarts
        x
        (list (find-restart 'muffle-warning))
        (restart-case
          (with-condition-restarts
            y
            (list (find-restart 'muffle-warning))
            (muffle-warning x))
          (muffle-warning () 100)))
      (muffle-warning () 200)))
  200)

(deftest muffle-warning.5
  (restart-case
    (muffle-warning)
    (muffle-warning () 10))
  10)

(deftest muffle-warning.6
  (restart-case
    (muffle-warning
      (make-condition 'simple-error))
    (muffle-warning () 10))
  10)

(deftest muffle-warning.7
  (handler-bind ((warning #'muffle-warning))
    (warn "Hello"))
  nil)

(deftest muffle-warning.8
  (with-output-to-string (*error-output*)
    (handler-bind ((warning #'muffle-warning))
      (warn "Hello")))
  "")

(deftest-error! muffle-warning-error.1
  (eval '(muffle-warning 10)))

(deftest-error! muffle-warning-error.2
  (eval '(muffle-warning nil nil)))


;;
;;  Function STORE-VALUE
;;
(deftest store-value.1
  (store-value 11)
  nil)

(deftest store-value.2
  (store-value 11 nil)
  nil)

(deftest store-value.3
  (store-value 11 (make-condition 'program-error))
  nil)

(deftest store-value.4
  (restart-case
    (store-value 11)
    (store-value (x)
      (1+ x)))
  12)

(deftest store-value.5
  (let ((x (make-condition 'program-error))
        (y (make-condition 'program-error)))
    (restart-case
      (with-condition-restarts
        x
        (list (find-restart 'store-value))
        (restart-case
          (with-condition-restarts
            y
            (list (find-restart 'store-value))
            (store-value 10 x))
          (store-value (x) (+ x 100))))
      (store-value (x) (+ x 200))))
  210)

(deftest-error! store-value-error.1
  (eval '(store-value 10 10)))

(deftest-error! store-value-error.2
  (eval '(store-value 10 nil nil)))


;;
;;  Function USE-VALUE
;;
(deftest use-value.1
  (use-value 11)
  nil)

(deftest use-value.2
  (use-value 11 nil)
  nil)

(deftest use-value.3
  (use-value 11 (make-condition 'program-error))
  nil)

(deftest use-value.4
  (restart-case
    (use-value 11)
    (use-value (x)
      (1+ x)))
  12)

(deftest use-value.5
  (let ((x (make-condition 'program-error))
        (y (make-condition 'program-error)))
    (restart-case
      (with-condition-restarts
        x
        (list (find-restart 'use-value))
        (restart-case
          (with-condition-restarts
            y
            (list (find-restart 'use-value))
            (use-value 10 x))
          (use-value (x) (+ x 100))))
      (use-value (x) (+ x 200))))
  210)

(deftest-error! use-value-error.1
  (eval '(use-value 10 10)))

(deftest-error! use-value-error.2
  (eval '(use-value 10 nil nil)))


;;
;;  ANSI Common Lisp
;;

;;; Example of the ABORT retart
(defmacro abort-on-error (&body forms)
  `(handler-bind ((error #'abort))
     ,@forms))

(deftest abort-test.1
  (abort-on-error (+ 3 5))
  8)

(deftest abort-test.2
  (restart-case
    (abort-on-error (error "You lose."))
    (abort () 'hit))
  hit)


;;; Example of the CONTINUE restart
(defun continue-real-sqrt (n)
  (when (minusp n)
    (setq n (- n))
    (cerror "Return sqrt(~D) instead." "Tried to take sqrt(-~D)." n))
  (isqrt n))

(deftest continue-test.1
  (continue-real-sqrt 4)
  2)

(deftest continue-test.2
  (handler-bind ((error #'(lambda (c)
                            (declare (ignore c))
                            (continue))))
    (continue-real-sqrt -9))
  3)


;;; Example of the MUFFLE-WARNING restart
(defun muffle-warning-count-down (x)
  (let (list)
    (do ((counter x (1- counter)))
      ((= counter 0) 'done)
      (when (= counter 1)
        (warn "Almost done"))
      (push counter list))
    (nreverse list)))

(deftest muffle-warning-test.1
  (let (value)
    (values
      (and (search
             "Almost"
             (with-output-to-string (*error-output*)
               (setq value (muffle-warning-count-down 3))))
           t)
      value))
  t (3 2 1))

(defun muffle-warning-ignore-warnings-while-counting (x)
  (handler-bind ((warning #'muffle-warning-ignore-warning))
    (muffle-warning-count-down x)))

(defun muffle-warning-ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))

(deftest muffle-warning-test.2
  (let (value)
    (values
      (with-output-to-string (*error-output*)
        (setq value (muffle-warning-ignore-warnings-while-counting 3)))
      value))
  "" (3 2 1))


;;; Example of the STORE-VALUE and USE-VALUE restarts
(defun store-value-careful-symbol-value (symbol)
  (check-type symbol symbol)
  (restart-case (if (boundp symbol)
                  (return-from store-value-careful-symbol-value
                               (symbol-value symbol))
                  (error 'unbound-variable
                    :name symbol))
    (use-value (value)
      :report "Specify a value to use this time."
      value)
    (store-value (value)
      :report "Specify a value to store and use in the future."
      (setf (symbol-value symbol) value))))

(defvar store-value-a)

(deftest store-value-test.1
  (progn
    (setq store-value-a 1234)
    (store-value-careful-symbol-value 'store-value-a))
  1234)

(deftest store-value-test.2
  (handler-bind
    ((unbound-variable
       (lambda (c)
         (declare (ignore c))
         (use-value 12))))
    (makunbound 'store-value-a)
    (store-value-careful-symbol-value 'store-value-a))
  12)

(deftest store-value-test.3
  (handler-bind
    ((unbound-variable
       (lambda (c)
         (declare (ignore c))
         (store-value 24))))
    (store-value-careful-symbol-value 'store-value-a))
  24)

(deftest store-value-test.4
  (store-value-careful-symbol-value 'store-value-a)
  24)


;;; Example of the USE-VALUE restart
(defun use-value-add-symbols-with-default (default &rest symbols)
  (handler-bind ((unbound-variable
                   #'(lambda (c)
                       (declare (ignore c))
                       (use-value default))))
    (apply #'+ (mapcar #'store-value-careful-symbol-value symbols))))

(deftest use-value-test.1
  (progn
    (makunbound 'use-value-a)
    (makunbound 'use-value-b)
    (makunbound 'use-value-c)
    (setq use-value-x 1 use-value-y 2)
    (use-value-add-symbols-with-default
      3 'use-value-x 'use-value-y 'use-value-z))
  6)

