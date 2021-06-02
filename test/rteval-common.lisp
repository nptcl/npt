;;
;;  ANSI COMMON LISP: 3. Evaluation and Compilation
;;

;;
;;  Function EVAL
;;
(deftest eval.1
  (eval nil)
  nil)

(deftest eval.2
  (eval '(+ 1 2))
  3)

(deftest eval.3
  (progn
    (setq test-eval-value 10)
    (let ((test-eval-value 1000))
      (declare (ignorable test-eval-value))
      (eval '(1+ test-eval-value))))
  11)

(deftest eval.4
  (eval (list 'cdr (car '((quote (a . b)) c))))
  b)

(deftest eval.5
  (eval '(values 10 20 30))
  10 20 30)

(deftest-error! eval-error.1
  (eval '(eval)))

(deftest-error! eval-error.2
  (eval '(eval nil nil)))

(deftest-error eval-error.3
  (eval '(eval unbound-variable-error-test))
  unbound-variable)

;;  ANSI Common Lisp
(deftest eval-test.1
  (setq eval-test-form '(1+ a) a 999)
  999)

(deftest eval-test.2
  (eval eval-test-form)
  1000)

(deftest eval-test.3
  (eval 'eval-test-form)
  (1+ a))

(deftest eval-test.4
  (let ((a '(this would break if eval used local value)))
    (declare (ignorable a))
    (eval eval-test-form))
  1000)

(deftest eval-test.5
  (eval (list 'cdr (car '((quote (a . b)) c))))
  b)


;;
;;  Special Operator EVAL-WHEN
;;
(deftest eval-when.1
  (eval-when (:execute))
  nil)

(deftest eval-when.2
  (eval-when (:execute)
    10)
  10)

(deftest eval-when.3
  (eval-when (eval)
    10)
  10)

(deftest eval-when.4
  (eval-when ()
    10)
  nil)

(deftest eval-when.5
  (let (list)
    (eval-when (:execute)
      (push 'a list)
      (push 'b list)
      (nreverse list)))
  (a b))

(deftest eval-when.6
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (values 10 20 30))
  10 20 30)

(deftest eval-when.7
  (eval-when (compile load eval)
    (values 10 20 30))
  10 20 30)

(deftest-error! eval-when-error.1
  (eval '(eval-when)))

(deftest-error eval-when-error.2
  (eval '(eval-when 10)))

(deftest-error eval-when-error.3
  (eval '(eval-when (hello) nil)))


;;
;;  Special Operator QUOTE
;;
(deftest quote.1
  (quote aaa)
  aaa)

(deftest quote.2
  'hello
  hello)

(deftest quote.3
  ''a
  (quote a))

(deftest quote.4
  '"Hello"
  "Hello")

(deftest-error quote-error.1
  (eval '(quote)))

(deftest-error quote-error.2
  (eval '(quote nil nil)))

;;  ANSI Common Lisp
(defvar quote-test-a)
(deftest quote-test.1
  (setq quote-test-a 1)
  1)

(deftest quote-test.2
  (quote (setq quote-test-a 3))
  (setq quote-test-a 3))

(deftest quote-test.3
  quote-test-a
  1)

(deftest quote-test.4
  'quote-test-a
  quote-test-a)

(deftest quote-test.5
  ''quote-test-a
  (quote quote-test-a))

(deftest quote-test.6
  '''quote-test-a
  (quote (quote quote-test-a)))

(deftest quote-test.7
  (setq quote-test-a 43)
  43)

(deftest quote-test.8
  (list quote-test-a (cons quote-test-a 3))
  (43 (43 . 3)))

(deftest quote-test.9
  (list (quote quote-test-a) (quote (cons quote-test-a 3)))
  (quote-test-a (CONS quote-test-a 3)))

(deftest quote-test.10
  1
  1)

(deftest quote-test.11
  '1
  1)

(deftest quote-test.12
  "foo"
  "foo")

(deftest quote-test.13
  '"foo"
  "foo")

(deftest quote-test.14
  (car '(quote-test-a b))
  quote-test-a)

(deftest quote-test.15
  '(car '(quote-test-a b))
  (car (quote (quote-test-a b))))

(deftest quote-test.16
  #(car '(quote-test-a b))
  #(car (quote (quote-test-a b))))

(deftest quote-test.17
  '#(car '(quote-test-a b))
  #(car (quote (quote-test-a b))))


;;
;;  Function SPECIAL-OPERATOR-P
;;
(deftest special-operator-p.1
  (and (special-operator-p 'block)
       (special-operator-p 'catch)
       (special-operator-p 'eval-when)
       (special-operator-p 'flet)
       (special-operator-p 'function)
       (special-operator-p 'go)
       (special-operator-p 'if)
       (special-operator-p 'labels)
       (special-operator-p 'let)
       (special-operator-p 'let*)
       (special-operator-p 'load-time-value)
       (special-operator-p 'locally)
       (special-operator-p 'macrolet)
       (special-operator-p 'multiple-value-call)
       (special-operator-p 'multiple-value-prog1)
       (special-operator-p 'progn)
       (special-operator-p 'progv)
       (special-operator-p 'quote)
       (special-operator-p 'return-from)
       (special-operator-p 'setq)
       (special-operator-p 'symbol-macrolet)
       (special-operator-p 'tagbody)
       (special-operator-p 'the)
       (special-operator-p 'throw)
       (special-operator-p 'unwind-protect))
  t)

(deftest special-operator-p.2
  (special-operator-p :hello)
  nil)

(deftest-error! special-operator-p-error.1
  (eval '(special-operator-p)))

(deftest-error! special-operator-p-error.2
  (eval '(special-operator-p 'hello 'hello)))

(deftest-error special-operator-p-error.3
  (eval '(special-operator-p 10))
  type-error)

;;  ANSI Common Lisp
(deftest special-operator-p-test.1
  (special-operator-p 'if)
  t)

(deftest special-operator-p-test.2
  (special-operator-p 'car)
  nil)

(deftest special-operator-p-test.3
  (special-operator-p 'one)
  nil)


;;
;;  Function CONSTANTP
;;
(deftest constantp.1
  (constantp 1)
  t)

(deftest constantp.2
  (constantp 'temp)
  nil)

(deftest constantp.3
  (constantp ''temp)
  t)

(deftest constantp.4
  (constantp :hello nil)
  t)

(deftest constantp.5
  (symbol-macrolet ((a 10))
    (macrolet ((aaa (&environment env) (constantp 'a env)))
      (aaa)))
  t)

(deftest constantp.6
  (symbol-macrolet ((a 10))
    (constantp 'a))
  nil)

(defconstant constantp-test-1 100)
(deftest constantp.7
  (constantp 'constantp-test-1)
  t)

(deftest constantp.8
  (constantp nil)
  t)

(deftest constantp.9
  (constantp t)
  t)

(deftest constantp.10
  (constantp "temp")
  t)

(setq constantp-test-2 6)
(deftest constantp.11
  (constantp constantp-test-2)
  t)

(deftest-error! constantp-error.1
  (eval '(constantp)))

(deftest-error! constantp-error.2
  (eval '(constantp 10 nil 30)))

(deftest-error constantp-error.3
  (eval '(constantp 10 20))
  type-error)

