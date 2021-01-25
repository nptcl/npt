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
;;  Special Operator LOAD-TIME-VALUE
;;
(deftest load-time-value.1
  (load-time-value 10)
  10)

(deftest load-time-value.2
  (load-time-value "Hello" t)
  "Hello")

(deftest load-time-value.3
  (load-time-value #\A nil)
  #\A)

(deftest load-time-value.4
  (load-time-value (+ 10 20 30) :hello)
  60)

(deftest load-time-value.5
  (+ (load-time-value 10)
     (load-time-value 20))
  30)

(deftest load-time-value.6
  (values
    (eql (load-time-value 10) 10)
    (eql (load-time-value 20) 20)
    (eql (load-time-value 30) 30)
    (equal (load-time-value "Hello") "Hello")
    (equal (load-time-value (concatenate 'string "AA" "BB" "CC")) "AABBCC"))
  t t t t t)

(deftest load-time-value.7
  (+ (eval '(load-time-value (+ 10 20)))
     (load-time-value 40))
  70)

(deftest-error load-time-value.8
  (eval '(let ((x 10))
           (declare (ignorable x))
           (load-time-value (+ x 20)))))

(defvar *load-time-value-test-1* nil)
(deftest load-time-value.9
  (eval '(progn
           (push (load-time-value
                   (progn
                     (push 10 *load-time-value-test-1*)
                     20))
                 *load-time-value-test-1*)
           *load-time-value-test-1*))
  (20 10))

(defvar *load-time-value-test-2* 10)
(deftest load-time-value.10
  (eval '(let (x)
           (dotimes (i 20 x)
             (setq x (load-time-value
                       (incf *load-time-value-test-2* 1))))))
  11)

(deftest load-time-value.11
  (let ((x (compile nil '(lambda () (load-time-value 10)))))
    (funcall x))
  10)

(defvar *load-time-value-test*)
(deftest load-time-value.12
  (progn
    (load #p"test/rteval-file1.lisp")
    (prog1
      (funcall *load-time-value-test*)
      (makunbound '*load-time-value-test*)))
  20)

(deftest load-time-value.13
  (let ((file #p"test/rteval-file2.fasl"))
    (compile-file #p"test/rteval-file2.lisp" :output-file file)
    (load file)
    (prog1
      (funcall *load-time-value-test*)
      (makunbound '*load-time-value-test*)
      (delete-file file)))
  30)

(deftest-error load-time-value-error.1
  (eval '(load-time-value)))

(deftest-error load-time-value-error.2
  (eval '(load-time-value 10 t t)))


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

