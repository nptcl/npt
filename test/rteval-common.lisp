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
;;  Macro DEFMACRO
;;
(deftest defmacro.1
  (defmacro defmacro-1 () :hello)
  defmacro-1)

(deftest defmacro.2
  (fboundp 'defmacro-1)
  t)

(defmacro defmacro-2 () :hello)
(deftest defmacro.3
  (defmacro-2)
  :hello)

(defmacro defmacro-3 () :hello)
(defmacro defmacro-3 (a b c) `(+ ,a ,b ,c))
(deftest defmacro.4
  (defmacro-3 10 20 30)
  60)

(defmacro defmacro-4 (a b c)
  "Hello"
  `(* ,a ,b ,c))

(deftest defmacro.5
  (documentation 'defmacro-4 'function)
  "Hello")

(defmacro defmacro-5 ())
(deftest defmacro.6
  (defmacro-5)
  nil)

(deftest-error defmacro-error.1
  (eval '(defmacro name)))

(deftest-error defmacro-error.2
  (eval '(defmacro name 10)))

;;  ANSI Common Lisp
(defmacro defmacro-test-mac1 (a b)
  "Mac1 multiplies and adds"
  `(+ ,a (* ,b 3)))

(deftest defmacro-test.1
  (defmacro-test-mac1 4 5)
  19)

(deftest defmacro-test.2
  (documentation 'defmacro-test-mac1 'function)
  "Mac1 multiplies and adds")

(defmacro defmacro-test-mac2 (&optional (a 2 b) (c 3 d) &rest x)
  `'(,a ,b ,c ,d ,x))

(deftest defmacro-test.3
  (defmacro-test-mac2 6)
  (6 t 3 nil nil))

(deftest defmacro-test.4
  (defmacro-test-mac2 6 3 8)
  (6 t 3 t (8)))

(defmacro defmacro-test-mac3 (&whole r a &optional (b 3) &rest x &key c (d a))
  `'(,r ,a ,b ,c ,d ,x))

(deftest defmacro-test.5
  (defmacro-test-mac3 1 6 :d 8 :c 9 :d 10)
  ((defmacro-test-mac3 1 6 :d 8 :c 9 :d 10) 1 6 9 8 (:d 8 :c 9 :d 10)))

(defmacro defmacro-test-dm1a (&whole x)
  `',x)

(deftest defmacro-test.6
  (values
    (macroexpand '(defmacro-test-dm1a)))
  (quote (defmacro-test-dm1a)))

(deftest-error defmacro-test.7
  (macroexpand '(defmacro-test-dm1a a)))

(defmacro defmacro-test-dm1b (&whole x a &optional b)
  `'(,x ,a ,b))

(deftest-error defmacro-test.8
  (macroexpand '(defmacro-test-dm1b)))

(deftest defmacro-test.9
  (values
    (macroexpand '(defmacro-test-dm1b q)))
  (quote ((defmacro-test-dm1b q) q nil)))

(deftest defmacro-test.10
  (values
    (macroexpand '(defmacro-test-dm1b q r)))
  (quote ((defmacro-test-dm1b q r) q r)))

(deftest-error defmacro-test.11
  (macroexpand '(defmacro-test-dm1b q r s)))

(defmacro defmacro-test-dm2a (&whole form a b)
  `'(form ,form a ,a b ,b))

(deftest defmacro-test.12
  (values
    (macroexpand '(defmacro-test-dm2a x y)))
  (quote (form (defmacro-test-dm2a x y) a x b y)))

(deftest defmacro-test.13
  (defmacro-test-dm2a x y)
  (form (defmacro-test-dm2a x y) a x b y))

(defmacro defmacro-test-dm2b
  (&whole form a (&whole b (c . d) &optional (e 5)) &body f &environment env)
  ``(,',form ,,a ,',b ,',(macroexpand c env) ,',d ,',e ,',f))

(deftest defmacro-test.14
  (let ((x1 5))
    (macrolet ((segundo (x) `(cadr ,x)))
      (defmacro-test-dm2b x1 (((segundo x2) x3 x4)) x5 x6)))
  ((defmacro-test-dm2b x1 (((segundo x2) x3 x4)) x5 x6)
   5 (((segundo x2) x3 x4)) (cadr x2) (x3 x4) 5 (x5 x6)))


;;
;;  Accessor MACRO-FUNCTION
;;
(deftest macro-function.1
  (functionp
    (macro-function 'dolist))
  t)

(deftest macro-function.2
  (functionp
    (macro-function 'dolist nil))
  t)

(deftest macro-function.3
  (macro-function 'no-such-macro-function)
  nil)

(defmacro macro-function-test-1 ()
  :hello)

(deftest macro-function.4
  (funcall
    (macro-function 'macro-function-test-1)
    nil nil)
  :hello)

(deftest macro-function.5
  (macrolet ((macro-function-test-2 () :hello))
    (macro-function 'macro-function-test-2))
  nil)

(deftest-error! macro-function-error.1
  (eval '(macro-function)))

(deftest-error! macro-function-error.2
  (eval '(macro-function 'hello nil nil)))

(deftest-error macro-function-error.3
  (eval '(macro-function 10))
  type-error)


;;
;;  Accessor (SETF MACRO-FUNCTION)
;;
(deftest macro-function-setf.1
  (functionp
    (setf (macro-function 'macro-function-setf-1) (constantly :hello)))
  t)

(deftest macro-function-setf.2
  (funcall
    (macro-function 'macro-function-setf-1))
  :hello)

(deftest-error macro-function-setf-error.1
  (eval '(setf (macro-function) (constantly 10))))

(deftest-error macro-function-setf-error.2
  (eval '(setf (macro-function 10) (constantly 10)))
  type-error)

;;  ANSI Common Lisp
(defmacro macrofunction-test-macfun (x)
  (declare (ignorable x))
  '(macro-function 'macrofunction-test-macfun))

(deftest macro-function-test.1
  (not (macro-function 'macrofunction-test-macfun))
  nil)

(deftest macro-function-test.2
  (macrolet ((foo (&environment env)
                  (if (macro-function 'bar env)
                    ''yes
                    ''no)))
    (list (foo) (macrolet ((bar () :beep))
                  (foo))))
  (no yes))


;;
;;  Function MACROEXPAND
;;
(defmacro macroexpand-test-1 (&rest args)
  (declare (ignorable args))
  :hello)

(deftest macroexpand.1
  (macroexpand '(macroexpand-test-1))
  :hello t)

(deftest macroexpand.2
  (macroexpand 10)
  10 nil)

(deftest macroexpand.3
  (macroexpand '(no-such-macro-function 100 200 300))
  (no-such-macro-function 100 200 300) nil)

(deftest-error! macroexpand-error.1
  (eval '(macroexpand)))

(deftest-error! macroexpand-error.2
  (eval '(macrolet ((aaa (&environment env) (macroexpand 'aaa env nil) t))
           (aaa))))

(deftest-error macroexpand-error.3
  (eval '(macroexpand 10 20))
  type-error)


;;
;;  Function MACROEXPAND-1
;;
(deftest macroexpand-1.1
  (macroexpand-1 '(macroexpand-test-1))
  :hello t)

(deftest macroexpand-1.2
  (macroexpand-1 10)
  10 nil)

(deftest macroexpand-1.3
  (macroexpand-1 '(no-such-macro-function 100 200 300))
  (no-such-macro-function 100 200 300) nil)

(deftest-error! macroexpand-1-error.1
  (eval '(macroexpand-1)))

(deftest-error! macroexpand-1-error.2
  (eval '(macrolet ((aaa (&environment env) (macroexpand-1 'aaa env nil) t))
           (aaa))))

(deftest-error macroexpand-1-error.3
  (eval '(macroexpand-1 10 20))
  type-error)

;;  ANSI Common Lisp
(defmacro macroexpand-test-alpha (x y)
  `(macroexpand-test-beta ,x ,y))

(defmacro macroexpand-test-beta (x y)
  `(gamma ,x ,y))

(defmacro macroexpand-test-delta (x y)
  `(gamma ,x ,y))

(defmacro macroexpand-test-expand (form &environment env)
  (multiple-value-bind (expansion expanded-p) (macroexpand form env)
    `(values ',expansion ',expanded-p)))

(defmacro macroexpand-test-expand-1 (form &environment env)
  (multiple-value-bind (expansion expanded-p) (macroexpand-1 form env)
    `(values ',expansion ',expanded-p)))

(deftest macroexpand-test.1
  (macroexpand-1
    '(macroexpand-test-alpha a b))
  (macroexpand-test-beta a b) t)

(deftest macroexpand-test.2
  (macroexpand-test-expand-1
    (macroexpand-test-alpha a b))
  (macroexpand-test-beta a b) t)

(deftest macroexpand-test.3
  (macroexpand
    '(macroexpand-test-alpha a b))
  (gamma a b) t)

(deftest macroexpand-test.4
  (macroexpand-test-expand
    (macroexpand-test-alpha a b))
  (gamma a b) t)

(deftest macroexpand-test.5
  (macroexpand-1 'not-a-macro)
  not-a-macro nil)

(deftest macroexpand-test.6
  (macroexpand-test-expand-1 not-a-macro)
  not-a-macro nil)

(deftest macroexpand-test.7
  (macroexpand '(not-a-macro a b))
  (not-a-macro a b) nil)

(deftest macroexpand-test.8
  (macroexpand-test-expand (not-a-macro a b))
  (not-a-macro a b) nil)

(deftest macroexpand-test.9
  (macrolet ((macroexpand-test-alpha (x y) `(macroexpand-test-delta ,x ,y)))
    (macroexpand-1 '(macroexpand-test-alpha a b)))
  (macroexpand-test-beta a b) t)

(deftest macroexpand-test.10
  (macrolet ((macroexpand-test-alpha (x y) `(macroexpand-test-delta ,x ,y)))
    (macroexpand-test-expand-1 (macroexpand-test-alpha a b)))
  (macroexpand-test-delta a b) t)

(deftest macroexpand-test.11
  (macrolet ((macroexpand-test-alpha (x y) `(macroexpand-test-delta ,x ,y)))
    (macroexpand '(macroexpand-test-alpha a b)))
  (gamma a b) t)

(deftest macroexpand-test.12
  (macrolet ((macroexpand-test-alpha (x y) `(macroexpand-test-delta ,x ,y)))
    (macroexpand-test-expand (macroexpand-test-alpha a b)))
  (gamma a b) t)

(deftest macroexpand-test.13
  (macrolet ((macroexpand-test-beta (x y) `(epsilon ,x ,y)))
    (macroexpand-test-expand (macroexpand-test-alpha a b)))
  (epsilon a b) t)

(deftest macroexpand-test.14
  (let ((x (list 1 2 3)))
    (declare (ignorable x))
    (symbol-macrolet ((a (first x)))
      (macroexpand-test-expand a)))
  (first x) t)

(deftest macroexpand-test.15
  (let ((x (list 1 2 3)))
    (declare (ignorable x))
    (symbol-macrolet ((a (first x)))
      (macroexpand 'a)))
  a nil)

(deftest macroexpand-test.16
  (symbol-macrolet ((b (macroexpand-test-alpha x y)))
    (macroexpand-test-expand-1 b))
  (macroexpand-test-alpha X Y) t)

(deftest macroexpand-test.17
  (symbol-macrolet ((b (macroexpand-test-alpha x y)))
    (macroexpand-test-expand b))
  (gamma x y) t)

(deftest macroexpand-test.18
  (symbol-macrolet ((b (macroexpand-test-alpha x y))
                    (a b))
    (macroexpand-test-expand-1 a))
  b t)

(deftest macroexpand-test.19
  (symbol-macrolet ((b (macroexpand-test-alpha x y))
                    (a b))
    (macroexpand-test-expand a))
  (gamma x y) t)

;;  shadowing
;(deftest macroexpand-test.20
;  (flet ((macroexpand-test-beta (x y) (+ x y)))
;    (macroexpand-test-expand (macroexpand-test-alpha a b)))
;  (macroexpand-test-beta a b) t)
;
;(deftest macroexpand-test.21
;  (macrolet ((macroexpand-test-alpha (x y) `(macroexpand-test-delta ,x ,y)))
;    (flet ((macroexpand-test-alpha (x y) (+ x y)))
;      (macroexpand-test-expand (macroexpand-test-alpha a b))))
;  (macroexpand-test-alpha a b) nil)
;
;(deftest macroexpand-test.22
;  (let ((x (list 1 2 3)))
;    (symbol-macrolet ((a (first x)))
;      (let ((a x))
;        (macroexpand-test-expand a))))
;  a nil)


;;
;;  macroexpand-hook
;;
(defmacro macroexpand-hook1 (x)
  (format nil "<<<~A>>>" x))

(deftest macroexpand-hook.1
  (let ((*macroexpand-hook*
          (lambda (call x e)
            (if (and (consp x) (eq (car x) 'macroexpand-hook1))
              :hello
              (funcall call x e)))))
    (macroexpand '(macroexpand-hook1 10)))
  :hello t)

(deftest macroexpand-hook.2
  (macroexpand '(macroexpand-hook1 10))
  "<<<10>>>" t)

(define-compiler-macro macroexpand-hook3 (x)
  (format nil "+++~A+++" x))

(deftest macroexpand-hook.3
  (let ((*macroexpand-hook*
          (lambda (call x e)
            (if (and (consp x) (eq (car x) 'macroexpand-hook3))
              :hello
              (funcall call x e)))))
    (handler-bind ((warning #'muffle-warning))
      (funcall
        (compile nil '(lambda () (macroexpand-hook3 20))))))
  :hello)

(deftest macroexpand-hook.4
  (handler-bind ((warning #'muffle-warning))
    (funcall
      (compile nil '(lambda () (macroexpand-hook3 20)))))
  "+++20+++")


;;
;;  define-symbol-macro
;;
(deftest define-symbol-macro.1
  (define-symbol-macro define-symbol-macro1 (car '(a . b)))
  define-symbol-macro1)

(deftest define-symbol-macro.2
  (progn
    (define-symbol-macro define-symbol-macro2 (car '(a . b)))
    define-symbol-macro2)
  a)


;;
;;  special-operator-p
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


;;
;;  constantp
;;
(deftest constantp.1
  (constantp 1)
  t)

(deftest constantp.2
  (constantp ''temp)
  t)


;;
;;  proclaim
;;
(deftest proclaim.1
  (proclaim nil)
  nil)

(deftest proclaim.2
  (progn
    (proclaim '(special proclaim-test2 proclaim-test2a proclaim-test2b))
    (lisp-system::specialp 'proclaim-test2))
  t)

(deftest proclaim.3
  (progn
    (proclaim '(type integer proclaim-test3))
    (eval '(setq proclaim-test3 100))
    (eval 'proclaim-test3))
  100)

(deftest proclaim.4
  (proclaim '(ftype function proclaim-test4))
  nil)

(deftest proclaim.5
  (proclaim '(inline proclaim-test5 (setf proclaim-test5a)))
  nil)

(deftest proclaim.6
  (proclaim '(notinline proclaim-test6 (setf proclaim-test6a)))
  nil)

(deftest proclaim.7
  (proclaim '(optimize (compilation-speed 0)
                       (debug 1)
                       (space 2)
                       (speed 3)
                       safety))
  nil)

(deftest proclaim.8
  (proclaim '(declaration hello-proclaim-test8))
  nil)

