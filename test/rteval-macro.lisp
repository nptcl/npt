;;
;;  ANSI COMMON LISP: 3. Evaluation and Compilation
;;

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

(deftest defmacro.7
  (defmacro defmacro-7 ()
    (return-from defmacro-7 100))
  defmacro-7)

(deftest defmacro.8
  (eval '(defmacro-7))
  100)

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
(deftest macroexpand-test.20
  (flet ((macroexpand-test-beta (x y) (+ x y)))
    (declare (ignorable #'macroexpand-test-beta))
    (macroexpand-test-expand (macroexpand-test-alpha a b)))
  (macroexpand-test-beta a b) t)

(deftest macroexpand-test.21
  (macrolet ((macroexpand-test-alpha (x y) `(macroexpand-test-delta ,x ,y)))
    (flet ((macroexpand-test-alpha (x y) (+ x y)))
      (declare (ignorable #'macroexpand-test-alpha))
      (macroexpand-test-expand (macroexpand-test-alpha a b))))
  (macroexpand-test-alpha a b) nil)

(deftest macroexpand-test.22
  (let ((x (list 1 2 3)))
    (declare (ignorable x))
    (symbol-macrolet ((a (first x)))
      (let ((a x))
        (declare (ignorable a))
        (macroexpand-test-expand a))))
  a nil)

(deftest macroexpand-test.23
  (let ((x (list 1 2 3)))
    (declare (ignorable x))
    (symbol-macrolet ((a (first x)))
      (funcall
        (lambda (a)
          (declare (ignorable a))
          (macroexpand-test-expand a))
        x)))
  a nil)

;;
;;  Macro DEFINE-SYMBOL-MACRO
;;
(deftest define-symbol-macro.1
  (define-symbol-macro define-symbol-macro-1 (car '(a . b)))
  define-symbol-macro-1)

(deftest define-symbol-macro.2
  (progn
    (define-symbol-macro define-symbol-macro-2 (car '(a . b)))
    define-symbol-macro-2)
  a)

(defvar *define-symbol-macro-1*)

(define-symbol-macro define-symbol-macro-3 (car *define-symbol-macro-1*))
(deftest define-symbol-macro.3
  (let ((*define-symbol-macro-1* (list 10 20 30)))
    define-symbol-macro-3)
  10)

(deftest define-symbol-macro.4
  (let ((*define-symbol-macro-1* (list 10 20 30)))
    (let ((define-symbol-macro-3 :hello))
      define-symbol-macro-3))
  :hello)

(deftest define-symbol-macro.5
  (let ((*define-symbol-macro-1* (list 10 20 30)))
    (setq define-symbol-macro-3 200)
    *define-symbol-macro-1*)
  (200 20 30))

(deftest define-symbol-macro.6
  (let ((*define-symbol-macro-1* (list 10 20 30)))
    (psetq define-symbol-macro-3 200)
    *define-symbol-macro-1*)
  (200 20 30))

(deftest define-symbol-macro.7
  (let ((*define-symbol-macro-1* (list 10 20 30)))
    (multiple-value-setq (define-symbol-macro-3) 200)
    *define-symbol-macro-1*)
  (200 20 30))

(defvar define-symbol-macro-4)
(deftest-error define-symbol-macro.8
  (eval '(define-symbol-macro define-symbol-macro-4 :hello))
  program-error)

(setq define-symbol-macro-5 100)
(deftest-error define-symbol-macro.9
  (eval '(define-symbol-macro define-symbol-macro-5 :hello))
  program-error)

(deftest define-symbol-macro.10
  (let ((*define-symbol-macro-1* (list 10 20 30)))
    (symbol-macrolet ((define-symbol-macro-3 :abc))
      define-symbol-macro-3))
  :abc)

(deftest-error define-symbol-macro-error.1
  (eval '(define-symbol-macro hello)))

(deftest-error define-symbol-macro-error.2
  (eval '(define-symbol-macro hello hello nil)))

(deftest-error define-symbol-macro-error.3
  (eval '(define-symbol-macro 100 nil)))

;;  ANSI Common Lisp
(defvar *define-symbol-macro-things* (list 'alpha 'beta 'gamma))
(define-symbol-macro define-symbol-macro-test-thing1
  (first *define-symbol-macro-things*))
(define-symbol-macro define-symbol-macro-thing2
  (second *define-symbol-macro-things*))
(define-symbol-macro define-symbol-macro-thing3
  (third *define-symbol-macro-things*))

(deftest define-symbol-macro-test.1
  define-symbol-macro-test-thing1
  alpha)

(deftest define-symbol-macro-test.2
  (setq define-symbol-macro-test-thing1 'one)
  one)

(deftest define-symbol-macro-test.3
  *define-symbol-macro-things*
  (one beta gamma))

(deftest define-symbol-macro-test.4
  (multiple-value-setq
    (define-symbol-macro-thing2 define-symbol-macro-thing3)
    (values 'two 'three))
  two)

(deftest define-symbol-macro-test.5
  define-symbol-macro-thing3
  three)

(deftest define-symbol-macro-test.6
  *define-symbol-macro-things*
  (one two three))

(deftest define-symbol-macro-test.7
  (list define-symbol-macro-thing2
        (let ((define-symbol-macro-thing2 2))
          define-symbol-macro-thing2))
  (two 2))


;;
;;  Special Operator SYMBOL-MACROLET
;;
(deftest symbol-macrolet.1
  (symbol-macrolet ())
  nil)

(deftest symbol-macrolet.2
  (symbol-macrolet ((a 'hello))
    a)
  hello)

(deftest symbol-macrolet.3
  (let ((*value* (list 10 20 30)))
    (declare (special *value*))
    (symbol-macrolet ((a (car *value*)))
      (+ a 40)))
  50)

(deftest symbol-macrolet.4
  (let ((*value* (list 10 20 30)))
    (declare (special *value*))
    (symbol-macrolet ((a (car *value*)))
      (setq a 999))
    *value*)
  (999 20 30))

(deftest symbol-macrolet.5
  (let ((*value* (list 10 20 30)))
    (declare (special *value*))
    (symbol-macrolet ((a (car *value*)))
      (psetq a 999))
    *value*)
  (999 20 30))

(deftest symbol-macrolet.6
  (let ((*value* (list 10 20 30)))
    (declare (special *value*))
    (symbol-macrolet ((a (car *value*)))
      (multiple-value-setq (a) 999))
    *value*)
  (999 20 30))

(deftest symbol-macrolet.7
  (let ((*value* (list 10 20 30)))
    (declare (special *value*))
    (symbol-macrolet ((a (car *value*)))
      (let ((a :hello))
        a)))
  :hello)

(deftest symbol-macrolet.8
  (let ((*value* (list 10 20 30)))
    (declare (special *value*))
    (symbol-macrolet ((a (car *value*)))
      (symbol-macrolet ((a (cdr *value*)))
        a)))
  (20 30))

(defvar *symbol-macrolet-test-1*)
(deftest-error symbol-macrolet.9
  (eval '(symbol-macrolet ((*symbol-macrolet-test-1* :hello))
           *symbol-macrolet-test-1*))
  program-error)

(deftest-error symbol-macrolet.10
  (eval '(symbol-macrolet ((aaa :hello)
                           (bbb :abc))
           (declare (special bbb))
           aaa bbb))
  program-error)

(deftest-error symbol-macrolet-error.1
  (eval '(symbol-macrolet)))

(deftest-error symbol-macrolet-error.2
  (eval '(symbol-macrolet (10))))

(deftest-error symbol-macrolet-error.3
  (eval '(symbol-macrolet 20)))


;;  ANSI Common Lisp
(deftest symbol-macrolet-test.1
  (symbol-macrolet ((x 'foo))
    (list x (let ((x 'bar)) x)))
  (foo bar))

(deftest symbol-macrolet-test.2
  (symbol-macrolet ((x '(foo x)))
    (list x))
  ((foo x)))


;;
;;  Variable *MACROEXPAND-HOOK*
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

(defun macroexpand-hook-call-1 (call x e)
  (if (and (consp x) (eq (car x) 'macroexpand-hook1))
    :hello
    (funcall call x e)))

(deftest macroexpand-hook.5
  (let ((*macroexpand-hook* 'macroexpand-hook-call-1))
    (macroexpand '(macroexpand-hook1 10)))
  :hello t)

(deftest-error macroexpand-hook.6
  (let ((*macroexpand-hook* 100))
    (macroexpand '(macroexpand-hook1 10))))

;;  ANSI Common Lisp
(defvar *macroexpand-hook-result*)
(defun macroexpand-hook-call-2 (expander form env)
  (setq *macroexpand-hook-result* (format nil "Now expanding: ~S" form))
  (funcall expander form env))

(defmacro macroexpand-hook-machook (x y)
  `(/ (+ ,x ,y) 2))

(deftest macroexpand-hook-test.1
  (macroexpand '(macroexpand-hook-machook 1 2))
  (/ (+ 1 2) 2) t)

(deftest macroexpand-hook-test.2
  (let ((*macroexpand-hook* #'macroexpand-hook-call-2)
        *macroexpand-hook-result*)
    (macroexpand '(macroexpand-hook-machook 1 2)))
  (/ (+ 1 2) 2) t)

(deftest macroexpand-hook-test.3
  (let ((*macroexpand-hook* #'macroexpand-hook-call-2)
        *macroexpand-hook-result*)
    (macroexpand '(macroexpand-hook-machook 1 2))
    *macroexpand-hook-result*)
  "Now expanding: (MACROEXPAND-HOOK-MACHOOK 1 2)")

