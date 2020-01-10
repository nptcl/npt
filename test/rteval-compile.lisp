;;
;;  ANSI COMMON LISP: 3. Evaluation and Compilation
;;

;;
;;  define-compiler-macro
;;
(deftest define-compiler-macro.1
  (define-compiler-macro define-compiler-macro1 () :hello)
  define-compiler-macro1)

(deftest define-compiler-macro.2
  (define-compiler-macro (setf define-compiler-macro2) (a) (+ a 10))
  (setf define-compiler-macro2))


;;
;;  compiler-macro-function
;;
(deftest compiler-macro-function.1
  (functionp
    (compiler-macro-function 'define-compiler-macro1))
  t)

(deftest compiler-macro-function.2
  (compiler-macro-function 'define-compiler-macro-error)
  nil)

(deftest compiler-macro-function.3
  (functionp
    (compiler-macro-function '(setf define-compiler-macro2)))
  t)

(deftest compiler-macro-function.4
  (compiler-macro-function '(setf define-compiler-macro-error))
  nil)

(deftest compiler-macro-function.5
  (functionp
    (setf (compiler-macro-function 'define-compiler-macro5) (lambda () :aaabbb)))
  t)

(deftest compiler-macro-function.6
  (functionp
    (compiler-macro-function 'define-compiler-macro5))
  t)

(deftest compiler-macro-function.7
  (functionp
    (setf (compiler-macro-function '(setf define-compiler-macro7))
          (lambda () :aaabbb)))
  t)

(deftest compiler-macro-function.8
  (functionp
    (compiler-macro-function '(setf define-compiler-macro7)))
  t)


;;
;;  compile
;;
(defun compile-test1 () :hello)

(deftest compile.1
  (handler-bind ((warning #'muffle-warning))
    (compile 'compile-test1))
  compile-test1 t nil)

(defun (setf compile-test2) () :hello)

(deftest compile.2
  (handler-bind ((warning #'muffle-warning))
    (compile '(setf compile-test2)))
  (setf compile-test2) t nil)

(deftest compile.3
  (handler-bind ((warning #'muffle-warning))
    (multiple-value-bind (x y z) (compile nil (lambda () :hello))
      (values (functionp x) y z)))
  t t nil)

(deftest compile.4
  (handler-bind ((warning #'muffle-warning))
    (funcall
      (compile nil (lambda () :hello))))
  :hello)

(deftest compile.5
  (handler-bind ((warning #'muffle-warning))
    (multiple-value-bind (x y z) (compile nil '(lambda () :abcde))
      (values (functionp x) y z)))
  t t nil)

(deftest compile.6
  (handler-bind ((warning #'muffle-warning))
    (funcall
      (compile nil '(lambda () :abcde))))
  :abcde)

(defun compile-test7 ()
  :test7)

(deftest compile.7
  (handler-bind ((warning #'muffle-warning))
    (compile 'compile-test7 (lambda () :test7a)))
  compile-test7 t nil)

(deftest compile.8
  (compile-test7)
  :test7a)

(defun (setf compile-test9) ()
  :test9)

(deftest compile.9
  (handler-bind ((warning #'muffle-warning))
    (compile '(setf compile-test9) (lambda () :test9a)))
  (setf compile-test9) t nil)

(deftest compile.10
  (funcall #'(setf compile-test9))
  :test9a)

(defun compile-test11 ()
  :test11)

(deftest compile.11
  (handler-bind ((warning #'muffle-warning))
    (compile 'compile-test11 '(lambda () :test11a)))
  compile-test11 t nil)

(deftest compile.12
  (compile-test11)
  :test11a)


;;
;;  compiler-macroexpand
;;
(define-compiler-macro compiler-macro1 (x)
  (+ x 100))

(defun compiler-macro1 (x)
  (+ x 200))

(deftest compiler-macro.1
  (eval '(compiler-macro1 300))
  500)

(deftest compiler-macro.2
  (handler-bind ((warning #'muffle-warning))
    (funcall
      (compile nil '(lambda () (compiler-macro1 300)))))
  400)

(defun compiler-macro3 (&rest args)
  (apply #'+ args))

(define-compiler-macro compiler-macro3 (&whole whole &rest args)
  (case (length args)
    (0 ''hello)
    (1 (car args))
    (t whole)))

(deftest compiler-macro.3
  (eval '(compiler-macro3 1 2 3))
  6)

(deftest compiler-macro.4
  (handler-bind ((warning #'muffle-warning))
    (funcall
      (compile nil '(lambda () (compiler-macro3)))))
  hello)

(deftest compiler-macro.5
  (handler-bind ((warning #'muffle-warning))
    (funcall
      (compile nil '(lambda () (compiler-macro3 999)))))
  999)

(deftest compiler-macro.6
  (handler-bind ((warning #'muffle-warning))
    (funcall
      (compile nil '(lambda () (compiler-macro3 1 2 3)))))
  6)


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

