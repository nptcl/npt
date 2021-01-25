;;
;;  ANSI COMMON LISP: 3. Evaluation and Compilation
;;

;;
;;  Function COMPILE
;;
(defun compile-test-1 () :hello)

(deftest compile.1
  (compile 'compile-test-1)
  compile-test-1 nil nil)

(defun (setf compile-test-2) () :hello)

(deftest compile.2
  (compile '(setf compile-test-2))
  (setf compile-test-2) nil nil)

(deftest compile.3
  (multiple-value-bind (x y z) (compile nil (lambda () :hello))
    (values (functionp x) y z))
  t nil nil)

(deftest compile.4
  (funcall
    (compile nil (lambda () :hello)))
  :hello)

(deftest compile.5
  (multiple-value-bind (x y z) (compile nil '(lambda () :abcde))
    (values (functionp x) y z))
  t nil nil)

(deftest compile.6
  (funcall
    (compile nil '(lambda () :abcde)))
  :abcde)

(defmacro compile-test-7 () :hello)

(deftest compile.7
  (compile 'compile-test-7)
  compile-test-7 nil nil)

(defun compile-test-8 ()
  :test-8a)

(deftest compile.8
  (compile 'compile-test-8 (lambda () :test-8b))
  compile-test-8 nil nil)

(deftest compile.9
  (compile-test-8)
  :test-8b)

(defun (setf compile-test-10) ()
  :test-10a)

(deftest compile.10
  (compile '(setf compile-test-10) (lambda () :test-10b))
  (setf compile-test-10) nil nil)

(deftest compile.11
  (funcall #'(setf compile-test-10))
  :test-10b)

(defun compile-test-12 ()
  :test-12a)

(deftest compile.12
  (compile 'compile-test-12 '(lambda () :test-12b))
  compile-test-12 nil nil)

(deftest compile.13
  (compile-test-12)
  :test-12b)

(defmacro compile-test-14 ()
  (warn "Hello")
  :hello)

(deftest compile.14
  (handler-bind ((warning #'muffle-warning))
    (multiple-value-bind (x y z) (compile nil '(lambda () (compile-test-14)))
      (values (functionp x) y z)))
  t t t)

(defmacro compile-test-15 ()
  (warn (make-condition 'style-warning))
  :hello)

(deftest compile.15
  (handler-bind ((warning #'muffle-warning))
    (multiple-value-bind (x y z) (compile nil '(lambda () (compile-test-15)))
      (values (functionp x) y z)))
  t t nil)

(deftest-error! compile-error.1
  (eval '(compile)))

(deftest-error! compile-error.2
  (eval '(compile nil '(lambda () :hello) nil)))

(deftest-error compile-error.3
  (eval '(compile 'no-such-function-name)))

(deftest-error compile-error.4
  (eval '(compile 10))
  type-error)

(deftest-error compile-error.5
  (eval '(compile nil '(hello))))

;;  ANSI Common Lisp
(defun compile-test-foo () "bar")

(deftest compile-test.1
  (compiled-function-p #'compile-test-foo)
  nil)

(deftest compile-test.2
  (values
    (compile 'compile-test-foo))
  compile-test-foo)

(deftest compile-test.3
  (compiled-function-p #'compile-test-foo)
  nil)  ;; t

(deftest compile-test.4
  (progn
    (setf (symbol-function 'compile-test-foo)
          (compile nil '(lambda () "replaced")))
    (compile-test-foo))
  "replaced")


;;
;;  Accessor COMPILER-MACRO-FUNCTION
;;
(define-compiler-macro compiler-macro-function-1 () :hello)
(deftest compiler-macro-function.1
  (functionp
    (compiler-macro-function 'compiler-macro-function-1))
  t)

(deftest compiler-macro-function.2
  (compiler-macro-function 'define-compiler-macro-error)
  nil)

(define-compiler-macro (setf compiler-macro-function-2) (a) (+ a 10))
(deftest compiler-macro-function.3
  (functionp
    (compiler-macro-function '(setf compiler-macro-function-2)))
  t)

(deftest compiler-macro-function.4
  (compiler-macro-function '(setf define-compiler-macro-error))
  nil)

(deftest-error! compiler-macro-function-error.1
  (eval '(compiler-macro-function)))

(deftest-error! compiler-macro-function-error.2
  (eval '(compiler-macro-function 'compiler-macro-function-1 nil)))

(deftest-error compiler-macro-function-error.3
  (eval '(compiler-macro-function 100))
  type-error)


;;
;;  Accessor (SETF COMPILER-MACRO-FUNCTION)
;;
(deftest compiler-macro-function-setf.1
  (functionp
    (setf (compiler-macro-function 'define-compiler-macro-setf-1)
          (lambda () :aaabbb)))
  t)

(deftest compiler-macro-function-setf.2
  (functionp
    (compiler-macro-function 'define-compiler-macro-setf-1))
  t)

(deftest compiler-macro-function-setf.3
  (functionp
    (setf (compiler-macro-function '(setf define-compiler-macro-setf-2))
          (lambda () :aaabbb)))
  t)

(deftest compiler-macro-function-setf.4
  (functionp
    (compiler-macro-function '(setf define-compiler-macro-setf-2)))
  t)

(deftest compiler-macro-function-setf.5
  (setf (compiler-macro-function 'define-compiler-macro-setf-1) nil)
  nil)

(deftest compiler-macro-function-setf.6
  (compiler-macro-function 'define-compiler-macro-setf-1)
  nil)

(deftest compiler-macro-function-setf.7
  (setf (compiler-macro-function '(setf define-compiler-macro-setf-2)) nil)
  nil)

(deftest compiler-macro-function-setf.8
  (compiler-macro-function '(setf define-compiler-macro-setf-2))
  nil)

(deftest-error! compiler-macro-function-setf-error.1
  (eval '(setf (compiler-macro-function) nil)))

(deftest-error! compiler-macro-function-setf-error.2
  (eval '(setf (compiler-macro-function 'error-name nil nil) nil)))

(deftest-error compiler-macro-function-setf-error.3
  (eval '(setf (compiler-macro-function 'define-compiler-macro-setf-1) 10))
  type-error)


;;
;;  Macro DEFINE-COMPILER-MACRO
;;
(deftest define-compiler-macro.1
  (define-compiler-macro define-compiler-macro-1 () :hello)
  define-compiler-macro-1)

(deftest define-compiler-macro.2
  (define-compiler-macro (setf define-compiler-macro-2) (a) (+ a 10))
  (setf define-compiler-macro-2))

(define-compiler-macro compiler-macro-1 (x)
  (+ x 100))

(defun compiler-macro-1 (x)
  (+ x 200))

(deftest compiler-macro.1
  (eval '(compiler-macro-1 300))
  500)

(deftest compiler-macro.2
  (funcall
    (compile nil '(lambda () (compiler-macro-1 300))))
  400)

(defvar *compiler-macro-test-1*)
(deftest compiler-macro.3
  (let ((input (lisp-system:make-memory-io-stream))
        (output (lisp-system:make-memory-io-stream)))
    (with-open-file (stream input :direction :output)
      (princ "(setq *compiler-macro-test-1* (compiler-macro-1 500))" stream))
    (with-open-file (stream input :direction :input)
      (compile-file stream :output-file output))
    (file-position output :start)
    (load output :type 'fasl)
    *compiler-macro-test-1*)
  600)

(defun compiler-macro-2 (&rest args)
  (apply #'+ args))

(define-compiler-macro compiler-macro-2 (&whole whole &rest args)
  (case (length args)
    (0 ''hello)
    (1 (car args))
    (t whole)))

(deftest compiler-macro.4
  (eval '(compiler-macro-2 1 2 3))
  6)

(deftest compiler-macro.5
  (funcall
    (compile nil '(lambda () (compiler-macro-2))))
  hello)

(deftest compiler-macro.6
  (funcall
    (compile nil '(lambda () (compiler-macro-2 999))))
  999)

(deftest compiler-macro.7
  (funcall
    (compile nil '(lambda () (compiler-macro-2 1 2 3))))
  6)

(defvar *compiler-macro-test-2*)
(deftest compiler-macro.8
  (let ((input (lisp-system:make-memory-io-stream))
        (output (lisp-system:make-memory-io-stream)))
    (with-open-file (stream input :direction :output)
      (princ "(setq *compiler-macro-test-2* (compiler-macro-2))" stream))
    (with-open-file (stream input :direction :input)
      (compile-file stream :output-file output))
    (file-position output :start)
    (load output :type 'fasl)
    *compiler-macro-test-2*)
  hello)

