;;
;;  rtcode-parse
;;
(declaim (optimize (speed 1) (safety 1)))

(import 'lisp-system::declare-parse)
(import 'lisp-system::optimize-check)

(defmacro parse-speed (&body body)
  `(progn
     (declaim (optimize (safety 0) (speed 3)))
     (eval (quote (progn ,@body)))))

(defmacro parse-safety (&body body)
  `(progn
     (declaim (optimize (safety 3) (speed 0)))
     (eval (quote (progn ,@body)))))

(deftest rtcode-parse-default
  (progn
    (declaim (optimize (speed 1) (safety 1)))
    (values)))


;;
;;  declare-parse
;;
(deftest declare-parse.1
  (integerp
    (declare-parse speed))
  t)

(deftest declare-parse.2
  (progn
    (declaim (optimize (speed 3)))
    (eval '(declare-parse speed)))
  3)

(deftest declare-parse.3
  (progn
    (declaim (optimize (safety 0)))
    (eval '(declare-parse safety)))
  0)

(deftest declare-parse.4
  (progn
    (declaim (optimize (space 2) (debug 3) (compilation-speed 0)))
    (values
      (eval '(declare-parse space))
      (eval '(declare-parse debug))
      (eval '(declare-parse compilation-speed))))
  2 3 0)


;;
;;  optimize-check
;;
(deftest optimize-check.1
  (parse-speed
    (optimize-check parse))
  1)

(deftest optimize-check.2
  (parse-safety
    (optimize-check parse))
  0)


;;
;;  progn
;;
(deftest parse-progn1.1
  (parse-speed
    (progn))
  nil)

(deftest parse-progn1.2
  (parse-safety
    (progn))
  nil)

(deftest parse-progn2.1
  (parse-speed
    (progn 10))
  10)

(deftest parse-progn2.2
  (parse-safety
    (progn 10))
  10)

(deftest parse-progn3.1
  (parse-speed
    (progn 10 20 30))
  30)

(deftest parse-progn3.2
  (parse-safety
    (progn 10 20 30))
  30)

(deftest parse-progn4.1
  (parse-speed
    (progn 10 (car nil) 20 30 (cdr '(a . b))))
  b)

(deftest parse-progn4.2
  (parse-safety
    (progn 10 (car nil) 20 30 (cdr '(a . b))))
  b)

(deftest parse-progn4.3
  (parse-speed
    (let (x)
      (values
        (progn 10 (setq x 333) 20 30 (cdr '(a . b)))
        x)))
  b 333)

(deftest parse-progn4.4
  (parse-safety
    (let (x)
      (values
        (progn 10 (setq x 333) 20 30 (cdr '(a . b)))
        x)))
  b 333)

(deftest parse-progn5.1
  (parse-speed
    (let (x)
      (values
        (progn 10 20 (setq x 333) 40)
        x)))
  40 333)

(deftest parse-progn5.2
  (parse-safety
    (let (x)
      (values
        (progn 10 20 (setq x 333) 40)
        x)))
  40 333)

(deftest parse-progn6.1
  (parse-speed
    (let (x)
      (values
        (progn 10 (progn 20 (progn 30 (setq x 40))) 50)
        x)))
  50 40)

(deftest parse-progn6.2
  (parse-safety
    (let (x)
      (values
        (progn 10 (progn 20 (progn 30 (setq x 40))) 50)
        x)))
  50 40)

(deftest parse-progn-all.1
  (parse-speed
    (progn (optimize-check parse) (optimize-check parse)))
  1)

(deftest parse-progn-all.2
  (parse-safety
    (progn (optimize-check parse) (optimize-check parse)))
  0)


;;
;;  let
;;
(deftest parse-let1.1
  (parse-speed
    (let ()
      :hello))
  :hello)

(deftest parse-let1.2
  (parse-safety
    (let ()
      :hello))
  :hello)

(deftest parse-let1.3
  (parse-speed
    (let* ()
      :hello))
  :hello)

(deftest parse-let1.4
  (parse-safety
    (let* ()
      :hello))
  :hello)

(deftest parse-let2.1
  (parse-speed
    (let ()
      (declare (optimize debug))
      :hello))
  :hello)

(deftest parse-let2.2
  (parse-safety
    (let ()
      (declare (optimize debug))
      :hello))
  :hello)

(deftest parse-let2.3
  (parse-speed
    (let* ()
      (declare (optimize debug))
      :hello))
  :hello)

(deftest parse-let2.4
  (parse-safety
    (let* ()
      (declare (optimize debug))
      :hello))
  :hello)

(deftest parse-let3.1
  (parse-speed
    (let ()))
  nil)

(deftest parse-let3.2
  (parse-safety
    (let ()))
  nil)

(deftest parse-let3.3
  (parse-speed
    (let* ()))
  nil)

(deftest parse-let3.4
  (parse-safety
    (let* ()))
  nil)

(deftest parse-let4.1
  (parse-speed
    (let (aaa bbb (ccc) (ddd nil))
      (declare (ignore aaa bbb ccc ddd))))
  nil)

(deftest parse-let4.2
  (parse-safety
    (let (aaa bbb (ccc) (ddd nil))
      (declare (ignore aaa bbb ccc ddd))))
  nil)

(deftest parse-let4.3
  (parse-speed
    (let* (aaa bbb (ccc) (ddd nil))
      (declare (ignore aaa bbb ccc ddd))))
  nil)

(deftest parse-let4.4
  (parse-safety
    (let* (aaa bbb (ccc) (ddd nil))
      (declare (ignore aaa bbb ccc ddd))))
  nil)

(deftest parse-let-args.1
  (parse-speed
    (let ((aaa (optimize-check parse)))
      aaa))
  1)

(deftest parse-let-args.2
  (parse-safety
    (let ((aaa (optimize-check parse)))
      aaa))
  0)

(deftest parse-let-args.3
  (parse-speed
    (let* ((aaa (optimize-check parse)))
      aaa))
  1)

(deftest parse-let-args.4
  (parse-safety
    (let* ((aaa (optimize-check parse)))
      aaa))
  0)

(deftest parse-let-body.1
  (parse-speed
    (let ((aaa 10))
      aaa
      (optimize-check parse)))
  1)

(deftest parse-let-body.2
  (parse-safety
    (let ((aaa 10))
      aaa
      (optimize-check parse)))
  0)

(deftest parse-let-body.3
  (parse-speed
    (let* ((aaa 10))
      aaa
      (optimize-check parse)))
  1)

(deftest parse-let-body.4
  (parse-safety
    (let* ((aaa 10))
      aaa
      (optimize-check parse)))
  0)

(deftest parse-let-body.5
  (parse-speed
    (let* ((aaa 10))
      aaa
      10 20 30 (values 40 50 60)))
  40 50 60)

(deftest parse-let-body.6
  (parse-safety
    (let* ((aaa 10))
      aaa
      10 20 30 (values 40 50 60)))
  40 50 60)


;;
;;  setq
;;
(deftest parse-setq1.1
  (parse-speed
    (setq))
  nil)

(deftest parse-setq1.2
  (parse-safety
    (setq))
  nil)

(deftest parse-setq-all.1
  (parse-speed
    (let (x y)
      (setq x (optimize-check parse)
            y (progn (optimize-check parse)))
      (values x y)))
  1 1)

(deftest parse-setq-all.2
  (parse-safety
    (let (x y)
      (setq x (optimize-check parse)
            y (progn (optimize-check parse)))
      (values x y)))
  0 0)


;;
;;  defun
;;
(deftest parse-defun-optional.1
  (parse-speed
    (defun parse-defun-optional1 (&optional (a (optimize-check parse)))
      a)
    (values
      (parse-defun-optional1)
      (parse-defun-optional1 100)))
  1 100)

(deftest parse-defun-optional.2
  (parse-safety
    (defun parse-defun-optional2 (&optional (a (optimize-check parse)))
      a)
    (values
      (parse-defun-optional2)
      (parse-defun-optional2 200)))
  0 200)

(deftest parse-defun-key.1
  (parse-speed
    (defun parse-defun-key1 (&key (a (optimize-check parse)))
      a)
    (values
      (parse-defun-key1)
      (parse-defun-key1 :a 100)))
  1 100)

(deftest parse-defun-key.2
  (parse-safety
    (defun parse-defun-key2 (&key (a (optimize-check parse)))
      a)
    (values
      (parse-defun-key2)
      (parse-defun-key2 :a 200)))
  0 200)

(deftest parse-defun-aux.1
  (parse-speed
    (defun parse-defun-aux1 (&aux (a (optimize-check parse)))
      a)
    (parse-defun-aux1))
  1)

(deftest parse-defun-aux.2
  (parse-safety
    (defun parse-defun-aux2 (&aux (a (optimize-check parse)))
      a)
    (parse-defun-aux2))
  0)

(deftest parse-defun-body.1
  (parse-speed
    (defun parse-defun-body1 ()
      (optimize-check parse))
    (parse-defun-body1))
  1)

(deftest parse-defun-body.2
  (parse-safety
    (defun parse-defun-body2 ()
      (optimize-check parse))
    (parse-defun-body2))
  0)

(deftest parse-defun-body.3
  (parse-speed
    (defun parse-defun-body3 ()
      10 20 30 (values 40 50 60))
    (parse-defun-body3))
  40 50 60)

(deftest parse-defun-body.4
  (parse-safety
    (defun parse-defun-body4 ()
      10 20 30 (values 40 50 60))
    (parse-defun-body4))
  40 50 60)


;;
;;  defmacro
;;
(deftest parse-defmacro-var.1
  (parse-speed
    (defmacro parse-defmacro-var1 ((&optional (x (optimize-check parse))))
      x)
    (parse-defmacro-var1 ()))
  1)

(deftest parse-defmacro-var.2
  (parse-safety
    (defmacro parse-defmacro-var2 ((&optional (x (optimize-check parse))))
      x)
    (parse-defmacro-var2 ()))
  0)

(deftest parse-defmacro-opt.1
  (parse-speed
    (defmacro parse-defmacro-opt1 (&optional (x (optimize-check parse)))
      x)
    (parse-defmacro-opt1))
  1)

(deftest parse-defmacro-opt.2
  (parse-safety
    (defmacro parse-defmacro-opt2 (&optional (x (optimize-check parse)))
      x)
    (parse-defmacro-opt2))
  0)

(deftest parse-defmacro-key.1
  (parse-speed
    (defmacro parse-defmacro-key1 (&key (x (optimize-check parse)))
      x)
    (parse-defmacro-key1))
  1)

(deftest parse-defmacro-key.2
  (parse-safety
    (defmacro parse-defmacro-key2 (&key (x (optimize-check parse)))
      x)
    (parse-defmacro-key2))
  0)

(deftest parse-defmacro-aux.1
  (parse-speed
    (defmacro parse-defmacro-aux1 (&aux (x (optimize-check parse)))
      x)
    (parse-defmacro-aux1))
  1)

(deftest parse-defmacro-aux.2
  (parse-safety
    (defmacro parse-defmacro-aux2 (&aux (x (optimize-check parse)))
      x)
    (parse-defmacro-aux2))
  0)

(deftest parse-defmacro-body.1
  (parse-speed
    (defmacro parse-defmacro-body1 ()
      (optimize-check parse))
    (parse-defmacro-body1))
  1)

(deftest parse-defmacro-body.2
  (parse-safety
    (defmacro parse-defmacro-body2 ()
      (optimize-check parse))
    (parse-defmacro-body2))
  0)


;;
;;  deftype
;;
(deftest parse-deftype-args.1
  (parse-speed
    (deftype parse-deftype-args1 (&optional (x (optimize-check parse)))
      x)
    (funcall
      (lisp-system:symbol-deftype 'parse-deftype-args1)
      nil nil))
  1)

(deftest parse-deftype-args.2
  (parse-safety
    (deftype parse-deftype-args1 (&optional (x (optimize-check parse)))
      x)
    (funcall
      (lisp-system:symbol-deftype 'parse-deftype-args1)
      nil nil))
  0)

(deftest parse-deftype-body.1
  (parse-speed
    (deftype parse-deftype-body1 ()
      (optimize-check parse))
    (funcall
      (lisp-system:symbol-deftype 'parse-deftype-body1)
      nil nil))
  1)

(deftest parse-deftype-body.2
  (parse-safety
    (deftype parse-deftype-body2 ()
      (optimize-check parse))
    (funcall
      (lisp-system:symbol-deftype 'parse-deftype-body2)
      nil nil))
  0)


;;
;;  define-compiler-macro
;;
(deftest parse-define-compiler-macro-args.1
  (parse-speed
    (define-compiler-macro parse-define-compiler-macro-args1
      (&optional (x (optimize-check parse)))
      x)
    (funcall
      (handler-bind
        ((warning #'muffle-warning))
        (compile nil '(lambda () (parse-define-compiler-macro-args1))))))
  1)

(deftest parse-define-compiler-macro-args.2
  (parse-safety
    (define-compiler-macro parse-define-compiler-macro-args2
      (&optional (x (optimize-check parse)))
      x)
    (funcall
      (handler-bind
        ((warning #'muffle-warning))
        (compile nil '(lambda () (parse-define-compiler-macro-args2))))))
  0)

(deftest parse-define-compiler-macro-body.1
  (parse-speed
    (define-compiler-macro parse-define-compiler-macro-body1 ()
      (optimize-check parse))
    (funcall
      (handler-bind
        ((warning #'muffle-warning))
        (compile nil '(lambda () (parse-define-compiler-macro-body1))))))
  1)

(deftest parse-define-compiler-macro-body.2
  (parse-safety
    (define-compiler-macro parse-define-compiler-macro-body2 ()
      (optimize-check parse))
    (funcall
      (handler-bind
        ((warning #'muffle-warning))
        (compile nil '(lambda () (parse-define-compiler-macro-body2))))))
  0)


;;
;;  destructuring-bind
;;
(deftest parse-destructuring-bind.1
  (parse-speed
    (destructuring-bind (x) (list (optimize-check parse))
      x))
  1)

(deftest parse-destructuring-bind.2
  (parse-safety
    (destructuring-bind (x) (list (optimize-check parse))
      x))
  0)

(deftest parse-destructuring-bind.3
  (parse-speed
    (destructuring-bind (&optional (x (optimize-check parse))) nil
      x))
  1)

(deftest parse-destructuring-bind.4
  (parse-safety
    (destructuring-bind (&optional (x (optimize-check parse))) nil
      x))
  0)

(deftest parse-destructuring-bind.5
  (parse-speed
    (destructuring-bind () nil
      (optimize-check parse)))
  1)

(deftest parse-destructuring-bind.6
  (parse-safety
    (destructuring-bind () nil
      (optimize-check parse)))
  0)


;;
;;  define-symbol-macro
;;
(deftest parse-define-symbol-macro.1
  (parse-speed
    (define-symbol-macro parse-define-symbol-macro1 (optimize-check parse))
    parse-define-symbol-macro1)
  1)

(deftest parse-define-symbol-macro.2
  (parse-safety
    (define-symbol-macro parse-define-symbol-macro2 (optimize-check parse))
    parse-define-symbol-macro2)
  0)


;;
;;  symbol-macrolet
;;
(deftest parse-symbol-macrolet-args.1
  (parse-speed
    (symbol-macrolet
      ((a (optimize-check parse)))
      a))
  1)

(deftest parse-symbol-macrolet-args.2
  (parse-safety
    (symbol-macrolet
      ((a (optimize-check parse)))
      a))
  0)

(deftest parse-symbol-macrolet-body.1
  (parse-speed
    (symbol-macrolet
      ((a 'hello))
      a
      (optimize-check parse)))
  1)

(deftest parse-symbol-macrolet-body.2
  (parse-safety
    (symbol-macrolet
      ((a 'hello))
      a
      (optimize-check parse)))
  0)


;;
;;  lambda
;;
(deftest parse-lambda-optional.1
  (parse-speed
    (let ((x (lambda (&optional (a (optimize-check parse))) a)))
      (values
        (funcall x)
        (funcall x 100))))
  1 100)

(deftest parse-lambda-optional.2
  (parse-safety
    (let ((x (lambda (&optional (a (optimize-check parse))) a)))
      (values
        (funcall x)
        (funcall x 200))))
  0 200)

(deftest parse-lambda-key.1
  (parse-speed
    (let ((x (lambda (&key (a (optimize-check parse))) a)))
      (values
        (funcall x)
        (funcall x :a 100))))
  1 100)

(deftest parse-lambda-key.2
  (parse-safety
    (let ((x (lambda (&key (a (optimize-check parse))) a)))
      (values
        (funcall x)
        (funcall x :a 200))))
  0 200)

(deftest parse-lambda-aux.1
  (parse-speed
    (let ((x (lambda (&aux (a (optimize-check parse))) a)))
      (funcall x)))
  1)

(deftest parse-lambda-aux.2
  (parse-safety
    (let ((x (lambda (&aux (a (optimize-check parse))) a)))
      (funcall x)))
  0)

(deftest parse-lambda-body.1
  (parse-speed
    (let ((x (lambda () (optimize-check parse))))
      (funcall x)))
  1)

(deftest parse-lambda-body.2
  (parse-safety
    (let ((x (lambda () (optimize-check parse))))
      (funcall x)))
  0)

(deftest parse-lambda-body.3
  (parse-speed
    (let ((x (lambda () 10 20 30 (values 40 50 60))))
      (funcall x)))
  40 50 60)

(deftest parse-lambda-body.4
  (parse-safety
    (let ((x (lambda () 10 20 30 (values 40 50 60))))
      (funcall x)))
  40 50 60)


;;
;;  if
;;
(deftest parse-if1.1
  (parse-speed
    (if nil 10 20))
  20)

(deftest parse-if1.2
  (parse-safety
    (if nil 10 20))
  20)

(deftest parse-if2.1
  (parse-speed
    (if #\a 10 20))
  10)

(deftest parse-if2.2
  (parse-safety
    (if #\a 10 20))
  10)

(deftest parse-if-expr.1
  (parse-speed
    (if (eql (optimize-check parse) 0)
      10 20))
  20)

(deftest parse-if-expr.2
  (parse-safety
    (if (eql (optimize-check parse) 0)
      10 20))
  10)

(deftest parse-if-then.1
  (parse-speed
    (if (evenp 20)
      (optimize-check parse)
      100))
  1)

(deftest parse-if-then.2
  (parse-safety
    (if (evenp 20)
      (optimize-check parse)
      100))
  0)

(deftest parse-if-else.1
  (parse-speed
    (if (oddp 20)
      100
      (optimize-check parse)))
  1)

(deftest parse-if-else.2
  (parse-safety
    (if (oddp 20)
      100
      (optimize-check parse)))
  0)


;;
;;  unwind-protect
;;
(deftest parse-unwind-protect1.1
  (parse-speed
    (unwind-protect
      100
      (cons 10 20)
      (concatenate 'string "Hello" "ABC")))
  100)

(deftest parse-unwind-protect1.2
  (parse-safety
    (unwind-protect
      100
      (cons 10 20)
      (concatenate 'string "Hello" "ABC")))
  100)

(deftest parse-unwind-protect2.1
  (parse-speed
    (unwind-protect
      (optimize-check parse)
      10 20 30 "Hello"))
  1)

(deftest parse-unwind-protect2.2
  (parse-safety
    (unwind-protect
      (optimize-check parse)
      10 20 30 "Hello"))
  0)

(deftest parse-unwind-protect-all.1
  (parse-speed
    (unwind-protect
      (optimize-check parse)
      (car nil)
      (cons nil nil)))
  1)

(deftest parse-unwind-protect-all.2
  (parse-safety
    (unwind-protect
      (optimize-check parse)
      (car nil)
      (cons nil nil)))
  0)

(deftest parse-unwind-protect-all.3
  (parse-speed
    (let (x)
      (unwind-protect
        (car nil)
        (setq x (optimize-check parse)))
      x))
  1)

(deftest parse-unwind-protect-all.4
  (parse-safety
    (let (x)
      (unwind-protect
        (car nil)
        (setq x (optimize-check parse)))
      x))
  0)


;;
;;  tagbody
;;
(deftest parse-tagbody1.1
  (parse-speed
    (tagbody))
  nil)

(deftest parse-tagbody1.2
  (parse-safety
    (tagbody))
  nil)

(deftest parse-tagbody1.3
  (parse-speed
    (tagbody
      10 20 30))
  nil)

(deftest parse-tagbody1.4
  (parse-safety
    (tagbody
      10 20 30))
  nil)

(deftest parse-tagbody2.1
  (parse-speed
    (tagbody
      (car nil)
      (cons 10 20)))
  nil)

(deftest parse-tagbody2.2
  (parse-safety
    (tagbody
      (car nil)
      (cons 10 20)))
  nil)

(deftest parse-tagbody2.3
  (parse-speed
    (let (x)
      (tagbody
        (car nil)
        (setq x (optimize-check parse)))
      x))
  1)

(deftest parse-tagbody2.4
  (parse-safety
    (let (x)
      (tagbody
        (car nil)
        (setq x (optimize-check parse)))
      x))
  0)

(deftest parse-tagbody-all.1
  (parse-speed
    (let (x)
      (tagbody
        (go 10)
        (setq x 10000)
        10
        (setq x (optimize-check parse)))
      x))
  1)

(deftest parse-tagbody-all.2
  (parse-safety
    (let (x)
      (tagbody
        (go 10)
        (setq x 10000)
        10
        (setq x (optimize-check parse)))
      x))
  0)


;;
;;  block / return-from
;;
(deftest parse-block1.1
  (parse-speed
    (block name))
  nil)

(deftest parse-block1.2
  (parse-safety
    (block name))
  nil)

(deftest parse-block2.1
  (parse-speed
    (block name
      10 20 30))
  30)

(deftest parse-block2.2
  (parse-safety
    (block name
      10 20 30))
  30)

(deftest parse-block-all.1
  (parse-speed
    (block name
      (optimize-check parse)))
  1)

(deftest parse-block-all.2
  (parse-safety
    (block name
      (optimize-check parse)))
  0)

(deftest parse-return-from.1
  (parse-speed
    (block name
      (return-from name (optimize-check parse))))
  1)

(deftest parse-return-from.2
  (parse-safety
    (block name
      (return-from name (optimize-check parse))))
  0)


;;
;;  catch / throw
;;
(deftest parse-catch1.1
  (parse-speed
    (catch 'hello))
  nil)

(deftest parse-catch1.2
  (parse-safety
    (catch 'hello))
  nil)

(deftest parse-catch1.3
  (parse-speed
    (let (x)
      (catch (setq x 'hello))
      x))
  hello)

(deftest parse-catch1.4
  (parse-safety
    (let (x)
      (catch (setq x 'hello))
      x))
  hello)

(deftest parse-catch2.1
  (parse-speed
    (catch 'hello 10 20 30 40))
  40)

(deftest parse-catch2.2
  (parse-safety
    (catch 'hello 10 20 30 40))
  40)

(deftest parse-catch2.3
  (parse-speed
    (let (x)
      (values
        (catch (setq x 'hello)
          10 20 30 40)
        x)))
  40 hello)

(deftest parse-catch2.4
  (parse-safety
    (let (x)
      (values
        (catch (setq x 'hello)
          10 20 30 40)
        x)))
  40 hello)

(deftest parse-catch-all.1
  (parse-speed
    (let (x y)
      (values
        (catch (setq x (optimize-check parse)
                     y 'hello)
          10 20 30)
        x)))
  30 1)

(deftest parse-catch-all.2
  (parse-safety
    (let (x y)
      (values
        (catch (setq x (optimize-check parse)
                     y 'hello)
          10 20 30)
        x)))
  30 0)

(deftest parse-catch-all.3
  (parse-speed
    (catch 'hello
      10 20 30 (optimize-check parse)))
  1)

(deftest parse-catch-all.4
  (parse-safety
    (catch 'hello
      10 20 30 (optimize-check parse)))
  0)

(deftest parse-throw.1
  (parse-speed
    (let (x y)
      (values
        (catch 'hello
          10 20
          (throw (setq x (optimize-check parse) y 'hello) 30)
          40)
        x)))
  30 1)

(deftest parse-throw.2
  (parse-safety
    (let (x y)
      (values
        (catch 'hello
          10 20
          (throw (setq x (optimize-check parse) y 'hello) 30)
          40)
        x)))
  30 0)

(deftest parse-throw.3
  (parse-speed
    (catch 'hello
      10 20
      (throw 'hello (optimize-check parse))
      30))
  1)

(deftest parse-throw.4
  (parse-safety
    (catch 'hello
      10 20
      (throw 'hello (optimize-check parse))
      30))
  0)


;;
;;  flet / lables
;;
(deftest parse-flet1.1
  (parse-speed
    (flet ()))
  nil)

(deftest parse-flet1.2
  (parse-safety
    (flet ()))
  nil)

(deftest parse-flet2.1
  (parse-speed
    (flet () 10 20 30 40))
  40)

(deftest parse-flet2.2
  (parse-safety
    (flet () 10 20 30 40))
  40)

(deftest parse-flet3.1
  (parse-speed
    (flet () (car nil) (optimize-check parse)))
  1)

(deftest parse-flet3.2
  (parse-safety
    (flet () (car nil) (optimize-check parse)))
  0)

(deftest parse-flet4.1
  (parse-speed
    (flet ()
      (declare (special *hello*))
      (optimize-check parse)))
  1)

(deftest parse-flet4.2
  (parse-safety
    (flet ()
      (declare (special *hello*))
      (optimize-check parse)))
  0)

(deftest parse-flet-args.1
  (parse-speed
    (flet ((hello (&optional (x (optimize-check parse))) x))
      (hello)))
  1)

(deftest parse-flet-args.2
  (parse-safety
    (flet ((hello (&optional (x (optimize-check parse))) x))
      (hello)))
  0)

(deftest parse-flet-args.3
  (parse-speed
    (flet ((hello () (optimize-check parse)))
      (hello)))
  1)

(deftest parse-flet-args.4
  (parse-safety
    (flet ((hello () (optimize-check parse)))
      (hello)))
  0)

(deftest parse-flet-body.1
  (parse-speed
    (flet ((hello () 10))
      (hello)
      (optimize-check parse)))
  1)

(deftest parse-flet-body.2
  (parse-safety
    (flet ((hello () 10))
      (hello)
      (optimize-check parse)))
  0)


;;
;;  the
;;
(deftest parse-the1.1
  (parse-speed
    (the integer 10))
  10)

(deftest parse-the1.2
  (parse-safety
    (the integer 10))
  10)

(deftest parse-the2.1
  (parse-speed
    (the integer (optimize-check parse)))
  1)

(deftest parse-the2.2
  (parse-safety
    (the integer (optimize-check parse)))
  0)


;;
;;  eval-when
;;
(deftest parse-eval-when1.1
  (parse-speed
    (eval-when (compile load eval)))
  nil)

(deftest parse-eval-when2.1
  (parse-safety
    (eval-when (compile load eval)))
  nil)

(deftest parse-eval-when-all.1
  (parse-speed
    (eval-when (compile load eval)
      (optimize-check parse)))
  1)

(deftest parse-eval-when-all.2
  (parse-safety
    (eval-when (compile load eval)
      (optimize-check parse)))
  0)


;;
;;  values
;;
(deftest parse-values.1
  (parse-speed
    (values)))

(deftest parse-values.2
  (parse-safety
    (values)))

(deftest parse-values.3
  (parse-speed
    (values (optimize-check parse) 10 20))
  1 10 20)

(deftest parse-values.4
  (parse-safety
    (values (optimize-check parse) 10 20))
  0 10 20)


;;
;;  locally
;;
(deftest parse-locally1.1
  (parse-speed
    (locally
      10 20 30))
  30)

(deftest parse-locally1.2
  (parse-safety
    (locally
      10 20 30))
  30)

(deftest parse-locally2.1
  (parse-speed
    (locally
      (declare (special *hello*))))
  nil)

(deftest parse-locally2.2
  (parse-safety
    (locally
      (declare (special *hello*))))
  nil)

(deftest parse-locally-all.1
  (parse-speed
    (locally
      (declare (special *hello*))
      (optimize-check parse)))
  1)

(deftest parse-locally-all.2
  (parse-safety
    (locally
      (declare (special *hello*))
      (optimize-check parse)))
  0)


;;
;;  call
;;
(deftest parse-call1.1
  (parse-speed
    ((lambda () (optimize-check parse))))
  1)

(deftest parse-call1.2
  (parse-safety
    ((lambda () (optimize-check parse))))
  0)

(deftest parse-call-all.1
  (parse-speed
    (+ (optimize-check parse) 2000))
  2001)

(deftest parse-call-all.2
  (parse-safety
    (+ (optimize-check parse) 2000))
  2000)


;;
;;  multiple-value-bind
;;
(deftest parse-multiple-value-bind1.1
  (parse-speed
    (multiple-value-bind (a b c) (optimize-check parse)
      (values a b c)))
  1 nil nil)

(deftest parse-multiple-value-bind1.2
  (parse-safety
    (multiple-value-bind (a b c) (optimize-check parse)
      (values a b c)))
  0 nil nil)

(deftest parse-multiple-value-bind2.1
  (parse-speed
    (multiple-value-bind (a) 10
      (values a (optimize-check parse))))
  10 1)

(deftest parse-multiple-value-bind2.2
  (parse-safety
    (multiple-value-bind (a) 10
      (values a (optimize-check parse))))
  10 0)


;;
;;  multiple-value-call
;;
(deftest parse-multiple-value-call1.1
  (parse-speed
    (let (x)
      (values
        (multiple-value-call
          (progn
            (setq x (optimize-check parse))
            #'+)
          (values 10 20 30))
        x)))
  60 1)

(deftest parse-multiple-value-call1.2
  (parse-safety
    (let (x)
      (values
        (multiple-value-call
          (progn
            (setq x (optimize-check parse))
            #'+)
          (values 10 20 30))
        x)))
  60 0)

(deftest parse-multiple-value-call2.1
  (parse-speed
    (multiple-value-call #'+ (values (optimize-check parse) 1000)))
  1001)

(deftest parse-multiple-value-call2.2
  (parse-safety
    (multiple-value-call #'+ (values (optimize-check parse) 1000)))
  1000)

