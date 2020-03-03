;;
;;  rtcode-optimize
;;
(declaim (optimize (speed 1) (safety 1)))

(import 'lisp-system::declare-parse)
(import 'lisp-system::optimize-check)

(defmacro optimize-speed (&body body)
  `(progn
     (declaim (optimize (safety 0) (speed 3)))
     (eval (quote (progn ,@body)))))

(defmacro optimize-safety (&body body)
  `(progn
     (declaim (optimize (safety 3) (speed 0)))
     (eval (quote (progn ,@body)))))

(deftest rtcode-optimize-default
  (progn
    (proclaim '(optimize (speed 1) (safety 1)))
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
  (optimize-speed
    (optimize-check type))
  1)

(deftest optimize-check.2
  (optimize-safety
    (optimize-check type))
  0)


;;
;;  progn
;;
(deftest optimize-progn1.1
  (optimize-speed
    (progn))
  nil)

(deftest optimize-progn1.2
  (optimize-safety
    (progn))
  nil)

(deftest optimize-progn2.1
  (optimize-speed
    (progn 10))
  10)

(deftest optimize-progn2.2
  (optimize-safety
    (progn 10))
  10)

(deftest optimize-progn3.1
  (optimize-speed
    (progn 10 20 30))
  30)

(deftest optimize-progn3.2
  (optimize-safety
    (progn 10 20 30))
  30)

(deftest optimize-progn4.1
  (optimize-speed
    (progn 10 (car nil) 20 30 (cdr '(a . b))))
  b)

(deftest optimize-progn4.2
  (optimize-safety
    (progn 10 (car nil) 20 30 (cdr '(a . b))))
  b)

(deftest optimize-progn4.3
  (optimize-speed
    (let (x)
      (values
        (progn 10 (setq x 333) 20 30 (cdr '(a . b)))
        x)))
  b 333)

(deftest optimize-progn4.4
  (optimize-safety
    (let (x)
      (values
        (progn 10 (setq x 333) 20 30 (cdr '(a . b)))
        x)))
  b 333)

(deftest optimize-progn5.1
  (optimize-speed
    (let (x)
      (values
        (progn 10 20 (setq x 333) 40)
        x)))
  40 333)

(deftest optimize-progn5.2
  (optimize-safety
    (let (x)
      (values
        (progn 10 20 (setq x 333) 40)
        x)))
  40 333)

(deftest optimize-progn6.1
  (optimize-speed
    (let (x)
      (values
        (progn 10 (progn 20 (progn 30 (setq x 40))) 50)
        x)))
  50 40)

(deftest optimize-progn6.2
  (optimize-safety
    (let (x)
      (values
        (progn 10 (progn 20 (progn 30 (setq x 40))) 50)
        x)))
  50 40)

(deftest optimize-progn-all.1
  (optimize-speed
    (progn (optimize-check type) (optimize-check type)))
  1)

(deftest optimize-progn-all.2
  (optimize-safety
    (progn (optimize-check type) (optimize-check type)))
  0)


;;
;;  let
;;
(deftest optimize-let1.1
  (optimize-speed
    (let ()
      :hello))
  :hello)

(deftest optimize-let1.2
  (optimize-safety
    (let ()
      :hello))
  :hello)

(deftest optimize-let1.3
  (optimize-speed
    (let* ()
      :hello))
  :hello)

(deftest optimize-let1.4
  (optimize-safety
    (let* ()
      :hello))
  :hello)

(deftest optimize-let2.1
  (optimize-speed
    (let ()
      (declare (optimize debug))
      :hello))
  :hello)

(deftest optimize-let2.2
  (optimize-safety
    (let ()
      (declare (optimize debug))
      :hello))
  :hello)

(deftest optimize-let2.3
  (optimize-speed
    (let* ()
      (declare (optimize debug))
      :hello))
  :hello)

(deftest optimize-let2.4
  (optimize-safety
    (let* ()
      (declare (optimize debug))
      :hello))
  :hello)

(deftest optimize-let3.1
  (optimize-speed
    (let ()))
  nil)

(deftest optimize-let3.2
  (optimize-safety
    (let ()))
  nil)

(deftest optimize-let3.3
  (optimize-speed
    (let* ()))
  nil)

(deftest optimize-let3.4
  (optimize-safety
    (let* ()))
  nil)

(deftest optimize-let4.1
  (optimize-speed
    (let (aaa bbb (ccc) (ddd nil))
      (declare (ignore aaa bbb ccc ddd))))
  nil)

(deftest optimize-let4.2
  (optimize-safety
    (let (aaa bbb (ccc) (ddd nil))
      (declare (ignore aaa bbb ccc ddd))))
  nil)

(deftest optimize-let4.3
  (optimize-speed
    (let* (aaa bbb (ccc) (ddd nil))
      (declare (ignore aaa bbb ccc ddd))))
  nil)

(deftest optimize-let4.4
  (optimize-safety
    (let* (aaa bbb (ccc) (ddd nil))
      (declare (ignore aaa bbb ccc ddd))))
  nil)

(deftest optimize-let-args.1
  (optimize-speed
    (let ((aaa (optimize-check type)))
      aaa))
  1)

(deftest optimize-let-args.2
  (optimize-safety
    (let ((aaa (optimize-check type)))
      aaa))
  0)

(deftest optimize-let-args.3
  (optimize-speed
    (let* ((aaa (optimize-check type)))
      aaa))
  1)

(deftest optimize-let-args.4
  (optimize-safety
    (let* ((aaa (optimize-check type)))
      aaa))
  0)

(deftest optimize-let-body.1
  (optimize-speed
    (let ((aaa 10))
      aaa
      (optimize-check type)))
  1)

(deftest optimize-let-body.2
  (optimize-safety
    (let ((aaa 10))
      aaa
      (optimize-check type)))
  0)

(deftest optimize-let-body.3
  (optimize-speed
    (let* ((aaa 10))
      aaa
      (optimize-check type)))
  1)

(deftest optimize-let-body.4
  (optimize-safety
    (let* ((aaa 10))
      aaa
      (optimize-check type)))
  0)

(deftest optimize-let-body.5
  (optimize-speed
    (let* ((aaa 10))
      aaa
      10 20 30 (values 40 50 60)))
  40 50 60)

(deftest optimize-let-body.6
  (optimize-safety
    (let* ((aaa 10))
      aaa
      10 20 30 (values 40 50 60)))
  40 50 60)


;;
;;  setq
;;
(deftest optimize-setq1.1
  (optimize-speed
    (setq))
  nil)

(deftest optimize-setq1.2
  (optimize-safety
    (setq))
  nil)

(deftest optimize-setq-all.1
  (optimize-speed
    (let (x y)
      (setq x (optimize-check type)
            y (progn (optimize-check type)))
      (values x y)))
  1 1)

(deftest optimize-setq-all.2
  (optimize-safety
    (let (x y)
      (setq x (optimize-check type)
            y (progn (optimize-check type)))
      (values x y)))
  0 0)


;;
;;  defun
;;
(deftest optimize-defun-optional.1
  (optimize-speed
    (defun optimize-defun-optional1 (&optional (a (optimize-check type)))
      a)
    (values
      (optimize-defun-optional1)
      (optimize-defun-optional1 100)))
  1 100)

(deftest optimize-defun-optional.2
  (optimize-safety
    (defun optimize-defun-optional2 (&optional (a (optimize-check type)))
      a)
    (values
      (optimize-defun-optional2)
      (optimize-defun-optional2 200)))
  0 200)

(deftest optimize-defun-key.1
  (optimize-speed
    (defun optimize-defun-key1 (&key (a (optimize-check type)))
      a)
    (values
      (optimize-defun-key1)
      (optimize-defun-key1 :a 100)))
  1 100)

(deftest optimize-defun-key.2
  (optimize-safety
    (defun optimize-defun-key2 (&key (a (optimize-check type)))
      a)
    (values
      (optimize-defun-key2)
      (optimize-defun-key2 :a 200)))
  0 200)

(deftest optimize-defun-aux.1
  (optimize-speed
    (defun optimize-defun-aux1 (&aux (a (optimize-check type)))
      a)
    (optimize-defun-aux1))
  1)

(deftest optimize-defun-aux.2
  (optimize-safety
    (defun optimize-defun-aux2 (&aux (a (optimize-check type)))
      a)
    (optimize-defun-aux2))
  0)

(deftest optimize-defun-body.1
  (optimize-speed
    (defun optimize-defun-body1 ()
      (optimize-check type))
    (optimize-defun-body1))
  1)

(deftest optimize-defun-body.2
  (optimize-safety
    (defun optimize-defun-body2 ()
      (optimize-check type))
    (optimize-defun-body2))
  0)

(deftest optimize-defun-body.3
  (optimize-speed
    (defun optimize-defun-body3 ()
      10 20 30 (values 40 50 60))
    (optimize-defun-body3))
  40 50 60)

(deftest optimize-defun-body.4
  (optimize-safety
    (defun optimize-defun-body4 ()
      10 20 30 (values 40 50 60))
    (optimize-defun-body4))
  40 50 60)


;;
;;  defmacro
;;
(deftest optimize-defmacro-var.1
  (optimize-speed
    (defmacro optimize-defmacro-var1 ((&optional (x (optimize-check type))))
      x)
    (optimize-defmacro-var1 ()))
  1)

(deftest optimize-defmacro-var.2
  (optimize-safety
    (defmacro optimize-defmacro-var2 ((&optional (x (optimize-check type))))
      x)
    (optimize-defmacro-var2 ()))
  0)

(deftest optimize-defmacro-opt.1
  (optimize-speed
    (defmacro optimize-defmacro-opt1 (&optional (x (optimize-check type)))
      x)
    (optimize-defmacro-opt1))
  1)

(deftest optimize-defmacro-opt.2
  (optimize-safety
    (defmacro optimize-defmacro-opt2 (&optional (x (optimize-check type)))
      x)
    (optimize-defmacro-opt2))
  0)

(deftest optimize-defmacro-key.1
  (optimize-speed
    (defmacro optimize-defmacro-key1 (&key (x (optimize-check type)))
      x)
    (optimize-defmacro-key1))
  1)

(deftest optimize-defmacro-key.2
  (optimize-safety
    (defmacro optimize-defmacro-key2 (&key (x (optimize-check type)))
      x)
    (optimize-defmacro-key2))
  0)

(deftest optimize-defmacro-aux.1
  (optimize-speed
    (defmacro optimize-defmacro-aux1 (&aux (x (optimize-check type)))
      x)
    (optimize-defmacro-aux1))
  1)

(deftest optimize-defmacro-aux.2
  (optimize-safety
    (defmacro optimize-defmacro-aux2 (&aux (x (optimize-check type)))
      x)
    (optimize-defmacro-aux2))
  0)

(deftest optimize-defmacro-body.1
  (optimize-speed
    (defmacro optimize-defmacro-body1 ()
      (optimize-check type))
    (optimize-defmacro-body1))
  1)

(deftest optimize-defmacro-body.2
  (optimize-safety
    (defmacro optimize-defmacro-body2 ()
      (optimize-check type))
    (optimize-defmacro-body2))
  0)


;;
;;  lambda
;;
(deftest optimize-lambda-optional.1
  (optimize-speed
    (let ((x (lambda (&optional (a (optimize-check type))) a)))
      (values
        (funcall x)
        (funcall x 100))))
  1 100)

(deftest optimize-lambda-optional.2
  (optimize-safety
    (let ((x (lambda (&optional (a (optimize-check type))) a)))
      (values
        (funcall x)
        (funcall x 200))))
  0 200)

(deftest optimize-lambda-key.1
  (optimize-speed
    (let ((x (lambda (&key (a (optimize-check type))) a)))
      (values
        (funcall x)
        (funcall x :a 100))))
  1 100)

(deftest optimize-lambda-key.2
  (optimize-safety
    (let ((x (lambda (&key (a (optimize-check type))) a)))
      (values
        (funcall x)
        (funcall x :a 200))))
  0 200)

(deftest optimize-lambda-aux.1
  (optimize-speed
    (let ((x (lambda (&aux (a (optimize-check type))) a)))
      (funcall x)))
  1)

(deftest optimize-lambda-aux.2
  (optimize-safety
    (let ((x (lambda (&aux (a (optimize-check type))) a)))
      (funcall x)))
  0)

(deftest optimize-lambda-body.1
  (optimize-speed
    (let ((x (lambda () (optimize-check type))))
      (funcall x)))
  1)

(deftest optimize-lambda-body.2
  (optimize-safety
    (let ((x (lambda () (optimize-check type))))
      (funcall x)))
  0)

(deftest optimize-lambda-body.3
  (optimize-speed
    (let ((x (lambda () 10 20 30 (values 40 50 60))))
      (funcall x)))
  40 50 60)

(deftest optimize-lambda-body.4
  (optimize-safety
    (let ((x (lambda () 10 20 30 (values 40 50 60))))
      (funcall x)))
  40 50 60)


;;
;;  if
;;
(deftest optimize-if1.1
  (optimize-speed
    (if nil 10 20))
  20)

(deftest optimize-if1.2
  (optimize-safety
    (if nil 10 20))
  20)

(deftest optimize-if2.1
  (optimize-speed
    (if #\a 10 20))
  10)

(deftest optimize-if2.2
  (optimize-safety
    (if #\a 10 20))
  10)

(deftest optimize-if-expr.1
  (optimize-speed
    (if (eql (optimize-check type) 0)
      10 20))
  20)

(deftest optimize-if-expr.2
  (optimize-safety
    (if (eql (optimize-check type) 0)
      10 20))
  10)

(deftest optimize-if-then.1
  (optimize-speed
    (if (evenp 20)
      (optimize-check type)
      100))
  1)

(deftest optimize-if-then.2
  (optimize-safety
    (if (evenp 20)
      (optimize-check type)
      100))
  0)

(deftest optimize-if-else.1
  (optimize-speed
    (if (oddp 20)
      100
      (optimize-check type)))
  1)

(deftest optimize-if-else.2
  (optimize-safety
    (if (oddp 20)
      100
      (optimize-check type)))
  0)


;;
;;  unwind-protect
;;
(deftest optimize-unwind-protect1.1
  (optimize-speed
    (unwind-protect
      100
      (cons 10 20)
      (concatenate 'string "Hello" "ABC")))
  100)

(deftest optimize-unwind-protect1.2
  (optimize-safety
    (unwind-protect
      100
      (cons 10 20)
      (concatenate 'string "Hello" "ABC")))
  100)

(deftest optimize-unwind-protect2.1
  (optimize-speed
    (unwind-protect
      (optimize-check type)
      10 20 30 "Hello"))
  1)

(deftest optimize-unwind-protect2.2
  (optimize-safety
    (unwind-protect
      (optimize-check type)
      10 20 30 "Hello"))
  0)

(deftest optimize-unwind-protect-all.1
  (optimize-speed
    (unwind-protect
      (optimize-check type)
      (car nil)
      (cons nil nil)))
  1)

(deftest optimize-unwind-protect-all.2
  (optimize-safety
    (unwind-protect
      (optimize-check type)
      (car nil)
      (cons nil nil)))
  0)

(deftest optimize-unwind-protect-all.3
  (optimize-speed
    (let (x)
      (unwind-protect
        (car nil)
        (setq x (optimize-check type)))
      x))
  1)

(deftest optimize-unwind-protect-all.4
  (optimize-safety
    (let (x)
      (unwind-protect
        (car nil)
        (setq x (optimize-check type)))
      x))
  0)


;;
;;  tagbody
;;
(deftest optimize-tagbody1.1
  (optimize-speed
    (tagbody))
  nil)

(deftest optimize-tagbody1.2
  (optimize-safety
    (tagbody))
  nil)

(deftest optimize-tagbody1.3
  (optimize-speed
    (tagbody
      10 20 30))
  nil)

(deftest optimize-tagbody1.4
  (optimize-safety
    (tagbody
      10 20 30))
  nil)

(deftest optimize-tagbody2.1
  (optimize-speed
    (tagbody
      (car nil)
      (cons 10 20)))
  nil)

(deftest optimize-tagbody2.2
  (optimize-safety
    (tagbody
      (car nil)
      (cons 10 20)))
  nil)

(deftest optimize-tagbody2.3
  (optimize-speed
    (let (x)
      (tagbody
        (car nil)
        (setq x (optimize-check type)))
      x))
  1)

(deftest optimize-tagbody2.4
  (optimize-safety
    (let (x)
      (tagbody
        (car nil)
        (setq x (optimize-check type)))
      x))
  0)

(deftest optimize-tagbody-all.1
  (optimize-speed
    (let (x)
      (tagbody
        (go 10)
        (setq x 10000)
        10
        (setq x (optimize-check type)))
      x))
  1)

(deftest optimize-tagbody-all.2
  (optimize-safety
    (let (x)
      (tagbody
        (go 10)
        (setq x 10000)
        10
        (setq x (optimize-check type)))
      x))
  0)


;;
;;  block / return-from
;;
(deftest optimize-block1.1
  (optimize-speed
    (block name))
  nil)

(deftest optimize-block1.2
  (optimize-safety
    (block name))
  nil)

(deftest optimize-block2.1
  (optimize-speed
    (block name
      10 20 30))
  30)

(deftest optimize-block2.2
  (optimize-safety
    (block name
      10 20 30))
  30)

(deftest optimize-block-all.1
  (optimize-speed
    (block name
      (optimize-check type)))
  1)

(deftest optimize-block-all.2
  (optimize-safety
    (block name
      (optimize-check type)))
  0)

(deftest optimize-return-from.1
  (optimize-speed
    (block name
      (return-from name (optimize-check type))))
  1)

(deftest optimize-return-from.2
  (optimize-safety
    (block name
      (return-from name (optimize-check type))))
  0)


;;
;;  catch / throw
;;
(deftest optimize-catch1.1
  (optimize-speed
    (catch 'hello))
  nil)

(deftest optimize-catch1.2
  (optimize-safety
    (catch 'hello))
  nil)

(deftest optimize-catch1.3
  (optimize-speed
    (let (x)
      (catch (setq x 'hello))
      x))
  hello)

(deftest optimize-catch1.4
  (optimize-safety
    (let (x)
      (catch (setq x 'hello))
      x))
  hello)

(deftest optimize-catch2.1
  (optimize-speed
    (catch 'hello 10 20 30 40))
  40)

(deftest optimize-catch2.2
  (optimize-safety
    (catch 'hello 10 20 30 40))
  40)

(deftest optimize-catch2.3
  (optimize-speed
    (let (x)
      (values
        (catch (setq x 'hello)
          10 20 30 40)
        x)))
  40 hello)

(deftest optimize-catch2.4
  (optimize-safety
    (let (x)
      (values
        (catch (setq x 'hello)
          10 20 30 40)
        x)))
  40 hello)


;;
;;  call
;;
(deftest optimize-call1.1
  (optimize-speed
    ((lambda () (optimize-check type))))
  1)

(deftest optimize-call1.2
  (optimize-safety
    ((lambda () (optimize-check type))))
  0)

(deftest optimize-call-all.1
  (optimize-speed
    (+ (optimize-check type) 2000))
  2001)

(deftest optimize-call-all.2
  (optimize-safety
    (+ (optimize-check type) 2000))
  2000)

