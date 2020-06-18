;;
;;  compile-code
;;

(deftest compile-code.1
  (test-compile nil)
  nil)

(deftest compile-code.2
  (test-compile t)
  nil)

(deftest compile-code.3
  (test-compile 10)
  nil)


;;
;;  nil
;;
(deftest compile-code-nil.1
  (expr-compile nil)
  nil)

(deftest compile-code-nil.2
  (expr-compile (values nil))
  nil)


;;
;;  t
;;
(deftest compile-code-t.1
  (expr-compile t)
  t)

(deftest compile-code-t.2
  (expr-compile (values t))
  t)


;;
;;  value
;;
(deftest compile-code-value.1
  (expr-compile 10)
  10)

(deftest compile-code-value.2
  (expr-compile (values 10))
  10)


;;
;;  symbol
;;
(deftest compile-code-symbol.1
  (expr-compile
    (let ((x 10))
      x))
  10)

(deftest compile-code-symbol.2
  (expr-compile
    (let ((x 20))
      (values x)))
  20)

(defvar compile-code-symbol)
(deftest compile-code-symbol.3
  (expr-compile
    (let ((compile-code-symbol 30))
      (declare (special compile-code-symbol))
      compile-code-symbol))
  30)

(deftest compile-code-symbol.4
  (expr-compile
    (let ((compile-code-symbol 40))
      (declare (special compile-code-symbol))
      (values compile-code-symbol)))
  40)


;;
;;  declare
;;
(deftest compile-code-declaim.1
  (expr-compile
    (progn
      (declaim (special compile-code-declaim-special))
      nil))
  nil)

(deftest compile-code-declaim.2
  (expr-compile
    (progn
      (declaim (type integer compile-code-declaim-type))
      nil))
  nil)

(deftest compile-code-declaim.3
  (expr-compile
    (progn
      (declaim (ftype (function * (values integer)) compile-code-declaim-function))
      nil))
  nil)

(deftest compile-code-declaim.4
  (expr-compile
    (progn
      (declaim (ftype function (setf compile-code-declaim-function)))
      nil))
  nil)

(deftest compile-code-declaim.5
  (expr-compile
    (progn
      (declaim (inline compile-code-declaim-inline))
      nil))
  nil)

(deftest compile-code-declaim.6
  (expr-compile
    (progn
      (declaim (inline (setf compile-code-declaim-inline)))
      nil))
  nil)

(deftest compile-code-declaim.7
  (expr-compile
    (progn
      (declaim (notinline compile-code-declaim-notinline))
      nil))
  nil)

(deftest compile-code-declaim.8
  (expr-compile
    (progn
      (declaim (notinline (setf compile-code-declaim-notinline)))
      nil))
  nil)

(deftest compile-code-declaim.9
  (expr-compile
    (progn
      (declaim (optimize
                 (compilation-speed 1)
                 (debug 2)
                 (safety 3)
                 (space 0)
                 (speed 1)))
      nil))
  nil)


;;
;;  progn
;;
(deftest compile-code-progn.1
  (expr-compile
    (progn 10 20 30))
  30)

(deftest compile-code-progn.2
  (expr-compile
    (values
      (progn 10 20 30)))
  30)


;;
;;  let
;;
(deftest compile-code-let.1
  (expr-compile
    (let ((x 10))
      (let ((y 20))
        (declare (type integer x))
        (+ x y))))
  30)

(defvar *compile-code-let* 10)
(deftest compile-code-let.2
  (expr-compile
    (let ((x 10))
      (declare (type integer *compile-code-let*))
      x))
  10)

(deftest compile-code-let.3
  (expr-compile
    (let ((x 10))
      (declare (ftype function car))
      x))
  10)

(deftest compile-code-let.4
  (expr-compile
    (let ((x 10))
      (declare (ftype function (setf cdr)))
      x))
  10)

(deftest compile-code-let.5
  (expr-compile
    (let ((x 10))
      (declare (type integer x))
      x))
  10)

(deftest compile-code-let.6
  (expr-compile
    (let ((x 10))
      (declare (special x))
      (declare (type integer x))
      x))
  10)


;;
;;  let*
;;
(deftest compile-code-let*.1
  (expr-compile
    (let* ((x 10))
      (declare (type integer x))
      x))
  10)

(deftest compile-code-let*.2
  (expr-compile
    (let* ((x 10))
      (declare (special x))
      (declare (type integer x))
      x))
  10)


;;
;;  setq
;;
(deftest compile-code-setq.1
  (expr-compile
    (let* (x)
      (setq x 10)
      x))
  10)

(deftest compile-code-setq.2
  (expr-compile
    (let* (x)
      (declare (special x))
      (setq x 10)
      x))
  10)


;;
;;  function
;;
(deftest compile-code-function.1
  (functionp
    (expr-compile
      #'car))
  t)

(deftest compile-code-function.2
  (functionp
    (expr-compile
      (values #'car)))
  t)

(deftest compile-code-function.3
  (functionp
    (expr-compile
      #'(setf cdr)))
  t)

(deftest compile-code-function.4
  (functionp
    (expr-compile
      (values #'(setf cdr))))
  t)

(deftest compile-code-function.5
  (functionp
    (expr-compile
      (flet ((x () :hello))
        #'x)))
  t)

(deftest compile-code-function.6
  (functionp
    (expr-compile
      (flet ((x () :hello))
        (values #'x))))
  t)

(deftest compile-code-function.7
  (functionp
    (expr-compile
      (flet (((setf x) () :hello))
        #'(setf x))))
  t)

(deftest compile-code-function.8
  (functionp
    (expr-compile
      (flet (((setf x) () :hello))
        (values #'(setf x)))))
  t)


;;
;;  values
;;
(deftest compile-code-values.1
  (expr-compile
    (values))
  nil)

(deftest compile-code-values.2
  (expr-compile
    (values 10 20 30))
  10)

(deftest compile-code-values.3
  (expr-compile
    (values
      (values)))
  nil)

(deftest compile-code-values.4
  (expr-compile
    (values
      (values 10 20 30)))
  10)

(deftest compile-code-values.5
  (expr-compile
    (nth-value 1 (values 10 20 30)))
  20)


;;
;;  the
;;
(deftest compile-code-the.1
  (expr-compile
    (let ((x 10))
      (declare (type * x))
      (the integer x)))
  10)

(deftest compile-code-the.2
  (expr-compile
    (let ((x 10))
      (declare (type * x))
      (values
        (the integer x))))
  10)


;;
;;  if
;;
(deftest compile-code-if.1
  (expr-compile
    (let ((x 10))
      (if x
        20 30)))
  20)

(deftest compile-code-if.2
  (expr-compile
    (let ((x nil))
      (if x
        20 30)))
  30)

(deftest compile-code-if.3
  (expr-compile
    (let ((x 10))
      (values
        (if x
          20 30))))
  20)

(deftest compile-code-if.4
  (expr-compile
    (let ((x nil))
      (values
        (if x
          20 30))))
  30)


;;
;;  unwind-protect
;;
(deftest compile-code-unwind-protect.1
  (expr-compile
    (let ((x 10)
          (y 20))
      (unwind-protect
        (setq x 30)
        (setq y 40))))
  30)

(deftest compile-code-unwind-protect.2
  (expr-compile
    (let ((x 10)
          (y 20))
      (values
        (unwind-protect
          (setq x 30)
          (setq y 40)))))
  30)

(deftest compile-code-unwind-protect.3
  (expr-compile
    (let ((x 10)
          (y 20))
      (unwind-protect
        (setq x 30)
        (setq y 40))
      y))
  40)


;;
;;  tagbody
;;
(deftest compile-code-tagbody.1
  (expr-compile
    (let ((x 10))
      (tagbody
        (go aaa)
        bbb
        (setq x 20)
        (let ((y 30))
          (setq x (+ x y))
          (go ccc))
        aaa
        (go bbb)
        ccc)))
  nil)

(deftest compile-code-tagbody.2
  (expr-compile
    (let ((x 10))
      (values
        (tagbody
          (go aaa)
          bbb
          (setq x 20)
          (let ((y 30))
            (setq x (+ x y))
            (go ccc))
          aaa
          (go bbb)
          ccc))))
  nil)

(deftest compile-code-tagbody.3
  (expr-compile
    (let ((x 10))
      (tagbody
        (go aaa)
        bbb
        (setq x 20)
        (let ((y 30))
          (setq x (+ x y))
          (go ccc))
        aaa
        (go bbb)
        ccc)
      x))
  50)


;;
;;  block
;;
(deftest compile-code-block.1
  (expr-compile
    (block hello
      (return-from hello 10)))
  10)

(deftest compile-code-block.2
  (expr-compile
    (values
      (block hello
        (return-from hello 10))))
  10)


;;
;;  catch
;;
(deftest compile-code-catch.1
  (expr-compile
    (catch 'hello
      (throw 'hello 10)))
  10)

(deftest compile-code-catch.2
  (expr-compile
    (values
      (catch 'hello
        (throw 'hello 10))))
  10)


;;
;;  multiple-value-bind
;;
(deftest compile-code-multiple-value-bind.1
  (expr-compile
    (multiple-value-bind (a b c) (values 10 20 30)
      (+ a b c)))
  60)

(deftest compile-code-multiple-value-bind.2
  (expr-compile
    (values
      (multiple-value-bind (a b c) (values 10 20 30)
        (+ a b c))))
  60)

(deftest compile-code-multiple-value-bind.3
  (expr-compile
    (multiple-value-bind (a b c) (values 10 20 30)
      (declare (special b))
      (+ a b c)))
  60)

(deftest compile-code-multiple-value-bind.4
  (expr-compile
    (values
      (multiple-value-bind (a b c) (values 10 20 30)
        (declare (special b))
        (+ a b c))))
  60)

(deftest compile-code-multiple-value-bind.5
  (expr-compile
    (multiple-value-bind (a b c) (values 10 20 30)
      (declare (type integer b))
      (+ a b c)))
  60)

(deftest compile-code-multiple-value-bind.6
  (expr-compile
    (multiple-value-bind
      (a b c d e f g h i j k l m n o)
      (values 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
      (declare (ignorable a b c d e f g h i j k l m n o))
      (+ m n o)))
  12)

(deftest compile-code-multiple-value-bind.7
  (expr-compile
    (values
      (multiple-value-bind
        (a b c d e f g h i j k l m n o)
        (values 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
        (declare (ignorable a b c d e f g h i j k l m n o))
        (+ m n o))))
  12)

(deftest compile-code-multiple-value-bind.8
  (expr-compile
    (multiple-value-bind
      (a b c d e f g h i j k l m n o)
      (values 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
      (declare (special m))
      (declare (ignorable a b c d e f g h i j k l m n o))
      (+ m n o)))
  12)

(deftest compile-code-multiple-value-bind.9
  (expr-compile
    (values
      (multiple-value-bind
        (a b c d e f g h i j k l m n o)
        (values 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
        (declare (special m))
        (declare (ignorable a b c d e f g h i j k l m n o))
        (+ m n o))))
  12)

(deftest compile-code-multiple-value-bind.10
  (expr-compile
    (multiple-value-bind
      (a b c d e f g h i j k l m n o)
      (values 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
      (declare (type integer m))
      (declare (ignorable a b c d e f g h i j k l m n o))
      (+ m n o)))
  12)


;;
;;  multiple-value-call
;;
(deftest compile-code-multiple-value-call.1
  (expr-compile
    (multiple-value-call #'list (values 10 20 30) 40))
  (10 20 30 40))

(deftest compile-code-multiple-value-call.2
  (expr-compile
    (values
      (multiple-value-call #'list (values 10 20 30) 40)))
  (10 20 30 40))


;;
;;  multiple-value-prog1
;;
(deftest compile-code-multiple-value-prog1.1
  (expr-compile
    (multiple-value-prog1
      (values 10 20 30)
      (values 40 50 60)))
  10)

(deftest compile-code-multiple-value-prog1.2
  (expr-compile
    (values
      (multiple-value-prog1
        (values 10 20 30)
        (values 40 50 60))))
  10)

(deftest compile-code-multiple-value-prog1.3
  (expr-compile
    (multiple-value-bind (x y z)
      (multiple-value-prog1
        (values 10 20 30)
        (values 40 50 60))
      (+ x y z)))
  60)


;;
;;  nth-value
;;
(deftest compile-code-nth-value.1
  (expr-compile
    (nth-value 1 (values 10 20 30 40 50)))
  20)

(deftest compile-code-nth-value.2
  (expr-compile
    (values
      (nth-value 1 (values 10 20 30 40 50))))
  20)


;;
;;  progv
;;
(deftest compile-code-nth-progv.1
  (expr-compile
    (progv '(a b c) '(10 20 30)
      (+ a b c)))
  60)

(deftest compile-code-nth-progv.2
  (expr-compile
    (values
      (progv '(a b c) '(10 20 30)
        (+ a b c))))
  60)


;;
;;  handler
;;
(defvar *compile-code-handler-bind*)

(deftest compile-code-handler-bind.1
  (expr-compile
    (handler-bind
      ((control-error (lambda (x)
                        (declare (ignore x))
                        (setq *compile-code-handler-bind* 10))))
      (signal 'control-error)
      (+ *compile-code-handler-bind* 20)))
  30)

(deftest compile-code-handler-bind.2
  (expr-compile
    (values
      (handler-bind
        ((control-error (lambda (x)
                          (declare (ignore x))
                          (setq *compile-code-handler-bind* 10))))
        (signal 'control-error)
        (+ *compile-code-handler-bind* 20))))
  30)

(deftest compile-code-handler-case.1
  (expr-compile
    (handler-case
      (progn
        (error "Hello")
        10)
      (error () 20)))
  20)

(deftest compile-code-handler-case.2
  (expr-compile
    (values
      (handler-case
        (progn
          (error "Hello")
          10)
        (error () 20))))
  20)


;;
;;  restart
;;
(defvar *compile-code-restart-bind*)

(deftest compile-code-restart-bind.1
  (expr-compile
    (restart-bind
      ((continue (lambda () (setq *compile-code-restart-bind* 10))))
      (setq *compile-code-restart-bind* 20)
      (continue)
      *compile-code-restart-bind*))
  10)

(deftest compile-code-restart-bind.2
  (expr-compile
    (values
      (restart-bind
        ((continue (lambda () (setq *compile-code-restart-bind* 10))))
        (setq *compile-code-restart-bind* 20)
        (continue)
        *compile-code-restart-bind*)))
  10)

(deftest compile-code-restart-case.1
  (expr-compile
    (restart-case
      (progn
        (continue)
        20)
      (continue () 10)))
  10)

(deftest compile-code-restart-case.2
  (expr-compile
    (values
      (restart-case
        (progn
          (continue)
          20)
        (continue () 10))))
  10)

