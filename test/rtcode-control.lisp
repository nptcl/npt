;;
;;  rt-code
;;
(declaim (optimize (speed 0) (safety 3)))

(deftest rtcode-control-default
  (progn
    (declaim (optimize (speed 0) (safety 3)))
    (values)))

(deftest code-optimize-check.1
  (lisp-system::optimize-check parse)
  0)

(defun push-values (&rest args)
  (values-list args))


;;
;;  nil
;;
(deftest nil-set.1
  nil
  nil)

(deftest nil-push.1
  (push-values nil nil nil)
  nil nil nil)


;;
;;  t
;;
(deftest t-set.1
  t
  t)

(deftest t-push.1
  (push-values t t nil nil t)
  t t nil nil t)


;;
;;  value
;;
(deftest value-set.1
  10
  10)

(deftest value-push.1
  (push-values 10 20 #\A "Hello")
  10 20 #\A "Hello")


;;
;;  keyword
;;
(deftest keyword-set.1
  :hello
  :hello)

(deftest keyword-push.1
  (push-values :hello :abc)
  :hello :abc)


;;
;;  lexical
;;
(deftest lexical-set.1
  (let ((a 10))
    a)
  10)

(deftest-error lexical-set.2
  no-such-symbol)

(deftest lexical-push.1
  (let ((a 10) (b 20) (c 'hello))
    (push-values a b c))
  10 20 hello)

(deftest-error lexical-push.2
  (push-values no-sych-symbol))

(deftest lexical-rem.1
  (let ((a 10))
    a
    20)
  20)

(deftest-error lexical-rem.2
  (progn
    no-such-symbol
    20))


;;
;;  special
;;
(deftest special-set.1
  (let ((a 10))
    (declare (special a))
    a)
  10)

(defvar *special-set-2*)
(deftest-error special-set.2
  *special-set-2*)

(deftest special-push.1
  (let ((a 10) (b 20) (c 'hello))
    (declare (special a b c))
    (push-values a b c))
  10 20 hello)

(defvar *speical-push-2*)
(deftest-error special-push.2
  (push-values *speical-push-2*))

(defvar *special-rem-1* 10)
(deftest special-rem.1
  (progn
    *special-rem-1*
    20)
  20)

(defvar *special-rem-2*)
(deftest-error special-rem.2
  (progn
    *special-rem-2*
    20))


;;
;;  declaim
;;
(deftest declaim-set.1
  (declaim)
  nil)

(deftest declaim-push.1
  (push-values (declaim))
  nil)


;;
;;  progn
;;
(deftest progn-set.1
  (progn 10)
  10)

(deftest progn-push.1
  (push-values
    (progn 10))
  10)

(deftest progn-rem.1
  (progn
    10
    (progn
      10)
    30)
  30)


;;
;;  let
;;
(deftest let-set.1
  (let () 10 20 30)
  30)

(deftest let-push.1
  (push-values (let () 10 20 30))
  30)

(deftest let*-set.1
  (let* () 10 20 30)
  30)

(deftest let*-push.1
  (push-values (let* () 10 20 30))
  30)


;;
;;  setq
;;
(deftest setq-set.1
  (setq)
  nil)

(deftest setq-set.2
  (let (a b c)
    (setq a 10 b 20 c 30))
  30)

(deftest setq-push.1
  (push-values (setq))
  nil)

(deftest setq-push.2
  (let (a b c)
    (push-values (setq a 10 b 20 c 30)))
  30)


;;
;;  function
;;
(deftest function-set.1
  #.(function car)
  #.(function car))

(deftest function-set.2
  (function car)
  #.(function car))

(deftest function-set.3
  (function (setf car))
  #.(function (setf car)))

(deftest function-push.1
  (push-values #.(function car))
  #.(function car))

(deftest function-push.2
  (push-values (function car))
  #.(function car))

(deftest function-push.3
  (push-values (function (setf car)))
  #.(function (setf car)))


;;
;;  lambda
;;
(deftest lambda-push.1
  (null
    (lambda () :hello))
  nil)

(deftest lambda-cache.1
  (flet ((aaa () (lambda () :hello)))
    (eq (aaa) (aaa)))
  t)

(deftest lambda-cache.2
  (locally
    (declare (optimize speed))
    (flet ((aaa () (lambda () :hello)))
      (eq (aaa) (aaa))))
  t)

(deftest lambda-cache.3
  (locally
    (declare (optimize (speed 0)))
    (flet ((aaa () (lambda () :hello)))
      (eq (aaa) (aaa))))
  nil)

(deftest lambda-cache.4
  (locally
    (declare (optimize (speed 1)))
    (flet ((aaa () (lambda () :hello)))
      (eq (aaa) (aaa))))
  t)

(deftest lambda-cache.5
  (locally
    (declare (optimize (speed 0)))
    (flet ((aaa (x) (lambda () x)))
      (eq (aaa 10) (aaa 10))))
  nil)

(deftest lambda-cache.6
  (locally
    (declare (optimize (speed 1)))
    (flet ((aaa (x) (lambda () x)))
      (eq (aaa 10) (aaa 10))))
  nil)


;;
;;  defun
;;
(deftest defun-set.1
  (defun defun-set-1 ()
    :hello)
  defun-set-1)

(deftest defun-push.1
  (push-values
    (defun defun-push-1 ()
      :hello))
  defun-push-1)


;;
;;  destructuring-bind
;;
(deftest destructuring-bind-set.1
  (destructuring-bind (a b c) '(10 20 30)
    (+ a b c))
  60)

(deftest destructuring-bind-push.1
  (push-values
    (destructuring-bind (a b c) '(10 20 30)
      (+ a b c)))
  60)


;;
;;  define-symbol-macro
;;
(deftest define-symbol-macro-set.1
  (define-symbol-macro
    define-symbol-macro-set-1
    nil)
  define-symbol-macro-set-1)

(deftest define-symbol-macro-push.1
  (push-values
    (define-symbol-macro
      define-symbol-macro-push-1
      nil))
  define-symbol-macro-push-1)


;;
;;  flet
;;
(deftest flet-set.1
  (flet () 10 20 30)
  30)

(deftest flet-push.1
  (push-values (flet () 10 20 30))
  30)


;;
;;  labels
;;
(deftest labels-set.1
  (labels () 10 20 30)
  30)

(deftest labels-push.1
  (push-values (labels () 10 20 30))
  30)


;;
;;  values
;;
(deftest values-set.1
  (values 10 20 30)
  10 20 30)

(deftest values-push.1
  (push-values
    (values 10 20 30)
    (values 40 50 60))
  10 40)


;;
;;  the
;;
(deftest the-set.1
  (the integer (the * 100))
  100)

(deftest-error the-set.2
  (the string (the * 100))
  type-error)

(deftest the-push.1
  (push-values
    (the integer (the * 100)))
  100)

(deftest-error the-push.2
  (push-values
    (the vector (the * 100)))
  type-error)


;;
;;  locally
;;
(deftest locally-set.1
  (locally 10 20 30)
  30)

(deftest locally-push.1
  (push-values
    (locally 10 20 30))
  30)


;;
;;  if
;;
(deftest if-set.1
  (if 10 20 30)
  20)

(deftest if-push.1
  (push-values
    (if 10 20 30))
  20)


;;
;;  unwind-protect
(deftest unwind-protect-set.1
  (unwind-protect 10 20 30)
  10)

(deftest unwind-protect-push.1
  (push-values (unwind-protect 10 20 30))
  10)


;;
;;  tagbody
;;
(deftest tagbody-set.1
  (tagbody)
  nil)

(deftest tagbody-set.2
  (tagbody (+ 10 20 30))
  nil)

(deftest tagbody-push.1
  (push-values (tagbody))
  nil)

(deftest tagbody-push.2
  (push-values (tagbody (+ 10 20 30)))
  nil)


;;
;;  blocl
;;
(deftest block-set.1
  (block aaa)
  nil)

(deftest block-set.2
  (block aaa 10 20 30)
  30)

(deftest block-set.3
  (block aaa 10 (return-from aaa 40) 30)
  40)

(deftest block-push.1
  (push-values
    (block aaa))
  nil)

(deftest block-push.2
  (push-values
    (block aaa 10 20 30))
  30)

(deftest block-push.3
  (push-values
    (block aaa 10 (return-from aaa 40) 30))
  40)


;;
;;  catch
;;
(deftest catch-set.1
  (catch 'aaa)
  nil)

(deftest catch-set.2
  (catch 'aaa 10 20 30)
  30)

(deftest catch-set.3
  (catch 'aaa 10 20 (throw 'aaa 30) 40)
  30)

(deftest catch-push.1
  (push-values
    (catch 'aaa))
  nil)

(deftest catch-push.2
  (push-values
    (catch 'aaa 10 20 30))
  30)

(deftest catch-push.3
  (push-values
    (catch 'aaa 10 20 30)
    (catch 'aaa 40 50 60))
  30 60)

(deftest catch-push.4
  (car
    (catch 'aaa))
  nil)

(deftest catch-push.5
  (push-values
    (catch 'aaa 10 20 (throw 'aaa 30) 40))
  30)


;;
;;  multiple-value-bind
;;
(deftest multiple-value-bind-set.1
  (multiple-value-bind (a b c) (values 10 20 30)
    (+ a b c))
  60)

(deftest multiple-value-bind-push.1
  (push-values
    (multiple-value-bind (a b c) (values 10 20 30)
      (+ a b c)))
  60)


;;
;;  multiple-value-call
;;
(deftest multiple-value-call-set.1
  (multiple-value-call #'+ (values 10 20 30))
  60)

(deftest multiple-value-call-push.1
  (push-values
    (multiple-value-call #'+ (values 10 20 30)))
  60)


;;
;;  multiple-value-prog1
;;
(deftest multiple-value-prog1-set.1
  (multiple-value-prog1 (values 10 20 30)
    40 50 60)
  10 20 30)

(deftest multiple-value-prog1-push.1
  (push-values
    (multiple-value-prog1 (values 10 20 30)
      40 50 60))
  10)


;;
;;  nth-value
;;
(deftest nth-value-set.1
  (nth-value 1 (values 10 20 30 40))
  20)

(deftest nth-value-push.1
  (push-values
    (nth-value 1 (values 10 20 30 40)))
  20)


;;
;;  progv
;;
(deftest progv-set.1
  (progv () 10 20 30)
  30)

(deftest progv-push.1
  (push-values
    (progv () 10 20 30))
  30)


;;
;;  call
;;
(deftest call-set.1
  (car '(10 . 20))
  10)

(deftest call-push.1
  (push-values (car '(10 . 20)))
  10)


;;
;;  specialized call
;;
(deftest handler-bind-set.1
  (handler-bind ()
    10 20 30)
  30)

(deftest handler-bind-set.2
  (handler-bind ((error (lambda (c) c)))
    10 20 30)
  30)

(deftest handler-bind-push.1
  (values
    (handler-bind ()
      10 20 30))
  30)

(deftest handler-bind-push.2
  (values
    (handler-bind ((error (lambda (c) c)))
      10 20 30))
  30)

(deftest handler-case-set.1
  (handler-case 10)
  10)

(deftest handler-case-set.2
  (handler-case 10 (error () 'hello))
  10)

(deftest handler-case-push.1
  (values
    (handler-case 10))
  10)

(deftest handler-case-push.2
  (values
    (handler-case 10 (error () 'hello)))
  10)

(deftest restart-bind-set.1
  (restart-bind ()
    10 20 30)
  30)

(deftest restart-bind-set.2
  (restart-bind ((abort (lambda () 'abort)))
    10 20 30)
  30)

(deftest restart-bind-push.1
  (values
    (restart-bind ()
      10 20 30))
  30)

(deftest restart-bind-push.2
  (values
    (restart-bind ((abort (lambda () 'abort)))
      10 20 30))
  30)

(deftest restart-case-set.1
  (restart-case 10)
  10)

(deftest restart-case-set.2
  (restart-case 10 (abort () 'abort))
  10)

(deftest restart-case-push.1
  (values
    (restart-case 10))
  10)

(deftest restart-case-push.2
  (values
    (restart-case 10 (abort () 'abort)))
  10)

