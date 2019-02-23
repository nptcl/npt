;;
;;  ANSI COMMON LISP: 5. Data and Control Flow
;;
(defmacro eval-quote (x)
  `(eval (quote ,x)))

(deftest apply.1
  (apply #'list '(10 20 30))
  (10 20 30))

(deftest apply.2
  (apply #'list 10 '(20 30))
  (10 20 30))

(deftest apply.3
  (apply #'list nil)
  nil)

(deftest apply.4
  (apply 'list '(10 20 30))
  (10 20 30))

(deftest apply.5
  (apply 'list 10 20 30 nil)
  (10 20 30))

(deftest apply.6
  (apply (lambda () (values 10 20 30)) nil)
  10 20 30)

(deftest defun.1
  (progn
    (defun defun-test-1 ()
      :hello)
    (defun-test-1))
  :hello)

(deftest defun.2
  (progn
    (defun defun-test-2 ()
      :hello)
    (functionp #'defun-test-2))
  t)

(defun test-defun-3 ()
  (return-from test-defun-3 100)
  200)

(deftest defun.3
  (test-defun-3)
  100)

(deftest fdefinition.1
  (functionp
    (fdefinition 'car))
  t)

(deftest-error fdefinition.2
  (fdefinition (gensym))
  undefined-function)

(deftest fdefinition.3
  (functionp
    (fdefinition '(setf car)))
  t)

(deftest-error fdefinition.4
  (fdefinition (list 'setf (gensym)))
  undefined-function)

(deftest setf-fdefinition.1
  (progn
    (setf (fdefinition 'aaa) (lambda () :hello))
    (aaa))
  :hello)

(deftest setf-fdefinition.2
  (let (value)
    (setf (fdefinition '(setf bbb)) (lambda (v) (setq value v)))
    (setf (bbb) 100)
    value)
  100)

(deftest fboundp.1
  (fboundp 'car)
  t)

(deftest fboundp.2
  (fboundp 'fboudp-test)
  nil)

(deftest fboundp.3
  (fboundp '(setf car))
  t)

(deftest fboundp.4
  (fboundp '(setf fboundp-setf-test))
  nil)

(deftest fmakunbound.1
  (fmakunbound 'hello)
  hello)

(deftest fmakunbound.2
  (progn
    (defun fmakunbound-2 ())
    (let ((result (fboundp 'fmakunbound-2)))
      (fmakunbound 'fmakunbound-2)
      (values result (fboundp 'fmakunbound-2))))
  t nil)

(deftest fmakunbound.3
  (fmakunbound '(setf hello))
  (setf hello))

(deftest fmakunbound.4
  (progn
    (setf (fdefinition '(setf fmakunbound-4)) (lambda (v) v))
    (let ((result (fboundp '(setf fmakunbound-4))))
      (fmakunbound '(setf fmakunbound-4))
      (values result (fboundp '(setf fmakunbound-4)))))
  t nil)

(deftest flet.1
  (flet ((aaa () :hello))
    (aaa))
  :hello)

(deftest flet.2
  (flet ((aaa () :aaa))
    (flet ((aaa () :bbb)
           (bbb () (aaa)))
      (values (aaa) (bbb))))
  :bbb :aaa)

(deftest flet.3
  (flet ((aaa () (return-from aaa 10) 20))
    (aaa))
  10)

(deftest labels.1
  (labels ((aaa () :hello))
    (aaa))
  :hello)

(deftest labels.2
  (labels ((aaa () :aaa))
    (declare (ignorable #'aaa))
    (labels ((aaa () :bbb)
             (bbb () (aaa)))
      (values (aaa) (bbb))))
  :bbb :bbb)

(deftest labels.3
  (labels ((aaa () (return-from aaa 10) 20))
    (aaa))
  10)

(deftest macrolet.1
  (macrolet ((aaa (x) `(list ,x)))
    (aaa 10))
  (10))

(deftest macrolet.2
  (macrolet ((aaa (x) (return-from aaa `(list ,x)) `nil))
    (aaa 10))
  (10))

(deftest funcall.1
  (funcall #'list 10 20 30)
  (10 20 30))

(deftest funcall.2
  (funcall 'list 10 20 30)
  (10 20 30))

(deftest funcall.3
  (funcall 'list)
  nil)

(deftest funcall.4
  (funcall (lambda () (values 10 20 30)))
  10 20 30)

(deftest function.1
  (functionp
    (function car))
  t)

(deftest function.2
  (functionp
    (function
      (lambda () :hello)))
  t)

(deftest function.3
  (functionp
    (function
      (setf car)))
  t)

(deftest function-lambda-expression.1
  (function-lambda-expression #'car)
  nil nil car)

(deftest function-lambda-expression.2
  (function-lambda-expression (lambda () :hello))
  (lambda () :hello) nil nil)

(deftest function-lambda-expression.3
  (progn
    (defun function-lambda-expression-test-3 () :hello)
    (function-lambda-expression #'function-lambda-expression-test-3))
  nil nil function-lambda-expression-test-3)

(deftest function-lambda-expression.4
  (progn
    (defun (setf function-lambda-expression-test-4) (v) v)
    (function-lambda-expression #'(setf function-lambda-expression-test-4)))
  nil nil (setf function-lambda-expression-test-4))

(deftest functionp.1
  (functionp #'car)
  t)

(deftest functionp.2
  (functionp #'(setf car))
  t)

(deftest functionp.3
  (functionp #'(lambda () :hello))
  t)

(deftest functionp.4
  (functionp '(lambda () :hello))
  nil)

(deftest functionp.5
  (functionp 'car)
  nil)

(deftest functionp.6
  (functionp 10)
  nil)

(deftest functionp.7
  (flet ((functionp-test-6 () :hello))
    (functionp #'functionp-test-6))
  t)

(deftest compiled-function-p.1
  (compiled-function-p #'car)
  t)

(deftest compiled-function-p.2
  (compiled-function-p nil)
  nil)

(deftest compiled-function-p.3
  (compiled-function-p (lambda () :hello))
  nil)

(deftest call-arguments-limit.1
  (integerp call-arguments-limit)
  t)

(deftest call-arguments-limit.2
  (< 1024 call-arguments-limit)
  t)

(deftest lambda-list-keywords.1
  (let ((a lambda-list-keywords))
    (and (member '&optional a)
         (member '&rest a)
         (member '&body a)
         (member '&key a)
         (member '&allow-other-keys a)
         (member '&aux a)
         (member '&whole a)
         (member '&environment a)
         t))
  t)

(deftest lambda-parameters-limit.1
  (integerp lambda-parameters-limit)
  t)

(deftest lambda-parameters-limit.2
  (< 1024 lambda-parameters-limit)
  t)

(deftest defconstant.1
  (progn
    (defconstant defconstant-1 100)
    defconstant-1)
  100)

(deftest defconstant.2
  (defconstant defconstant-2 100)
  defconstant-2)

(deftest defconstant.3
  (progn
    (defconstant defconstant-3 200)
    (defconstant defconstant-3 200)
    defconstant-3)
  200)

(deftest-error defconstant.4
  (progn
    (defconstant defconstant-4 100)
    (defconstant defconstant-4 200)))

(deftest defconstant.5
  (progn
    (defconstant defconstant-5 333 "HELLO")
    defconstant-5)
  333)

(deftest-error defconstant.6
  (progn
    (defconstant defconstant-6 444)
    (setq defconstant-6 555)))

(deftest defconstant.7
  (progn
    (defconstant defconstant-7 333 "HELLO")
    (lisp-system::getdoc-variable 'defconstant-7))
  "HELLO")

(deftest defparameter.1
  (defparameter defparameter-1 100)
  defparameter-1)

(deftest defparameter.2
  (progn
    (defparameter defparameter-2 100)
    defparameter-2)
  100)

(deftest defparameter.3
  (progn
    (defparameter defparameter-3 100)
    (defparameter defparameter-3 200)
    defparameter-3)
  200)

(deftest defparameter.4
  (progn
    (defparameter defparameter-4 100)
    (setq defparameter-4 300)
    defparameter-4)
  300)

(deftest defparameter.5
  (progn
    (defparameter defparameter-5 100)
    (lisp-system::specialp 'defparameter-5))
  t)

(deftest defparameter.6
  (progn
    (defparameter defparameter-6 100 "AAA")
    (lisp-system::getdoc-variable 'defparameter-6))
  "AAA")

(deftest defparameter.7
  (progn
    (defparameter defparameter-7 100 "AAA")
    (lisp-system::specialp 'defparameter-7))
  t)

(deftest defvar.1
  (defvar defvar-1)
  defvar-1)

(deftest defvar.2
  (progn
    (defvar defvar-2)
    (boundp 'defvar-2))
  nil)

(deftest defvar.3
  (progn
    (defvar defvar-3)
    (lisp-system::specialp 'defvar-2))
  t)

(deftest defvar.4
  (defvar defvar-4 100)
  defvar-4)

(deftest defvar.5
  (progn
    (defvar defvar-5 200)
    defvar-5)
  200)

(deftest defvar.6
  (progn
    (defvar defvar-6 300)
    (lisp-system::specialp 'defvar-6))
  t)

(deftest defvar.7
  (defvar defvar-7 400 "ZZZ")
  defvar-7)

(deftest defvar.8
  (progn
    (defvar defvar-8 500 "ZZZ")
    defvar-8)
  500)

(deftest defvar.9
  (progn
    (defvar defvar-9 600 "ZZZ")
    (lisp-system::specialp 'defvar-9))
  t)

(deftest defvar.10
  (progn
    (defvar defvar-10 700 "ZZZ")
    (lisp-system::getdoc-variable 'defvar-10))
  "ZZZ")

(deftest destructuring-bind.1
  (destructuring-bind () nil
    10)
  10)

(deftest destructuring-bind.2
  (destructuring-bind (a) '(10)
    a)
  10)

(deftest destructuring-bind.3
  (destructuring-bind (a b) '(10 20)
    (values a b))
  10 20)

(deftest destructuring-bind.4
  (destructuring-bind (a &optional b) '(10 20)
    (values a b))
  10 20)

(deftest destructuring-bind.5
  (destructuring-bind (a &optional b) '(10)
    (values a b))
  10 nil)

(deftest destructuring-bind.6
  (destructuring-bind (a &optional b (c 20)) '(10)
    (values a b c))
  10 nil 20)

(deftest destructuring-bind.7
  (destructuring-bind (a &rest b) '(10)
    (values a b))
  10 nil)

(deftest destructuring-bind.8
  (destructuring-bind (a &rest b) '(10 20 30)
    (values a b))
  10 (20 30))

(deftest destructuring-bind.9
  (destructuring-bind (a &key hello) '(10 :hello 20)
    (values a hello))
  10 20)

(deftest destructuring-bind.10
  (destructuring-bind (a &key hello) '(10)
    (values a hello))
  10 nil)

(deftest-error destructuring-bind.11
  (destructuring-bind (a &key hello zzz) '(10 :aaa 20)
    (declare (ignore zzz))
    (values a hello)))

(deftest destructuring-bind.12
  (destructuring-bind (a &key hello zzz &allow-other-keys) '(10 :aaa 20)
    (declare (ignore zzz))
    (values a hello))
  10 nil)

(deftest destructuring-bind.13
  (destructuring-bind (a &aux z) '(10)
    (values a z))
  10 nil)

(deftest destructuring-bind.14
  (destructuring-bind (a &aux (z 20)) '(10)
    (values a z))
  10 20)

(deftest destructuring-bind.15
  (destructuring-bind (a (b) . c) '(10 (20) 30 40)
    (values a b c))
  10 20 (30 40))

(deftest destructuring-bind.16
  (destructuring-bind (&whole w a (&whole z b) . c) '(10 (20) 30 40)
    (values a b c w z))
  10 20 (30 40) (10 (20) 30 40) (20))

(deftest let.1
  (let ())
  nil)

(deftest let.2
  (let () 10)
  10)

(deftest let.3
  (let (a)
    a)
  nil)

(deftest let.4
  (let ((a 10))
    a)
  10)

(deftest let.5
  (let ((a 10))
    (let ((a (1+ a))
          (b a))
      (values a b)))
  11 10)

(deftest let.6
  (let ()
    10 20 30)
  30)

(deftest let*.1
  (let* ())
  nil)

(deftest let*.2
  (let* () 10)
  10)

(deftest let*.3
  (let* (a)
    a)
  nil)

(deftest let*.4
  (let* ((a 10))
    a)
  10)

(deftest let*.5
  (let* ((a 10))
    (let* ((a (1+ a))
           (b a))
      (values a b)))
  11 11)

(deftest let*.6
  (let* ()
    10 20 30)
  30)

(deftest progv.1
  (progv nil nil)
  nil)

(deftest progv.2
  (progn
    100
    (progv nil nil))
  nil)

(deftest progv.3
  (let ((a 100))
    (declare (special a) (ignorable a))
    (progv '(a) nil
      (boundp 'a)))
  nil)

(deftest progv.4
  (let ((a 100))
    (declare (special a) (ignorable a))
    (progv '(a) '(200)
      a))
  200)

(deftest progv.5
  (let ((a 100))
    (declare (special a) (ignorable a))
    (progv '(a) '(200 300)
      a))
  200)

(deftest progv.6
  (let ((a 100))
    (declare (special a) (ignorable a))
    (progv '(a) '(200)
      (symbol-value 'a)))
  200)

(deftest progv.7
  (let (a)
    (declare (special a) (ignorable a))
    (progv '(a b) '(200)
      (values
        (symbol-value 'a)
        (boundp 'b))))
  200 nil)

(deftest progv.8
  (let (a)
    (declare (special a) (ignorable a))
    (progv '(a b) '(200 300)
      (values
        a
        (symbol-value 'b))))
  200 300)

(deftest setq.1
  (setq)
  nil)

(deftest setq.2
  (let (a)
    (setq a 10))
  10)

(deftest setq.3
  (let (a)
    (setq a 10)
    a)
  10)

(deftest setq.4
  (let (a b)
    (setq a 10 b 20))
  20)

(deftest setq.5
  (let (a b)
    (setq a 10 b 20)
    (values a b))
  10 20)

(deftest setq.6
  (let ((a 10))
    (setq a (1+ a) b a)
    (values a b))
  11 11)

(deftest setq.7
  (let ((a 10))
    (declare (special a))
    (setq a 20)
    a)
  20)

(deftest setq.8
  (let ((x (list 10 20 30)))
    (symbol-macrolet
      ((y (car x)) (z (cadr x)))
      (setq y (1+ z) z (1+ y))
      (list x y z)))
  ((21 22 30) 21 22))

(deftest psetq.1
  (psetq)
  nil)

(deftest psetq.2
  (let (a)
    (psetq a 10))
  10)

(deftest psetq.3
  (let (a)
    (psetq a 10)
    a)
  10)

(deftest psetq.4
  (let ((a 10) b)
    (psetq a 20 b a)
    (values a b))
  20 10)

(deftest psetq.5
  (let ((a (list 10 20 30)))
    (symbol-macrolet
      ((b (car a)))
      (psetq b 999)
      a))
  (999 20 30))

(deftest psetq.6
  (let ((x (list 10 20 30)))
    (symbol-macrolet
      ((y (car x)) (z (cadr x)))
      (psetq y (1+ z) z (1+ y))
      (list x y z)))
  ((21 11 30) 21 11))

(deftest psetq.7
  (let ((a 1) (b 2))
    (psetq a b  b a)
    (values a b))
  2 1)

(deftest block.1
  (block nil)
  nil)

(deftest block.2
  (block hello 10)
  10)

(deftest block.3
  (block aaa
    (return-from aaa 10)
    20)
  10)

(deftest block.4
  (block aaa
    (block bbb
      (return-from aaa 10)
      20)
    30)
  10)

(deftest block.5
  (block aaa
    (block bbb
      (return-from bbb 10)
      20)
    30)
  30)

(deftest block.6
  (block nil
    (block aaa
      (return 100)
      200)
    300)
  100)

(deftest block.7
  (block nil
    (block aaa
      (return)
      200)
    300)
  nil)

(deftest block.8
  (block aaa
    (block aaa
      (return-from aaa 10)
      20)
    30)
  30)

(deftest block.9
  (let (a)
    (block aaa
      (setq a (block aaa
                (return-from aaa 10)
                20))
      30)
    a)
  10)

(deftest block.10
  (let (call)
    (block aaa
      (setq call (lambda () (return-from aaa 10)))
      (funcall call)))
  10)

(deftest block.11
  (let (call)
    (block aaa
      (setq call (lambda () (return-from aaa 10)))
      (funcall call)
      20))
  10)

(deftest block.12
  (let (a call)
    (block aaa
      (setq a (block aaa
                (setq call (lambda () (return-from aaa 10)))
                (funcall call)))
      20))
  20)

(deftest block.13
  (let (a call)
    (block aaa
      (setq a (block aaa
                (setq call (lambda () (return-from aaa 10)))
                (funcall call)))
      20)
    a)
  10)

(deftest block.14
  (block aaa
    (let ()
      (block bbb
        (return-from aaa 10))))
  10)

(deftest block.15
  (block aaa
    (block bbb
      (block ccc
        (block ddd
          (block eee
            (return-from aaa 10)
            111)))))
  10)

(deftest-error block.16
  (let (call)
    (block aaa
      (setq call (lambda () (return-from aaa 10))))
    (funcall call)))

(deftest-error block.17
  (let (call)
    (block aaa
      (block aaa
        (setq call (lambda () (return-from aaa 10))))
      (funcall call))))

(deftest block.18
  (block nil
    (tagbody
      (return-from nil 100)))
  100)

(deftest catch.1
  (catch nil)
  nil)

(deftest catch.2
  (catch 'hello 10)
  10)

(deftest catch.3
  (catch 'hello
    (throw 'hello 10)
    20)
  10)

(deftest catch.4
  (catch 'aaa
    (catch 'bbb
      (throw 'aaa 10)
      20)
    30)
  10)

(deftest catch.5
  (catch 'aaa
    (catch 'bbb
      (throw 'bbb 10)
      20)
    30)
  30)

(deftest tagbody.1
  (tagbody)
  nil)

(deftest tagbody.2
  (tagbody 10)
  nil)

(deftest tagbody.3
  (let (a)
    (tagbody
      10
      20
      (go 30)
      (setq a 100)
      30)
    a)
  nil)

(deftest tagbody.4
  (let (a)
    (tagbody
      10
      (tagbody
        20
        (go 30)
        (setq a 111)
        30
        (setq a 222))
      (go 40)
      30
      (setq a 333)
      40)
    a)
  222)

(deftest tagbody.5
  (let ((a 0))
    (tagbody
      (go 10)
      20
      (setq a (+ a 22))
      (go 30)
      10
      (setq a (+ a 10))
      (go 20)
      30)
    a)
  32)

(deftest tagbody.6
  (let (a call)
    (tagbody
      (setq call (lambda () (go 10)))
      (funcall call)
      (setq a 10)
      (go 20)
      10
      (setq a 20)
      20)
    a)
  20)

(deftest-error tagbody.7
  (let (call)
    (tagbody
      10
      (tagbody
        10
        (setq call (lambda () (go 10))))
      (funcall call))))

(deftest tagbody.8
  (let (a)
    (tagbody
      (tagbody
        (tagbody
          (tagbody
            (go 10))))
      10
      (setq a 999))
    a)
  999)

(deftest unwind-protect.1
  (unwind-protect nil)
  nil)

(deftest unwind-protect.2
  (unwind-protect 100)
  100)

(deftest unwind-protect.3
  (let (a b)
    (unwind-protect
      100
      (setq a 200)
      (setq b 300)))
  100)

(deftest unwind-protect.4
  (let (a b)
    (unwind-protect
      100
      (setq a 200)
      (setq b 300))
    (values a b))
  200 300)

(deftest unwind-protect.5
  (unwind-protect
    (block aaa
      (block bbb
        (block ccc
          (return-from aaa 100))))
    (block aaa
      (block bbb
        (block ccc
          (return-from aaa 200)))))
  100)

(deftest unwind-protect.6
  (let (a)
    (handler-case
      (unwind-protect
        (error "Hello")
        (setq a 100))
      (error () :hello)))
  :hello)

(deftest unwind-protect.7
  (let (a)
    (handler-case
      (unwind-protect
        (error "Hello")
        (setq a 100))
      (error () :hello))
    a)
  100)

(deftest unwind-protect.8
  (let (a)
    (catch 'hello
      (let ()
        (unwind-protect
          (let ()
            (let ()
              (throw 'hello 10)))
          (setq a 20)))))
  10)

(deftest unwind-protect.9
  (let (a)
    (catch 'hello
      (let ()
        (unwind-protect
          (let ()
            (let ()
              (throw 'hello 10)))
          (setq a 20))))
    a)
  20)

(deftest nil.1
  (null nil)
  t)

(deftest nil.2
  (symbolp nil)
  t)

(deftest nil.3
  (listp nil)
  t)

(deftest nil.4
  (consp nil)
  nil)

(deftest nil.5
  (symbol-value 'nil)
  nil)

(deftest nil.6
  (symbol-name 'nil)
  "NIL")

(deftest-error nil.7
  (eval-quote
    (setq nil 100)))

(deftest-error nil.8
  (eval-quote
    (let ((nil 10))
      nil)))

(deftest t.1
  t t)

(deftest t.2
  (symbolp t)
  t)

(deftest t.3
  (null t)
  nil)

(deftest t.4
  (symbol-value t)
  t)

(deftest t.5
  (symbol-name 't)
  "T")

(deftest-error t.6
  (eval-quote
    (setq t 100)))

(deftest-error t.7
  (eval-quote
    (let ((t 100))
      t)))

(deftest not.1
  (not nil)
  t)

(deftest not.2
  (not t)
  nil)

(deftest not.3
  (not 10)
  nil)

(deftest eq.1
  (eq 'hello 'hello)
  t)

(deftest eq.2
  (eq (list nil) (list nil))
  nil)

(deftest eq.3
  (eq nil nil)
  t)

(deftest eql.1
  (eql 10 10)
  t)

(deftest eql.2
  (eql nil nil)
  t)

(deftest eql.3
  (eql (list nil) (list nil))
  nil)

(deftest eql.4
  (eql #\a #\a)
  t)

(deftest eql.5
  (eql #\a #\A)
  nil)

(deftest equal.1
  (equal nil nil)
  t)

(deftest equal.2
  (equal 10 10)
  t)

(deftest equal.3
  (equal (list nil) (list nil))
  t)

(deftest equal.4
  (equal (list t) (list nil))
  nil)

(deftest equalp.1
  (equalp nil nil)
  t)

(deftest equalp.2
  (equalp (list #\a) (list #\a))
  t)

(deftest equalp.3
  (equalp (list #\A) (list #\a))
  t)

(deftest equalp.4
  (equalp (list #\z) (list #\a))
  nil)

(deftest identity.1
  (identity 10)
  10)

(deftest identity.2
  (identity nil)
  nil)

(deftest complement.1
  (functionp
    (complement #'identity))
  t)

(deftest complement.2
  (funcall
    (complement #'functionp)
    10)
  t)

(deftest complement.3
  (funcall
    (complement #'functionp)
    #'complement)
  nil)

(deftest constantly.1
  (functionp
    (constantly nil))
  t)

(deftest constantly.2
  (funcall (constantly 10))
  10)

(deftest constantly.3
  (funcall (constantly 20) t)
  20)

(deftest constantly.4
  (funcall (constantly nil) 10 20 30 40)
  nil)

(deftest every.1
  (every #'= nil)
  t)

(deftest every.2
  (every #'= '(10))
  t)

(deftest every.3
  (every #'= '(10 20 30))
  t)

(deftest every.4
  (every #'= '(10) '(10))
  t)

(deftest every.5
  (every #'= '(10) '(20))
  nil)

(deftest every.6
  (every #'= '(10) '(10 20))
  t)

(deftest every.7
  (every #'= '(10 20) '(10))
  t)

(deftest every.8
  (every #'= '(10 20) '(10 20))
  t)

(deftest every.9
  (every #'= '(10 20) '(10 30))
  nil)

(deftest every.10
  (every #'= '(10 20 30 40 50) '(10 20))
  t)

(deftest every.11
  (every #'= '(11 20 30 40 50) '(10 20))
  nil)

(deftest every.12
  (every #'= #(10 20 30 40) nil)
  t)

(deftest every.13
  (every #'= #() '(10 20 30 40))
  t)

(deftest every.14
  (every #'= #(10 20) #(10 20 30) #(10 20 40 50))
  t)

(deftest every.15
  (every #'= #(10 20) #(10 20 30) #(10 21 40 50))
  nil)

(deftest every.16
  (every #'/= #(1 2 3) #(2 3 4) '(3 4 5))
  t)

(deftest every.17
  (every #'/= #(1 2 3) #(2 4 4) '(3 4 5))
  nil)

(deftest some.1
  (some #'= nil)
  nil)

(deftest some.2
  (some #'= '(10))
  t)

(deftest some.3
  (some #'evenp '(11))
  nil)

(defun some-equal (&rest args)
  (if (apply #'= args)
    (list-length args)))

(deftest some.4
  (some #'some-equal '(11 12 13))
  1)

(deftest some.5
  (some #'some-equal '(11 12 13) '(12 13 14))
  nil)

(deftest some.6
  (some #'some-equal '(11 12 14) '(12 13 14))
  2)

(deftest some.7
  (some #'some-equal '(11 12 14) '(11 13 15))
  2)

(deftest some.8
  (some #'some-equal #())
  nil)

(deftest some.9
  (some #'some-equal #(11 12 13))
  1)

(deftest some.10
  (some #'some-equal #(11 12 13) '(12 13 14))
  nil)

(deftest some.11
  (some #'some-equal '(11 12 13) #(12 12 14))
  2)

(deftest some.12
  (some #'some-equal #(11 12 13) #(12 12 14) #(11 12 22))
  3)

(deftest notany.1
  (notany #'> '(1 2 3 4) '(5 6 7 8) '(9 10 11 12))
  t)

(deftest notevery.1
  (notevery #'< '(1 2 3 4) '(5 6 7 8) '(9 10 11 12))
  nil)

(deftest and.1
  (and)
  t)

(deftest and.2
  (and 10)
  10)

(deftest and.3
  (and 10 20 30)
  30)

(deftest and.4
  (and nil 20 30)
  nil)

(deftest and.5
  (and 10 nil 30)
  nil)

(deftest and.6
  (and 10 20 nil)
  nil)

(deftest cond.1
  (cond)
  nil)

(deftest cond.2
  (cond (10 20))
  20)

(deftest cond.3
  (cond (nil 20))
  nil)

(deftest cond.4
  (cond (nil 10) (20 30) (t 40))
  30)

(deftest cond.5
  (cond (nil 10) (nil 30) (t 40))
  40)

(deftest cond.6
  (cond ((functionp 10) 20) ((functionp #'car) 30))
  30)

(deftest if.1
  (if 10 20)
  20)

(deftest if.2
  (if t 10 20)
  10)

(deftest if.3
  (if nil 10)
  nil)

(deftest if.4
  (if nil 10 20)
  20)

(deftest or.1
  (or)
  nil)

(deftest or.2
  (or 10)
  10)

(deftest or.3
  (or nil)
  nil)

(deftest or.4
  (or 10 20 30)
  10)

(deftest or.5
  (or nil 20 30)
  20)

(deftest or.6
  (or nil nil 30)
  30)

(deftest or.7
  (or nil nil nil)
  nil)

(deftest or.8
  (or nil nil (values 10 20 30))
  10 20 30)

(deftest when.1
  (when 10 20 30)
  30)

(deftest when.2
  (when nil 20 30)
  nil)

(deftest unless.1
  (unless 10 20 30)
  nil)

(deftest unless.2
  (unless nil 20 30)
  30)

(deftest case.1
  (case 10)
  nil)

(deftest case.2
  (case 10
    (10 111) (20 222) (30 333))
  111)

(deftest case.3
  (case 20
    (10 111) (20 222) (30 333))
  222)

(deftest case.4
  (case :hello
    (10 111) (20 222) (30 333))
  nil)

(deftest case.5
  (case 30
    (10 111) ((1 2 3) 222) (30 333))
  333)

(deftest case.6
  (case 3
    (10 111) ((1 2 3) 222) (30 333))
  222)

(deftest case.7
  (case 9
    (10 111) ((1 2 3) 222) (30 333) (t 999))
  999)

(deftest case.8
  (case 9
    (10 111) ((1 2 3) 222) (30 333) (otherwise 999))
  999)

(deftest case.9
  (case 3
    (10 111) ((1 2 3) 222) (30 333) (otherwise 999))
  222)

(deftest ecase.1
  (ecase 10
    (10 :aaa))
  :aaa)

(deftest ecase.2
  (ecase 2
    (10 :aaa)
    ((1 2 3) :bbb)
    (30 :ccc))
  :bbb)

(deftest ecase.3
  (ecase 30
    (10 :aaa)
    ((1 2 3) :bbb)
    (30 :ccc))
  :ccc)

(deftest-error ecase.4
  (ecase 30
    (10 :aaa))
  type-error)

(deftest typecase.1
  (typecase 10)
  nil)

(deftest typecase.2
  (typecase 10
    (integer :aaa))
  :aaa)

(deftest typecase.3
  (typecase 10
    (integer :aaa)
    (string :bbb))
  :aaa)

(deftest typecase.4
  (typecase "Hello"
    (integer :aaa)
    (string :bbb))
  :bbb)

(deftest typecase.5
  (typecase #\a
    (integer :aaa)
    (string :bbb))
  nil)

(deftest typecase.6
  (typecase #\a
    (integer :aaa)
    (string :bbb)
    (otherwise :ccc))
  :ccc)

(deftest typecase.7
  (typecase #\a
    (integer :aaa)
    (string :bbb)
    (t :ccc))
  :ccc)

(deftest typecase.8
  (typecase "Hello"
    (integer :aaa)
    (string :bbb)
    (otherwise :ccc))
  :bbb)

(deftest etypecase.1
  (etypecase 10
    (integer :aaa))
  :aaa)

(deftest etypecase.2
  (etypecase 10
    (integer :aaa)
    (string :bbb))
  :aaa)

(deftest etypecase.3
  (etypecase "Hello"
    (integer :aaa)
    (string :bbb))
  :bbb)

(deftest-error etypecase.4
  (etypecase :hello
    (integer :aaa)
    (string :bbb))
  type-error)

(deftest multiple-value-bind.1
  (multiple-value-bind nil nil 10)
  10)

(deftest multiple-value-bind.2
  (multiple-value-bind (a) nil a 10)
  10)

(deftest multiple-value-bind.3
  (multiple-value-bind (a) nil a)
  nil)

(deftest multiple-value-bind.4
  (multiple-value-bind (a) 10 a)
  10)

(deftest multiple-value-bind.5
  (multiple-value-bind (a) (values 10 20) a)
  10)

(deftest multiple-value-bind.6
  (multiple-value-bind (a b) (values 10 20) b a)
  10)

(deftest multiple-value-bind.7
  (multiple-value-bind (a b) (values 10 20) a b)
  20)

(deftest multiple-value-bind.8
  (multiple-value-bind (a b c) (values 10 20) a b c)
  nil)

(deftest multiple-value-call.1
  (multiple-value-call #'list)
  nil)

(deftest multiple-value-call.2
  (multiple-value-call #'list 10)
  (10))

(deftest multiple-value-call.3
  (multiple-value-call #'list 10 20 30)
  (10 20 30))

(deftest multiple-value-call.4
  (multiple-value-call #'list 10 (values 20 30 40) 50)
  (10 20 30 40 50))

(deftest multiple-value-call.5
  (multiple-value-call #'list 10 (values) 50)
  (10 50))

(deftest multiple-value-list.1
  (multiple-value-list (values))
  nil)

(deftest multiple-value-list.2
  (multiple-value-list 10)
  (10))

(deftest multiple-value-list.3
  (multiple-value-list (values 10 20 30))
  (10 20 30))

(deftest multiple-value-prog1.1
  (multiple-value-prog1 nil)
  nil)

(deftest multiple-value-prog1.2
  (multiple-value-prog1 nil 10 20 30)
  nil)

(deftest multiple-value-prog1.3
  (multiple-value-prog1 (values 10 20))
  10 20)

(deftest multiple-value-prog1.4
  (multiple-value-prog1 (values 10 20) 30 40 50)
  10 20)

(deftest multiple-value-prog1.5
  (let (a b c)
    (multiple-value-prog1
      (setq a 10)
      (setq b 20)
      (setq c 30)))
  10)

(deftest multiple-value-prog1.6
  (let (a b c)
    (multiple-value-prog1
      (setq a 10)
      (setq b 20)
      (setq c 30))
    (values a b c))
  10 20 30)

(deftest multiple-value-setq.1
  (let (a)
    (multiple-value-setq (a) 10))
  10)

(deftest multiple-value-setq.2
  (let (a)
    (multiple-value-setq (a) 10)
    a)
  10)

(deftest multiple-value-setq.3
  (let ((a 10) (b 20) (c 30))
    (multiple-value-setq (a b c) 40)
    (values a b c))
  40 nil nil)

(deftest multiple-value-setq.4
  (let (a b c)
    (multiple-value-setq (a b c) (values 10 20 30 40 50)))
  10)

(deftest multiple-value-setq.5
  (let (a b c)
    (multiple-value-setq (a b c) (values 10 20 30 40 50))
    (values a b c))
  10 20 30)

(deftest values.1
  (values 10)
  10)

(deftest values.2
  (values (values 10 20))
  10)

(deftest values.3
  (values 10 20 30)
  10 20 30)

(deftest values.4
  (values (values 10 20) (values 30 40))
  10 30)

(deftest values.5
  (values))

(deftest values.6
  (funcall #'values))

(deftest values.7
  (funcall #'values 10 20 30)
  10 20 30)

(deftest values.8
  (values))

(deftest setf-values.1
  (let (a b)
    (setf (values a b) (values 10 20 30 40)))
  10 20)

(deftest setf-values.2
  (let (a b)
    (setf (values a b) (values 10 20 30 40))
    (values a b))
  10 20)

(deftest setf-values.3
  (let ((a 999) (b 888))
    (setf (values a b) (values 10))
    (values a b))
  10 nil)

(deftest values-list.1
  (values-list nil))

(deftest values-list.2
  (values-list '(10 20 30))
  10 20 30)

(deftest multiple-values-limit.1
  (integerp
    multiple-values-limit)
  t)

(deftest nth-value.1
  (nth-value 0 10)
  10)

(deftest nth-value.2
  (nth-value 1 (values 10 20 30 40))
  20)

(deftest nth-value.3
  (nth-value 3 (values 10 20 30 40))
  40)

(deftest nth-value.4
  (nth-value 3 (values 10 20 30 40))
  40)

(deftest nth-value.5
  (nth-value 4 (values 10 20 30 40))
  nil)

(deftest nth-value.6
  (nth-value 10 (values))
  nil)

(deftest prog.1
  (prog ())
  nil)

(deftest prog.2
  (prog () 10)
  nil)

(deftest prog.3
  (prog (a) 10 (progn a))
  nil)

(deftest prog.4
  (prog (a)
    (setq a 10)
    (return a))
  10)

(deftest prog.5
  (let ((a 10))
    (prog ((a (1+ a))
           (b a))
      (return (values a b))))
  11 10)

(deftest prog.6
  (prog (a)
    (go 10)
    (setq a 999)
    10
    (return a))
  nil)

(deftest prog.7
  (prog (a)
    (declare (ignore a))
    10)
  nil)

(deftest prog*.1
  (prog* ())
  nil)

(deftest prog*.2
  (prog* () 10)
  nil)

(deftest prog*.3
  (prog* (a) 10 (progn a))
  nil)

(deftest prog*.4
  (prog* (a)
    (setq a 10)
    (return a))
  10)

(deftest prog*.5
  (let ((a 10))
    (prog* ((a (1+ a))
            (b a))
      (return (values a b))))
  11 11)

(deftest prog*.6
  (prog* (a)
    (go 10)
    (setq a 999)
    10
    (return a))
  nil)

(deftest prog1.1
  (prog1 10)
  10)

(deftest prog1.2
  (prog1 10 20)
  10)

(deftest prog1.3
  (prog1 10 20 30)
  10)

(deftest prog2.1
  (prog2 10 20)
  20)

(deftest prog2.2
  (prog2 10 20 30 40)
  20)

(deftest progn.1
  (progn)
  nil)

(deftest progn.2
  (progn 10)
  10)

(deftest progn.3
  (progn 10 20 (progn 30))
  30)

(deftest define-modify-macro.1
  (progn
    (define-modify-macro test-modify-1 (&rest args) append)
    (let ((x (list 10 20 30)))
      (test-modify-1 x '(a b c))))
  (10 20 30 a b c))

(deftest define-modify-macro.2
  (progn
    (define-modify-macro test-modify-2 (&rest args) append)
    (let ((x (list 10 20 30)))
      (test-modify-2 x '(a b c))
      x))
  (10 20 30 a b c))

(deftest get-setf-expansion.1
  (list-length
    (multiple-value-list
      (get-setf-expansion 'x)))
  5)

(deftest get-setf-expansion.2
  (list-length
    (multiple-value-list
      (get-setf-expansion '(car x))))
  5)

(deftest define-setf-expander.1
  (define-setf-expander
    setf-expander-1 (x)
    x (values nil nil nil nil nil))
  setf-expander-1)

(deftest define-setf-expander.2
  (progn
    (define-setf-expander
      setf-expander-2 (x)
      (values nil nil nil nil x))
    (get-setf-expansion '(setf-expander-2 10)))
  nil nil nil nil 10)

(deftest define-setf-expander.3
  (progn
    (define-setf-expander
      setf-expander-3 (&environment env x y z)
      (values (null env) x y z nil))
    (get-setf-expansion '(setf-expander-3 10 20 30)))
  t 10 20 30 nil)

(deftest defsetf-short.1
  (defsetf defsetf-short-1 set)
  defsetf-short-1)

(defsetf defsetf-short-2 set)
(deftest defsetf-short.2
  (let (x)
    (declare (special x) (ignorable x))
    (list-length
      (multiple-value-list
        (get-setf-expansion '(defsetf-short-2 x)))))
  5)

(defsetf defsetf-short-3 set)
(deftest defsetf-short.3
  (let (x)
    (declare (special x) (ignorable x))
    (setf (defsetf-short-3 'x) 10))
  10)

(defsetf defsetf-short-4 set)
(deftest defsetf-short.4
  (let (x)
    (declare (special x) (ignorable x))
    (setf (defsetf-short-4 'x) 10)
    x)
  10)

(deftest defsetf-long.1
  (defsetf defsetf-long-1 (x) (g) `(set ,x ,g))
  defsetf-long-1)

(defsetf defsetf-long-2 (x) (g) `(set ,x ,g))
(deftest defsetf-long.2
  (let (a)
    (declare (special a) (ignorable a))
    (setf (defsetf-long-2 'a) 10))
  10)

(defsetf defsetf-long-3 (x) (g) `(set ,x ,g))
(deftest defsetf-long.3
  (let (a)
    (declare (special a) (ignorable a))
    (setf (defsetf-long-3 'a) 10)
    a)
  10)

(deftest setf.1
  (setf)
  nil)

(deftest setf.2
  (let (x)
    (setf x 10))
  10)

(deftest setf.3
  (let (x)
    (setf x 10)
    x)
  10)

(deftest setf.4
  (let (x y)
    (values (setf x 10 y 20) x y))
  20 10 20)

(deftest setf.5
  (let (x y)
    (values (setf x 10 y x) x y))
  10 10 10)

(deftest setf.6
  (let ((x '(10 . 20)))
    (values (setf (car x) 30) x))
  30 (30 . 20))

(deftest setf.7
  (let (x y z)
    (setf (values x y z) (values 10 20 30 40 50)))
  10 20 30)

(deftest setf.8
  (let (x y z)
    (setf (values x y z) (values 10 20 30 40 50))
    (values x y z))
  10 20 30)

(deftest psetf.1
  (psetf)
  nil)

(deftest psetf.2
  (let (x)
    (psetf x 10))
  10)

(deftest psetf.3
  (let (x)
    (psetf x 10)
    x)
  10)

(deftest psetf.4
  (let (x y)
    (values (psetf x 10 y 20) x y))
  20 10 20)

(deftest psetf.5
  (let ((x '(10 . 20)))
    (values (psetf (car x) 30) x))
  30 (30 . 20))

(deftest psetf.6
  (let ((x 10)
        (y 20))
    (psetf x 30 y x)
    (values x y))
  30 10)

(deftest shiftf.1
  (let ((a 10))
    (declare (special a))
    (shiftf a 20))
  10)

(deftest shiftf.2
  (let ((a 10))
    (declare (special a))
    (shiftf a 20)
    a)
  20)

(deftest shiftf.3
  (let ((a 10) (b '(20 . 30)))
    (declare (special a b))
    (values
      (shiftf a (car b) 40)
      a b))
  10 20 (40 . 30))

(deftest shiftf.4
  (let ((a 10) (b 20) (c 30) d)
    (shiftf (values a b) (values c d) (values 777 888 999))
    (values a b c d))
  30 nil 777 888)

(deftest shiftf.5
  (let ((a 10) (b 20) (c 30) d)
    (shiftf (values a b) (values c d) (values 777 888 999)))
  10 20)

(deftest rotatef.1
  (rotatef)
  nil)

(deftest rotatef.2
  (let ((x 10))
    (rotatef x))
  nil)

(deftest rotatef.3
  (let ((x 10))
    (rotatef x)
    x)
  10)

(deftest rotatef.4
  (let ((a 10) (b 20) (c 30))
    (values
      (rotatef a b c)
      a b c))
  nil 20 30 10)

(deftest rotatef.5
  (let ((a 10) (b 20) (c 30) (d 40) (e 50) (f 60))
    (rotatef (values a b) (values c d) (values e f))
    (values a b c d e f))
  30 40 50 60 10 20)


;;
;;  do-tests
;;
(do-tests :test t)

