;;
;;  ANSI COMMON LISP: 3. Evaluation and Compilation
;;

;;
;;  optimizer off  [degrade]
;;
#+rt-degrade
(deftest rteval-optimize-check-degrade.1
  (lisp-system:optimize-check parse)
  0)

#+rt-degrade
(deftest rteval-optimize-check-degrade.2
  (lisp-system:optimize-check scope)
  0)


;;
;;  optimizer on  [load]
;;
#-rt-degrade
(deftest rteval-optimize-check-load.1
  (lisp-system:optimize-check parse)
  1)

#-rt-degrade
(deftest rteval-optimize-check-load.2
  (lisp-system:optimize-check scope)
  1)


;;
;;  lambda
;;
(deftest lambda.1
  (functionp
    (function (lambda () :hello)))
  t)

(deftest lambda.2
  (functionp
    #'(lambda (x) x))
  t)

(deftest lambda.3
  (functionp
    (lambda () :hello))
  t)

(deftest lambda.4
  (funcall (lambda () :hello))
  :hello)

(deftest-error lambda.5
  (funcall (lambda () :hello) 100))

(deftest lambda.6
  (funcall (lambda (x) (evenp x)) 10)
  t)

(deftest lambda.7
  (funcall (lambda (x) (evenp x)) 11)
  nil)

(deftest lambda.8
  (funcall (lambda () (values 10 20 30)))
  10 20 30)

(deftest lambda.9
  (funcall (lambda () (values))))

(deftest lambda.10
  (let* ((a 10)
         (c (lambda () (setq a (+ a 10)))))
    (funcall c)
    (funcall c)
    (funcall c)
    a)
  40)

(deftest lambda-var.1
  (funcall (lambda (x) x) 100)
  100)

(deftest-error lambda-var.2
  (funcall (lambda (x) (evenp x))))

(deftest-error lambda-var.3
  (funcall (lambda (x) (evenp x)) 10 20))

(deftest lambda-var.4
  (funcall (lambda (x y) (+ 10 x y)) 20 30)
  60)

(deftest-error lambda-var.5
  (funcall (lambda (x y) (+ 10 x y)) 20))

(deftest-error lambda-var.6
  (funcall (lambda (x y) (+ 10 x y)) 20 30 40))

(deftest lambda-optional.1
  (funcall (lambda (&optional a) a))
  nil)

(deftest lambda-optional.2
  (funcall (lambda (&optional a) a) 10)
  10)

(deftest-error lambda-optional.3
  (funcall (lambda (&optional a) a) 10 20))

(deftest lambda-optional.4
  (funcall (lambda (&optional (a)) a))
  nil)

(deftest lambda-optional.5
  (funcall (lambda (&optional (a)) a) 10)
  10)

(deftest lambda-optional.6
  (funcall (lambda (&optional (a 'hello)) a))
  hello)

(deftest lambda-optional.7
  (funcall (lambda (&optional (a 'hello)) a) 10)
  10)

(deftest lambda-optional.8
  (funcall (lambda (&optional (a 'hello s)) (values a s)))
  hello nil)

(deftest lambda-optional.9
  (funcall (lambda (&optional (a 'hello s)) (values a s)) 10)
  10 t)

(deftest lambda-optional.10
  (funcall (lambda (a b &optional c (d 999)) (values a b c d)) 10 20)
  10 20 nil 999)

(deftest lambda-optional.11
  (funcall (lambda (a b &optional c d) (values a b c d)) 10 20 30 40)
  10 20 30 40)

(deftest-error lambda-optional.12
  (funcall (lambda (a b &optional c d) (values a b c d)) 10 20 30 40 50))

(deftest lambda-rest.1
  (funcall (lambda (&rest arg) arg))
  nil)

(deftest lambda-rest.2
  (funcall (lambda (&rest arg) arg) 10 20 30 40)
  (10 20 30 40))

(deftest lambda-rest.3
  (funcall (lambda (a &rest arg) (values a arg)) 10)
  10 nil)

(deftest lambda-rest.4
  (funcall (lambda (a &rest arg) (values a arg)) 10 20 30 40)
  10 (20 30 40))

(deftest-error lambda-rest.5
  (funcall (lambda (a &rest arg) (values a arg))))

(deftest lambda-rest.6
  (funcall (lambda (a &optional b &rest arg) (values a b arg)) 10)
  10 nil nil)

(deftest lambda-rest.7
  (funcall (lambda (a &optional b &rest arg) (values a b arg)) 10 20)
  10 20 nil)

(deftest lambda-rest.8
  (funcall (lambda (a &optional b &rest arg) (values a b arg)) 10 20 30 40)
  10 20 (30 40))

(deftest-error lambda-rest.9
  (funcall (lambda (a b &optional c &rest arg) (values a b c arg)) 10))

(deftest lambda-key.1
  (funcall (lambda (&key aaa) aaa) :aaa 10)
  10)

(deftest lambda-key.2
  (funcall (lambda (&key (aaa)) aaa) :aaa 10)
  10)

(deftest lambda-key.3
  (funcall (lambda (&key aaa) aaa))
  nil)

(deftest lambda-key.4
  (funcall (lambda (&key (aaa 200)) aaa) :aaa 999)
  999)

(deftest lambda-key.5
  (funcall (lambda (&key (aaa 200) bbb) (values aaa bbb)))
  200 nil)

(deftest lambda-key.6
  (funcall (lambda (&key (aaa 200) (bbb aaa)) (values aaa bbb)) :aaa 10)
  10 10)

(deftest lambda-key.7
  (let ((aaa 100)
        (bbb 200))
    (declare (ignorable aaa bbb))
    (funcall (lambda (&key (aaa bbb) (bbb aaa)) (values aaa bbb)) :aaa 10))
  10 10)

(deftest lambda-key.8
  (let ((aaa 100)
        (bbb 200))
    (declare (ignorable aaa bbb))
    (funcall (lambda (&key (aaa bbb) (bbb aaa)) (values aaa bbb)) :bbb 10))
  200 10)

(deftest lambda-key.9
  (let ((aaa 100)
        (bbb 200))
    (declare (ignorable aaa bbb))
    (funcall (lambda (&key (aaa bbb) (bbb aaa)) (values aaa bbb)) :aaa 10 :bbb 20))
  10 20)

(deftest lambda-key.10
  (funcall (lambda (&key ((hello a))) a) 'hello 100)
  100)

(deftest lambda-key.11
  (funcall (lambda (&key ((hello a) 10 s)) (values a s)) 'hello 100)
  100 t)

(deftest lambda-key.12
  (funcall (lambda (&key ((hello a) 10 s)) (values a s)))
  10 nil)

(deftest lambda-key.13
  (funcall (lambda (&key aaa bbb) (values aaa bbb)) :aaa 10 :bbb 20)
  10 20)

(deftest lambda-key.14
  (funcall (lambda (&key aaa bbb) (values aaa bbb)) :aaa 10 :bbb 20 :aaa 30)
  10 20)

(deftest-error lambda-key.15
  (funcall (lambda (&key aaa bbb) (values aaa bbb)) :aaa 10 :bbb 20 :ccc 30))

(deftest lambda-key.16
  (funcall (lambda (&key aaa bbb &allow-other-keys) (values aaa bbb))
           :aaa 10 :bbb 20 :ccc 30)
  10 20)

(deftest lambda-key.17
  (funcall (lambda (&rest args &key aaa bbb &allow-other-keys)
             (values args aaa bbb))
           :aaa 10 :bbb 20 :ccc 30)
  (:aaa 10 :bbb 20 :ccc 30) 10 20)

(deftest lambda-aux.1
  (funcall (lambda (&aux a) a))
  nil)

(deftest lambda-aux.2
  (funcall (lambda (&aux (a 10)) a))
  10)

(deftest lambda-aux.3
  (let ((a 999))
    (declare (ignorable a))
    (funcall (lambda (&aux (a 10) (b a) c) (values a b c))))
  10 10 nil)

(deftest-error lambda-aux.4
  (funcall (lambda (&aux a) a) 10))

(deftest-error lambda-error.1
  (eval '(lambda (a a) a)))

(deftest-error lambda-error.2
  (eval '(lambda (a &optional a) a)))

(deftest-error lambda-error.3
  (eval '(lambda (a &optional (b 10 a)) a b)))

(deftest-error lambda-error.4
  (eval '(lambda (a &key a) a)))

(deftest-error lambda-error.5
  (eval '(lambda (a &key (b nil a)) a b)))

(deftest-error lambda-error.6
  (eval '(lambda (a &aux a) a)))

(deftest lambda-closure.1
  (let (inc show)
    (let* ((value 10)
           (a (lambda () (setq value (1+ value))))
           (b (lambda () value)))
      (setq inc a)
      (setq show b))
    (funcall inc)
    (funcall inc)
    (funcall inc)
    (funcall show))
  13)

(deftest lambda-closure.2
  (let ((a 10) call)
    (flet ((aaa () (setq a (1+ a))))
      (setq call (lambda () (aaa))))
    (funcall call)
    (funcall call)
    (funcall call)
    a)
  13)

(deftest lambda-closure.3
  (let (call)
    (block aaa
      (block bbb
        (setq call (lambda () (return-from aaa 10))))
      (funcall call)
      200))
  10)

(deftest lambda-closure.4
  (let (call)
    (block aaa
      (setq call (lambda () (return-from aaa 10)))
      (block aaa
        (funcall call)
        111)
      222))
  10)

(deftest lambda-closure.5
  (let (call)
    (block aaa
      (setq call (lambda () (return-from aaa 10)))
      (block aaa
        (funcall call)
        111)
      222)
    333)
  333)

(deftest-error lambda-closure.6
  (let (call)
    (block aaa
      (block bbb
        (setq call (lambda () (return-from aaa 10)))))
    (funcall call)
    200))

(deftest lambda-closure.7
  (let (call value)
    (tagbody
      10
      (setq call (lambda () (go 999)))
      (tagbody
        (funcall call)
        999
        (setq value 333)
        (go final))
      (setq value 111)
      (go final)
      999
      (setq value 222)
      final)
    value)
  222)

(deftest-error lambda-closure.8
  (let (call)
    (tagbody
      (setq call (lambda () (go final)))
      final)
    (funcall call)))

(deftest lambda-closure.9
  (let (call cons)
    (flet (((setf aaa) (value cons) (setf (car cons) value)))
      (setq call (lambda (x) (setf (aaa x) 100))))
    (setq cons (cons 10 20))
    (funcall call cons)
    cons)
  (100 . 20))

(deftest lambda-documentation.1
  (funcall (lambda () "Hello"))
  "Hello")

(deftest lambda-declare.1
  (funcall (lambda (x)
             (declare (type integer x))
             x)
           100)
  100)

(deftest-error lambda-declare.2
  (funcall (lambda (x)
             (declare (type integer x))
             x)
           "Hello")
  type-error)


;;
;;  eval
;;
(deftest eval.1
  (eval '(+ 1 2))
  3)

(deftest eval.2
  (progn
    (setq test-eval-value 10)
    (let ((test-eval-value 1000))
      (declare (ignorable test-eval-value))
      (eval '(1+ test-eval-value))))
  11)

(deftest eval.3
  (eval (list 'cdr (car '((quote (a . b)) c))))
  b)


;;
;;  eval-when
;;
(deftest eval-when.1
  (eval-when (:execute)
    10)
  10)

(deftest eval-when.2
  (eval-when (eval)
    10)
  10)

(deftest eval-when.3
  (eval-when ()
    10)
  nil)


;;
;;  quote
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
;;  macro-function
;;
(deftest macro-function.1
  (functionp
    (macro-function 'dolist))
  t)


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

