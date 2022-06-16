;;
;;  ANSI COMMON LISP: 5. Data and Control Flow
;;

;;
;;  Macro DESTRUCTURING-BIND
;;
(deftest destructuring-bind-var.1
  (destructuring-bind () nil
    10)
  10)

(deftest destructuring-bind-var.2
  (destructuring-bind (a) '(10)
    a)
  10)

(deftest destructuring-bind-var.3
  (destructuring-bind (a b) '(10 20)
    (values a b))
  10 20)

(deftest-error destructuring-bind-var.4
  (destructuring-bind (a b) '(10)
    (values a b)))

(deftest-error destructuring-bind-var.5
  (destructuring-bind (a b) '(10 20 30)
    (values a b)))

(deftest-error destructuring-bind-var.6
  (eval '(destructuring-bind (a a) '(10 20)
           a)))


;;  &optional
(deftest destructuring-bind-opt.1
  (destructuring-bind (&optional a) nil
    a)
  nil)

(deftest destructuring-bind-opt.2
  (destructuring-bind (&optional a) '(10)
    a)
  10)

(deftest-error destructuring-bind-opt.3
  (destructuring-bind (&optional a) '(10 20)
    a))

(deftest destructuring-bind-opt.4
  (destructuring-bind (a &optional b) '(10)
    (values a b))
  10 nil)

(deftest destructuring-bind-opt.5
  (destructuring-bind (a &optional b) '(10 20)
    (values a b))
  10 20)

(deftest-error destructuring-bind-opt.6
  (destructuring-bind (a &optional b) nil
    (values a b)))

(deftest-error destructuring-bind-opt.7
  (destructuring-bind (a &optional b) '(10 20 30)
    (values a b)))

(deftest destructuring-bind-opt.8
  (destructuring-bind (a &optional (b 99)) '(10)
    (values a b))
  10 99)

(deftest destructuring-bind-opt.9
  (destructuring-bind (a &optional (b 99)) '(10 20)
    (values a b))
  10 20)

(deftest destructuring-bind-opt.10
  (destructuring-bind (a &optional (b 99 c)) '(10)
    (values a b c))
  10 99 nil)

(deftest destructuring-bind-opt.11
  (destructuring-bind (a &optional (b 99 c)) '(10 20)
    (values a b c))
  10 20 t)

(deftest destructuring-bind-opt.12
  (destructuring-bind (&optional a (b 99) c) nil
    (values a b c))
  nil 99 nil)

(deftest destructuring-bind-opt.13
  (destructuring-bind (&optional a b c) '(10 20)
    (values a b c))
  10 20 nil)

(deftest destructuring-bind-opt.14
  (destructuring-bind (&optional a b c) '(10 20 30)
    (values a b c))
  10 20 30)

(deftest-error destructuring-bind-opt.15
  (eval '(destructuring-bind (&optional a b a) '(10 20 30)
           (values a b))))

(deftest-error destructuring-bind-opt.16
  (eval '(destructuring-bind (a &optional (b 20 a)) '(10 20)
           (values a b))))


;;  &rest, &body, dot
(deftest destructuring-bind-rest.1
  (destructuring-bind (&rest a) nil
    a)
  nil)

(deftest destructuring-bind-rest.2
  (destructuring-bind (&rest a) '(10 20 30)
    a)
  (10 20 30))

(deftest destructuring-bind-rest.3
  (destructuring-bind (a &body b) '(10)
    (values a b))
  10 nil)

(deftest destructuring-bind-rest.4
  (destructuring-bind (&optional a &body b) '(10 20 30)
    (values a b))
  10 (20 30))

(deftest destructuring-bind-rest.5
  (destructuring-bind (a . b) '(10)
    (values a b))
  10 nil)

(deftest destructuring-bind-rest.6
  (destructuring-bind (a . b) '(10 20 30)
    (values a b))
  10 (20 30))

(deftest-error destructuring-bind-rest.7
  (eval '(destructuring-bind (&rest a b) '(10 20 30)
           (values a b))))

(deftest destructuring-bind-rest.8
  (destructuring-bind (a b &optional c d &rest e) '(10 20 30 40 50 60)
    (values a b c d e))
  10 20 30 40 (50 60))


;;  &key
(deftest destructuring-bind-key.1
  (destructuring-bind (a &key hello) '(10)
    (values a hello))
  10 nil)

(deftest destructuring-bind-key.2
  (destructuring-bind (a &key hello) '(10 :hello 20)
    (values a hello))
  10 20)

(deftest-error destructuring-bind-key.3
  (destructuring-bind (a &key hello) '(10 :aaa 20)
    (values a hello)))

(deftest-error destructuring-bind-key.4
  (destructuring-bind (a &key hello) '(10 :hello)
    (values a hello)))

(deftest destructuring-bind-key.5
  (destructuring-bind (a &key (hello 99)) '(10)
    (values a hello))
  10 99)

(deftest destructuring-bind-key.6
  (destructuring-bind (a &key (hello 99)) '(10 :hello 20)
    (values a hello))
  10 20)

(deftest destructuring-bind-key.7
  (destructuring-bind (a &key (hello 99 b)) '(10)
    (values a hello b))
  10 99 nil)

(deftest destructuring-bind-key.8
  (destructuring-bind (a &key (hello 99 b)) '(10 :hello 20)
    (values a hello b))
  10 20 t)

(deftest destructuring-bind-key.9
  (destructuring-bind (a &key (hello)) '(10 :hello 20)
    (values a hello))
  10 20)

(deftest destructuring-bind-key.10
  (destructuring-bind (a &key ((:hello b))) '(10 :hello 20)
    (values a b))
  10 20)

(deftest destructuring-bind-key.11
  (destructuring-bind (a &key ((hello b) 99 c)) '(10 hello 20)
    (values a b c))
  10 20 t)

(deftest destructuring-bind-key.12
  (destructuring-bind (a &key hello &allow-other-keys) '(10 :aaa 20)
    (values a hello))
  10 nil)

(deftest destructuring-bind-key.13
  (destructuring-bind (a &key hello &allow-other-keys) '(10 :hello 20)
    (values a hello))
  10 20)

(deftest destructuring-bind-key.14
  (destructuring-bind (&key aaa bbb) '(:aaa 10 :bbb 20)
    (values aaa bbb))
  10 20)

(deftest destructuring-bind-key.15
  (destructuring-bind (&key aaa bbb) '(:bbb 10 :aaa 20)
    (values aaa bbb))
  20 10)

(deftest destructuring-bind-key.16
  (destructuring-bind (&key aaa bbb) '(:bbb 10)
    (values aaa bbb))
  nil 10)

(deftest destructuring-bind-key.17
  (destructuring-bind (&key aaa bbb &allow-other-keys) '(:hello 20)
    (values aaa bbb))
  nil nil)

(deftest destructuring-bind-key.18
  (destructuring-bind (&rest a &key aaa bbb &allow-other-keys) '(:hello 20)
    (values a aaa bbb))
  (:hello 20) nil nil)


;;  &aux
(deftest destructuring-bind-aux.1
  (destructuring-bind (a &aux z) '(10)
    (values a z))
  10 nil)

(deftest destructuring-bind-aux.2
  (destructuring-bind (a &aux (z 20)) '(10)
    (values a z))
  10 20)


;;  &whole
(deftest destructuring-bind-whole.1
  (destructuring-bind (&whole a b c) '(10 20)
    (values a b c))
  (10 20) 10 20)

(deftest destructuring-bind-whole.2
  (destructuring-bind (&whole a b c &optional d) '(10 20)
    (values a b c d))
  (10 20) 10 20 nil)

(deftest destructuring-bind-whole.3
  (destructuring-bind (&whole a &rest b) '(10 20 30)
    (values a b))
  (10 20 30) (10 20 30))


;;  cons
(deftest destructuring-bind-cons.1
  (destructuring-bind (a (b) . c) '(10 (20) 30 40)
    (values a b c))
  10 20 (30 40))

(deftest destructuring-bind-cons.2
  (destructuring-bind (&whole w a (&whole z b) . c) '(10 (20) 30 40)
    (values a b c w z))
  10 20 (30 40) (10 (20) 30 40) (20))

(deftest destructuring-bind-cons.3
  (destructuring-bind (a (b &optional c) &body d) '(10 (20) 30 40)
    (values a b c d))
  10 20 nil (30 40))

(deftest destructuring-bind-cons.4
  (destructuring-bind (a (b &optional c) &body d) '(10 (20 21) 30 40)
    (values a b c d))
  10 20 21 (30 40))


;;  others
(deftest destructuring-bind.1
  (let ((a 111) (b 222))
    (declare (ignorable a b))
    (destructuring-bind (a b) '(10 20)
      (destructuring-bind (c d) '(30 40)
        (values a b c d))))
  10 20 30 40)

(deftest destructuring-bind.2
  (destructuring-bind (destructuring-bind-test-1) '(10)
    (declare (special destructuring-bind-test-1))
    (symbol-value 'destructuring-bind-test-1))
  10)

(deftest destructuring-bind.3
  (destructuring-bind nil nil)
  nil)

(deftest destructuring-bind.4
  (destructuring-bind (x) '(10)
    (declare (ignore x)))
  nil)

(deftest-error destructuring-bind-error.1
  (eval '(destructuring-bind nil)))

(deftest-error destructuring-bind-error.2
  (eval '(destructuring-bind 10)))


;;  ANSI Common Lisp
(defun destructuring-bind-iota (n)
  (loop for i from 1 to n collect i))

(deftest destructuring-bind-test.1
  (destructuring-bind ((a &optional (b 'bee)) one two three)
    `((alpha) ,@(destructuring-bind-iota 3))
    (list a b three two one))
  (alpha bee 3 2 1))


;;
;;  Bugfix
;;
(deftest destructuring-bind-bugfix.1
  (let ((cons (list 10 20 30)))
    (destructuring-bind (root . tail) cons
      (declare (ignore root))
      (setf (cdr tail) 40)
      cons))
  (10 20 . 40))

(deftest destructuring-bind-bugfix.2
  (let ((cons (list 10 20 30)))
    (destructuring-bind (root . tail) cons
      (declare (ignore root))
      (values
        (eq (cdr cons) tail)
        (eq (cddr cons) (cdr tail)))))
  t t)

(defun destructuring-bind-bugfix-call (car &rest cdr)
  (declare (ignore car))
  cdr)

(deftest destructuring-bind-bugfix.3
  (let ((x (list 10 20 30 40)))
    (values
      (eq (apply #'destructuring-bind-bugfix-call x) (cdr x))
      (equal (apply #'destructuring-bind-bugfix-call x) (cdr x))))
  nil t)


;;
;;  Bugfix: bind &optional, &rest, &key.
;;
(deftest bind-optional.1
  (destructuring-bind (&optional (())) nil
    10)
  10)

(deftest bind-optional.2
  (progn
    (defmacro bind-optional-2 (&optional (()))
      20)
    (bind-optional-2))
  20)

(deftest bind-optional.3
  (destructuring-bind (&optional (())) '(nil)
    30)
  30)

(deftest bind-optional.4
  (progn
    (defmacro bind-optional-4 (&optional (()))
      40)
    (bind-optional-4 nil))
  40)

(deftest bind-optional.5
  (destructuring-bind (&optional ((a))) '((50))
    a)
  50)

(deftest bind-optional.6
  (progn
    (defmacro bind-optional-6 (&optional ((a)))
      a)
    (bind-optional-6 (50)))
  50)

(deftest-error bind-optional.7
  (destructuring-bind (&optional ((a))) nil
    a))

(deftest-error bind-optional.8
  (destructuring-bind (&optional ((a))) '(80)
    a))

(deftest-error bind-optional.9
  (progn
    (defmacro bind-optional-9 (&optional ((a)))
      a)
    (eval '(bind-optional-9))))

(deftest-error bind-optional.10
  (progn
    (defmacro bind-optional-10 (&optional ((a)))
      a)
    (eval '(bind-optional-10 100))))

(deftest bind-optional.11
  (destructuring-bind (a &optional ((b c &rest d))) '(10 (20 30 40 50))
    (list a b c d))
  (10 20 30 (40 50)))

(deftest bind-optional.12
  (progn
    (defmacro bind-optional-12 (a &optional ((b c &rest d)))
      `(list ',a ',b ',c ',d))
    (bind-optional-12 10 (20 30 40 50)))
  (10 20 30 (40 50)))

(deftest-error bind-optional.13
  (destructuring-bind (a &optional ((b c &rest d))) '(10)
    (list a b c d)))

(deftest-error bind-optional.14
  (progn
    (defmacro bind-optional-14 (a &optional ((b c &rest d)))
      `(list ',a ',b ',c ',d))
    (eval '(bind-optional-14 10))))

(deftest bind-optional.15
  (let (value)
    (handler-bind ((warning
                     (lambda (c)
                       (setq value t)
                       (muffle-warning c))))
      (eval '(destructuring-bind (&optional ((a))) '((30))))
      value))
  t)

(deftest bind-optional.16
  (let (value)
    (handler-bind ((warning
                     (lambda (c)
                       (setq value t)
                       (muffle-warning c))))
      (eval '(defmacro bind-optional-16 (&optional ((a))) '((30))))
      value))
  t)

(deftest bind-optional.17
  (destructuring-bind (&optional ((a b) '(10 20))) nil
    (list a b))
  (10 20))

(deftest bind-optional.18
  (progn
    (defmacro bind-optional-18 (&optional ((a b) '(10 20)))
      `(list ',a ',b))
    (bind-optional-18))
  (10 20))

(deftest bind-rest.1
  (destructuring-bind (a &rest nil) '(10)
    a)
  10)

(deftest bind-rest.2
  (progn
    (defmacro bind-rest-2 (a &rest nil)
      a)
    (bind-rest-2 20))
  20)

(deftest bind-rest.3
  (destructuring-bind (a &rest (b c)) '(10 20 30)
    (list a b c))
  (10 20 30))

(deftest bind-rest.4
  (progn
    (defmacro bind-rest-4 (a &rest (b c))
      `(list ',a ',b ',c))
    (bind-rest-4 10 20 30))
  (10 20 30))

(deftest-error bind-rest.5
  (destructuring-bind (&rest (a b)) nil
    (list a b)))

(deftest-error bind-rest.6
  (progn
    (defmacro bind-rest-6 (&rest (a b))
      `(list ',a ',b))
    (eval '(bind-rest-6))))

(deftest bind-key.1
  (destructuring-bind (a &key ((:b nil))) '(10 :b nil)
    a)
  10)

(deftest bind-key.2
  (progn
    (defmacro bind-key-2 (a &key ((:b nil)))
      a)
    (bind-key-2 20 :b nil))
  20)

(deftest bind-key.3
  (destructuring-bind (a &key ((:b (b c)))) '(10 :b (20 30))
    (list a b c))
  (10 20 30))

(deftest bind-key.4
  (progn
    (defmacro bind-key-4 (a &key ((:b (b c))))
      `(list ',a ',b ',c))
    (bind-key-4 10 :b (20 30)))
  (10 20 30))

(deftest-error bind-key.5
  (destructuring-bind (a &key ((:b (b c)))) '(10 :b (20 30 40))
    (list a b c)))

(deftest-error bind-key.6
  (progn
    (defmacro bind-key-6 (a &key ((:b (b c))))
      `(list ',a ',b ',c))
    (eval '(bind-key-6 10 :b (20 30 40)))))

(deftest-error bind-others.1
  (eval '(destructuring-bind (a &aux ((b c) '(20 30))) '(10)
           (list a b c))))

(deftest-error bind-others.2
  (eval '(defmacro bind-others-2 (a &aux ((b c) '(20 30))) '(10)
           `(list ',a ',b ',c))))

(deftest bind-others.3
  (destructuring-bind (&optional ((a b))) '((10 20))
    (declare (special a))
    (list (symbol-value 'a) b))
  (10 20))

(deftest bind-others.4
  (progn
    (defmacro bind-others-4 (&optional ((a b)))
      (declare (special a))
      `(list ',(symbol-value 'a) ',b))
    (bind-others-4 (10 20)))
  (10 20))

(deftest bind-others.5
  (progn
    (defmacro bind-others-5 (&optional ((a b &rest c) '(nil nil)) &rest z)
      `(list ',a ',b ',c ',z))
    (values
      (bind-others-5)
      (bind-others-5 (10 20 30 40) 50 60)))
  (nil nil nil nil)
  (10 20 (30 40) (50 60)))

(deftest-error bind-others.6
  (progn
    (defmacro bind-others-6 (&optional ((a b &rest c) '(nil nil)) &rest z)
      `(list ',a ',b ',c ',z))
    (eval '(bind-others-6 (10)))))

(deftest bind-others.7
  (progn
    (defmacro bind-others-7 (&optional ((&optional a b &rest c)) &rest z)
      `(list ',a ',b ',c ',z))
    (bind-others-7 (10)))
  (10 nil nil nil))

