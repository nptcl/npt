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

