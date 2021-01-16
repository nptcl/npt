;;
;;  ANSI COMMON LISP: 5. Data and Control Flow
;;

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
  nil)

(deftest psetf.3
  (let (x)
    (psetf x 10)
    x)
  10)

(deftest psetf.4
  (let (x y)
    (values (psetf x 10 y 20) x y))
  nil 10 20)

(deftest psetf.5
  (let ((x '(10 . 20)))
    (values (psetf (car x) 30) x))
  nil (30 . 20))

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

(deftest ccase.1
  (let ((x 'hello))
    (values
      (handler-bind
        ((type-error
           (lambda (c)
             (store-value 999 c))))
        (ccase x (10 :aaa) (20 :bbb) (999 :ccc)))
      x))
  :ccc 999)

(deftest ccase.2
  (let ((x '(a b c d)))
    (values
      (handler-bind
        ((type-error
           (lambda (c)
             (store-value 999 c))))
        (ccase (car x) (10 :aaa) (20 :bbb) (999 :ccc)))
      x))
  :ccc (999 b c d))

(deftest ctypecase.1
  (let ((x 'hello))
    (values
      (handler-bind
        ((type-error
           (lambda (c)
             (store-value 999 c))))
        (ctypecase x (string :aaa) (integer :bbb) (float :ccc)))
      x))
  :bbb 999)

(deftest ctypecase.2
  (let ((x '(a b c d)))
    (values
      (handler-bind
        ((type-error
           (lambda (c)
             (store-value 999 c))))
        (ctypecase (car x) (string :aaa) (integer :bbb) (float :ccc)))
      x))
  :bbb (999 b c d))

