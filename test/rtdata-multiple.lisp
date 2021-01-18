;;
;;  ANSI COMMON LISP: 5. Data and Control Flow
;;

;;
;;  Macro MULTIPLE-VALUE-BIND
;;
(deftest multiple-value-bind.1
  (multiple-value-bind nil nil)
  nil)

(deftest multiple-value-bind.2
  (multiple-value-bind nil (values))
  nil)

(deftest multiple-value-bind.3
  (multiple-value-bind nil nil 10)
  10)

(deftest multiple-value-bind.4
  (multiple-value-bind (a) nil a 10)
  10)

(deftest multiple-value-bind.5
  (multiple-value-bind (a) nil a)
  nil)

(deftest multiple-value-bind.6
  (multiple-value-bind (a) (values) a)
  nil)

(deftest multiple-value-bind.7
  (multiple-value-bind (a) 10 a)
  10)

(deftest multiple-value-bind.8
  (multiple-value-bind (a) (values 10 20) a)
  10)

(deftest multiple-value-bind.9
  (multiple-value-bind (a b) (values 10 20) b a)
  10)

(deftest multiple-value-bind.10
  (multiple-value-bind (a b) (values 10 20) a b)
  20)

(deftest multiple-value-bind.11
  (multiple-value-bind (a b c) (values 10 20) a b c)
  nil)

(deftest multiple-value-bind.12
  (multiple-value-bind (a b c) (values 10 20)
    (declare (ignorable a b c) (special b))
    (symbol-value 'b))
  20)

(deftest-error multiple-value-bind-error.1
  (eval '(multiple-value-bind (a b c))))

(deftest-error multiple-value-bind-error.2
  (eval '(multiple-value-bind 10 20)))

;;  ANSI Common Lisp
(deftest multiple-value-bind-test.1
  (multiple-value-bind (f r)
    (floor 130 11)
    (list f r))
  (11 9))


;;
;;  Special Operator MULTIPLE-VALUE-CALL
;;
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

(deftest-error multiple-value-call-error.1
  (eval '(multiple-value-call)))

(deftest-error multiple-value-call-error.2
  (eval '(multiple-value-call 10)))

;;  ANSI Common Lisp
(deftest multiple-value-call-test.1
  (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5))
  (1 / 2 3 / / 2 0.5))

(deftest multiple-value-call-test.2
  (+ (floor 5 3) (floor 19 4))
  5)

(deftest multiple-value-call-test.3
  (multiple-value-call #'+ (floor 5 3) (floor 19 4))
  10)


;;
;;  Macro MULTIPLE-VALUE-LIST
;;
(deftest multiple-value-list.1
  (multiple-value-list (values))
  nil)

(deftest multiple-value-list.2
  (multiple-value-list nil)
  (nil))

(deftest multiple-value-list.3
  (multiple-value-list 10)
  (10))

(deftest multiple-value-list.4
  (multiple-value-list (values 10 20 30))
  (10 20 30))

(deftest-error multiple-value-list-error.1
  (eval '(multiple-value-list)))

(deftest-error multiple-value-list-error.2
  (eval '(multiple-value-list 10 20)))

(deftest multiple-value-list-test.1
  (multiple-value-list (floor -3 4))
  (-1 1))


;;
;;  Special Operator MULTIPLE-VALUE-PROG1
;;
(deftest multiple-value-prog1.1
  (multiple-value-prog1 nil)
  nil)

(deftest multiple-value-prog1.2
  (multiple-value-prog1 (values)))

(deftest multiple-value-prog1.3
  (multiple-value-prog1 nil 10 20 30)
  nil)

(deftest multiple-value-prog1.4
  (multiple-value-prog1 (values) 10 20 30))

(deftest multiple-value-prog1.5
  (multiple-value-prog1 (values 10 20))
  10 20)

(deftest multiple-value-prog1.6
  (multiple-value-prog1 (values 10 20) 30 40 50)
  10 20)

(deftest multiple-value-prog1.7
  (let (a b c)
    (multiple-value-prog1
      (setq a 10)
      (setq b 20)
      (setq c 30)))
  10)

(deftest multiple-value-prog1.8
  (let (a b c)
    (multiple-value-prog1
      (setq a 10)
      (setq b 20)
      (setq c 30))
    (values a b c))
  10 20 30)

(deftest-error multiple-value-prog1-error.1
  (eval '(multiple-value-prog1)))

(deftest multiple-value-prog1-test.1
  (let ((temp '(1 2 3)))
    (multiple-value-prog1
      (values-list temp)
      (setq temp nil)
      (values-list temp)))
  1 2 3)


;;
;;  Macro MULTIPLE-VALUE-SETQ
;;
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
  (let (a)
    (multiple-value-setq (a) (values 10 20 30 40)))
  10)

(deftest multiple-value-setq.4
  (let ((a 10) (b 20) (c 30))
    (multiple-value-setq (a b c) 40)
    (values a b c))
  40 nil nil)

(deftest multiple-value-setq.5
  (let (a b c)
    (multiple-value-setq (a b c) (values 10 20 30 40 50)))
  10)

(deftest multiple-value-setq.6
  (let (a b c)
    (multiple-value-setq (a b c) (values 10 20 30 40 50))
    (values a b c))
  10 20 30)

(deftest multiple-value-setq.7
  (progn
    (defclass multiple-value-setq-test () (aaa bbb))
    (let ((inst (make-instance 'multiple-value-setq-test)))
      (with-slots (aaa bbb) inst
        (multiple-value-setq (aaa bbb) (values 10 20 30 40 50)))
      (values (slot-value inst 'aaa)
              (slot-value inst 'bbb))))
  10 20)

(deftest-error multiple-value-setq-error.1
  (eval '(let (a b) (multiple-value-setq (a b)))))

(deftest-error multiple-value-setq-error.2
  (eval '(let (a b) (multiple-value-setq (a b) 10 20))))

(deftest-error multiple-value-setq-error.3
  (eval '(multiple-value-setq 10 20)))

(deftest multiple-value-setq-test.1
  (let (quotient remainder)
    (values
      (multiple-value-setq (quotient remainder) (truncate 3.2 2))
      quotient
      remainder))
  1 1 1.2)

(deftest multiple-value-setq-test.2
  (let (a b c)
    (values
      (multiple-value-setq (a b c) (values 1 2))
      a b c))
  1 1 2 nil)

(deftest multiple-value-setq-test.3
  (let (a b)
    (values
      (multiple-value-setq (a b) (values 4 5 6))
      a b))
  4 4 5)


;;
;;  Accessor VALUES
;;
(deftest values.1
  (values))

(deftest values.2
  (values 10)
  10)

(deftest values.3
  (values 10 20 30)
  10 20 30)

(deftest values.4
  (values 10 20 (+ 30 40))
  10 20 70)

(deftest values.5
  (values (values 10 20))
  10)

(deftest values.6
  (values (values 10 20) (values 30 40))
  10 30)

(deftest values.7
  (funcall #'values))

(deftest values.8
  (funcall #'values 10 20 30)
  10 20 30)

(deftest values-test.1
  (values))

(deftest values-test.2
  (values 1)
  1)

(deftest values-test.3
  (values 1 2)
  1 2)

(deftest values-test.4
  (values 1 2 3)
  1 2 3)

(deftest values-test.5
  (values (values 1 2 3) 4 5)
  1 4 5)

(defun values-test-polar (x y)
  (values (sqrt (+ (* x x) (* y y))) (atan y x)))

(deftest values-test.6
  (multiple-value-bind (r theta) (values-test-polar 3.0 4.0)
    (vector r theta))
  #(5.0 0.9272952))


;;
;;  Accessor (SETF VALUES)
;;
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

(deftest setf-values.4
  (let ((a (cons 10 20))
        (b (cons 30 40)))
    (setf (values (car a) (cdr b)) (values 5 6 7 8 9)))
  5 6)

(deftest setf-values.5
  (let ((a (cons 10 20))
        (b (cons 30 40)))
    (setf (values (car a) (cdr b)) (values 5 6 7 8 9))
    (values a b))
  (5 . 20) (30 . 6))


;;
;;  Function VALUES-LIST
;;
(deftest values-list.1
  (values-list nil))

(deftest values-list.2
  (values-list '(10 20 30))
  10 20 30)

(deftest-error! values-list-error.1
  (eval '(values-list)))

(deftest-error values-list-error.2
  (eval '(values-list 10))
  type-error)

(deftest-error! values-list-error.3
  (eval '(values-list nil nil)))

;;  ANSI Common Lisp
(deftest values-list-test.1
  (values-list nil))

(deftest values-list-test.2
  (values-list '(1))
  1)

(deftest values-list-test.3
  (values-list '(1 2))
  1 2)

(deftest values-list-test.4
  (values-list '(1 2 3))
  1 2 3)


;;
;;  Macro NTH-VALUE
;;
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

(deftest-error nth-value-error.1
  (eval '(nth-value 10)))

(deftest-error nth-value-error.2
  (eval '(nth-value :hello)))

(deftest-error nth-value-error.3
  (eval '(nth-value 10 20 30)))

;;  ANSI Common Lisp
(deftest nth-value-test.1
  (nth-value 0 (values 'a 'b))
  a)

(deftest nth-value-test.2
  (nth-value 1 (values 'a 'b))
  b)

(deftest nth-value-test.3
  (nth-value 2 (values 'a 'b))
  nil)

(deftest nth-value-test.4
  (let* ((x 83927472397238947423879243432432432)
         (y 32423489732)
         (a (nth-value 1 (floor x y)))
         (b (mod x y)))
    (values a b (= a b)))
  3332987528 3332987528 t)

