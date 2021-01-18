;;
;;  ANSI COMMON LISP: 5. Data and Control Flow
;;

;;
;;  Macro DEFINE-MODIFY-MACRO
;;
(deftest define-modify-macro.1
  (define-modify-macro test-modify-1 (&rest args) append)
  test-modify-1)

(deftest define-modify-macro.2
  (progn
    (define-modify-macro test-modify-2 (&rest args) append)
    (let ((x (list 10 20 30)))
      (test-modify-2 x '(a b c))))
  (10 20 30 a b c))

(deftest define-modify-macro.3
  (progn
    (define-modify-macro test-modify-3 (&rest args) append)
    (let ((x (list 10 20 30)))
      (test-modify-3 x '(a b c))
      x))
  (10 20 30 a b c))

(deftest define-modify-macro.4
  (progn
    (define-modify-macro test-modify-4 (&rest args) append "Hello")
    (documentation 'test-modify-4 'function))
  "Hello")

(deftest-error define-modify-macro-error.1
  (eval '(define-modify-macro tset-modify-error (a))))

(deftest-error define-modify-macro-error.2
  (eval '(define-modify-macro tset-modify-error (a) append nil)))

;;  ANSI Common Lisp
(deftest define-modify-macro-test.1
  (define-modify-macro define-modify-macro-appendf (&rest args)
    append "Append onto list")
  define-modify-macro-appendf)

(deftest define-modify-macro-test.2
  (let* ((x '(a b c))
         (y x))
    (values
      (define-modify-macro-appendf x '(d e f) '(1 2 3))
      x y))
  (a b c d e f 1 2 3)
  (a b c d e f 1 2 3)
  (a b c))

(deftest define-modify-macro-test.3
  (progn
    (define-modify-macro define-modify-macro-new-incf (&optional (delta 1)) +)
    (let ((x 10))
      (define-modify-macro-new-incf x)
      x))
  11)

(deftest define-modify-macro-test.4
  (progn
    (define-modify-macro define-modify-macro-new-incf (&optional (delta 1)) +)
    (let ((x 10))
      (define-modify-macro-new-incf x 2)
      x))
  12)

(deftest define-modify-macro-test.5
  (progn
    (define-modify-macro define-modify-macro-unionf (other-set &rest keywords) union)
    (let ((x '(a b c d)))
      (define-modify-macro-unionf x '(a b e d))
      (sort x #'string< :key #'symbol-name)))
  (a b c d e))



;;
;;
;;
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

