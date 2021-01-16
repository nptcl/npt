;;
;;  ANSI COMMON LISP: 5. Data and Control Flow
;;

;;
;;  Special Operator LET
;;
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

(deftest let.7
  (let ((let-special-variable 100))
    (declare (special let-special-variable))
    (symbol-value 'let-special-variable))
  100)

(deftest-error let-error.1
  (eval '(let)))

(deftest-error let-error.2
  (eval '(let 10)))

(deftest-error let-error.3
  (eval '(let (30))))

(deftest-error let-error.4
  (eval '(let ((40 50)))))


;;
;;  Special Operator LET*
;;
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

(deftest let*.7
  (let* ((let*-special-variable 100))
    (declare (special let*-special-variable))
    (symbol-value 'let*-special-variable))
  100)

(deftest-error let*-error.1
  (eval '(let*)))

(deftest-error let*-error.2
  (eval '(let* 10)))

(deftest-error let*-error.3
  (eval '(let* (30))))

(deftest-error let*-error.4
  (eval '(let* ((40 50)))))

;;  ANSI Common Lisp
(setq let-test-variable 'top)

(defun let-test-dummy ()
  let-test-variable)

(deftest let-test.1
  (let ((let-test-variable 'inside)
        (b let-test-variable))
    (format nil "~S ~S ~S" let-test-variable b (let-test-dummy)))
  "INSIDE TOP TOP")

(deftest let-test.2
  (let* ((let-test-variable 'inside)
         (b let-test-variable))
    (format nil "~S ~S ~S" let-test-variable b (let-test-dummy)))
  "INSIDE INSIDE TOP")

(deftest let-test.3
  (let ((let-test-variable 'inside)
        (b let-test-variable))
    (declare (special let-test-variable))
    (format nil "~S ~S ~S" let-test-variable b (let-test-dummy)))
  "INSIDE TOP INSIDE")


;;
;;  Special Operator PROGV
;;
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

(deftest progv.9
  (let ((x 10))
    (declare (ignorable x))
    (progv '(x) '(20)
      x))
  10)

(deftest progv.10
  (let ((x 10))
    (declare (ignorable x))
    (progv '(x) '(20)
      (symbol-value 'x)))
  20)

(deftest-error progv-error.1
  (eval '(progv nil)))

(deftest-error progv-error.2
  (eval '(progv 10 nil)))

(deftest-error progv-error.3
  (eval '(progv '(x) 20)))

(deftest-error progv-error.4
  (eval '(progv '(10) nil)))

;;  ANSI Common Lisp
(setq *progv-test-1* 1)

(deftest progv-test.1
  (progv '(*progv-test-1*) '(2)
    *progv-test-1*)
  2)

(deftest progv-test.2
  *progv-test-1*
  1)

(deftest progv-test.3
  (let ((*progv-test-1* 3))
    (progv '(*progv-test-1*) '(4)
      (list *progv-test-1* (symbol-value '*progv-test-1*))))
  (3 4))


;;
;;  Special Form SETQ
;;
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

(deftest setq.9
  (progn
    (defclass setq-test-class () (a))
    (let ((inst (make-instance 'setq-test-class)))
      (with-slots (a) inst
        (setq a 100))
      (slot-value inst 'a)))
  100)

(deftest-error setq-error.1
  (eval '(setq a)))

(deftest-error setq-error.2
  (eval '(setq 10 20)))

(defconstant setq-error-variable 10)
(deftest-error setq-error.3
  (eval '(setq setq-error-variable 20)))

;;  ANSI Common Lisp
(deftest setq-test.1
  (let (a b c)
    (values
      (setq a 1 b 2 c 3)
      a b c))
  3 1 2 3)

(deftest setq-test.2
  (let (a b c)
    (setq a 1 b 2 c 3)
    (values
      (setq a (1+ b) b (1+ a) c (+ a b))
      a b c))
  7 3 4 7)

(deftest setq-test.3
  (let ((x (list 10 20 30)))
    (symbol-macrolet ((y (car x)) (z (cadr x)))
      (setq y (1+ z) z (1+ y))
      (list x y z)))
  ((21 22 30) 21 22))


;;
;;  Macro PSETQ
;;
(deftest psetq.1
  (psetq)
  nil)

(deftest psetq.2
  (let (a)
    (psetq a 10))
  nil)

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

(deftest psetq.8
  (progn
    (defclass setq-test-class () (a))
    (let ((inst (make-instance 'setq-test-class)))
      (with-slots (a) inst
        (psetq a 100))
      (slot-value inst 'a)))
  100)

(deftest-error psetq-error.1
  (eval '(psetq a)))

(deftest-error psetq-error.2
  (eval '(psetq 10 20)))

(defconstant psetq-error-variable 10)
(deftest-error psetq-error.3
  (eval '(psetq psetq-error-variable 20)))

;;  ANSI Common Lisp
(deftest psetq-test.1
  (let (a b c)
    (values
      (psetq a 1 b 2 c 3)
      a b c))
  nil 1 2 3)

(deftest psetq-test.2
  (let (a b c)
    (psetq a 1 b 2 c 3)
    (values
      (psetq a (1+ b) b (1+ a) c (+ a b))
      a b c))
  nil 3 2 3)

(deftest psetq-test.3
  (let ((x (list 10 20 30)))
    (symbol-macrolet ((y (car x)) (z (cadr x)))
      (psetq y (1+ z) z (1+ y))
      (list x y z)))
  ((21 11 30) 21 11))

(deftest psetq-test.4
  (let ((a 1) (b 2))
    (psetq a b  b a)
    (values a b))
  2 1)


;;
;;  Special Operator BLOCK
;;
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

(deftest-error block-error.1
  (eval '(block)))

(deftest-error block-error.2
  (eval '(block 100)))

;;  ANSI Common Lisp
(deftest block-test.1
  (block empty)
  nil)

(deftest block-test.2
  (block whocares
    (values 1 2)
    (values 3 4))
  3 4)

(deftest block-test.3
  (let ((x 1))
    (block stop
      (setq x 2)
      (return-from stop)
      (setq x 3))
    x)
  2)

(deftest block-test.4
  (block early
    (return-from early (values 1 2))
    (values 3 4))
  1 2)

(deftest block-test.5
  (block outer
    (block inner
      (return-from outer 1))
    2)
  1)

(deftest block-test.6
  (block twin
    (block twin
      (return-from twin 1))
    2)
  2)

(deftest block-test.7
  (block b
    (flet ((b1 () (return-from b 1)))
      (block b (b1) (print 'unreachable))
      2))
  1)


;;
;;  Special Operator CATCH
;;
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

(deftest catch.6
  (catch 'aaa
    (catch 'aaa
      (throw 'aaa 10)
      20)
    30)
  30)

(deftest catch.7
  (catch 10
    (throw 10 20))
  20)

(deftest catch.8
  (catch "Hello" 10)
  10)

(deftest-error catch-error.1
  (eval '(catch)))

(deftest-error catch-error.2
  (catch 'hello
    (throw 'no-such-tag-name 10))
  control-error)

;; ANSI Commom Lisp
(deftest catch-test.1
  (catch 'dummy-tag
    1 2
    (throw 'dummy-tag 3)
    4)
  3)

(deftest catch-test.2
  (catch 'dummy-tag
    1 2 3 4)
  4)

(defun catch-test-throw-back (tag)
  (throw tag t))

(deftest catch-test.3
  (catch 'dummy-tag
    (catch-test-throw-back 'dummy-tag)
    2)
  t)

(deftest catch-test.4
  (catch 'c
    (flet ((c1 () (throw 'c 1)))
      (catch 'c (c1)
        (print 'unreachable))
      2))
  2)


;;
;;  Special Operator TAGBODY
;;
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

;;  ANSI Common Lisp
(deftest tagbody-test.1
  (let (val)
    (tagbody
      (setq val 1)
      (go point-a)
      (incf val 16)
      point-c
      (incf val 04)
      (go point-b)
      (incf val 32)
      point-a
      (incf val 02)
      (go point-c)
      (incf val 64)
      point-b
      (incf val 08))
    val)
  15)

(defvar *tagbody-test*)

(defun tagbody-test-f2 (flag escape)
  (if flag
    (funcall escape)
    2))

(defun tagbody-test-f1 (flag)
  (let ((n 1))
    (tagbody
      (setq n (tagbody-test-f2 flag #'(lambda () (go out))))
      out
      (setq *tagbody-test* n))))

(deftest tagbody-test.2
  (progn
    (setq *tagbody-test* nil)
    (values
      (tagbody-test-f1 nil)
      *tagbody-test*))
  nil 2)

(deftest tagbody-test.3
  (progn
    (setq *tagbody-test* nil)
    (values
      (tagbody-test-f1 t)
      *tagbody-test*))
  nil 1)


;;
;;  Special Operator RETURN-FROM
;;
(deftest return-from.1
  (block nil
    (return-from nil)
    999)
  nil)

(deftest return-from.2
  (block tag
    (return-from tag 100)
    999)
  100)

(deftest return-from.3
  (block aaa
    (return-from aaa (values 10 20 30))
    999)
  10 20 30)

(deftest-error return-from-error.1
  (eval '(return-from)))

(deftest-error return-from-error.2
  (eval '(return-from nil 10 20)))

;;  ANSI Common Lisp
(deftest return-from-test.1
  (block alpha
    (return-from alpha)
    1)
  nil)

(deftest return-from-test.2
  (block alpha
    (return-from alpha 1)
    2)
  1)

(deftest return-from-test.3
  (block alpha
    (return-from alpha (values 1 2))
    3)
  1 2)

(deftest return-from-test.4
  (let ((a 0))
    (dotimes (i 10) (incf a) (when (oddp i) (return)))
    a)
  2)

(defun return-from-temp (x)
  (if x
    (return-from return-from-temp 'dummy))
  44)

(deftest return-from-test.5
  (return-from-temp nil)
  44)

(deftest return-from-test.6
  (return-from-temp t)
  dummy)

(deftest return-from-test.7
  (block out
    (flet ((exit (n) (return-from out n)))
      (block out (exit 1)))
    2)
  1)

(deftest return-from-test.8
  (block nil
    (unwind-protect (return-from nil 1)
      (return-from nil 2)))
  2)

(deftest return-from-test.9
  (let (list)
    (dolist (flag '(nil t))
      (block nil
        (let ((x 5))
          (declare (special x))
          (unwind-protect (return-from nil)
            (push x list))))
      (push 'here list))
    (nreverse list))
  (5 here 5 here))

(deftest return-from-test.10
  (let (list)
    (dolist (flag '(nil t))
      (block nil
        (let ((x 5))
          (declare (special x))
          (unwind-protect
            (if flag (return-from nil))
            (push x list))))
      (push 'here list))
    (nreverse list))
  (5 here 5 here))


;;
;;  Macro RETURN
;;
(deftest return.1
  (block nil
    (return)
    999)
  nil)

(deftest return.2
  (block nil
    (return 200)
    999)
  200)

(deftest return.3
  (block nil
    (return (values 10 20 30))
    999)
  10 20 30)

(deftest return.4
  (block nil
    (block aaa
      (return 10)
      20))
  10)

(deftest-error return-error.1
  (eval '(return 10)))

(deftest-error return-error.2
  (eval '(block nil (return 10 20))))

;;  ANSI Common Lisp
(deftest return-test.1
  (block nil
    (return)
    1)
  nil)

(deftest return-test.2
  (block nil
    (return 1)
    2)
  1)

(deftest return-test.3
  (block nil
    (return (values 1 2))
    3)
  1 2)

(deftest return-test.4
  (block nil
    (block alpha
      (return 1)
      2))
  1)

(deftest return-test.5
  (block alpha
    (block nil
      (return 1))
    2)
  2)

(deftest return-test.6
  (block nil
    (block nil
      (return 1)
      2))
  1)


;;
;;  Special Operator THROW
;;
(deftest throw.1
  (catch 'hello
    (throw 'hello 10)
    999)
  10)

(deftest throw.2
  (catch 10
    (throw 10 20)
    999)
  20)

(deftest-error throw.3
  (throw 'no-such-tag-name 20)
  control-error)

(deftest-error throw.4
  (catch 10
    (throw (lisp-system::make-fixnum 10) 20)
    999)
  control-error)

(deftest-error throw-error.1
  (eval '(throw 10)))

(deftest-error throw-error.2
  (eval '(throw 10 20 30)))

;;  ANSI Common Lisp
(deftest throw-test.1
  (catch 'result
    (setq i 0 j 0)
    (loop (incf j 3) (incf i)
          (if (= i 3) (throw 'result (values i j)))))
  3 9)

(deftest throw-test.2
  (catch nil
    (unwind-protect (throw nil 1)
      (throw nil 2)))
  2)

(deftest throw-test.3
  (catch 'a
    (catch 'b
      (unwind-protect (throw 'a 1)
        (throw 'b 2))))
  2)  ;; undefined

(deftest throw-test.4
  (catch 'foo
    (format nil "The inner catch returns ~s."
            (catch 'foo
              (unwind-protect (throw 'foo :first-throw)
                (throw 'foo :second-throw))))
    :outer-catch)
  :outer-catch)

(deftest throw-test.5
  (let (x)
    (catch 'foo
      (setq x (format nil "The inner catch returns ~s."
                      (catch 'foo
                        (unwind-protect (throw 'foo :first-throw)
                          (throw 'foo :second-throw)))))
      :outer-catch)
    x)
  "The inner catch returns :SECOND-THROW.")


;;
;;  Special Operator GO
;;
(deftest go.1
  (let (x)
    (tagbody
      (go 10)
      (setq x 20)
      10)
    x)
  nil)

(deftest go.2
  (let (x)
    (tagbody
      (go 10)
      (setq x 20)
      #.(lisp-system:make-fixnum 10))
    x)
  nil)

(deftest-error go-error.1
  (eval '(tagbody (go))))

(deftest-error go-error.2
  (eval '(tagbody (go 10 20) 10)))

;;  ANSI Common Lisp
(deftest go-test.1
  (values
    (tagbody
      (setq val 2)
      (go lp)
      (incf val 3)
      lp (incf val 4))
    val)
  nil 6)

(deftest-error go-test.2
  (let ((a nil))
    (tagbody t (setq a #'(lambda () (go t))))
    (funcall a)))


;;
;;  Special Operator UNWIND-PROTECT
;;
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

(deftest-error unwind-protect.10
  (eval '(unwind-protect)))

;;  ANSI Common Lisp
(defvar *unwind-protect-state*)

(defun unwind-protect-test-dummy-function (x)
  (setq *unwind-protect-state* 'running)
  (unless (numberp x)
    (throw 'abort 'not-a-number))
  (setq *unwind-protect-state* (1+ x)))

(deftest unwind-protect-test.1
  (catch 'abort (unwind-protect-test-dummy-function 1))
  2)

(deftest unwind-protect-test.2
  *unwind-protect-state*
  2)

(deftest unwind-protect-test.3
  (catch 'abort (unwind-protect-test-dummy-function 'trash))
  not-a-number)

(deftest unwind-protect-test.4
  *unwind-protect-state*
  running)

(deftest unwind-protect-test.5
  (catch 'abort
    (unwind-protect
      (unwind-protect-test-dummy-function 'trash)
      (setq *unwind-protect-state* 'aborted)))
  not-a-number)

(deftest unwind-protect-test.6
  *unwind-protect-state*
  aborted)

(deftest unwind-protect-test.7
  (block nil
    (unwind-protect (return 1)
      (return 2)))
  2)

(deftest unwind-protect-test.8
  (catch nil
    (unwind-protect (throw nil 1)
      (throw nil 2)))
  2)

(deftest unwind-protect-test.9
  (catch 'foo
    (format nil "The inner catch returns ~s."
            (catch 'foo
              (unwind-protect (throw 'foo :first-throw)
                (throw 'foo :second-throw))))
    :outer-catch)
  :outer-catch)

(deftest unwind-protect-test.10
  (let (x)
    (catch 'foo
      (setq x (format nil "The inner catch returns ~s."
                      (catch 'foo
                        (unwind-protect (throw 'foo :first-throw)
                          (throw 'foo :second-throw)))))
      :outer-catch)
    x)
  "The inner catch returns :SECOND-THROW.")

(deftest unwind-protect-test.11
  (catch 'a
    (catch 'b
      (unwind-protect (1+ (catch 'a (throw 'b 1)))
        (throw 'a 10))))
  10)

(deftest unwind-protect-test.12
  (let (x)
    (catch 'bar
      (catch 'foo
        (unwind-protect (throw 'foo 3)
          (throw 'bar 4)
          (setq x 'xxx)))))
  4)

(deftest unwind-protect-test.13
  (let (x)
    (catch 'bar
      (catch 'foo
        (unwind-protect (throw 'foo 3)
          (throw 'bar 4)
          (setq x 'xxx))))
    x)
  nil)

(deftest unwind-protect-test.14
  (let (y)
    (block nil
      (let ((x 5))
        (declare (special x))
        (unwind-protect (return)
          (setq y x))))
    y)
  5)

