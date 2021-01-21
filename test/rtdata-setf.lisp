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
;;  Function GET-SETF-EXPANSION
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

(deftest get-setf-expansion.3
  (let ((x (cons 10 20)))
    (declare (special x) (ignorable x))
    (multiple-value-bind (a b g w r) (get-setf-expansion '(cdr x) nil)
      (eval `(let (,@(mapcar #'list a b) ,@g)
               (declare (ignorable ,@a ,@g))
               (setq ,(car g) 999)
               ,w
               ,r))
      x))
  (10 . 999))

(deftest-error! get-setf-expansion-error.1
  (eval '(get-setf-expansion)))

(deftest-error! get-setf-expansion-error.2
  (eval '(get-setf-expansion '(car x) nil nil)))


;;
;;  Macro DEFINE-SETF-EXPANDER
;;
(deftest define-setf-expander.1
  (define-setf-expander setf-expander-1 ())
  setf-expander-1)

(deftest define-setf-expander.2
  (progn
    (define-setf-expander setf-expander-2 (x)
      (values nil nil nil nil x))
    (get-setf-expansion '(setf-expander-2 10)))
  nil nil nil nil 10)

(deftest define-setf-expander.3
  (progn
    (define-setf-expander setf-expander-3 (&environment env x y z)
      (values (null env) x y z nil))
    (get-setf-expansion '(setf-expander-3 10 20 30)))
  t 10 20 30 nil)

(deftest define-setf-expander.4
  (define-setf-expander setf-expander-4 (&environment env x y z)
    (declare (ignore env x y z))
    (values nil nil nil nil nil))
  setf-expander-4)

(deftest define-setf-expander.5
  (progn
    (define-setf-expander setf-expander-5 ()
      "HelloSetf"
      nil)
    (documentation 'setf-expander-5 'setf))
  "HelloSetf")

(deftest-error define-setf-expander-error.1
  (eval '(define-setf-expander-error name)))

(deftest-error define-setf-expander-error.2
  (eval '(define-setf-expander-error name 10)))

;;  ANSI Common Lisp
(defun define-setf-expander-test-lastguy (x)
  (car (last x)))

(define-setf-expander define-setf-expander-test-lastguy (x &environment env)
  "Set the last element in a list to the given value."
  (multiple-value-bind (dummies vals newval setter getter)
    (get-setf-expansion x env)
    (declare (ignorable newval setter))
    (let ((store (gensym)))
      (values dummies
              vals
              `(,store)
              `(progn (rplaca (last ,getter) ,store) ,store)
              `(define-setf-expander-test-lastguy ,getter)))))

(deftest define-setf-expander-test.1
  (let ((a (list 'a 'b 'c 'd))
        (b (list 'x))
        (c (list 1 2 3 (list 4 5 6))))
    (values
      (setf (define-setf-expander-test-lastguy a) 3)
      (setf (define-setf-expander-test-lastguy b) 7)
      (setf (define-setf-expander-test-lastguy
              (define-setf-expander-test-lastguy c)) 'lastguy-symbol)
      a b c))
  3 7 lastguy-symbol
  (a b c 3)
  (7)
  (1 2 3 (4 5 lastguy-symbol)))


;;
;;  Macro DEFSETF
;;
;;  short-form
(deftest defsetf-short.1
  (defsetf defsetf-short-1 set)
  defsetf-short-1)

(deftest defsetf-short.2
  (progn
    (defsetf defsetf-short-2 set "HelloShort")
    (documentation 'defsetf-short-2 'setf))
  "HelloShort")

(defsetf defsetf-short-3 set)
(deftest defsetf-short.3
  (let (x)
    (declare (special x) (ignorable x))
    (list-length
      (multiple-value-list
        (get-setf-expansion '(defsetf-short-2 x)))))
  5)

(defsetf defsetf-short-4 set)
(deftest defsetf-short.4
  (let (x)
    (declare (special x) (ignorable x))
    (setf (defsetf-short-4 'x) 10))
  10)

(defsetf defsetf-short-5 set)
(deftest defsetf-short.5
  (let (x)
    (declare (special x) (ignorable x))
    (setf (defsetf-short-5 'x) 10)
    x)
  10)

(deftest-error defsetf-short-error.1
  (eval '(defsetf name)))

(deftest-error defsetf-short-error.2
  (eval '(defsetf name set "Hello" nil)))


;;  long-form
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

(deftest defsetf-long.4
  (progn
    (defsetf defsetf-long-4 (x) (g)
      (declare (ignorable x))
      `(set ,x ,g)))
  defsetf-long-4)

(deftest defsetf-long.5
  (progn
    (defsetf defsetf-long-5 (x) (g)
      "HelloLong"
      `(set ,x ,g))
    (documentation 'defsetf-long-5 'setf))
  "HelloLong")

(defsetf defsetf-long-6 (x) (g)
  (when (symbol-package x)
    (error "not-gensym"))
  (when (symbol-package g)
    (error "not-gensym"))
  `(set ,x ,g))

(deftest defsetf-long.6
  (let ((x 10))
    (declare (special x) (ignorable x))
    (setf (defsetf-long-6 'x) 100)
    x)
  100)

(deftest-error defsetf-long-error.1
  (eval '(defsetf name (x))))

(deftest-error defsetf-long-error.2
  (eval '(defsetf name (x) 10)))

;;  ANSI Common Lisp
(defun defsetf-short-middleguy (x)
  (nth (truncate (1- (list-length x)) 2) x))

(defun defsetf-short-set-middleguy (x v)
  (unless (null x)
    (rplaca (nthcdr (truncate (1- (list-length x)) 2) x) v))
  v)

(defsetf defsetf-short-middleguy defsetf-short-set-middleguy)

(deftest defsetf-test.1
  (let ((a (list 'a 'b 'c 'd))
        (b (list 'x))
        (c (list 1 2 3 (list 4 5 6) 7 8 9)))
    (values
      (setf (defsetf-short-middleguy a) 3)
      (setf (defsetf-short-middleguy b) 7)
      (setf (defsetf-short-middleguy
              (defsetf-short-middleguy c)) 'middleguy-symbol)
      a b c))
  3 7 middleguy-symbol
  (a 3 c d)
  (7)
  (1 2 3 (4 middleguy-symbol 6) 7 8 9))

(defvar *defsetf-test-xy* (make-array '(10 10)))

(defun defsetf-test-xy (&key ((x x) 0) ((y y) 0))
  (aref *defsetf-test-xy* x y))

(defun defsetf-set-xy (new-value &key ((x x) 0) ((y y) 0))
  (setf (aref *defsetf-test-xy* x y) new-value))

(defsetf defsetf-test-xy (&key ((x x) 0) ((y y) 0)) (store)
  `(defsetf-set-xy ,store 'x ,x 'y ,y))

(deftest defsetf-test.2
  (defsetf-test-xy 'x 1)
  nil)

(deftest defsetf-test.3
  (setf (defsetf-test-xy 'x 1) 1)
  1)

(deftest defsetf-test.4
  (defsetf-test-xy 'x 1)
  1)

(deftest defsetf-test.5
  (let ((a 'x) (b 'y))
    (setf (defsetf-test-xy a 1 b 2) 3)
    (setf (defsetf-test-xy b 5 a 9) 14))
  14)

(deftest defsetf-test.6
  (defsetf-test-xy 'y 0 'x 1)
  1)

(deftest defsetf-test.7
  (defsetf-test-xy 'x 1 'y 2)
  3)


;;
;;  Macro SETF
;;
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

(deftest setf.9
  (let ((a 10))
    (declare (special a))
    (setf a 20)
    (symbol-value 'a))
  20)

(deftest setf.10
  (let ((x (list 10 20 30)))
    (symbol-macrolet
      ((y (car x)) (z (cadr x)))
      (setf y (1+ z) z (1+ y))
      (list x y z)))
  ((21 22 30) 21 22))

(deftest setf.11
  (progn
    (defclass setf-test-class () (a))
    (let ((inst (make-instance 'setf-test-class)))
      (with-slots (a) inst
        (setf a 100))
      (slot-value inst 'a)))
  100)

(deftest-error setf-error.1
  (eval '(setf a)))

(deftest-error setf-error.2
  (eval '(setf 10 20)))

(defconstant setf-error-variable 10)
(deftest-error setf-error.3
  (eval '(setf setf-error-variable 20)))


;;
;;  Macro PSETF
;;
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
  (let ((a 10) b)
    (psetf a 20 b a)
    (values a b))
  20 10)

(deftest psetf.5
  (let (x y)
    (values (psetf x 10 y 20) x y))
  nil 10 20)

(deftest psetf.6
  (let ((x '(10 . 20)))
    (values (psetf (car x) 30) x))
  nil (30 . 20))

(deftest psetf.7
  (let ((x 10)
        (y 20))
    (psetf x 30 y x)
    (values x y))
  30 10)

(deftest psetf.8
  (let ((a (list 10 20 30)))
    (symbol-macrolet
      ((b (car a)))
      (psetf b 999)
      a))
  (999 20 30))

(deftest psetf.9
  (let ((x (list 10 20 30)))
    (symbol-macrolet
      ((y (car x)) (z (cadr x)))
      (psetf y (1+ z) z (1+ y))
      (list x y z)))
  ((21 11 30) 21 11))

(deftest psetf.10

  (let ((a 1) (b 2))
    (psetf a b  b a)
    (values a b))
  2 1)

(deftest psetf.11
  (progn
    (defclass psetq-test-class () (a))
    (let ((inst (make-instance 'psetq-test-class)))
      (with-slots (a) inst
        (psetf a 100))
      (slot-value inst 'a)))
  100)

(deftest-error psetf-error.1
  (eval '(psetf a)))

(deftest-error psetf-error.2
  (eval '(psetf 10 20)))

(defconstant psetf-error-variable 10)
(deftest-error psetf-error.3
  (eval '(psetf psetf-error-variable 20)))

;;  ANSI Common Lisp
(defvar setf-test-x)
(defvar setf-test-y)

(deftest setf-test.1
  (setq setf-test-x (cons 'a 'b) setf-test-y (list 1 2 3))
  (1 2 3))

(deftest setf-test.2
  (setf (car setf-test-x) 'setf-test-x
        (cadr setf-test-y) (car setf-test-x)
        (cdr setf-test-x) setf-test-y)
  (1 setf-test-x 3))

(deftest setf-test.3
  setf-test-x
  (setf-test-x 1 setf-test-x 3))

(deftest setf-test.4
  setf-test-y
  (1 setf-test-x 3))

(deftest setf-test.5
  (setq setf-test-x (cons 'a 'b)
        setf-test-y (list 1 2 3))
  (1 2 3))

(deftest setf-test.6
  (psetf (car setf-test-x) 'setf-test-x
         (cadr setf-test-y) (car setf-test-x)
         (cdr setf-test-x) setf-test-y)
  nil)

(deftest setf-test.7
  setf-test-x
  (setf-test-x 1 a 3))

(deftest setf-test.8
  setf-test-y
  (1 a 3))


;;
;;  Macro SHIFTF
;;
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

(deftest-error shiftf-error.1
  (eval '(let (x) (shiftf x))))

(deftest-error shiftf-error.2
  (eval '(shiftf 10 20)))

;;  ANSI Common Lisp
(defvar shiftf-test-x)
(defvar shiftf-test-y)
(defvar shiftf-test-n)

(deftest shiftf-test.1
  (setq shiftf-test-x (list 1 2 3)
        shiftf-test-y 'trash)
  trash)

(deftest shiftf-test.2
  (shiftf shiftf-test-y shiftf-test-x
          (cdr shiftf-test-x) '(hi there))
  trash)

(deftest shiftf-test.3
  shiftf-test-x
  (2 3))

(deftest shiftf-test.4
  shiftf-test-y
  (1 hi there))

(deftest shiftf-test.5
  (setq shiftf-test-x (list 'a 'b 'c))
  (a b c))

(deftest shiftf-test.6
  (shiftf (cadr shiftf-test-x) 'z)
  b)

(deftest shiftf-test.7
  shiftf-test-x
  (a z c))

(deftest shiftf-test.8
  (shiftf (cadr shiftf-test-x) (cddr shiftf-test-x) 'q)
  z)

(deftest shiftf-test.9
  shiftf-test-x
  (a (c) . q))

(deftest shiftf-test.10
  (setq shiftf-test-n 0)
  0)

(deftest shiftf-test.11
  (setq shiftf-test-x (list 'a 'b 'c 'd))
  (a b c d))

(deftest shiftf-test.12
  (shiftf (nth (setq shiftf-test-n (+ shiftf-test-n 1)) shiftf-test-x) 'z)
  b)

(deftest shiftf-test.13
  shiftf-test-x
  (a z c d))


;;
;;  Macro ROTATEF
;;
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

;;  ANSI Common Lisp
(deftest rotatef-test.1
  (let ((n 0)
        (x (list 'a 'b 'c 'd 'e 'f 'g)))
    (rotatef (nth (incf n) x)
             (nth (incf n) x)
             (nth (incf n) x))
    x)
  (a c d b e f g))

