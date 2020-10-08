;;
;;  ANSI COMMON LISP: 10. Symbols
;;

;;
;;  Accessor SYMBOL-FUNCTION
;;
(deftest symbol-function.1
  (symbol-function 'car)
  #.#'car)

(deftest symbol-function.2
  (progn
    (setf (symbol-function 'hello-symbol-function) (lambda () :testcase))
    (values
      (functionp
        (symbol-function 'hello-symbol-function))
      (funcall (symbol-function 'hello-symbol-function))))
  t :testcase)

(deftest-error symbol-function.3
  (symbol-function 'no-such-symbol-function)
  undefined-function)

(deftest-error symbol-function.4
  (flet ((no-such-symbol-function () :hello))
    (no-such-symbol-function)
    (symbol-function 'no-such-symbol-function))
  undefined-function)

(deftest-error symbol-function-error.1
  (eval '(symbol-function 10))
  type-error)

(deftest-error! symbol-function-error.2
  (eval '(symbol-function)))

(deftest-error! symbol-function-error.3
  (eval '(symbol-function 'hello-symbol-function 10)))

(deftest-error symbol-function-error.4
  (eval '(setf (symbol-function 10) (lambda ()))))

(deftest-error! symbol-function-error.5
  (eval '(setf (symbol-function) (lambda ()))))

(deftest-error! symbol-function-error.6
  (eval '(setf (symbol-function 'hello-symbol-function 20) (lambda ()))))

(deftest-error! symbol-function-error.7
  (eval '(setf (symbol-function nil) (lambda ()))))

(deftest-error! symbol-function-error.8
  (eval '(setf (symbol-function t) (lambda ()))))

(deftest-error! symbol-function-error.9
  (eval '(setf (symbol-function :hello) (lambda ()))))

(deftest-error! symbol-function-error.10
  (eval '(setf (symbol-function '(setf hello)) (lambda ()))))

;;  ANSI Common Lisp
(deftest symbol-funciton-test.1
  (symbol-function 'car)
  #.#'car)

(deftest-error symbol-funciton-test.2
  (symbol-function 'symbol-function-twice))

(deftest symbol-funciton-test.3
  (defun symbol-function-twice (n) (* n 2))
  symbol-function-twice)

(deftest symbol-funciton-test.4
  (functionp
    (symbol-function 'symbol-function-twice))
  t)

(deftest symbol-funciton-test.5
  (list (symbol-function-twice 3)
        (funcall (function symbol-function-twice) 3)
        (funcall (symbol-function 'symbol-function-twice) 3))
  (6 6 6))

(deftest symbol-funciton-test.6
  (flet ((symbol-function-twice (x) (list x x)))
    (list (symbol-function-twice 3)
          (funcall (function symbol-function-twice) 3)
          (funcall (symbol-function 'symbol-function-twice) 3)))
  ((3 3) (3 3) 6))

(deftest symbol-funciton-test.7
  (progn
    (setf (symbol-function 'symbol-function-twice) #'(lambda (x) (list x x)))
    (list (symbol-function-twice 3)
          (funcall (function symbol-function-twice) 3)
          (funcall (symbol-function 'symbol-function-twice) 3)))
  ((3 3) (3 3) (3 3)))

(deftest symbol-funciton-test.8
  (fboundp 'defun)
  t)

(deftest-error symbol-funciton-test.9
  (symbol-function 'defun)
  undefined-function)

(deftest-error symbol-funciton-test.10
  (functionp (symbol-function 'defun))
  undefined-function)

(defun symbol-function-or-nil (symbol)
  (if (and (fboundp symbol)
           (not (macro-function symbol))
           (not (special-operator-p symbol)))
    (symbol-function symbol)
    nil))

(deftest symbol-funciton-test.11
  (functionp
    (symbol-function-or-nil 'car))
  t)

(deftest symbol-funciton-test.12
  (symbol-function-or-nil 'defun)
  nil)


;;
;;  Function SYMBOL-NAME
;;
(deftest symbol-name.1
  (symbol-name nil)
  "NIL")

(deftest symbol-name.2
  (symbol-name T)
  "T")

(deftest symbol-name.3
  (symbol-name 'hello)
  "HELLO")

(deftest symbol-name.4
  (symbol-name :test)
  "TEST")

(deftest symbol-name.5
  (let ((*gensym-counter* 999))
    (symbol-name (gensym)))
  "G999")

(deftest-error symbol-name-error.1
  (eval '(symbol-name 10))
  type-error)

(deftest-error! symbol-name-error.2
  (eval '(symbol-name)))

(deftest-error! symbol-name-error.3
  (eval '(symbol-name 'hello 'hello)))


;;
;;  Function SYMBOL-PACKAGE
;;
(deftest symbol-package.1
  (package-name
    (symbol-package nil))
  "COMMON-LISP")

(deftest symbol-package.2
  (package-name
    (symbol-package t))
  "COMMON-LISP")

(deftest symbol-package.3
  (package-name
    (symbol-package :hello))
  "KEYWORD")

(deftest symbol-package.4
  (symbol-package (gensym))
  nil)

(deftest symbol-package.5
  (package-name
    (symbol-package 'hello))
  "COMMON-LISP-USER")

(deftest-error symbol-package-error.1
  (eval '(symbol-package 10))
  type-error)

(deftest-error! symbol-package-error.2
  (eval '(symbol-package)))

(deftest-error! symbol-package-error.3
  (eval '(symbol-package 'hello 'hello)))

;;  ANSI Common Lisp
(deftest symbol-package-test.1
  (package-name
    (in-package "CL-USER"))
  "COMMON-LISP-USER")

(deftest symbol-package-test.2
  (package-name
    (symbol-package 'car))
  "COMMON-LISP")

(deftest symbol-package-test.3
  (package-name
    (symbol-package 'bus))
  "COMMON-LISP-USER")

(deftest symbol-package-test.4
  (package-name
    (symbol-package :optional))
  "KEYWORD")

(deftest symbol-package-test.5
  (symbol-package (gensym))
  nil)

(deftest symbol-package-test.6
  (package-name
    (make-package 'symbol-package-pk1))
  "SYMBOL-PACKAGE-PK1")

(deftest symbol-package-test.7
  (let ((x (intern "SAMPLE1" "SYMBOL-PACKAGE-PK1")))
    (values (package-name (symbol-package x))
            (symbol-name x)))
  "SYMBOL-PACKAGE-PK1"
  "SAMPLE1")

(deftest symbol-package-test.8
  (export (find-symbol "SAMPLE1" "SYMBOL-PACKAGE-PK1") "SYMBOL-PACKAGE-PK1")
  t)

(deftest symbol-package-test.9
  (package-name
    (make-package 'symbol-package-pk2 :use '(symbol-package-pk1)))
  "SYMBOL-PACKAGE-PK2")

(deftest symbol-package-test.10
  (multiple-value-bind (x y)
    (find-symbol "SAMPLE1" "SYMBOL-PACKAGE-PK2")
    (values (package-name (symbol-package x))
            (symbol-name x)
            y))
  "SYMBOL-PACKAGE-PK1"
  "SAMPLE1"
  :inherited)

(deftest symbol-package-test.11
  (package-name
    (symbol-package
      (intern "SAMPLE1" "SYMBOL-PACKAGE-PK1")))
  "SYMBOL-PACKAGE-PK1")

(deftest symbol-package-test.12
  (package-name
    (symbol-package
      (intern "SAMPLE1" "SYMBOL-PACKAGE-PK2")))
  "SYMBOL-PACKAGE-PK1")

(deftest symbol-package-test.13
  (package-name
    (symbol-package
      (intern "SAMPLE2" "SYMBOL-PACKAGE-PK1")))
  "SYMBOL-PACKAGE-PK1")

(deftest symbol-package-test.14
  (package-name
    (symbol-package
      (intern "SAMPLE2" "SYMBOL-PACKAGE-PK2")))
  "SYMBOL-PACKAGE-PK2")

(deftest symbol-package-test.15
  (let ((s3 (intern "SAMPLE3" "SYMBOL-PACKAGE-PK1"))
        )
    (import s3 'symbol-package-pk2)
    (unintern s3 'symbol-package-pk1)
    (values
      (symbol-package s3)
      (eq s3 (intern "SAMPLE3" "SYMBOL-PACKAGE-PK2"))))
  nil t)


;;
;;  Accessor SYMBOL-PLIST
;;
(deftest symbol-plist.1
  (symbol-plist (gensym))
  nil)

(deftest symbol-plist.2
  (let ((x (gensym)))
    (setf (get x 'prop) 'val)
    (symbol-plist x))
  (prop val))

(deftest symbol-plist.3
  (let ((x (gensym)))
    (setf (get x 'prop1) 'val1)
    (setf (get x 'prop2) 'val2)
    (symbol-plist x))
  (prop2 val2 prop1 val1))

(deftest symbol-plist.4
  (let ((x 'hello-plist))
    (setf (get x 'prop1) 'val1)
    (setf (get x 'prop2) 'val2)
    (setf (symbol-plist x) (list 'prop3 'val3)))
  (prop3 val3))

(deftest-error symbol-plist-error.1
  (eval '(symbol-plist 10))
  type-error)

(deftest-error! symbol-plist-error.2
  (eval '(symbol-plist)))

(deftest-error! symbol-plist-error.3
  (eval '(symbol-plist 'hello 20)))

(deftest-error symbol-plist-error.4
  (eval '(setf (symbol-plist 10) nil))
  type-error)

(deftest-error! symbol-plist-error.5
  (eval '(setf (symbol-plist) nil)))

(deftest-error! symbol-plist-error.6
  (eval '(setf (symbol-plist 'hello 20) nil)))

(deftest-error symbol-plist-error.7
  (eval '(setf (symbol-plist 'hello) 30)))


;;
;;  Accessor SYMBOL-VALUE
;;
(deftest symbol-value.1
  (values
    (symbol-value nil)
    (symbol-value 'nil)
    (symbol-value ())
    (symbol-value '()))
  nil nil nil nil)

(deftest symbol-value.2
  (symbol-value t)
  t)

(deftest symbol-value.3
  (symbol-value :hello)
  :hello)

(deftest symbol-value.4
  (floatp
    (symbol-value 'pi))
  t)

(deftest symbol-value.5
  (progn
    (setq symbol-value-lexical 100)
    (symbol-value 'symbol-value-lexical))
  100)

(deftest symbol-value.6
  (progn
    (setf (symbol-value 'symbol-value-lexical) 200)
    (symbol-value 'symbol-value-lexical))
  200)

(deftest symbol-value.7
  (let ((symbol-value-special 200))
    (declare (special symbol-value-special))
    (symbol-value 'symbol-value-special))
  200)

(deftest symbol-value.8
  (let (symbol-value-special)
    (declare (special symbol-value-special))
    (setf (symbol-value 'symbol-value-special) 300)
    (symbol-value 'symbol-value-special))
  300)

(defvar symbol-value-shadow 100)
(deftest symbol-value.9
  (values
    (symbol-value 'symbol-value-shadow)
    (let ((symbol-value-shadow 200))
      (declare (special symbol-value-shadow))
      (symbol-value 'symbol-value-shadow)))
  100 200)

(deftest-error symbol-value.10
  (symbol-value 'no-such-symvol-value-name)
  unbound-variable)

(deftest-error symbol-value-error.1
  (eval '(symbol-value 10))
  type-error)

(deftest-error! symbol-value-error.2
  (eval '(symbol-value)))

(deftest-error! symbol-value-error.3
  (eval '(symbol-value 'hello 20)))

(deftest-error symbol-value-error.4
  (eval '(setf (symbol-value 10) nil))
  type-error)

(deftest-error! symbol-value-error.5
  (eval '(setf (symbol-value) nil)))

(deftest-error! symbol-value-error.6
  (eval '(setf (symbol-value 'hello 20) nil)))

(deftest-error symbol-value-error.7
  (setf (symbol-value nil) 10))

(deftest-error symbol-value-error.8
  (setf (symbol-value t) 10))

(deftest-error symbol-value-error.9
  (setf (symbol-value :hello) 10))

(deftest-error symbol-value-error.10
  (setf (symbol-value 'pi) 10))

;;  ANSI Common Lisp
(deftest symbol-value-test.1
  (setf (symbol-value 'symbol-value-test) 1)
  1)

(deftest symbol-value-test.2
  (symbol-value 'symbol-value-test)
  1)

(deftest symbol-value-test.3
  (let ((symbol-value-test 2))
    (declare (ignorable symbol-value-test))
    (symbol-value 'symbol-value-test))
  1)

(deftest symbol-value-test.4
  (let ((symbol-value-test 2))
    (setq symbol-value-test 3)
    (symbol-value 'symbol-value-test))
  1)

(deftest symbol-value-test.5
  (let ((symbol-value-test 2))
    (declare (special symbol-value-test))
    (symbol-value 'symbol-value-test))
  2)

(deftest symbol-value-test.6
  (let ((symbol-value-test 2))
    (declare (special symbol-value-test))
    (setq symbol-value-test 3)
    (symbol-value 'symbol-value-test))
  3)

(deftest symbol-value-test.7
  (let ((symbol-value-test 2))
    (setf (symbol-value 'symbol-value-test) 3)
    symbol-value-test)
  2)

(deftest symbol-value-test.8
  symbol-value-test
  3)

(deftest symbol-value-test.9
  (symbol-value 'symbol-value-test)
  3)

(deftest symbol-value-test.10
  (let ((symbol-value-test 4))
    (declare (special symbol-value-test))
    (let ((b (symbol-value 'symbol-value-test)))
      (setf (symbol-value 'symbol-value-test) 5)
      (values symbol-value-test b)))
  5 4)

(deftest symbol-value-test.11
  symbol-value-test
  3)

(deftest symbol-value-test.12
  (symbol-value :any-keyword)
  :any-keyword)

(deftest symbol-value-test.13
  (symbol-value 'nil)
  nil)

(deftest symbol-value-test.14
  (symbol-value '())
  nil)

(deftest symbol-value-test.15
  (<= 3 (symbol-value 'pi) 4)
  t)


;;
;;  Accessor GET
;;
(deftest get.1
  (get (gensym) 'hello)
  nil)

(deftest get.2
  (let ((x (gensym)))
    (setf (symbol-plist x) (list 'a 'b 'c 'd 'e 'f 'g 'h))
    (get x 'c))
  d)

(deftest get.3
  (let ((x (gensym)))
    (setf (symbol-plist x) (list 'a 'b 'c 'd 'e 'f 'g 'h))
    (setf (get x 'c) :hello)
    (symbol-plist x))
  (a b c :hello e f g h))

(deftest get.4
  (let ((x (gensym)))
    (setf (get x 'a) 'b)
    (symbol-plist x))
  (a b))

(deftest get.5
  (let ((x (gensym)))
    (values
      (get x 'a 'default-value)
      (symbol-plist x)))
  default-value nil)

(deftest get.6
  (let ((x (gensym)))
    (setf (get x 'a 10) 'b)
    (symbol-plist x))
  (a b))

(deftest get.7
  (let ((x (gensym)))
    (incf (get x 'a 20))
    (symbol-plist x))
  (a 21))


;;  ANSI Common Lisp
(defun get-make-person (first-name last-name)
  (let ((person (gensym "PERSON")))
    (setf (get person 'first-name) first-name)
    (setf (get person 'last-name) last-name)
    person))

(defvar *get-john* (get-make-person "John" "Dow"))

(deftest get-test.1
  (symbolp *get-john*)
  t)

(defvar *get-sally* (get-make-person "Sally" "Jones"))

(deftest get-test.2
  (get *get-john* 'first-name)
  "John")

(deftest get-test.3
  (get *get-sally* 'last-name)
  "Jones")

(defun get-marry (man woman married-name)
  (setf (get man 'wife) woman)
  (setf (get woman 'husband) man)
  (setf (get man 'last-name) married-name)
  (setf (get woman 'last-name) married-name)
  married-name)

(deftest get-test.4
  (get-marry *get-john* *get-sally* "Dow-Jones")
  "Dow-Jones")

(deftest get-test.5
  (get *get-john* 'last-name)
  "Dow-Jones")

(deftest get-test.6
  (get (get *get-john* 'wife) 'first-name)
  "Sally")

(deftest get-test.7
  ;; (WIFE #:PERSON4604 LAST-NAME "Dow-Jones" FIRST-NAME "John"))
  (length
    (symbol-plist *get-john*))
  6)

(defmacro get-age (person &optional (default ''thirty-something))
  `(get ,person 'get-age ,default))

(deftest get-test.8
  (get-age *get-john*)
  thirty-something)

(deftest get-test.9
  (get-age *get-john* 20)
  20)

(deftest get-test.10
  (setf (get-age *get-john*) 25)
  25)

(deftest get-test.11
  (get-age *get-john*)
  25)

(deftest get-test.12
  (get-age *get-john* 20)
  25)


;;
;;  Function REMPROP
;;
(deftest remprop.1
  (remprop (gensym) 'key)
  nil)

(deftest remprop.2
  (let ((x (gensym)))
    (setf (symbol-plist x) (list 'a 'b))
    (values
      (remprop x 'a)
      (symbol-plist x)))
  t nil)

(deftest remprop.3
  (let ((x (gensym)))
    (setf (symbol-plist x) (list 'a 'b))
    (values
      (remprop x 'zzz)
      (symbol-plist x)))
  nil (a b))

(deftest remprop.4
  (let ((x (gensym)))
    (setf (symbol-plist x) (list 'a 'b 'c 'd 'e 'f))
    (values
      (remprop x 'a)
      (symbol-plist x)))
  t (c d e f))

(deftest remprop.5
  (let ((x (gensym)))
    (setf (symbol-plist x) (list 'a 'b 'c 'd 'e 'f))
    (values
      (remprop x 'c)
      (symbol-plist x)))
  t (a b e f))

(deftest remprop.6
  (let ((x (gensym)))
    (setf (symbol-plist x) (list 'a 'b 'c 'd 'e 'f))
    (values
      (remprop x 'e)
      (symbol-plist x)))
  t (a b c d))

(deftest remprop.7
  (let ((x (gensym)))
    (setf (symbol-plist x) (list 'a 'b 'c 'd 'e 'f))
    (values
      (remprop x 'b)
      (symbol-plist x)))
  nil (a b c d e f))

(deftest-error remprop-error.1
  (eval '(remprop 10 20))
  type-error)

(deftest-error! remprop-error.2
  (eval '(remprop)))

(deftest-error! remprop-error.3
  (eval '(remprop 'hello 20 30)))


;;  ANSI Common Lisp
(deftest remprop-test.1
  (progn
    (setq *remprop-test* (make-symbol "PSEUDO-PI"))
    (symbol-plist *remprop-test*))
  ())

(deftest remprop-test.2
  (setf (get *remprop-test* 'constant) t)
  t)

(deftest remprop-test.3
  (setf (get *remprop-test* 'approximation) 3.14)
  3.14)

(deftest remprop-test.4
  (setf (get *remprop-test* 'error-range) 'noticeable)
  noticeable)

(deftest remprop-test.5
  (symbol-plist *remprop-test*)
  (error-range noticeable approximation 3.14 constant t))

(deftest remprop-test.6
  (setf (get *remprop-test* 'approximation) nil)
  nil)

(deftest remprop-test.7
  (symbol-plist *remprop-test*)
  (error-range noticeable approximation nil constant t))

(deftest remprop-test.8
  (get *remprop-test* 'approximation)
  nil)

(deftest remprop-test.9
  (remprop *remprop-test* 'approximation)
  t)

(deftest remprop-test.10
  (get *remprop-test* 'approximation)
  nil)

(deftest remprop-test.11
  (symbol-plist *remprop-test*)
  (error-range noticeable constant t))

(deftest remprop-test.12
  (remprop *remprop-test* 'approximation)
  nil)

(deftest remprop-test.13
  (symbol-plist *remprop-test*)
  (error-range noticeable constant t))

(deftest remprop-test.14
  (remprop *remprop-test* 'error-range)
  t)

(deftest remprop-test.15
  (setf (get *remprop-test* 'approximation) 3)
  3)

(deftest remprop-test.16
  (symbol-plist *remprop-test*)
  (approximation 3 constant t))


;;
;;  Function SET
;;
(defvar set-value)
(deftest set.1
  (progn
    (set 'set-value 100)
    set-value)
  100)

(deftest set.2
  (let ((x (gensym)))
    (set x 200)
    (symbol-value x))
  200)

(deftest-error set-error.1
  (eval '(set 10 20))
  type-error)

(deftest-error! set-error.2
  (eval '(set 'hello)))

(deftest-error! set-error.3
  (eval '(set 'hello 10 20)))

;;  ANSI Common Lisp
(deftest set-test.1
  (progn
    (setf (symbol-value 'set-test-n) 1)
    (set 'set-test-n 2))
  2)

(deftest set-test.2
  (symbol-value 'set-test-n)
  2)

(deftest set-test.3
  (let ((set-test-n 3))
    (declare (special set-test-n))
    (setq set-test-n (+ set-test-n 1))
    (setf (symbol-value 'set-test-n) (* set-test-n 10))
    (set 'set-test-n (+ (symbol-value 'set-test-n) set-test-n))
    set-test-n)
  80)

(deftest set-test.4
  set-test-n
  2)

(deftest set-test.5
  (let ((set-test-n 3))
    (setq set-test-n (+ set-test-n 1))
    (setf (symbol-value 'set-test-n) (* set-test-n 10))
    (set 'set-test-n (+ (symbol-value 'set-test-n) set-test-n))
    set-test-n)
  4)

(deftest set-test.6
  set-test-n
  44)

(defvar *set-test-n* 2)
(deftest set-test.7
  (let ((*set-test-n* 3))
    (setq *set-test-n* (+ *set-test-n* 1))
    (setf (symbol-value '*set-test-n*) (* *set-test-n* 10))
    (set '*set-test-n* (+ (symbol-value '*set-test-n*) *set-test-n*))
    *set-test-n*)
  80)

(deftest set-test.8
  *set-test-n*
  2)

(defvar *set-even-count* 0)
(defvar *set-odd-count* 0)
(defun set-tally-list (list)
  (dolist (element list)
    (set (if (evenp element) '*set-even-count* '*set-odd-count*)
         (+ element (if (evenp element) *set-even-count* *set-odd-count*)))))

(deftest set-test.9
  (set-tally-list '(1 9 4 3 2 7))
  nil)

(deftest set-test.10
  *set-even-count*
  6)

(deftest set-test.11
  *set-odd-count*
  20)

