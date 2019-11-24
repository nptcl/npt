;;
;;  ANSI COMMON LISP: 10. Symbols
;;
(deftest symbolp.1
  (symbolp 10)
  nil)

(deftest symbolp.2
  (symbolp 'hello)
  t)

(deftest symbolp.3
  (symbolp :test)
  t)

(deftest symbolp.4
  (symbolp '#:hello-gensym)
  t)

(deftest keywordp.1
  (keywordp 10)
  nil)

(deftest keywordp.2
  (keywordp 'hello)
  nil)

(deftest keywordp.3
  (keywordp :test)
  t)

(deftest keywordp.4
  (keywordp '#:hello-gensym)
  nil)

(deftest make-symbol.1
  (let ((x (make-symbol "Hello")))
    (values
      (symbol-name x)
      (symbol-package x)))
  "Hello" nil)

(deftest make-symbol.2
  (eq (make-symbol "aaa")
      (make-symbol "aaa"))
  nil)

(deftest copy-symbol.1
  (let ((x (copy-symbol 'hello)))
    (values
      (symbol-name x)
      (symbol-package x)
      (eq x 'hello)))
  "HELLO" nil nil)

(deftest copy-symbol.2
  (progn
    (setf (symbol-plist 'hello) '(x 10 y 20 z 30))
    (let ((x (copy-symbol 'hello)))
      (values
        (symbol-name x)
        (symbol-package x)
        (eq x 'hello)
        (get x 'y)
        (get 'hello 'y))))
  "HELLO" nil nil nil 20)

(deftest copy-symbol.3
  (progn
    (setf (symbol-value 'hello) 100)
    (setf (symbol-function 'hello) #'car)
    (setf (symbol-plist 'hello) '(x 10 y 20 z 30))
    (let ((x (copy-symbol 'hello t)))
      (values
        (symbol-name x)
        (symbol-package x)
        (symbol-value x)
        (symbol-function x)
        (eq x 'hello)
        (get x 'y)
        (get 'hello 'y))))
  "HELLO" nil 100 #.#'car nil 20 20)

(deftest gensym.1
  (let ((x (gensym)))
    (values
      (char (symbol-name x) 0)
      (symbol-package x)))
  #\G nil)

(deftest gensym.2
  (let* ((*gensym-counter* 10)
         (x (gensym)))
    (values
      (symbol-name x)
      (symbol-package x)
      *gensym-counter*))
  "G10" nil 11)

(deftest gensym.3
  (let* ((*gensym-counter* 10)
         (x (gensym 20)))
    (values
      (symbol-name x)
      (symbol-package x)
      *gensym-counter*))
  "G20" nil 10)

(deftest gensym.4
  (let* ((*gensym-counter* 10)
         (x (gensym "Hello")))
    (values
      (symbol-name x)
      (symbol-package x)
      *gensym-counter*))
  "Hello10" nil 11)

(deftest gensym.5
  (symbolp (gensym 0))
  t)

(deftest-error gensym.6
  (eval '(gensym -10))
  type-error)

(deftest gensym.7
  (let ((*gensym-counter* 1000000000000000000000000000))
    (values
      (typep *gensym-counter* 'bignum)
      (symbol-name (gensym))
      *gensym-counter*))
  t
  "G1000000000000000000000000000"
  1000000000000000000000000001)

(deftest gentemp.1
  (let ((x (gentemp)))
    (values
      (char (symbol-name x) 0)
      (eq (symbol-package x) *package*)))
  #\T t)

(deftest gentemp.2
  (let ((x (gentemp))
        (y (gentemp)))
    (values
      (eq x y)
      (equal (symbol-name x) (symbol-name y))))
  nil nil)

(deftest gentemp.3
  (let ((x (gentemp "HELLO")))
    (values
      (subseq (symbol-name x) 0 5)
      (eq (symbol-package x) *package*)))
  "HELLO" t)

(defpackage gentemp-package)
(deftest gentemp.4
  (let ((x (gentemp "A" 'gentemp-package))
        (p (find-package 'gentemp-package)))
    (values
      (char (symbol-name x) 0)
      (packagep p)
      (eq (symbol-package x) p)
      (eq (symbol-package x) *package*)))
  #\A t t nil)

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
    (setq hello-value 100)
    (symbol-value 'hello-value))
  100)

(deftest symbol-value.6
  (let ((hello-special 200))
    (declare (special hello-special))
    (symbol-value 'hello-special))
  200)

(defvar hello-shadow 100)
(deftest symbol-value.7
  (values
    (symbol-value 'hello-shadow)
    (let ((hello-shadow 200))
      (declare (special hello-shadow))
      (symbol-value 'hello-shadow)))
  100 200)

(deftest-error symbol-value.8
  (symbol-value 'no-such-symvol-value-name)
  unbound-variable)

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
    (setf (get x 'a 'ignored) 'b)
    (symbol-plist x))
  (a b))

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

(deftest boundp.1
  (boundp nil)
  t)

(deftest boundp.2
  (boundp t)
  t)

(deftest boundp.3
  (boundp :hello)
  t)

(deftest boundp.4
  (boundp (gensym))
  nil)

(deftest boundp.5
  (boundp 'hello-boundp-value)
  nil)

(deftest boundp.6
  (let ((hello-boundp-value 100))
    (declare (ignore hello-boundp-value))
    (boundp 'hello-boundp-value))
  nil)

(deftest boundp.7
  (let ((hello-boundp-value 100))
    (declare (special hello-boundp-value))
    (declare (ignore hello-boundp-value))
    (boundp 'hello-boundp-value))
  t)

(deftest boundp.8
  (boundp '*print-case*)
  t)

(defvar hello-boundp-true)
(deftest boundp.9
  (progn
    (setq hello-boundp-true 100)
    (boundp 'hello-boundp-true))
  t)

(deftest boundp.10
  (progn
    (setq hello-boundp-true 100)
    (makunbound 'hello-boundp-true)
    (boundp 'hello-boundp-true))
  nil)

(defvar makunbound-value)
(deftest makunbound.1
  (progn
    (setq makunbound-value 100)
    (makunbound 'makunbound-value))
  makunbound-value)

(deftest makunbound.2
  (progn
    (setq makunbound-value 100)
    (makunbound 'makunbound-value)
    (makunbound 'makunbound-value))
  makunbound-value)

(defvar set-value)
(deftest set.1
  (progn
    (set 'set-value 100)
    set-value)
  100)

(deftest unbound-variable.1
  (handler-case
    (symbol-value 'hello-no-such-variable)
    (unbound-variable (c) (cell-error-name c)))
  hello-no-such-variable)


;;
;;  do-tests
;;
(do-tests :test t)

