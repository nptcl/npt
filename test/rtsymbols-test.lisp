;;
;;  ANSI COMMON LISP: 10. Symbols
;;

;;
;;  Function MAKE-SYMBOL
;;
(deftest make-symbol.1
  (symbol-name
    (make-symbol "Hello"))
  "Hello")

(deftest make-symbol.2
  (symbol-package
    (make-symbol "temp"))
  nil)

(deftest make-symbol.3
  (eq (make-symbol "temp")
      (make-symbol "temp"))
  nil)

(deftest make-symbol.4
  (find-symbol "temp")
  nil nil)

(deftest-error make-symbol-error.1
  (eval '(make-symbol :hello))
  type-error)

(deftest-error! make-symbol-error.2
  (eval '(make-symbol)))

(deftest-error! make-symbol-error.3
  (eval '(make-symbol "hello" "hello")))


;;
;;  Function COPY-SYMBOL
;;
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

(deftest copy-symbol.4
  (symbolp
    (copy-symbol :hello))
  t)

(deftest-error copy-symbol-error.1
  (eval '(copy-symbol 10))
  type-error)

(deftest-error! copy-symbol-error.2
  (eval '(copy-symbol)))

(deftest-error! copy-symbol-error.3
  (eval '(copy-symbol 'hello t t)))

(deftest copy-symbol-test.1
  (setq *copy-symbol1* 'fred-smith)
  fred-smith)

(deftest copy-symbol-test.2
  (progn
    (setf (symbol-value *copy-symbol1*) 3)
    (setq *copy-symbol2* (copy-symbol *copy-symbol1* nil))
    (setq *copy-symbol3* (copy-symbol *copy-symbol1* nil))
    (setq *copy-symbol4* (copy-symbol *copy-symbol1* t))
    (setq *copy-symbol5* (copy-symbol *copy-symbol1* t))
    (every
      (lambda (x)
        (equalp (symbol-name x) "fred-smith"))
      (list *copy-symbol1*
            *copy-symbol2*
            *copy-symbol3*
            *copy-symbol4*
            *copy-symbol5*)))
  t)

(deftest copy-symbol-test.3
  (eq *copy-symbol1* *copy-symbol2*)
  nil)

(deftest copy-symbol-test.4
  (eq *copy-symbol2* *copy-symbol3*)
  nil)

(deftest copy-symbol-test.5
  (eq *copy-symbol4* *copy-symbol5*)
  nil)

(deftest copy-symbol-test.6
  (eq *copy-symbol2* *copy-symbol4*)
  nil)

(deftest copy-symbol-test.7
  (symbol-value *copy-symbol1*)
  3)

(deftest copy-symbol-test.8
  (boundp *copy-symbol2*)
  nil)

(deftest copy-symbol-test.9
  (symbol-value *copy-symbol4*)
  3)

(deftest copy-symbol-test.10
  (setf (symbol-value *copy-symbol4*) 4)
  4)

(deftest copy-symbol-test.11
  (symbol-value *copy-symbol1*)
  3)

(deftest copy-symbol-test.12
  (symbol-value *copy-symbol4*)
  4)

(deftest copy-symbol-test.13
  (symbol-value *copy-symbol5*)
  3)

(deftest copy-symbol-test.14
  (boundp *copy-symbol2*)
  nil)

(deftest copy-symbol-test.15
  (progn
    (setf (symbol-function *copy-symbol1*) #'(lambda (x) x))
    (fboundp *copy-symbol1*))
  t)

(deftest copy-symbol-test.16
  (fboundp *copy-symbol2*)
  nil)

(deftest copy-symbol-test.17
  (fboundp *copy-symbol4*)
  nil)


;;
;;  Function GENSYM
;;
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

(deftest gensym.6
  (let ((*gensym-counter* 1000000000000000000000000000))
    (values
      (typep *gensym-counter* 'bignum)
      (symbol-name (gensym))
      *gensym-counter*))
  t
  "G1000000000000000000000000000"
  1000000000000000000000000001)

(deftest gensym.7
  (eq (gensym 10)
      (gensym 10))
  nil)

(deftest gensym.8
  (progn
    (gensym 100)
    (find-symbol "G100"))
  nil nil)

(deftest-error gensym-error.1
  (eval '(gensym :hello))
  type-error)

(deftest-error gensym-error.2
  (eval '(gensym -10))
  type-error)

(deftest-error! gensym-error.3
  (eval '(gensym 10 20)))


;;
;;  Variable *GENSYM-COUNTER*
;;
(deftest gensym-counter.1
  (integerp *gensym-counter*)
  t)


;;
;;  Function GENTEMP
;;
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

(deftest gentemp.5
  (let ((x (gentemp)))
    (symbolp
      (find-symbol
        (symbol-name x))))
  t)

(deftest-error gentemp-error.1
  (eval '(gentemp :hello))
  type-error)

(deftest-error gentemp-error.2
  (eval '(gentemp 10))
  type-error)

(deftest-error gentemp-error.3
  (eval '(gentemp "HELLO" 20))
  type-error)

(deftest-error! gentemp-error.4
  (eval '(gentemp "HELLO" *package* nil)))


;;
;;  Function BOUNDP
;;
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
  (boundp 'boundp-lexical)
  nil)

(deftest boundp.6
  (let ((boundp-lexical 100))
    (declare (ignore boundp-lexical))
    (boundp 'boundp-lexical))
  nil)

(deftest boundp.7
  (let ((boundp-lexical 100))
    (declare (special boundp-lexical))
    (declare (ignore boundp-lexical))
    (boundp 'boundp-lexical))
  t)

(deftest boundp.8
  (boundp '*print-case*)
  t)

(defvar boundp-special)
(deftest boundp.9
  (progn
    (setq boundp-special 100)
    (boundp 'boundp-special))
  t)

(deftest boundp.10
  (progn
    (setq boundp-special 100)
    (makunbound 'boundp-special)
    (boundp 'boundp-special))
  nil)

(deftest-error boundp-error.1
  (eval '(boundp 10))
  type-error)

(deftest-error boundp-error.2
  (eval '(boundp '(setf car)))
  type-error)

(deftest-error! boundp-error.3
  (eval '(boundp)))

(deftest-error! boundp-error.4
  (eval '(boundp 'hello 'hello)))

;;  ANSI Common Lisp
(deftest boundp-test.1
  (progn
    (setq *boundp-test* 1)
    (boundp '*boundp-test*))
  t)

(deftest boundp-test.2
  (makunbound '*boundp-test*)
  *boundp-test*)

(deftest boundp-test.3
  (boundp '*boundp-test*)
  nil)

(deftest boundp-test.4
  (let ((*boundp-test* 2))
    (declare (ignorable *boundp-test*))
    (boundp '*boundp-test*))
  nil)

(deftest boundp-test.5
  (let ((*boundp-test* 2))
    (declare (ignorable *boundp-test*))
    (declare (special *boundp-test*))
    (boundp '*boundp-test*))
  t)


;;
;;  Function MAKUNBOUND
;;
(defvar *makunbound-value*)
(deftest makunbound.1
  (progn
    (setq *makunbound-value* 100)
    (makunbound '*makunbound-value*))
  *makunbound-value*)

(deftest makunbound.2
  (progn
    (setq *makunbound-value* 100)
    (makunbound '*makunbound-value*)
    (makunbound '*makunbound-value*))
  *makunbound-value*)

(deftest makunbound.3
  (progn
    (setq *makunbound-value* 100)
    (makunbound '*makunbound-value*)
    (boundp '*makunbound-value*))
  nil)

(deftest-error makunbound-error.1
  (eval '(makunbound 10))
  type-error)

(deftest-error! makunbound-error.2
  (eval '(makunbound)))

(deftest-error! makunbound-error.3
  (eval '(makunbound 'hello 'hello)))

(deftest-error makunbound-error.4
  (eval '(makunbound nil)))

;;  ANSI Common Lisp
(deftest makunbound-test.1
  (progn
    (setf (symbol-value '*makunbound-test*) 1)
    (boundp '*makunbound-test*))
  t)

(deftest makunbound-test.2
  *makunbound-test*
  1)

(deftest makunbound-test.3
  (makunbound '*makunbound-test*)
  *makunbound-test*)

(deftest makunbound-test.4
  (boundp '*makunbound-test*)
  nil)


;;
;;  Condition Type UNBOUND-VARIABLE
;;
(deftest unbound-variable.1
  (lisp-system:closp
    (find-class 'unbound-variable))
  t)

(deftest unbound-variable.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'unbound-variable)))
  (unbound-variable cell-error error serious-condition
                    condition standard-object t))

(deftest unbound-variable.3
  (handler-case
    (symbol-value 'hello-no-such-variable)
    (unbound-variable (c) (cell-error-name c)))
  hello-no-such-variable)

