;;
;;  ANSI COMMON LISP: 5. Data and Control Flow
;;

(defmacro eval-quote (x)
  `(eval (quote ,x)))

;;
;;  Constant Variable CALL-ARGUMENTS-LIMIT
;;
(deftest call-arguments-limit.1
  (integerp call-arguments-limit)
  t)

(deftest call-arguments-limit.2
  (<= 50 call-arguments-limit)
  t)

(deftest call-arguments-limit.3
  (<= lambda-parameters-limit call-arguments-limit)
  t)

(deftest call-arguments-limit.4
  (constantp 'call-arguments-limit)
  t)

(deftest-error call-arguments-limit.5
  (eval '(setf call-arguments-limit 10)))


;;
;;  Constant Variable LAMBDA-PARAMETERS-LIMIT
;;
(deftest lambda-parameters-limit.1
  (integerp lambda-parameters-limit)
  t)

(deftest lambda-parameters-limit.2
  (<= 50 lambda-parameters-limit)
  t)

(deftest lambda-parameters-limit.3
  (constantp 'lambda-parameters-limit)
  t)

(deftest-error lambda-parameters-limit.4
  (eval '(setf lambda-parameters-limit 10)))


;;
;;  Constant Variable MULTIPLE-VALUES-LIMIT
;;
(deftest multiple-values-limit.1
  (integerp multiple-values-limit)
  t)

(deftest multiple-values-limit.2
  (<= 20 multiple-values-limit)
  t)

(deftest multiple-values-limit.3
  (constantp multiple-values-limit)
  t)

(deftest-error multiple-values-limit.4
  (eval '(setf multiple-values-limit 10)))


;;
;;  Constant Variable LAMBDA-LIST-KEYWORDS
;;
(deftest lambda-list-keywords.1
  (listp lambda-list-keywords)
  t)

(deftest lambda-list-keywords.2
  (let ((a lambda-list-keywords))
    (and (member '&optional a)
         (member '&rest a)
         (member '&body a)
         (member '&key a)
         (member '&allow-other-keys a)
         (member '&aux a)
         (member '&whole a)
         (member '&environment a)
         t))
  t)

(deftest lambda-list-keywords.3
  (constantp 'lambda-list-keywords)
  t)

(deftest-error lambda-list-keywords.4
  (eval '(setf lambda-list-keywords nil)))


;;
;;  Constant Variable NIL
;;
(deftest nil.1
  (null nil)
  t)

(deftest nil.2
  (symbolp nil)
  t)

(deftest nil.3
  (listp nil)
  t)

(deftest nil.4
  (consp nil)
  nil)

(deftest nil.5
  (typep nil 'boolean)
  t)

(deftest nil.6
  (symbol-value 'nil)
  nil)

(deftest nil.7
  (symbol-name 'nil)
  "NIL")

(deftest nil.8
  (constantp nil)
  t)

(deftest-error nil.9
  (eval-quote
    (setq nil 100)))

(deftest-error nil.10
  (eval-quote
    (let ((nil 10))
      nil)))


;;
;;  Constant Variable T
;;
(deftest t.1
  (symbolp t)
  t)

(deftest t.2
  (null t)
  nil)

(deftest t.3
  (typep t 'boolean)
  t)

(deftest t.4
  (constantp t)
  t)

(deftest t.5
  (symbol-value t)
  t)

(deftest t.6
  (symbol-name 't)
  "T")

(deftest-error t.7
  (eval-quote
    (setq t 100)))

(deftest-error t.8
  (eval-quote
    (let ((t 100))
      t)))

;;  ANSI Common Lisp
(deftest t-test.1
  t t)

(deftest t-test.2
  (eq t 't)
  t)

(deftest t-test.3
  (lisp-system:closp
    (find-class 't))
  t)

(deftest t-test.4
  (case 'a (a 1) (t 2))
  1)

(deftest t-test.5
  (case 'b (a 1) (t 2))
  2)

(deftest t-test.6
  (with-output-to-string (*terminal-io*)
    (prin1 'hello t))
  "HELLO")


;;
;;  Macro DEFCONSTANT
;;
(deftest defconstant.1
  (progn
    (defconstant defconstant-1 100)
    defconstant-1)
  100)

(deftest defconstant.2
  (defconstant defconstant-2 100)
  defconstant-2)

(deftest defconstant.3
  (progn
    (defconstant defconstant-3 200)
    (defconstant defconstant-3 200)
    defconstant-3)
  200)

(deftest-error defconstant.4
  (progn
    (defconstant defconstant-4 100)
    (defconstant defconstant-4 200)))

(deftest defconstant.5
  (progn
    (defconstant defconstant-5 333 "HELLO")
    defconstant-5)
  333)

(deftest-error defconstant.6
  (progn
    (defconstant defconstant-6 444)
    (setq defconstant-6 555)))

(deftest defconstant.7
  (progn
    (defconstant defconstant-7 333 "HELLO")
    (lisp-system::getdoc-variable 'defconstant-7))
  "HELLO")

(deftest defconstant.8
  (progn
    (defconstant defconstant-8 333 "ABC")
    (documentation 'defconstant-8 'variable))
  "ABC")

(deftest-error defconstant.9
  (progn
    (defconstant defconstant-9 333)
    (setq defconstant-9 444)))

(deftest-error defconstant-error.1
  (eval '(defconstant aaa)))

(deftest-error defconstant-error.2
  (eval '(defconstant 10 20)))

(deftest-error defconstant-error.3
  (eval '(defconstant aaa 20 nil)))

(deftest-error defconstant-error.4
  (eval '(defconstant aaa 20 "Hello" nil)))

;;  ANSI Common Lisp
(deftest defconstant-test.1
  (progn
    (setf (symbol-value 'defconstant-test-1) 10)
    (setf (documentation 'defconstant-test-1 'variable) "DOCUMENT-TEST")
    (lisp-system::getdoc-variable 'defconstant-test-1))
  "DOCUMENT-TEST")

(deftest defconstant-test.2
  (defconstant defconstant-test-2 'never-changing "for a test")
  defconstant-test-2)

(deftest defconstant-test.3
  (eval 'defconstant-test-2)
  never-changing)

(deftest defconstant-test.4
  (documentation 'defconstant-test-2 'variable)
  "for a test")

(deftest defconstant-test.5
  (constantp 'defconstant-test-2)
  t)


;;
;;  Macro DEFPARAMETER
;;
(deftest defparameter.1
  (defparameter defparameter-1 100)
  defparameter-1)

(deftest defparameter.2
  (progn
    (defparameter defparameter-2 100)
    defparameter-2)
  100)

(deftest defparameter.3
  (progn
    (defparameter defparameter-3 100)
    (defparameter defparameter-3 200)
    defparameter-3)
  200)

(deftest defparameter.4
  (progn
    (defparameter defparameter-4 100)
    (setq defparameter-4 300)
    defparameter-4)
  300)

(deftest defparameter.5
  (progn
    (defparameter defparameter-5 100)
    (lisp-system::specialp 'defparameter-5))
  t)

(deftest defparameter.6
  (progn
    (defparameter defparameter-6 100 "AAA")
    (lisp-system::getdoc-variable 'defparameter-6))
  "AAA")

(deftest defparameter.7
  (progn
    (defparameter defparameter-7 100 "AAA")
    (lisp-system::specialp 'defparameter-7))
  t)

(deftest defparameter.8
  (progn
    (defparameter defparameter-8 333 "ABC")
    (documentation 'defparameter-8 'variable))
  "ABC")

(deftest defparameter.9
  (progn
    (defparameter defparameter-9 333)
    (setq defparameter-9 444)
    defparameter-9)
  444)

(deftest-error defparameter-error.1
  (eval '(defparameter aaa)))

(deftest-error defparameter-error.2
  (eval '(defparameter 10 20)))

(deftest-error defparameter-error.3
  (eval '(defparameter aaa 20 nil)))

(deftest-error defparameter-error.4
  (eval '(defparameter aaa 20 "Hello" nil)))


;;
;;  Macro DEFVAR
;;
(deftest defvar.1
  (defvar defvar-1)
  defvar-1)

(deftest defvar.2
  (progn
    (defvar defvar-2)
    (boundp 'defvar-2))
  nil)

(deftest defvar.3
  (progn
    (defvar defvar-3)
    (lisp-system::specialp 'defvar-2))
  t)

(deftest defvar.4
  (defvar defvar-4 100)
  defvar-4)

(deftest defvar.5
  (progn
    (defvar defvar-5 200)
    defvar-5)
  200)

(deftest defvar.6
  (progn
    (defvar defvar-6 300)
    (lisp-system::specialp 'defvar-6))
  t)

(deftest defvar.7
  (defvar defvar-7 400 "ZZZ")
  defvar-7)

(deftest defvar.8
  (progn
    (defvar defvar-8 500 "ZZZ")
    defvar-8)
  500)

(deftest defvar.9
  (progn
    (defvar defvar-9 600 "ZZZ")
    (lisp-system::specialp 'defvar-9))
  t)

(deftest defvar.10
  (progn
    (defvar defvar-10 700 "ZZZ")
    (lisp-system::getdoc-variable 'defvar-10))
  "ZZZ")

(deftest defvar.11
  (progn
    (defvar defvar-11 333 "ABC")
    (documentation 'defvar-11 'variable))
  "ABC")

(deftest defvar.12
  (progn
    (defvar defvar-12 333)
    (setq defvar-12 444)
    defvar-12)
  444)

(deftest-error defvar-error.1
  (eval '(defvar)))

(deftest-error defvar-error.2
  (eval '(defvar 10 20)))

(deftest-error defvar-error.3
  (eval '(defvar aaa 20 nil)))

(deftest-error defvar-error.4
  (eval '(defvar aaa 20 "Hello" nil)))

;;  ANSI Common Lisp
(deftest defvar-test.1
  (defparameter *defvar-test-1* 1)
  *defvar-test-1*)

(deftest defvar-test.2
  (eval '*defvar-test-1*)
  1)

(deftest defvar-test.3
  (constantp '*defvar-test-1*)
  nil)

(deftest defvar-test.4
  (setq *defvar-test-1* 2)
  2)

(deftest defvar-test.5
  (defparameter *defvar-test-1* 3)
  *defvar-test-1*)

(deftest defvar-test.6
  (eval '*defvar-test-1*)
  3)

(deftest defvar-test.7
  (defvar *defvar-test-2* 1)
  *defvar-test-2*)

(deftest defvar-test.8
  (eval '*defvar-test-2*)
  1)

(deftest defvar-test.9
  (constantp '*defvar-test-2*)
  nil)

(deftest defvar-test.10
  (setq *defvar-test-2* 2)
  2)

(deftest defvar-test.11
  (defvar *defvar-test-2* 3)
  *defvar-test-2*)

(deftest defvar-test.12
  (eval '*defvar-test-2*)
  2)

(deftest defvar-test.13
  (progn
    (defun defvar-test-3 ()
      (let ((*defvar-test-1* 'defvar-test-1)
            (*defvar-test-2* 'defvar-test-2))
        (defvar-test-4)))
    (defun defvar-test-4 ()
      (list *defvar-test-1* *defvar-test-2*))
    (defvar-test-3))
  (defvar-test-1 defvar-test-2))


;;
;;  Macro PROG
;;
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
    (return a))
  nil)

(deftest prog.5
  (prog (a)
    (setq a 10)
    (return a))
  10)

(deftest prog.6
  (let ((a 10))
    (prog ((a (1+ a))
           (b a))
      (return (values a b))))
  11 10)

(deftest prog.7
  (prog (a)
    (go 10)
    (setq a 999)
    10
    (return a))
  nil)

(deftest prog.8
  (prog (a)
    (declare (ignore a))
    10)
  nil)

(deftest-error prog-error.1
  (eval '(prog)))

(deftest-error prog-error.2
  (eval '(prog 10)))


;;
;;  Macro PROG*
;;
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
    (return a))
  nil)

(deftest prog*.5
  (prog* (a)
    (setq a 10)
    (return a))
  10)

(deftest prog*.6
  (let ((a 10))
    (prog* ((a (1+ a))
            (b a))
      (return (values a b))))
  11 11)

(deftest prog*.7
  (prog* (a)
    (go 10)
    (setq a 999)
    10
    (return a))
  nil)

(deftest-error prog*-error.1
  (eval '(prog*)))

(deftest-error prog*-error.2
  (eval '(prog* 10)))

;;  ANSI Common Lisp
(deftest prog-test.1
  (let ((a 1))
    (declare (ignorable a))
    (prog ((a 2) (b a))
      (return (if (= a b) '= '/=))))
  /=)

(deftest prog-test.2
  (let ((a 1))
    (declare (ignorable a))
    (prog* ((a 2) (b a)) (return (if (= a b) '= '/=))))
  =)

(deftest prog-test.3
  (prog () 'no-return-value)
  nil)


;;
;;  Macro PROG1
;;
(deftest prog1.1
  (prog1 10)
  10)

(deftest prog1.2
  (prog1 10 20)
  10)

(deftest prog1.3
  (prog1 10 20 30)
  10)

(deftest prog1.4
  (prog1 (values 10 20 30)
    (values 40 50 60))
  10)

(deftest-error prog1.5
  (eval '(prog1)))


;;
;;  Macro PROG2
;;
(deftest prog2.1
  (prog2 10 20)
  20)

(deftest prog2.2
  (prog2 10 20 30 40)
  20)

(deftest prog2.3
  (prog2 (values 10 20 30)
         (values 40 50 60)
         (values 70 80 90))
  40)

(deftest-error prog2.4
  (eval '(prog2 10)))

;; ANSI Common Lisp
(setq prog1-test-temp 1)

(deftest prog1-test.1
  (let (list)
    (values
      (prog1 prog1-test-temp
        (push prog1-test-temp list)
        (incf prog1-test-temp)
        (push prog1-test-temp list))
      (nreverse list)))
  1 (1 2))

(deftest prog1-test.2
  (prog1 prog1-test-temp
    (setq prog1-test-temp nil))
  2)

(deftest prog1-test.3
  prog1-test-temp
  nil)

(deftest prog1-test.4
  (prog1 (values 1 2 3) 4)
  1)

(deftest prog1-test.5
  (progn
    (setq prog1-test-temp (list 'a 'b 'c))
    (prog1 (car prog1-test-temp) (setf (car prog1-test-temp) 'alpha)))
  a)

(deftest prog1-test.6
  prog1-test-temp
  (alpha b c))

(deftest prog1-test.7
  (flet ((swap-symbol-values
           (x y) (setf (symbol-value x)
                       (prog1 (symbol-value y)
                         (setf (symbol-value y) (symbol-value x))))))
    (let ((*prog1-test-foo* 1) (*prog1-test-bar* 2))
      (declare (special *prog1-test-foo* *prog1-test-bar*))
      (swap-symbol-values '*prog1-test-foo* '*prog1-test-bar*)
      (values *prog1-test-foo* *prog1-test-bar*)))
  2 1)

(deftest prog1-test.8
  (progn
    (setq prog1-test-temp 1)
    (prog2 (incf prog1-test-temp)
           (incf prog1-test-temp)
           (incf prog1-test-temp)))
  3)

(deftest prog1-test.9
  prog1-test-temp
  4)

(deftest prog1-test.10
  (prog2 1 (values 2 3 4) 5)
  2)


;;
;;  Special Operator PROGN
;;
(deftest progn.1
  (progn)
  nil)

(deftest progn.2
  (progn 10)
  10)

(deftest progn.3
  (progn 10 20 (progn 30))
  30)

(deftest progn.4
  (progn
    (values 10 20 30)
    (values 40 50 60))
  40 50 60)

(deftest progn.5
  (progn
    (values)))

;;  ANSI Common Lisp
(deftest progn-test.1
  (progn)
  nil)

(deftest progn-test.2
  (progn 1 2 3)
  3)

(deftest progn-test.3
  (progn (values 1 2 3))
  1 2 3)

(deftest progn-test.4
  (let ((a 1))
    (values
      (if a
        (progn (setq a nil) 'here)
        (progn (setq a t) 'there))
      a))
  here nil)

