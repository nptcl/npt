;;
;;  ANSI COMMON LISP: 3. Evaluation and Compilation
;;

;;
;;  eval
;;
(deftest eval.1
  (eval '(+ 1 2))
  3)

(deftest eval.2
  (progn
    (setq test-eval-value 10)
    (let ((test-eval-value 1000))
      (declare (ignorable test-eval-value))
      (eval '(1+ test-eval-value))))
  11)

(deftest eval.3
  (eval (list 'cdr (car '((quote (a . b)) c))))
  b)


;;
;;  eval-when
;;
(deftest eval-when.1
  (eval-when (:execute)
    10)
  10)

(deftest eval-when.2
  (eval-when (eval)
    10)
  10)

(deftest eval-when.3
  (eval-when ()
    10)
  nil)


;;
;;  quote
;;
(deftest quote.1
  (quote aaa)
  aaa)

(deftest quote.2
  'hello
  hello)

(deftest quote.3
  ''a
  (quote a))

(deftest quote.4
  '"Hello"
  "Hello")


;;
;;  define-symbol-macro
;;
(deftest define-symbol-macro.1
  (define-symbol-macro define-symbol-macro1 (car '(a . b)))
  define-symbol-macro1)

(deftest define-symbol-macro.2
  (progn
    (define-symbol-macro define-symbol-macro2 (car '(a . b)))
    define-symbol-macro2)
  a)


;;
;;  macro-function
;;
(deftest macro-function.1
  (functionp
    (macro-function 'dolist))
  t)


;;
;;  special-operator-p
;;
(deftest special-operator-p.1
  (and (special-operator-p 'block)
       (special-operator-p 'catch)
       (special-operator-p 'eval-when)
       (special-operator-p 'flet)
       (special-operator-p 'function)
       (special-operator-p 'go)
       (special-operator-p 'if)
       (special-operator-p 'labels)
       (special-operator-p 'let)
       (special-operator-p 'let*)
       (special-operator-p 'load-time-value)
       (special-operator-p 'locally)
       (special-operator-p 'macrolet)
       (special-operator-p 'multiple-value-call)
       (special-operator-p 'multiple-value-prog1)
       (special-operator-p 'progn)
       (special-operator-p 'progv)
       (special-operator-p 'quote)
       (special-operator-p 'return-from)
       (special-operator-p 'setq)
       (special-operator-p 'symbol-macrolet)
       (special-operator-p 'tagbody)
       (special-operator-p 'the)
       (special-operator-p 'throw)
       (special-operator-p 'unwind-protect))
  t)


;;
;;  constantp
;;
(deftest constantp.1
  (constantp 1)
  t)

(deftest constantp.2
  (constantp ''temp)
  t)


;;
;;  proclaim
;;
(deftest proclaim.1
  (proclaim nil)
  nil)

(deftest proclaim.2
  (progn
    (proclaim '(special proclaim-test2 proclaim-test2a proclaim-test2b))
    (lisp-system::specialp 'proclaim-test2))
  t)

(deftest proclaim.3
  (progn
    (proclaim '(type integer proclaim-test3))
    (eval '(setq proclaim-test3 100))
    (eval 'proclaim-test3))
  100)

(deftest proclaim.4
  (proclaim '(ftype function proclaim-test4))
  nil)

(deftest proclaim.5
  (proclaim '(inline proclaim-test5 (setf proclaim-test5a)))
  nil)

(deftest proclaim.6
  (proclaim '(notinline proclaim-test6 (setf proclaim-test6a)))
  nil)

(deftest proclaim.7
  (proclaim '(optimize (compilation-speed 0)
                       (debug 1)
                       (space 2)
                       (speed 3)
                       safety))
  nil)

(deftest proclaim.8
  (proclaim '(declaration hello-proclaim-test8))
  nil)


;;
;;  load-time-value
;;
(deftest load-time-value.1
  (load-time-value 10)
  10)

(deftest load-time-value.2
  (load-time-value "Hello" t)
  "Hello")

(deftest load-time-value.3
  (load-time-value #\A nil)
  #\A)

(deftest load-time-value.4
  (load-time-value (+ 10 20 30) :hello)
  60)

(deftest load-time-value.5
  (+ (load-time-value 10)
     (load-time-value 20))
  30)

(deftest load-time-value.6
  (values
    (eql (load-time-value 10) 10)
    (eql (load-time-value 20) 20)
    (eql (load-time-value 30) 30)
    (equal (load-time-value "Hello") "Hello")
    (equal (load-time-value (concatenate 'string "AA" "BB" "CC")) "AABBCC"))
  t t t t t)

(deftest load-time-value.7
  (+ (eval '(load-time-value (+ 10 20)))
     (load-time-value 40))
  70)

(deftest-error load-time-value.8
  (eval '(let ((x 10))
           (declare (ignorable x))
           (load-time-value (+ x 20)))))

(defvar *load-time-value-test-1* nil)
(deftest load-time-value.9
  (eval '(progn
           (push (load-time-value
                   (progn
                     (push 10 *load-time-value-test-1*)
                     20))
                 *load-time-value-test-1*)
           *load-time-value-test-1*))
  (20 10))

(defvar *load-time-value-test-2* 10)
(deftest load-time-value.10
  (eval '(let (x)
           (dotimes (i 20 x)
             (setq x (load-time-value
                       (incf *load-time-value-test-2* 1))))))
  11)

