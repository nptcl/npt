;;
;;  ANSI COMMON LISP: 7. Objects
;;
(import 'lisp-system::closp)
(import 'lisp-clos::referenced-class)
(use-package 'lisp-clos)

(deftest find-class.1
  (closp
    (find-class 'standard-class))
  t)

(deftest find-class.2
  (closp
    (find-class 'standard-class nil))
  t)

(deftest-error find-class.3
  (find-class 'no-such-class-name))

(deftest find-class.4
  (find-class 'no-such-class-name nil)
  nil)

(deftest referenced-class.1
  (null
    (referenced-class 'no-such-class))
  nil)

(deftest referenced-class.2
  (typep
    (referenced-class 'number)
    'built-in-class)
  t)

(deftest referenced-class.3
  (typep
    (referenced-class 'no-such-class)
    'lisp-clos::forward-referenced-class)
  t)

(deftest referenced-class.4
  (slot-value
    (referenced-class 'no-such-class)
    :name)
  no-such-class)

(deftest class-name.1
  (class-name
    (find-class 'standard-class))
  standard-class)

(deftest class-name.2
  (class-name
    (find-class 'string))
  string)

(deftest defclass.1
  (closp
    (defclass defclass1 () ()))
  t)

(deftest defclass.2
  (closp
    (find-class 'defclass1 nil))
  t)

(deftest defclass.3
  (class-name
    (find-class 'defclass1))
  defclass1)

(deftest defclass.4
  (values
    (subtypep 'defclass1 t)
    (subtypep 'defclass1 'standard-object)
    (subtypep 'defclass1 'standard-class))
  t t nil)

(defclass defclass2 () ())
(defclass defclass3 (defclass2) ())

(deftest defclass.5
  (values
    (subtypep 'defclass2 'defclass3)
    (subtypep 'defclass3 'defclass2)
    (subtypep 'defclass3 'standard-object)
    (subtypep 'defclass3 t))
  nil t t t)

(deftest defclass.6
  (class-name
    (find-class 'defclass3))
  defclass3)

(defclass defclass4 () (hello))

(deftest defclass.7
  (let ((inst (make-instance 'defclass4)))
    (slot-boundp inst 'hello))
  nil)

(deftest defclass.8
  (let ((inst (make-instance 'defclass4)))
    (setf (slot-value inst 'hello) 100)
    (slot-boundp inst 'hello))
  t)



;;
;;  do-tests
;;
(do-tests :test t)

