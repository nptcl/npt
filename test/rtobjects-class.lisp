;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  find-class
;;
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

(defclass setf-find1 () ())
(defclass setf-find2 () ())

(deftest setf-find-class.1
  (let ((inst (find-class 'setf-find2)))
    (setf (find-class 'setf-find1) inst)
    (class-name
      (find-class 'setf-find1)))
  setf-find2)

(defclass setf-find3 () ())
(deftest setf-find-class.2
  (let ((inst (find-class 'setf-find3)))
    (setf (find-class 'setf-find1 :hello-hello) inst)
    (class-name
      (find-class 'setf-find1)))
  setf-find3)


;;
;;  referenced-class
;;
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
    'lisp-clos::name)
  no-such-class)


;;
;;  slot-boundp
;;
(defclass boundp1 ()
  (aaa (bbb :initform nil)))

(deftest slot-boundp.1
  (let ((inst (make-instance 'boundp1)))
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  nil t)

(deftest-error slot-boundp.2
  (let ((inst (make-instance 'boundp1)))
    (slot-boundp inst 'hello)))

(deftest slot-boundp.3
  (slot-boundp
    (find-class 'standard-class)
    'lisp-clos::name)
  t)


;;
;;  slot-exists-p
;;
(deftest slot-exists-p.1
  (let ((inst (make-instance 'boundp1)))
    (values
      (slot-exists-p inst 'aaa)
      (slot-exists-p inst 'bbb)
      (slot-exists-p inst 'hello)))
  t t nil)

(deftest slot-exists-p.2
  (let ((inst (make-instance 'standard-class)))
    (values
      (slot-exists-p inst 'lisp-clos::name)
      (slot-exists-p inst 'hello)))
  t nil)


;;
;;  slot-makunbound
;;
(deftest slot-makunbound.1
  (let ((inst (make-instance 'boundp1)))
    (slot-makunbound inst 'aaa)
    (slot-makunbound inst 'bbb)
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  nil nil)

(deftest-error slot-makunbound.2
  (let ((inst (make-instance 'boundp1)))
    (slot-makunbound inst 'hello)))


;;
;;  slot-value
;;
(defclass slot-value1 ()
  ((aaa)
   (bbb :initform "Hello")))

(deftest slot-value.1
  (let ((inst (make-instance 'slot-value1)))
    (slot-value inst 'bbb))
  "Hello")

(deftest-error slot-value.2
  (let ((inst (make-instance 'slot-value1)))
    (slot-value inst 'aaa)))


;;
;;  (setf slot-value)
;;
(deftest setf-slot-value.1
  (let ((inst (make-instance 'slot-value1)))
    (values
      (setf (slot-value inst 'bbb) "zzz")
      (slot-value inst 'bbb)))
  "zzz" "zzz")

(deftest setf-slot-value.2
  (let ((inst (make-instance 'slot-value1)))
    (values
      (setf (slot-value inst 'aaa) "1234")
      (slot-value inst 'aaa)
      (slot-boundp inst 'aaa)))
  "1234" "1234" t)


;;
;;  class-of
;;
(deftest class-of.1
  (subtypep
    (class-of 'hello)
    'symbol)
  t t)

(deftest class-of.2
  (class-name
    (class-of
      (find-class 'standard-class)))
  standard-class)

(deftest class-of.3
  (class-name
    (class-of
      (find-class 'class)))
  standard-class)

(defclass class-of1 () ())
(defclass class-of2 (class-of1) ())
(deftest class-of.4
  (let ((inst (make-instance 'class-of2)))
    (class-name
      (class-of inst)))
  class-of2)

(defclass with-slots1 ()
  ((aaa :initform 100)))

(deftest with-slots.1
  (with-slots (aaa) (make-instance 'with-slots1)
    aaa)
  100)

(deftest with-slots.2
  (with-slots (aaa) (make-instance 'with-slots1)
    (setq aaa 200)
    aaa)
  200)

(defclass with-slots2 ()
  (aaa bbb ccc))

(deftest with-slots.3
  (let ((inst (make-instance 'with-slots2)))
    (with-slots (aaa bbb ccc) inst
      (setq aaa 111 bbb 222 ccc 333))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  111 222 333)

(deftest with-slots.4
  (let ((inst (make-instance 'with-slots2)))
    (with-slots (aaa (zzz bbb) (hello ccc)) inst
      (setq aaa 111 zzz 222 hello 333)
      (values
        aaa
        zzz
        hello
        (slot-value inst 'aaa)
        (slot-value inst 'bbb)
        (slot-value inst 'ccc))))
  111 222 333 111 222 333)


;;
;;  make-instance
;;
(defclass make-instance1 ()
  ((aaa :initarg :xxx)
   (bbb :initarg :yyy)))

(deftest make-instance.1
  (let ((inst (make-instance 'make-instance1)))
    (subtypep (class-of inst) 'make-instance1))
  t t)

(deftest make-instance.2
  (let ((inst (make-instance 'make-instance1)))
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  nil nil)

(deftest make-instance.3
  (let ((inst (make-instance 'make-instance1 :xxx 100 :yyy 200)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  100 200)


;;
;;  defclass
;;
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


;;  initarg
(defclass initarg1 ()
  ((aaa :initarg :bbb)
   (ccc :initarg :ddd)))

(defclass initarg2 (initarg1)
  ((eee :initarg :fff)))

(deftest defclass-initarg.1
  (let ((inst (make-instance 'initarg2)))
    (values
      (slot-exists-p inst 'aaa)
      (slot-exists-p inst 'ccc)
      (slot-exists-p inst 'eee)))
  t t t)

(deftest defclass-initarg.2
  (let ((inst (make-instance 'initarg2 :bbb 10 :ddd 20 :fff 30)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'ccc)
      (slot-value inst 'eee)))
  10 20 30)

(defclass initarg3 (initarg1)
  ((eee :initarg :bbb)))

(deftest defclass-initarg.3
  (let ((inst (make-instance 'initarg3 :bbb 10 :ddd 20)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'ccc)
      (slot-value inst 'eee)))
  10 20 10)


;;  initform
(let ((value 0))
  (defun initform-call (&optional x)
    (if x
      (setq value x)
      (prog1 value
        (incf value)))))

(defclass initform1 ()
  ((aaa :initarg :bbb :initform 100)
   (ccc :initarg :ddd :initform (initform-call))))

(deftest defclass-initform.1
  (let ((inst1 (make-instance 'initform1))
        (inst2 (make-instance 'initform1)))
    (values
      (slot-value inst1 'aaa)
      (slot-value inst1 'ccc)
      (slot-value inst2 'aaa)
      (slot-value inst2 'ccc)))
  100 0 100 1)

(deftest defclass-initform.2
  (let ((inst1 (make-instance 'initform1 :bbb 11 :ddd 22))
        (inst2 (make-instance 'initform1 :bbb 33)))
    (values
      (slot-value inst1 'aaa)
      (slot-value inst1 'ccc)
      (slot-value inst2 'aaa)
      (slot-value inst2 'ccc)
      (initform-call)))
  11 22 33 2 3)

(defclass initform2 (initform1)
  ((aaa :initarg :bbb :initform 200)
   (eee :initarg :fff :initform 333)))

(deftest defclass-initform.3
  (let ((inst1 (make-instance 'initform2))
        (inst2 (make-instance 'initform2)))
    (values
      (slot-value inst1 'aaa)
      (slot-value inst1 'ccc)
      (slot-value inst1 'eee)
      (slot-value inst2 'aaa)
      (slot-value inst2 'ccc)
      (slot-value inst2 'eee)))
  200 4 333 200 5 333)

(deftest defclass-initform.4
  (let ((inst1 (make-instance 'initform2 :bbb 11 :ddd 22))
        (inst2 (make-instance 'initform2 :fff 33)))
    (values
      (slot-value inst1 'aaa)
      (slot-value inst1 'ccc)
      (slot-value inst1 'eee)
      (slot-value inst2 'aaa)
      (slot-value inst2 'ccc)
      (slot-value inst2 'eee)
      (initform-call)))
  11 22 333 200 6 33 7)


;;  :class instance
(defclass instclass1 ()
  ((aaa :initarg :aaa :allocation :class)
   (bbb :initarg :bbb :allocation :instance)
   (ccc :initarg :ccc :allocation :class)
   (ddd :initarg :bbb :allocation :instance)))

(deftest defclass-class.1
  (let ((inst1 (make-instance 'instclass1))
        (inst2 (make-instance 'instclass1)))
    (setf (slot-value inst1 'aaa) 10)
    (setf (slot-value inst1 'bbb) 20)
    (setf (slot-value inst2 'aaa) 30)
    (setf (slot-value inst2 'bbb) 40)
    (values
      (slot-value inst1 'aaa)
      (slot-value inst1 'bbb)
      (slot-value inst2 'aaa)
      (slot-value inst2 'bbb)))
  30 20 30 40)

(defclass instclass1a ()
  ((aaa :initarg :aaa :allocation :class)
   (bbb :initarg :bbb :allocation :instance)
   (ccc :initarg :ccc :allocation :class)
   (ddd :initarg :bbb :allocation :instance)))

(deftest defclass-class.2
  (let ((inst1 (make-instance 'instclass1a))
        (inst2 (make-instance 'instclass1a)))
    (values
      ;; init-boundp
      (slot-boundp inst1 'aaa)
      (slot-boundp inst1 'bbb)
      (slot-boundp inst2 'aaa)
      (slot-boundp inst2 'bbb)
      ;; set-boundp
      (progn
        (setf (slot-value inst1 'aaa) 10)
        (setf (slot-value inst1 'bbb) 20)
        (slot-boundp inst1 'aaa))
      (slot-boundp inst1 'bbb)
      (slot-boundp inst2 'aaa)
      (slot-boundp inst2 'bbb)
      ;; exists
      (slot-exists-p inst1 'aaa)
      (slot-exists-p inst1 'bbb)
      (slot-exists-p inst1 'zzz)
      ;; makunbuond
      (progn
        (setf (slot-value inst2 'aaa) 30)
        (setf (slot-value inst2 'bbb) 40)
        (slot-makunbound inst1 'aaa)
        (slot-makunbound inst1 'bbb)
        (slot-boundp inst1 'aaa))
      (slot-boundp inst1 'bbb)
      (slot-boundp inst2 'aaa)
      (slot-boundp inst2 'bbb)))
  nil nil nil nil
  t t t nil
  t t nil
  nil nil nil t)

(defclass instclass1b ()
  ((aaa :initarg :aaa :allocation :class)
   (bbb :initarg :bbb :allocation :instance)
   (ccc :initarg :ccc :allocation :class)
   (ddd :initarg :bbb :allocation :instance)))

(deftest defclass-class.3
  (let ((inst1 (make-instance 'instclass1b))
        (inst2 (make-instance 'instclass1b)))
    (values
      ;; init-bondp
      (slot-boundp inst1 'aaa)
      (slot-boundp inst1 'bbb)
      (slot-boundp inst2 'aaa)
      (slot-boundp inst2 'bbb)
      ;; set-boundp
      (progn
        (setf (slot-value inst2 'aaa) 30)
        (setf (slot-value inst2 'bbb) 40)
        (slot-boundp inst1 'aaa))
      (slot-boundp inst1 'bbb)
      (slot-boundp inst2 'aaa)
      (slot-boundp inst2 'bbb)
      ;; exists
      (slot-exists-p inst2 'aaa)
      (slot-exists-p inst2 'bbb)
      (slot-exists-p inst2 'zzz)
      ;; makunbound
      (progn
        (setf (slot-value inst1 'aaa) 10)
        (setf (slot-value inst1 'bbb) 20)
        (slot-makunbound inst2 'aaa)
        (slot-makunbound inst2 'bbb)
        (slot-boundp inst1 'aaa))
      (slot-boundp inst1 'bbb)
      (slot-boundp inst2 'aaa)
      (slot-boundp inst2 'bbb)))
  nil nil nil nil
  t nil t t
  t t nil
  nil t nil nil)

(defclass instclass2 (instclass1)
  ((ccc :initarg :ccc :allocation :class)
   (ddd :initarg :ddd :allocation :instance)
   (eee :initarg :eee :allocation :class)
   (fff :initarg :fff :allocation :instance)))

(deftest defclass-class.4
  (let ((inst1 (make-instance 'instclass1))
        (inst2 (make-instance 'instclass2)))
    (setf (slot-value inst1 'aaa) 10)
    (setf (slot-value inst1 'bbb) 20)
    (setf (slot-value inst1 'ccc) 30)
    (setf (slot-value inst1 'ddd) 40)
    (setf (slot-value inst2 'ccc) 50)
    (setf (slot-value inst2 'ddd) 60)
    (setf (slot-value inst2 'eee) 70)
    (setf (slot-value inst2 'fff) 80)
    (values
      (slot-value inst1 'aaa)
      (slot-value inst1 'bbb)
      (slot-value inst1 'ccc)
      (slot-value inst1 'ddd)
      (slot-value inst2 'ccc)
      (slot-value inst2 'ddd)
      (slot-value inst2 'eee)
      (slot-value inst2 'fff)
      ;; bound
      (progn
        (slot-makunbound inst1 'ccc)
        (slot-makunbound inst1 'ddd)
        (slot-boundp inst1 'ccc))
      (slot-boundp inst1 'ddd)
      (slot-boundp inst2 'ccc)
      (slot-boundp inst2 'ddd)
      (progn
        (setf (slot-value inst1 'ccc) 30)
        (setf (slot-value inst1 'ddd) 40)
        (slot-makunbound inst2 'ccc)
        (slot-makunbound inst2 'ddd)
        (slot-boundp inst1 'ccc))
      (slot-boundp inst1 'ddd)
      (slot-boundp inst2 'ccc)
      (slot-boundp inst2 'ddd)
      (slot-exists-p inst1 'ccc)
      (slot-exists-p inst1 'eee)
      (slot-exists-p inst2 'ccc)
      (slot-exists-p inst2 'eee)))
  10 20 30 40 50 60 70 80
  nil nil t t
  t t nil nil
  t nil t t)

(defclass instclass3 (instclass1)
  ((eee :initarg :eee :allocation :class)
   (fff :initarg :fff :allocation :instance)))

(deftest defclass-class.5
  (let ((inst1 (make-instance 'instclass1))
        (inst2 (make-instance 'instclass3)))
    (setf (slot-value inst1 'aaa) 10)
    (setf (slot-value inst1 'bbb) 20)
    (setf (slot-value inst1 'ccc) 30)
    (setf (slot-value inst1 'ddd) 40)
    (setf (slot-value inst2 'ccc) 50)
    (setf (slot-value inst2 'ddd) 60)
    (setf (slot-value inst2 'eee) 70)
    (setf (slot-value inst2 'fff) 80)
    (values
      (slot-value inst1 'aaa)
      (slot-value inst1 'bbb)
      (slot-value inst1 'ccc)
      (slot-value inst1 'ddd)
      (slot-value inst2 'ccc)
      (slot-value inst2 'ddd)
      (slot-value inst2 'eee)
      (slot-value inst2 'fff)
      ;; bound
      (progn
        (slot-makunbound inst1 'ccc)
        (slot-makunbound inst1 'ddd)
        (slot-boundp inst1 'ccc))
      (slot-boundp inst1 'ddd)
      (slot-boundp inst2 'ccc)
      (slot-boundp inst2 'ddd)
      (progn
        (setf (slot-value inst1 'ccc) 30)
        (setf (slot-value inst1 'ddd) 40)
        (slot-makunbound inst2 'ccc)
        (slot-makunbound inst2 'ddd)
        (slot-boundp inst1 'ccc))
      (slot-boundp inst1 'ddd)
      (slot-boundp inst2 'ccc)
      (slot-boundp inst2 'ddd)
      (slot-exists-p inst1 'ccc)
      (slot-exists-p inst1 'eee)
      (slot-exists-p inst2 'ccc)
      (slot-exists-p inst2 'eee)))
  10 20 50 40 50 60 70 80
  nil nil nil t
  nil t nil nil
  t nil t t)

;; initargs
(defclass initargs1 ()
  ((aaa :initarg :aaa :allocation :class)
   (bbb :initarg :bbb :allocation :class)
   (ccc :initarg :ccc)))

(defclass initargs2 (initargs1)
  ((bbb :initarg :ddd :allocation :class)
   (eee :initarg :eee)))

(deftest defclass-initargs.1
  (let ((inst1 (make-instance 'initargs1))
        (inst2 (make-instance 'initargs2)))
    (values
      (slot-boundp inst1 'aaa)
      (slot-boundp inst1 'bbb)
      (slot-boundp inst1 'ccc)
      (slot-boundp inst2 'bbb)
      (slot-boundp inst2 'eee)))
  nil nil nil nil nil)

(deftest defclass-initargs.2
  (let ((inst (make-instance 'initargs1 :aaa 10 :bbb 20 :ccc 30)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  10 20 30)

(deftest defclass-initargs.3
  (let ((inst1 (make-instance 'initargs1 :aaa 10 :bbb 20 :ccc 30))
        (inst2 (make-instance 'initargs2 :aaa 40 :bbb 50 :ccc 60 :ddd 70 :eee 80)))
    (values
      (slot-value inst1 'aaa)
      (slot-value inst1 'bbb)
      (slot-value inst1 'ccc)
      (slot-value inst2 'aaa)
      (slot-value inst2 'bbb)
      (slot-value inst2 'ccc)
      (slot-value inst2 'eee)))
  40 20 30 40 50 60 80)

(deftest defclass-initargs.4
  (let ((inst (make-instance 'initargs1 :aaa 10 :bbb 20 :aaa 30)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  10 20)


;;  initform
(defclass initform5 ()
  ((aaa :initarg :aaa :initform 100)))

(deftest defclass-initform-init.1
  (let ((inst (make-instance 'initform5)))
    (slot-value inst 'aaa))
  100)

(deftest defclass-initform-init.2
  (let ((inst (make-instance 'initform5 :aaa 200)))
    (slot-value inst 'aaa))
  200)

(defclass initform6 (initform5)
  ((bbb :initarg :bbb :initform 200)))

(deftest defclass-initform-init.3
  (let ((inst (make-instance 'initform6)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  100 200)

(deftest defclass-initform-init.4
  (let ((inst (make-instance 'initform6 :bbb 333 :aaa 444)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  444 333)

(defclass initform7 ()
  ((aaa :initarg :aaa :initform 10 :allocation :class)
   (bbb :initarg :bbb :initform 20 :allocation :class)))

(deftest defclass-initform-init.5
  (let ((inst (make-instance 'initform7)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (progn
        (setf (slot-value inst 'bbb) 999)
        (slot-value (make-instance 'initform7) 'bbb))))
  10 20 999)

(deftest defclass-initform-init.6
  (let ((inst (make-instance 'initform7 :aaa 44 :bbb 55)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (progn
        (setf (slot-value inst 'bbb) 999)
        (slot-value (make-instance 'initform7) 'bbb))))
  44 55 999)

(defclass initform8 (initform7)
  ((bbb :initarg :bbb :initform 30 :allocation :class)
   (ccc :initarg :ccc :initform 40 :allocation :class)))

(deftest defclass-initform-init.7
  (let ((inst (make-instance 'initform8 :aaa 11 :bbb 22 :ccc 33)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  11 22 33)


;;  default-initargs
(defclass defargs1 ()
  ((aaa :initarg :bbb)
   (ccc :initarg :ddd))
  (:default-initargs :bbb 10 :ddd 20))

(deftest defclass-default-initargs.1
  (let ((inst (make-instance 'defargs1)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'ccc)))
  10 20)

(defclass defargs2 ()
  ((aaa :initarg :bbb :initform 88)
   (ccc :initarg :ddd :initform 99))
  (:default-initargs :bbb 10))

(deftest defclass-default-initargs.2
  (let ((inst (make-instance 'defargs2)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'ccc)))
  10 99)

(defclass defargs3 ()
  ((aaa :initarg :bbb))
  (:default-initargs :zzz 100))

(deftest-error defclass-default-initargs.3
  (make-instance 'defargs3))


;;  :class default-initargs
(defclass defargs4 ()
  ((aaa :initarg :bbb :allocation :class))
  (:default-initargs :bbb 100))

(deftest defclass-default-initargs.4
  (let ((inst (make-instance 'defargs4)))
    (slot-value inst 'aaa))
  100)

;;  :class default-initargs shadow
(defclass defargs5 (defargs4)
  ((aaa :initarg :bbb :allocation :class))
  (:default-initargs :bbb 200))

(deftest defclass-default-initargs.5
  (let ((inst (make-instance 'defargs5)))
    (slot-value inst 'aaa))
  200)


;;  forward-referenced-class
(defclass forward1 (forward2)
  (aaa bbb))

(deftest forward-referenced-class.1
  (let ((inst (find-class 'forward1)))
    (values
      (closp inst)
      (class-finalized-p inst)
      (class-name
        (class-of
          (car (class-direct-superclasses inst))))))

  t nil lisp-clos::forward-referenced-class)

(deftest-error forward-referenced-class.2
  (make-instance 'forward1))

(defclass forward3 (forward4)
  (aaa bbb))

(defclass forward4 ()
  (ccc))

(deftest forward-referenced-class.3
  (let ((inst (make-instance 'forward3)))
    (values
      (slot-exists-p inst 'aaa)
      (slot-exists-p inst 'bbb)
      (slot-exists-p inst 'ccc)))
  t t t)


;;  documentation
(defclass document1 () ()
  (:documentation "Hello"))

(deftest defclass-documentation.1
  (closp
    (make-instance 'document1))
  t)

(defclass metaclass1 (standard-class) ())
(defclass metaclass2 () ()
  (:metaclass metaclass1))

(deftest defclass-metaclass.1
  t t)


;;  readers
(defclass readers1 ()
  ((aaa :reader readers1-aaa)))

(deftest readers.1
  (let ((inst (make-instance 'readers1)))
    (setf (slot-value inst 'aaa) 100)
    (readers1-aaa inst))
  100)

(defclass readers2 ()
  ((bbb :reader readers1-aaa)))

(deftest readers.2
  (let ((a (make-instance 'readers1))
        (b (make-instance 'readers2)))
    (setf (slot-value a 'aaa) 200)
    (setf (slot-value b 'bbb) 300)
    (values
      (readers1-aaa a)
      (readers1-aaa b)))
  200 300)

(defclass readers3 (readers1)
  ((ccc :reader readers3-ccc)
   (ddd :reader readers3-ddd
        :reader readers3-eee)))

(deftest readers.3
  (let ((inst (make-instance 'readers3)))
    (setf (slot-value inst 'aaa) 111)
    (setf (slot-value inst 'ccc) 222)
    (setf (slot-value inst 'ddd) 333)
    (values
      (readers1-aaa inst)
      (readers3-ccc inst)
      (readers3-ddd inst)
      (readers3-eee inst)))
  111 222 333 333)

(defclass readers4 ()
  ((aaa :allocation :class :reader readers4-aaa)))

(defclass readers5 (readers4)
  ((bbb :allocation :class :reader readers5-bbb)
   (ccc :allocation :class :reader readers5-ccc)))

(deftest readers.4
  (let ((inst (make-instance 'readers5))
        (other (make-instance 'readers5)))
    (setf (slot-value inst 'aaa) 11)
    (setf (slot-value inst 'bbb) 22)
    (setf (slot-value inst 'ccc) 33)
    (values
      (readers4-aaa inst)
      (readers5-bbb inst)
      (readers5-ccc inst)
      (readers4-aaa other)
      (readers5-bbb other)
      (readers5-ccc other)))
  11 22 33 11 22 33)

;;  writers
(defclass writers1 ()
  ((aaa :writer writers1-aaa)))

(deftest writers.1
  (let ((inst (make-instance 'writers1)))
    (values
      (writers1-aaa 100 inst)
      (slot-value inst 'aaa)))
  100 100)

(defclass writers2 ()
  ((bbb :writer writers1-aaa)))

(deftest writers.2
  (let ((a (make-instance 'writers1))
        (b (make-instance 'writers2)))
    (values
      (writers1-aaa 200 a)
      (writers1-aaa 300 b)
      (slot-value a 'aaa)
      (slot-value b 'bbb)))
  200 300 200 300)

(defclass writers3 (writers1)
  ((ccc :writer writers3-ccc)
   (ddd :writer writers3-ddd
        :writer writers3-eee)))

(deftest writers.3
  (let ((inst (make-instance 'writers3)))
    (values
      (writers1-aaa 111 inst)
      (writers3-ccc 222 inst)
      (writers3-ddd 333 inst)
      (writers3-eee 444 inst)
      (slot-value inst 'aaa)
      (slot-value inst 'ccc)
      (slot-value inst 'ddd)))
  111 222 333 444 111 222 444)

;;  accessors
(defclass accessors1 ()
  ((aaa :accessor accessors1-aaa)))

(deftest accessors.1
  (let ((inst (make-instance 'accessors1)))
    (values
      (setf (accessors1-aaa inst) 100)
      (accessors1-aaa inst)))
  100 100)

(defclass accessors2 ()
  ((bbb :accessor accessors1-aaa)))

(deftest accessors.2
  (let ((a (make-instance 'accessors1))
        (b (make-instance 'accessors2)))
    (values
      (setf (accessors1-aaa a) 200)
      (setf (accessors1-aaa b) 300)
      (accessors1-aaa a)
      (accessors1-aaa b)))
  200 300 200 300)

(defclass accessors3 (accessors1)
  ((ccc :accessor accessors3-ccc)
   (ddd :accessor accessors3-ddd
        :accessor accessors3-eee)))

(deftest accessors.3
  (let ((inst (make-instance 'accessors3)))
    (values
      (setf (accessors1-aaa inst) 111)
      (setf (accessors3-ccc inst) 222)
      (setf (accessors3-ddd inst) 333)
      (setf (accessors3-eee inst) 444)
      (accessors1-aaa inst)
      (accessors3-ccc inst)
      (accessors3-ddd inst)
      (accessors3-eee inst)))
  111 222 333 444 111 222 444 444)


;;
;;  allocate-instance
;;
(defclass allocate1 ()
  ((aaa :initarg :aaa)
   (bbb :initarg :bbb :initform 10)
   (ccc :initarg :ccc))
  (:default-initargs :ccc 20))

(deftest allocate-instance.1
  (let* ((class (find-class 'allocate1))
         (inst (allocate-instance class)))
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)
      (slot-boundp inst 'ccc)))
  nil nil nil)

(defclass allocate2 ()
  ((aaa :allocation :class :initarg :aaa)
   (bbb :allocation :class :initarg :bbb :initform 10)
   (ccc :allocation :class :initarg :ccc))
  (:default-initargs :ccc 20))

(deftest allocate-instance.2
  (let* ((class (find-class 'allocate2))
         (inst (allocate-instance class)))
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)
      (slot-boundp inst 'ccc)))
  nil t nil)


;;
;;  initialize-instance
;;
(defclass initialize1 ()
  ((aaa :initarg :aaa)
   (bbb :initarg :bbb :initform 10)
   (ccc :initarg :ccc))
  (:default-initargs :ccc 20))

(deftest initialize-instance.1
  (let* ((class (find-class 'initialize1))
         (inst (allocate-instance class)))
    (typep
      (initialize-instance inst)
      'initialize1))
  t)

(deftest initialize-instance.2
  (let* ((class (find-class 'initialize1))
         (inst (allocate-instance class)))
    (initialize-instance inst)
    (values
      (slot-boundp inst 'aaa)
      (slot-value inst 'bbb)
      (slot-boundp inst 'ccc)))
  nil 10 nil)

(deftest initialize-instance.3
  (let* ((class (find-class 'initialize1))
         (inst (allocate-instance class)))
    (initialize-instance inst :aaa 10 :bbb 20 :ccc 30 :ddd 40 :aaa 50)
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  10 20 30)

(defclass initialize2 (initialize1) ())

(defmethod initialize-instance :after ((inst initialize2) &rest args)
  (declare (ignore args))
  (unless (slot-boundp inst 'ccc)
    (setf (slot-value inst 'ccc) :hello)))

(deftest initialize-instance.4
  (let* ((class (find-class 'initialize2))
         (inst (allocate-instance class)))
    (initialize-instance inst :aaa 10 :bbb 20 :ccc 30 :ddd 40 :aaa 50)
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  10 20 30)

(deftest initialize-instance.5
  (let* ((class (find-class 'initialize2))
         (inst (allocate-instance class)))
    (initialize-instance inst)
    (values
      (slot-boundp inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  nil 10 :hello)


;;
;;  reinitialize-instance
;;
(defclass reinitialize1 ()
  ((aaa :initarg :aaa)
   (bbb :initarg :bbb :initform 10)
   (ccc :initarg :ccc))
  (:default-initargs :ccc 20))

(deftest reinitialize-instance.1
  (let* ((class (find-class 'reinitialize1))
         (inst (allocate-instance class)))
    (typep
      (reinitialize-instance inst)
      'reinitialize1))
  t)

(deftest reinitialize-instance.2
  (let* ((class (find-class 'reinitialize1))
         (inst (allocate-instance class)))
    (reinitialize-instance inst)
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)
      (slot-boundp inst 'ccc)))
  nil nil nil)

(deftest reinitialize-instance.3
  (let* ((class (find-class 'reinitialize1))
         (inst (allocate-instance class)))
    (reinitialize-instance inst :aaa 10 :bbb 20 :ccc 30 :ddd 40 :aaa 50)
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  10 20 30)

(deftest reinitialize-instance.4
  (let* ((class (find-class 'reinitialize1))
         (inst (allocate-instance class)))
    (reinitialize-instance inst :aaa 10)
    (values
      (slot-value inst 'aaa)
      (slot-boundp inst 'bbb)
      (slot-boundp inst 'ccc)))
  10 nil nil)

(defclass reinitialize2 (reinitialize1) ())

(defmethod reinitialize-instance :after ((inst reinitialize2) &rest args)
  (declare (ignore args))
  (unless (slot-boundp inst 'ccc)
    (setf (slot-value inst 'ccc) :hello)))

(deftest reinitialize-instance.5
  (let* ((class (find-class 'reinitialize2))
         (inst (allocate-instance class)))
    (reinitialize-instance inst :aaa 10 :bbb 20 :ccc 30 :ddd 40 :aaa 50)
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  10 20 30)

(deftest reinitialize-instance.6
  (let* ((class (find-class 'reinitialize2))
         (inst (allocate-instance class)))
    (reinitialize-instance inst)
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)
      (slot-value inst 'ccc)))
  nil nil :hello)


;;
;;  shared-initialize
;;
(defclass shared1 ()
  ((aaa :initarg :aaa)
   (bbb :initarg :bbb :initform 10)
   (ccc :initarg :ccc)
   (ddd :allocation :class :initarg :ddd :initarg :zzz)
   (eee :allocation :class :initarg :eee :initform 20)
   (fff :allocation :class :initarg :ddd :initform 30))
  (:default-initargs :ccc 40 :fff 50))

(deftest shared-initialize.1
  (let* ((class (find-class 'shared1))
         (inst (allocate-instance class)))
    (shared-initialize inst '(bbb eee))
    (values
      (slot-boundp inst 'aaa)
      (slot-value inst 'bbb)
      (slot-boundp inst 'ccc)
      (slot-boundp inst 'ddd)
      (slot-value inst 'eee)
      (slot-value inst 'fff)))
  nil 10 nil nil 20 30)

(deftest shared-initialize.2
  (let* ((class (find-class 'shared1))
         (inst (allocate-instance class)))
    (shared-initialize inst '(bbb) :bbb 111 :ccc 222)
    (values
      (slot-boundp inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  nil 111 222)

(deftest shared-initialize.3
  (let* ((class (find-class 'shared1))
         (inst (allocate-instance class)))
    (shared-initialize inst t :bbb 111 :ccc 222 :zzz 333)
    (values
      (slot-boundp inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)
      (slot-value inst 'ddd)))
  nil 111 222 333)

