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
    :name)
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
    :name)
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
      (slot-exists-p inst :name)
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
  40 20 30 40 70 60 80)

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
  (:documenation "Hello"))

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
;;  writers

;;  redefine
;;    slot-value
;;    slot-boundp
;;    slot-exists-p
;;    slot-makunbound
;;  change-class

