;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  Macro DEFCLASS
;;
(deftest defclass.1
  (closp
    (defclass defclass-1 () ()))
  t)

(deftest defclass.2
  (closp
    (find-class 'defclass-1 nil))
  t)

(deftest defclass.3
  (class-name
    (find-class 'defclass-1))
  defclass-1)

(deftest defclass.4
  (values
    (subtypep 'defclass-1 t)
    (subtypep 'defclass-1 'standard-object)
    (subtypep 'defclass-1 'standard-class))
  t t nil)

(deftest defclass.5
  (progn
    (defclass defclass-2 () ())
    (defclass defclass-3 (defclass-2) ())
    (values)))

(deftest defclass.6
  (values
    (subtypep 'defclass-2 'defclass-3)
    (subtypep 'defclass-3 'defclass-2)
    (subtypep 'defclass-3 'standard-object)
    (subtypep 'defclass-3 t))
  nil t t t)

(deftest defclass.7
  (class-name
    (find-class 'defclass-3))
  defclass-3)

(deftest defclass.8
  (progn
    (defclass defclass-4 () (hello))
    (values)))

(deftest defclass.9
  (let ((inst (make-instance 'defclass-4)))
    (slot-boundp inst 'hello))
  nil)

(deftest defclass.10
  (let ((inst (make-instance 'defclass-4)))
    (setf (slot-value inst 'hello) 100)
    (slot-boundp inst 'hello))
  t)


;;
;;  initarg
;;
(deftest defclass-initarg.1
  (progn
    (defclass defclass-initarg-1 ()
      ((aaa :initarg :bbb)
       (ccc :initarg :ddd)))
    (defclass defclass-initarg-2 (defclass-initarg-1)
      ((eee :initarg :fff)))
    (values)))

(deftest defclass-initarg.2
  (let ((inst (make-instance 'defclass-initarg-2)))
    (values
      (slot-exists-p inst 'aaa)
      (slot-exists-p inst 'ccc)
      (slot-exists-p inst 'eee)))
  t t t)

(deftest defclass-initarg.3
  (let ((inst (make-instance 'defclass-initarg-2 :bbb 10 :ddd 20 :fff 30)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'ccc)
      (slot-value inst 'eee)))
  10 20 30)

(deftest defclass-initarg.4
  (progn
    (defclass defclass-initarg-3 (defclass-initarg-1)
      ((eee :initarg :bbb)))
    (values)))

(deftest defclass-initarg.5
  (let ((inst (make-instance 'defclass-initarg-3 :bbb 10 :ddd 20)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'ccc)
      (slot-value inst 'eee)))
  10 20 10)

(deftest defclass-initarg.6
  (progn
    (defclass defclass-initarg-4 ()
      ((aaa :initarg :aaa :initarg :bbb)
       (bbb :initarg :aaa :initarg :ccc)
       (ccc :initarg :aaa)))
    (values)))

(deftest defclass-initarg.7
  (let ((inst (make-instance 'defclass-initarg-4 :aaa 10)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  10 10 10)

(deftest defclass-initarg.8
  (let ((inst (make-instance 'defclass-initarg-4 :ccc 20)))
    (slot-value inst 'bbb))
  20)


;;
;;  initform
;;
(deftest defclass-initform.1
  (progn
    (let ((value 0))
      (defun defclass-initform-call (&optional x)
        (if x
          (setq value x)
          (prog1 value
            (incf value)))))
    (defclass defclass-initform-1 ()
      ((aaa :initarg :bbb :initform 100)
       (ccc :initarg :ddd :initform (defclass-initform-call))))
    (values)))

(deftest defclass-initform.2
  (let ((inst1 (make-instance 'defclass-initform-1))
        (inst2 (make-instance 'defclass-initform-1)))
    (values
      (slot-value inst1 'aaa)
      (slot-value inst1 'ccc)
      (slot-value inst2 'aaa)
      (slot-value inst2 'ccc)))
  100 0 100 1)

(deftest defclass-initform.3
  (let ((inst1 (make-instance 'defclass-initform-1 :bbb 11 :ddd 22))
        (inst2 (make-instance 'defclass-initform-1 :bbb 33)))
    (values
      (slot-value inst1 'aaa)
      (slot-value inst1 'ccc)
      (slot-value inst2 'aaa)
      (slot-value inst2 'ccc)
      (defclass-initform-call)))
  11 22 33 2 3)

(deftest defclass-initform.4
  (progn
    (defclass defclass-initform-2 (defclass-initform-1)
      ((aaa :initarg :bbb :initform 200)
       (eee :initarg :fff :initform 333)))
    (values)))

(deftest defclass-initform.5
  (let ((inst1 (make-instance 'defclass-initform-2))
        (inst2 (make-instance 'defclass-initform-2)))
    (values
      (slot-value inst1 'aaa)
      (slot-value inst1 'ccc)
      (slot-value inst1 'eee)
      (slot-value inst2 'aaa)
      (slot-value inst2 'ccc)
      (slot-value inst2 'eee)))
  200 4 333 200 5 333)

(deftest defclass-initform.6
  (let ((inst1 (make-instance 'defclass-initform-2 :bbb 11 :ddd 22))
        (inst2 (make-instance 'defclass-initform-2 :fff 33)))
    (values
      (slot-value inst1 'aaa)
      (slot-value inst1 'ccc)
      (slot-value inst1 'eee)
      (slot-value inst2 'aaa)
      (slot-value inst2 'ccc)
      (slot-value inst2 'eee)
      (defclass-initform-call)))
  11 22 333 200 6 33 7)


;;
;;  :class instance
;;
(deftest defclass-class.1
  (progn
    (defclass defclass-class-1 ()
      ((aaa :initarg :aaa :allocation :class)
       (bbb :initarg :bbb :allocation :instance)
       (ccc :initarg :ccc :allocation :class)
       (ddd :initarg :bbb :allocation :instance)))
    (values)))

(deftest defclass-class.2
  (let ((inst1 (make-instance 'defclass-class-1))
        (inst2 (make-instance 'defclass-class-1)))
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

(deftest defclass-class.3
  (progn
    (defclass defclass-class-2 ()
      ((aaa :initarg :aaa :allocation :class)
       (bbb :initarg :bbb :allocation :instance)
       (ccc :initarg :ccc :allocation :class)
       (ddd :initarg :bbb :allocation :instance)))
    (values)))

(deftest defclass-class.4
  (let ((inst1 (make-instance 'defclass-class-2))
        (inst2 (make-instance 'defclass-class-2)))
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

(deftest defclass-class.5
  (progn
    (defclass declass-class-3 ()
      ((aaa :initarg :aaa :allocation :class)
       (bbb :initarg :bbb :allocation :instance)
       (ccc :initarg :ccc :allocation :class)
       (ddd :initarg :bbb :allocation :instance)))
    (values)))

(deftest defclass-class.6
  (let ((inst1 (make-instance 'declass-class-3))
        (inst2 (make-instance 'declass-class-3)))
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

(deftest defclass-class.7
  (progn
    (defclass defclass-class-4 (defclass-class-1)
      ((ccc :initarg :ccc :allocation :class)
       (ddd :initarg :ddd :allocation :instance)
       (eee :initarg :eee :allocation :class)
       (fff :initarg :fff :allocation :instance)))
    (values)))

(deftest defclass-class.8
  (let ((inst1 (make-instance 'defclass-class-1))
        (inst2 (make-instance 'defclass-class-4)))
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

(deftest defclass-class.9
  (progn
    (defclass defclass-class-5 (defclass-class-1)
      ((eee :initarg :eee :allocation :class)
       (fff :initarg :fff :allocation :instance)))
    (values)))

(deftest defclass-class.10
  (let ((inst1 (make-instance 'defclass-class-1))
        (inst2 (make-instance 'defclass-class-5)))
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


;;
;; initargs
;;
(deftest defclass-initargs.1
  (progn
    (defclass defclass-initargs-1 ()
      ((aaa :initarg :aaa :allocation :class)
       (bbb :initarg :bbb :allocation :class)
       (ccc :initarg :ccc)))
    (defclass defclass-initargs-2 (defclass-initargs-1)
      ((bbb :initarg :ddd :allocation :class)
       (eee :initarg :eee)))
    (values)))

(deftest defclass-initargs.2
  (let ((inst1 (make-instance 'defclass-initargs-1))
        (inst2 (make-instance 'defclass-initargs-2)))
    (values
      (slot-boundp inst1 'aaa)
      (slot-boundp inst1 'bbb)
      (slot-boundp inst1 'ccc)
      (slot-boundp inst2 'bbb)
      (slot-boundp inst2 'eee)))
  nil nil nil nil nil)

(deftest defclass-initargs.3
  (let ((inst (make-instance 'defclass-initargs-1 :aaa 10 :bbb 20 :ccc 30)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  10 20 30)

(deftest defclass-initargs.4
  (let ((inst1 (make-instance 'defclass-initargs-1
                              :aaa 10 :bbb 20 :ccc 30))
        (inst2 (make-instance 'defclass-initargs-2
                              :aaa 40 :bbb 50 :ccc 60 :ddd 70 :eee 80)))
    (values
      (slot-value inst1 'aaa)
      (slot-value inst1 'bbb)
      (slot-value inst1 'ccc)
      (slot-value inst2 'aaa)
      (slot-value inst2 'bbb)
      (slot-value inst2 'ccc)
      (slot-value inst2 'eee)))
  40 20 30 40 50 60 80)

(deftest defclass-initargs.5
  (let ((inst (make-instance 'defclass-initargs-1 :aaa 10 :bbb 20 :aaa 30)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  10 20)



;;
;;  initform-init
;;
(deftest defclass-initform-init.1
  (progn
    (defclass defclass-initform-init-1 ()
      ((aaa :initarg :aaa :initform 100)))
    (values)))

(deftest defclass-initform-init.2
  (let ((inst (make-instance 'defclass-initform-init-1)))
    (slot-value inst 'aaa))
  100)

(deftest defclass-initform-init.3
  (let ((inst (make-instance 'defclass-initform-init-1 :aaa 200)))
    (slot-value inst 'aaa))
  200)

(deftest defclass-initform-init.4
  (progn
    (defclass defclass-initform-init-2 (defclass-initform-init-1)
      ((bbb :initarg :bbb :initform 200)))
    (values)))

(deftest defclass-initform-init.5
  (let ((inst (make-instance 'defclass-initform-init-2)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  100 200)

(deftest defclass-initform-init.6
  (let ((inst (make-instance 'defclass-initform-init-2 :bbb 333 :aaa 444)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  444 333)

(deftest defclass-initform-init.7
  (progn
    (defclass defclass-initform-init-3 ()
      ((aaa :initarg :aaa :initform 10 :allocation :class)
       (bbb :initarg :bbb :initform 20 :allocation :class)))
    (values)))

(deftest defclass-initform-init.8
  (let ((inst (make-instance 'defclass-initform-init-3)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (progn
        (setf (slot-value inst 'bbb) 999)
        (slot-value (make-instance 'defclass-initform-init-3) 'bbb))))
  10 20 999)

(deftest defclass-initform-init.9
  (let ((inst (make-instance 'defclass-initform-init-3 :aaa 44 :bbb 55)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (progn
        (setf (slot-value inst 'bbb) 999)
        (slot-value (make-instance 'defclass-initform-init-3) 'bbb))))
  44 55 999)

(deftest defclass-initform-init.10
  (progn
    (defclass defclass-initform-init-4 (defclass-initform-init-3)
      ((bbb :initarg :bbb :initform 30 :allocation :class)
       (ccc :initarg :ccc :initform 40 :allocation :class)))
    (values)))

(deftest defclass-initform-init.11
  (let ((inst (make-instance 'defclass-initform-init-4 :aaa 11 :bbb 22 :ccc 33)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  11 22 33)


;;
;;  default-initargs
;;
(deftest defclass-default-initargs.1
  (progn
    (defclass defclass-default-initargs-1 ()
      ((aaa :initarg :bbb)
       (ccc :initarg :ddd))
      (:default-initargs :bbb 10 :ddd 20))
    (values)))

(deftest defclass-default-initargs.2
  (let ((inst (make-instance 'defclass-default-initargs-1)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'ccc)))
  10 20)

(deftest defclass-default-initargs.3
  (progn
    (defclass defclass-default-initargs-2 ()
      ((aaa :initarg :bbb :initform 88)
       (ccc :initarg :ddd :initform 99))
      (:default-initargs :bbb 10))
    (values)))

(deftest defclass-default-initargs.4
  (let ((inst (make-instance 'defclass-default-initargs-2)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'ccc)))
  10 99)

(deftest defclass-default-initargs.5
  (progn
    (defclass defclass-default-initargs-3 ()
      ((aaa :initarg :bbb))
      (:default-initargs :zzz 100))
    (values)))

(deftest-error defclass-default-initargs.6
  (make-instance 'defclass-default-initargs-3))


;;
;;  :class default-initargs
;;
(deftest defclass-default-initargs.7
  (progn
    (defclass defclass-default-initargs-4 ()
      ((aaa :initarg :bbb :allocation :class))
      (:default-initargs :bbb 100))
    (values)))

(deftest defclass-default-initargs.8
  (let ((inst (make-instance 'defclass-default-initargs-4)))
    (slot-value inst 'aaa))
  100)


;;
;;  :class default-initargs shadow
;;
(deftest defclass-default-initargs.9
  (progn
    (defclass defclass-default-initargs-5 (defclass-default-initargs-4)
      ((aaa :initarg :bbb :allocation :class))
      (:default-initargs :bbb 200))
    (values)))

(deftest defclass-default-initargs.10
  (let ((inst (make-instance 'defclass-default-initargs-5)))
    (slot-value inst 'aaa))
  200)


;;
;;  forward-referenced-class
;;
(deftest forward-referenced-class.1
  (progn
    (defclass forward-referenced-class-1 (forward-referenced-class-2)
      (aaa bbb))
    (values)))

(deftest forward-referenced-class.2
  (let ((inst (find-class 'forward-referenced-class-1)))
    (values
      (closp inst)
      (class-finalized-p inst)
      (class-name
        (class-of
          (car (class-direct-superclasses inst))))))
  t nil lisp-clos::forward-referenced-class)

(deftest-error forward-referenced-class.3
  (make-instance 'forward-referenced-class-1))

(deftest forward-referenced-class.4
  (progn
    (defclass forward-referenced-class-3 (forward-referenced-class-4)
      (aaa bbb))
    (defclass forward-referenced-class-4 ()
      (ccc))
    (values)))

(deftest forward-referenced-class.5
  (let ((inst (make-instance 'forward-referenced-class-3)))
    (values
      (slot-exists-p inst 'aaa)
      (slot-exists-p inst 'bbb)
      (slot-exists-p inst 'ccc)))
  t t t)


;;
;;  documentation
;;
(deftest defclass-documentation.1
  (progn
    (defclass defclass-document-1 () ()
      (:documentation "Hello"))
    (values)))

(deftest defclass-documentation.2
  (closp
    (make-instance 'defclass-document-1))
  t)


;;
;;  metaclass
;;
(deftest defclass-metaclass.1
  (progn
    (defclass defclass-metaclass-1 (standard-class) ())
    (defclass defclass-metaclass-2 () ()
      (:metaclass defclass-metaclass-1))
    (values)))


;;
;;  readers
;;
(deftest defclass-readers.1
  (progn
    (defclass defclass-readers-1 ()
      ((aaa :reader defclass-readers-1-aaa)))
    (values)))

(deftest defclass-readers.2
  (let ((inst (make-instance 'defclass-readers-1)))
    (setf (slot-value inst 'aaa) 100)
    (defclass-readers-1-aaa inst))
  100)

(deftest defclass-readers.3
  (progn
    (defclass defclass-readers-2 ()
      ((bbb :reader defclass-readers-1-aaa)))
    (values)))

(deftest defclass-readers.4
  (let ((a (make-instance 'defclass-readers-1))
        (b (make-instance 'defclass-readers-2)))
    (setf (slot-value a 'aaa) 200)
    (setf (slot-value b 'bbb) 300)
    (values
      (defclass-readers-1-aaa a)
      (defclass-readers-1-aaa b)))
  200 300)

(deftest defclass-readers.5
  (progn
    (defclass defclass-readers-3 (defclass-readers-1)
      ((ccc :reader defclass-readers-3-ccc)
       (ddd :reader defclass-readers-3-ddd
            :reader defclass-readers-3-eee)))
    (values)))

(deftest defclass-readers.6
  (let ((inst (make-instance 'defclass-readers-3)))
    (setf (slot-value inst 'aaa) 111)
    (setf (slot-value inst 'ccc) 222)
    (setf (slot-value inst 'ddd) 333)
    (values
      (defclass-readers-1-aaa inst)
      (defclass-readers-3-ccc inst)
      (defclass-readers-3-ddd inst)
      (defclass-readers-3-eee inst)))
  111 222 333 333)

(deftest defclass-readers.7
  (progn
    (defclass defclass-readers-4 ()
      ((aaa :allocation :class :reader defclass-readers-4-aaa)))
    (defclass defclass-readers-5 (defclass-readers-4)
      ((bbb :allocation :class :reader defclass-readers-5-bbb)
       (ccc :allocation :class :reader defclass-readers-5-ccc)))
    (values)))

(deftest defclass-readers.8
  (let ((inst (make-instance 'defclass-readers-5))
        (other (make-instance 'defclass-readers-5)))
    (setf (slot-value inst 'aaa) 11)
    (setf (slot-value inst 'bbb) 22)
    (setf (slot-value inst 'ccc) 33)
    (values
      (defclass-readers-4-aaa inst)
      (defclass-readers-5-bbb inst)
      (defclass-readers-5-ccc inst)
      (defclass-readers-4-aaa other)
      (defclass-readers-5-bbb other)
      (defclass-readers-5-ccc other)))
  11 22 33 11 22 33)

;;
;;  writers
;;
(deftest defclass-writers.1
  (progn
    (defclass defclass-writers-1 ()
      ((aaa :writer defclass-writers-1-aaa)))
    (values)))

(deftest defclass-writers.2
  (let ((inst (make-instance 'defclass-writers-1)))
    (values
      (defclass-writers-1-aaa 100 inst)
      (slot-value inst 'aaa)))
  100 100)

(deftest defclass-writers.3
  (progn
    (defclass defclass-writers-2 ()
      ((bbb :writer defclass-writers-1-aaa)))
    (values)))

(deftest defclass-writers.4
  (let ((a (make-instance 'defclass-writers-1))
        (b (make-instance 'defclass-writers-2)))
    (values
      (defclass-writers-1-aaa 200 a)
      (defclass-writers-1-aaa 300 b)
      (slot-value a 'aaa)
      (slot-value b 'bbb)))
  200 300 200 300)

(deftest defclass-writers.5
  (progn
    (defclass defclass-writers-3 (defclass-writers-1)
      ((ccc :writer defclass-writers-3-ccc)
       (ddd :writer defclass-writers-3-ddd
            :writer defclass-writers-3-eee)))
    (values)))

(deftest defclass-writers.6
  (let ((inst (make-instance 'defclass-writers-3)))
    (values
      (defclass-writers-1-aaa 111 inst)
      (defclass-writers-3-ccc 222 inst)
      (defclass-writers-3-ddd 333 inst)
      (defclass-writers-3-eee 444 inst)
      (slot-value inst 'aaa)
      (slot-value inst 'ccc)
      (slot-value inst 'ddd)))
  111 222 333 444 111 222 444)


;;
;;  accessors
;;
(deftest defclass-accessors.1
  (progn
    (defclass defclass-accessors-1 ()
      ((aaa :accessor defclass-accessors-1-aaa)))
    (values)))

(deftest defclass-accessors.2
  (let ((inst (make-instance 'defclass-accessors-1)))
    (values
      (setf (defclass-accessors-1-aaa inst) 100)
      (defclass-accessors-1-aaa inst)))
  100 100)

(deftest defclass-accessors.3
  (progn
    (defclass defclass-accessors-2 ()
      ((bbb :accessor defclass-accessors-1-aaa)))
    (values)))

(deftest defclass-accessors.4
  (let ((a (make-instance 'defclass-accessors-1))
        (b (make-instance 'defclass-accessors-2)))
    (values
      (setf (defclass-accessors-1-aaa a) 200)
      (setf (defclass-accessors-1-aaa b) 300)
      (defclass-accessors-1-aaa a)
      (defclass-accessors-1-aaa b)))
  200 300 200 300)

(deftest defclass-accessors.5
  (progn
    (defclass defclass-accessors-3 (defclass-accessors-1)
      ((ccc :accessor defclass-accessors-3-ccc)
       (ddd :accessor defclass-accessors-3-ddd
            :accessor defclass-accessors-3-eee)))
    (values)))

(deftest defclass-accessors.6
  (let ((inst (make-instance 'defclass-accessors-3)))
    (values
      (setf (defclass-accessors-1-aaa inst) 111)
      (setf (defclass-accessors-3-ccc inst) 222)
      (setf (defclass-accessors-3-ddd inst) 333)
      (setf (defclass-accessors-3-eee inst) 444)
      (defclass-accessors-1-aaa inst)
      (defclass-accessors-3-ccc inst)
      (defclass-accessors-3-ddd inst)
      (defclass-accessors-3-eee inst)))
  111 222 333 444 111 222 444 444)


;;
;;  error
;;
(deftest-error defclass-error.1
  (eval '(defclass defclass-error-1 ())))

(deftest-error defclass-error.2
  (eval '(defclass defclass-error-2 (10) ())))

(deftest-error defclass-error.3
  (eval '(defclass defclass-error-3 () (20))))

;;  program-error
(deftest-error defclass-program-error.1
  (eval '(defclass defclass-program-error-1 () (aaa aaa)))
  program-error)

(deftest-error defclass-program-error.2
  (eval '(defclass defclass-prognram-error-2 () ()
           (:default-initargs :aaa 10 :bbb 20 :aaa 30)))
  program-error)

(deftest-error defclass-program-error.3
  (eval '(defclass defclass-prognram-error-3 ()
           ((aaa :allocation :class :allocation :instance))))
  program-error)

(deftest-error defclass-program-error.4
  (eval '(defclass defclass-prognram-error-4 ()
           ((aaa :initform 10 :initform 20))))
  program-error)

(deftest-error defclass-program-error.5
  (eval '(defclass defclass-prognram-error-5 ()
           ((aaa :type integer :type string))))
  program-error)

(deftest-error defclass-program-error.6
  (eval '(defclass defclass-prognram-error-6 ()
           ((aaa :documentation "Hello" :documentation "ABC"))))
  program-error)

(deftest-error defclass-program-error.7
  (eval '(defclass defclass-prognram-error-7 ()
           ((aaa :no-such-slot-option 10))))
  program-error)

(deftest-error defclass-program-error.8
  (eval '(defclass defclass-prognram-error-8 () ()
           (:no-such-class-option 10)))
  program-error)


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

