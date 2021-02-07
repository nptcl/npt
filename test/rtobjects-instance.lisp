;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  Standard Generic Function ALLOCATE-INSTANCE
;;
(deftest allocate-instance.1
  (progn
    (defclass allocate-instance-1 ()
      ((aaa :initarg :aaa :initform 100)
       bbb))
    (values)))

(deftest allocate-instance.2
  (typep (allocate-instance
           (find-class 'allocate-instance-1))
         'allocate-instance-1)
  t)

(deftest allocate-instance.3
  (let ((inst (allocate-instance
                (find-class 'allocate-instance-1))))
    (values (slot-boundp inst 'aaa)
            (slot-boundp inst 'bbb)))
  nil nil)

(deftest allocate-instance.4
  (let ((inst (allocate-instance
                (find-class 'allocate-instance-1)
                :aaa 11
                :bbb 22
                :ccc 33)))
    (values (slot-boundp inst 'aaa)
            (slot-boundp inst 'bbb)))
  nil nil)

;;  structure-class
(deftest allocate-instance.5
  (progn
    (defstruct allocate-instance-2
      (aaa 999) bbb)
    (values)))

(deftest allocate-instance.6
  (let ((inst (allocate-instance
                (find-class 'allocate-instance-2))))
    (values (slot-value inst 'aaa)
            (slot-value inst 'bbb)))
  nil nil)

(deftest allocate-instance.7
  (let ((inst (allocate-instance
                (find-class 'allocate-instance-2)
                :aaa 10 :bbb 20 :ccc 30)))
    (values (slot-value inst 'aaa)
            (slot-value inst 'bbb)))
  nil nil)

(deftest allocate-instance.8
  (progn
    (defclass allocate-instance-3 ()
      ((aaa :initarg :aaa)
       (bbb :initarg :bbb :initform 10)
       (ccc :initarg :ccc))
      (:default-initargs :ccc 20))
    (let* ((class (find-class 'allocate-instance-3))
           (inst (allocate-instance class)))
      (values
        (slot-boundp inst 'aaa)
        (slot-boundp inst 'bbb)
        (slot-boundp inst 'ccc))))
  nil nil nil)

(deftest allocate-instance.9
  (progn
    (defclass allocate-instance-4 ()
      ((aaa :allocation :class :initarg :aaa)
       (bbb :allocation :class :initarg :bbb :initform 10)
       (ccc :allocation :class :initarg :ccc))
      (:default-initargs :ccc 20))
    (let* ((class (find-class 'allocate-instance-4))
           (inst (allocate-instance class)))
      (values
        (slot-boundp inst 'aaa)
        (slot-boundp inst 'bbb)
        (slot-boundp inst 'ccc))))
  nil t nil)

(deftest-error! allocate-instance-error.1
  (eval '(allocate-instance)))

(deftest-error! allocate-instance-error.2
  (eval '(allocate-instance (find-class 'allocate-instance-1) :hello)))

(deftest-error allocate-instance-error.3
  (eval '(allocate-instance 'allocate-instance-1)))


;;
;;  Standard Generic Function SHARED-INITIALIZE
;;
(deftest shared-initialize.1
  (progn
    (defclass shared-initialize-1 () ())
    (let ((inst (make-instance 'shared-initialize-1)))
      (typep
        (shared-initialize inst t)
        'shared-initialize-1)))
  t)

(deftest shared-initialize.2
  (progn
    (defclass shared-initialize-2 ()
      ((aaa :initarg :aaa :initform 10)
       (bbb :initarg :bbb :initform 20)
       (ccc :initarg :ccc)))
    (values)))

(deftest shared-initialize.3
  (let ((inst (allocate-instance (find-class 'shared-initialize-2))))
    (shared-initialize inst t)
    (values (slot-value inst 'aaa)
            (slot-value inst 'bbb)
            (slot-boundp inst 'ccc)))
  10 20 nil)

(deftest shared-initialize.4
  (let ((inst (allocate-instance (find-class 'shared-initialize-2))))
    (shared-initialize inst nil)
    (values (slot-boundp inst 'aaa)
            (slot-boundp inst 'bbb)
            (slot-boundp inst 'ccc)))
  nil nil nil)

(deftest shared-initialize.5
  (let ((inst (allocate-instance (find-class 'shared-initialize-2))))
    (shared-initialize inst '(bbb ccc))
    (values (slot-boundp inst 'aaa)
            (slot-boundp inst 'bbb)
            (slot-boundp inst 'ccc)))
  nil t nil)

(deftest shared-initialize.6
  (let ((inst (allocate-instance (find-class 'shared-initialize-2))))
    (shared-initialize inst nil :aaa 111 :bbb 222 :ccc 333 :ddd 444)
    (values (slot-value inst 'aaa)
            (slot-value inst 'bbb)
            (slot-value inst 'ccc)))
  111 222 333)

(deftest shared-initialize.7
  (let ((inst (allocate-instance (find-class 'shared-initialize-2))))
    (shared-initialize inst t :aaa 111 :bbb 222 :ccc 333 :ddd 444)
    (values (slot-value inst 'aaa)
            (slot-value inst 'bbb)
            (slot-value inst 'ccc)))
  111 222 333)

(deftest shared-initialize.8
  (progn
    (defclass shared-initialize-3 ()
      ((aaa :initarg :aaa :initform 10)
       (bbb :initarg :bbb :initform 20)
       (ccc :initarg :ccc)))
    (defmethod shared-initialize :before
      ((inst shared-initialize-3) list &rest initargs)
      (declare (ignore list initargs))
      (setf (slot-value inst 'bbb) "Hello"))
    (values)))

(deftest shared-initialize.9
  (let ((inst (allocate-instance (find-class 'shared-initialize-3))))
    (shared-initialize inst t)
    (values (slot-value inst 'aaa)
            (slot-value inst 'bbb)
            (slot-boundp inst 'ccc)))
  10 "Hello" nil)

(deftest shared-initialize.10
  (progn
    (defclass shared-initialize-4 ()
      ((aaa :initarg :aaa)
       (bbb :initarg :bbb :initform 10)
       (ccc :initarg :ccc)
       (ddd :allocation :class :initarg :ddd :initarg :zzz)
       (eee :allocation :class :initarg :eee :initform 20)
       (fff :allocation :class :initarg :ddd :initform 30))
      (:default-initargs :ccc 40 :fff 50))
    (values)))

(deftest shared-initialize.11
  (let* ((class (find-class 'shared-initialize-4))
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

(deftest shared-initialize.12
  (let* ((class (find-class 'shared-initialize-4))
         (inst (allocate-instance class)))
    (shared-initialize inst '(bbb) :bbb 111 :ccc 222)
    (values
      (slot-boundp inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  nil 111 222)

(deftest shared-initialize.13
  (let* ((class (find-class 'shared-initialize-4))
         (inst (allocate-instance class)))
    (shared-initialize inst t :bbb 111 :ccc 222 :zzz 333)
    (values
      (slot-boundp inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)
      (slot-value inst 'ddd)))
  nil 111 222 333)

(deftest-error! shared-initialize-error.1
  (eval '(shared-initialize
           (allocate-instance
             (find-class 'shared-initialize-1)))))

(deftest-error shared-initialize-error.2
  (eval '(shared-initialize
           (allocate-instance
             (find-class 'shared-initialize-1))
           100))
  type-error)

(deftest-error shared-initialize-error.3
  (eval '(shared-initialize
           (allocate-instance
             (find-class 'shared-initialize-1))
           nil :hello)))


;;
;;  Standard Generic Function INITIALIZE-INSTANCE
;;
;;  (defmethod initialize-instance ((instance standard-object) &rest initargs)
;;    (apply #'shared-initialize instance t initargs))
;;
(deftest initialize-instance.1
  (progn
    (defclass initialize-instance-1 ()
      ((aaa :initarg :aaa)
       (bbb :initarg :bbb :initform 10)
       (ccc :initarg :ccc))
      (:default-initargs :ccc 20))
    (values)))

(deftest initialize-instance.2
  (let* ((class (find-class 'initialize-instance-1))
         (inst (allocate-instance class)))
    (typep
      (initialize-instance inst)
      'initialize-instance-1))
  t)

(deftest initialize-instance.3
  (let* ((class (find-class 'initialize-instance-1))
         (inst (allocate-instance class)))
    (initialize-instance inst)
    (values
      (slot-boundp inst 'aaa)
      (slot-value inst 'bbb)
      (slot-boundp inst 'ccc)))
  nil 10 nil)

(deftest initialize-instance.4
  (let* ((class (find-class 'initialize-instance-1))
         (inst (allocate-instance class)))
    (initialize-instance inst :aaa 10 :bbb 20 :ccc 30 :ddd 40 :aaa 50)
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  10 20 30)

(deftest initialize-instance.5
  (progn
    (defclass initialize-instance-2 (initialize-instance-1) ())
    (defmethod initialize-instance :after ((inst initialize-instance-2) &rest args)
      (declare (ignore args))
      (unless (slot-boundp inst 'ccc)
        (setf (slot-value inst 'ccc) :hello)))
    (values)))

(deftest initialize-instance.6
  (let* ((class (find-class 'initialize-instance-2))
         (inst (allocate-instance class)))
    (initialize-instance inst :aaa 10 :bbb 20 :ccc 30 :ddd 40 :aaa 50)
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  10 20 30)

(deftest initialize-instance.7
  (let* ((class (find-class 'initialize-instance-2))
         (inst (allocate-instance class)))
    (initialize-instance inst)
    (values
      (slot-boundp inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  nil 10 :hello)

(deftest-error! initialize-instance-error.1
  (eval '(initialize-instance)))

(deftest-error! initialize-instance-error.2
  (eval '(initialize-instance (find-class 'initialize-instance-1) :hello)))

(deftest-error initialize-instance-error.3
  (eval '(initialize-instance 'initialize-instance-1)))


;;
;;  Standard Generic Function REINITIALIZE-INSTANCE
;;
;;  (defmethod reinitialize-instance ((instance standard-object) &rest initargs)
;;    (apply #'shared-initialize instance nil initargs))
;;
(deftest reinitialize-instance.1
  (progn
    (defclass reinitialize-instance-1 ()
      ((aaa :initarg :aaa)
       (bbb :initarg :bbb :initform 10)
       (ccc :initarg :ccc))
      (:default-initargs :ccc 20))
    (values)))

(deftest reinitialize-instance.2
  (let* ((class (find-class 'reinitialize-instance-1))
         (inst (allocate-instance class)))
    (typep
      (reinitialize-instance inst)
      'reinitialize-instance-1))
  t)

(deftest reinitialize-instance.3
  (let* ((class (find-class 'reinitialize-instance-1))
         (inst (allocate-instance class)))
    (reinitialize-instance inst)
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)
      (slot-boundp inst 'ccc)))
  nil nil nil)

(deftest reinitialize-instance.4
  (let* ((class (find-class 'reinitialize-instance-1))
         (inst (allocate-instance class)))
    (reinitialize-instance inst :aaa 10 :bbb 20 :ccc 30 :ddd 40 :aaa 50)
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  10 20 30)

(deftest reinitialize-instance.5
  (let* ((class (find-class 'reinitialize-instance-1))
         (inst (allocate-instance class)))
    (reinitialize-instance inst :aaa 10)
    (values
      (slot-value inst 'aaa)
      (slot-boundp inst 'bbb)
      (slot-boundp inst 'ccc)))
  10 nil nil)

(deftest reinitialize-instance.6
  (progn
    (defclass reinitialize-instance-2 (reinitialize-instance-1) ())
    (defmethod reinitialize-instance :after ((inst reinitialize-instance-2) &rest args)
      (declare (ignore args))
      (unless (slot-boundp inst 'ccc)
        (setf (slot-value inst 'ccc) :hello)))
    (values)))

(deftest reinitialize-instance.7
  (let* ((class (find-class 'reinitialize-instance-2))
         (inst (allocate-instance class)))
    (reinitialize-instance inst :aaa 10 :bbb 20 :ccc 30 :ddd 40 :aaa 50)
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  10 20 30)

(deftest reinitialize-instance.8
  (let* ((class (find-class 'reinitialize-instance-2))
         (inst (allocate-instance class)))
    (reinitialize-instance inst)
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)
      (slot-value inst 'ccc)))
  nil nil :hello)

(deftest-error! reinitialize-instance-error.1
  (eval '(reinitialize-instance)))

(deftest-error! reinitialize-instance-error.2
  (eval '(reinitialize-instance (find-class 'reinitialize-instance-1) :hello)))

(deftest-error reinitialize-instance-error.3
  (eval '(reinitialize-instance 'reinitialize-instance-1)))


;;
;;  Standard Generic Function MAKE-INSTANCE
;;
;; (defmethod make-instance ((class standard-class) &rest initargs)
;;   ...
;;   (let ((instance (apply #'allocate-instance class initargs)))
;;     (apply #'initialize-instance instance initargs)
;;     instance))
;; (defmethod make-instance ((class-name symbol) &rest initargs)
;;   (apply #'make-instance (find-class class-name) initargs))
;;
(deftest make-instance.1
  (progn
    (defclass make-instance-1 ()
      ((aaa :initarg :xxx)
       (bbb :initarg :yyy)))
    (values)))

(deftest make-instance.2
  (let ((inst (make-instance 'make-instance-1)))
    (subtypep (class-of inst) 'make-instance-1))
  t t)

(deftest make-instance.3
  (let ((inst (make-instance 'make-instance-1)))
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  nil nil)

(deftest make-instance.4
  (let ((inst (make-instance 'make-instance-1 :xxx 100 :yyy 200)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  100 200)

(deftest make-instance.5
  (let ((inst (make-instance
                (find-class 'make-instance-1)
                :xxx 100 :yyy 200)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  100 200)

(deftest-error! make-instance-error.1
  (eval '(make-instance)))

(deftest-error! make-instance-error.2
  (eval '(make-instance (find-class 'make-instance-1) :hello)))

(deftest-error make-instance-error.3
  (eval '(make-instance 100)))

