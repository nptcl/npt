;;
;;  ANSI COMMON LISP: 7. Objects
;;
(deftest redefined-finalize.1
  (progn
    (defclass redefined-finalize1 ()
      (aaa bbb ccc))
    (make-instance 'redefined-finalize1)
    (defclass redefined-finalize1 ()
      (ddd fff ggg))
    (null
      (find-class 'redefined-finalize1)))
  nil)

(deftest redefined-finalize.2
  (let (x y)
    (defclass redefined-finalize2 () (aaa))
    (setq x (make-instance 'redefined-finalize2))
    (defclass redefined-finalize2 () (aaa))
    (setq y (make-instance 'redefined-finalize2))
    (slot-boundp x 'aaa))
  nil)

(deftest redefined-finalize.3
  (let (x y)
    (defclass redefined-finalize3 () (aaa))
    (setq x (make-instance 'redefined-finalize3))
    (setf (slot-value x 'aaa) :hello)
    (defclass redefined-finalize3 () (aaa))
    (setq y (make-instance 'redefined-finalize3))
    (slot-value x 'aaa))
  :hello)

(deftest redefined-finalize.4
  (let (x y)
    (defclass redefined-finalize4 () (aaa))
    (setq x (make-instance 'redefined-finalize4))
    (setf (slot-value x 'aaa) :hello)
    (defclass redefined-finalize4 () (bbb))
    (setq y (make-instance 'redefined-finalize4))
    (values
      (slot-exists-p x 'aaa)
      (slot-exists-p x 'bbb)
      (slot-boundp x 'bbb)))
  nil t nil)

(deftest redefined-finalize.5
  (let (x y)
    (defclass redefined-finalize5 () (aaa))
    (setq x (make-instance 'redefined-finalize5))
    (setf (slot-value x 'aaa) :hello)
    (defclass redefined-finalize5 () (bbb))
    (setq y (make-instance 'redefined-finalize5))
    (defmethod update-instance-for-redefined-class :before
      ((inst redefined-finalize5) add del prop &rest args &key &allow-other-keys)
      (declare (ignore add del args))
      (setf (slot-value inst 'bbb) (getf prop 'aaa)))
    (values
      (slot-exists-p x 'aaa)
      (slot-exists-p x 'bbb)
      (slot-value x 'bbb)))
  nil t :hello)

(defun name-direct-superclasses (x)
  (let ((list (list (find-class 'standard-object)
                    (find-class 't))))
    (mapcar #'class-name
            (remove-if
              (lambda (c)
                (find c list :test #'eq))
              (class-direct-superclasses
                (find-class x))))))

(deftest redefined-finalize-superclass.1
  (progn
    (defclass redefinef-superclass1 () ())
    (make-instance 'redefinef-superclass1)
    (defclass redefinef-superclass1a (redefinef-superclass1) ())
    (make-instance 'redefinef-superclass1a)
    (defclass redefinef-superclass1a () ())
    (make-instance 'redefinef-superclass1a)
    (name-direct-superclasses
      'redefinef-superclass1a))
  nil)

(deftest redefined-finalize-superclass.2
  (progn
    (defclass redefinef-superclass2 () ())
    (make-instance 'redefinef-superclass2)
    (defclass redefinef-superclass2a () ())
    (make-instance 'redefinef-superclass2a)
    (defclass redefinef-superclass2a (redefinef-superclass2) ())
    (make-instance 'redefinef-superclass2a)
    (name-direct-superclasses
      'redefinef-superclass2a))
  (redefinef-superclass2))

(defun name-direct-subclasses (x)
  (mapcar #'class-name
          (class-direct-subclasses
            (find-class x))))

(deftest redefined-finalize-subclass.1
  (progn
    (defclass redefinef-subclass1 () ())
    (make-instance 'redefinef-subclass1)
    (defclass redefinef-subclass1a (redefinef-subclass1) ())
    (make-instance 'redefinef-subclass1a)
    (defclass redefinef-subclass1a () ())
    (make-instance 'redefinef-subclass1a)
    (name-direct-subclasses
      'redefinef-subclass1))
  nil)

(deftest redefined-finalize-subclass.2
  (progn
    (defclass redefinef-subclass2 () ())
    (make-instance 'redefinef-subclass2)
    (defclass redefinef-subclass2a () ())
    (make-instance 'redefinef-subclass2a)
    (defclass redefinef-subclass2a (redefinef-subclass2) ())
    (make-instance 'redefinef-subclass2a)
    (name-direct-subclasses
      'redefinef-subclass2))
  (redefinef-subclass2a))

(deftest redefined-finalize-initargs.1
  (progn
    (defclass redefinef-initargs1 ()
      ((aaa :initarg :aaa))
      (:default-initargs :aaa 100))
    (make-instance 'redefinef-initargs1)
    (defclass redefinef-initargs1 ()
      ((aaa :initarg :aaa))
      (:default-initargs :aaa 200))
    (slot-value
      (make-instance 'redefinef-initargs1)
      'aaa))
  200)

(deftest redefined-finalize-access.1
  (let (x)
    (defclass redefinef-access1 () (aaa bbb))
    (setq x (make-instance 'redefinef-access1))
    (setf (slot-value x 'aaa) 10)
    (defmethod update-instance-for-redefined-class :before
      ((inst redefinef-access1) add del prop &rest args &key &allow-other-keys)
      (declare (ignore add del prop args))
      (setf (slot-value inst 'ccc) 20))
    (defclass redefinef-access1 () (aaa ccc ddd))
    (values
      (slot-value x 'aaa)
      (slot-value x 'ccc)))
  10 20)

(deftest redefined-finalize-access.2
  (let (x)
    (defclass redefinef-access2 () (aaa bbb))
    (setq x (make-instance 'redefinef-access2))
    (setf (slot-value x 'aaa) 10)
    (defmethod update-instance-for-redefined-class :before
      ((inst redefinef-access2) add del prop &rest args &key &allow-other-keys)
      (declare (ignore add del prop args))
      (setf (slot-value inst 'ccc) 20))
    (defclass redefinef-access2 () (aaa ccc ddd))
    (values
      (slot-boundp x 'aaa)
      (slot-boundp x 'ccc)
      (slot-boundp x 'ddd)))
  t t nil)

(deftest redefined-finalize-access.3
  (let (x)
    (defclass redefinef-access3 () (aaa bbb))
    (setq x (make-instance 'redefinef-access3))
    (setf (slot-value x 'aaa) 10)
    (defmethod update-instance-for-redefined-class :before
      ((inst redefinef-access3) add del prop &rest args &key &allow-other-keys)
      (declare (ignore add del prop args))
      (setf (slot-value inst 'ccc) 20))
    (defclass redefinef-access3 () (aaa ccc ddd))
    (values
      (slot-exists-p x 'aaa)
      (slot-exists-p x 'bbb)
      (slot-exists-p x 'ccc)
      (slot-exists-p x 'ddd)))
  t nil t t)

(deftest redefined-finalize-access.4
  (let (x)
    (defclass redefinef-access4 () (aaa bbb))
    (setq x (make-instance 'redefinef-access4))
    (setf (slot-value x 'aaa) 10)
    (defmethod update-instance-for-redefined-class :before
      ((inst redefinef-access4) add del prop &rest args &key &allow-other-keys)
      (declare (ignore add del prop args))
      (setf (slot-value inst 'ccc) 20))
    (defclass redefinef-access4 () (aaa ccc ddd))
    (values
      (typep
        (slot-makunbound x 'aaa)
        'redefinef-access4)
      (slot-boundp x 'aaa)
      (slot-boundp x 'ccc)
      (slot-boundp x 'ddd)))
  t nil t nil)

(deftest redefined-finalize-reader.1
  (progn
    (defclass redefinef-reader1 () ((aaa :reader redefinef-reader-call1a)))
    (make-instance 'redefinef-reader1)
    (defclass redefinef-reader1 () ((aaa :reader redefinef-reader-call1b)))
    (make-instance 'redefinef-reader1)
    (values
      (functionp #'redefinef-reader-call1a)
      (functionp #'redefinef-reader-call1b)))
  t t)

(deftest-error redefined-finalize-reader.2
  (let (x)
    (defclass redefinef-reader1 () ((aaa :reader redefinef-reader-call1a)))
    (make-instance 'redefinef-reader1)
    (defclass redefinef-reader1 () ((aaa :reader redefinef-reader-call1b)))
    (setq x (make-instance 'redefinef-reader1))
    (setf (slot-value x 'aaa) 10)
    (redefinef-reader-call1a x)))

(deftest redefined-finalize-reader.3
  (let (x)
    (defclass redefinef-reader1 () ((aaa :reader redefinef-reader-call1a)))
    (make-instance 'redefinef-reader1)
    (defclass redefinef-reader1 () ((aaa :reader redefinef-reader-call1b)))
    (setq x (make-instance 'redefinef-reader1))
    (setf (slot-value x 'aaa) 10)
    (redefinef-reader-call1b x))
  10)

(deftest redefined-finalize-writer.1
  (progn
    (defclass redefinef-writer1 () ((aaa :writer redefinef-writer-call1a)))
    (make-instance 'redefinef-writer1)
    (defclass redefinef-writer1 () ((aaa :writer redefinef-writer-call1b)))
    (make-instance 'redefinef-writer1)
    (values
      (functionp #'redefinef-writer-call1a)
      (functionp #'redefinef-writer-call1b)))
  t t)

(deftest-error redefined-finalize-writer.2
  (let (x)
    (defclass redefinef-writer1 () ((aaa :writer redefinef-writer-call1a)))
    (make-instance 'redefinef-writer1)
    (defclass redefinef-writer1 () ((aaa :writer redefinef-writer-call1b)))
    (setq x (make-instance 'redefinef-writer1))
    (setf (slot-value x 'aaa) 10)
    (redefinef-writer-call1a 20 x)))

(deftest redefined-finalize-writer.3
  (let (x)
    (defclass redefinef-writer1 () ((aaa :writer redefinef-writer-call1a)))
    (make-instance 'redefinef-writer1)
    (defclass redefinef-writer1 () ((aaa :writer redefinef-writer-call1b)))
    (setq x (make-instance 'redefinef-writer1))
    (setf (slot-value x 'aaa) 10)
    (values
      (redefinef-writer-call1b 20 x)
      (slot-value x 'aaa)))
  20 20)

(deftest redefined-reference.1
  (progn
    (defclass redefined-reference1 (redefiner-reference1a) ())
    (defclass redefined-reference1 (redefiner-reference1b) ())
    (null
      (find-class 'redefined-reference1)))
  nil)

(deftest redefined-reference.2
  (progn
    (defclass redefined-reference2 (redefiner-reference2a) ())
    (defclass redefined-reference2 (redefiner-reference2b) ())
    (defclass redefiner-reference2b () (aaa))
    (slot-exists-p
      (make-instance 'redefined-reference2)
      'aaa))
  t)

(defclass redefined-make-instances-obsolete1 () ())
(defclass redefined-make-instances-obsolete1 () ())

(deftest redefined-make-instances-obsolete.1
  (progn
    (make-instances-obsolete 'redefined-make-instances-obsolete1)
    nil)
  nil)

(deftest redefined-make-instances-obsolete.2
  (progn
    (make-instances-obsolete
      (find-class 'redefined-make-instances-obsolete1))
    nil)
  nil)

(deftest redefined-make-instances-obsolete.3
  (let (x y)
    (defclass redefined-make-instances-obsolete3 () ())
    (setq x (make-instance 'redefined-make-instances-obsolete3))
    (defmethod make-instances-obsolete :before ((inst standard-class))
      (declare (ignore inst))
      (setq y 100))
    (defclass redefined-make-instances-obsolete3 () (aaa))
    y)
  100)


;;
;;  change-class
;;
(defclass change-class1 () (aaa bbb))
(defclass change-class2 () (ccc ddd))

(deftest change-class.1
  (let ((x (make-instance 'change-class1)))
    (typep
      (change-class x 'change-class2)
      'change-class2))
  t)

(deftest change-class.2
  (let ((x (make-instance 'change-class1)))
    (change-class x 'change-class2)
    (values
      (slot-exists-p x 'aaa)
      (slot-exists-p x 'bbb)
      (slot-exists-p x 'ccc)
      (slot-exists-p x 'ddd)))
  nil nil t t)

(defclass change-class3 () (aaa bbb))
(defclass change-class4 () (ccc ddd))

(defmethod update-instance-for-different-class :before
  ((prev change-class3) (inst change-class4) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (setf (slot-value inst 'ccc) (slot-value prev 'bbb))
  (setf (slot-value inst 'ddd) (slot-value prev 'aaa)))

(deftest change-class.3
  (let ((x (make-instance 'change-class3)))
    (setf (slot-value x 'aaa) 10)
    (setf (slot-value x 'bbb) 20)
    (change-class x 'change-class4)
    (values
      (slot-value x 'ccc)
      (slot-value x 'ddd)))
  20 10)

