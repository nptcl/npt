;;
;;  ANSI COMMON LISP: 7. Objects
;;
(deftest redefined-finalize.1
  (progn
    (defclass redefined-finalize-1 ()
      (aaa bbb ccc))
    (make-instance 'redefined-finalize-1)
    (defclass redefined-finalize-1 ()
      (ddd fff ggg))
    (null
      (find-class 'redefined-finalize-1)))
  nil)

(deftest redefined-finalize.2
  (let (x y)
    (defclass redefined-finalize-2 () (aaa))
    (setq x (make-instance 'redefined-finalize-2))
    (defclass redefined-finalize-2 () (aaa))
    (setq y (make-instance 'redefined-finalize-2))
    (slot-boundp x 'aaa))
  nil)

(deftest redefined-finalize.3
  (let (x y)
    (defclass redefined-finalize-3 () (aaa))
    (setq x (make-instance 'redefined-finalize-3))
    (setf (slot-value x 'aaa) :hello)
    (defclass redefined-finalize-3 () (aaa))
    (setq y (make-instance 'redefined-finalize-3))
    (slot-value x 'aaa))
  :hello)

(deftest redefined-finalize.4
  (let (x y)
    (defclass redefined-finalize-4 () (aaa))
    (setq x (make-instance 'redefined-finalize-4))
    (setf (slot-value x 'aaa) :hello)
    (defclass redefined-finalize-4 () (bbb))
    (setq y (make-instance 'redefined-finalize-4))
    (values
      (slot-exists-p x 'aaa)
      (slot-exists-p x 'bbb)
      (slot-boundp x 'bbb)))
  nil t nil)

(deftest redefined-finalize.5
  (let (x y)
    (defclass redefined-finalize-5 () (aaa))
    (setq x (make-instance 'redefined-finalize-5))
    (setf (slot-value x 'aaa) :hello)
    (defclass redefined-finalize-5 () (bbb))
    (setq y (make-instance 'redefined-finalize-5))
    (defmethod update-instance-for-redefined-class :before
      ((inst redefined-finalize-5) add del prop &rest args &key &allow-other-keys)
      (declare (ignore add del args))
      (setf (slot-value inst 'bbb) (getf prop 'aaa)))
    (values
      (slot-exists-p x 'aaa)
      (slot-exists-p x 'bbb)
      (slot-value x 'bbb)))
  nil t :hello)

(deftest redefined-finalize-superclass.1
  (progn
    (defun redefined-finalize-superclass-name (x)
      (let ((list (list (find-class 'standard-object)
                        (find-class 't))))
        (mapcar #'class-name
                (remove-if
                  (lambda (c)
                    (find c list :test #'eq))
                  (class-direct-superclasses
                    (find-class x))))))
    (values)))

(deftest redefined-finalize-superclass.2
  (progn
    (defclass redefinef-superclass-1 () ())
    (make-instance 'redefinef-superclass-1)
    (defclass redefinef-superclass-2 (redefinef-superclass-1) ())
    (make-instance 'redefinef-superclass-2)
    (defclass redefinef-superclass-2 () ())
    (make-instance 'redefinef-superclass-2)
    (redefined-finalize-superclass-name
      'redefinef-superclass-2))
  nil)

(deftest redefined-finalize-superclass.3
  (progn
    (defclass redefinef-superclass-3 () ())
    (make-instance 'redefinef-superclass-3)
    (defclass redefinef-superclass-4 () ())
    (make-instance 'redefinef-superclass-4)
    (defclass redefinef-superclass-4 (redefinef-superclass-3) ())
    (make-instance 'redefinef-superclass-4)
    (redefined-finalize-superclass-name
      'redefinef-superclass-4))
  (redefinef-superclass-3))

(deftest redefined-finalize-subclass.1
  (progn
    (defun redefined-finalize-subclass-name (x)
      (mapcar #'class-name
              (class-direct-subclasses
                (find-class x))))
    (values)))

(deftest redefined-finalize-subclass.2
  (progn
    (defclass redefinef-subclass-1 () ())
    (make-instance 'redefinef-subclass-1)
    (defclass redefinef-subclass-2 (redefinef-subclass-1) ())
    (make-instance 'redefinef-subclass-2)
    (defclass redefinef-subclass-2 () ())
    (make-instance 'redefinef-subclass-2)
    (redefined-finalize-subclass-name
      'redefinef-subclass-1))
  nil)

(deftest redefined-finalize-subclass.3
  (progn
    (defclass redefinef-subclass-3 () ())
    (make-instance 'redefinef-subclass-3)
    (defclass redefinef-subclass-4 () ())
    (make-instance 'redefinef-subclass-4)
    (defclass redefinef-subclass-4 (redefinef-subclass-3) ())
    (make-instance 'redefinef-subclass-4)
    (redefined-finalize-subclass-name
      'redefinef-subclass-3))
  (redefinef-subclass-4))

(deftest redefined-finalize-initargs.1
  (progn
    (defclass redefinef-initargs-1 ()
      ((aaa :initarg :aaa))
      (:default-initargs :aaa 100))
    (make-instance 'redefinef-initargs-1)
    (defclass redefinef-initargs-1 ()
      ((aaa :initarg :aaa))
      (:default-initargs :aaa 200))
    (slot-value
      (make-instance 'redefinef-initargs-1)
      'aaa))
  200)

(deftest redefined-finalize-access.1
  (let (x)
    (defclass redefinef-access-1 () (aaa bbb))
    (setq x (make-instance 'redefinef-access-1))
    (setf (slot-value x 'aaa) 10)
    (defmethod update-instance-for-redefined-class :before
      ((inst redefinef-access-1) add del prop &rest args &key &allow-other-keys)
      (declare (ignore add del prop args))
      (setf (slot-value inst 'ccc) 20))
    (defclass redefinef-access-1 () (aaa ccc ddd))
    (values
      (slot-value x 'aaa)
      (slot-value x 'ccc)))
  10 20)

(deftest redefined-finalize-access.2
  (let (x)
    (defclass redefinef-access-2 () (aaa bbb))
    (setq x (make-instance 'redefinef-access-2))
    (setf (slot-value x 'aaa) 10)
    (defmethod update-instance-for-redefined-class :before
      ((inst redefinef-access-2) add del prop &rest args &key &allow-other-keys)
      (declare (ignore add del prop args))
      (setf (slot-value inst 'ccc) 20))
    (defclass redefinef-access-2 () (aaa ccc ddd))
    (values
      (slot-boundp x 'aaa)
      (slot-boundp x 'ccc)
      (slot-boundp x 'ddd)))
  t t nil)

(deftest redefined-finalize-access.3
  (let (x)
    (defclass redefinef-access-3 () (aaa bbb))
    (setq x (make-instance 'redefinef-access-3))
    (setf (slot-value x 'aaa) 10)
    (defmethod update-instance-for-redefined-class :before
      ((inst redefinef-access-3) add del prop &rest args &key &allow-other-keys)
      (declare (ignore add del prop args))
      (setf (slot-value inst 'ccc) 20))
    (defclass redefinef-access-3 () (aaa ccc ddd))
    (values
      (slot-exists-p x 'aaa)
      (slot-exists-p x 'bbb)
      (slot-exists-p x 'ccc)
      (slot-exists-p x 'ddd)))
  t nil t t)

(deftest redefined-finalize-access.4
  (let (x)
    (defclass redefinef-access-4 () (aaa bbb))
    (setq x (make-instance 'redefinef-access-4))
    (setf (slot-value x 'aaa) 10)
    (defmethod update-instance-for-redefined-class :before
      ((inst redefinef-access-4) add del prop &rest args &key &allow-other-keys)
      (declare (ignore add del prop args))
      (setf (slot-value inst 'ccc) 20))
    (defclass redefinef-access-4 () (aaa ccc ddd))
    (values
      (typep
        (slot-makunbound x 'aaa)
        'redefinef-access-4)
      (slot-boundp x 'aaa)
      (slot-boundp x 'ccc)
      (slot-boundp x 'ddd)))
  t nil t nil)

(deftest redefined-finalize-reader.1
  (progn
    (defclass redefinef-reader-1 () ((aaa :reader redefinef-reader-call-1)))
    (make-instance 'redefinef-reader-1)
    (defclass redefinef-reader-1 () ((aaa :reader redefinef-reader-call-2)))
    (make-instance 'redefinef-reader-1)
    (values
      (functionp #'redefinef-reader-call-1)
      (functionp #'redefinef-reader-call-2)))
  t t)

(deftest-error redefined-finalize-reader.2
  (let (x)
    (defclass redefinef-reader-1 () ((aaa :reader redefinef-reader-call-1)))
    (make-instance 'redefinef-reader-1)
    (defclass redefinef-reader-1 () ((aaa :reader redefinef-reader-call-2)))
    (setq x (make-instance 'redefinef-reader-1))
    (setf (slot-value x 'aaa) 10)
    (redefinef-reader-call-1 x)))

(deftest redefined-finalize-reader.3
  (let (x)
    (defclass redefinef-reader-1 () ((aaa :reader redefinef-reader-call-1)))
    (make-instance 'redefinef-reader-1)
    (defclass redefinef-reader-1 () ((aaa :reader redefinef-reader-call-2)))
    (setq x (make-instance 'redefinef-reader-1))
    (setf (slot-value x 'aaa) 10)
    (redefinef-reader-call-2 x))
  10)

(deftest redefined-finalize-writer.1
  (progn
    (defclass redefinef-writer-1 () ((aaa :writer redefinef-writer-call-1)))
    (make-instance 'redefinef-writer-1)
    (defclass redefinef-writer-1 () ((aaa :writer redefinef-writer-call-2)))
    (make-instance 'redefinef-writer-1)
    (values
      (functionp #'redefinef-writer-call-1)
      (functionp #'redefinef-writer-call-2)))
  t t)

(deftest-error redefined-finalize-writer.2
  (let (x)
    (defclass redefinef-writer-1 () ((aaa :writer redefinef-writer-call-1)))
    (make-instance 'redefinef-writer-1)
    (defclass redefinef-writer-1 () ((aaa :writer redefinef-writer-call-2)))
    (setq x (make-instance 'redefinef-writer-1))
    (setf (slot-value x 'aaa) 10)
    (redefinef-writer-call-1 20 x)))

(deftest redefined-finalize-writer.3
  (let (x)
    (defclass redefinef-writer-1 () ((aaa :writer redefinef-writer-call-1)))
    (make-instance 'redefinef-writer-1)
    (defclass redefinef-writer-1 () ((aaa :writer redefinef-writer-call-2)))
    (setq x (make-instance 'redefinef-writer-1))
    (setf (slot-value x 'aaa) 10)
    (values
      (redefinef-writer-call-2 20 x)
      (slot-value x 'aaa)))
  20 20)

(deftest redefined-reference.1
  (progn
    (defclass redefined-reference-1 (redefiner-reference-1a) ())
    (defclass redefined-reference-1 (redefiner-reference-1b) ())
    (null
      (find-class 'redefined-reference-1)))
  nil)

(deftest redefined-reference.2
  (progn
    (defclass redefined-reference-2 (redefiner-reference-2a) ())
    (defclass redefined-reference-2 (redefiner-reference-2b) ())
    (defclass redefiner-reference-2b () (aaa))
    (slot-exists-p
      (make-instance 'redefined-reference-2)
      'aaa))
  t)

(defclass redefined-make-instances-obsolete-1 () ())
(defclass redefined-make-instances-obsolete-1 () ())

(deftest redefined-make-instances-obsolete.1
  (progn
    (make-instances-obsolete 'redefined-make-instances-obsolete-1)
    nil)
  nil)

(deftest redefined-make-instances-obsolete.2
  (progn
    (make-instances-obsolete
      (find-class 'redefined-make-instances-obsolete-1))
    nil)
  nil)

(deftest redefined-make-instances-obsolete.3
  (let (x y)
    (defclass redefined-make-instances-obsolete-3 () ())
    (setq x (make-instance 'redefined-make-instances-obsolete-3))
    (defmethod make-instances-obsolete :before ((inst standard-class))
      (declare (ignore inst))
      (setq y 100))
    (defclass redefined-make-instances-obsolete-3 () (aaa))
    y)
  100)

