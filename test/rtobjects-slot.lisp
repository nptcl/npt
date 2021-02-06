;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  Function SLOT-VALUE
;;
(defclass slot-value-1 ()
  ((aaa)
   (bbb :initform "Hello")))

(deftest slot-value.1
  (let ((inst (make-instance 'slot-value-1)))
    (slot-value inst 'bbb))
  "Hello")

(deftest-error slot-value.2
  (let ((inst (make-instance 'slot-value-1)))
    (slot-value inst 'aaa))
  unbound-slot)

(deftest-error slot-value.3
  (let ((inst (make-instance 'slot-value-1)))
    (slot-value inst 'no-such-slot-name)))

(deftest-error slot-value.4
  (eval '(slot-value 10 'no-such-slot-name))
  type-error)

(defstruct slot-value-2 aaa)
(deftest slot-value.5
  (let ((inst (make-slot-value-2)))
    (slot-value inst 'aaa))
  nil)

(deftest slot-value.6
  (let ((inst (make-slot-value-2 :aaa 10)))
    (slot-value inst 'aaa))
  10)

;;  slot-missing
(defclass slot-value-missing-1 () (aaa))
(defmethod slot-missing (x (inst slot-value-missing-1) name operation &optional value)
  (declare (ignore x inst name operation value))
  :ignore)

(deftest slot-value-missing.1
  (let ((inst (make-instance 'slot-value-missing-1)))
    (slot-value inst 'no-such-slot-name))
  :ignore)

(defclass slot-value-missing-2 () ())
(defmethod slot-missing (x (inst slot-value-missing-2) name operation &optional value)
  (and (eq x (class-of inst))
       (eq name 'aaa)
       (eq operation 'slot-value)
       (null value)))

(deftest slot-value-missing.2
  (let ((inst (make-instance 'slot-value-missing-2)))
    (slot-value inst 'aaa))
  t)

;;  slot-unbound
(defclass slot-value-unbound-1 () (aaa))
(defmethod slot-unbound (x (inst slot-value-unbound-1) name)
  (declare (ignore x inst name))
  :abcdef)

(deftest slot-value-unbound.1
  (let ((inst (make-instance 'slot-value-unbound-1)))
    (slot-value inst 'aaa))
  :abcdef)

(defclass slot-value-unbound-2 () (aaa))
(defmethod slot-unbound (x (inst slot-value-unbound-2) name)
  (and (eq x (class-of inst))
       (eq name 'aaa)))

(deftest slot-value-unbound.2
  (let ((inst (make-instance 'slot-value-unbound-2)))
    (slot-value inst 'aaa))
  t)

;;  error
(deftest-error! slot-value-error.1
  (eval '(slot-value 'slot-value-1)))

(deftest-error! slot-value-error.2
  (eval '(slot-value 'slot-value-1 'aaa nil)))

(deftest-error! slot-value-error.3
  (eval '(slot-value 'slot-value-1 100))
  type-error)


;;
;;  Function (SETF SLOT-VALUE)
;;
(defclass setf-slot-value-1 ()
  ((aaa)
   (bbb :initform "Hello")))

(deftest setf-slot-value.1
  (let ((inst (make-instance 'setf-slot-value-1)))
    (values
      (setf (slot-value inst 'bbb) "zzz")
      (slot-value inst 'bbb)))
  "zzz" "zzz")

(deftest setf-slot-value.2
  (let ((inst (make-instance 'setf-slot-value-1)))
    (values
      (slot-boundp inst 'aaa)
      (setf (slot-value inst 'aaa) "1234")
      (slot-value inst 'aaa)
      (slot-boundp inst 'aaa)))
  nil "1234" "1234" t)

(deftest-error setf-slot-value.3
  (let ((inst (make-instance 'setf-slot-value-1)))
    (setf (slot-value inst 'no-such-slot-name) 100)))

(deftest-error setf-slot-value.4
  (eval '(setf (slot-value 10 'no-such-slot-name) 20))
  type-error)

(deftest setf-slot-value.5
  (let ((inst (make-slot-value-2)))
    (setf (slot-value inst 'aaa) 100)
    (slot-value inst 'aaa))
  100)

(deftest setf-slot-value.6
  (let ((inst (make-slot-value-2 :aaa 10)))
    (setf (slot-value inst 'aaa) 20)
    (slot-value inst 'aaa))
  20)

;;  slot-missing
(defclass setf-slot-value-missing-1 () (aaa))
(defmethod slot-missing
  (x (inst setf-slot-value-missing-1) name operation &optional value)
  (declare (ignore x inst name operation value))
  :ignore)

(deftest setf-slot-value-missing.1
  (let ((inst (make-instance 'setf-slot-value-missing-1)))
    (setf (slot-value inst 'no-such-slot-name) 10))
  10)

(defclass setf-slot-value-missing-2 () ())
(defvar *setf-slot-value-missing-2*)
(defmethod slot-missing
  (x (inst setf-slot-value-missing-2) name operation &optional value)
  (setq *setf-slot-value-missing-2*
        (and (eq x (class-of inst))
             (eq name 'aaa)
             (eq operation 'setf)
             (eql value 10))))

(deftest setf-slot-value-missing.2
  (let (*setf-slot-value-missing-2*)
    (let ((inst (make-instance 'setf-slot-value-missing-2)))
      (setf (slot-value inst 'aaa) 10)
      *setf-slot-value-missing-2*))
  t)

;;  error
(deftest-error! setf-slot-value-error.1
  (eval '(setf (slot-value 'setf-slot-value-1) 10)))

(deftest-error! setf-slot-value-error.2
  (eval '(setf (slot-value 'setf-slot-value-1 'aaa nil) 10)))

(deftest-error! setf-slot-value-error.3
  (eval '(setf (slot-value 'slot-value-1 100) 20))
  type-error)

;;  ANSI Common Lisp
(defclass slot-value-test-foo ()
  ((a :accessor foo-a :initarg :a :initform 1)
   (b :accessor foo-b :initarg :b)
   (c :accessor foo-c :initform 3)))

(defvar *slot-value-test-foo1*)

(deftest slot-value-test.1
  (typep
    (setq *slot-value-test-foo1* (make-instance 'slot-value-test-foo :a 'one :b 'two))
    'slot-value-test-foo)
  t)

(deftest slot-value-test.2
  (slot-value *slot-value-test-foo1* 'a)
  one)

(deftest slot-value-test.3
  (slot-value *slot-value-test-foo1* 'b)
  two)

(deftest slot-value-test.4
  (slot-value *slot-value-test-foo1* 'c)
  3)

(deftest slot-value-test.5
  (setf (slot-value *slot-value-test-foo1* 'a) 'uno)
  uno)

(deftest slot-value-test.6
  (slot-value *slot-value-test-foo1* 'a)
  uno)

(deftest slot-value-test.7
  (typep
    (defmethod slot-value-test-foo-method ((x slot-value-test-foo))
      (slot-value x 'a))
    'standard-method)
  t)

(deftest slot-value-test.8
  (slot-value-test-foo-method *slot-value-test-foo1*)
  uno)


;;
;;  Function SLOT-BOUNDP
;;
(defclass slot-boundp-1 ()
  ((aaa)
   (bbb :initform "Hello")))

(deftest slot-boundp.1
  (let ((inst (make-instance 'slot-boundp-1)))
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  nil t)

(deftest-error slot-boundp.2
  (let ((inst (make-instance 'slot-boundp-1)))
    (slot-boundp inst 'no-such-slot-name)))

(deftest-error slot-boundp.3
  (let ((inst (make-instance 'slot-boundp-1)))
    (slot-boundp inst 'no-such-slot-name)))

(deftest-error slot-boundp.4
  (eval '(slot-boundp 10 'no-such-slot-name))
  type-error)

(defstruct slot-boundp-2 aaa)
(deftest slot-boundp.5
  (let ((inst (make-slot-boundp-2)))
    (slot-boundp inst 'aaa))
  t)

(deftest slot-boundp.6
  (let ((inst (make-slot-boundp-2 :aaa 10)))
    (slot-boundp inst 'aaa))
  t)

(defclass slot-boundp-3 ()
  (aaa (bbb :initform nil)))

(deftest slot-boundp.7
  (let ((inst (make-instance 'slot-boundp-3)))
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  nil t)

(deftest-error slot-boundp.8
  (let ((inst (make-instance 'slot-boundp-3)))
    (slot-boundp inst 'hello)))

(deftest slot-boundp.9
  (slot-boundp
    (find-class 'standard-class)
    'lisp-clos::name)
  t)

;;  slot-missing
(defclass slot-boundp-missing-1 () (aaa))
(defmethod slot-missing (x (inst slot-boundp-missing-1) name operation &optional value)
  (declare (ignore x inst name operation value))
  nil)

(deftest slot-boundp-missing.1
  (let ((inst (make-instance 'slot-boundp-missing-1)))
    (slot-boundp inst 'no-such-slot-name))
  nil)

(defclass slot-boundp-missing-2 () (aaa))
(defmethod slot-missing (x (inst slot-boundp-missing-2) name operation &optional value)
  (declare (ignore x inst name operation value))
  :hello)

(deftest slot-boundp-missing.2
  (let ((inst (make-instance 'slot-boundp-missing-2)))
    (slot-boundp inst 'no-such-slot-name))
  t)

(defclass slot-boundp-missing-3 () ())
(defvar *slot-boundp-missing-3*)
(defmethod slot-missing (x (inst slot-boundp-missing-3) name operation &optional value)
  (setq *slot-boundp-missing-3*
        (and (eq x (class-of inst))
             (eq name 'aaa)
             (eq operation 'slot-boundp)
             (null value)))
  t)

(deftest slot-boundp-missing.3
  (let ((inst (make-instance 'slot-boundp-missing-3))
        *slot-boundp-missing-3*)
    (values
      (slot-boundp inst 'aaa)
      *slot-boundp-missing-3*))
  t t)

;;  error
(deftest-error! slot-boundp-error.1
  (eval '(slot-boundp 'slot-boundp-1)))

(deftest-error! slot-boundp-error.2
  (eval '(slot-boundp 'slot-boundp-1 'aaa nil)))

(deftest-error! slot-boundp-error.3
  (eval '(slot-boundp 'slot-boundp-1 100))
  type-error)


;;
;;  Function SLOT-EXISTS-P
;;
(defclass slot-exists-p-1 ()
  ((aaa)
   (bbb :initform "Hello")))

(deftest slot-exists-p.1
  (let ((inst (make-instance 'slot-exists-p-1)))
    (values
      (slot-exists-p inst 'aaa)
      (slot-exists-p inst 'bbb)
      (slot-exists-p inst 'ccc)))
  t t nil)

(deftest-error slot-exists-p.2
  (eval '(slot-exists-p 10 'no-such-slot-name))
  type-error)

(defstruct slot-exists-p-2 aaa)
(deftest slot-exists-p.3
  (let ((inst (make-slot-exists-p-2)))
    (values
      (slot-exists-p inst 'aaa)
      (slot-exists-p inst 'bbb)))
  t nil)

(deftest slot-exists-p.4
  (let ((inst (make-slot-exists-p-2 :aaa 10)))
    (slot-exists-p inst 'aaa))
  t)

(deftest slot-exists-p.5
  (let ((inst (make-instance 'slot-boundp-3)))
    (values
      (slot-exists-p inst 'aaa)
      (slot-exists-p inst 'bbb)
      (slot-exists-p inst 'hello)))
  t t nil)

(deftest slot-exists-p.6
  (let ((inst (make-instance 'standard-class)))
    (values
      (slot-exists-p inst 'lisp-clos::name)
      (slot-exists-p inst 'hello)))
  t nil)

;;  error
(deftest-error! slot-exists-p-error.1
  (eval '(slot-exists-p 'slot-exists-p-1)))

(deftest-error! slot-exists-p-error.2
  (eval '(slot-exists-p 'slot-exists-p-1 'aaa nil)))

(deftest-error! slot-exists-p-error.3
  (eval '(slot-exists-p 'slot-exists-p-1 100))
  type-error)


;;
;;  Function SLOT-MAKUNBOUND
;;
(defclass slot-makunbound-1 ()
  ((aaa)
   (bbb :initform "Hello")))

(deftest slot-makunbound.1
  (let ((inst (make-instance 'slot-makunbound-1)))
    (values
      (typep (slot-makunbound inst 'aaa) 'slot-makunbound-1)
      (slot-boundp inst 'aaa)))
  t nil)

(deftest slot-makunbound.2
  (let ((inst (make-instance 'slot-makunbound-1)))
    (values
      (typep (slot-makunbound inst 'bbb) 'slot-makunbound-1)
      (slot-boundp inst 'bbb)))
  t nil)

(deftest-error slot-makunbound.3
  (let ((inst (make-instance 'slot-makunbound-1)))
    (slot-makunbound inst 'no-such-slot-name)))

(deftest-error slot-makunbound.4
  (eval '(slot-makunbound 10 'no-such-slot-name))
  type-error)

(defstruct slot-makunbound-2 aaa)
(deftest slot-makunbound.5
  (let ((inst (make-slot-makunbound-2)))
    (values
      (typep (slot-makunbound inst 'aaa) 'slot-makunbound-2)
      (slot-boundp inst 'aaa)))
  t nil)

(deftest slot-makunbound.6
  (let ((inst (make-slot-makunbound-2 :aaa 10)))
    (values
      (typep (slot-makunbound inst 'aaa) 'slot-makunbound-2)
      (slot-boundp inst 'aaa)))
  t nil)

(deftest slot-makunbound.7
  (let ((inst (make-instance 'slot-boundp-3)))
    (slot-makunbound inst 'aaa)
    (slot-makunbound inst 'bbb)
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  nil nil)

(deftest-error slot-makunbound.8
  (let ((inst (make-instance 'slot-boundp-3)))
    (slot-makunbound inst 'hello)))

;;  slot-missing
(defclass slot-makunbound-missing-1 () (aaa))
(defmethod slot-missing
  (x (inst slot-makunbound-missing-1) name operation &optional value)
  (declare (ignore x inst name operation value))
  :ignore)

(deftest slot-makunbound-missing.1
  (let ((inst (make-instance 'slot-makunbound-missing-1)))
    (typep
      (slot-makunbound inst 'no-such-slot-name)
      'slot-makunbound-missing-1))
  t)

(defclass slot-makunbound-missing-2 () ())
(defvar *slot-makunbound-missing-2*)
(defmethod slot-missing
  (x (inst slot-makunbound-missing-2) name operation &optional value)
  (setq *slot-makunbound-missing-2*
        (and (eq x (class-of inst))
             (eq name 'aaa)
             (eq operation 'slot-makunbound)
             (null value))))

(deftest slot-makunbound-missing.2
  (let ((inst (make-instance 'slot-makunbound-missing-2))
        (*slot-makunbound-missing-2*))
    (values
      (typep
        (slot-makunbound inst 'aaa)
        'slot-makunbound-missing-2)
      *slot-makunbound-missing-2*))
  t t)

;;  error
(deftest-error! slot-makunbound-error.1
  (eval '(slot-makunbound 'slot-makunbound-1)))

(deftest-error! slot-makunbound-error.2
  (eval '(slot-makunbound 'slot-makunbound-1 'aaa nil)))

(deftest-error! slot-makunbound-error.3
  (eval '(slot-makunbound 'slot-makunbound-1 100))
  type-error)


;;
;;  Standard Generic Function SLOT-MISSING
;;
(defclass slot-missing-1 () (aaa))

(defmethod slot-missing (x (inst slot-missing-1) name operation &optional value)
  (declare (ignore x inst name operation value)))

(deftest slot-missing.1
  (let ((inst (make-instance 'slot-missing-1)))
    (slot-value inst 'no-such-slot-name))
  nil)

(defclass slot-missing-2 () (aaa))

(defmethod slot-missing (x (inst slot-missing-2) name operation &optional value)
  (declare (ignore x inst name operation value))
  (values 10 20 30))

(deftest slot-missing.2
  (let ((inst (make-instance 'slot-missing-2)))
    (slot-value inst 'no-such-slot-name))
  10)


;;  Standard Generic Function SLOT-UNBOUND
;;  Macro WITH-ACCESSORS
;;  Macro WITH-SLOTS
;;  Condition Type UNBOUND-SLOT
;;  Function UNBOUND-SLOT-INSTANCE

