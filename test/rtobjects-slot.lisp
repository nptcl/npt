;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  Function SLOT-VALUE
;;
(deftest slot-value.1
  (progn
    (defclass slot-value-1 ()
      ((aaa)
       (bbb :initform "Hello")))
    (values)))

(deftest slot-value.2
  (let ((inst (make-instance 'slot-value-1)))
    (slot-value inst 'bbb))
  "Hello")

(deftest-error slot-value.3
  (let ((inst (make-instance 'slot-value-1)))
    (slot-value inst 'aaa))
  unbound-slot)

(deftest-error slot-value.4
  (let ((inst (make-instance 'slot-value-1)))
    (slot-value inst 'no-such-slot-name)))

(deftest-error slot-value.5
  (eval '(slot-value 10 'no-such-slot-name))
  type-error)

(deftest slot-value.6
  (progn
    (defstruct slot-value-2 aaa)
    (values)))

(deftest slot-value.7
  (let ((inst (make-slot-value-2)))
    (slot-value inst 'aaa))
  nil)

(deftest slot-value.8
  (let ((inst (make-slot-value-2 :aaa 10)))
    (slot-value inst 'aaa))
  10)

;;  slot-missing
(deftest slot-value-missing.1
  (progn
    (defclass slot-value-missing-1 () (aaa))
    (defmethod slot-missing
      ((x standard-class) (inst slot-value-missing-1) name operation &optional value)
      (declare (ignore x inst name operation value))
      :ignore)
    (values)))

(deftest slot-value-missing.2
  (let ((inst (make-instance 'slot-value-missing-1)))
    (slot-value inst 'no-such-slot-name))
  :ignore)

(deftest slot-value-missing.3
  (progn
    (defclass slot-value-missing-2 () ())
    (defmethod slot-missing
      ((x standard-class) (inst slot-value-missing-2) name operation &optional value)
      (and (eq x (class-of inst))
           (eq name 'aaa)
           (eq operation 'slot-value)
           (null value)))
    (values)))

(deftest slot-value-missing.4
  (let ((inst (make-instance 'slot-value-missing-2)))
    (slot-value inst 'aaa))
  t)

;;  slot-unbound
(deftest slot-value-unbound.1
  (progn
    (defclass slot-value-unbound-1 () (aaa))
    (defmethod slot-unbound
      ((x standard-class) (inst slot-value-unbound-1) name)
      (declare (ignore x inst name))
      :abcdef)
    (values)))

(deftest slot-value-unbound.2
  (let ((inst (make-instance 'slot-value-unbound-1)))
    (slot-value inst 'aaa))
  :abcdef)

(deftest slot-value-unbound.3
  (progn
    (defclass slot-value-unbound-2 () (aaa))
    (defmethod slot-unbound (x (inst slot-value-unbound-2) name)
      (and (eq x (class-of inst))
           (eq name 'aaa)))
    (values)))

(deftest slot-value-unbound.4
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
(deftest setf-slot-value.1
  (progn
    (defclass setf-slot-value-1 ()
      ((aaa)
       (bbb :initform "Hello")))
    (values)))

(deftest setf-slot-value.2
  (let ((inst (make-instance 'setf-slot-value-1)))
    (values
      (setf (slot-value inst 'bbb) "zzz")
      (slot-value inst 'bbb)))
  "zzz" "zzz")

(deftest setf-slot-value.3
  (let ((inst (make-instance 'setf-slot-value-1)))
    (values
      (slot-boundp inst 'aaa)
      (setf (slot-value inst 'aaa) "1234")
      (slot-value inst 'aaa)
      (slot-boundp inst 'aaa)))
  nil "1234" "1234" t)

(deftest-error setf-slot-value.4
  (let ((inst (make-instance 'setf-slot-value-1)))
    (setf (slot-value inst 'no-such-slot-name) 100)))

(deftest-error setf-slot-value.5
  (eval '(setf (slot-value 10 'no-such-slot-name) 20))
  type-error)

(deftest setf-slot-value.6
  (let ((inst (make-slot-value-2)))
    (setf (slot-value inst 'aaa) 100)
    (slot-value inst 'aaa))
  100)

(deftest setf-slot-value.7
  (let ((inst (make-slot-value-2 :aaa 10)))
    (setf (slot-value inst 'aaa) 20)
    (slot-value inst 'aaa))
  20)

;;  slot-missing
(deftest setf-slot-value-missing.1
  (progn
    (defclass setf-slot-value-missing-1 () (aaa))
    (defmethod slot-missing
      (x (inst setf-slot-value-missing-1) name operation &optional value)
      (declare (ignore x inst name operation value))
      :ignore)
    (values)))

(deftest setf-slot-value-missing.2
  (let ((inst (make-instance 'setf-slot-value-missing-1)))
    (setf (slot-value inst 'no-such-slot-name) 10))
  10)

(deftest setf-slot-value-missing.3
  (progn
    (defclass setf-slot-value-missing-2 () ())
    (defvar *setf-slot-value-missing-2*)
    (defmethod slot-missing
      (x (inst setf-slot-value-missing-2) name operation &optional value)
      (setq *setf-slot-value-missing-2*
            (and (eq x (class-of inst))
                 (eq name 'aaa)
                 (eq operation 'setf)
                 (eql value 10))))
    (values)))

(deftest setf-slot-value-missing.4
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
(deftest slot-value-test.1
  (progn
    (defclass slot-value-test-foo ()
      ((a :accessor foo-a :initarg :a :initform 1)
       (b :accessor foo-b :initarg :b)
       (c :accessor foo-c :initform 3)))
    (defvar *slot-value-test-foo1*)
    (values)))

(deftest slot-value-test.2
  (typep
    (setq *slot-value-test-foo1* (make-instance 'slot-value-test-foo :a 'one :b 'two))
    'slot-value-test-foo)
  t)

(deftest slot-value-test.3
  (slot-value *slot-value-test-foo1* 'a)
  one)

(deftest slot-value-test.4
  (slot-value *slot-value-test-foo1* 'b)
  two)

(deftest slot-value-test.5
  (slot-value *slot-value-test-foo1* 'c)
  3)

(deftest slot-value-test.6
  (setf (slot-value *slot-value-test-foo1* 'a) 'uno)
  uno)

(deftest slot-value-test.7
  (slot-value *slot-value-test-foo1* 'a)
  uno)

(deftest slot-value-test.8
  (typep
    (defmethod slot-value-test-foo-method ((x slot-value-test-foo))
      (slot-value x 'a))
    'standard-method)
  t)

(deftest slot-value-test.9
  (slot-value-test-foo-method *slot-value-test-foo1*)
  uno)


;;
;;  Function SLOT-BOUNDP
;;
(deftest slot-boundp.1
  (progn
    (defclass slot-boundp-1 ()
      ((aaa)
       (bbb :initform "Hello")))
    (values)))

(deftest slot-boundp.2
  (let ((inst (make-instance 'slot-boundp-1)))
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  nil t)

(deftest-error slot-boundp.3
  (let ((inst (make-instance 'slot-boundp-1)))
    (slot-boundp inst 'no-such-slot-name)))

(deftest-error slot-boundp.4
  (let ((inst (make-instance 'slot-boundp-1)))
    (slot-boundp inst 'no-such-slot-name)))

(deftest-error slot-boundp.5
  (eval '(slot-boundp 10 'no-such-slot-name))
  type-error)

(deftest slot-boundp.6
  (progn
    (defstruct slot-boundp-2 aaa)
    (values)))

(deftest slot-boundp.7
  (let ((inst (make-slot-boundp-2)))
    (slot-boundp inst 'aaa))
  t)

(deftest slot-boundp.8
  (let ((inst (make-slot-boundp-2 :aaa 10)))
    (slot-boundp inst 'aaa))
  t)

(deftest slot-boundp.9
  (progn
    (defclass slot-boundp-3 ()
      (aaa (bbb :initform nil)))
    (values)))

(deftest slot-boundp.10
  (let ((inst (make-instance 'slot-boundp-3)))
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  nil t)

(deftest-error slot-boundp.11
  (let ((inst (make-instance 'slot-boundp-3)))
    (slot-boundp inst 'hello)))

(deftest slot-boundp.12
  (slot-boundp
    (find-class 'standard-class)
    'lisp-clos::name)
  t)

;;  slot-missing
(deftest slot-boundp-missing.1
  (progn
    (defclass slot-boundp-missing-1 () (aaa))
    (defmethod slot-missing
      ((x standard-class) (inst slot-boundp-missing-1) name operation &optional value)
      (declare (ignore x inst name operation value))
      nil)
    (values)))

(deftest slot-boundp-missing.2
  (let ((inst (make-instance 'slot-boundp-missing-1)))
    (slot-boundp inst 'no-such-slot-name))
  nil)

(deftest slot-boundp-missing.3
  (progn
    (defclass slot-boundp-missing-2 () (aaa))
    (defmethod slot-missing
      ((x standard-class) (inst slot-boundp-missing-2) name operation &optional value)
      (declare (ignore x inst name operation value))
      :hello)
    (values)))

(deftest slot-boundp-missing.4
  (let ((inst (make-instance 'slot-boundp-missing-2)))
    (slot-boundp inst 'no-such-slot-name))
  t)

(deftest slot-boundp-missing.5
  (progn
    (defclass slot-boundp-missing-3 () ())
    (defvar *slot-boundp-missing-3*)
    (defmethod slot-missing
      ((x standard-class) (inst slot-boundp-missing-3) name operation &optional value)
      (setq *slot-boundp-missing-3*
            (and (eq x (class-of inst))
                 (eq name 'aaa)
                 (eq operation 'slot-boundp)
                 (null value)))
      t)
    (values)))

(deftest slot-boundp-missing.6
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
(deftest slot-exists-p.1
  (progn
    (defclass slot-exists-p-1 ()
      ((aaa)
       (bbb :initform "Hello")))
    (values)))

(deftest slot-exists-p.2
  (let ((inst (make-instance 'slot-exists-p-1)))
    (values
      (slot-exists-p inst 'aaa)
      (slot-exists-p inst 'bbb)
      (slot-exists-p inst 'ccc)))
  t t nil)

(deftest-error slot-exists-p.3
  (eval '(slot-exists-p 10 'no-such-slot-name))
  type-error)

(deftest slot-exists-p.4
  (progn
    (defstruct slot-exists-p-2 aaa)
    (values)))

(deftest slot-exists-p.5
  (let ((inst (make-slot-exists-p-2)))
    (values
      (slot-exists-p inst 'aaa)
      (slot-exists-p inst 'bbb)))
  t nil)

(deftest slot-exists-p.6
  (let ((inst (make-slot-exists-p-2 :aaa 10)))
    (slot-exists-p inst 'aaa))
  t)

(deftest slot-exists-p.7
  (let ((inst (make-instance 'slot-boundp-3)))
    (values
      (slot-exists-p inst 'aaa)
      (slot-exists-p inst 'bbb)
      (slot-exists-p inst 'hello)))
  t t nil)

(deftest slot-exists-p.8
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
(deftest slot-makunbound.1
  (progn
    (defclass slot-makunbound-1 ()
      ((aaa)
       (bbb :initform "Hello")))
    (values)))

(deftest slot-makunbound.2
  (let ((inst (make-instance 'slot-makunbound-1)))
    (values
      (typep (slot-makunbound inst 'aaa) 'slot-makunbound-1)
      (slot-boundp inst 'aaa)))
  t nil)

(deftest slot-makunbound.3
  (let ((inst (make-instance 'slot-makunbound-1)))
    (values
      (typep (slot-makunbound inst 'bbb) 'slot-makunbound-1)
      (slot-boundp inst 'bbb)))
  t nil)

(deftest-error slot-makunbound.4
  (let ((inst (make-instance 'slot-makunbound-1)))
    (slot-makunbound inst 'no-such-slot-name)))

(deftest-error slot-makunbound.5
  (eval '(slot-makunbound 10 'no-such-slot-name))
  type-error)

(deftest slot-makunbound.6
  (progn
    (defstruct slot-makunbound-2 aaa)
    (values)))

(deftest slot-makunbound.7
  (let ((inst (make-slot-makunbound-2)))
    (values
      (typep (slot-makunbound inst 'aaa) 'slot-makunbound-2)
      (slot-boundp inst 'aaa)))
  t nil)

(deftest slot-makunbound.8
  (let ((inst (make-slot-makunbound-2 :aaa 10)))
    (values
      (typep (slot-makunbound inst 'aaa) 'slot-makunbound-2)
      (slot-boundp inst 'aaa)))
  t nil)

(deftest slot-makunbound.9
  (let ((inst (make-instance 'slot-boundp-3)))
    (slot-makunbound inst 'aaa)
    (slot-makunbound inst 'bbb)
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  nil nil)

(deftest-error slot-makunbound.10
  (let ((inst (make-instance 'slot-boundp-3)))
    (slot-makunbound inst 'hello)))

;;  slot-missing
(deftest slot-makunbound-missing.1
  (progn
    (defclass slot-makunbound-missing-1 () (aaa))
    (defmethod slot-missing
      (x (inst slot-makunbound-missing-1) name operation &optional value)
      (declare (ignore x inst name operation value))
      :ignore)
    (values)))

(deftest slot-makunbound-missing.2
  (let ((inst (make-instance 'slot-makunbound-missing-1)))
    (typep
      (slot-makunbound inst 'no-such-slot-name)
      'slot-makunbound-missing-1))
  t)

(deftest slot-makunbound-missing.3
  (progn
    (defclass slot-makunbound-missing-2 () ())
    (defvar *slot-makunbound-missing-2*)
    (defmethod slot-missing
      (x (inst slot-makunbound-missing-2) name operation &optional value)
      (setq *slot-makunbound-missing-2*
            (and (eq x (class-of inst))
                 (eq name 'aaa)
                 (eq operation 'slot-makunbound)
                 (null value))))
    (values)))

(deftest slot-makunbound-missing.4
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
(deftest slot-missing.1
  (progn
    (defclass slot-missing-1 () (aaa))
    (defmethod slot-missing (x (inst slot-missing-1) name operation &optional value)
      (declare (ignore x inst name operation value)))
    (values)))

(deftest slot-missing.2
  (let ((inst (make-instance 'slot-missing-1)))
    (slot-value inst 'no-such-slot-name))
  nil)

(deftest slot-missing.3
  (progn
    (defclass slot-missing-2 () (aaa))
    (defmethod slot-missing (x (inst slot-missing-2) name operation &optional value)
      (declare (ignore x inst name operation value))
      (values 10 20 30))
    (values)))

(deftest slot-missing.4
  (let ((inst (make-instance 'slot-missing-2)))
    (slot-value inst 'no-such-slot-name))
  10)


;;
;;  Standard Generic Function SLOT-UNBOUND
;;
(deftest slot-unbound.1
  (progn
    (defclass slot-unbound-1 () (aaa))
    (defmethod slot-unbound ((x standard-class) (inst slot-unbound-1) name)
      (declare (ignore x inst name operation value)))
    (values)))

(deftest slot-unbound.2
  (let ((inst (make-instance 'slot-unbound-1)))
    (slot-value inst 'aaa))
  nil)

(deftest slot-unbound.3
  (progn
    (defclass slot-unbound-2 () (aaa))
    (defmethod slot-unbound ((x standard-class) (inst slot-unbound-2) name)
      (declare (ignore x inst name operation value))
      (values 10 20 30))
    (values)))

(deftest slot-unbound.4
  (let ((inst (make-instance 'slot-unbound-2)))
    (slot-value inst 'aaa))
  10)


;;
;;  Macro WITH-ACCESSORS
;;
(deftest with-accessors.1
  (progn
    (defclass with-accessors-1 ()
      ((aaa :initform 10 :accessor with-accessors-call-1)
       (bbb :accessor with-accessors-call-2)))
    (values)))

(deftest with-accessors.2
  (with-accessors () (make-instance 'with-accessors-1))
  nil)

(deftest with-accessors.3
  (let ((inst (make-instance 'with-accessors-1)))
    (with-accessors () inst
      111))
  111)

(deftest with-accessors.4
  (with-accessors ((x with-accessors-call-1)) (make-instance 'with-accessors-1)
    x)
  10)

(deftest with-accessors.5
  (let ((inst (make-instance 'with-accessors-1)))
    (with-accessors ((x with-accessors-call-1)) inst
      (+ x 20)))
  30)

(deftest with-accessors.6
  (let ((inst (make-instance 'with-accessors-1)))
    (with-accessors ((x with-accessors-call-1)) inst
      (incf x 200)
      (values x (slot-value inst 'aaa))))
  210 210)

(deftest with-accessors.7
  (let ((inst (make-instance 'with-accessors-1)))
    (with-accessors ((x with-accessors-call-1)
                     (y with-accessors-call-2)) inst
      (incf x 200)
      (setq y 300)
      (values x y
              (slot-value inst 'aaa)
              (slot-value inst 'bbb))))
  210 300 210 300)

(deftest with-accessors.8
  (let ((inst (make-instance 'with-accessors-1)))
    (with-accessors ((x with-accessors-call-1)) inst
      (declare (ignore x))
      :hello))
  :hello)

(deftest-error with-accessors-error.1
  (eval '(with-accessors ())))

(deftest-error with-accessors-error.2
  (eval '(with-accessors (aaa) (make-instance 'with-accessors-1))))

;;  ANSI Common Lisp
(defvar *with-accessors-test-list* nil)
(deftest with-accessors-test.1
  (progn
    (defclass with-accessors-test-thing ()
      ((x :initarg :x :accessor with-accessors-test-thing-x)
       (y :initarg :y :accessor with-accessors-test-thing-y)))
    (defmethod (setf with-accessors-test-thing-x) :before
      (new-x (thing with-accessors-test-thing))
      (push
        (format nil "Changing X from ~D to ~D"
                (with-accessors-test-thing-x thing) new-x)
        *with-accessors-test-list*))
    (values)))

(deftest with-accessors-test.2
  (progn
    (setq with-accessors-test-thing1
          (make-instance 'with-accessors-test-thing :x 1 :y 2)
          with-accessors-test-thing2
          (make-instance 'with-accessors-test-thing :x 7 :y 8))
    (values)))

(deftest with-accessors-test.3
  (with-accessors ((x1 with-accessors-test-thing-x)
                   (y1 with-accessors-test-thing-y)) with-accessors-test-thing1
    (with-accessors ((x2 with-accessors-test-thing-x)
                     (y2 with-accessors-test-thing-y)) with-accessors-test-thing2
      (list (list x1 (with-accessors-test-thing-x with-accessors-test-thing1)
                  y1 (with-accessors-test-thing-y with-accessors-test-thing1)
                  x2 (with-accessors-test-thing-x with-accessors-test-thing2)
                  y2 (with-accessors-test-thing-y with-accessors-test-thing2))
            (setq x1 (+ y1 x2))
            (list x1 (with-accessors-test-thing-x with-accessors-test-thing1)
                  y1 (with-accessors-test-thing-y with-accessors-test-thing1)
                  x2 (with-accessors-test-thing-x with-accessors-test-thing2)
                  y2 (with-accessors-test-thing-y with-accessors-test-thing2))
            (setf (with-accessors-test-thing-x with-accessors-test-thing2)
                  (list x1))
            (list x1 (with-accessors-test-thing-x with-accessors-test-thing1)
                  y1 (with-accessors-test-thing-y with-accessors-test-thing1)
                  x2 (with-accessors-test-thing-x with-accessors-test-thing2)
                  y2 (with-accessors-test-thing-y with-accessors-test-thing2))
            (nreverse *with-accessors-test-list*))))
  ((1 1 2 2 7 7 8 8)
   9
   (9 9 2 2 7 7 8 8)
   (9)
   (9 9 2 2 (9) (9) 8 8)
   ("Changing X from 1 to 9"
    "Changing X from 7 to (9)")))


;;
;;  Macro WITH-SLOTS
;;
(deftest with-slots.1
  (progn
    (defclass with-slots-1 ()
      ((aaa :initform 100)))
    (values)))

(deftest with-slots.2
  (with-slots () (make-instance 'with-slots-1))
  nil)

(deftest with-slots.3
  (with-slots () (make-instance 'with-slots-1)
    222)
  222)

(deftest with-slots.4
  (with-slots (aaa) (make-instance 'with-slots-1)
    aaa)
  100)

(deftest with-slots.5
  (let ((inst (make-instance 'with-slots-1)))
    (with-slots (aaa) inst
      (setq aaa 200)
      (values aaa (slot-value inst 'aaa))))
  200 200)

(deftest with-slots.6
  (progn
    (defclass with-slots-2 ()
      (aaa bbb ccc))
    (values)))

(deftest with-slots.7
  (let ((inst (make-instance 'with-slots-2)))
    (with-slots (aaa bbb ccc) inst
      (setq aaa 111 bbb 222 ccc 333))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  111 222 333)

(deftest with-slots.8
  (let ((inst (make-instance 'with-slots-2)))
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

(deftest-error with-slots-error.1
  (eval '(with-slots ())))

(deftest-error with-slots-error.2
  (eval '(with-slots (no-such-slot-name) (make-instance 'with-slots-1)
           no-such-slot-anme)))

;;  ANSI Common Lisp
(defvar *with-slots-test-list* nil)

(deftest with-slots-test.1
  (progn
    (defclass with-slots-test-thing ()
      ((x :initarg :x :accessor with-slots-test-thing-x)
       (y :initarg :y :accessor with-slots-test-thing-y)))
    (defmethod (setf with-slots-test-thing-x) :before
      (new-x (thing with-slots-test-thing))
      (push
        (format nil "Changing X from ~D to ~D"
                (with-slots-test-thing-x thing) new-x)
        *with-slots-test-list*))
    (values)))

(deftest with-slots-test.2
  (progn
    (setq with-slots-test-thing (make-instance 'with-slots-test-thing :x 0 :y 1))
    (values)))

(deftest with-slots-test.3
  (with-slots (x y) with-slots-test-thing
    (incf x) (incf y))
  2)

(deftest with-slots-test.4
  (values (with-slots-test-thing-x with-slots-test-thing)
          (with-slots-test-thing-y with-slots-test-thing))
  1 2)

(deftest with-slots-test.5
  (progn
    (setq with-slots-test-thing1 (make-instance 'with-slots-test-thing :x 1 :y 2))
    (setq with-slots-test-thing2 (make-instance 'with-slots-test-thing :x 7 :y 8))
    (values)))

(deftest with-slots-test.6
  (with-slots ((x1 x) (y1 y)) with-slots-test-thing1
    (with-slots ((x2 x) (y2 y)) with-slots-test-thing2
      (list (list x1 (with-slots-test-thing-x with-slots-test-thing1)
                  y1 (with-slots-test-thing-y with-slots-test-thing1)
                  x2 (with-slots-test-thing-x with-slots-test-thing2)
                  y2 (with-slots-test-thing-y with-slots-test-thing2))
            (setq x1 (+ y1 x2))
            (list x1 (with-slots-test-thing-x with-slots-test-thing1)
                  y1 (with-slots-test-thing-y with-slots-test-thing1)
                  x2 (with-slots-test-thing-x with-slots-test-thing2)
                  y2 (with-slots-test-thing-y with-slots-test-thing2))
            (setf (with-slots-test-thing-x with-slots-test-thing2) (list x1))
            (list x1 (with-slots-test-thing-x with-slots-test-thing1)
                  y1 (with-slots-test-thing-y with-slots-test-thing1)
                  x2 (with-slots-test-thing-x with-slots-test-thing2)
                  y2 (with-slots-test-thing-y with-slots-test-thing2))
            (nreverse *with-slots-test-list*))))
  ((1 1 2 2 7 7 8 8)
   9
   (9 9 2 2 7 7 8 8)
   (9)
   (9 9 2 2 (9) (9) 8 8)
   ("Changing X from 7 to (9)")))


;;
;;  Condition Type UNBOUND-SLOT
;;
(deftest unbound-slot.1
  (lisp-system:closp
    (find-class 'unbound-slot))
  t)

(deftest unbound-slot.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'unbound-slot)))
  (unbound-slot cell-error error serious-condition condition standard-object t))

(deftest unbound-slot.3
  (progn
    (defclass unbound-slot-1 () (aaa))
    (values)))

(deftest unbound-slot.4
  (handler-case
    (slot-value (make-instance 'unbound-slot-1) 'aaa)
    (unbound-slot () :hello))
  :hello)


;;
;;  Function UNBOUND-SLOT-INSTANCE
;;
(deftest unbound-slot-instance.1
  (handler-case
    (slot-value (make-instance 'unbound-slot-1) 'aaa)
    (unbound-slot (c) (typep
                        (unbound-slot-instance c)
                        'unbound-slot-1)))
  t)

(deftest-error! unbound-slot-instance-error.1
  (eval '(unbound-slot-instance)))

(deftest-error! unbound-slot-instance-error.2
  (eval '(unbound-slot-instance
           (make-instance 'unbound-slot-1)
           nil)))

(deftest-error unbound-slot-instance-error.3
  (eval '(unbound-slot-instance 100))
  type-error)

