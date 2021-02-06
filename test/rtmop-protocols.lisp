;;
;;  Metaobject Protocol
;;

;;
;;  Generic Function SLOT-VALUE-USING-CLASS
;;
(defclass slot-value-using-class-1 ()
  ((aaa :initform 100)))

(defmethod slot-value-using-class
  ((class standard-class) (inst slot-value-using-class-1) name)
  (declare (ignore class inst name))
  (+ (call-next-method) 20))

(deftest slot-value-using-class.1
  (let ((inst (make-instance 'slot-value-using-class-1)))
    (slot-value inst 'aaa))
  120)

;;  slot-missing
(defclass slot-value-using-class-2 ()
  ((aaa :initform 100)
   bbb))

(defmethod slot-value-using-class
  ((class standard-class) (inst slot-value-using-class-2) name)
  (declare (ignore class inst name))
  111)

(deftest slot-value-using-class.2
  (let ((inst (make-instance 'slot-value-using-class-2)))
    (slot-value inst 'ccc))
  111)

;;  slot-unbound
(deftest slot-value-using-class.3
  (let ((inst (make-instance 'slot-value-using-class-2)))
    (slot-value inst 'bbb))
  111)


;;
;;  Generic Function (SETF SLOT-VALUE-USING-CLASS)
;;
(defclass setf-slot-value-using-class-1 ()
  ((aaa :initform 100)))

(defvar *setf-slot-value-using-class-1-instance*
  (make-instance 'setf-slot-value-using-class-1))

(defvar *setf-slot-value-using-class-1-check*)

(defmethod (setf slot-value-using-class)
  (value (class standard-class) (inst setf-slot-value-using-class-1) name)
  (declare (ignore value class inst name))
  (setq *setf-slot-value-using-class-1-check* t)
  nil)

(deftest setf-slot-value-using-class.1
  (let ((inst *setf-slot-value-using-class-1-instance*)
        (*setf-slot-value-using-class-1-check*))
    (values
      (setf (slot-value inst 'aaa) 200)
      (slot-value inst 'aaa)
      *setf-slot-value-using-class-1-check*))
  nil 100 t)


;;
;;  Generic Function SLOT-BOUNDP-USING-CLASS
;;
(defclass slot-boundp-using-class-1 () (aaa))

(defmethod slot-boundp-using-class
  ((class standard-class) (inst slot-boundp-using-class-1) name)
  (declare (ignore class inst name))
  100)

(deftest slot-boundp-using-class.1
  (let ((inst (make-instance 'slot-boundp-using-class-1)))
    (slot-boundp inst 'aaa))
  100)

;;  slot-missing
(deftest slot-boundp-using-class.2
  (let ((inst (make-instance 'slot-boundp-using-class-1)))
    (slot-boundp inst 'bbb))
  100)


;;
;;  Generic Function SLOT-EXISTS-P-USING-CLASS
;;
(defclass slot-exists-p-using-class-1 () (aaa))

(defmethod slot-exists-p-using-class
  ((class standard-class) (inst slot-exists-p-using-class-1) name)
  (declare (ignore class inst name))
  222)

(deftest slot-exists-p-using-class.1
  (let ((inst (make-instance 'slot-exists-p-using-class-1)))
    (slot-exists-p inst 'aaa))
  222)


;;
;;  Generic Function SLOT-MAKUNBOUND-USING-CLASS
;;
(defclass slot-makunbound-using-class-1 ()
  ((aaa :initform 100)))

(defmethod slot-makunbound-using-class
  ((class standard-class) (inst slot-makunbound-using-class-1) name)
  (declare (ignore class inst name))
  333)

(deftest slot-makunbound-using-class.1
  (let ((inst (make-instance 'slot-makunbound-using-class-1)))
    (slot-makunbound inst 'aaa))
  333)

;;  slot-missing
(defclass slot-makunbound-using-class-2 ()
  ((aaa :initform 100)
   bbb))

(defmethod slot-makunbound-using-class
  ((class standard-class) (inst slot-makunbound-using-class-2) name)
  (declare (ignore class inst name))
  444)

(deftest slot-makunbound-using-class.2
  (let ((inst (make-instance 'slot-makunbound-using-class-2)))
    (slot-makunbound inst 'ccc))
  444)


;;
;;  Generic Function MAKE-METHOD-LAMBDA
;;
(defmacro method-lambda (args &body body)
  `(make-method-lambda
     (class-prototype (find-class 'standard-generic-function))
     (class-prototype (find-class 'standard-method))
     '(lambda ,args ,@body)
     nil))

(deftest make-method-lambda.1
  (listp
    (method-lambda (a) (1+ a)))
  t)

(deftest make-method-lambda.2
  (functionp
    (eval
      (method-lambda (a) (1+ a))))
  t)

(deftest make-method-lambda.3
  (funcall
    (eval
      (method-lambda (a) (1+ a)))
    nil nil 100)
  101)

