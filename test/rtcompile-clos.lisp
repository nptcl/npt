;;
;;  compile-clos
;;

;;
;;  form
;;
(defclass compile-clos-form-1 () ())

(deftest-error compile-clos-form.1
  (expr-compile
    #,(make-instance 'compile-clos-form-1)))

(defclass compile-clos-form-2 ()
  ((value :initarg :value)))

(defmethod make-load-form ((x compile-clos-form-2) &optional env)
  (declare (ignore x env))
  `(make-instance 'compile-clos-form-2 :value 123))

(deftest compile-clos-form.2
  (slot-value
    (expr-compile
      #,(make-instance 'compile-clos-form-2))
    'value)
  123)

(defclass compile-clos-form-3 ()
  ((value :initarg :value)))

(defmethod make-load-form ((x compile-clos-form-3) &optional env)
  (declare (ignore x env))
  `(make-instance 'compile-clos-form-3 :value 456))

(deftest compile-clos-form.3
  (values
    (slot-value
      (expr-compile #,(make-instance 'compile-clos-form-2))
      'value)
    (slot-value
      (expr-compile #,(make-instance 'compile-clos-form-3))
      'value)
    (slot-value
      (expr-compile #,(make-instance 'compile-clos-form-2))
      'value)
    (slot-value
      (expr-compile #,(make-instance 'compile-clos-form-3))
      'value))
  123 456 123 456)


;;
;;  init
;;
(defclass compile-clos-init-1 ()
  ((value :initarg :value)))

(defmethod make-load-form ((x compile-clos-init-1) &optional env)
  (declare (ignore env))
  (values
    `(make-instance 'compile-clos-init-1)
    `(setf (slot-value ,x 'value) ,(+ 111 (slot-value x 'value)))))

(deftest compile-clos-init.1
  (slot-value
    (expr-compile #,(make-instance 'compile-clos-init-1 :value 10))
    'value)
  121)

(defclass compile-clos-init-2 ()
  ((value :initarg :value)))

(defmethod make-load-form ((x compile-clos-init-2) &optional env)
  (declare (ignore env))
  (values
    `(make-instance 'compile-clos-init-2)
    `(setf (slot-value ,x 'value) ,(+ 222 (slot-value x 'value)))))

(deftest compile-clos-init.2
  (values
    (slot-value
      (expr-compile #,(make-instance 'compile-clos-init-1 :value 10))
      'value)
    (slot-value
      (expr-compile #,(make-instance 'compile-clos-init-2 :value 20))
      'value)
    (slot-value
      (expr-compile #,(make-instance 'compile-clos-init-2 :value 30))
      'value))
  121 242 252)

