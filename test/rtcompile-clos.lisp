;;
;;  compile-clos
;;
(defclass compile-clos-1 () ())

(deftest-error compile-clos.1
  (expr-compile
    #,(make-instance 'compile-clos-1)))

(defclass compile-clos-2 ()
  ((value :initarg :value)))

(defmethod make-load-form ((x compile-clos-2) &optional env)
  (declare (ignore x env))
  `(make-instance 'compile-clos-2 :value 123))

(deftest compile-clos.2
  (slot-value
    (expr-compile
      #,(make-instance 'compile-clos-2))
    'value)
  123)

(defclass compile-clos-3 ()
  ((value :initarg :value)))

(defmethod make-load-form ((x compile-clos-3) &optional env)
  (declare (ignore x env))
  `(make-instance 'compile-clos-3 :value 456))

(deftest compile-clos.3
  (values
    (slot-value
      (expr-compile #,(make-instance 'compile-clos-2))
      'value)
    (slot-value
      (expr-compile #,(make-instance 'compile-clos-3))
      'value)
    (slot-value
      (expr-compile #,(make-instance 'compile-clos-2))
      'value)
    (slot-value
      (expr-compile #,(make-instance 'compile-clos-3))
      'value))
  123 456 123 456)

