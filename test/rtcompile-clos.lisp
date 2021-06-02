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


;;
;;  structure
;;
(defstruct compile-structure-1 value)

(deftest compile-structure.1
  (compile-structure-1-value
    (expr-compile
      (make-compile-structure-1 :value 10)))
  10)

(deftest compile-structure.2
  (values
    (compile-structure-1-value
      (expr-compile (make-compile-structure-1 :value 10)))
    (compile-structure-1-value
      (expr-compile (make-compile-structure-1 :value 20)))
    (compile-structure-1-value
      (expr-compile (make-compile-structure-1 :value 30))))
  10 20 30)

(defstruct compile-structure-3 aaa bbb ccc)

(deftest compile-structure.3
  (let ((x (expr-compile
             (make-compile-structure-3
               :aaa 10
               :bbb 20
               :ccc 30))))
    (values
      (compile-structure-3-aaa x)
      (compile-structure-3-bbb x)
      (compile-structure-3-ccc x)))
  10 20 30)


;;
;;  quote
;;
(defstruct compile-quote-clos-1 aaa bbb ccc)
(defmethod make-load-form ((x compile-quote-clos-1) &optional env)
  (make-load-form-saving-slots x :environment env))

(deftest compile-quote-clos.1
  (compile-quote-clos-1-aaa
    (expr-compile
      #,(make-compile-quote-clos-1 :aaa 10)))
  10)

(deftest compile-quote-clos.2
  (compile-quote-clos-1-aaa
    (expr-compile
      '#,(make-compile-quote-clos-1 :aaa 10)))
  10)

(deftest-error compile-quote-clos.3
  (compile-quote-clos-1-aaa
    (expr-compile
      '(#,(make-compile-quote-clos-1 :aaa 10))))
  type-error)

(deftest-error compile-quote-clos.4
  (compile-quote-clos-1-aaa
    (expr-compile
      `(#,(make-compile-quote-clos-1 :aaa 10))))
  type-error)

