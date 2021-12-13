;;
;;  ANSI COMMON LISP: 9. Conditions
;;

;;
;;  Macro DEFINE-CONDITION
;;
(deftest define-condition.1
  (define-condition define-condition-1 () ())
  define-condition-1)

(deftest define-condition.2
  (subtypep 'define-condition-1 'condition)
  t t)

(deftest define-condition.3
  (define-condition define-condition-2 () ()
    (:report "Hello"))
  define-condition-2)

(deftest define-condition.4
  (with-output-to-string (stream)
    (princ (make-condition 'define-condition-2) stream))
  "Hello")

(deftest define-condition.5
  (progn
    (define-condition define-condition-3
      (define-condition-1 define-condition-2) ())
    (let ((x (make-condition 'define-condition-3)))
      (values
        (typep x 'condition)
        (typep x 'define-condition-1)
        (typep x 'define-condition-2)
        (typep x 'define-condition-3))))
  t t t t)


;;  initarg
(deftest define-condition-initarg.1
  (progn
    (define-condition define-condition-initarg-1 ()
      ((aaa :initarg :aaaa)))
    (let ((x (make-condition 'define-condition-initarg-1 :aaaa 100)))
      (slot-value x 'aaa)))
  100)

(deftest define-condition-initarg.2
  (progn
    (define-condition define-condition-initarg-2 ()
      ((aaa :initarg abc :initarg def)))
    (let ((x (make-condition 'define-condition-initarg-2 'abc 100))
          (y (make-condition 'define-condition-initarg-2 'def 200)))
      (values
        (slot-value x 'aaa)
        (slot-value y 'aaa))))
  100 200)

(deftest define-condition-initarg.3
  (progn
    (define-condition define-condition-initarg-3 ()
      ((aaa :initarg bbb :initarg ccc :initform nil)
       (ddd :initarg ccc :initarg eee :initform nil)))
    (let ((x (make-condition 'define-condition-initarg-3 'bbb 100))
          (y (make-condition 'define-condition-initarg-3 'ccc 200)))
      (values
        (slot-value x 'aaa)
        (slot-value x 'ddd)
        (slot-value y 'aaa)
        (slot-value y 'ddd))))
  100 nil 200 200)


;;  initform
(deftest define-condition-initform.1
  (progn
    (define-condition define-condition-initform-1 ()
      ((aaa :initform 100 :initarg :aaa)
       (bbb :initform 200 :initarg :bbb)))
    (let ((x (make-condition 'define-condition-initform-1 :aaa 999)))
      (values
        (slot-value x 'aaa)
        (slot-value x 'bbb))))
  999 200)


;;  type
(deftest define-condition-type.1
  (progn
    (define-condition define-condition-type-1 ()
      ((aaa :initform 100 :type integer)))
    (let ((x (make-condition 'define-condition-type-1)))
      (slot-value x 'aaa)))
  100)


;;  allocation
(deftest define-condition-allocation.1
  (progn
    (define-condition define-condition-allocation-1 ()
      ((aaa :initarg :aaa :allocation :instance)))
    (let ((x (make-condition 'define-condition-allocation-1 :aaa 100))
          (y (make-condition 'define-condition-allocation-1 :aaa 200)))
      (values
        (slot-value x 'aaa)
        (slot-value y 'aaa))))
  100 200)

(deftest define-condition-allocation.2
  (progn
    (define-condition define-condition-allocation-2 ()
      ((aaa :initarg :aaa :allocation :class)))
    (let ((x (make-condition 'define-condition-allocation-2 :aaa 100))
          (y (make-condition 'define-condition-allocation-2 :aaa 200)))
      (values
        (slot-value x 'aaa)
        (slot-value y 'aaa))))
  200 200)


;;  reader
(deftest define-condition-reader.1
  (define-condition define-condition-reader-1 ()
    ((aaa :initform 100 :reader define-condition-reader-call-1)))
  define-condition-reader-1)

(deftest define-condition-reader.2
  (let ((x (make-condition 'define-condition-reader-1)))
    (define-condition-reader-call-1 x))
  100)

(deftest define-condition-reader.3
  (typep #'define-condition-reader-call-1 'generic-function)
  t)


;;  writer
(deftest define-condition-writer.1
  (progn
    (define-condition define-condition-writer-1 ()
      ((aaa :writer define-condition-writer-call-1)))
    (let ((x (make-condition 'define-condition-writer-1)))
      (define-condition-writer-call-1 100 x)
      (slot-value x 'aaa)))
  100)

(deftest define-condition-writer.2
  (progn
    (define-condition define-condition-writer-2 ()
      ((aaa :writer (setf define-condition-writer-call-2))))
    (let ((x (make-condition 'define-condition-writer-2)))
      (setf (define-condition-writer-call-2 x) 200)
      (slot-value x 'aaa)))
  200)


;;  accessor
(deftest define-condition-accessor.1
  (progn
    (define-condition define-condition-accessor-1 ()
      ((aaa :accessor define-condition-accessor-call-1)))
    (let ((x (make-condition 'define-condition-accessor-1)))
      (setf (define-condition-accessor-call-1 x) 100)
      (define-condition-accessor-call-1 x)))
  100)


;;  default-initargs
(deftest define-condition-default-initargs.1
  (progn
    (define-condition define-condition-default-initargs-1 ()
      ((aaa :initarg :aaa))
      (:default-initargs :aaa 100))
    (let ((x (make-condition 'define-condition-default-initargs-1)))
      (slot-value x 'aaa)))
  100)


;;  documentation
(deftest define-condition-documentation.1
  (progn
    (define-condition define-condition-documentation-1 () ()
      (:documentation "Hello"))
    (documentation 'define-condition-documentation-1 'type))
  "Hello")


;;  report function
(deftest define-condition-report-function.1
  (define-condition define-condition-report-1 () ()
    (:report
      (lambda (condition stream)
        (declare (ignore condition))
        (format stream "ABC~A" 100))))
  define-condition-report-1)

(deftest define-condition-report-function.2
  (with-output-to-string (stream)
    (princ (make-condition 'define-condition-report-1) stream))
  "ABC100")

(deftest define-condition-report-function.3
  (with-output-to-string (stream)
    (prin1 (make-condition 'define-condition-report-1) stream))
  "ABC100")


;;  report symbol
(defun define-condition-2-report (inst stream)
  (declare (ignore inst))
  (format stream "DEF~A" 200))

(deftest define-condition-report-symbol.1
  (define-condition define-condition-report-2 () ()
    (:report define-condition-2-report))
  define-condition-report-2)

(deftest define-condition-report-symbol.2
  (with-output-to-string (stream)
    (princ (make-condition 'define-condition-report-2) stream))
  "DEF200")

(deftest define-condition-report-symbol.3
  (equal
    (with-output-to-string (stream)
      (prin1 (make-condition 'define-condition-report-2) stream))
    "DEF200")
  nil)


;;  report string
(deftest define-condition-report-string.1
  (define-condition define-condition-report-3 () ()
    (:report "Hello"))
  define-condition-report-3)

(deftest define-condition-report-string.2
  (with-output-to-string (stream)
    (princ (make-condition 'define-condition-report-3) stream))
  "Hello")

(deftest define-condition-report-string.3
  (with-output-to-string (stream)
    (prin1 (make-condition 'define-condition-report-3) stream))
  "Hello")


;;  error
(deftest-error define-condition-error.1
  (eval '(define-condition aaa () () (:metaclass standard-class))))


;;  ANSI Common Lisp
(deftest define-condition-test.1
  (define-condition define-condition-ate-too-much (error)
    ((person :initarg :person :reader define-condition-ate-too-much-person)
     (weight :initarg :weight :reader define-condition-ate-too-much-weight)
     (kind-of-food :initarg :kind-of-food
                   :reader define-condition-ate-too-much-kind-of-food)))
  define-condition-ate-too-much)

(deftest define-condition-test.2
  (define-condition define-condition-ate-too-much-ice-cream
    (define-condition-ate-too-much)
    ((kind-of-food :initform 'ice-cream)
     (flavor       :initarg :flavor
                   :reader define-condition-ate-too-much-ice-cream-flavor
                   :initform 'vanilla ))
    (:report (lambda (condition stream)
               (format stream "~A ate too much ~A ice-cream"
                       (define-condition-ate-too-much-person condition)
                       (define-condition-ate-too-much-ice-cream-flavor condition)))))
  define-condition-ate-too-much-ice-cream)

(deftest define-condition-test.3
  (let ((x (make-condition
             'define-condition-ate-too-much-ice-cream
             :person 'fred
             :weight 300
             :flavor 'chocolate)))
    (format nil "~A" x))
  "FRED ate too much CHOCOLATE ice-cream")


;;
;;  Function MAKE-CONDITION
;;
(deftest make-condition.1
  (progn
    (define-condition make-condition-1 () ())
    (typep
      (make-condition 'make-condition-1)
      'condition))
  t)

(deftest make-condition.2
  (progn
    (define-condition make-condition-2 ()
      ((aaa :initarg :aaa)
       (bbb :initarg :bbb)))
    (let ((x (make-condition 'make-condition-2 :aaa 10 :bbb 20)))
      (values
        (slot-value x 'aaa)
        (slot-value x 'bbb))))
  10 20)

(deftest-error make-condition.3
  (progn
    (defclass make-condition-1 () ())
    (make-condition 'make-condition-1))
  type-error)

(deftest-error! make-condition-error.1
  (eval '(make-condition)))

(deftest-error! make-condition-error.2
  (eval '(make-condition 100))
  type-error)

;;  ANSI Common Lisp
(defvar *make-condition-oops-count* 0)

(deftest make-condition-test.1
  (let ((a (make-condition
             'simple-error
             :format-control "This is your ~:R error."
             :format-arguments (list (incf *make-condition-oops-count*)))))
    (format nil "~A" a))
  "This is your first error.")

(deftest make-condition-test.2
  (let ((a (make-condition
             'simple-error
             :format-control "This is your ~:R error."
             :format-arguments (list (incf *make-condition-oops-count*)))))
    (handler-case
      (error a)
      (simple-error (c)
        (apply #'format nil
               (simple-condition-format-control c)
               (simple-condition-format-arguments c)))))
  "This is your second error.")

