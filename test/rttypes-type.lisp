;;
;;  ANSI COMMON LISP: 4. Types and Classes
;;

;;
;;  Type NIL
;;
(deftest nil-type.1
  (typep nil nil)
  nil)

(deftest nil-type.2
  (typep t nil)
  nil)

(deftest nil-type.3
  (typep 10 nil)
  nil)


;;
;;  System Class T
;;
(deftest t-type.1
  (lisp-system:closp
    (find-class 't))
  t)

(deftest t-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 't)))
  (t))

(deftest t-type.3
  (typep nil t)
  t)

(deftest t-type.4
  (typep 10 t)
  t)

(deftest t-type.5
  (typep t t)
  t)


;;
;;  Type BOOLEAN
;;
(deftest boolean-type.1
  (subtypep 'boolean 'symbol)
  t t)

(deftest boolean-type.2
  (typep t 'boolean)
  t)

(deftest boolean-type.3
  (typep nil 'boolean)
  t)

(deftest boolean-type.4
  (typep 10 'boolean)
  nil)


;;
;;  System Class FUNCTION
;;
(deftest function-type.1
  (lisp-system:closp
    (find-class 'function))
  t)

(deftest function-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'function)))
  (function t))

(deftest function-type.3
  (typep #'car 'function)
  t)

(deftest function-type.4
  (typep 'car 'function)
  nil)

(deftest function-type.5
  (typep (lambda () :hello) 'function)
  t)


;;
;;  Type COMPILED-FUNCTION
;;
(deftest compiled-function-type.1
  (subtypep 'compiled-function 'function)
  t t)

(deftest compiled-function-type.2
  (typep #'car 'compiled-function)
  t)

(deftest compiled-function-type.3
  (typep (lambda () :hello) 'compiled-function)
  nil)

(deftest compiled-function-type.4
  (typep 10 'compiled-function)
  nil)


;;
;;  System Class GENERIC-FUNCTION
;;
(deftest generic-function-type.1
  (lisp-system:closp
    (find-class 'generic-function))
  t)

(deftest generic-function-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'generic-function)))
  (generic-function
    lisp-clos::metaobject lisp-clos::funcallable-standard-object
    function standard-object t))

(deftest generic-function-type.3
  (typep 10 'generic-function)
  nil)

(deftest generic-function-type.4
  (typep (make-instance 'generic-function) 'generic-function)
  t)

(defgeneric generic-function-type-test-1 ())

(deftest generic-function-type.5
  (typep #'generic-function-type-test-1 'generic-function)
  t)


;;
;;  System Class STANDARD-GENERIC-FUNCTION
;;
(deftest standard-generic-function-type.1
  (lisp-system:closp
    (find-class 'standard-generic-function))
  t)

(deftest standard-generic-function-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'standard-generic-function)))
  (standard-generic-function
    generic-function lisp-clos::metaobject
    lisp-clos::funcallable-standard-object
    function standard-object t))

(deftest standard-generic-function-type.3
  (typep 10 'standard-generic-function)
  nil)

(deftest standard-generic-function-type.4
  (typep (make-instance 'generic-function) 'standard-generic-function)
  nil)

(deftest standard-generic-function-type.5
  (typep (make-instance 'standard-generic-function) 'standard-generic-function)
  t)

(defgeneric standard-generic-function-type-test-1 ())

(deftest standard-generic-function-type.6
  (typep #'standard-generic-function-type-test-1 'standard-generic-function)
  t)


;;
;;  System Class CLASS
;;
(deftest class-type.1
  (lisp-system:closp
    (find-class 'class))
  t)

(deftest class-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'class)))
  (class lisp-clos::specializer lisp-clos::metaobject standard-object t))

(deftest class-type.3
  (typep 10 'class)
  nil)

(deftest class-type.4
  (typep (make-instance 'class) 'class)
  t)

(defclass class-type-1 () ())

(deftest class-type.5
  (typep (find-class 'class-type-1) 'class)
  t)

(defstruct class-type-2)

(deftest class-type.6
  (typep (find-class 'class-type-2) 'class)
  t)


;;
;;  System Class BUILT-IN-CLASS
;;
(deftest built-in-class-type.1
  (lisp-system:closp
    (find-class 'built-in-class))
  t)

(deftest built-in-class-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'built-in-class)))
  (built-in-class
    class lisp-clos::specializer lisp-clos::metaobject standard-object t))

(deftest built-in-class-type.3
  (typep 10 'built-in-class)
  nil)

(deftest built-in-class-type.4
  (typep (find-class 'integer) 'built-in-class)
  t)

(defclass built-in-class-type-1 () ())

(deftest built-in-class-type.5
  (typep (find-class 'built-in-class-type-1) 'built-in-class)
  nil)

(defstruct built-in-class-type-2)

(deftest built-in-class-type.6
  (typep (find-class 'built-in-class-type-2) 'built-in-class)
  nil)

(deftest-error built-in-class-type.7
  (eval '(defclass build-in-class-type-3 (integer) ()))
  type-error)

(deftest-error built-in-class-type.8
  (make-instance 'built-in-class)
  type-error)

(deftest-error built-in-class-type.9
  (eval '(slot-value 10 'hello))
  type-error)

(deftest-error built-in-class-type.10
  (defclass built-in-class () (aaa))
  type-error)

(defclass buit-in-class-type-4 () ())

(deftest-error built-in-class-type.11
  (change-class
    (make-instance 'buit-in-class-type-4)
    'built-in-class)
  type-error)

(defgeneric built-in-class-type-5 (x))

(defmethod built-in-class-type-5 ((x integer))
  (declare (ignore x))
  :aaa)

(defmethod built-in-class-type-5 ((x cons))
  (declare (ignore x))
  :bbb)

(deftest built-in-class-type.12
  (values
    (built-in-class-type-5 10)
    (built-in-class-type-5 (cons 20 30)))
  :aaa :bbb)


;;
;;  System Class STRUCTURE-CLASS
;;
(deftest structure-class-type.1
  (lisp-system:closp
    (find-class 'structure-class))
  t)

(deftest structure-class-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'structure-class)))
  (structure-class
    class lisp-clos::specializer lisp-clos::metaobject standard-object t))

(deftest structure-class-type.3
  (typep 10 'structure-class)
  nil)

(defclass structure-class-type-1 () ())

(deftest structure-class-type.4
  (typep (find-class 'structure-class-type-1) 'structure-class)
  nil)

(defstruct structure-class-type-2)

(deftest structure-class-type.5
  (typep (find-class 'structure-class-type-2) 'structure-class)
  t)

(deftest structure-class-type.6
  (typep (make-structure-class-type-2) 'structure-class)
  nil)


;;
;;  System Class STANDARD-CLASS
;;
(deftest standard-class-type.1
  (lisp-system:closp
    (find-class 'standard-class))
  t)

(deftest standard-class-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'standard-class)))
  (standard-class
    class lisp-clos::specializer lisp-clos::metaobject standard-object t))

(deftest standard-class-type.3
  (typep 10 'standard-class)
  nil)

(defclass standard-class-type-1 () ())

(deftest standard-class-type.4
  (typep (find-class 'standard-class-type-1) 'standard-class)
  t)

(defstruct standard-class-type-2)

(deftest standard-class-type.5
  (typep (find-class 'standard-class-type-2) 'standard-class)
  nil)

(deftest standard-class-type.6
  (typep (make-instance 'standard-class-type-2) 'standard-class)
  nil)


;;
;;  System Class METHOD
;;
(deftest method-type.1
  (lisp-system:closp
    (find-class 'method))
  t)

(deftest method-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'method)))
  (method lisp-clos::metaobject standard-object t))

(deftest method-type.3
  (typep 10 'method)
  nil)

(deftest method-type.4
  (typep (make-instance 'method) 'method)
  t)

(defgeneric method-type-test-1 ())

(deftest method-type.5
  (typep #'method-type-test-1 'method)
  nil)

(deftest method-type.6
  (typep (defmethod method-type-test-1 () :hello) 'method)
  t)


;;
;;  System Class STANDARD-METHOD
;;
(deftest standard-method-type.1
  (lisp-system:closp
    (find-class 'standard-method))
  t)

(deftest standard-method-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'standard-method)))
  (standard-method method lisp-clos::metaobject standard-object t))

(deftest standard-method-type.3
  (typep 10 'standard-method)
  nil)

(deftest standard-method-type.4
  (typep (make-instance 'standard-method) 'standard-method)
  t)

(defgeneric standard-method-type-test-1 ())

(deftest standard-method-type.5
  (typep #'standard-method-type-test-1 'standard-method)
  nil)

(deftest standard-method-type.6
  (typep (defmethod standard-method-type-test-1 () :hello) 'standard-method)
  t)


;;
;;  Class STRUCTURE-OBJECT
;;
(deftest structure-object-type.1
  (lisp-system:closp
    (find-class 'structure-object))
  t)

(deftest structure-object-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'structure-object)))
  (structure-object t))

(deftest structure-object-type.3
  (typep 10 'structure-object)
  nil)

(defclass structure-object-type-1 () ())

(deftest structure-object-type.4
  (typep (make-instance 'structure-object-type-1) 'structure-object)
  nil)

(defstruct structure-object-type-2)

(deftest structure-object-type.5
  (typep (make-structure-object-type-2) 'structure-object)
  t)

(deftest structure-object-type.6
  (typep (find-class 'structure-object-type-1) 'structure-object)
  nil)

(deftest structure-object-type.7
  (typep (find-class 'structure-object-type-2) 'structure-object)
  nil)


;;
;;  Class STANDARD-OBJECT
;;
(deftest standard-object-type.1
  (lisp-system:closp
    (find-class 'standard-object))
  t)

(deftest standard-object-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'standard-object)))
  (standard-object t))

(deftest standard-object-type.3
  (typep 10 'standard-object)
  nil)

(defclass standard-object-type-1 () ())

(deftest standard-object-type.4
  (typep (make-instance 'standard-object-type-1) 'standard-object)
  t)

(defstruct standard-object-type-2)

(deftest standard-object-type.5
  (typep (make-structure-object-type-2) 'standard-object)
  nil)

(deftest standard-object-type.6
  (typep (find-class 'standard-object-type-1) 'standard-object)
  t)

(deftest standard-object-type.7
  (typep (find-class 'standard-object-type-2) 'standard-object)
  t)


;;
;;  System Class METHOD-COMBINATION
;;
(deftest method-combination-type.1
  (lisp-system:closp
    (find-class 'method-combination))
  t)

(deftest method-combination-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'method-combination)))
  (method-combination lisp-clos::metaobject standard-object t))

(deftest method-combination-type.3
  (typep 10 'method-combination)
  nil)


;;
;;  System Class T
;;
(deftest t-class-type.1
  (lisp-system:closp
    (find-class 't))
  t)

(deftest t-class-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 't)))
  (t))

(deftest t-class-type.3
  (typep 10 't)
  t)


;;
;;  Condition Type TYPE-ERROR
;;
(deftest type-error-type.1
  (lisp-system:closp
    (find-class 'type-error))
  t)

(deftest type-error-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'type-error)))
  (type-error error serious-condition condition standard-object t))

(deftest type-error-type.3
  (typep 10 'type-error)
  nil)

(deftest type-error-type.4
  (typep
    (make-instance 'type-error :datum 'x :expected-type 'integer)
    'type-error)
  t)

(deftest type-error-type.5
  (handler-case
    (eval '(car 10))
    (type-error () :hello))
  :hello)


;;
;;  Function TYPE-ERROR-DATUM
;;
(deftest type-error-datum.1
  (type-error-datum
    (make-instance 'type-error :datum 'x :expected-type 'integer))
  x)

(deftest-error! type-error-datum-error.1
  (eval '(type-error-datum)))

(deftest-error! type-error-datum-error.2
  (eval '(type-error-datum
           (make-instance 'type-error)
           nil)))

(deftest-error type-error-datum-error.3
  (eval '(type-error-datum 10))
  type-error)


;;
;;  Function TYPE-ERROR-EXPECTED-TYPE
;;
(deftest type-error-expected-type.1
  (type-error-expected-type
    (make-instance 'type-error :datum 'x :expected-type 'integer))
  integer)

(deftest-error! type-error-expected-type-error.1
  (eval '(type-error-expected-type)))

(deftest-error! type-error-expected-type-error.2
  (eval '(type-error-expected-type
           (make-instance 'type-error)
           nil)))

(deftest-error type-error-expected-type-error.3
  (eval '(type-error-expected-type 10))
  type-error)

;;  ANSI Common Lisp
(defun type-error-fix-digits (condition)
  (check-type condition type-error)
  (let* ((digits '(zero one two three four five six seven eight nine))
         (val (position (type-error-datum condition) digits)))
    (if (and val (subtypep 'fixnum (type-error-expected-type condition)))
      (store-value 7))))

(defun type-error-foo (x)
  (handler-bind ((type-error #'type-error-fix-digits))
    (check-type x number)
    (+ x 3)))

(deftest type-error-test.1
  (type-error-foo 'seven)
  10)


;;
;;  Condition Type SIMPLE-TYPE-ERROR
;;
(deftest simple-type-error-type.1
  (lisp-system:closp
    (find-class 'simple-type-error))
  t)

(deftest simple-type-error-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'simple-type-error)))
  (simple-type-error
    simple-condition type-error error
    serious-condition condition standard-object t))

(deftest simple-type-error-type.3
  (typep 10 'simple-type-error)
  nil)

(deftest simple-type-error-type.4
  (typep
    (make-instance 'simple-type-error)
    'simple-type-error)
  t)

(deftest simple-type-error-type.5
  (handler-case
    (error
      (make-instance 'simple-type-error))
    (simple-type-error () :hello))
  :hello)

(deftest simple-type-error-type.6
  (let ((inst (make-instance 'simple-type-error
                             :format-control "Hello: ~S"
                             :format-arguments '(10)
                             :datum 'x
                             :expected-type 'integer)))
    (values
      (simple-condition-format-control inst)
      (simple-condition-format-arguments inst)
      (type-error-datum inst)
      (type-error-expected-type inst)))
  "Hello: ~S" (10) x integer)

