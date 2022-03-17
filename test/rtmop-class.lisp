;;
;;  MetaObject Protocol: Class
;;
(defun mop-precedence-list (symbol)
  (let ((c (find-class symbol)))
    (values
      (mapcar #'class-name (class-direct-superclasses c))
      (mapcar #'class-name (class-precedence-list c)))))

(deftest mop-standard-object.1
  (mop-precedence-list 'standard-object)
  (t)
  (standard-object t))

(deftest mop-structure-object.1
  (mapcar #'class-name
          (class-precedence-list
            (find-class 'structure-object)))
  (structure-object t))

(deftest mop-funcallable-standard-object.1
  (mop-precedence-list 'lisp-clos::funcallable-standard-object)
  (function standard-object)
  (lisp-clos::funcallable-standard-object function standard-object t))

(deftest mop-metaobject.1
  (mop-precedence-list 'lisp-clos::metaobject)
  (standard-object)
  (lisp-clos::metaobject standard-object t))

(deftest mop-generic-function.1
  (mop-precedence-list 'generic-function)
  (lisp-clos::metaobject lisp-clos::funcallable-standard-object)
  (generic-function
    lisp-clos::metaobject lisp-clos::funcallable-standard-object
    function standard-object t))

(deftest mop-standard-generic-function.1
  (mop-precedence-list 'standard-generic-function)
  (generic-function)
  (standard-generic-function
    generic-function lisp-clos::metaobject lisp-clos::funcallable-standard-object
    function standard-object t))

(deftest mop-method.1
  (mop-precedence-list 'method)
  (lisp-clos::metaobject)
  (method lisp-clos::metaobject standard-object t))

(deftest mop-standard-method.1
  (mop-precedence-list 'standard-method)
  (method)
  (standard-method method lisp-clos::metaobject standard-object t))

(deftest mop-standard-accessor-method.1
  (mop-precedence-list 'lisp-clos::standard-accessor-method)
  (standard-method)
  (lisp-clos::standard-accessor-method
    standard-method method lisp-clos::metaobject standard-object t))

(deftest mop-standard-reader-method.1
  (mop-precedence-list 'lisp-clos::standard-reader-method)
  (lisp-clos::standard-accessor-method)
  (lisp-clos::standard-reader-method
    lisp-clos::standard-accessor-method
    standard-method method lisp-clos::metaobject standard-object t))

(deftest mop-standard-writer-method.1
  (mop-precedence-list 'lisp-clos::standard-writer-method)
  (lisp-clos::standard-accessor-method)
  (lisp-clos::standard-writer-method
    lisp-clos::standard-accessor-method
    standard-method method lisp-clos::metaobject standard-object t))

(deftest mop-method-combination.1
  (mop-precedence-list 'method-combination)
  (lisp-clos::metaobject)
  (method-combination lisp-clos::metaobject standard-object t))

(deftest mop-slot-definition.1
  (mop-precedence-list 'lisp-clos::slot-definition)
  (lisp-clos::metaobject)
  (lisp-clos::slot-definition lisp-clos::metaobject standard-object t))

(deftest mop-direct-slot-definition.1
  (mop-precedence-list 'lisp-clos::direct-slot-definition)
  (lisp-clos::slot-definition)
  (lisp-clos::direct-slot-definition
    lisp-clos::slot-definition lisp-clos::metaobject standard-object t))

(deftest mop-effective-slot-definition.1
  (mop-precedence-list 'lisp-clos::effective-slot-definition)
  (lisp-clos::slot-definition)
  (lisp-clos::effective-slot-definition
    lisp-clos::slot-definition lisp-clos::metaobject standard-object t))

(deftest mop-standard-slot-definition.1
  (mop-precedence-list 'lisp-clos::standard-slot-definition)
  (lisp-clos::slot-definition)
  (lisp-clos::standard-slot-definition
    lisp-clos::slot-definition lisp-clos::metaobject standard-object t))

(deftest mop-standard-direct-slot-definition.1
  (mop-precedence-list 'lisp-clos::standard-direct-slot-definition)
  (lisp-clos::standard-slot-definition lisp-clos::direct-slot-definition)
  (lisp-clos::standard-direct-slot-definition
    lisp-clos::standard-slot-definition lisp-clos::direct-slot-definition
    lisp-clos::slot-definition lisp-clos::metaobject standard-object t))

(deftest mop-standard-effective-slot-definition.1
  (mop-precedence-list 'lisp-clos::standard-effective-slot-definition)
  (lisp-clos::standard-slot-definition lisp-clos::effective-slot-definition)
  (lisp-clos::standard-effective-slot-definition
    lisp-clos::standard-slot-definition lisp-clos::effective-slot-definition
    lisp-clos::slot-definition lisp-clos::metaobject standard-object t))

(deftest mop-specializer.1
  (mop-precedence-list 'lisp-clos::specializer)
  (lisp-clos::metaobject)
  (lisp-clos::specializer lisp-clos::metaobject standard-object t))

(deftest mop-eql-specializer.1
  (mop-precedence-list 'lisp-clos::eql-specializer)
  (lisp-clos::specializer)
  (lisp-clos::eql-specializer
    lisp-clos::specializer lisp-clos::metaobject standard-object t))

(deftest mop-class.1
  (mop-precedence-list 'class)
  (lisp-clos::specializer)
  (class lisp-clos::specializer lisp-clos::metaobject standard-object t))

(deftest mop-standard-class.1
  (mop-precedence-list 'standard-class)
  (class)
  (standard-class
    class lisp-clos::specializer lisp-clos::metaobject standard-object t))

(deftest mop-forward-referenced-class.1
  (mop-precedence-list 'lisp-clos::forward-referenced-class)
  (class)
  (lisp-clos::forward-referenced-class
    class lisp-clos::specializer lisp-clos::metaobject standard-object t))

(deftest mop-funcallable-standard-class.1
  (mop-precedence-list 'lisp-clos::funcallable-standard-class)
  (class)
  (lisp-clos::funcallable-standard-class
    class lisp-clos::specializer lisp-clos::metaobject standard-object t))

(deftest mop-built-in-class.1
  (mop-precedence-list 'built-in-class)
  (class)
  (built-in-class
    class lisp-clos::specializer lisp-clos::metaobject standard-object t))

(deftest mop-structure-class.1
  (mop-precedence-list 'structure-class)
  (class)
  (structure-class
    class lisp-clos::specializer lisp-clos::metaobject standard-object t))

