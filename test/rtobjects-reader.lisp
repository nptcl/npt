;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  class-name
;;
(deftest class-name.1
  (class-name
    (find-class 'standard-class))
  standard-class)

(deftest class-name.2
  (class-name
    (find-class 'string))
  string)

(defclass class-name1 () ())

(deftest setf-class-name.1
  (progn
    (setf (class-name
            (find-class 'class-name1))
          'class-name2)
    (values
      (closp (find-class 'class-name1 nil))
      (closp (find-class 'class-name2 nil))
      (class-name (find-class 'class-name1))))
  t nil class-name2)


;;
;;  class-slots
;;
(defclass class-slots1 ()
  (aaa bbb))

(deftest class-slots.1
  (mapcar
    #'slot-definition-name
    (class-slots
      (find-class 'class-slots1)))
  (aaa bbb))

(defclass class-slots2 (class-slots1)
  (ccc ddd))

(deftest class-slots.2
  (mapcar
    #'slot-definition-name
    (class-slots
      (find-class 'class-slots2)))
  (ccc ddd aaa bbb))

(deftest class-slots.3
  (listp
    (class-slots
      (find-class 'string)))
  t)


;;
;;  class-direct-slots
;;
(deftest class-direct-slots.1
  (mapcar
    #'slot-definition-name
    (class-direct-slots
      (find-class 'class-slots1)))
  (aaa bbb))

(deftest class-direct-slots.2
  (mapcar
    #'slot-definition-name
    (class-direct-slots
      (find-class 'class-slots2)))
  (ccc ddd))

(deftest class-direct-slots.3
  (listp
    (class-direct-slots
      (find-class 'number)))
  t)


;;
;;  class-default-initargs
;;
(defclass class-initargs1 ()
  (aaa bbb)
  (:default-initargs xxx 100 yyy 200))

(deftest class-default-initargs.1
  (class-default-initargs
    (find-class 'class-slots1))
  nil)

(deftest class-default-initargs.2
  (mapcar #'car
          (class-default-initargs
            (find-class 'class-initargs1)))
  (xxx yyy))

(deftest class-default-initargs.3
  (length
    (car (class-default-initargs
           (find-class 'class-initargs1))))
  3)

(deftest class-default-initargs.4
  (listp
    (car (class-default-initargs
           (find-class 'integer))))
  t)


;;
;;  class-direct-default-initargs
;;
(deftest class-direct-default-initargs.1
  (class-direct-default-initargs
    (find-class 'class-slots1))
  nil)

(deftest class-direct-default-initargs.2
  (mapcar #'car
          (class-direct-default-initargs
            (find-class 'class-initargs1)))
  (xxx yyy))

(deftest class-direct-default-initargs.3
  (length
    (car (class-direct-default-initargs
           (find-class 'class-initargs1))))
  3)

(deftest class-direct-default-initargs.4
  (listp
    (class-direct-default-initargs
      (find-class 'string)))
  t)


;;
;;  class-precedence-list
;;
(defclass precedence1 () ())

(deftest class-precedence-list.1
  (mapcar #'class-name
          (class-precedence-list
            (find-class 'precedence1)))
  (precedence1 standard-object t))

(defclass precedence2 (precedence1) ())
(defclass precedence3 (precedence2) ())

(deftest class-precedence-list.2
  (mapcar #'class-name
          (class-precedence-list
            (find-class 'precedence3)))
  (precedence3 precedence2 precedence1 standard-object t))

(deftest class-precedence-list.3
  (listp
    (class-precedence-list
      (find-class 'real)))
  t)


;;
;;  class-direct-superclasses
;;
(deftest class-direct-superclasses.1
  (mapcar #'class-name
          (class-direct-superclasses
            (find-class 'precedence3)))
  (precedence2))

(defclass direct-superclasses1 () ())
(defclass direct-superclasses2 () ())
(defclass direct-superclasses3 (direct-superclasses1 direct-superclasses2) ())

(deftest class-direct-superclasses.2
  (mapcar #'class-name
          (class-direct-superclasses
            (find-class 'direct-superclasses3)))
  (direct-superclasses1 direct-superclasses2))

(deftest class-direct-superclasses.3
  (listp
    (class-direct-superclasses
      (find-class 'rational)))
  t)


;;
;;  class-direct-subclasses
;;
(deftest class-direct-subclasses.1
  (mapcar #'class-name
          (class-direct-subclasses
            (find-class 'direct-superclasses3)))
  nil)

(deftest class-direct-subclasses.2
  (mapcar #'class-name
          (class-direct-subclasses
            (find-class 'direct-superclasses1)))
  (direct-superclasses3))

(deftest class-direct-subclasses.3
  (listp
    (class-direct-subclasses
      (find-class 'number)))
  t)


;;
;;  class-finalized-p
;;
(deftest class-finalized-p.1
  (class-finalized-p
    (find-class 'standard-class))
  t)

(defclass finalized1 () ())

(deftest class-finalized-p.2
  (progn
    (make-instance 'finalized1)
    (class-finalized-p
      (find-class 'finalized1)))
  t)

(defclass finalized2 (no-such-finalized-class) ())

(deftest class-finalized-p.3
  (class-finalized-p
    (find-class 'finalized2))
  nil)

(deftest class-finalized-p.4
  (class-finalized-p
    (find-class 'string))
  t)


;;
;;  prototype
;;
(deftest class-prototype.1
  (progn
    (class-prototype
      (find-class 'standard-class))
    (values)))

(deftest class-prototype.2
  (progn
    (class-prototype
      (find-class 'number))
    (values)))


;;
;;  slot-definition-name
;;
(defclass test-slot-definition1 ()
  ((aaa :initarg hello :initform "abc" :type string)))

(defun test-slot-first (name)
  (car (class-slots
         (find-class name))))

(deftest slot-definition-name.1
  (slot-definition-name
    (test-slot-first 'test-slot-definition1))
  aaa)



;;
;;  slot-definition-type
;;
(deftest slot-definition-type.1
  (slot-definition-type
    (test-slot-first 'test-slot-definition1))
  string)


;;
;;  slot-definition-allocation
;;
(deftest slot-definition-allocation.1
  (slot-definition-allocation
    (test-slot-first 'test-slot-definition1))
  :instance)

(defclass test-slot-definition2 ()
  ((aaa :allocation :class)))

(deftest slot-definition-allocation.2
  (slot-definition-allocation
    (test-slot-first 'test-slot-definition2))
  :class)


;;
;;  slot-definition-initargs
;;
(deftest slot-definition-initargs.1
  (slot-definition-initargs
    (test-slot-first 'test-slot-definition1))
  (hello))

(defclass test-slot-definition3 ()
  ((aaa :initarg :aaa :initarg :bbb :initarg ccc :initform "Hello")))

(deftest slot-definition-initargs.2
  (slot-definition-initargs
    (test-slot-first 'test-slot-definition3))
  (:aaa :bbb ccc))


;;
;;  slot-definition-initform
;;
(deftest slot-definition-initform.1
  (slot-definition-initform
    (test-slot-first 'test-slot-definition1))
  "abc")

(defclass test-slot-definition4 ()
  (aaa))

(deftest-error slot-definition-initform.2
  (slot-definition-initform
    (test-slot-first 'test-slot-definition4))
  unbound-slot)


;;
;;  slot-definition-initfunction
;;
(deftest slot-definition-initfunction.1
  (funcall
    (slot-definition-initfunction
      (test-slot-first 'test-slot-definition1)))
  "abc")

(deftest slot-definition-initfunction.2
  (null
    (slot-definition-initfunction
      (test-slot-first 'test-slot-definition4)))
  t)


;;
;;  generic-function-name
;;
(defgeneric generic-function-name1 (a))

(deftest generic-function-name.1
  (generic-function-name #'generic-function-name1)
  generic-function-name1)

(deftest generic-function-name.2
  (setf (generic-function-name #'generic-function-name1) 'generic-function-name2)
  generic-function-name2)

(deftest generic-function-name.3
  (generic-function-name #'generic-function-name1)
  generic-function-name2)


;;
;;  generic-function-methods
;;
(defgeneric generic-function-methods1 (a))

(deftest generic-function-methods.1
  (generic-function-methods #'generic-function-methods1)
  nil)

(defgeneric generic-function-methods2 (a))
(defmethod generic-function-methods2 (a)
  (+ a 10))

(deftest generic-function-methods.2
  (length
    (generic-function-methods #'generic-function-methods2))
  1)

(deftest generic-function-methods.3
  (typep
    (car (generic-function-methods #'generic-function-methods2))
    'standard-method)
  t)

(defgeneric generic-function-methods4 (a))
(defmethod generic-function-methods4 (a)
  (+ a 10))

(defmethod generic-function-methods4 :around (a)
  (+ a 20))

(deftest generic-function-methods.4
  (length
    (generic-function-methods #'generic-function-methods4))
  2)


;;
;;  generic-function-methods
;;
(defgeneric generic-function-lambda-list1 (a b c &optional d))

(deftest generic-function-lambda-list.1
  (equal
    (generic-function-lambda-list
      #'generic-function-lambda-list1)
    '(a b c &optional d))
  t)


;;
;;  generic-function-methods
;;
(defgeneric generic-function-argument-precedence-order1 ())
(deftest generic-function-argument-precedence-order.1
  (generic-function-argument-precedence-order
    #'generic-function-argument-precedence-order1)
  nil)

(defgeneric generic-function-argument-precedence-order2 (a b c)
            (:argument-precedence-order c a b))
(deftest generic-function-argument-precedence-order.2
  (generic-function-argument-precedence-order
    #'generic-function-argument-precedence-order2)
  (c a b))


;;
;;  generic-function-declarations
;;
(defgeneric generic-function-declarations1 (a))
(deftest generic-function-declarations.1
  (generic-function-declarations
    #'generic-function-declarations1)
  nil)


;;
;;  generic-function-method-class
;;
(defgeneric generic-function-method-class1 (a b c))
(deftest generic-function-method-class.1
  (class-name
    (generic-function-method-class
      #'generic-function-method-class1))
  standard-method)

(defclass generic-function-method-class2 (standard-method) ())
(defgeneric generic-function-method-class3 (a b)
            (:method-class generic-function-method-class2))
(deftest generic-function-method-class.2
  (class-name
    (generic-function-method-class
      #'generic-function-method-class3))
  generic-function-method-class2)


;;
;;  generic-function-method-combination
;;
(defgeneric generic-function-method-combination1 (a))
(deftest generic-function-method-combination.1
  (slot-value
    (generic-function-method-combination
      #'generic-function-method-combination1)
    'lisp-clos::name)
  standard)

(defgeneric generic-function-method-combination2 (a)
            (:method-combination progn))
(deftest generic-function-method-combination.2
  (slot-value
    (generic-function-method-combination
      #'generic-function-method-combination2)
    'lisp-clos::name)
  progn)


;;
;;  method-function
;;
(defgeneric method-function1 (a))
(defmethod method-function1 (a)
  (+ a 10))
(deftest method-function.1
  (functionp
    (method-function
      (car (generic-function-methods #'method-function1))))
  t)

(deftest method-function.2
  (typep
    (method-function
      (car (generic-function-methods #'method-function1)))
    'generic-function)
  nil)


;;
;;  method-generic-function
;;
(defgeneric method-generic-function1 (a))
(defmethod method-generic-function1 (a)
  a)
(deftest method-generic-function.1
  (generic-function-name
    (method-generic-function
      (car (generic-function-methods #'method-generic-function1))))
  method-generic-function1)

(deftest method-generic-function.2
  (class-name
    (class-of
      (method-generic-function
        (car (generic-function-methods #'method-generic-function1)))))
  standard-generic-function)


;;
;;  method-lambda-list
;;
(defgeneric method-lambda-list1 (a &rest b))
(defmethod method-lambda-list1 (b &key)
  (declare (ignore b)))
(deftest method-lambda-list.1
  (method-lambda-list
    (car (generic-function-methods #'method-lambda-list1)))
  (b &key))

(defgeneric method-lambda-list2 (a b))
(defmethod method-lambda-list2 ((c integer) d)
  (declare (ignore c d)))
(deftest method-lambda-list.2
  (method-lambda-list
    (car (generic-function-methods #'method-lambda-list2)))
  (c d))


;;
;;  method-specializers
;;
(defgeneric method-specializers1 (a b c))
(defmethod method-specializers1 ((a integer) (b string) c)
  (values a b c))
(deftest method-specializers.1
  (mapcar
    #'class-name
    (method-specializers
      (car (generic-function-methods #'method-specializers1))))
  (integer string t))


;;
;;  method-qualifiers
;;
(defgeneric method-qualifiers1 (a))
(defmethod method-qualifiers1 (a)
  a)
(deftest method-qualifiers.1
  (method-qualifiers
    (car (generic-function-methods #'method-qualifiers1)))
  nil)

(defgeneric method-qualifiers2 (a))
(defmethod method-qualifiers2 :around (a)
  a)
(deftest method-qualifiers.2
  (method-qualifiers
    (car (generic-function-methods #'method-qualifiers2)))
  (:around))


;;
;;  accessor-method-slot-definition
;;
(defgeneric accessor-method-slot-definition1 (a))
(defmethod accessor-method-slot-definition1 (a)
  a)
(deftest-error accessor-method-slot-definition.1
  (accessor-method-slot-definition
    (car (generic-function-methods #'accessor-method-slot-definition1))))

