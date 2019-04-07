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
  (aaa bbb ccc ddd))

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

