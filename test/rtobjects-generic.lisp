;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  Standard Generic Function NO-APPLICABLE-METHOD
;;
(deftest-error no-applicable-method.1
  (progn
    (sysctl 'recovery 'no-applicable-method)
    (defgeneric no-applicable-method-1 (x))
    (no-applicable-method-1 :aaa)))

(deftest no-applicable-method.2
  (progn
    (defgeneric no-applicable-method-2 (x))
    (defmethod no-applicable-method ((x standard-generic-function) &rest args)
      (declare (ignore x args))
      (values 10 20 30))
    (no-applicable-method-2 :aaa))
  10 20 30)

(deftest no-applicable-method.3
  (progn
    (sysctl 'recovery 'no-applicable-method)
    (defgeneric no-applicable-method-3 (x))
    (defmethod no-applicable-method-3 ((x integer))
      (1+ x))
    (defmethod no-applicable-method ((x standard-generic-function) &rest args)
      (declare (ignore x args))
      (values 10 20 30))
    (no-applicable-method-3 :aaa))
  10 20 30)

(deftest-error no-applicable-method.4
  (progn
    (sysctl 'recovery 'no-applicable-method)
    (no-applicable-method-2 :aaa)))

(deftest no-applicable-method.5
  (progn
    (sysctl 'recovery 'no-applicable-method)
    (defgeneric no-applicable-method-5 (x &rest args))
    (defmethod no-applicable-method-5 ((x string) &rest args)
      (declare (ignore x))
      args)
    (defmethod no-applicable-method ((x standard-generic-function) &rest args)
      (declare (ignore x))
      (cons :hello args))
    (no-applicable-method-5 10 20 30 40))
  (:hello 10 20 30 40))

(deftest no-applicable-method.6
  (sysctl 'recovery 'no-applicable-method)
  t t)


;;
;;  Standard Generic Function NO-NEXT-METHOD
;;
(deftest-error no-next-method.1
  (progn
    (sysctl 'recovery 'no-next-method)
    (defgeneric no-next-method-1 ())
    (defmethod no-next-method-1 ()
      (call-next-method))
    (no-next-method-1)))

(deftest no-next-method.2
  (progn
    (defgeneric no-next-method-2 ())
    (defmethod no-next-method
      ((x standard-generic-function) (y standard-method) &rest args)
      (declare (ignore x y args))
      (values 10 20 30))
    (defmethod no-next-method-2 ()
      (call-next-method))
    (no-next-method-2))
  10 20 30)

(deftest-error no-next-method.3
  (progn
    (sysctl 'recovery 'no-next-method)
    (no-next-method-2 :aaa)))

(deftest no-next-method.4
  (progn
    (defgeneric no-next-method-4 (&rest list))
    (defmethod no-next-method
      ((x standard-generic-function) (y standard-method) &rest args)
      (declare (ignore x y))
      (cons :hello args))
    (defmethod no-next-method-4 (&rest list)
      (declare (ignore list))
      (call-next-method))
    (no-next-method-4 10 20 30 40))
  (:hello 10 20 30 40))

(deftest no-next-method.5
  (progn
    (defgeneric no-next-method-5 (&rest list))
    (defmethod no-next-method
      ((x standard-generic-function) (y standard-method) &rest args)
      (declare (ignore x y))
      (cons :hello args))
    (defmethod no-next-method-5 (&rest list)
      (declare (ignore list))
      (call-next-method 'a 'b 'c))
    (no-next-method-5 10 20 30 40))
  (:hello a b c))

(deftest no-next-method.6
  (sysctl 'recovery 'no-next-method)
  t t)

