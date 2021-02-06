;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  Macro DEFMETHOD
;;
(defgeneric defmethod-eql-specializer-1 (value))

(defmethod defmethod-eql-specializer-1 (value)
  value)

(deftest defmethod-eql-specializer.1
  (typep
    (defmethod defmethod-eql-specializer-1 ((value (eql 100)))
      (format nil "Hello: ~A" value))
    'standard-method)
  t)

(deftest defmethod-eql-specializer.2
  (defmethod-eql-specializer-1 10)
  10)

(deftest defmethod-eql-specializer.3
  (defmethod-eql-specializer-1 100)
  "Hello: 100")


;;  Local Function NEXT-METHOD-P
;;  Local Function CALL-NEXT-METHOD
;;  Standard Generic Function METHOD-QUALIFIERS
;;  Standard Generic Function NO-APPLICABLE-METHOD
;;  Standard Generic Function NO-NEXT-METHOD
;;  Standard Generic Function REMOVE-METHOD
;;  Standard Generic Function COMPUTE-APPLICABLE-METHODS
;;  Standard Generic Function FIND-METHOD
;;  Standard Generic Function ADD-METHOD

