;;
;;  ANSI COMMON LISP: 25. Environment
;;

;;
;;  Standard Generic Function DOCUMENTATION, (SETF DOCUMENTATION)
;;

;;  function
(defun document-function1 ()
  :hello)

(defun (setf document-function1) ()
  :abc)

(deftest documentation-function.1
  (documentation #'document-function1 t)
  nil)

(deftest documentation-function.2
  (documentation #'(setf document-function1) t)
  nil)

(deftest documentation-function.3
  (setf (documentation #'document-function1 t) "Hello")
  "Hello")

(deftest documentation-function.4
  (setf (documentation #'(setf document-function1) t) "ZZYYXX")
  "ZZYYXX")

(deftest documentation-function.5
  (progn
    (setf (documentation #'document-function1 t) "AAABBB")
    (documentation #'document-function1 t))
  "AAABBB")

(deftest documentation-function.6
  (progn
    (setf (documentation #'document-function1 t) "CCCDDD")
    (documentation #'document-function1 t))
  "CCCDDD")

(defun document-function2 ()
  "ABCD"
  :hello)

(defun (setf document-function3) ()
  "EDFG"
  :hello)

(deftest documentation-function.7
  (documentation #'document-function2 t)
  "ABCD")

(deftest documentation-function.8
  (documentation #'(setf document-function3) t)
  "EDFG")

(defun document-function4 ()
  "HIJK")

(defun (setf document-function5) ()
  "LMNO")

(deftest documentation-function.9
  (documentation #'document-function4 t)
  nil)

(deftest documentation-function.10
  (documentation #'(setf document-function5) t)
  nil)

(deftest documentation-function.11
  (setf (documentation #'document-function1 'function) "Hello")
  "Hello")

(deftest documentation-function.12
  (setf (documentation #'(setf document-function1) 'function) "ZZYYXX")
  "ZZYYXX")

(deftest documentation-function.13
  (progn
    (setf (documentation #'document-function1 'function) "AAABBB")
    (documentation #'document-function1 'function))
  "AAABBB")

(deftest documentation-function.14
  (progn
    (setf (documentation #'document-function1 'function) "CCCDDD")
    (documentation #'document-function1 'function))
  "CCCDDD")

(deftest documentation-function.15
  (documentation (lambda () "Hello" :abc) 't)
  "Hello")

(deftest documentation-function.16
  (let ((x (lambda () "Hello" :abc)))
    (setf (documentation x t) "abcd")
    (documentation x t))
  "abcd")


;;  list
(deftest documentation-list.1
  (progn
    (setf (documentation #'(setf document-function1) 'function) "SETF1")
    (documentation '(setf document-function1) 'function))
  "SETF1")

(deftest documentation-list.2
  (progn
    (setf (documentation '(setf document-function1) 'function) "SETF2")
    (documentation '(setf document-function1) 'function))
  "SETF2")


;;  symbol
(deftest documentation-symbol.1
  (setf (documentation 'document-function1 'function) "Symbol1")
  "Symbol1")

(deftest documentation-symbol.2
  (progn
    (setf (documentation #'document-function1 t) "Symbol2")
    (documentation 'document-function1 'function))
  "Symbol2")

(deftest documentation-setf.1
  (setf (documentation 'document-function3 'setf) "Symbol4")
  "Symbol4")

(deftest documentation-setf.2
  (progn
    (setf (documentation 'document-function3 'setf) "Symbol5")
    (documentation 'document-function3 'setf))
  "Symbol5")

(define-setf-expander
  document-setf1 (a)
  (declare (ignore a))
  "Setf1"
  :hello)

(deftest documentation-setf.3
  (documentation 'document-setf1 'setf)
  "Setf1")

(deftest documentation-setf.4
  (setf (documentation 'document-setf1 'setf) "Setf2")
  "Setf2")

(deftest documentation-setf.5
  (progn
    (setf (documentation 'document-setf1 'setf) "Setf3")
    (documentation 'document-setf1 'setf))
  "Setf3")


;;  method-combination
(define-method-combination
  document-combination1 ()
  ((primary () :required t))
  "Combination1"
  `(progn
     :hello))

(defgeneric document-combination2 () (:method-combination document-combination1))

(deftest documentation-method-combination.1
  (documentation
    (lisp-clos::generic-function-method-combination
      #'document-combination2)
    't)
  "Combination1")

(deftest documentation-method-combination.2
  (let ((inst (lisp-clos::generic-function-method-combination
                #'document-combination2)))
    (setf (documentation inst t) "Combination2"))
  "Combination2")

(deftest documentation-method-combination.3
  (let ((inst (lisp-clos::generic-function-method-combination
                #'document-combination2)))
    (setf (documentation inst t) "Combination3")
    (documentation inst t))
  "Combination3")

(define-method-combination document-combination3 :documentation "Combination4")

(defgeneric document-combination4 ()
  (:method-combination document-combination3))

(deftest documentation-method-combination.4
  (documentation
    (lisp-clos::generic-function-method-combination
      #'document-combination4)
    't)
  "Combination4")

(deftest documentation-method-combination.5
  (let ((inst (lisp-clos::generic-function-method-combination
                #'document-combination4)))
    (setf (documentation inst t) "Combination5"))
  "Combination5")

(deftest documentation-method-combination.6
  (let ((inst (lisp-clos::generic-function-method-combination
                #'document-combination4)))
    (setf (documentation inst t) "Combination6")
    (documentation inst t))
  "Combination6")

(define-method-combination
  document-combination5 ()
  ((primary () :required t))
  "Combination7"
  `(progn
     :hello))

(defgeneric document-combination6 ()
  (:method-combination document-combination5))

(deftest documentation-method-combination.7
  (documentation
    (lisp-clos::generic-function-method-combination
      #'document-combination6)
    'method-combination)
  "Combination7")

(deftest documentation-method-combination.8
  (let ((inst (lisp-clos::generic-function-method-combination
                #'document-combination6)))
    (setf (documentation inst 'method-combination) "Combination8"))
  "Combination8")

(deftest documentation-method-combination.9
  (let ((inst (lisp-clos::generic-function-method-combination
                #'document-combination6)))
    (setf (documentation inst 'method-combination) "Combination9")
    (documentation inst 'method-combination))
  "Combination9")

(define-method-combination document-combination10 :documentation "Combination10")

(deftest documentation-method-combination.10
  (documentation 'document-combination10 'method-combination)
  "Combination10")

(deftest documentation-method-combination.11
  (setf (documentation 'document-combination10 'method-combination) "Combination11")
  "Combination11")

(deftest documentation-method-combination.12
  (progn
    (setf (documentation 'document-combination10 'method-combination) "Combination12")
    (documentation 'document-combination10 'method-combination))
  "Combination12")

(define-method-combination
  document-combination13 ()
  ((primary () :required t))
  `(progn :hello))

(deftest documentation-method-combination.13
  (documentation 'document-combination13 'method-combination)
  nil)

(define-method-combination document-combination14)

(deftest documentation-method-combination.14
  (documentation 'document-combination14 'method-combination)
  nil)


;;  standard-method
(defgeneric document-standard-method1 (a))
(defmethod document-standard-method1 (a)
  (declare (ignore a))
  "Method1"
  :hello)

(deftest documentation-standard-method.1
  (documentation
    (find-method #'document-standard-method1 nil (list (find-class t)))
    t)
  "Method1")

(defgeneric document-standard-method2 (a))
(defmethod document-standard-method2 (a)
  (declare (ignore a))
  "Method1")

(deftest documentation-standard-method.2
  (documentation
    (find-method #'document-standard-method2 nil (list (find-class t)))
    t)
  nil)

(deftest documentation-standard-method.3
  (let ((inst (find-method #'document-standard-method2 nil (list (find-class t)))))
    (setf (documentation inst t) "Method2"))
  "Method2")

(deftest documentation-standard-method.4
  (let ((inst (find-method #'document-standard-method2 nil (list (find-class t)))))
    (setf (documentation inst t) "Method3")
    (documentation inst t))
  "Method3")


;;  package
(defpackage documentation-package1)

(deftest documentation-package.1
  (documentation
    (find-package 'documentation-package1)
    t)
  nil)

(defpackage documentation-package2 (:documentation "Package1"))

(deftest documentation-package.2
  (documentation
    (find-package 'documentation-package2)
    t)
  "Package1")

(deftest documentation-package.3
  (let ((inst (find-package 'documentation-package2)))
    (setf (documentation inst t) "Package3"))
  "Package3")

(deftest documentation-package.4
  (let ((inst (find-package 'documentation-package2)))
    (setf (documentation inst t) "Package4")
    (documentation inst t))
  "Package4")


;;  standard-class
(defclass document-class1 () () (:documentation "Class1"))

(deftest documentation-standard-class.1
  (documentation (find-class 'document-class1) t)
  "Class1")

(defclass document-class2 () ())

(deftest documentation-standard-class.2
  (documentation (find-class 'document-class2) t)
  nil)

(deftest documentation-standard-class.3
  (let ((inst (find-class 'document-class2)))
    (setf (documentation inst t) "Class3"))
  "Class3")

(deftest documentation-standard-class.4
  (let ((inst (find-class 'document-class2)))
    (setf (documentation inst t) "Class4")
    (documentation inst t))
  "Class4")

(deftest documentation-standard-class.5
  (let ((inst (find-class 'document-class2)))
    (setf (documentation inst 'type) "Class5"))
  "Class5")

(deftest documentation-standard-class.6
  (let ((inst (find-class 'document-class2)))
    (setf (documentation inst 'type) "Class6")
    (documentation inst 'type))
  "Class6")


;;  structure-class
(defstruct document-struct1 "Struct1")

(deftest documentation-structure-class.1
  (documentation (find-class 'document-struct1) t)
  "Struct1")

(defstruct document-struct2)

(deftest documentation-structure-class.2
  (documentation (find-class 'document-struct2) t)
  nil)

(deftest documentation-structure-class.3
  (let ((inst (find-class 'document-struct2)))
    (setf (documentation inst t) "Struct3"))
  "Struct3")

(deftest documentation-structure-class.4
  (let ((inst (find-class 'document-struct2)))
    (setf (documentation inst t) "Struct4")
    (documentation inst t))
  "Struct4")

(deftest documentation-structure-class.5
  (let ((inst (find-class 'document-struct2)))
    (setf (documentation inst 'type) "Struct5"))
  "Struct5")

(deftest documentation-structure-class.6
  (let ((inst (find-class 'document-struct2)))
    (setf (documentation inst 'type) "Struct6")
    (documentation inst 'type))
  "Struct6")


;;  symbol type
(defclass document-type1 () () (:documentation "Type1"))

(deftest documentation-symbol-type.1
  (documentation 'document-type1 'type)
  "Type1")

(deftest documentation-symbol-type.2
  (setf (documentation 'document-type1 'type) "Type2")
  "Type2")

(deftest documentation-symbol-type.3
  (progn
    (setf (documentation 'document-type1 'type) "Type3")
    (documentation 'document-type1 'type))
  "Type3")

(deftype document-type2 ()
  "Type4"
  :hello)

(deftest documentation-symbol-type.4
  (documentation 'document-type2 'type)
  "Type4")

(deftype document-type3 ()
  "Type4")

(deftest documentation-symbol-type.5
  (documentation 'document-type3 'type)
  nil)

(deftest documentation-symbol-type.6
  (setf (documentation 'document-type3 'type) "Type6")
  "Type6")

(deftest documentation-symbol-type.7
  (progn
    (setf (documentation 'document-type3 'type) "Type7")
    (documentation 'document-type3 'type))
  "Type7")


;;  symbol structure
(defstruct document-symbol-struct1 "Struct1")

(deftest documentation-symbol-struct.1
  (documentation 'document-symbol-struct1 'structure)
  "Struct1")

(deftest documentation-symbol-struct.2
  (setf (documentation 'document-symbol-struct1 'structure) "Struct2")
  "Struct2")

(deftest documentation-symbol-struct.3
  (progn
    (setf (documentation 'document-symbol-struct1 'structure) "Struct3")
    (documentation 'document-symbol-struct1 'structure))
  "Struct3")


;;  symbol variable
(defvar document-variable1 10)
(defparameter document-variable2 10)
(defconstant document-variable3 10)

(deftest documentation-symbol-variable.1
  (documentation 'document-variable1 'variable)
  nil)

(deftest documentation-symbol-variable.2
  (documentation 'document-variable2 'variable)
  nil)

(deftest documentation-symbol-variable.3
  (documentation 'document-variable3 'variable)
  nil)

(defvar document-variable4 10 "Variable1")
(defparameter document-variable5 10 "Variable2")
(defconstant document-variable6 10 "Variable3")

(deftest documentation-symbol-variable.4
  (documentation 'document-variable4 'variable)
  "Variable1")

(deftest documentation-symbol-variable.5
  (documentation 'document-variable5 'variable)
  "Variable2")

(deftest documentation-symbol-variable.6
  (documentation 'document-variable6 'variable)
  "Variable3")

(deftest documentation-symbol-variable.7
  (setf (documentation 'document-variable1 'variable) "Variable7")
  "Variable7")

(deftest documentation-symbol-variable.8
  (progn
    (setf (documentation 'document-variable1 'variable) "Variable8")
    (documentation 'document-variable1 'variable))
  "Variable8")

;;  compiler-macro
(define-compiler-macro document-compiler-macro-1 ()
  "Hello Compiler"
  :abc)

(define-compiler-macro document-compiler-macro-2 ()
  :cde)

(deftest documentation-compiler-macro.1
  (documentation 'document-compiler-macro-1 'compiler-macro)
  "Hello Compiler")

(deftest documentation-compiler-macro.2
  (documentation 'document-compiler-macro-2 'compiler-macro)
  nil)

(deftest documentation-compiler-macro.3
  (setf (documentation 'document-compiler-macro-1 'compiler-macro) "ABC")
  "ABC")

(deftest documentation-compiler-macro.4
  (documentation 'document-compiler-macro-1 'compiler-macro)
  "ABC")

(deftest documentation-compiler-macro.5
  (documentation 'no-such-compiler-macro 'compiler-macro)
  nil)

(deftest-error documentation-compiler-macro.6
  (setf (documentation 'no-such-compiler-macro 'compiler-macro) "ERROR"))

;;  (setf compiler-macro)
(define-compiler-macro (setf document-compiler-macro-1) ()
  "HELLO"
  :aaa)

(define-compiler-macro (setf document-compiler-macro-2) ()
  :bbb)

(deftest documentation-setf-compiler-macro.1
  (documentation '(setf document-compiler-macro-1) 'compiler-macro)
  "HELLO")

(deftest documentation-setf-compiler-macro.2
  (documentation '(setf document-compiler-macro-2) 'compiler-macro)
  nil)

(deftest documentation-setf-compiler-macro.3
  (setf (documentation '(setf document-compiler-macro-1) 'compiler-macro) "ZZZ")
  "ZZZ")

(deftest documentation-setf-compiler-macro.4
  (documentation '(setf document-compiler-macro-1) 'compiler-macro)
  "ZZZ")

(deftest documentation-setf-compiler-macro.5
  (documentation '(setf no-such-compiler-macro) 'compiler-macro)
  nil)

(deftest-error documentation-setf-compiler-macro.6
  (setf (documentation '(setf no-such-compiler-macro) 'compiler-macro) "ERROR"))

;;  error
(deftest-error! documentation-error.1
  (eval '(documentation)))

(deftest-error! documentation-error.2
  (eval '(documentation 'car 't nil)))

(deftest-error! documentation-error.3
  (eval '(setf (documentation #'document-function1) "Hello")))

(deftest-error! documentation-error.4
  (eval '(setf (documentation #'document-function1 t nil) "Hello")))

