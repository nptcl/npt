;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  Macro DEFMETHOD
;;

;;
;;  defmethod
;;
(defgeneric defmethod-1 ())
(defmethod defmethod-1 ()
  :hello)

(deftest defmethod.1
  (defmethod-1)
  :hello)

(defgeneric defmethod-2 ())
(deftest defmethod.2
  (closp
    (defmethod defmethod-2 ()))
  t)

(defgeneric defmethod-3 (a))
(defmethod defmethod-3 (a)
  (+ a 10))

(deftest defmethod.3
  (defmethod-3 111)
  121)

(defgeneric defmethod-4 (a))
(defmethod defmethod-4 (a)
  (+ a 10))

(defmethod defmethod-4 ((a string))
  (concatenate 'string "abc" a))

(deftest defmethod.4
  (defmethod-4 100)
  110)

(deftest defmethod.5
  (defmethod-4 "def")
  "abcdef")

(defgeneric defmethod-6 (a))
(defmethod defmethod-6 (a)
  (+ a 10))

(defmethod defmethod-6 ((a string))
  (+ (length a) (call-next-method 1000)))

(deftest defmethod.6
  (defmethod-6 30)
  40)

(deftest defmethod.7
  (defmethod-6 "Hello")
  1015)

(defgeneric defmethod-8 (a))
(defmethod defmethod-8 (a)
  (declare (ignore a))
  (next-method-p))

(defmethod defmethod-8 ((a string))
  (declare (ignore a))
  (next-method-p))

(deftest defmethod.8
  (defmethod-8 30)
  nil)

(deftest defmethod.9
  (defmethod-8 "Hello")
  t)

(deftest defmethod.10
  (progn
    (defgeneric defmethod-10 ())
    (typep
      (defmethod defmethod-10 ())
      'standard-method))
  t)

(deftest defmethod-setf.1
  (progn
    (defgeneric (setf defmethod-setf-1) ())
    (typep
      (defmethod (setf defmethod-setf-1) ())
      'standard-method))
  t)

(defvar *defmethod-qualifier*)
(deftest defmethod-qualifier.1
  (progn
    (defgeneric defmethod-qualifier-1 ())
    (defmethod defmethod-qualifier-1 :before ()
      (push :aaa *defmethod-qualifier*)
      10)
    (defmethod defmethod-qualifier-1 ()
      (push :bbb *defmethod-qualifier*)
      20)
    (let (*defmethod-qualifier*)
      (values
        (defmethod-qualifier-1)
        (nreverse *defmethod-qualifier*))))
  20 (:aaa :bbb))

(deftest defmethod-declaration.1
  (progn
    (defgeneric defmethod-declaration-1 (x))
    (defmethod defmethod-declaration-1 (x)
      (declare (special x))
      x)
    (defmethod-declaration-1 100))
  100)

(deftest defmethod-documentation.1
  (progn
    (defgeneric defmethod-documentation-1 (x))
    (documentation
      (defmethod defmethod-documentation-1 (x)
        (declare (ignore x))
        "Hello"
        (+ 10 20 30))
      't))
  "Hello")


;;
;;  new generic
;;
(deftest defmethod-new.1
  (progn
    (fmakunbound 'defmethod-new-1)
    (defmethod defmethod-new-1 (x y)
      (+ 100 x y))
    (defmethod-new-1 11 22))
  133)

(deftest defmethod-new.2
  (typep
    #'defmethod-new-1
    'standard-generic-function)
  t)

(deftest defmethod-new.3
  (class-name
    (generic-function-method-class
      #'defmethod-new-1))
  standard-method)

(deftest defmethod-new.4
  (slot-value
    (generic-function-method-combination #'defmethod-new-1)
    'lisp-clos::name)
  standard)

(defun defmethod-new-5 ())
(deftest-error defmethod-new.5
  (defmethod defmethod-new-5 ()))

(defmacro defmethod-new-6 ())
(deftest-error defmethod-new.6
  (defmethod defmethod-new-6 ()))

(deftest-error defmethod-new.7
  (defmethod load-time-value ()))

(defun (setf defmethod-new-8) ())
(deftest-error defmethod-new.8
  (defmethod (setf defmethod-new-8) ()))


;;
;;  overwrite
;;
(defgeneric defmethod-overwrite-1 (x))
(defmethod defmethod-overwrite-1 ((x integer))
  (declare (ignore x))
  :aaa)

(deftest defmethod-overwrite.1
  (closp
    (defmethod defmethod-overwrite-1 ((x integer))
      (declare (ignore x))
      :bbb))
  t)

(deftest defmethod-overwrite.2
  (defmethod-overwrite-1 100)
  :bbb)


;;
;;  lambda-list
;;
(deftest-error defmethod-lambda.1
  (progn
    (defgeneric defmethod-lambda-1 (x))
    (defmethod defmethod-lambda-1 ()
      :hello)))

(deftest-error defmethod-lambda.2
  (progn
    (defgeneric defmethod-lambda-2 (x))
    (defmethod defmethod-lambda-2 (x y)
      (declare (ignore x y))
      :hello)))

(deftest-error defmethod-lambda.3
  (progn
    (defgeneric defmethod-lambda-3 (x &optional y))
    (defmethod defmethod-lambda-3 (x y)
      (declare (ignore x y))
      :hello)))

(deftest-error defmethod-lambda.4
  (progn
    (defgeneric defmethod-lambda-4 (x y))
    (defmethod defmethod-lambda-4 (x &optional y)
      (declare (ignore x y))
      :hello)))

(deftest defmethod-lambda.5
  (progn
    (defgeneric defmethod-lambda-5 (x &rest args))
    (defmethod defmethod-lambda-5 (x &key)
      (declare (ignore x))
      :hello)
    (defmethod-lambda-5 10))
  :hello)

(deftest-error defmethod-lambda.6
  (progn
    (defgeneric defmethod-lambda-6 (x &key))
    (defmethod defmethod-lambda-6 (x)
      (declare (ignore x y))
      :hello)))

(deftest defmethod-block.1
  (progn
    (defgeneric defmethod-block-1 (x))
    (defmethod defmethod-block-1 (x)
      (return-from defmethod-block-1 (+ x 100)))
    (defmethod-block-1 200))
  300)

(deftest defmethod-block.2
  (progn
    (defgeneric (setf defmethod-block-2) (x))
    (defmethod (setf defmethod-block-2) (x)
      (return-from defmethod-block-2 (+ x 500)))
    (funcall #'(setf defmethod-block-2) 200))
  700)

(deftest defmethod-class.1
  (progn
    (defclass defmethod-method-class-1 (standard-method) ())
    (make-instance 'defmethod-method-class-1)
    (defgeneric defmethod-class-1 () (:method-class defmethod-method-class-1))
    (let ((x (defmethod defmethod-class-1 ())))
      (class-name
        (class-of x))))
  defmethod-method-class-1)


;;
;;  eql-specializer
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
;;  Standard Generic Function REMOVE-METHOD
;;  Standard Generic Function COMPUTE-APPLICABLE-METHODS
;;  Standard Generic Function FIND-METHOD
;;  Standard Generic Function ADD-METHOD


;;
;;  find-method
;;
(defgeneric find-method1 (a b))
(defmethod find-method1 ((a integer) (b string))
  (list a b))

(deftest find-method.1
  (typep
    (find-method
      #'find-method1
      nil
      (mapcar #'find-class '(integer string)))
    'standard-method)
  t)

(deftest-error find-method.2
  (find-method
    #'find-method1
    nil
    (mapcar #'find-class '(t string))))

(deftest find-method.3
  (find-method
    #'find-method1
    nil
    (mapcar #'find-class '(t string))
    nil)
  nil)

(deftest-error find-method.4
  (find-method
    #'find-method1
    nil
    (mapcar #'find-class '(integer))
    nil))

(deftest-error find-method.5
  (find-method
    #'find-method1
    nil
    (mapcar #'find-class '(integer t t))
    nil))

(defgeneric find-method2 (a b))
(defmethod find-method2 ((a integer) (b string))
  (list a b))

(defmethod find-method2 ((a ratio) (b string))
  (list a b))

(defmethod find-method2 ((a t) (b t))
  (list a b))

(deftest find-method.6
  (typep
    (find-method
      #'find-method2
      nil
      (mapcar #'find-class '(integer string)))
    'standard-method)
  t)

(deftest find-method.7
  (typep
    (find-method
      #'find-method2
      nil
      (mapcar #'find-class '(t t)))
    'standard-method)
  t)

(deftest find-method.8
  (typep
    (find-method
      #'find-method2
      nil
      (mapcar #'find-class '(ratio string)))
    'standard-method)
  t)

(deftest find-method.9
  (find-method
    #'find-method2
    nil
    (mapcar #'find-class '(integer integer))
    nil)
  nil)


;;
;;  remove-method
;;
(defvar *value* nil)

(defgeneric remove-method1 (a))
(defmethod remove-method1 (a)
  (push (list :remove1 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod remove-method1 ((a integer))
  (push (list :remove2 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defun find-method-list (gen qua &rest rest)
  (find-method gen qua (mapcar #'find-class rest)))

(deftest remove-method.1
  (typep
    (remove-method
      #'remove-method1
      (find-method-list #'remove-method1 nil 'integer))
    'standard-generic-function)
  t)

(defgeneric remove-method2 (a))
(deftest remove-method.2
  (typep
    (remove-method
      #'remove-method2
      (find-method-list #'remove-method1 nil 't))
    'standard-generic-function)
  t)

(defgeneric remove-method3 (a))
(defmethod remove-method3 (a)
  (push (list :remove1 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod remove-method3 ((a integer))
  (push (list :remove2 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(deftest remove-method.3
  (progn
    (setq *value* nil)
    (remove-method
      #'remove-method3
      (find-method-list #'remove-method3 nil 'integer))
    (values
      (remove-method3 10)
      *value*))
  nil ((:remove1 10)))

(defgeneric remove-method4 (a))
(defmethod remove-method4 (a)
  (push (list :remove1 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod remove-method4 ((a integer))
  (push (list :remove2 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(deftest remove-method.4
  (progn
    (setq *value* nil)
    (remove-method
      #'remove-method4
      (find-method-list #'remove-method4 nil 't))
    (values
      (remove-method4 20)
      *value*))
  nil ((:remove2 20)))

(defgeneric remove-method5 (a))

(defmethod remove-method5 :before (a)
  (push (list :remove1 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod remove-method5 :around ((a integer))
  (push (list :remove2 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod remove-method5 (a)
  (push (list :remove3 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod remove-method5 ((a integer))
  (push (list :remove4 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(deftest remove-method.5
  (progn
    (setq *value* nil)
    (remove-method
      #'remove-method5
      (find-method-list #'remove-method5 '(:around) 'integer))
    (values
      (remove-method5 30)
      *value*))
  nil ((:remove3 30) (:remove4 30) (:remove1 30)))

(defgeneric remove-method6 (a))
(defmethod remove-method6 (a)
  a)

(defgeneric remove-method7 (a))
(defmethod remove-method7 (a)
  a)

(deftest remove-method.6
  (let ((method (find-method-list #'remove-method6 nil 't)))
    (values
      (generic-function-name
        (method-generic-function method))
      (progn
        (remove-method #'remove-method6 method)
        (method-generic-function method))))
  remove-method6
  nil)

(defgeneric remove-method8 (a))
(defmethod remove-method8 (a)
  a)

(defgeneric remove-method9 (a))
(defmethod remove-method9 (a)
  a)

(deftest remove-method.7
  (let ((method (find-method-list #'remove-method8 nil 't)))
    (values
      (generic-function-name
        (method-generic-function method))
      (progn
        (remove-method #'remove-method9 method)
        (generic-function-name
          (method-generic-function method)))))
  remove-method8
  remove-method8)


;;
;;  add-method
;;
(defgeneric add-method1 (a))
(defmethod add-method1 (a)
  (push (list :method1 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod add-method1 ((a integer))
  (push (list :method2 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(deftest add-method.1
  (let ((method (find-method-list #'add-method1 nil 'integer)))
    (remove-method #'add-method1 method)
    (add-method #'add-method1 method)
    (setq *value* nil)
    (add-method1 111)
    *value*)
  ((:method1 111) (:method2 111)))

(deftest add-method.2
  (generic-function-name
    (method-generic-function
      (find-method-list #'add-method1 nil 'integer)))
  add-method1)

(defgeneric add-method2 (a))
(defmethod add-method2 (a)
  (push (list :method1 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defgeneric add-method3 (a))

(deftest-error add-method.3
  (let ((method (find-method-list #'add-method2 nil 't)))
    (add-method #'add-method3 method)))

(deftest add-method.4
  (let ((method (find-method-list #'add-method2 nil 't)))
    (remove-method #'add-method2 method)
    (add-method #'add-method3 method)
    (setq *value* nil)
    (add-method3 222)
    *value*)
  ((:method1 222)))

(defgeneric add-method4 (a))
(defmethod add-method4 (a)
  (push (list :method1 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defgeneric add-method5 (a b))

(deftest-error add-method.5
  (let ((method (find-method-list #'add-method4 nil 't)))
    (remove-method #'add-method4 method)
    (add-method #'add-method5 method)))


;;
;;  overwrite
;;
(defclass add-method-over-1 () ())
(defclass add-method-over-2 () ())

(defgeneric add-method-over-3 (x))
(defmethod add-method-over-3 ((x add-method-over-1))
  (+ x 10))

(defgeneric add-method-over-4 (x))
(defmethod add-method-over-4 ((x add-method-over-2))
  (+ x 20))

(deftest-error add-method-overwrite.1
  (let ((method (find-method-list #'add-method-over-4 nil 'add-method-over-2)))
    (add-method #'add-method-over-3 method)))


;;
;;  Standard Generic Function FUNCTION-KEYWORDS
;;
(deftest function-keywords.1
  (progn
    (defgeneric function-keywords-1 (a &key))
    (defmethod function-keywords-1 ((a integer) &key bb ((hello ccc)) (ddd 10))
      (values a bb ccc ddd))
    (function-keywords
      (car (generic-function-methods #'function-keywords-1))))
  (:bb hello :ddd) nil)

(deftest function-keywords.2
  (progn
    (defgeneric function-keywords-2 (a &key))
    (defmethod function-keywords-2 ((a integer) &key &allow-other-keys)
      a)
    (function-keywords
      (car (generic-function-methods #'function-keywords-2))))
  nil t)

(deftest function-keywords.3
  (progn
    (defgeneric function-keywords-3 ())
    (defmethod function-keywords-3 ())
    (function-keywords
      (car (generic-function-methods #'function-keywords-3))))
  nil nil)

(deftest function-keywords.4
  (progn
    (defclass function-keywords-method (standard-method) ())
    (make-instance 'function-keywords-method)
    (defgeneric function-keywords-4 () (:method-class function-keywords-method))
    (defmethod function-keywords-4 ())
    (defmethod function-keywords ((x function-keywords-method))
      (declare (ignore x))
      (values :aaa :bbb))
    (function-keywords
      (car (generic-function-methods #'function-keywords-4))))
  :aaa :bbb)

;;  error
(deftest-error! function-keywords-error.1
  (eval '(function-keywords)))

(deftest-error! function-keywords-error.2
  (eval '(function-keywords 100)))

(deftest-error! function-keywords-error.3
  (progn
    (defgeneric function-keywords-error-3 ())
    (defmethod function-keywords-error-3 ())
    (eval '(function-keywords
             (car (generic-function-methods #'function-keywords-error-3))
             nil))))

;;  ANSI Common Lisp
(defgeneric function-keywords-test-1 (a &optional b &key))
(defmethod function-keywords-test-1
  ((a integer) &optional (b 2) &key (c 3) ((:dee d) 4) e ((eff f)))
  (list a b c d e f))

(deftest function-keywords-test.1
  (typep
    (find-method #'function-keywords-test-1 '() (list (find-class 'integer)))
    'standard-method)
  t)

(deftest function-keywords-test.2
  (function-keywords
    (find-method #'function-keywords-test-1 '() (list (find-class 'integer))) )
  (:c :dee :e eff) nil)

(deftest function-keywords-test.3
  (progn
    (defgeneric function-keywords-test-3 (a))
    (function-keywords
      (defmethod function-keywords-test-3 ((a integer))
        (list a b c d e f))))
  () nil)

(deftest function-keywords-test.4
  (progn
    (defgeneric function-keywords-test-4 (a &key))
    (function-keywords
      (defmethod function-keywords-test-4 ((a integer) &key b c d &allow-other-keys)
        (list a b c d e f))))
  (:b :c :d) t)

