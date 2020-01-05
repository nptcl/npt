;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  defgeneric
;;
(defgeneric defgeneric1 ())

(deftest defgeneric.1
  (functionp #'defgeneric1)
  t)

(deftest defgeneric.2
  (closp #'defgeneric1)
  t)

(defgeneric defgeneric3 (a b c))
(deftest defgeneric.3
  (functionp #'defgeneric3)
  t)

(defgeneric defgeneric4 (a b &optional c d) (:argument-precedence-order b a))
(deftest defgeneric.4
  (functionp #'defgeneric4)
  t)

(defgeneric defgeneric5 (c d &optional &rest e) (:argument-precedence-order c d))
(deftest defgeneric.5
  (functionp #'defgeneric5)
  t)

(defgeneric defgeneric6 (a) (:documentation "Hello"))
(deftest defgeneric.6
  (functionp #'defgeneric6)
  t)

(defgeneric defgeneric7 (a) (:method-combination standard))
(deftest defgeneric.7
  (functionp #'defgeneric7)
  t)

(defgeneric defgeneric8 (a &optional b)
            (:generic-function-class standard-generic-function))
(deftest defgeneric.8
  (functionp #'defgeneric8)
  t)

(defgeneric defgeneric9 (a b) (:method-class standard-method))
(deftest defgeneric.9
  (functionp #'defgeneric9)
  t)


;;
;;  defmethod
;;
(defgeneric defmethod1 ())
(defmethod defmethod1 ()
  :hello)

(deftest defmethod.1
  (defmethod1)
  :hello)

(defgeneric defmethod2 ())
(deftest defmethod.2
  (closp
    (defmethod defmethod2 ()))
  t)

(defgeneric defmethod3 (a))
(defmethod defmethod3 (a)
  (+ a 10))

(deftest defmethod.3
  (defmethod3 111)
  121)

(defgeneric defmethod4 (a))
(defmethod defmethod4 (a)
  (+ a 10))

(defmethod defmethod4 ((a string))
  (concatenate 'string "abc" a))

(deftest defmethod.4
  (defmethod4 100)
  110)

(deftest defmethod.5
  (defmethod4 "def")
  "abcdef")

(defgeneric defmethod6 (a))
(defmethod defmethod6 (a)
  (+ a 10))

(defmethod defmethod6 ((a string))
  (+ (length a) (call-next-method 1000)))

(deftest defmethod.6
  (defmethod6 30)
  40)

(deftest defmethod.7
  (defmethod6 "Hello")
  1015)

(defgeneric defmethod8 (a))
(defmethod defmethod8 (a)
  (declare (ignore a))
  (next-method-p))

(defmethod defmethod8 ((a string))
  (declare (ignore a))
  (next-method-p))

(deftest defmethod.8
  (defmethod8 30)
  nil)

(deftest defmethod.9
  (defmethod8 "Hello")
  t)


;;
;;  function-keywords
;;
(defgeneric function-keywords1 (a &key))
(defmethod function-keywords1 ((a integer) &key bb ((hello ccc)) (ddd 10))
  (values a bb ccc ddd))
(deftest function-keywords.1
  (function-keywords
    (car (generic-function-methods #'function-keywords1)))
  (:bb hello :ddd) nil)

(defgeneric function-keywords2 (a &key))
(defmethod function-keywords2 ((a integer) &key &allow-other-keys)
  a)
(deftest function-keywords.2
  (function-keywords
    (car (generic-function-methods #'function-keywords2)))
  nil t)


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
;;  error
;;
(deftest-error defgeneric-error.1
  (eval '(defgeneric defgeneric-error1 (a b &optional c d)
                     (:argument-precedence-order :most-specific-first))))

(deftest-error defgeneric-error.2
  (eval '(defgeneric defgeneric-error2 (&optional c d &rest e)
                     (:argument-precedence-order :most-specific-last))))

(deftest-error defgeneric-error.3
  (eval '(defgeneric defgeneric-error2 (a b)
                     (:argument-precedence-order c d))))

(defgeneric defgeneric-error4 (x y z) (:argument-precedence-order z y x))

(defmethod defgeneric-error4 ((x integer) y (z t))
  (declare (ignore x y z))
  "integer-t")

(defmethod defgeneric-error4 ((x t) y (z string))
  (declare (ignore x y z))
  "t-string")

(deftest defgeneric-error.4
  (defgeneric-error4 10 20 "Hello")
  "t-string")

(defgeneric defgeneric-error5 (x y z) (:argument-precedence-order x y z))

(defmethod defgeneric-error5 ((x integer) y (z t))
  (declare (ignore x y z))
  "integer-t")

(defmethod defgeneric-error5 ((x t) y (z string))
  (declare (ignore x y z))
  "t-string")

(deftest defgeneric-error.5
  (defgeneric-error5 10 20 "Hello")
  "integer-t")

