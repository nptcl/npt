;;
;;  ANSI COMMON LISP: 7. Objects
;;
(defun find-method-list (gen qua &rest rest)
  (find-method gen qua (mapcar #'find-class rest)))


;;
;;  Standard Generic Function FIND-METHOD
;;
(defgeneric find-method-1 (a b))
(defmethod find-method-1 ((a integer) (b string))
  (list a b))

(deftest find-method.1
  (typep
    (find-method
      #'find-method-1
      nil
      (mapcar #'find-class '(integer string)))
    'standard-method)
  t)

(deftest-error find-method.2
  (find-method
    #'find-method-1
    nil
    (mapcar #'find-class '(t string))))

(deftest find-method.3
  (find-method
    #'find-method-1
    nil
    (mapcar #'find-class '(t string))
    nil)
  nil)

(deftest-error find-method.4
  (find-method
    #'find-method-1
    nil
    (mapcar #'find-class '(integer))
    nil))

(deftest-error find-method.5
  (find-method
    #'find-method-1
    nil
    (mapcar #'find-class '(integer t t))
    nil))

(defgeneric find-method-2 (a b))
(defmethod find-method-2 ((a integer) (b string))
  (list a b))

(defmethod find-method-2 ((a ratio) (b string))
  (list a b))

(defmethod find-method-2 ((a t) (b t))
  (list a b))

(deftest find-method.6
  (typep
    (find-method
      #'find-method-2
      nil
      (mapcar #'find-class '(integer string)))
    'standard-method)
  t)

(deftest find-method.7
  (typep
    (find-method
      #'find-method-2
      nil
      (mapcar #'find-class '(t t)))
    'standard-method)
  t)

(deftest find-method.8
  (typep
    (find-method
      #'find-method-2
      nil
      (mapcar #'find-class '(ratio string)))
    'standard-method)
  t)

(deftest find-method.9
  (find-method
    #'find-method-2
    nil
    (mapcar #'find-class '(integer integer))
    nil)
  nil)

(deftest find-method-qualifiers.1
  (let (x y)
    (defgeneric find-method-qualifiers-1 (x))
    (setq x (defmethod find-method-qualifiers-1 (x) x))
    (setq y (defmethod find-method-qualifiers-1 :around (x) x))
    (values
      (eq x (find-method #'find-method-qualifiers-1 nil
                         (mapcar #'find-class '(t))))
      (eq y (find-method #'find-method-qualifiers-1 '(:around)
                         (mapcar #'find-class '(t))))))
  t t)

(deftest find-method-qualifiers.2
  (find-method #'find-method-qualifiers-1 '(a b c)
               (mapcar #'find-class '(t)) nil)
  nil)

(defgeneric find-method-error-1 ())
(defmethod find-method-error-1 ())

(deftest-error find-method-error.1
  (eval '(find-method #'find-method-error-1 nil)))

(deftest-error find-method-error.2
  (eval '(find-method #'find-method-error-1 nil nil nil nil)))

(deftest-error! find-method-error.3
  (eval '(find-method #'find-method-error-1 10 nil))
  type-error)

;;  ANSI Common Lisp
(defmethod find-method-test-1 ((a integer) (b float)) (list a b))

(deftest find-method-test.1
  (typep
    (find-method #'find-method-test-1 '() (mapcar #'find-class '(integer float)))
    'standard-method)
  t)

(deftest-error find-method-test.2
  (find-method #'find-method-test-1 '() (mapcar #'find-class '(integer integer))))

(deftest find-method-test.3
  (find-method #'find-method-test-1 '() (mapcar #'find-class '(integer integer)) nil)
  nil)


;;
;;  Standard Generic Function ADD-METHOD
;;
(defvar *add-method* nil)

(defgeneric add-method-1 (a))
(defmethod add-method-1 (a)
  (push (list :method1 a) *add-method*)
  (when (next-method-p)
    (call-next-method)))

(defmethod add-method-1 ((a integer))
  (push (list :method2 a) *add-method*)
  (when (next-method-p)
    (call-next-method)))

(deftest add-method.1
  (let ((method (find-method-list #'add-method-1 nil 'integer)))
    (remove-method #'add-method-1 method)
    (add-method #'add-method-1 method)
    (setq *add-method* nil)
    (add-method-1 111)
    *add-method*)
  ((:method1 111) (:method2 111)))

(deftest add-method.2
  (generic-function-name
    (method-generic-function
      (find-method-list #'add-method-1 nil 'integer)))
  add-method-1)

(defgeneric add-method-2 (a))
(defmethod add-method-2 (a)
  (push (list :method1 a) *add-method*)
  (when (next-method-p)
    (call-next-method)))

(defgeneric add-method-3 (a))

(deftest-error add-method.3
  (let ((method (find-method-list #'add-method-2 nil 't)))
    (add-method #'add-method-3 method)))

(deftest add-method.4
  (let ((method (find-method-list #'add-method-2 nil 't)))
    (remove-method #'add-method-2 method)
    (add-method #'add-method-3 method)
    (setq *add-method* nil)
    (add-method-3 222)
    *add-method*)
  ((:method1 222)))

(defgeneric add-method-4 (a))
(defmethod add-method-4 (a)
  (push (list :method1 a) *add-method*)
  (when (next-method-p)
    (call-next-method)))

(defgeneric add-method-5 (a b))

(deftest-error add-method.5
  (let ((method (find-method-list #'add-method-4 nil 't)))
    (remove-method #'add-method-4 method)
    (add-method #'add-method-5 method)))

;;  replace
(defclass add-method-replace-1 () ())
(defclass add-method-replace-2 () ())

(defgeneric add-method-replace-3 (x))
(defmethod add-method-replace-3 ((x add-method-replace-1))
  (+ x 10))

(defgeneric add-method-replace-4 (x))
(defmethod add-method-replace-4 ((x add-method-replace-2))
  (+ x 20))

(deftest-error add-method-replace.1
  (let ((method (find-method
                  #'add-method-replace-4 nil
                  (mapcar #'find-class '(add-method-replace-2)))))
    (add-method #'add-method-replace-3 method)))

(deftest add-method-replace.2
  (let ((method (find-method
                  #'add-method-replace-4 nil
                  (mapcar #'find-class '(add-method-replace-2)))))
    (remove-method #'add-method-replace-4 method)
    (eq (add-method #'add-method-replace-3 method)
        #'add-method-replace-3))
  t)

;;  error
(deftest-error add-method-error.1
  (eval '(add-method #'add-method-replace-3)))

(deftest-error add-method-error.2
  (eval '(let ((method (find-method
                         #'add-method-replace-3 nil
                         (mapcar #'find-class '(add-method-replace-2)))))
           (add-method #'add-method-replace-3 method nil))))

(deftest-error! add-method-error.3
  (eval '(add-method #'add-method-replace-3 100)))


;;
;;  Standard Generic Function REMOVE-METHOD
;;
(defvar *remove-method* nil)

(defgeneric remove-method-1 (a))
(defmethod remove-method-1 (a)
  (push (list :remove1 a) *remove-method*)
  (when (next-method-p)
    (call-next-method)))

(defmethod remove-method-1 ((a integer))
  (push (list :remove2 a) *remove-method*)
  (when (next-method-p)
    (call-next-method)))

(deftest remove-method.1
  (typep
    (remove-method
      #'remove-method-1
      (find-method-list #'remove-method-1 nil 'integer))
    'standard-generic-function)
  t)

(defgeneric remove-method-2 (a))
(deftest remove-method.2
  (typep
    (remove-method
      #'remove-method-2
      (find-method-list #'remove-method-1 nil 't))
    'standard-generic-function)
  t)

(defgeneric remove-method-3 (a))
(defmethod remove-method-3 (a)
  (push (list :remove1 a) *remove-method*)
  (when (next-method-p)
    (call-next-method)))

(defmethod remove-method-3 ((a integer))
  (push (list :remove2 a) *remove-method*)
  (when (next-method-p)
    (call-next-method)))

(deftest remove-method.3
  (progn
    (setq *remove-method* nil)
    (remove-method
      #'remove-method-3
      (find-method-list #'remove-method-3 nil 'integer))
    (values
      (remove-method-3 10)
      *remove-method*))
  nil ((:remove1 10)))

(defgeneric remove-method-4 (a))
(defmethod remove-method-4 (a)
  (push (list :remove1 a) *remove-method*)
  (when (next-method-p)
    (call-next-method)))

(defmethod remove-method-4 ((a integer))
  (push (list :remove2 a) *remove-method*)
  (when (next-method-p)
    (call-next-method)))

(deftest remove-method.4
  (progn
    (setq *remove-method* nil)
    (remove-method
      #'remove-method-4
      (find-method-list #'remove-method-4 nil 't))
    (values
      (remove-method-4 20)
      *remove-method*))
  nil ((:remove2 20)))

(defgeneric remove-method-5 (a))

(defmethod remove-method-5 :before (a)
  (push (list :remove1 a) *remove-method*)
  (when (next-method-p)
    (call-next-method)))

(defmethod remove-method-5 :around ((a integer))
  (push (list :remove2 a) *remove-method*)
  (when (next-method-p)
    (call-next-method)))

(defmethod remove-method-5 (a)
  (push (list :remove3 a) *remove-method*)
  (when (next-method-p)
    (call-next-method)))

(defmethod remove-method-5 ((a integer))
  (push (list :remove4 a) *remove-method*)
  (when (next-method-p)
    (call-next-method)))

(deftest remove-method.5
  (progn
    (setq *remove-method* nil)
    (remove-method
      #'remove-method-5
      (find-method-list #'remove-method-5 '(:around) 'integer))
    (values
      (remove-method-5 30)
      *remove-method*))
  nil ((:remove3 30) (:remove4 30) (:remove1 30)))

(defgeneric remove-method-6 (a))
(defmethod remove-method-6 (a)
  a)

(defgeneric remove-method-7 (a))
(defmethod remove-method-7 (a)
  a)

(deftest remove-method.6
  (let ((method (find-method-list #'remove-method-6 nil 't)))
    (values
      (generic-function-name
        (method-generic-function method))
      (progn
        (remove-method #'remove-method-6 method)
        (method-generic-function method))))
  remove-method-6
  nil)

(defgeneric remove-method-8 (a))
(defmethod remove-method-8 (a)
  a)

(defgeneric remove-method-9 (a))
(defmethod remove-method-9 (a)
  a)

(deftest remove-method.7
  (let ((method (find-method-list #'remove-method-8 nil 't)))
    (values
      (generic-function-name
        (method-generic-function method))
      (progn
        (remove-method #'remove-method-9 method)
        (generic-function-name
          (method-generic-function method)))))
  remove-method-8
  remove-method-8)

;;  error
(deftest-error remove-method-error.1
  (eval '(remove-method #'remove-method-9)))

(defgeneric remove-method-error-2 ())
(defmethod remove-method-error-2 ())
(deftest-error remove-method-error.2
  (eval '(remove-method #'remove-method-error-2
                        (find-method #'remove-method-error-2 nil nil)
                        nil)))

(deftest-error! remove-method-error.3
  (eval '(remove-method #'remove-method-9 100)))


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


;;
;;  Standard Generic Function COMPUTE-APPLICABLE-METHODS
;;
(defgeneric compute-applicable-methods-1 (a))
(defmethod compute-applicable-methods-1 (a)
  (+ a 100))

(deftest compute-applicable-methods.1
  (length
    (compute-applicable-methods
      #'compute-applicable-methods-1
      '(10)))
  1)

(deftest compute-applicable-methods.2
  (eq (car (compute-applicable-methods
             #'compute-applicable-methods-1
             '(10)))
      (find-method #'compute-applicable-methods-1 nil
                   (mapcar #'find-class '(t))))
  t)

(defgeneric compute-applicable-methods-2 (a))
(defmethod compute-applicable-methods-2 (a)
  (+ a 100))

(defmethod compute-applicable-methods-2 ((a integer))
  (+ a 200))

(defmethod compute-applicable-methods-2 ((a string))
  (values 300 a))

(deftest compute-applicable-methods.3
  (length
    (compute-applicable-methods
      #'compute-applicable-methods-2
      '(10)))
  2)

(deftest compute-applicable-methods.4
  (every
    (lambda (x)
      (typep x 'standard-method))
    (compute-applicable-methods
      #'compute-applicable-methods-2
      '(10)))
  t)

(deftest compute-applicable-methods.5
  (length
    (compute-applicable-methods
      #'compute-applicable-methods-2
      '("hello")))
  2)

(deftest compute-applicable-methods.6
  (length
    (compute-applicable-methods
      #'compute-applicable-methods-2
      '(t)))
  1)

(defgeneric compute-applicable-methods-3 (a))
(defmethod compute-applicable-methods-3 (a)
  (+ a 100))

(defmethod compute-applicable-methods-3 ((a integer))
  (+ a 200))

(defmethod compute-applicable-methods-3 :around (a)
  (values 300 a))

(defmethod compute-applicable-methods-3 :before (a)
  (values 400 a))

(deftest compute-applicable-methods.7
  (length
    (compute-applicable-methods
      #'compute-applicable-methods-3
      '(10)))
  4)

(deftest-error compute-applicable-methods-error.1
  (eval '(compute-applicable-methods #'compute-applicable-methods-1)))

(deftest-error compute-applicable-methods-error.2
  (eval '(compute-applicable-methods #'compute-applicable-methods-1 '(10) nil)))

(deftest-error! compute-applicable-methods-error.3
  (eval '(compute-applicable-methods 100 '(10))))

