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

(defgeneric defgeneric4 (a b &optional c d)
            (:argument-precedence-order :most-specific-first))
(deftest defgeneric.4
  (functionp #'defgeneric4)
  t)

(defgeneric defgeneric5 (&optional c d &rest e)
            (:argument-precedence-order :most-specific-last))
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

