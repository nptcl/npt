;;
;;  ANSI COMMON LISP: 7. Objects
;;
(import 'lisp-clos::method-combination-instance)

;;
;;  define-method-combination long
;;
(deftest define-method-combination-long.1
  (define-method-combination combination1 () nil)
  combination1)

(deftest define-method-combination-long.2
  (null
    (method-combination-instance 'combination1))
  nil)

(define-method-combination
  test-standard (&optional (order :most-specific-first))
  ((around (:aaa))
   (before (:bbb))
   (primary () :order order :required t)
   (after (:ccc)))
  (flet ((calls (s) (mapcar (lambda (m) `(call-method ,m)) s)))
    (let ((form (destructuring-bind (a . b) primary
                  (if (or before after b)
                    `(multiple-value-prog1
                       (progn ,@(calls before)
                              (call-method ,a ,b))
                       ,@(calls after))
                    `(call-method ,a)))))
      (if around
        (destructuring-bind (a . b) around
          `(call-method ,a (,@b (make-method ,form))))
        form))))

(defgeneric combination3a (a) (:method-combination test-standard))
(defmethod combination3a (a)
  (+ a 10))

(defgeneric combination3b (a) (:method-combination standard))
(defmethod combination3b (a)
  (+ a 20))

(deftest define-method-combination-long.3
  (combination3a 111)
  121)

(deftest define-method-combination-long.4
  (combination3a 2000)
  2010)

(deftest define-method-combination-long.5
  (combination3b 111)
  131)

(deftest define-method-combination-long.6
  (combination3b 2000)
  2020)

;; lambda-list
(defgeneric lambda-list1 (a)
            (:method-combination
              test-standard :most-specific-first))
(defmethod lambda-list1 (a)
  (+ a 10))

(deftest lambda-list.1
  (lambda-list1 111)
  121)

(defgeneric lambda-list2 (a)
            (:method-combination
              test-standard :most-specific-last))
(defmethod lambda-list2 (a)
  (+ a 10))

(deftest lambda-list.2
  (lambda-list2 111)
  121)

;; next-method-p
(defvar *value*)
(defgeneric next-method-p1 (a) (:method-combination test-standard))
(defmethod next-method-p1 (a)
  (declare (ignore a))
  (setq *value* :primary1)
  (next-method-p))

(defmethod next-method-p1 ((a integer))
  (declare (ignore a))
  (setq *value* :primary2)
  (next-method-p))

(deftest next-method-p.1
  (progn
    (setq *value* nil)
    (values
      (next-method-p1 100)
      *value*))
  t :primary2)

(deftest next-method-p.2
  (progn
    (setq *value* nil)
    (values
      (next-method-p1 "Hello")
      *value*))
  nil :primary1)

(defgeneric next-method-p2 (a) (:method-combination standard))
(defmethod next-method-p2 (a)
  (declare (ignore a))
  (setq *value* :primary1)
  (next-method-p))

(defmethod next-method-p2 ((a integer))
  (declare (ignore a))
  (setq *value* :primary2)
  (next-method-p))

(deftest next-method-p.3
  (progn
    (setq *value* nil)
    (values
      (next-method-p2 100)
      *value*))
  t :primary2)

(deftest next-method-p.4
  (progn
    (setq *value* nil)
    (values
      (next-method-p2 "Hello")
      *value*))
  nil :primary1)

(defgeneric next-method-p3 (a)
            (:method-combination test-standard :most-specific-last))
(defmethod next-method-p3 (a)
  (declare (ignore a))
  (setq *value* :primary1)
  (next-method-p))

(defmethod next-method-p3 ((a integer))
  (declare (ignore a))
  (setq *value* :primary2)
  (next-method-p))

(deftest next-method-p.5
  (progn
    (setq *value* nil)
    (values
      (next-method-p3 100)
      *value*))
  t :primary1)

(deftest next-method-p.6
  (progn
    (setq *value* nil)
    (values
      (next-method-p3 "Hello")
      *value*))
  nil :primary1)

;; call-next-method
(defgeneric call-next-method1 (a) (:method-combination test-standard))
(defmethod call-next-method1 (a)
  (declare (ignore a))
  (push :primary1 *value*)
  :finish1)

(defmethod call-next-method1 ((a integer))
  (declare (ignore a))
  (push :primary2 *value*)
  (call-next-method))

(deftest call-next-method.1
  (progn
    (setq *value* nil)
    (values
      (call-next-method1 100)
      *value*))
  :finish1 (:primary1 :primary2))

(deftest call-next-method.2
  (progn
    (setq *value* nil)
    (values
      (call-next-method1 "Hello")
      *value*))
  :finish1 (:primary1))

(defgeneric call-next-method2 (a) (:method-combination standard))
(defmethod call-next-method2 (a)
  (declare (ignore a))
  (push :primary1 *value*)
  :finish1)

(defmethod call-next-method2 ((a integer))
  (declare (ignore a))
  (push :primary2 *value*)
  (call-next-method))

(deftest call-next-method.3
  (progn
    (setq *value* nil)
    (values
      (call-next-method2 100)
      *value*))
  :finish1 (:primary1 :primary2))

(deftest call-next-method.4
  (progn
    (setq *value* nil)
    (values
      (call-next-method2 "Hello")
      *value*))
  :finish1 (:primary1))

;; around
(defgeneric around1 (a) (:method-combination test-standard))
(defmethod around1 (a)
  (push :primary *value*)
  (when (next-method-p)
    (call-next-method))
  (list :call1 a))

(defmethod around1 :aaa (a)
  (push :around *value*)
  (when (next-method-p)
    (call-next-method))
  (list :call2 a))

(deftest around.1
  (progn
    (setq *value* nil)
    (values
      (around1 100)
      *value*))
  (:call2 100) (:primary :around))

(defgeneric around2 (a) (:method-combination standard))
(defmethod around2 (a)
  (push :primary *value*)
  (when (next-method-p)
    (call-next-method))
  (list :call1 a))

(defmethod around2 :around (a)
  (push :around *value*)
  (when (next-method-p)
    (call-next-method))
  (list :call2 a))

(deftest around.2
  (progn
    (setq *value* nil)
    (values
      (around2 200)
      *value*))
  (:call2 200) (:primary :around))

;; before after
(defgeneric beforeafter1 (a) (:method-combination test-standard))
(defmethod beforeafter1 (a)
  (push :primary *value*)
  (list :call1 a))

(defmethod beforeafter1 :bbb (a)
  (push :before *value*)
  (list :call2 a))

(defmethod beforeafter1 :ccc (a)
  (push :after *value*)
  (list :call3 a))

(deftest beforeafter.1
  (progn
    (setq *value* nil)
    (values
      (beforeafter1 111)
      *value*))
  (:call1 111) (:after :primary :before))

(defgeneric beforeafter2 (a) (:method-combination standard))
(defmethod beforeafter2 (a)
  (push :primary *value*)
  (list :call1 a))

(defmethod beforeafter2 :before (a)
  (push :before *value*)
  (list :call2 a))

(defmethod beforeafter2 :after (a)
  (push :after *value*)
  (list :call3 a))

(deftest beforeafter.2
  (progn
    (setq *value* nil)
    (values
      (beforeafter2 111)
      *value*))
  (:call1 111) (:after :primary :before))

;; required
(defgeneric required1 (a) (:method-combination test-standard))
(defmethod required1 :aaa (a)
  (list :call1 a))

(deftest-error required.1
  (required1 100))

(defgeneric required2 (a) (:method-combination standard))
(defmethod required2 :around (a)
  (list :call1 a))

(deftest-error required.2
  (required2 100))

;; no-generic
(fmakunbound 'no-generic)
(defmethod no-generic (a b c)
  (+ a b c))

