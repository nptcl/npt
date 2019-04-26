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

(deftest no-generic.1
  (no-generic 10 20 30)
  60)

(deftest no-generic.2
  (closp #'no-generic)
  t)

;; generic-function
(define-method-combination
  generic-combination1 ()
  ((primary () :required t))
  (:generic-function gen)
  `(progn
     (setq *value* ,gen)
     (call-method ,(car primary) ,@(cdr primary))))

(defgeneric generic-combination2 (a) (:method-combination generic-combination1))
(defmethod generic-combination2 (a)
  (+ a 111))

(deftest generic-combination.1
  (progn
    (setq *value* nil)
    (values
      (generic-combination2 90000)
      (generic-function-name *value*)))
  90111
  generic-combination2)

(define-method-combination
  generic-combination3 ()
  ((primary () :required t))
  (:generic-function gen)
  `(call-method ,(car primary) ,@(cdr primary)))

(defgeneric generic-combination4 (a) (:method-combination generic-combination3))
(defmethod generic-combination4 (a)
  (+ a 222))

(deftest generic-combination.2
  (generic-combination4 90000)
  90222)

;; arguments
(define-method-combination
  arguments1c ()
  ((primary () :required t))
  (:arguments a b c)
  `(progn
     (setq *value* (list ,a ,b ,c))
     (call-method ,(car primary) ,@(cdr primary))))

(defgeneric arguments1 (x y z) (:method-combination arguments1c))

(defmethod arguments1 (a b c)
  (+ a b c))

(deftest arguments.1
  (values
    (arguments1 10 20 30)
    *value*)
  60 (10 20 30))

(defgeneric arguments2 (x &rest a) (:method-combination arguments1c))

(defmethod arguments2 (a &rest r)
  (list a r))

(deftest arguments.2
  (values
    (arguments2 10 20 30)
    *value*)
  (10 (20 30)) (10 20 30))

(deftest arguments.3
  (values
    (arguments2 10 20)
    *value*)
  (10 (20)) (10 20 nil))


;;
;;  define-method-combination short
;;
(defun combination-short1 (&rest args)
  (apply #'+ args))

(deftest define-method-combination-short.1
  (define-method-combination combination-short1)
  combination-short1)

(defun combination-short2 (&rest args)
  (apply #'+ args))

(define-method-combination combination-short2)
(defgeneric combination-short3 (a) (:method-combination combination-short2))

(defmethod combination-short3 combination-short2 (a)
  a)

(deftest define-method-combination-short.2
  (combination-short3 100)
  100)

(define-method-combination combination-short4 :operator *)
(defgeneric combination-short5 (a) (:method-combination combination-short4))
(defmethod combination-short5 combination-short4 (a)
  (+ a 10))

(defmethod combination-short5 combination-short4 ((z integer))
  (+ z 99))

(deftest define-method-combination-short.3
  (combination-short5 100)
  21890)

;; primary 0 (error)
(define-method-combination combination-primary0 :operator +)
(defgeneric combination-primary0 (a) (:method-combination combination-primary0))
(deftest-error combination-primary0.1
  (combination-primary0 :Hello))

;; primary 2
(define-method-combination combination-primary2 :operator +)
(defgeneric combination-primary2 (a) (:method-combination combination-primary2))
(defmethod combination-primary2 combination-primary2 (a)
  (push (list :primary1 a) *value*)
  a)

(defmethod combination-primary2 combination-primary2 ((a integer))
  (push (list :primary2 a) *value*)
  a)

(deftest combination-primary2.1
  (progn
    (setq *value* nil)
    (values
      (combination-primary2 100)
      *value*))
  200 ((:primary1 100) (:primary2 100)))

;; primary 2, :most-specific-first
(define-method-combination combination-primary2a :operator +)
(defgeneric combination-primary2a (a)
            (:method-combination combination-primary2a :most-specific-first))
(defmethod combination-primary2a combination-primary2a (a)
  (push (list :primary1 a) *value*)
  a)

(defmethod combination-primary2a combination-primary2a ((a integer))
  (push (list :primary2 a) *value*)
  a)

(deftest combination-primary2a.1
  (progn
    (setq *value* nil)
    (values
      (combination-primary2a 100)
      *value*))
  200 ((:primary1 100) (:primary2 100)))

;; primary 2, :most-specific-last
(define-method-combination combination-primary2b :operator +)
(defgeneric combination-primary2b (a)
            (:method-combination combination-primary2b :most-specific-last))
(defmethod combination-primary2b combination-primary2b (a)
  (push (list :primary1 a) *value*)
  a)

(defmethod combination-primary2b combination-primary2b ((a integer))
  (push (list :primary2 a) *value*)
  a)

(deftest combination-primary2b.1
  (progn
    (setq *value* nil)
    (values
      (combination-primary2b 100)
      *value*))
  200 ((:primary2 100) (:primary1 100)))

;; around, primary
(define-method-combination combination-plus :operator +)
(defgeneric combination-a1p1 (a) (:method-combination combination-plus))
(defmethod combination-a1p1 combination-plus (a)
  (push (list :primary a) *value*)
  a)

(defmethod combination-a1p1 :around (a)
  (push (list :around a) *value*)
  a)

(deftest combination-a1p1.1
  (progn
    (setq *value* nil)
    (values
      (combination-a1p1 11)
      *value*))
  11 ((:around 11)))

(defgeneric combination-a1p1a (a) (:method-combination combination-plus))
(defmethod combination-a1p1a combination-plus (a)
  (push (list :primary a) *value*)
  a)

(defmethod combination-a1p1a :around (a)
  (push (list :around a) *value*)
  (call-next-method))

(deftest combination-a1p1.2
  (progn
    (setq *value* nil)
    (values
      (combination-a1p1a 11)
      *value*))
  11 ((:primary 11) (:around 11)))

;; around 2, primary 0 (error)
(defgeneric combination-a2 (a) (:method-combination combination-plus))
(defmethod combination-a2 :around (a)
  a)

(defmethod combination-a2 :around ((a integer))
  a)

(deftest-error combination-around2.1
  (combination-a2 100))


;; around 2, primary 2
(defgeneric combination-a2p2 (a) (:method-combination combination-plus))
(defmethod combination-a2p2 :around ((a integer))
  (push (list :around1 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod combination-a2p2 :around (a)
  (push (list :around2 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod combination-a2p2 combination-plus ((a integer))
  (push (list :primary1 a) *value*)
  a)

(defmethod combination-a2p2 combination-plus (a)
  (push (list :primary2 a) *value*)
  a)

(deftest combination-around2.2
  (progn
    (setq *value* nil)
    (values
      (combination-a2p2 100)
      *value*))
  200 ((:primary2 100) (:primary1 100) (:around2 100) (:around1 100)))

;; around 2, primary 2, :most-specific-first
(defgeneric combination-a2p2a (a)
            (:method-combination combination-plus :most-specific-first))
(defmethod combination-a2p2a :around ((a integer))
  (push (list :around1 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod combination-a2p2a :around (a)
  (push (list :around2 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod combination-a2p2a combination-plus ((a integer))
  (push (list :primary1 a) *value*)
  a)

(defmethod combination-a2p2a combination-plus (a)
  (push (list :primary2 a) *value*)
  a)

(deftest combination-around2.3
  (progn
    (setq *value* nil)
    (values
      (combination-a2p2a 100)
      *value*))
  200 ((:primary2 100) (:primary1 100) (:around2 100) (:around1 100)))

;; around 2, primary 2, :most-specific-last
(defgeneric combination-a2p2b (a)
            (:method-combination combination-plus :most-specific-last))
(defmethod combination-a2p2b :around ((a integer))
  (push (list :around1 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod combination-a2p2b :around (a)
  (push (list :around2 a) *value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod combination-a2p2b combination-plus ((a integer))
  (push (list :primary1 a) *value*)
  a)

(defmethod combination-a2p2b combination-plus (a)
  (push (list :primary2 a) *value*)
  a)

(deftest combination-around2.4
  (progn
    (setq *value* nil)
    (values
      (combination-a2p2b 100)
      *value*))
  200 ((:primary1 100) (:primary2 100) (:around2 100) (:around1 100)))

;; primary 1, identity
(defun combination-identity (&rest args)
  (with-output-to-string (stream)
    (format stream "<<<")
    (let (check)
      (dolist (arg args)
        (if check
          (format stream " ~A" arg)
          (format stream "~A" arg))
        (setq check t)))
    (format stream ">>>")))

(define-method-combination combination-identity1 :operator combination-identity)
(defgeneric combination-identity1 (a) (:method-combination combination-identity1))
(defmethod combination-identity1 combination-identity1 (a)
  (+ 10 a))

(deftest combination-identity.1
  (combination-identity1 111)
  "<<<121>>>")

(define-method-combination
  combination-identity2
  :operator combination-identity
  :identity-with-one-argument nil)
(defgeneric combination-identity2 (a) (:method-combination combination-identity2))
(defmethod combination-identity2 combination-identity2 (a)
  (+ 20 a))

(deftest combination-identity.2
  (combination-identity2 111)
  "<<<131>>>")

;; primary 1, no-identity
(define-method-combination
  combination-identity3
  :operator combination-identity
  :identity-with-one-argument t)
(defgeneric combination-identity3 (a) (:method-combination combination-identity3))
(defmethod combination-identity3 combination-identity3 (a)
  (+ 30 a))

(deftest combination-identity.3
  (combination-identity3 111)
  141)

;; primary 2, identity
(define-method-combination
  combination-identity4
  :operator combination-identity
  :identity-with-one-argument t)
(defgeneric combination-identity4 (a) (:method-combination combination-identity4))
(defmethod combination-identity4 combination-identity4 (a)
  (+ 40 a))

(defmethod combination-identity4 combination-identity4 ((a integer))
  (+ 50 a))

(deftest combination-identity.4
  (combination-identity4 111)
  "<<<161 151>>>")

;; primary 2, no-identity
(define-method-combination
  combination-identity5
  :operator combination-identity
  :identity-with-one-argument nil)
(defgeneric combination-identity5 (a) (:method-combination combination-identity5))
(defmethod combination-identity5 combination-identity5 (a)
  (+ 50 a))

(defmethod combination-identity5 combination-identity5 ((a integer))
  (+ 60 a))

(deftest combination-identity.5
  (combination-identity5 111)
  "<<<171 161>>>")

;; default progn
(defgeneric combination-progn1 (z) (:method-combination progn))
(defmethod combination-progn1 progn (a)
  (push (list :primary1 a) *value*)
  :primary1)

(defmethod combination-progn1 progn ((a integer))
  (push (list :primary2 a) *value*)
  :primary2)

(deftest combination-progn.1
  (progn
    (setq *value* nil)
    (values
      (combination-progn1 10)
      *value*))
  :primary1 ((:primary1 10) (:primary2 10)))

;; default list 2
(defgeneric combination-list1 (z) (:method-combination list))
(defmethod combination-list1 list (a)
  (push (list :primary1 a) *value*)
  :primary1)

(defmethod combination-list1 list ((a integer))
  (push (list :primary2 a) *value*)
  :primary2)

(deftest combination-list.1
  (progn
    (setq *value* nil)
    (values
      (combination-list1 10)
      *value*))
  (:primary2 :primary1) ((:primary1 10) (:primary2 10)))

;; default list, no-identity
(defgeneric combination-list2 (z) (:method-combination list))
(defmethod combination-list2 list (a)
  (push (list :primary1 a) *value*)
  :primary1)

(deftest combination-list.2
  (progn
    (setq *value* nil)
    (values
      (combination-list2 10)
      *value*))
  (:primary1) ((:primary1 10)))

;; default list, :most-specific-first
(defgeneric combination-list3 (z) (:method-combination list :most-specific-first))
(defmethod combination-list3 list (a)
  (push (list :primary1 a) *value*)
  :primary1)

(defmethod combination-list3 list ((a integer))
  (push (list :primary2 a) *value*)
  :primary2)

(deftest combination-list.3
  (progn
    (setq *value* nil)
    (values
      (combination-list3 10)
      *value*))
  (:primary2 :primary1) ((:primary1 10) (:primary2 10)))

;; default list, :most-specific-last
(defgeneric combination-list4 (z) (:method-combination list :most-specific-last))
(defmethod combination-list4 list (a)
  (push (list :primary1 a) *value*)
  :primary1)

(defmethod combination-list4 list ((a integer))
  (push (list :primary2 a) *value*)
  :primary2)

(deftest combination-list.4
  (progn
    (setq *value* nil)
    (values
      (combination-list4 10)
      *value*))
  (:primary1 :primary2) ((:primary2 10) (:primary1 10)))


;;
;;  compute-applicable-methods
;;
(defgeneric compute-applicable-methods1 (a))
(defmethod compute-applicable-methods1 (a)
  (+ a 100))

(deftest compute-applicable-methods.1
  (length
    (compute-applicable-methods
      #'compute-applicable-methods1
      '(10)))
  1)

(defgeneric compute-applicable-methods2 (a))
(defmethod compute-applicable-methods2 (a)
  (+ a 100))

(defmethod compute-applicable-methods2 ((a integer))
  (+ a 200))

(defmethod compute-applicable-methods2 ((a string))
  (values 300 a))

(deftest compute-applicable-methods.2
  (length
    (compute-applicable-methods
      #'compute-applicable-methods2
      '(10)))
  2)

(deftest compute-applicable-methods.3
  (length
    (compute-applicable-methods
      #'compute-applicable-methods2
      '("hello")))
  2)

(deftest compute-applicable-methods.4
  (length
    (compute-applicable-methods
      #'compute-applicable-methods2
      '(t)))
  1)

(defgeneric compute-applicable-methods3 (a))
(defmethod compute-applicable-methods3 (a)
  (+ a 100))

(defmethod compute-applicable-methods3 ((a integer))
  (+ a 200))

(defmethod compute-applicable-methods3 :around (a)
  (values 300 a))

(defmethod compute-applicable-methods3 :before (a)
  (values 400 a))

(deftest compute-applicable-methods.5
  (length
    (compute-applicable-methods
      #'compute-applicable-methods3
      '(10)))
  4)

