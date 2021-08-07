;;
;;  ANSI COMMON LISP: 7. Objects
;;
(defvar *combination-short-value*)

;;
;;  define-method-combination short
;;
(defun combination-short-1 (&rest args)
  (apply #'+ args))

(deftest combination-short.1
  (define-method-combination combination-short-1)
  combination-short-1)

(defun combination-short-2 (&rest args)
  (apply #'+ args))

(define-method-combination combination-short-2)
(defgeneric combination-short-3 (a)
  (:method-combination combination-short-2))

(defmethod combination-short-3 combination-short-2 (a)
  a)

(deftest combination-short.2
  (combination-short-3 100)
  100)

(define-method-combination combination-short-4 :operator *)
(defgeneric combination-short-5 (a)
  (:method-combination combination-short-4))
(defmethod combination-short-5 combination-short-4 (a)
  (+ a 10))

(defmethod combination-short-5 combination-short-4 ((z integer))
  (+ z 99))

(deftest combination-short.3
  (combination-short-5 100)
  21890)


;;
;;  macro
;;
(defmacro combination-short-macro-1 (&rest args)
  `(* 100 (+ ,@args)))

(define-method-combination
  combination-short-macro-2
  :operator combination-short-macro-1)

(defgeneric combination-short-macro-3 (x)
  (:method-combination combination-short-macro-2))

(defmethod combination-short-macro-3 combination-short-macro-2 (x)
  (declare (ignore x))
  1)

(defmethod combination-short-macro-3 combination-short-macro-2 ((x integer))
  (declare (ignore x))
  2)

(defmethod combination-short-macro-3 combination-short-macro-2 ((x real))
  (declare (ignore x))
  3)

(deftest combination-short-macro.1
  (combination-short-macro-3 100)
  600)


;;
;;  documentation
;;
(define-method-combination
  combination-short-documentation-1
  :operator +
  :documentation "Hello-Short")

(deftest combination-short-documentation.1
  (documentation 'combination-short-documentation-1 'method-combination)
  "Hello-Short")

(deftest combination-short-documentation.2
  (documentation
    (find-method-combination
      (defgeneric combination-short-documentation-2 ())
      'combination-short-documentation-1
      nil)
    'method-combination)
  "Hello-Short")


;;
;;  primary 0 (error)
;;
(define-method-combination combination-short-primary-1 :operator +)
(defgeneric combination-short-primary-1 (a)
  (:method-combination combination-short-primary-1))
(deftest-error combination-short-primary.1
  (combination-short-primary-1 :Hello))


;;
;;  primary 2
;;
(define-method-combination combination-short-primary-2 :operator +)
(defgeneric combination-short-primary-2 (a)
  (:method-combination combination-short-primary-2))
(defmethod combination-short-primary-2 combination-short-primary-2 (a)
  (push (list :primary1 a) *combination-short-value*)
  a)

(defmethod combination-short-primary-2 combination-short-primary-2 ((a integer))
  (push (list :primary2 a) *combination-short-value*)
  a)

(deftest combination-short-primary.2
  (progn
    (setq *combination-short-value* nil)
    (values
      (combination-short-primary-2 100)
      *combination-short-value*))
  200 ((:primary1 100) (:primary2 100)))


;;
;;  :most-specific-first
;;
(define-method-combination combination-short-most-1 :operator +)
(defgeneric combination-short-most-1 (a)
  (:method-combination combination-short-most-1 :most-specific-first))
(defmethod combination-short-most-1 combination-short-most-1 (a)
  (push (list :primary1 a) *combination-short-value*)
  a)

(defmethod combination-short-most-1 combination-short-most-1 ((a integer))
  (push (list :primary2 a) *combination-short-value*)
  a)

(deftest combination-short-most.1
  (progn
    (setq *combination-short-value* nil)
    (values
      (combination-short-most-1 100)
      *combination-short-value*))
  200 ((:primary1 100) (:primary2 100)))

;; primary 2, :most-specific-last
(define-method-combination combination-short-most-2 :operator +)
(defgeneric combination-short-most-2 (a)
  (:method-combination combination-short-most-2 :most-specific-last))
(defmethod combination-short-most-2 combination-short-most-2 (a)
  (push (list :primary1 a) *combination-short-value*)
  a)

(defmethod combination-short-most-2 combination-short-most-2 ((a integer))
  (push (list :primary2 a) *combination-short-value*)
  a)

(deftest combination-short-most.2
  (progn
    (setq *combination-short-value* nil)
    (values
      (combination-short-most-2 100)
      *combination-short-value*))
  200 ((:primary2 100) (:primary1 100)))


;;
;;  around, primary
;;
(define-method-combination combination-short-plus :operator +)
(defgeneric combination-short-around-primary-1 (a)
  (:method-combination combination-short-plus))
(defmethod combination-short-around-primary-1 combination-short-plus (a)
  (push (list :primary a) *combination-short-value*)
  a)

(defmethod combination-short-around-primary-1 :around (a)
  (push (list :around a) *combination-short-value*)
  a)

(deftest combination-short-around-primary.1
  (progn
    (setq *combination-short-value* nil)
    (values
      (combination-short-around-primary-1 11)
      *combination-short-value*))
  11 ((:around 11)))

(defgeneric combination-short-around-primary-2 (a)
  (:method-combination combination-short-plus))
(defmethod combination-short-around-primary-2 combination-short-plus (a)
  (push (list :primary a) *combination-short-value*)
  a)

(defmethod combination-short-around-primary-2 :around (a)
  (push (list :around a) *combination-short-value*)
  (call-next-method))

(deftest combination-short-around-primary.2
  (progn
    (setq *combination-short-value* nil)
    (values
      (combination-short-around-primary-2 11)
      *combination-short-value*))
  11 ((:primary 11) (:around 11)))


;;
;;  around 2, primary 0 (error)
;;
(defgeneric combinatino-around-error-1 (a)
  (:method-combination combination-short-plus))
(defmethod combinatino-around-error-1 :around (a)
  a)

(defmethod combinatino-around-error-1 :around ((a integer))
  a)

(deftest-error combination-short-around-error.1
  (combinatino-around-error-1 100))


;;
;;  around 2, primary 2
;;
(defgeneric combination-short-around-primary-3 (a)
  (:method-combination combination-short-plus))
(defmethod combination-short-around-primary-3 :around ((a integer))
  (push (list :around1 a) *combination-short-value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod combination-short-around-primary-3 :around (a)
  (push (list :around2 a) *combination-short-value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod combination-short-around-primary-3 combination-short-plus ((a integer))
  (push (list :primary1 a) *combination-short-value*)
  a)

(defmethod combination-short-around-primary-3 combination-short-plus (a)
  (push (list :primary2 a) *combination-short-value*)
  a)

(deftest combination-short-around-primary.3
  (progn
    (setq *combination-short-value* nil)
    (values
      (combination-short-around-primary-3 100)
      *combination-short-value*))
  200 ((:primary2 100) (:primary1 100) (:around2 100) (:around1 100)))


;;
;;  around 2, primary 2, :most-specific-first
;;
(defgeneric combination-short-around-primary-4 (a)
  (:method-combination combination-short-plus :most-specific-first))
(defmethod combination-short-around-primary-4 :around ((a integer))
  (push (list :around1 a) *combination-short-value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod combination-short-around-primary-4 :around (a)
  (push (list :around2 a) *combination-short-value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod combination-short-around-primary-4 combination-short-plus ((a integer))
  (push (list :primary1 a) *combination-short-value*)
  a)

(defmethod combination-short-around-primary-4 combination-short-plus (a)
  (push (list :primary2 a) *combination-short-value*)
  a)

(deftest combination-short-around-primary.4
  (progn
    (setq *combination-short-value* nil)
    (values
      (combination-short-around-primary-4 100)
      *combination-short-value*))
  200 ((:primary2 100) (:primary1 100) (:around2 100) (:around1 100)))


;;
;;  around 2, primary 2, :most-specific-last
;;
(defgeneric combination-short-most-3 (a)
  (:method-combination combination-short-plus :most-specific-last))
(defmethod combination-short-most-3 :around ((a integer))
  (push (list :around1 a) *combination-short-value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod combination-short-most-3 :around (a)
  (push (list :around2 a) *combination-short-value*)
  (when (next-method-p)
    (call-next-method)))

(defmethod combination-short-most-3 combination-short-plus ((a integer))
  (push (list :primary1 a) *combination-short-value*)
  a)

(defmethod combination-short-most-3 combination-short-plus (a)
  (push (list :primary2 a) *combination-short-value*)
  a)

(deftest combination-short-most.3
  (progn
    (setq *combination-short-value* nil)
    (values
      (combination-short-most-3 100)
      *combination-short-value*))
  200 ((:primary1 100) (:primary2 100) (:around2 100) (:around1 100)))


;;
;;  primary 1, identity
;;
(defun combination-short-identity-1 (&rest args)
  (with-output-to-string (stream)
    (format stream "<<<")
    (let (check)
      (dolist (arg args)
        (if check
          (format stream " ~A" arg)
          (format stream "~A" arg))
        (setq check t)))
    (format stream ">>>")))

(define-method-combination combination-short-identity-2 :operator combination-short-identity-1)
(defgeneric combination-short-identity-2 (a)
  (:method-combination combination-short-identity-2))
(defmethod combination-short-identity-2 combination-short-identity-2 (a)
  (+ 10 a))

(deftest combination-short-identity.1
  (combination-short-identity-2 111)
  "<<<121>>>")

(define-method-combination
  combination-short-identity-3
  :operator combination-short-identity-1
  :identity-with-one-argument nil)
(defgeneric combination-short-identity-3 (a)
  (:method-combination combination-short-identity-3))
(defmethod combination-short-identity-3 combination-short-identity-3 (a)
  (+ 20 a))

(deftest combination-short-identity.2
  (combination-short-identity-3 111)
  "<<<131>>>")


;;
;;  primary 1, no-identity
;;
(define-method-combination
  combination-short-identity-4
  :operator combination-short-identity-1
  :identity-with-one-argument t)
(defgeneric combination-short-identity-4 (a)
  (:method-combination combination-short-identity-4))
(defmethod combination-short-identity-4 combination-short-identity-4 (a)
  (+ 30 a))

(deftest combination-short-identity.3
  (combination-short-identity-4 111)
  141)


;;
;;  primary 2, identity
;;
(define-method-combination
  combination-short-identity-5
  :operator combination-short-identity-1
  :identity-with-one-argument t)
(defgeneric combination-short-identity-5 (a)
  (:method-combination combination-short-identity-5))
(defmethod combination-short-identity-5 combination-short-identity-5 (a)
  (+ 40 a))

(defmethod combination-short-identity-5 combination-short-identity-5 ((a integer))
  (+ 50 a))

(deftest combination-short-identity.4
  (combination-short-identity-5 111)
  "<<<161 151>>>")


;;
;;  primary 2, no-identity
;;
(define-method-combination
  combination-short-identity-6
  :operator combination-short-identity-1
  :identity-with-one-argument nil)
(defgeneric combination-short-identity-6 (a)
  (:method-combination combination-short-identity-6))
(defmethod combination-short-identity-6 combination-short-identity-6 (a)
  (+ 50 a))

(defmethod combination-short-identity-6 combination-short-identity-6 ((a integer))
  (+ 60 a))

(deftest combination-short-identity.5
  (combination-short-identity-6 111)
  "<<<171 161>>>")


;;
;;  default progn
;;
(defgeneric combination-short-progn-1 (z)
  (:method-combination progn))
(defmethod combination-short-progn-1 progn (a)
  (push (list :primary1 a) *combination-short-value*)
  :primary1)

(defmethod combination-short-progn-1 progn ((a integer))
  (push (list :primary2 a) *combination-short-value*)
  :primary2)

(deftest combination-short-progn.1
  (progn
    (setq *combination-short-value* nil)
    (values
      (combination-short-progn-1 10)
      *combination-short-value*))
  :primary1 ((:primary1 10) (:primary2 10)))


;;
;;  default list 2
;;
(defgeneric combination-short-list-1 (z)
  (:method-combination list))
(defmethod combination-short-list-1 list (a)
  (push (list :primary1 a) *combination-short-value*)
  :primary1)

(defmethod combination-short-list-1 list ((a integer))
  (push (list :primary2 a) *combination-short-value*)
  :primary2)

(deftest combination-short-list.1
  (progn
    (setq *combination-short-value* nil)
    (values
      (combination-short-list-1 10)
      *combination-short-value*))
  (:primary2 :primary1) ((:primary1 10) (:primary2 10)))


;;
;;  default list, no-identity
;;
(defgeneric combination-short-list-2 (z)
  (:method-combination list))
(defmethod combination-short-list-2 list (a)
  (push (list :primary1 a) *combination-short-value*)
  :primary1)

(deftest combination-short-list.2
  (progn
    (setq *combination-short-value* nil)
    (values
      (combination-short-list-2 10)
      *combination-short-value*))
  (:primary1) ((:primary1 10)))


;;
;;  default list, :most-specific-first
;;
(defgeneric combination-short-list-3 (z)
  (:method-combination list :most-specific-first))
(defmethod combination-short-list-3 list (a)
  (push (list :primary1 a) *combination-short-value*)
  :primary1)

(defmethod combination-short-list-3 list ((a integer))
  (push (list :primary2 a) *combination-short-value*)
  :primary2)

(deftest combination-short-list.3
  (progn
    (setq *combination-short-value* nil)
    (values
      (combination-short-list-3 10)
      *combination-short-value*))
  (:primary2 :primary1) ((:primary1 10) (:primary2 10)))


;;
;;  default list, :most-specific-last
;;
(defgeneric combination-short-list-4 (z)
  (:method-combination list :most-specific-last))
(defmethod combination-short-list-4 list (a)
  (push (list :primary1 a) *combination-short-value*)
  :primary1)

(defmethod combination-short-list-4 list ((a integer))
  (push (list :primary2 a) *combination-short-value*)
  :primary2)

(deftest combination-short-list.4
  (progn
    (setq *combination-short-value* nil)
    (values
      (combination-short-list-4 10)
      *combination-short-value*))
  (:primary1 :primary2) ((:primary2 10) (:primary1 10)))


;;
;;  error
;;
(define-method-combination combination-short-error-1 :operator +)
(defgeneric combination-short-error-2 ()
  (:method-combination combination-short-error-1))

(deftest-error combination-short-error.1
  (eval '(defmethod combination-short-error-2 ()
           :hello)))

(deftest-error combination-short-error.2
  (eval '(defmethod combination-short-error-2 combination-short-error-1 hello ()
           :hello)))

