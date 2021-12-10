;;
;;  ANSI COMMON LISP: 9. Conditions
;;

;;
;;  Function ERROR
;;
(deftest-error error.1
  (error "Hello")
  error)

(deftest-error error.2
  (error "Hello" 10 20 30)
  simple-error)

(deftest-error error.3
  (error 'program-error)
  program-error)

(deftest-error error.4
  (error (make-condition 'program-error))
  program-error)

(deftest error.5
  (handler-case
    (error "Hello: ~A ~A ~A" 10 20 30)
    (simple-error (c)
      (values
        (simple-condition-format-control c)
        (simple-condition-format-arguments c))))
  "Hello: ~A ~A ~A" (10 20 30))

(deftest error.6
  (handler-case
    (error 'simple-error
      :format-control "Hello: ~A ~A ~A"
      :format-arguments '(10 20 30))
    (simple-error (c)
      (values
        (simple-condition-format-control c)
        (simple-condition-format-arguments c))))
  "Hello: ~A ~A ~A" (10 20 30))

(deftest-error error.7
  (handler-case
    ;; invalid-arguments
    (error (make-condition 'program-error) 10 20 30)
    (program-error () 'ignore)))

(deftest-error! error-error.1
  (eval '(error)))

(deftest-error! error-error.2
  (eval '(error 100)))

;;  ANSI Common Lisp
(defun error-factorial (x)
  (cond ((or (not (typep x 'integer)) (minusp x))
         (error "~S is not a valid argument to FACTORIAL." x))
        ((zerop x) 1)
        (t (* x (error-factorial (- x 1))))))

(deftest error-test.1
  (error-factorial 20)
  2432902008176640000)

(deftest-error error-test.2
  (error-factorial -1))

(deftest-error error-test.3
  (let ((a 'fred))
    (if (numberp a)
      (1+ a)
      (error "~S is not a number." A))))

(define-condition error-not-a-number (error)
  ((argument :reader error-not-a-number-argument :initarg :argument))
  (:report (lambda (condition stream)
             (format stream "~S is not a number."
                     (error-not-a-number-argument condition)))))

(deftest error-test.4
  (handler-case
    (let ((a 'fred))
      (if (numberp a)
        (1+ a)
        (error 'error-not-a-number :argument a)))
    (error-not-a-number (c) (format nil "~S" c)))
  "FRED is not a number.")


;;
;;  Function CERROR
;;
(deftest-error cerror.1
  (cerror "Hello" "ABCD")
  error)

(deftest-error cerror.2
  (cerror "Hello" "ABCD")
  simple-error)

(deftest cerror.3
  (handler-case
    (cerror "Hello" "ABCD" 10 20 30)
    (simple-error (c)
      (values
        (simple-condition-format-control c)
        (simple-condition-format-arguments c))))
  "ABCD" (10 20 30))

(deftest cerror.4
  (handler-bind ((error #'continue))
    (cerror "AAA" "BBB"))
  nil)

(deftest cerror.5
  (handler-bind
    ((simple-error
       (lambda (c)
         (unless (find-restart 'continue c)
           (error "cerror find-restart error."))
         (continue c))))
    (cerror "AAA: ~A" "BBB: ~A" 10))
  nil)

(deftest cerror.6
  (let (x)
    (handler-bind
      ((simple-error
         (lambda (c)
           (setq x (format nil "~A" (find-restart 'continue c)))
           (continue c))))
      (cerror "AAA: ~A" "BBB: ~A" 10)
      x))
  "AAA: 10")

(deftest cerror.7
  (handler-case
    (cerror "AAA: ~A" "BBB: ~A" 10)
    (simple-error (c)
      (values
        (simple-condition-format-control c)
        (simple-condition-format-arguments c))))
  "BBB: ~A" (10))

(deftest-error cerror.8
  (cerror "ABC" "Hello" 10 20 30)
  simple-error)

(deftest-error cerror.9
  (cerror "ABC" 'program-error)
  program-error)

(deftest-error cerror.10
  (cerror "ABC" (make-condition 'program-error))
  program-error)

(deftest-error! cerror-error.1
  (eval '(cerror "HELLO")))

(deftest-error! cerror-error.2
  (eval '(cerror 10 "Hello")))


;;  ANSI Common Lisp
(defun cerror-real-sqrt (n)
  (when (minusp n)
    (setq n (- n))
    (cerror "Return sqrt(~D) instead." "Tried to take sqrt(-~D)." n))
  (sqrt n))

(deftest cerror-test.1
  (cerror-real-sqrt 4)
  2.0)

(deftest-error cerror-test.2
  (cerror-real-sqrt -9))

(deftest cerror-test.3
  (handler-bind ((simple-error #'continue))
    (cerror-real-sqrt -9))
  3.0)

(define-condition cerror-not-a-number (error)
  ((argument :reader cerror-not-a-number-argument :initarg :argument))
  (:report (lambda (condition stream)
             (format stream "~S is not a number."
                     (cerror-not-a-number-argument condition)))))

(defun cerror-assure-number (n)
  (loop (when (numberp n) (return n))
        (cerror "Enter a number." 'cerror-not-a-number :argument n)
        (setq n 1/2)))

(deftest cerror-test.4
  (cerror-assure-number 100)
  100)

(deftest cerror-test.5
  (handler-bind ((cerror-not-a-number #'continue))
    (cerror-assure-number 'a))
  1/2)

(defun cerror-assure-large-number (n)
  (loop (when (and (numberp n) (> n 73)) (return n))
        (cerror "Enter a number~:[~; a bit larger than ~D~]."
                "~*~A is not a large number."
                (numberp n) n)
        (setq n 999)))

(deftest cerror-test.6
  (cerror-assure-large-number 10000)
  10000)

(deftest-error cerror-test.7
  (cerror-assure-large-number 'a))

(deftest cerror-test.8
  (handler-bind ((simple-error #'continue))
    (cerror-assure-large-number 'a))
  999)

(deftest cerror-test.9
  (handler-bind ((simple-error #'continue))
    (cerror-assure-large-number 37))
  999)

(define-condition cerror-not-a-large-number (error)
  ((argument :reader cerror-not-a-large-number-argument :initarg :argument))
  (:report (lambda (condition stream)
             (format stream "~S is not a large number."
                     (cerror-not-a-large-number-argument condition)))))

(defun cerror-assure-large-number-2 (n)
  (loop (when (and (numberp n) (> n 73)) (return n))
        (cerror "Enter a number~3*~:[~; a bit larger than ~*~D~]."
                'cerror-not-a-large-number
                :argument n
                :ignore (numberp n)
                :ignore n
                :allow-other-keys t)
        (setq n 888)))

(deftest-error cerror-test.10
  (cerror-assure-large-number-2 'a))

(deftest cerror-test.11
  (handler-bind ((simple-error #'continue))
    (cerror-assure-large-number-2 'a))
  888)

(deftest-error cerror-test.12
  (cerror-assure-large-number-2 37))

(deftest cerror-test.13
  (handler-bind ((simple-error #'continue))
    (cerror-assure-large-number-2 37))
  888)


;;
;;  Function SIGNAL
;;
(deftest signal.1
  (signal "Hello")
  nil)

(deftest signal.2
  (handler-case
    (signal "Hello")
    (simple-condition (c)
      (values (simple-condition-format-control c)
              (simple-condition-format-arguments c))))
  "Hello" nil)

(deftest signal.3
  (signal "Hello" 10 20 30)
  nil)

(deftest signal.4
  (handler-case
    (signal "Hello" 10 20 30)
    (simple-condition (c)
      (values (simple-condition-format-control c)
              (simple-condition-format-arguments c))))
  "Hello" (10 20 30))

(deftest signal.5
  (signal 'program-error)
  nil)

(deftest signal.6
  (handler-case
    (signal 'program-error)
    (program-error () 'hit))
  hit)

(deftest signal.7
  (signal (make-condition 'program-error))
  nil)

(deftest signal.8
  (handler-case
    (signal (make-condition 'program-error))
    (program-error () 'hit))
  hit)

(deftest signal.9
  (signal 'simple-error :format-control "AAA" :format-arguments '(10 20 30))
  nil)

(deftest signal.10
  (handler-case
    (signal 'simple-error :format-control "AAA" :format-arguments '(aa bb))
    (simple-condition (c)
      (values (simple-condition-format-control c)
              (simple-condition-format-arguments c))))
  "AAA" (aa bb))

(deftest signal.11
  (handler-case
    (signal "Hello: ~A ~A ~A" 10 20 30)
    (simple-error (c)
      (values
        (simple-condition-format-control c)
        (simple-condition-format-arguments c))))
  nil)

(deftest signal.12
  (handler-case
    (signal "Hello: ~A ~A ~A" 10 20 30)
    (simple-condition (c)
      (values
        (simple-condition-format-control c)
        (simple-condition-format-arguments c))))
  "Hello: ~A ~A ~A" (10 20 30))

(deftest-error signal.13
  (handler-case
    ;; invalid-arguments
    (signal (make-condition 'program-error) 10 20 30)
    (program-error () 'ignore)))

(deftest-error! signal-error.1
  (eval '(signal)))

(deftest-error! signal-error.2
  (eval '(signal 100)))


;;
;;  Function WARN
;;
(deftest warn.1
  (equal
    (with-output-to-string (*error-output*)
      (warn "Hello"))
    (format nil "WARNING: Hello~%"))
  t)

(deftest warn.2
  (let (value)
    (with-output-to-string (*error-output*)
      (setq value (warn "Hello")))
    value)
  nil)

(deftest warn.3
  (handler-case
    (warn "Hello: ~A ~A ~A" 10 20 30)
    (simple-warning (c)
      (values
        (simple-condition-format-control c)
        (simple-condition-format-arguments c))))
  "Hello: ~A ~A ~A" (10 20 30))

(deftest warn.4
  (with-output-to-string (*error-output*)
    (handler-bind ((warning #'muffle-warning))
      (warn "Hello")))
  "")

(deftest warn.5
  (let (value)
    (with-output-to-string (*error-output*)
      (handler-bind ((warning #'muffle-warning))
        (setq value (warn "Hello"))))
    value)
  nil)

(deftest-error warn.6
  (warn (make-condition 'simple-error))
  type-error)

(deftest-error warn.7
  (warn 'simple-error)
  type-error)

(deftest-error! warn-error.1
  (eval '(warn)))

(deftest-error! warn-error.2
  (eval '(warn 10)))

;;  ANSI Common Lisp
(defun warn-foo (x)
  (let ((result (* x 2)))
    (if (not (typep result 'fixnum))
      (warn "You're using very big numbers."))
    result))

(deftest warn-test.1
  (warn-foo 3)
  6)

(deftest warn-test.2
  (handler-bind ((warning #'muffle-warning))
    (warn-foo most-positive-fixnum))
  #.(* 2 most-positive-fixnum))

(deftest warn-test.3
  (let (value)
    (handler-bind ((warning
                     (lambda (c)
                       (setq value t)
                       (muffle-warning c))))
      (warn-foo most-positive-fixnum))
    value)
  t)


;;
;;
;;
(deftest handler-case.1
  (handler-case
    (error "Hello")
    (error () :hello))
  :hello)

(deftest handler-case.2
  (handler-case
    (handler-case
      (error "Hello")
      (warning () :warning))
    (error () :hello))
  :hello)

(deftest handler-case.3
  (handler-case
    (handler-case
      (error "Hello")
      (warning () :warning))
    (error () :hello))
  :hello)

(deftest handler-case-no-error.1
  (handler-case
    10
    (:no-error (x) (list x)))
  (10))

(deftest-error handler-case-no-error.2
  (handler-case
    (values)
    (:no-error (x) (list x))))

(deftest-error handler-case-no-error.3
  (handler-case
    (values 10 20 30)
    (:no-error (x) (list x))))

(deftest handler-case-no-error.4
  (handler-case
    (values 10 20 30)
    (:no-error (x y z) (list x y z)))
  (10 20 30))

(deftest handler-case-no-error.5
  (handler-case
    (values 10 20 30)
    (:no-error (x y z) (values x y z)))
  10 20 30)

(deftest handler-case-no-error.6
  (handler-case
    (values 10 20 30)
    (simple-error () :hello)
    (:no-error (x y z) (list x y z)))
  (10 20 30))

(deftest handler-case-no-error.7
  (handler-case
    (error () "Hello")
    (simple-error () :hello)
    (:no-error (x y z) (list x y z)))
  :hello)

(deftest handler-case-no-error.8
  (handler-case
    (values 10 20 30)
    (:no-error (x y z) (list x y z))
    (simple-error () :hello))
  (10 20 30))

(deftest handler-case-no-error.9
  (handler-case
    (error () "Hello")
    (:no-error (x y z) (list x y z))
    (simple-error () :hello))
  :hello)

(deftest handler-bind.1
  (let ((check :aaa))
    (handler-case
      (handler-bind
        ((error (lambda (c) (declare (ignore c))
                  (setq check t))))
        (error "Hello"))
      (error ()
        (values :error check))))
  :error t)

(define-condition handler-return () ())
(deftest handler-return.1
  (values 10 20
          (handler-case 33 (handler-return () 30))
          40 50 60)
  10 20 33 40 50 60)

(deftest handler-return.2
  (list 10 20
        (handler-case
          (signal 'handler-return)
          (handler-return () 30))
        40 50 60)
  (10 20 30 40 50 60))

(deftest handler-return.3
  (values 10 20
          (handler-case
            (signal 'handler-return)
            (handler-return () 30))
          40 50 60)
  10 20 30 40 50 60)

(deftest handler-return.4
  (list 10 20
        (handler-bind ((handler-return (lambda () 30))) 33)
        40 50 60)
  (10 20 33 40 50 60))

(deftest handler-return.5
  (values 10 20
          (handler-bind ((handler-return (lambda () 30))) 33)
          40 50 60)
  10 20 33 40 50 60)

(deftest handler-return.6
  (list 10 20
        (handler-bind ((handler-return
                         (lambda (c) (declare (ignore c)) 30)))
          (signal 'handler-return) 33)
        40 50 60)
  (10 20 33 40 50 60))

(deftest handler-return.7
  (values 10 20
          (handler-bind ((handler-return
                           (lambda (c) (declare (ignore c)) 30)))
            (signal 'handler-return) 33)
          40 50 60)
  10 20 33 40 50 60)


;;
;;  function
;;
(deftest ignore-errors.1
  (ignore-errors)
  nil)

(deftest ignore-errors.2
  (ignore-errors
    10)
  10)

(deftest ignore-errors.3
  (ignore-errors
    10 20 30)
  30)

(defvar *error-variables*)
(deftest ignore-errors.4
  (multiple-value-bind (a b)
    (ignore-errors
      10 20 *error-variables*)
    (values a (null b)))
  nil nil)

(deftest ignore-errors.5
  (multiple-value-bind (a b)
    (ignore-errors
      10 20 *error-variables* 30 40)
    (values a (null b)))
  nil nil)

