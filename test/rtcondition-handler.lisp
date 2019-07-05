;;
;;  ANSI COMMON LISP: 9. Conditions
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
;;  error
;;
(deftest-error error.1
  (error "Hello")
  error)

(deftest-error error.2
  (error "Hello")
  simple-error)

(deftest-error error.3
  (error (make-condition 'program-error))
  program-error)

(deftest error.4
  (handler-case
    (error "Hello: ~A ~A ~A" 10 20 30)
    (error (c)
      (values
        (simple-condition-format-control c)
        (simple-condition-format-arguments c))))
  "Hello: ~A ~A ~A" (10 20 30))


;;
;;  signal
;;
(deftest signal.1
  (signal "Hello")
  nil)

(deftest signal.2
  (signal (make-condition 'program-error))
  nil)

(deftest signal.3
  (handler-case
    (signal "Hello: ~A ~A ~A" 10 20 30)
    (error (c)
      (values
        (simple-condition-format-control c)
        (simple-condition-format-arguments c))))
  nil)

(deftest signal.4
  (handler-case
    (signal "Hello: ~A ~A ~A" 10 20 30)
    (simple-condition (c)
      (values
        (simple-condition-format-control c)
        (simple-condition-format-arguments c))))
  "Hello: ~A ~A ~A" (10 20 30))


;;
;;  warn
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

(deftest cerror.1
  (handler-case
    (cerror "Hello" "ABCD")
    (error () :hello))
  :hello)

