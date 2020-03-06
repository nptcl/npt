;;
;;  optimize-scope
;;
(declaim (optimize (speed 1) (safety 1)))

(import 'lisp-system::optimize-check)

(deftest rtcode-scope-default
  (progn
    (declaim (optimize (speed 1) (safety 1)))
    (values)))

(defmacro scope-speed (&body body)
  `(locally
     (declare (optimize (safety 0) (speed 3)))
     ,@body))

(defmacro scope-safety (&body body)
  `(locally
     (declare (optimize (safety 3) (speed 0)))
     ,@body))

(deftest scope-speed.1
  (scope-speed
    (optimize-check scope))
  1)

(deftest scope-safety.1
  (scope-safety
    (optimize-check scope))
  0)


;;
;;  car
;;
(deftest scope-car.1
  (scope-speed
    (car '(a . b)))
  a)

(deftest scope-car.2
  (scope-speed
    (values
      (car '(a . b))))
  a)

(deftest scope-car.3
  (scope-speed
    (multiple-value-prog1 t
      (car '(a . b))))
  t)

(deftest-error scope-car.4
  (scope-speed
    (funcall (lambda (x) (car x)) 10))
  type-error)

(deftest-error scope-car.5
  (scope-speed
    (funcall (lambda (x)
               (values (car x)))
             10))
  type-error)

(deftest-error scope-car.6
  (scope-speed
    (funcall (lambda (x)
               (multiple-value-prog1 t
                 (car x)))
             10))
  type-error)

(deftest scope-car.7
  (scope-speed
    (let (y)
      (values
        (funcall (lambda (x)
                   (multiple-value-prog1 t
                     (car (progn
                            (setq y 20)
                            x))))
                 '(a . b))
        y)))
  t 20)

(deftest scope-car.8
  (scope-speed
    (let (y)
      (values
        (funcall (lambda (x)
                   (declare (type list x))
                   (multiple-value-prog1 t
                     (car (progn
                            (setq y 20)
                            x))))
                 '(a . b))
        y)))
  t 20)


;;
;;  cdr
;;
(deftest scope-cdr.1
  (scope-speed
    (cdr '(a . b)))
  b)

(deftest scope-cdr.2
  (scope-speed
    (values
      (cdr '(a . b))))
  b)

(deftest scope-cdr.3
  (scope-speed
    (multiple-value-prog1 t
      (cdr '(a . b))))
  t)

(deftest-error scope-cdr.4
  (scope-speed
    (funcall (lambda (x) (cdr x)) 10))
  type-error)

(deftest-error scope-cdr.5
  (scope-speed
    (funcall (lambda (x)
               (values (cdr x)))
             10))
  type-error)

(deftest-error scope-cdr.6
  (scope-speed
    (funcall (lambda (x)
               (multiple-value-prog1 t
                 (cdr x)))
             10))
  type-error)

(deftest scope-cdr.7
  (scope-safety
    (cdr '(a . b)))
  b)


;;
;;  cons
;;
(deftest scope-cons.1
  (scope-speed
    (cons 10 20))
  (10 . 20))

(deftest scope-cons.2
  (scope-safety
    (values
      (cons 10 20)))
  (10 . 20))

(deftest scope-cons.3
  (scope-safety
    (let (x)
      (values
        (multiple-value-prog1 t
          (cons (setq x 10) 20))
        x)))
  t 10)

(deftest scope-cons.4
  (scope-safety
    (cons 10 20))
  (10 . 20))

