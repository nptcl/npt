;;
;;  ANSI COMMON LISP: 9. Conditions
;;
(deftest define-condition.1
  (define-condition define-condition1 () ())
  define-condition1)

(deftest define-condition.2
  (subtypep 'define-condition1 'condition)
  t t)

(deftest define-condition.3
  (define-condition define-condition3 () ()
    (:report "Hello"))
  define-condition3)

(deftest define-condition.4
  (with-output-to-string (stream)
    (princ (make-condition 'define-condition3) stream))
  "Hello")

(deftest define-condition.5
  (define-condition define-condition5 () ()
    (:report
      (lambda (condition stream)
        (declare (ignore condition))
        (format stream "ABC~A" 100))))
  define-condition5)

(deftest define-condition.6
  (with-output-to-string (stream)
    (princ (make-condition 'define-condition5) stream))
  "ABC100")

(defun define-condition-test (inst stream)
  (declare (ignore inst))
  (format stream "DEF~A" 200))

(deftest define-condition.7
  (define-condition define-condition7 () ()
    (:report define-condition-test))
  define-condition7)

(deftest define-condition.8
  (with-output-to-string (stream)
    (princ (make-condition 'define-condition7) stream))
  "DEF200")


;;
;;  degrade
;;
(deftest check-type-error.1
  (let ((x 'hello))
    (handler-bind
      ((type-error
         (lambda (c)
           (store-value 999 c))))
      (check-type x integer))
    x)
  999)

(deftest check-type-error.2
  (let ((x '(a b c d)))
    (handler-bind
      ((type-error
         (lambda (c)
           (store-value 999 c))))
      (check-type (car x) integer))
    x)
  (999 b c d))

