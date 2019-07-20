;;
;;  ANSI COMMON LISP: 9. Conditions
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

