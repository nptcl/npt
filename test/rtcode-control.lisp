;;
;;  rt-code
;;
(declaim (optimize (speed 0) (safety 3)))

(deftest rtcode-control-default
  (progn
    (proclaim '(optimize (speed 0) (safety 3)))
    (values)))

(deftest code-optimize-check.1
  (lisp-system::optimize-check type)
  0)

(deftest let-set.1
  (let () 10 20 30)
  30)

(deftest let-push.1
  (values (let () 10 20 30))
  30)

(deftest let*-set.1
  (let* () 10 20 30)
  30)

(deftest let*-push.1
  (values (let* () 10 20 30))
  30)

(deftest destructuring-bind-set.1
  (destructuring-bind (a b c) '(10 20 30)
    (+ a b c))
  60)

(deftest destructuring-bind-push.1
  (values
    (destructuring-bind (a b c) '(10 20 30)
      (+ a b c)))
  60)

(deftest flet-set.1
  (flet () 10 20 30)
  30)

(deftest flet-push.1
  (values (flet () 10 20 30))
  30)

(deftest labels-set.1
  (labels () 10 20 30)
  30)

(deftest labels-push.1
  (values (labels () 10 20 30))
  30)

(deftest call-set.1
  (car '(10 . 20))
  10)

(deftest call-push.1
  (values (car '(10 . 20)))
  10)

(deftest values-set.1
  (values 10 20 30)
  10 20 30)

(deftest values-push.1
  (values (values 10 20 30)
          (values 40 50 60))
  10 40)

(deftest unwind-protect-set.1
  (unwind-protect 10 20 30)
  10)

(deftest unwind-protect-push.1
  (values (unwind-protect 10 20 30))
  10)

(deftest tagbody-set.1
  (tagbody)
  nil)

(deftest tagbody-set.2
  (tagbody (+ 10 20 30))
  nil)

(deftest tagbody-push.1
  (values (tagbody))
  nil)

(deftest tagbody-push.2
  (values (tagbody (+ 10 20 30)))
  nil)

(deftest block-set.1
  (block aaa)
  nil)

(deftest block-set.2
  (block aaa 10 20 30)
  30)

(deftest block-set.3
  (block aaa 10 (return-from aaa 40) 30)
  40)

(deftest block-push.1
  (values
    (block aaa))
  nil)

(deftest block-push.2
  (values
    (block aaa 10 20 30))
  30)

(deftest block-push.3
  (values
    (block aaa 10 (return-from aaa 40) 30))
  40)

(deftest catch-set.1
  (catch 'aaa)
  nil)

(deftest catch-set.2
  (catch 'aaa 10 20 30)
  30)

(deftest catch-set.3
  (catch 'aaa 10 20 (throw 'aaa 30) 40)
  30)

(deftest catch-push.1
  (values
    (catch 'aaa))
  nil)

(deftest catch-push.2
  (values
    (catch 'aaa 10 20 30))
  30)

(deftest catch-push.3
  (values
    (catch 'aaa 10 20 30)
    (catch 'aaa 40 50 60))
  30 60)

(deftest catch-push.4
  (car
    (catch 'aaa))
  nil)

(deftest catch-push.5
  (values
    (catch 'aaa 10 20 (throw 'aaa 30) 40))
  30)

(deftest multiple-value-bind-set.1
  (multiple-value-bind (a b c) (values 10 20 30)
    (+ a b c))
  60)

(deftest multiple-value-bind-push.1
  (values
    (multiple-value-bind (a b c) (values 10 20 30)
      (+ a b c)))
  60)

(deftest multiple-value-call-set.1
  (multiple-value-call #'+ (values 10 20 30))
  60)

(deftest multiple-value-call-push.1
  (values
    (multiple-value-call #'+ (values 10 20 30)))
  60)

(deftest multiple-value-prog1-set.1
  (multiple-value-prog1 (values 10 20 30)
    40 50 60)
  10 20 30)

(deftest multiple-value-prog1-push.1
  (values
    (multiple-value-prog1 (values 10 20 30)
      40 50 60))
  10)

(deftest nth-value-set.1
  (nth-value 1 (values 10 20 30 40))
  20)

(deftest nth-value-push.1
  (values
    (nth-value 1 (values 10 20 30 40)))
  20)

(deftest progv-set.1
  (progv () 10 20 30)
  30)

(deftest progv-push.1
  (values
    (progv () 10 20 30))
  30)

(deftest handler-bind-set.1
  (handler-bind ()
    10 20 30)
  30)

(deftest handler-bind-set.2
  (handler-bind ((error (lambda (c) c)))
    10 20 30)
  30)

(deftest handler-bind-push.1
  (values
    (handler-bind ()
      10 20 30))
  30)

(deftest handler-bind-push.2
  (values
    (handler-bind ((error (lambda (c) c)))
      10 20 30))
  30)

(deftest handler-case-set.1
  (handler-case 10)
  10)

(deftest handler-case-set.2
  (handler-case 10 (error () 'hello))
  10)

(deftest handler-case-push.1
  (values
    (handler-case 10))
  10)

(deftest handler-case-push.2
  (values
    (handler-case 10 (error () 'hello)))
  10)

(deftest restart-bind-set.1
  (restart-bind ()
    10 20 30)
  30)

(deftest restart-bind-set.2
  (restart-bind ((abort (lambda () 'abort)))
    10 20 30)
  30)

(deftest restart-bind-push.1
  (values
    (restart-bind ()
      10 20 30))
  30)

(deftest restart-bind-push.2
  (values
    (restart-bind ((abort (lambda () 'abort)))
      10 20 30))
  30)

(deftest restart-case-set.1
  (restart-case 10)
  10)

(deftest restart-case-set.2
  (restart-case 10 (abort () 'abort))
  10)

(deftest restart-case-push.1
  (values
    (restart-case 10))
  10)

(deftest restart-case-push.2
  (values
    (restart-case 10 (abort () 'abort)))
  10)

