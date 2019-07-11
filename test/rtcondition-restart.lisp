;;
;;  ANSI COMMON LISP: 9. Conditions
;;

;;
;;  restart-bind
;;  restart-case
;;
(deftest restart-bind.1
  (restart-bind
    ((hello (lambda () :aaa)))
    10)
  10)

(deftest restart-bind.2
  (restart-bind
    ((hello (lambda () :aaa)))
    (invoke-restart
      (find-restart 'hello)))
  :aaa)

(deftest restart-bind.3
  (restart-bind
    ((hello (lambda () :aaa)))
    (invoke-restart
      (find-restart 'hello))
    :hello)
  :hello)

(deftest restart-case.1
  (restart-case
    20
    (hello () :aaa))
  20)

(deftest restart-case.2
  (restart-case
    (invoke-restart
      (find-restart 'hello))
    (hello () :aaa))
  :aaa)

(deftest restart-case.3
  (restart-case
    (progn
      (invoke-restart
        (find-restart 'hello))
      100)
    (hello () :aaa))
  :aaa)

(deftest restart-name.1
  (restart-case
    (restart-name
      (find-restart 'hello))
    (hello ()))
  hello)


;;
;;  standard restart
;;
(deftest abort.1
  (restart-case
    (abort)
    (abort ()
      'hello))
  hello)

(deftest abort.2
  (restart-case
    (abort nil)
    (abort ()
      'hello))
  hello)

(deftest abort.3
  (restart-case
    (abort (make-condition 'testcond :aaa 10))
    (abort ()
      :test (lambda (x) (eql (slot-exists-and-value x 'aaa) 10))
      'hello))
  hello)

(deftest continue.1
  (continue)
  nil)

(deftest continue.2
  (continue (make-condition 'testcond :aaa 10))
  nil)

(deftest continue.3
  (continue nil)
  nil)

(deftest continue.4
  (restart-case
    (continue)
    (continue () 'hello))
  hello)

(deftest-error muffle-warning.1
  (muffle-warning)
  control-error)

(deftest-error muffle-warning.2
  (muffle-warning
    (make-condition 'testcond :aaa 10))
  control-error)

(deftest-error muffle-warning.3
  (muffle-warning nil)
  control-error)

(deftest muffle-warning.4
  (restart-case
    (muffle-warning)
    (muffle-warning () 10))
  10)

(deftest muffle-warning.5
  (restart-case
    (muffle-warning
      (make-condition 'testcond :aaa 10))
    (muffle-warning () 10))
  10)

(deftest muffle-warning.6
  (handler-bind ((warning #'muffle-warning))
    (warn "Hello"))
  nil)

(deftest store-value.1
  (store-value 11)
  nil)

(deftest store-value.2
  (store-value 11 (make-condition 'testcond :aaa 10))
  nil)

(deftest store-value.3
  (store-value 11 nil)
  nil)

(deftest store-value.4
  (restart-case
    (store-value 11)
    (store-value (x)
      (1+ x)))
  12)

(deftest use-value.1
  (use-value 11)
  nil)

(deftest use-value.2
  (use-value 11 (make-condition 'testcond :aaa 10))
  nil)

(deftest use-value.3
  (use-value 11 nil)
  nil)

(deftest use-value.4
  (restart-case
    (use-value 11)
    (use-value (x)
      (1+ x)))
  12)


;;
;;  function
;;
(define-condition testcond () ((aaa :initarg :aaa)))

(defun slot-exists-and-value (inst name)
  (and (slot-exists-p inst name)
       (slot-value inst name)))

(deftest find-restart.1
  (let ((xx (make-condition 'testcond :aaa 100))
        (yy (make-condition 'testcond :aaa 999)))
    (restart-bind
      ((aa (lambda () :aa) :test-function
           (lambda (x) (eql (slot-exists-and-value x 'aaa) 10)))
       (bb (lambda () :aa) :test-function
           (lambda (x) (eql (slot-exists-and-value x 'aaa) 100)))
       (cc (lambda () :aa) :test-function
           (lambda (x) (eql (slot-exists-and-value x 'aaa) 1000))))
      (values
        (eq (restart-name (find-restart 'bb)) 'bb)
        (eq (restart-name (find-restart 'bb xx)) 'bb)
        (find-restart 'bb yy))))
  t t nil)

(deftest compute-restarts.1
  (listp
    (compute-restarts))
  t)

(defun find-list-restart (x list)
  (null (null (find x list :key #'restart-name))))

(deftest compute-restarts.2
  (restart-case
    (let ((list (compute-restarts)))
      (values
        (find-list-restart 'aa list)
        (find-list-restart 'bb list)
        (find-list-restart 'cc list)
        (find-list-restart 'dd list)))
    (aa () :hello)
    (bb ())
    (cc ()))
  t t t nil)

(deftest compute-restarts.3
  (let ((inst (make-condition 'testcond :aaa 10)))
    (restart-case
      (<= 2 (length (compute-restarts inst)))
      (aa () :hello)
      (bb () :test (lambda (x) (eql (slot-exists-and-value x 'aaa) 100)))
      (cc ())))
  t)

(deftest invoke-restart.1
  (restart-case
    (invoke-restart 'bbb)
    (aaa () 'aaaa)
    (bbb () 'bbbb)
    (ccc () 'cccc))
  bbbb)

(deftest invoke-restart.2
  (let (value)
    (restart-bind
      ((aaa (lambda () (setq value 'aaaa)))
       (bbb (lambda () (setq value 'bbbb)))
       (ccc (lambda () (setq value 'cccc))))
      (invoke-restart 'bbb))
    value)
  bbbb)

(deftest invoke-restart.3
  (restart-case
    (invoke-restart 'aaa 10 20)
    (aaa (a b) (+ a b)))
  30)

(deftest invoke-restart.4
  (restart-case
    (restart-case
      (invoke-restart 'aaa 10 20)
      (aaa (a b) (+ a b)))
    (aaa (a b) (* a b)))
  30)

(deftest restart-return.1
  (values 10 20
          (restart-case 33 (restart-return () 30))
          40 50 60)
  10 20 33 40 50 60)

(deftest restart-return.2
  (list 10 20
        (restart-case
          (invoke-restart 'restart-return)
          (restart-return () 30))
        40 50 60)
  (10 20 30 40 50 60))

(deftest restart-return.3
  (values 10 20
          (restart-case
            (invoke-restart 'restart-return)
            (restart-return () 30))
          40 50 60)
  10 20 30 40 50 60)

(deftest restart-return.4
  (list 10 20
        (restart-bind ((restart-return (lambda () 30))) 33)
        40 50 60)
  (10 20 33 40 50 60))

(deftest restart-return.5
  (values 10 20
          (restart-bind ((restart-return (lambda () 30))) 33)
          40 50 60)
  10 20 33 40 50 60)

(deftest restart-return.6
  (list 10 20
        (restart-bind ((restart-return (lambda () 30)))
          (invoke-restart 'restart-return) 33)
        40 50 60)
  (10 20 33 40 50 60))

(deftest restart-return.7
  (values 10 20
          (restart-bind ((restart-return (lambda () 30)))
            (invoke-restart 'restart-return) 33)
          40 50 60)
  10 20 33 40 50 60)


;;
;;  restart-case arguments
;;
(deftest restart-case-interactive.1
  (restart-case
    (invoke-restart-interactively 'aa)
    (aa (a b c) :interactive (lambda () (list 10 20 30))
        (+ a b c)))
  60)

(defun restart-case-interactive-call ()
  (list 20 30 40))

(deftest restart-case-interactive.2
  (restart-case
    (invoke-restart-interactively 'aa)
    (aa (a b c) :interactive restart-case-interactive-call
        (* a b c)))
  24000)

(deftest restart-case-report.1
  (restart-case
    (format nil "~A" (find-restart 'aa))
    (aa () :report (lambda (s) (princ "abc" s))
        'hello))
  "abc")

(defun restart-case-report-call (s)
  (princ "def" s))

(deftest restart-case-report.2
  (restart-case
    (format nil "~A" (find-restart 'aa))
    (aa () :report restart-case-report-call
        'hello))
  "def")

(deftest restart-case-report.3
  (restart-case
    (format nil "~A" (find-restart 'aa))
    (aa () :report "ghi"
        'hello))
  "ghi")

(deftest restart-case-test.1
  (restart-case
    (restart-name
      (find-restart 'aa))
    (aa () :test (lambda (x)
                   (declare (ignore x))
                   t)))
  aa)

(defun restart-case-test-call (x)
  (declare (ignore x))
  t)

(deftest restart-case-test.2
  (restart-case
    (restart-name
      (find-restart 'aa))
    (aa () :test restart-case-test-call))
  aa)


;;
;;  restart-bind arguments
;;
(deftest restart-bind-interactive.1
  (restart-bind
    ((aa (lambda (a b c) (+ a b c))
         :interactive-function (lambda () (list 10 20 30))))
    (invoke-restart-interactively 'aa))
  60)

(deftest restart-bind-report.1
  (restart-bind
    ((aa (lambda () 'hello)
         :report-function (lambda (s) (princ "abc" s))))
    (format nil "~A" (find-restart 'aa)))
  "abc")

(deftest restart-bind-test.1
  (restart-bind
    ((aa (lambda () 'hello)
         :test-function (lambda (x)
                          (declare (ignore x))
                          t)))
    (restart-name
      (find-restart 'aa)))
  aa)


;;
;;  function
;;
(deftest invoke-restart-interactively.1
  (restart-case
    (invoke-restart-interactively 'aa)
    (aa (a b c)
        :interactive (lambda () (list 10 20 30))
        (list a b c)))
  (10 20 30))

(deftest handler-bind-continue.1
  (handler-bind ((error #'continue))
    (cerror "Hello" "aaa"))
  nil)

(defun type-error-hello (c)
  (when (typep c 'type-error)
    (let ((r (find-restart 'store-value c)))
      (handler-case
        (invoke-restart r :hello)
        (error ())))))

(deftest handler-bind-store-value.1
  (let ((x 100))
    (handler-bind ((type-error #'type-error-hello))
      (check-type x symbol)
      x))
  :hello)

(defgeneric invalid-method-error-test ())
(defmethod invalid-method-error-test () :hello)

(deftest invalid-method-error.1
  (handler-case
    (progn
      (invalid-method-error
        (find-method #'invalid-method-error-test nil nil) "Hello: ~A" 10)
      :hello)
    (error (c)
      (let ((s (apply #'format nil
                      (simple-condition-format-control c)
                      (simple-condition-format-arguments c))))
        (values
          (null (search "Hello" s))
          (null (search "10" s))))))
  nil nil)

(deftest method-combination-error.1
  (handler-case
    (progn
      (method-combination-error "Hello: ~A" 10)
      :hello)
    (error (c)
      (let ((s (apply #'format nil
                      (simple-condition-format-control c)
                      (simple-condition-format-arguments c))))
        (values
          (null (search "Hello" s))
          (null (search "10" s))))))
  nil nil)

