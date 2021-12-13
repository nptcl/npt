;;
;;  ANSI COMMON LISP: 9. Conditions
;;

;;
;;  Function INVALID-METHOD-ERROR
;;
(defgeneric invalid-method-error-test ())
(defmethod invalid-method-error-test () :hello)

(deftest-error invalid-method-error.1
  (invalid-method-error
    (find-method #'invalid-method-error-test nil nil) "Hello: ~A" 10))

(deftest invalid-method-error.2
  (handler-case
    (invalid-method-error
      (find-method #'invalid-method-error-test nil nil) "Hello: ~A" 10)
    (error (c)
      (let ((s (apply #'format nil
                      (simple-condition-format-control c)
                      (simple-condition-format-arguments c))))
        (values
          (null (search "Hello" s))
          (null (search "10" s))))))
  nil nil)

(deftest-error! invalid-method-error-error.1
  (eval '(invalid-method-error
           (find-method #'invalid-method-error-test nil nil))))

(deftest-error! invalid-method-error-error.2
  (eval '(invalid-method-error 10 "AAA"))
  type-error)


;;
;;  Function METHOD-COMBINATION-ERROR
;;
(deftest-error method-combination-error.1
  (method-combination-error "Hello: ~A" 10))

(deftest method-combination-error.2
  (handler-case
    (method-combination-error "Hello: ~A" 10)
    (error (c)
      (let ((s (apply #'format nil
                      (simple-condition-format-control c)
                      (simple-condition-format-arguments c))))
        (values
          (null (search "Hello" s))
          (null (search "10" s))))))
  nil nil)

(deftest-error! method-combination-error-error.1
  (eval '(method-combination-error)))

(deftest-error! method-combination-error-error.2
  (eval '(method-combination-error 10))
  type-error)


;;
;;  System Class RESTART
;;
(deftest restart-type.1
  (lisp-system:closp
    (find-class 'restart))
  t)

(deftest restart-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'restart)))
  (restart t))


;;
;;  Function COMPUTE-RESTARTS
;;
(defun find-list-restart (x list)
  (if (find x list :key #'restart-name)
    t nil))

(defun slot-exists-and-value (inst name)
  (and (slot-exists-p inst name)
       (slot-value inst name)))

(deftest compute-restarts.1
  (listp
    (compute-restarts))
  t)

(deftest compute-restarts.2
  (listp
    (compute-restarts nil))
  t)

(deftest compute-restarts.3
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

(deftest compute-restarts.4
  (progn
    (define-condition compute-restarts-1 ()
      ((aaa :initarg :aaa)))
    (let ((inst (make-condition 'compute-restarts-1 :aaa 10)))
      (restart-case
        (<= 2 (length (compute-restarts inst)))
        (aa () :hello)
        (bb () :test (lambda (x) (eql (slot-exists-and-value x 'aaa) 100)))
        (cc ()))))
  t)

(deftest-error! compute-restarts-error.1
  (eval '(compute-restarts 100))
  type-error)

(deftest-error! compute-restarts-error.2
  (eval '(compute-restarts nil nil)))


;;
;;  Function FIND-RESTART
;;
(deftest find-restart.1
  (find-restart 'no-such-restart)
  nil)

(deftest find-restart.2
  (restart-bind ((restart-1 (constantly nil)))
    (typep
      (find-restart 'restart-1)
      'restart))
  t)

(deftest find-restart.3
  (restart-bind ((restart-1 (constantly nil)))
    (typep
      (find-restart 'restart-1 nil)
      'restart))
  t)

(deftest find-restart.4
  (restart-bind ((restart-1 (constantly nil)))
    (typep
      (find-restart
        (find-restart 'restart-1))
      'restart))
  t)

(deftest find-restart.5
  (let (restart)
    (restart-bind ((restart-1 (constantly nil)))
      (setq restart (find-restart 'restart-1))
      (check-type restart restart))
    (find-restart restart))
  nil)

(deftest-error find-restart.6
  (find-restart nil))

(deftest find-restart.7
  (restart-bind
    ((restart-2
       (constantly nil)
       :test-function
       (lambda (x)
         (typep x 'program-error))))
    (typep
      (find-restart 'restart-2)
      'restart))
  t)

(deftest find-restart.8
  (restart-bind
    ((restart-2
       (constantly nil)
       :test-function
       (lambda (x)
         (typep x 'program-error))))
    (let ((condition (make-condition 'program-error)))
      (typep
        (find-restart 'restart-2 condition)
        'restart)))
  t)

(deftest find-restart.9
  (restart-bind
    ((restart-2
       (constantly nil)
       :test-function
       (lambda (x)
         (typep x 'program-error))))
    (let ((condition (make-condition 'reader-error)))
      (find-restart 'restart-2 condition)))
  nil)

(deftest find-restart.10
  (progn
    (define-condition find-restart-1 ()
      ((aaa :initarg :aaa)))
    (let ((xx (make-condition 'find-restart-1 :aaa 100))
          (yy (make-condition 'find-restart-1 :aaa 999)))
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
          (find-restart 'bb yy)))))
  t t nil)

(deftest-error! find-restart-error.1
  (eval '(find-restart)))

(deftest-error! find-restart-error.2
  (eval '(find-restart 100)))

(deftest-error! find-restart-error.3
  (eval '(find-restart 'no-such-restart nil nil)))

;;  ANSI Common Lisp
(deftest find-restart-test.1
  (restart-case
    (let ((r (find-restart 'my-restart)))
      (format nil "name: ~S" (restart-name r)))
    (my-restart () nil))
  "name: MY-RESTART")

(deftest find-restart-test.2
  (find-restart 'my-restart)
  nil)


;;
;;  Function INVOKE-RESTART
;;
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

(deftest-error invoke-restart.5
  (restart-case
    (invoke-restart nil)))

(deftest invoke-restart.6
  (restart-case
    (invoke-restart
      (find-restart 'aaa))
    (aaa () 100))
  100)

(deftest-error invoke-restart.7
  (let (restart)
    (restart-case
      (setq restart (find-restart 'aaa))
      (aaa () 100))
    (check-type restart restart)
    (invoke-restart restart))
  control-error)

(deftest invoke-restart.8
  (restart-case
    (invoke-restart
      (find-restart 'aaa))
    (aaa () (values 10 20 30)))
  10 20 30)

(deftest-error! invoke-restart-error.1
  (eval '(invoke-restart)))

(deftest-error! invoke-restart-error.2
  (eval '(invoke-restart 100))
  type-error)

;;  ANSI Common Lisp
(defun invoke-restart-add3 (x)
  (check-type x number)
  (+ x 3))

(deftest invoke-restart-test.1
  (handler-bind
    ((error
       (lambda (c)
         (declare (ignore c))
         (invoke-restart 'store-value 7))))
    (invoke-restart-add3 'seven))
  10)


;;
;;  Function INVOKE-RESTART-INTERACTIVELY
;;
(deftest invoke-restart-interactively.1
  (restart-case
    (invoke-restart-interactively 'aa)
    (aa (a b c)
        :interactive (lambda () (list 10 20 30))
        (+ a b c)))
  60)

(deftest invoke-restart-interactively.2
  (restart-case
    (invoke-restart-interactively
      (find-restart 'aa))
    (aa (a b c)
        :interactive (lambda () (list 10 20 30))
        (+ a b c)))
  60)

(deftest-error invoke-restart-interactively.3
  (invoke-restart-interactively nil))

(deftest-error invoke-restart-interactively.4
  (let (restart)
    (restart-case
      (setq restart (find-restart 'aaa))
      (aaa () 100))
    (check-type restart restart)
    (invoke-restart-interactively restart))
  control-error)

(deftest-error! invoke-restart-interactively-error.1
  (eval '(invoke-restart-interactively)))

(deftest-error! invoke-restart-interactively-error.2
  (eval '(invoke-restart-interactively 100))
  type-error)

(deftest-error! invoke-restart-interactively-error.3
  (eval '(invoke-restart-interactively 'aa nil)))



;;
;;
;;
(deftest restart-bind.1
  (restart-bind nil)
  nil)

(deftest restart-bind.2
  (restart-bind
    ((hello (lambda () :aaa)))
    10)
  10)

(deftest restart-bind.3
  (restart-bind
    ((hello (lambda () :aaa)))
    (invoke-restart
      (find-restart 'hello)))
  :aaa)

(deftest restart-bind.4
  (restart-bind
    ((hello (lambda () :aaa)))
    (invoke-restart
      (find-restart 'hello))
    :hello)
  :hello)

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

(deftest handler-bind-continue.1
  (handler-bind ((error #'continue))
    (cerror "Hello" "aaa"))
  nil)

(defun restart-bind-store-value-1 (c)
  (when (typep c 'type-error)
    (let ((r (find-restart 'store-value c)))
      (handler-case
        (invoke-restart r :hello)
        (error ())))))

(deftest handler-bind-store-value.1
  (let ((x 100))
    (handler-bind ((type-error #'restart-bind-store-value-1))
      (check-type x symbol)
      x))
  :hello)

(deftest restart-bind-return.1
  (list 10 20
        (restart-bind ((restart-bind-return (lambda () 30))) 33)
        40 50 60)
  (10 20 33 40 50 60))

(deftest restart-bind-return.2
  (values 10 20
          (restart-bind ((restart-bind-return (lambda () 30))) 33)
          40 50 60)
  10 20 33 40 50 60)

(deftest restart-bind-return.3
  (list 10 20
        (restart-bind ((restart-bind-return (lambda () 30)))
          (invoke-restart 'restart-bind-return) 33)
        40 50 60)
  (10 20 33 40 50 60))

(deftest restart-bind-return.4
  (values 10 20
          (restart-bind ((restart-bind-return (lambda () 30)))
            (invoke-restart 'restart-bind-return) 33)
          40 50 60)
  10 20 33 40 50 60)

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

(define-condition testcond () ((aaa :initarg :aaa)))

(deftest restart-case-return.1
  (values 10 20
          (restart-case 33 (restart-case-return () 30))
          40 50 60)
  10 20 33 40 50 60)

(deftest restart-case-return.2
  (list 10 20
        (restart-case
          (invoke-restart 'restart-case-return)
          (restart-case-return () 30))
        40 50 60)
  (10 20 30 40 50 60))

(deftest restart-case-return.3
  (values 10 20
          (restart-case
            (invoke-restart 'restart-case-return)
            (restart-case-return () 30))
          40 50 60)
  10 20 30 40 50 60)

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

(deftest restart-name.1
  (restart-case
    (restart-name
      (find-restart 'hello))
    (hello ()))
  hello)

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

