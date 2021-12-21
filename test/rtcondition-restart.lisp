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
  nil)

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
;;  Macro RESTART-BIND
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
    (invoke-restart 'hello))
  :aaa)

(deftest restart-bind.4
  (restart-bind
    ((hello (lambda () :aaa)))
    (invoke-restart 'hello)
    :hello)
  :hello)

(deftest restart-bind.5
  (restart-bind
    ((nil (lambda () 'aaa)))
    (if (find nil (compute-restarts) :key #'restart-name)
      t nil))
  t)

(deftest-error restart-bind.6
  (let (restart)
    (restart-bind
      ((hello (lambda () 'aaa)))
      (setq restart (find-restart 'hello))
      (check-type restart restart))
    (invoke-restart restart))
  control-error)

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

(deftest restart-bind-report.2
  (restart-bind
    ((aa (lambda () 'hello)
         :report-function (lambda (s) (prin1 "abc" s))))
    (equal (format nil "~A" (find-restart 'aa))
           "abc"))
  nil)

(deftest restart-bind-test-function.1
  (restart-bind
    ((aa (lambda () 'hello)
         :test-function (lambda (x)
                          (declare (ignore x))
                          t)))
    (restart-name
      (find-restart 'aa (make-condition 'error))))
  aa)

(deftest restart-bind-test-function.2
  (restart-bind
    ((aa (lambda () 'hello)
         :test-function (lambda (x)
                          (declare (ignore x))
                          nil)))
    (find-restart 'aa (make-condition 'error)))
  nil)

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

(deftest-error restart-bind-error.1
  (eval '(restart-bind)))

(deftest-error restart-bind-error.2
  (eval '(restart-bind 100)))

(deftest-error restart-bind-error.3
  (eval '(restart-bind ((100 () 200)) 300)))


;;
;;  Macro RESTART-CASE
;;
(deftest restart-case.1
  (restart-case nil)
  nil)

(deftest restart-case.2
  (restart-case
    20
    (hello () :aaa))
  20)

(deftest restart-case.3
  (restart-case
    20
    (nil () :aaa))
  20)

(deftest restart-case.4
  (restart-case
    (invoke-restart 'hello)
    (hello () :aaa))
  :aaa)

(deftest restart-case.5
  (restart-case
    (progn
      (invoke-restart 'hello)
      100)
    (hello () :aaa))
  :aaa)

(deftest restart-case.6
  (restart-case
    (if (find nil (compute-restarts) :key #'restart-name)
      t nil)
    (nil () 'aaa))
  t)

(deftest-error restart-case.7
  (let (restart)
    (restart-case
      (progn
        (setq restart (find-restart 'hello))
        (check-type restart restart))
      (hello () 'aaa))
    (invoke-restart restart))
  control-error)

(deftest restart-case.8
  (restart-case
    (values 10 20 30)
    (hello () :aaa))
  10 20 30)

(deftest restart-case.9
  (restart-case
    (invoke-restart 'hello)
    (hello () (values 40 50 60)))
  40 50 60)

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

(deftest restart-case-interactive.3
  (flet ((call () (list 20 30 40)))
    (restart-case
      (invoke-restart-interactively 'aa)
      (aa (a b c) :interactive call
          (* a b c))))
  24000)

(deftest restart-case-report.1
  (restart-case
    (format nil "~A" (find-restart 'aa))
    (aa () :report (lambda (s) (princ "abc" s))
        'hello))
  "abc")

(deftest restart-case-report.2
  (equal
    (restart-case
      (format nil "~S" (find-restart 'aa))
      (aa () :report (lambda (s) (princ "abc" s))
          'hello))
    "abc")
  nil)

(defun restart-case-report-call (s)
  (princ "def" s))

(deftest restart-case-report.3
  (restart-case
    (format nil "~A" (find-restart 'aa))
    (aa () :report restart-case-report-call
        'hello))
  "def")

(deftest restart-case-report.4
  (equal
    (restart-case
      (format nil "~S" (find-restart 'aa))
      (aa () :report restart-case-report-call
          'hello))
    "def")
  nil)

(deftest restart-case-report.5
  (restart-case
    (format nil "~A" (find-restart 'aa))
    (aa () :report "ghi"
        'hello))
  "ghi")

(deftest restart-case-report.6
  (equal
    (restart-case
      (format nil "~S" (find-restart 'aa))
      (aa () :report "ghi"
          'hello))
    "ghi")
  nil)

(deftest restart-case-report.7
  (labels ((call (s) (princ "Hello" s)))
    (restart-case
      (format nil "~A" (find-restart 'aa))
      (aa () :report call
          'hello)))
  "Hello")

(deftest restart-case-colon-test.1
  (restart-case
    (restart-name
      (find-restart 'aa))
    (aa () :test (lambda (x)
                   (declare (ignore x))
                   t)))
  aa)

(defun restart-case-colon-test-1 (x)
  (declare (ignore x))
  t)

(deftest restart-case-colon-test.2
  (restart-case
    (restart-name
      (find-restart 'aa))
    (aa () :test restart-case-colon-test-1))
  aa)

(deftest restart-case-colon-test.3
  (flet ((call (x) (declare (ignore x)) t))
    (restart-case
      (restart-name
        (find-restart 'aa))
      (aa () :test call)))
  aa)

(defun restart-case-colon-test-2 (x)
  (typep x 'program-error))

(deftest restart-case-colon-test.4
  (let ((c (make-condition 'program-error)))
    (restart-case
      (restart-name
        (find-restart 'aa c))
      (aa () :test restart-case-colon-test-2)))
  aa)

(deftest restart-case-colon-test.5
  (let ((c (make-condition 'reader-error)))
    (restart-case
      (find-restart 'aa c)
      (aa () :test restart-case-colon-test-2)))
  nil)

(deftest restart-case-declare.1
  (flet ((call (x) (declare (ignore x)) t))
    (restart-case
      (restart-name
        (find-restart 'aa))
      (aa () :test call
          (declare (special *no-such-variables*)))))
  aa)

(deftest restart-case-declare.2
  (flet ((call () (list 20 30 40)))
    (restart-case
      (invoke-restart-interactively 'aa)
      (aa (a b c) :interactive call
          (declare (type integer a b c))
          (* a b c))))
  24000)

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

(deftest restart-case-restart.1
  (handler-bind ((error
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'aaa))))
    (restart-case
      (error "Hello")
      (aaa () 100)))
  100)

(deftest restart-case-restart.2
  (let ((foo (make-condition 'simple-condition)))
    (restart-case
      (with-condition-restarts
        foo
        (list (find-restart 'alpha))
        (restart-case
          (invoke-restart 'alpha)
          (alpha () 2)))
      (alpha () 1)))
  2)

(deftest restart-case-restart.3
  (let ((foo (make-condition 'simple-condition)))
    (restart-case
      (with-condition-restarts
        foo
        (list (find-restart 'alpha))
        (restart-case
          (invoke-restart (find-restart 'alpha foo))
          (alpha () 2)))
      (alpha () 1)))
  2)

(deftest restart-case-restart.4
  (let ((foo (make-condition 'simple-condition)))
    (restart-case
      (with-condition-restarts
        foo
        (list (find-restart 'alpha))
        (restart-case
          (invoke-restart 'alpha)
          (alpha () :test (lambda (c) (declare (ignore c)) nil)
                 2)))
      (alpha () 1)))
  1)

(deftest restart-case-condition.1
  (block nil
    (handler-bind
      ((simple-condition
         (lambda (c)
           (return (values (simple-condition-format-control c)
                           (simple-condition-format-arguments c))))))
      (restart-case
        (signal "Hello" 10 20 30)
        (aaa () 'aaa))))
  "Hello" (10 20 30))

(deftest restart-case-condition.2
  (block nil
    (handler-bind
      ((simple-error
         (lambda (c)
           (return (values (simple-condition-format-control c)
                           (simple-condition-format-arguments c))))))
      (restart-case
        (error "Hello" 10 20 30)
        (aaa () 'aaa))))
  "Hello" (10 20 30))

(deftest restart-case-condition.3
  (block nil
    (handler-bind
      ((simple-error
         (lambda (c)
           (return (values (simple-condition-format-control c)
                           (simple-condition-format-arguments c))))))
      (restart-case
        (cerror "ABC" "Hello" 10 20 30)
        (aaa () 'aaa))))
  "Hello" (10 20 30))

(deftest restart-case-condition.4
  (block nil
    (handler-bind
      ((simple-warning
         (lambda (c)
           (return (values (simple-condition-format-control c)
                           (simple-condition-format-arguments c))))))
      (restart-case
        (warn "Hello" 10 20 30)
        (aaa () 'aaa))))
  "Hello" (10 20 30))

(deftest restart-case-condition.5
  (block nil
    (handler-bind
      ((program-error
         (lambda (c)
           (declare (ignore c))
           (return (restart-name (find-restart 'aaa))))))
      (restart-case
        (signal 'program-error)
        (aaa () 'aaa))))
  aaa)

(deftest restart-case-condition.6
  (block nil
    (handler-bind
      ((program-error
         (lambda (c)
           (declare (ignore c))
           (return (restart-name (find-restart 'aaa))))))
      (restart-case
        (signal (make-condition 'program-error))
        (aaa () 'aaa))))
  aaa)

(deftest restart-case-condition.7
  (block nil
    (handler-bind
      ((program-error
         (lambda (c)
           (declare (ignore c))
           (return (restart-name (find-restart 'aaa))))))
      (restart-case
        (cerror "AAA" 'program-error)
        (aaa () 'aaa))))
  aaa)

(deftest-error restart-case-error.1
  (eval '(restart-case)))

(deftest-error restart-case-error.2
  (eval '(restart-case 10 (20 () 30))))

;;  ANSI Common Lisp
(deftest restart-case-test.1
  (restart-case
    (handler-bind ((error #'(lambda (c)
                              (declare (ignore c))
                              (invoke-restart 'my-restart 7))))
      (error "Foo."))
    (my-restart (&optional v) v))
  7)

(define-condition restart-case-food-error (error) ())

(define-condition restart-case-bad-tasting-sundae (restart-case-food-error)
  ((ice-cream :initarg :ice-cream :reader bad-tasting-sundae-ice-cream)
   (sauce :initarg :sauce :reader bad-tasting-sundae-sauce)
   (topping :initarg :topping :reader bad-tasting-sundae-topping))
  (:report (lambda (condition stream)
             (format stream "Bad tasting sundae with ~S, ~S, and ~S"
                     (bad-tasting-sundae-ice-cream condition)
                     (bad-tasting-sundae-sauce condition)
                     (bad-tasting-sundae-topping condition)))))

(defun restart-case-all-start-with-same-letter (symbol1 symbol2 symbol3)
  (let ((first-letter (char (symbol-name symbol1) 0)))
    (and (eql first-letter (char (symbol-name symbol2) 0))
         (eql first-letter (char (symbol-name symbol3) 0)))))

(defun restart-case-read-new-value ()
  ;(format t "Enter a new value: ")
  ;(multiple-value-list (eval (read)))
  (list 'chocolate))

(defun restart-case-verify-or-fix-perfect-sundae (ice-cream sauce topping)
  (do ()
    ((restart-case-all-start-with-same-letter ice-cream sauce topping))
    (restart-case
      (error 'restart-case-bad-tasting-sundae
        :ice-cream ice-cream
        :sauce sauce
        :topping topping)
      (use-new-ice-cream (new-ice-cream)
                         :report "Use a new ice cream."
                         :interactive restart-case-read-new-value
                         (setq ice-cream new-ice-cream))
      (use-new-sauce (new-sauce)
                     :report "Use a new sauce."
                     :interactive restart-case-read-new-value
                     (setq sauce new-sauce))
      (use-new-topping (new-topping)
                       :report "Use a new topping."
                       :interactive restart-case-read-new-value
                       (setq topping new-topping))))
  (values ice-cream sauce topping))

(deftest restart-case-test.2
  (handler-bind ((error
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart-interactively 'use-new-ice-cream))))
    (restart-case-verify-or-fix-perfect-sundae 'vanilla 'caramel 'cherry))
  chocolate caramel cherry)


;;
;;  Function RESTART-NAME
;;
(deftest restart-name.1
  (restart-case
    (restart-name
      (find-restart 'hello))
    (hello ()))
  hello)

(deftest-error! restart-name-error.1
  (eval '(restart-name)))

(deftest-error! restart-name-error.2
  (eval '(restart-name 'hello))
  type-error)

(deftest-error! restart-name-error.3
  (eval '(restart-case
           (restart-name
             (find-restart 'hello)
             nil)
           (hello ()))))

;;  ANSI Common Lisp
(deftest restart-name-test.1
  (let ((list
          (restart-case
            (loop for restart in (compute-restarts)
                  collect (restart-name restart))
            (case1 () :report "Return 1." 1)
            (nil   () :report "Return 2." 2)
            (case3 () :report "Return 3." 3)
            (case1 () :report "Return 4." 4))))
    (values
      (and (member 'case1 list) t)
      (and (member 'nil list) t)
      (and (member 'case3 list) t)))
  t t t)


;;
;;  Macro WITH-CONDITION-RESTARTS
;;
(deftest with-condition-restarts.1
  (with-condition-restarts nil nil)
  nil)

(deftest with-condition-restarts.2
  (with-condition-restarts
    nil nil
    10)
  10)

(deftest with-condition-restarts.3
  (with-condition-restarts
    nil nil
    (values 10 20 30))
  10 20 30)

(deftest with-condition-restarts.4
  (restart-case
    (with-condition-restarts
      'simple-condition
      (list (find-restart 'aaa)
            (find-restart 'bbb)
            (find-restart 'ccc))
      (values
        (restart-name (find-restart 'aaa))
        (restart-name (find-restart 'bbb))
        (restart-name (find-restart 'ccc))))
    (aaa () 'aaa)
    (bbb () 'bbb)
    (ccc () 'ccc))
  aaa bbb ccc)

(deftest with-condition-restarts.5
  (restart-case
    (with-condition-restarts
      (make-condition 'simple-condition)
      (list (find-restart 'aaa)
            (find-restart 'bbb)
            (find-restart 'ccc))
      (values
        (restart-name (find-restart 'aaa))
        (restart-name (find-restart 'bbb))
        (restart-name (find-restart 'ccc))))
    (aaa () 'aaa)
    (bbb () 'bbb)
    (ccc () 'ccc))
  aaa bbb ccc)

(deftest with-condition-restarts-check.1
  (let ((x (make-instance 'simple-error :format-control "aaa")))
    (restart-case
      (with-condition-restarts
        x
        (list (find-restart 'aaa)
              (find-restart 'bbb)
              (find-restart 'ccc))
        (values
          (restart-name (find-restart 'aaa x))
          (restart-name (find-restart 'bbb x))
          (restart-name (find-restart 'ccc x))))
      (aaa () 'aaa)
      (bbb () 'bbb)
      (ccc () 'ccc)))
  aaa bbb ccc)

(deftest with-condition-restarts-check.2
  (let ((x (make-instance 'program-error))
        (y (make-instance 'program-error)))
    (restart-case
      (with-condition-restarts
        x
        (list (find-restart 'aaa)
              (find-restart 'bbb)
              (find-restart 'ccc))
        (values
          (restart-name (find-restart 'bbb nil))
          (restart-name (find-restart 'bbb x))
          (find-restart 'bbb y)))
      (aaa () 'aaa)
      (bbb () 'bbb)
      (ccc () 'ccc)))
  bbb bbb nil)

(deftest with-condition-restarts-check.3
  (let ((x (make-instance 'program-error))
        (y (make-instance 'program-error)))
    (restart-case
      (values
        (restart-name (find-restart 'bbb nil))
        (restart-name (find-restart 'bbb x))
        (restart-name (find-restart 'bbb y)))
      (aaa () 'aaa)
      (bbb () 'bbb)
      (ccc () 'ccc)))
  bbb bbb bbb)

(deftest-error! with-condition-restarts-error.1
  (eval '(with-condition-restarts)))



;;
;;
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

(define-condition testcond () ((aaa :initarg :aaa)))

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

