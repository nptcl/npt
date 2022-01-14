;;
;;  System Call: memory-stream
;;
(import 'lisp-system:sysctl)

;;  error
(deftest-error! sysctl.1
  (eval '(sysctl)))

(deftest sysctl.2
  (sysctl 'hello)
  nil nil)


;;  clos
(defclass clos-sysctl-1 ()
  (aaa bbb ccc))

(deftest clos-sysctl.1
  (let ((x (make-instance 'clos-sysctl-1)))
    (sysctl x 'slots))
  (aaa bbb ccc) t)

(deftest clos-sysctl.2
  (let ((x (make-instance 'clos-sysctl-1)))
    (sysctl x 'hello))
  nil nil)


;;  recovery
(deftest recovery-sysctl.1
  (sysctl 'recovery 'no-applicable-method)
  t t)

(deftest recovery-sysctl.2
  (let ((x #'no-applicable-method))
    (sysctl 'recovery 'no-applicable-method)
    (eq x #'no-applicable-method))
  nil)

(deftest recovery-sysctl.3
  (sysctl 'recovery 'no-next-method)
  t t)

(deftest recovery-sysctl.4
  (let ((x #'no-next-method))
    (sysctl 'recovery 'no-next-method)
    (eq x #'no-next-method))
  nil)

(deftest recovery-sysctl.5
  (sysctl 'recovery 'hello)
  nil nil)


;;  structure
(deftest structure-check-sysctl.1
  (progn
    (defstruct (structure-check-sysctl-1 (:type list)))
    (sysctl 'structure 'check 'structure-check-sysctl-1))
  t t)

(deftest structure-check-sysctl.2
  (progn
    (defstruct structure-check-sysctl-2)
    (sysctl 'structure 'check 'structure-check-sysctl-2))
  nil t)

(deftest structure-check-sysctl.3
  (sysctl 'structure 'check 'no-such-structure-name)
  nil t)

(deftest structure-delete-sysctl.1
  (sysctl 'structure 'delete 'no-such-structure-name)
  nil t)

(deftest structure-delete-sysctl.2
  (progn
    (defstruct structure-delete-sysctl-2)
    (sysctl 'structure 'delete 'structure-delete-sysctl-2))
  t t)

(deftest structure-delete-sysctl.3
  (progn
    (defstruct structure-delete-sysctl-3)
    (sysctl 'structure 'delete 'structure-delete-sysctl-3)
    (values
      (sysctl 'structure 'check 'structure-delete-sysctl-3)
      (find-class 'structure-delete-sysctl-3 nil)))
  nil nil)

(deftest structure-delete-sysctl.4
  (progn
    (defstruct (structure-delete-sysctl-4 (:type vector)))
    (sysctl 'structure 'delete 'structure-delete-sysctl-4))
  t t)

(deftest structure-delete-sysctl.5
  (progn
    (defstruct (structure-delete-sysctl-5 (:type vector)))
    (sysctl 'structure 'delete 'structure-delete-sysctl-5)
    (values
      (sysctl 'structure 'check 'structure-delete-sysctl-5)
      (find-class 'structure-delete-sysctl-5 nil)))
  nil nil)

(deftest structure-type-sysctl.1
  (sysctl 'structure 'type 'no-such-structure-name)
  nil t)

(deftest structure-type-sysctl.2
  (progn
    (defstruct structure-type-sysctl-2)
    (sysctl 'structure 'type 'structure-type-sysctl-2))
  class t)

(deftest structure-type-sysctl.3
  (progn
    (defstruct (structure-type-sysctl-3 (:type list)))
    (sysctl 'structure 'type 'structure-type-sysctl-3))
  list t)

(deftest structure-type-sysctl.4
  (progn
    (defstruct (structure-type-sysctl-4 (:type (vector character))))
    (sysctl 'structure 'type 'structure-type-sysctl-4))
  (vector character) t)

(deftest structure-sysctl.1
  (sysctl 'structure 'hello)
  nil nil)


;;  random
(deftest random-integer-sysctl.1
  (let ((r (make-random-state t)))
    (multiple-value-bind (x y) (sysctl 'random 'integer r)
      (values (integerp x) y)))
  t t)

(deftest random-integer-sysctl.2
  (let* ((x (make-random-state t))
         (y (make-random-state x)))
    (= (sysctl 'random 'integer x)
       (sysctl 'random 'integer y)))
  t)

(deftest random-integer-sysctl.3
  (let* ((x (make-random-state t))
         (y (make-random-state x)))
    (random 100 x)
    (= (sysctl 'random 'integer x)
       (sysctl 'random 'integer y)))
  nil)

(deftest random-make-sysctl.1
  (multiple-value-bind (x y) (sysctl 'random 'make 10)
    (values
      (sysctl 'random 'integer x)
      y))
  10 t)

(deftest random-make-sysctl.2
  (let ((x (sysctl 'random 'make 10)))
    (lisp-system:fixnump
      (sysctl 'random 'integer x)))
  t)

(deftest random-make-sysctl.3
  (let ((x (sysctl 'random 'make #x0123456789ABCDEFFFFFEEEEDDDDBBBB)))
    (sysctl 'random 'integer x))
  #x0123456789ABCDEFFFFFEEEEDDDDBBBB t)

(deftest random-make-sysctl.4
  (let ((x (sysctl 'random 'make -2)))
    (sysctl 'random 'integer x))
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE t)

(deftest random-write-sysctl.1
  (let ((r (make-random-state)))
    (multiple-value-bind (x y) (sysctl 'random 'write r 10)
      (values
        (sysctl 'random 'integer r)
        (eq x r)
        y)))
  10 t t)

(deftest random-write-sysctl.2
  (let ((r (make-random-state)))
    (sysctl 'random 'write r 10)
    (lisp-system:fixnump
      (sysctl 'random 'integer r)))
  t)

(deftest random-write-sysctl.3
  (let ((r (make-random-state)))
    (sysctl 'random 'write r #x0123456789ABCDEFFFFFEEEEDDDDBBBB)
    (sysctl 'random 'integer r))
  #x0123456789ABCDEFFFFFEEEEDDDDBBBB t)

(deftest random-write-sysctl.4
  (let ((r (make-random-state)))
    (sysctl 'random 'write r -3)
    (sysctl 'random 'integer r))
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD t)

(deftest random-sysctl.1
  (sysctl 'random 'hello)
  nil nil)

