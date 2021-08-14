;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  make-instance
;;
(defstruct make-instance-structure aaa bbb)
(deftest make-instance-structure.1
  (let ((inst (make-instance 'make-instance-structure :aaa 100)))
    (values
      (make-instance-structure-aaa inst)
      (make-instance-structure-bbb inst)))
  100 nil)


;;  list
(defstruct (list-make-instance-structure (:type list)) aaa bbb)
(deftest list-make-instance-structure.1
  (let ((inst (make-instance 'list-make-instance-structure :aaa 100)))
    (values
      (list-make-instance-structure-aaa inst)
      (list-make-instance-structure-bbb inst)))
  100 nil)


;;  vector
(defstruct (vector-make-instance-structure (:type vector)) aaa bbb)
(deftest vector-make-instance-structure.1
  (let ((inst (make-instance 'vector-make-instance-structure :aaa 100)))
    (values
      (vector-make-instance-structure-aaa inst)
      (vector-make-instance-structure-bbb inst)))
  100 nil)


;;
;;  slot-value
;;
(deftest slot-value-structure.1
  (let ((inst (make-instance 'make-instance-structure :aaa 100)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  100 nil)


;;  list
(defstruct (list-slot-value (:type list) :named) aaa bbb)
(deftest-error list-slot-value.1
  (slot-value (make-list-slot-value) 'aaa))

(deftest-error list-slot-value.2
  (slot-boundp (make-list-slot-value) 'aaa))

(deftest-error list-slot-value.3
  (slot-exists-p (make-list-slot-value) 'aaa))

(deftest-error list-slot-value.4
  (slot-makunbound (make-list-slot-value) 'aaa))


;;  vector
(defstruct (vector-slot-value (:type vector) :named) aaa bbb)
(deftest-error vector-slot-value.1
  (slot-value (make-vector-slot-value) 'aaa))

(deftest-error vector-slot-value.2
  (slot-boundp (make-vector-slot-value) 'aaa))

(deftest-error vector-slot-value.3
  (slot-exists-p (make-vector-slot-value) 'aaa))

(deftest-error vector-slot-value.4
  (slot-makunbound (make-vector-slot-value) 'aaa))


;;
;;  slot-boundp
;;
(deftest slot-boundp-structure.1
  (let ((inst (make-instance 'make-instance-structure :aaa 100)))
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  t t)


;;
;;  slot-exists-p
;;
(deftest slot-exists-p-structure.1
  (let ((inst (make-instance 'make-instance-structure :aaa 100)))
    (values
      (slot-exists-p inst 'aaa)
      (slot-exists-p inst 'bbb)
      (slot-exists-p inst 'ccc)))
  t t nil)


;;
;;  slot-makunbound
;;
(deftest slot-makunbound-structure.1
  (let ((inst (make-instance 'make-instance-structure :aaa 100)))
    (slot-makunbound inst 'aaa)
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  nil t)


;;
;;  #S()
;;
(deftest dispatch-structure.1
  (let ((inst (read-from-string "#S(make-instance-structure :aaa 100)")))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  100 nil)

