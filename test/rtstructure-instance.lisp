;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  instance
;;

;;  clos
(defstruct clos-structure-1)
(defstruct (clos-structure-2 (:include clos-structure-1)))
(defstruct (clos-structure-3 (:include clos-structure-2)))

(deftest clos-structure.1
  (null (find-class 'clos-structure-1))
  nil)

(deftest clos-structure.2
  (typep (find-class 'clos-structure-1) 'structure-class)
  t)

(deftest clos-structure.3
  (typep (find-class 'clos-structure-1) 'structure-object)
  nil)

(deftest clos-structure.4
  (typep (find-class 'clos-structure-1) t)
  t)

(deftest clos-structure.5
  (typep (find-class 'clos-structure-1) 'clos-structure-1)
  nil)

(deftest clos-structure.6
  (typep (find-class 'clos-structure-1) 'clos-structure-2)
  nil)

(deftest clos-structure.7
  (typep (find-class 'clos-structure-2) 'clos-structure-1)
  nil)

(deftest clos-structure.8
  (subtypep 'clos-structure-1 'clos-structure-1)
  t t)

(deftest clos-structure.9
  (subtypep 'clos-structure-2 'clos-structure-1)
  t t)

(deftest clos-structure.10
  (subtypep 'clos-structure-1 'clos-structure-2)
  nil t)

(deftest clos-structure.11
  (subtypep 'clos-structure-3 'clos-structure-1)
  t t)

(deftest clos-structure.12
  (subtypep 'clos-structure-1 'clos-structure-3)
  nil t)

(deftest clos-structure.13
  (subtypep 'clos-structure-1 'structure-class)
  nil t)

(deftest clos-structure.14
  (subtypep 'clos-structure-1 'structure-object)
  t t)

(deftest clos-structure.15
  (subtypep 'clos-structure-1 t)
  t t)

(defvar clos-instance-1 (make-clos-structure-1))
(defvar clos-instance-2 (make-clos-structure-2))
(defvar clos-instance-3 (make-clos-structure-3))

(deftest clos-instance.1
  (typep clos-instance-1 'structure-object)
  t)

(deftest clos-instance.2
  (typep clos-instance-1 'standard-object)
  nil)

(deftest clos-instance.3
  (typep clos-instance-1 t)
  t)

(deftest clos-instance.4
  (typep clos-instance-1 'clos-structure-1)
  t)

(deftest clos-instance.5
  (typep clos-instance-1 'clos-structure-2)
  nil)

(deftest clos-instance.6
  (typep clos-instance-2 'structure-object)
  t)

(deftest clos-instance.7
  (typep clos-instance-2 't)
  t)

(deftest clos-instance.8
  (typep clos-instance-2 'clos-structure-1)
  t)

(deftest clos-instance.9
  (typep clos-instance-2 'clos-structure-2)
  t)

(deftest clos-instance.10
  (typep clos-instance-2 'clos-structure-3)
  nil)

(deftest clos-instance.11
  (typep clos-instance-3 'clos-structure-3)
  t)

(deftest clos-instance.12
  (typep clos-instance-3 'clos-structure-1)
  t)

(deftest clos-instance.13
  (typep clos-instance-1 'clos-structure-3)
  nil)

(deftest clos-instance.14
  (< 3 (length (lisp-clos::class-precedence-list
                 (find-class 'clos-structure-3))))
  t)

(deftest clos-instance.15
  (lisp-clos::class-name
    (find-class 'clos-structure-1))
  clos-structure-1)


;;  list
(defstruct (list-structure-1 (:type list)))
(defstruct (list-structure-2 (:type list) (:include list-structure-1)))
(defstruct (list-structure-3 (:type list) (:include list-structure-2) :named))

(deftest list-structure.1
  (null (find-class 'list-structure-1))
  nil)

(deftest list-structure.2
  (typep (find-class 'list-structure-1) 'structure-class)
  t)

(deftest list-structure.3
  (typep (find-class 'list-structure-1) 'structure-object)
  nil)

(deftest list-structure.4
  (typep (find-class 'list-structure-1) t)
  t)

(deftest list-structure.5
  (typep (find-class 'list-structure-1) 'list-structure-1)
  nil)

(deftest list-structure.6
  (typep (find-class 'list-structure-1) 'list-structure-2)
  nil)

(deftest list-structure.7
  (typep (find-class 'list-structure-2) 'list-structure-1)
  nil)

(deftest list-structure.8
  (subtypep 'list-structure-1 'list-structure-1)
  t t)

(deftest list-structure.9
  (subtypep 'list-structure-2 'list-structure-1)
  t t)

(deftest list-structure.10
  (subtypep 'list-structure-1 'list-structure-2)
  nil t)

(deftest list-structure.11
  (subtypep 'list-structure-3 'list-structure-1)
  t t)

(deftest list-structure.12
  (subtypep 'list-structure-1 'list-structure-3)
  nil t)

(deftest list-structure.13
  (subtypep 'list-structure-1 'structure-class)
  nil t)

(deftest list-structure.14
  (subtypep 'list-structure-1 'structure-object)
  t t)

(deftest list-structure.15
  (subtypep 'list-structure-1 t)
  t t)

(deftest list-instance.1
  (typep (make-list-structure-1) 'list)
  t)


;;  vector
(defstruct (vector-structure-1 (:type vector)))
(defstruct (vector-structure-2 (:type vector) (:include vector-structure-1)))
(defstruct (vector-structure-3 (:type vector) (:include vector-structure-2) :named))

(deftest vector-structure.1
  (null (find-class 'vector-structure-1))
  nil)

(deftest vector-structure.2
  (typep (find-class 'vector-structure-1) 'structure-class)
  t)

(deftest vector-structure.3
  (typep (find-class 'vector-structure-1) 'structure-object)
  nil)

(deftest vector-structure.4
  (typep (find-class 'vector-structure-1) t)
  t)

(deftest vector-structure.5
  (typep (find-class 'vector-structure-1) 'vector-structure-1)
  nil)

(deftest vector-structure.6
  (typep (find-class 'vector-structure-1) 'vector-structure-2)
  nil)

(deftest vector-structure.7
  (typep (find-class 'vector-structure-2) 'vector-structure-1)
  nil)

(deftest vector-structure.8
  (subtypep 'vector-structure-1 'vector-structure-1)
  t t)

(deftest vector-structure.9
  (subtypep 'vector-structure-2 'vector-structure-1)
  t t)

(deftest vector-structure.10
  (subtypep 'vector-structure-1 'vector-structure-2)
  nil t)

(deftest vector-structure.11
  (subtypep 'vector-structure-3 'vector-structure-1)
  t t)

(deftest vector-structure.12
  (subtypep 'vector-structure-1 'vector-structure-3)
  nil t)

(deftest vector-structure.13
  (subtypep 'vector-structure-1 'structure-class)
  nil t)

(deftest vector-structure.14
  (subtypep 'vector-structure-1 'structure-object)
  t t)

(deftest vector-structure.15
  (subtypep 'vector-structure-1 t)
  t t)

(deftest vector-instance.1
  (typep (make-vector-structure-1) 'vector)
  t)


;;  change
(deftest change-clos-structure.1
  (progn
    (defstruct change-clos-structure-1)
    (defstruct change-clos-structure-1)
    (null (find-class 'change-clos-structure-1)))
  nil)

(deftest change-clos-structure.2
  (progn
    (defstruct change-clos-structure-2)
    (defstruct change-clos-structure-2)
    (typep (find-class 'change-clos-structure-2) 'structure-class))
  t)

(deftest change-clos-structure.3
  (let (x y)
    (defstruct change-clos-structure-3)
    (setq x (find-class 'change-clos-structure-3))
    (defstruct change-clos-structure-3)
    (setq y (find-class 'change-clos-structure-3))
    (eq x y))
  t)

(deftest change-clos-structure.4
  (let (inst x y z)
    (defstruct change-clos-structure-4)
    (setq inst (make-change-clos-structure-4))
    (setq x (typep inst 'change-clos-structure-4))
    (defstruct change-clos-structure-4)
    (setq y (typep inst 'change-clos-structure-4))
    (setq inst (make-change-clos-structure-4))
    (setq z (typep inst 'change-clos-structure-4))
    (values x y z))
  t t t)

(deftest change-list-structure.1
  (progn
    (defstruct (change-list-structure-1 (:type list)))
    (defstruct (change-list-structure-1 (:type list)))
    (null (find-class 'change-list-structure-1)))
  nil)

(deftest change-list-structure.2
  (progn
    (defstruct (change-list-structure-2 (:type list)))
    (defstruct (change-list-structure-2 (:type list)))
    (typep (find-class 'change-list-structure-2) 'structure-class))
  t)

(deftest change-list-structure.3
  (let (x y)
    (defstruct (change-list-structure-3 (:type list)))
    (setq x (find-class 'change-list-structure-3))
    (defstruct (change-list-structure-3 (:type list)))
    (setq y (find-class 'change-list-structure-3))
    (eq x y))
  t)

(deftest change-vector-structure.1
  (progn
    (defstruct (change-vector-structure-1 (:type vector)))
    (defstruct (change-vector-structure-1 (:type vector)))
    (null (find-class 'change-vector-structure-1)))
  nil)

(deftest change-vector-structure.2
  (progn
    (defstruct (change-vector-structure-2 (:type vector)))
    (defstruct (change-vector-structure-2 (:type vector)))
    (typep (find-class 'change-vector-structure-2) 'structure-class))
  t)

(deftest change-vector-structure.3
  (let (x y)
    (defstruct (change-vector-structure-3 (:type vector)))
    (setq x (find-class 'change-vector-structure-3))
    (defstruct (change-vector-structure-3 (:type vector)))
    (setq y (find-class 'change-vector-structure-3))
    (eq x y))
  t)

