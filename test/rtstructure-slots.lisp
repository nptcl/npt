;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  empty
;;

;;  clos
(deftest clos-empty.1
  (defstruct clos-empty-1)
  clos-empty-1)

(deftest clos-empty.2
  (defstruct clos-empty-1)
  clos-empty-1)

(deftest clos-empty.3
  (defstruct clos-empty-3 aaa bbb ccc)
  clos-empty-3)

(deftest-error clos-empty.4
  (defstruct clos-empty-4 aaa bbb ccc bbb))

(deftest-error clos-empty.5
  (defstruct clos-empty-5 aaa bbb ccc :aaa))


;;  list
(deftest list-empty.1
  (defstruct (list-empty-1 (:type list)))
  list-empty-1)

(deftest list-empty.2
  (defstruct (list-empty-1 (:type list)))
  list-empty-1)

(deftest list-empty.3
  (defstruct (list-empty-3 (:type list)) aaa bbb ccc)
  list-empty-3)

(deftest-error list-empty.4
  (defstruct (list-empty-4 (:type list)) aaa bbb ccc bbb))

(deftest-error list-empty.5
  (defstruct (list-empty-5 (:type list)) aaa bbb ccc :aaa))

(deftest list-empty.6
  (defstruct (list-empty-named (:type list) :named) aaa bbb ccc)
  list-empty-named)


;;  vector
(deftest vector-empty.1
  (defstruct (vector-empty-1 (:type vector)))
  vector-empty-1)

(deftest vector-empty.2
  (defstruct (vector-empty-1 (:type vector)))
  vector-empty-1)

(deftest vector-empty.3
  (defstruct (vector-empty-3 (:type vector)) aaa bbb ccc)
  vector-empty-3)

(deftest-error vector-empty.4
  (defstruct (vector-empty-4 (:type vector)) aaa bbb ccc bbb))

(deftest-error vector-empty.5
  (defstruct (vector-empty-5 (:type vector)) aaa bbb ccc :aaa))

(deftest vector-empty.6
  (defstruct (vector-empty-named (:type vector) :named) aaa bbb ccc)
  vector-empty-named)


;;  change
(deftest change-empty.1
  (progn
    (defstruct change-empty-1)
    (defstruct change-empty-1))
  change-empty-1)

(deftest change-empty.2
  (progn
    (defstruct change-empty-2 aaa bbb ccc)
    (defstruct change-empty-2 aaa bbb ccc))
  change-empty-2)

(deftest-error change-empty.3
  (progn
    (defstruct change-empty-3 aaa bbb ccc)
    (defstruct change-empty-3 aaa bbb)))

(deftest-error change-empty.4
  (progn
    (defstruct change-empty-4 aaa bbb)
    (defstruct change-empty-4 aaa bbb ccc)))

(deftest change-empty.5
  (let (x y)
    (defstruct change-empty-5)
    (setq x (find-class 'change-empty-5))
    (defstruct change-empty-5)
    (setq y (find-class 'change-empty-5))
    (eq x y))
  t)


;;
;;  value
;;

;;  clos
(deftest clos-initialize-slot-value.1
  (defstruct clos-initialize-slot-value-1
    aaa)
  clos-initialize-slot-value-1)

(deftest clos-initialize-slot-value.2
  (defstruct clos-initialize-slot-value-2
    (aaa))
  clos-initialize-slot-value-2)

(deftest clos-initialize-slot-value.3
  (defstruct clos-initialize-slot-value-3
    (ccc 10))
  clos-initialize-slot-value-3)

(deftest clos-initialize-slot-value.4
  (defstruct clos-initialize-slot-value-4
    (ddd 20 :type integer))
  clos-initialize-slot-value-4)

(deftest clos-initialize-slot-value.5
  (defstruct clos-initialize-slot-value-5
    (eee 30 :read-only t))
  clos-initialize-slot-value-5)

(deftest clos-initialize-slot-value.6
  (defstruct clos-initialize-slot-value-6
    (fff 40 :read-only nil :type integer :type string :read-only t))
  clos-initialize-slot-value-6)


;;  list
(deftest list-initialize-slot-value.1
  (defstruct (list-initialize-slot-value-1 (:type list))
    aaa)
  list-initialize-slot-value-1)

(deftest list-initialize-slot-value.2
  (defstruct (list-initialize-slot-value-2 (:type list))
    (aaa))
  list-initialize-slot-value-2)

(deftest list-initialize-slot-value.3
  (defstruct (list-initialize-slot-value-3 (:type list))
    (ccc 10))
  list-initialize-slot-value-3)

(deftest list-initialize-slot-value.4
  (defstruct (list-initialize-slot-value-4 (:type list))
    (ddd 20 :type integer))
  list-initialize-slot-value-4)

(deftest list-initialize-slot-value.5
  (defstruct (list-initialize-slot-value-5 (:type list))
    (eee 30 :read-only t))
  list-initialize-slot-value-5)

(deftest list-initialize-slot-value.6
  (defstruct (list-initialize-slot-value-6 (:type list) :named)
    (fff 40 :read-only nil :type integer :type string :read-only t))
  list-initialize-slot-value-6)


;;  vector
(deftest vector-initialize-slot-value.1
  (defstruct (vector-initialize-slot-value-1 (:type vector))
    aaa)
  vector-initialize-slot-value-1)

(deftest vector-initialize-slot-value.2
  (defstruct (vector-initialize-slot-value-2 (:type vector))
    (aaa))
  vector-initialize-slot-value-2)

(deftest vector-initialize-slot-value.3
  (defstruct (vector-initialize-slot-value-3 (:type vector))
    (ccc 10))
  vector-initialize-slot-value-3)

(deftest vector-initialize-slot-value.4
  (defstruct (vector-initialize-slot-value-4 (:type vector))
    (ddd 20 :type integer))
  vector-initialize-slot-value-4)

(deftest vector-initialize-slot-value.5
  (defstruct (vector-initialize-slot-value-5 (:type vector))
    (eee 30 :read-only t))
  vector-initialize-slot-value-5)

(deftest vector-initialize-slot-value.6
  (defstruct (vector-initialize-slot-value-6 (:type vector) :named)
    (fff 40 :read-only nil :type integer :type string :read-only t))
  vector-initialize-slot-value-6)

