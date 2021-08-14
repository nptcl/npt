;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  constructor
;;

;;  clos
(defstruct clos-constructor-1)
(deftest clos-constructor.1
  (fboundp 'make-clos-constructor-1)
  t)

(defstruct (clos-constructor-2 :constructor) aaa)
(deftest clos-constructor.2
  (fboundp 'make-clos-constructor-2)
  t)

(defstruct (clos-constructor-3 (:constructor)) aaa)
(deftest clos-constructor.3
  (fboundp 'make-clos-constructor-3)
  t)

(defstruct (clos-constructor-4 (:constructor nil)) aaa)
(deftest clos-constructor.4
  (fboundp 'make-clos-constructor-4)
  nil)

(defstruct (clos-constructor-5 (:constructor hello-clos-constructor-5)) aaa)
(deftest clos-constructor.5
  (fboundp 'hello-clos-constructor-5)
  t)

(defstruct clos-constructor-6 aaa bbb)
(deftest clos-constructor.6
  (let ((inst (make-clos-constructor-6 :aaa 10 :bbb 20)))
    (values
      (clos-constructor-6-aaa inst)
      (clos-constructor-6-bbb inst)))
  10 20)

(defstruct (clos-constructor-7 (:include clos-constructor-6)) ccc)
(deftest clos-constructor.7
  (let ((inst (make-clos-constructor-7 :aaa 10 :bbb 20 :ccc 30 :bbb 40)))
    (values
      (clos-constructor-7-aaa inst)
      (clos-constructor-7-bbb inst)
      (clos-constructor-7-ccc inst)))
  10 20 30)

(defstruct (clos-constructor-error-1
             (:constructor make-clos-constructor-error-1a)
             (:constructor make-clos-constructor-error-1b))
  value)

(deftest clos-constructor-error.1
  (values
    (clos-constructor-error-1-value (make-clos-constructor-error-1a :value 10))
    (clos-constructor-error-1-value (make-clos-constructor-error-1a :value 20)))
  10 20)


;;  list
(defstruct (list-constructor-1 (:type list)))
(deftest list-constructor.1
  (fboundp 'make-list-constructor-1)
  t)

(defstruct (list-constructor-2 (:type list) :constructor) aaa)
(deftest list-constructor.2
  (fboundp 'make-list-constructor-2)
  t)

(defstruct (list-constructor-3 (:type list) (:constructor)) aaa)
(deftest list-constructor.3
  (fboundp 'make-list-constructor-3)
  t)

(defstruct (list-constructor-4 (:type list) (:constructor nil)) aaa)
(deftest list-constructor.4
  (fboundp 'make-list-constructor-4)
  nil)

(defstruct (list-constructor-5
             (:type list) :named
             (:constructor hello-list-constructor-5)) aaa)
(deftest list-constructor.5
  (fboundp 'hello-list-constructor-5)
  t)

(defstruct (list-constructor-6 (:type list)) aaa bbb)
(deftest list-constructor.6
  (let ((inst (make-list-constructor-6 :aaa 10 :bbb 20)))
    (values
      (list-constructor-6-aaa inst)
      (list-constructor-6-bbb inst)))
  10 20)

(defstruct (list-constructor-7
             (:type list)
             (:include list-constructor-6)) ccc)
(deftest list-constructor.7
  (let ((inst (make-list-constructor-7 :aaa 10 :bbb 20 :ccc 30 :bbb 40)))
    (values
      (list-constructor-7-aaa inst)
      (list-constructor-7-bbb inst)
      (list-constructor-7-ccc inst)))
  10 20 30)


;;  vector
(defstruct (vector-constructor-1 (:type vector)))
(deftest vector-constructor.1
  (fboundp 'make-vector-constructor-1)
  t)

(defstruct (vector-constructor-2 (:type vector) :constructor) aaa)
(deftest vector-constructor.2
  (fboundp 'make-vector-constructor-2)
  t)

(defstruct (vector-constructor-3 (:type vector) (:constructor)) aaa)
(deftest vector-constructor.3
  (fboundp 'make-vector-constructor-3)
  t)

(defstruct (vector-constructor-4 (:type vector) (:constructor nil)) aaa)
(deftest vector-constructor.4
  (fboundp 'make-vector-constructor-4)
  nil)

(defstruct (vector-constructor-5
             (:type vector) :named
             (:constructor hello-vector-constructor-5)) aaa)
(deftest vector-constructor.5
  (fboundp 'hello-vector-constructor-5)
  t)

(defstruct (vector-constructor-6 (:type vector)) aaa bbb)
(deftest vector-constructor.6
  (let ((inst (make-vector-constructor-6 :aaa 10 :bbb 20)))
    (values
      (vector-constructor-6-aaa inst)
      (vector-constructor-6-bbb inst)))
  10 20)

(defstruct (vector-constructor-7
             (:type vector)
             (:include vector-constructor-6)) ccc)
(deftest vector-constructor.7
  (let ((inst (make-vector-constructor-7 :aaa 10 :bbb 20 :ccc 30 :bbb 40)))
    (values
      (vector-constructor-7-aaa inst)
      (vector-constructor-7-bbb inst)
      (vector-constructor-7-ccc inst)))
  10 20 30)


;;  change
(deftest change-clos-constructor.1
  (progn
    (defstruct change-clos-constructor-1)
    (defstruct change-clos-constructor-1)
    (change-clos-constructor-1-p
      (make-change-clos-constructor-1)))
  t)

(deftest change-clos-constructor.2
  (progn
    (defstruct change-clos-constructor-2)
    (defstruct (change-clos-constructor-2
                 (:constructor make-change-clos-constructor-3)))
    (values
      (fboundp 'make-change-clos-constructor-2)
      (fboundp 'make-change-clos-constructor-3)))
  nil t)

(deftest change-clos-constructor.3
  (progn
    (defstruct (change-clos-constructor-4
                 (:constructor make-change-clos-constructor-5)
                 (:constructor make-change-clos-constructor-6)
                 (:constructor make-change-clos-constructor-7)))
    (defstruct (change-clos-constructor-4
                 (:constructor make-change-clos-constructor-6)
                 (:constructor make-change-clos-constructor-8)))
    (values
      (fboundp 'make-change-clos-constructor-4)
      (fboundp 'make-change-clos-constructor-5)
      (fboundp 'make-change-clos-constructor-6)
      (fboundp 'make-change-clos-constructor-7)
      (fboundp 'make-change-clos-constructor-8)))
  nil nil t nil t)

