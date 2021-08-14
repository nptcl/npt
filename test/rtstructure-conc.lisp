;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  conc-name
;;

;;  clos
(defstruct (clos-conc-name-1 :conc-name) aaa)
(deftest clos-conc-name.1
  (values
    (fboundp 'clos-conc-name-1-aaa)
    (fboundp '(setf clos-conc-name-1-aaa)))
  nil nil)

(defstruct (clos-conc-name-2 (:conc-name)) aaa)
(deftest clos-conc-name.2
  (values
    (fboundp 'clos-conc-name-2-aaa)
    (fboundp '(setf clos-conc-name-2-aaa)))
  nil nil)

(defstruct (clos-conc-name-3 (:conc-name nil)) aaa)
(deftest clos-conc-name.3
  (values
    (fboundp 'clos-conc-name-3-aaa)
    (fboundp '(setf clos-conc-name-3-aaa)))
  nil nil)

(defstruct (clos-conc-name-4 (:conc-name hello-clos-conc-name-4-)) aaa)
(deftest clos-conc-name.4
  (values
    (fboundp 'clos-conc-name-4-aaa)
    (fboundp 'hello-clos-conc-name-4-aaa)
    (fboundp '(setf clos-conc-name-4-aaa))
    (fboundp '(setf hello-clos-conc-name-4-aaa)))
  nil t nil t)

(defstruct (clos-conc-name-5 (:conc-name hello-clos-conc-name-5-)
                             (:include clos-conc-name-4))
  bbb ccc)
(deftest clos-conc-name.5
  (let ((inst (make-clos-conc-name-5 :aaa 10 :bbb 20 :ccc 30)))
    (values
      (hello-clos-conc-name-5-aaa inst)
      (hello-clos-conc-name-5-bbb inst)
      (hello-clos-conc-name-5-ccc inst)))
  10 20 30)

(deftest clos-conc-name.6
  (let ((inst (make-clos-conc-name-5 :aaa 10 :bbb 20 :ccc 30)))
    (setf (hello-clos-conc-name-5-aaa inst) 40)
    (setf (hello-clos-conc-name-5-bbb inst) 50)
    (setf (hello-clos-conc-name-5-ccc inst) 60)
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  40 50 60)

(defstruct (clos-conc-name-7) aaa)
(deftest clos-conc-name.7
  (values
    (fboundp 'clos-conc-name-7-aaa)
    (fboundp '(setf clos-conc-name-7-aaa)))
  t t)


;;  list
(defstruct (list-conc-name-1 :conc-name (:type list) :named) aaa)
(deftest list-conc-name.1
  (values
    (fboundp 'list-conc-name-1-aaa)
    (fboundp '(setf list-conc-name-1-aaa)))
  nil nil)

(defstruct (list-conc-name-2 (:conc-name) (:type list)) aaa)
(deftest list-conc-name.2
  (values
    (fboundp 'list-conc-name-2-aaa)
    (fboundp '(setf list-conc-name-2-aaa)))
  nil nil)

(defstruct (list-conc-name-3 (:conc-name nil) (:type list)) aaa)
(deftest list-conc-name.3
  (values
    (fboundp 'list-conc-name-3-aaa)
    (fboundp '(setf list-conc-name-3-aaa)))
  nil nil)

(defstruct (list-conc-name-4
             (:type list) :named
             (:conc-name hello-list-conc-name-4-)) aaa)
(deftest list-conc-name.4
  (values
    (fboundp 'list-conc-name-4-aaa)
    (fboundp 'hello-list-conc-name-4-aaa)
    (fboundp '(setf list-conc-name-4-aaa))
    (fboundp '(setf hello-list-conc-name-4-aaa)))
  nil t nil t)

(defstruct (list-conc-name-5
             (:type list)
             (:conc-name hello-list-conc-name-5-)
             (:include list-conc-name-4))
  bbb ccc)
(deftest list-conc-name.5
  (let ((inst (make-list-conc-name-5 :aaa 10 :bbb 20 :ccc 30)))
    (values
      (hello-list-conc-name-5-aaa inst)
      (hello-list-conc-name-5-bbb inst)
      (hello-list-conc-name-5-ccc inst)))
  10 20 30)

(defstruct (list-conc-name-6) aaa)
(deftest list-conc-name.6
  (values
    (fboundp 'list-conc-name-6-aaa)
    (fboundp '(setf list-conc-name-6-aaa)))
  t t)


;;  vector
(defstruct (vector-conc-name-1 :conc-name (:type vector) :named) aaa)
(deftest vector-conc-name.1
  (values
    (fboundp 'vector-conc-name-1-aaa)
    (fboundp '(setf vector-conc-name-1-aaa)))
  nil nil)

(defstruct (vector-conc-name-2 (:conc-name) (:type vector)) aaa)
(deftest vector-conc-name.2
  (values
    (fboundp 'vector-conc-name-2-aaa)
    (fboundp '(setf vector-conc-name-2-aaa)))
  nil nil)

(defstruct (vector-conc-name-3 (:conc-name nil) (:type vector)) aaa)
(deftest vector-conc-name.3
  (values
    (fboundp 'vector-conc-name-3-aaa)
    (fboundp '(setf vector-conc-name-3-aaa)))
  nil nil)

(defstruct (vector-conc-name-4
             (:type vector) :named
             (:conc-name hello-vector-conc-name-4-)) aaa)
(deftest vector-conc-name.4
  (values
    (fboundp 'vector-conc-name-4-aaa)
    (fboundp 'hello-vector-conc-name-4-aaa)
    (fboundp '(setf vector-conc-name-4-aaa))
    (fboundp '(setf hello-vector-conc-name-4-aaa)))
  nil t nil t)

(defstruct (vector-conc-name-5
             (:type vector)
             (:conc-name hello-vector-conc-name-5-)
             (:include vector-conc-name-4))
  bbb ccc)
(deftest vector-conc-name.5
  (let ((inst (make-vector-conc-name-5 :aaa 10 :bbb 20 :ccc 30)))
    (values
      (hello-vector-conc-name-5-aaa inst)
      (hello-vector-conc-name-5-bbb inst)
      (hello-vector-conc-name-5-ccc inst)))
  10 20 30)

(defstruct (vector-conc-name-6) aaa)
(deftest vector-conc-name.6
  (values
    (fboundp 'vector-conc-name-6-aaa)
    (fboundp '(setf vector-conc-name-6-aaa)))
  t t)


;;  change
(deftest change-slot-conc-name.1
  (progn
    (defstruct change-slot-conc-name-1 aaa)
    (defstruct change-slot-conc-name-1 aaa)
    (let ((x (make-change-slot-conc-name-1)))
      (setf (change-slot-conc-name-1-aaa x) 100)
      (change-slot-conc-name-1-aaa x)))
  100)

(deftest change-slot-conc-name.2
  (progn
    (defstruct change-slot-conc-name-2 aaa)
    (defstruct (change-slot-conc-name-2 (:conc-name change-slot-conc-name-3-)) aaa)
    (values
      (fboundp 'change-slot-conc-name-2-aaa)
      (fboundp '(setf change-slot-conc-name-2-aaa))
      (fboundp 'change-slot-conc-name-3-aaa)
      (fboundp '(setf change-slot-conc-name-3-aaa))))
  nil nil t t)

(deftest change-slot-conc-name.3
  (progn
    (defstruct change-slot-conc-name-4 aaa)
    (defstruct (change-slot-conc-name-4 (:conc-name change-slot-conc-name-5-)) aaa)
    (let ((x (make-change-slot-conc-name-4)))
      (setf (change-slot-conc-name-5-aaa x) 100)
      (change-slot-conc-name-5-aaa x)))
  100)

(deftest change-slot-conc-name.4
  (progn
    (defstruct change-slot-conc-name-6 aaa)
    (let ((x (make-change-slot-conc-name-6 :aaa 100)))
      (defstruct (change-slot-conc-name-6 (:conc-name change-slot-conc-name-7-)) aaa)
      (change-slot-conc-name-7-aaa x)))
  100)

