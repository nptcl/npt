;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  boa
;;

;;  clos
(defstruct (clos-boa-1 (:constructor make-clos-boa-1 ())) aaa bbb)
(deftest clos-boa.1
  (let ((inst (make-clos-boa-1)))
    (values
      (clos-boa-1-aaa inst)
      (clos-boa-1-bbb inst)))
  nil nil)

(defstruct (clos-boa-2 (:constructor make-clos-boa-2 (aaa bbb))) aaa bbb)
(deftest clos-boa.2
  (let ((inst (make-clos-boa-2 10 20)))
    (values
      (clos-boa-2-aaa inst)
      (clos-boa-2-bbb inst)))
  10 20)

(defstruct (clos-boa-3 (:constructor make-clos-boa-3 (aaa bbb ccc))) aaa bbb)
(deftest clos-boa.3
  (let ((inst (make-clos-boa-3 10 20 30)))
    (values
      (clos-boa-3-aaa inst)
      (clos-boa-3-bbb inst)))
  10 20)

(defstruct (clos-boa-4 (:constructor make-clos-boa-4 (aaa)))
  (aaa 999) (bbb 888))
(deftest clos-boa.4
  (let ((inst (make-clos-boa-4 10)))
    (values
      (clos-boa-4-aaa inst)
      (clos-boa-4-bbb inst)))
  10 888)

(defstruct (clos-boa-5 (:constructor make-clos-boa-5 (aaa &optional bbb)))
  (aaa 999) (bbb 888))
(deftest clos-boa.5
  (let ((inst (make-clos-boa-5 10)))
    (values
      (clos-boa-5-aaa inst)
      (clos-boa-5-bbb inst)))
  10 888)

(deftest clos-boa.6
  (let ((inst (make-clos-boa-5 10 20)))
    (values
      (clos-boa-5-aaa inst)
      (clos-boa-5-bbb inst)))
  10 20)

(defstruct (clos-boa-7 (:constructor make-clos-boa-7 (&key (aaa 200) ((:hello bbb)))))
  (aaa 999) (bbb 888))
(deftest clos-boa.7
  (let ((inst (make-clos-boa-7)))
    (values
      (clos-boa-7-aaa inst)
      (clos-boa-7-bbb inst)))
  200 888)

(deftest clos-boa.8
  (let ((inst (make-clos-boa-7 :aaa 123)))
    (values
      (clos-boa-7-aaa inst)
      (clos-boa-7-bbb inst)))
  123 888)

(deftest clos-boa.9
  (let ((inst (make-clos-boa-7 :hello 123)))
    (values
      (clos-boa-7-aaa inst)
      (clos-boa-7-bbb inst)))
  200 123)

(defstruct (clos-boa-10 (:constructor make-clos-boa-10 (&rest aaa &aux bbb)))
  (aaa 999) (bbb 888))
(deftest clos-boa.10
  (let ((inst (make-clos-boa-10)))
    (values
      (clos-boa-10-aaa inst)
      (clos-boa-10-bbb inst)))
  nil nil)

(deftest clos-boa.11
  (let ((inst (make-clos-boa-10 10 20 30)))
    (values
      (clos-boa-10-aaa inst)
      (clos-boa-10-bbb inst)))
  (10 20 30) nil)

(defstruct (clos-boa-12 (:constructor make-clos-boa-12 (&rest aaa &aux (bbb 777))))
  (aaa 999) (bbb 888))
(deftest clos-boa.12
  (let ((inst (make-clos-boa-12)))
    (values
      (clos-boa-12-aaa inst)
      (clos-boa-12-bbb inst)))
  nil 777)


;;  list
(defstruct (list-boa-1
             (:type list)
             (:constructor make-list-boa-1 ())) aaa bbb)
(deftest list-boa.1
  (let ((inst (make-list-boa-1)))
    (values
      (list-boa-1-aaa inst)
      (list-boa-1-bbb inst)))
  nil nil)

(defstruct (list-boa-2
             (:type list) :named
             (:constructor make-list-boa-2 (aaa bbb))) aaa bbb)
(deftest list-boa.2
  (let ((inst (make-list-boa-2 10 20)))
    (values
      (list-boa-2-aaa inst)
      (list-boa-2-bbb inst)))
  10 20)

(defstruct (list-boa-3
             (:type list)
             (:constructor make-list-boa-3 (aaa bbb ccc))) aaa bbb)
(deftest list-boa.3
  (let ((inst (make-list-boa-3 10 20 30)))
    (values
      (list-boa-3-aaa inst)
      (list-boa-3-bbb inst)))
  10 20)

(defstruct (list-boa-4
             (:type list) :named
             (:constructor make-list-boa-4 (aaa)))
  (aaa 999) (bbb 888))
(deftest list-boa.4
  (let ((inst (make-list-boa-4 10)))
    (values
      (list-boa-4-aaa inst)
      (list-boa-4-bbb inst)))
  10 888)

(defstruct (list-boa-5
             (:type list)
             (:constructor make-list-boa-5 (aaa &optional bbb)))
  (aaa 999) (bbb 888))
(deftest list-boa.5
  (let ((inst (make-list-boa-5 10)))
    (values
      (list-boa-5-aaa inst)
      (list-boa-5-bbb inst)))
  10 888)

(deftest list-boa.6
  (let ((inst (make-list-boa-5 10 20)))
    (values
      (list-boa-5-aaa inst)
      (list-boa-5-bbb inst)))
  10 20)

(defstruct (list-boa-7
             (:type list) :named
             (:constructor make-list-boa-7 (&key (aaa 200) ((:hello bbb)))))
  (aaa 999) (bbb 888))
(deftest list-boa.7
  (let ((inst (make-list-boa-7)))
    (values
      (list-boa-7-aaa inst)
      (list-boa-7-bbb inst)))
  200 888)

(deftest list-boa.8
  (let ((inst (make-list-boa-7 :aaa 123)))
    (values
      (list-boa-7-aaa inst)
      (list-boa-7-bbb inst)))
  123 888)

(deftest list-boa.9
  (let ((inst (make-list-boa-7 :hello 123)))
    (values
      (list-boa-7-aaa inst)
      (list-boa-7-bbb inst)))
  200 123)

(defstruct (list-boa-10
             (:type list)
             (:constructor make-list-boa-10 (&rest aaa &aux bbb)))
  (aaa 999) (bbb 888))
(deftest list-boa.10
  (let ((inst (make-list-boa-10)))
    (values
      (list-boa-10-aaa inst)
      (list-boa-10-bbb inst)))
  nil nil)

(deftest list-boa.11
  (let ((inst (make-list-boa-10 10 20 30)))
    (values
      (list-boa-10-aaa inst)
      (list-boa-10-bbb inst)))
  (10 20 30) nil)

(defstruct (list-boa-12
             (:type list) :named
             (:constructor make-list-boa-12 (&rest aaa &aux (bbb 777))))
  (aaa 999) (bbb 888))
(deftest list-boa.12
  (let ((inst (make-list-boa-12)))
    (values
      (list-boa-12-aaa inst)
      (list-boa-12-bbb inst)))
  nil 777)


;;  vector
(defstruct (vector-boa-1
             (:type vector)
             (:constructor make-vector-boa-1 ())) aaa bbb)
(deftest vector-boa.1
  (let ((inst (make-vector-boa-1)))
    (values
      (vector-boa-1-aaa inst)
      (vector-boa-1-bbb inst)))
  nil nil)

(defstruct (vector-boa-2
             (:type vector) :named
             (:constructor make-vector-boa-2 (aaa bbb))) aaa bbb)
(deftest vector-boa.2
  (let ((inst (make-vector-boa-2 10 20)))
    (values
      (vector-boa-2-aaa inst)
      (vector-boa-2-bbb inst)))
  10 20)

(defstruct (vector-boa-3
             (:type vector)
             (:constructor make-vector-boa-3 (aaa bbb ccc))) aaa bbb)
(deftest vector-boa.3
  (let ((inst (make-vector-boa-3 10 20 30)))
    (values
      (vector-boa-3-aaa inst)
      (vector-boa-3-bbb inst)))
  10 20)

(defstruct (vector-boa-4
             (:type vector) :named
             (:constructor make-vector-boa-4 (aaa)))
  (aaa 999) (bbb 888))
(deftest vector-boa.4
  (let ((inst (make-vector-boa-4 10)))
    (values
      (vector-boa-4-aaa inst)
      (vector-boa-4-bbb inst)))
  10 888)

(defstruct (vector-boa-5
             (:type vector)
             (:constructor make-vector-boa-5 (aaa &optional bbb)))
  (aaa 999) (bbb 888))
(deftest vector-boa.5
  (let ((inst (make-vector-boa-5 10)))
    (values
      (vector-boa-5-aaa inst)
      (vector-boa-5-bbb inst)))
  10 888)

(deftest vector-boa.6
  (let ((inst (make-vector-boa-5 10 20)))
    (values
      (vector-boa-5-aaa inst)
      (vector-boa-5-bbb inst)))
  10 20)

(defstruct (vector-boa-7
             (:type vector) :named
             (:constructor make-vector-boa-7 (&key (aaa 200) ((:hello bbb)))))
  (aaa 999) (bbb 888))
(deftest vector-boa.7
  (let ((inst (make-vector-boa-7)))
    (values
      (vector-boa-7-aaa inst)
      (vector-boa-7-bbb inst)))
  200 888)

(deftest vector-boa.8
  (let ((inst (make-vector-boa-7 :aaa 123)))
    (values
      (vector-boa-7-aaa inst)
      (vector-boa-7-bbb inst)))
  123 888)

(deftest vector-boa.9
  (let ((inst (make-vector-boa-7 :hello 123)))
    (values
      (vector-boa-7-aaa inst)
      (vector-boa-7-bbb inst)))
  200 123)

(defstruct (vector-boa-10
             (:type vector)
             (:constructor make-vector-boa-10 (&rest aaa &aux bbb)))
  (aaa 999) (bbb 888))
(deftest vector-boa.10
  (let ((inst (make-vector-boa-10)))
    (values
      (vector-boa-10-aaa inst)
      (vector-boa-10-bbb inst)))
  nil nil)

(deftest vector-boa.11
  (let ((inst (make-vector-boa-10 10 20 30)))
    (values
      (vector-boa-10-aaa inst)
      (vector-boa-10-bbb inst)))
  (10 20 30) nil)

(defstruct (vector-boa-12
             (:type vector) :named
             (:constructor make-vector-boa-12 (&rest aaa &aux (bbb 777))))
  (aaa 999) (bbb 888))
(deftest vector-boa.12
  (let ((inst (make-vector-boa-12)))
    (values
      (vector-boa-12-aaa inst)
      (vector-boa-12-bbb inst)))
  nil 777)

