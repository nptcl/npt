;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  copier
;;

;;  clos
(defstruct clos-copier-1)
(deftest clos-copier.1
  (fboundp 'copy-clos-copier-1)
  t)

(defstruct (clos-copier-2 :copier) aaa)
(deftest clos-copier.2
  (fboundp 'copy-clos-copier-2)
  t)

(defstruct (clos-copier-3 (:copier)) aaa)
(deftest clos-copier.3
  (fboundp 'copy-clos-copier-3)
  t)

(defstruct (clos-copier-4 (:copier nil)) aaa)
(deftest clos-copier.4
  (fboundp 'copy-clos-copier-4)
  nil)

(defstruct (clos-copier-5 (:copier clos-copier-5-hello)) aaa)
(deftest clos-copier.5
  (fboundp 'clos-copier-5-hello)
  t)

(defstruct clos-copier-6 aaa bbb ccc)
(deftest clos-copier.6
  (let* ((x (make-clos-copier-6 :aaa 10 :bbb 20 :ccc 30))
         (y (copy-clos-copier-6 x)))
    (values
      (eq x y)
      (clos-copier-6-aaa y)
      (clos-copier-6-bbb y)
      (clos-copier-6-ccc y)))
  nil 10 20 30)

(deftest-error clos-copier.7
  (copy-clos-copier-6
    (make-clos-copier-5)))


;;  list
(defstruct (list-copier-1 (:type list)))
(deftest list-copier.1
  (fboundp 'copy-list-copier-1)
  t)

(defstruct (list-copier-2 :copier (:type list)) aaa)
(deftest list-copier.2
  (fboundp 'copy-list-copier-2)
  t)

(defstruct (list-copier-3 (:copier) (:type list)) aaa)
(deftest list-copier.3
  (fboundp 'copy-list-copier-3)
  t)

(defstruct (list-copier-4 (:copier nil) (:type list)) aaa)
(deftest list-copier.4
  (fboundp 'copy-list-copier-4)
  nil)

(defstruct (list-copier-5
             (:type list) :named
             (:copier list-copier-5-hello)) aaa)
(deftest list-copier.5
  (fboundp 'list-copier-5-hello)
  t)

(defstruct (list-copier-6 (:type list) :named) aaa bbb ccc)
(deftest list-copier.6
  (let* ((x (make-list-copier-6 :aaa 10 :bbb 20 :ccc 30))
         (y (copy-list-copier-6 x)))
    (values
      (eq x y)
      (list-copier-6-aaa y)
      (list-copier-6-bbb y)
      (list-copier-6-ccc y)))
  nil 10 20 30)

(deftest-error list-copier.7
  (copy-list-copier-6
    (make-list-copier-5)))


;;  vector
(defstruct (vector-copier-1 (:type vector)))
(deftest vector-copier.1
  (fboundp 'copy-vector-copier-1)
  t)

(defstruct (vector-copier-2 :copier (:type vector)) aaa)
(deftest vector-copier.2
  (fboundp 'copy-vector-copier-2)
  t)

(defstruct (vector-copier-3 (:copier) (:type vector)) aaa)
(deftest vector-copier.3
  (fboundp 'copy-vector-copier-3)
  t)

(defstruct (vector-copier-4 (:copier nil) (:type vector)) aaa)
(deftest vector-copier.4
  (fboundp 'copy-vector-copier-4)
  nil)

(defstruct (vector-copier-5
             (:type vector) :named
             (:copier vector-copier-5-hello)) aaa)
(deftest vector-copier.5
  (fboundp 'vector-copier-5-hello)
  t)

(defstruct (vector-copier-6 (:type vector) :named) aaa bbb ccc)
(deftest vector-copier.6
  (let* ((x (make-vector-copier-6 :aaa 10 :bbb 20 :ccc 30))
         (y (copy-vector-copier-6 x)))
    (values
      (eq x y)
      (vector-copier-6-aaa y)
      (vector-copier-6-bbb y)
      (vector-copier-6-ccc y)))
  nil 10 20 30)

(deftest-error vector-copier.7
  (copy-vector-copier-6
    (make-vector-copier-5)))


;;  change
(deftest change-clos-copier.1
  (progn
    (defstruct change-clos-copier-1)
    (defstruct change-clos-copier-1)
    (fboundp 'copy-change-clos-copier-1))
  t)

(deftest change-clos-copier.2
  (let (x)
    (defstruct change-clos-copier-2 aaa)
    (setq x (make-change-clos-copier-2 :aaa 100))
    (defstruct change-clos-copier-2 aaa)
    (change-clos-copier-2-aaa
      (copy-change-clos-copier-2 x)))
  100)

(deftest change-clos-copier.3
  (progn
    (defstruct change-clos-copier-3)
    (defstruct (change-clos-copier-3 (:copier copy-change-clos-copier-4)))
    (values
      (fboundp 'copy-change-clos-copier-3)
      (fboundp 'copy-change-clos-copier-4)))
  nil t)

(deftest change-list-copier.1
  (progn
    (defstruct (change-list-copier-1 (:type list)))
    (defstruct (change-list-copier-1 (:type list)))
    (fboundp 'copy-change-list-copier-1))
  t)

(deftest change-list-copier.2
  (let (x)
    (defstruct (change-list-copier-2 (:type list)) aaa)
    (setq x (make-change-list-copier-2 :aaa 100))
    (defstruct (change-list-copier-2 (:type list)) aaa)
    (change-list-copier-2-aaa
      (copy-change-list-copier-2 x)))
  100)

(deftest change-list-copier.3
  (progn
    (defstruct (change-list-copier-3 (:type list)))
    (defstruct (change-list-copier-3
                 (:copier copy-change-list-copier-4)
                 (:type list)))
    (values
      (fboundp 'copy-change-list-copier-3)
      (fboundp 'copy-change-list-copier-4)))
  nil t)

(deftest change-vector-copier.1
  (progn
    (defstruct (change-vector-copier-1 (:type vector)))
    (defstruct (change-vector-copier-1 (:type vector)))
    (fboundp 'copy-change-vector-copier-1))
  t)

(deftest change-vector-copier.2
  (let (x)
    (defstruct (change-vector-copier-2 (:type vector)) aaa)
    (setq x (make-change-vector-copier-2 :aaa 100))
    (defstruct (change-vector-copier-2 (:type vector)) aaa)
    (change-vector-copier-2-aaa
      (copy-change-vector-copier-2 x)))
  100)

(deftest change-vector-copier.3
  (progn
    (defstruct (change-vector-copier-3 (:type vector)))
    (defstruct (change-vector-copier-3
                 (:copier copy-change-vector-copier-4)
                 (:type vector)))
    (values
      (fboundp 'copy-change-vector-copier-3)
      (fboundp 'copy-change-vector-copier-4)))
  nil t)

