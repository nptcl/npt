;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  predicate-check
;;

;;  clos
(deftest clos-predicate-check.1
  (defstruct (clos-predicate-check-1 :predicate))
  clos-predicate-check-1)

(deftest clos-predicate-check.2
  (progn
    (defstruct (clos-predicate-check-2 :predicate))
    (clos-predicate-check-2-p 10))
  nil)

(deftest clos-predicate-check.3
  (progn
    (defstruct (clos-predicate-check-3 :predicate))
    (clos-predicate-check-3-p
      (make-clos-predicate-check-3)))
  t)

(deftest clos-predicate-check.4
  (progn
    (defstruct (clos-predicate-check-4 (:predicate)))
    (clos-predicate-check-4-p
      (make-clos-predicate-check-4)))
  t)

(deftest clos-predicate-check.5
  (progn
    (defstruct (clos-predicate-check-5
                 (:predicate hello-clos-predicate-check-5)))
    (hello-clos-predicate-check-5
      (make-clos-predicate-check-5)))
  t)

(deftest clos-predicate-check.6
  (progn
    (defstruct (clos-predicate-check-6 (:predicate nil)))
    (fboundp 'clos-predicate-check-6-p))
  nil)

(deftest clos-predicate-check.7
  (defstruct (clos-predicate-check-7 (:type list)))
  clos-predicate-check-7)

(deftest-error clos-predicate-check.8
  (defstruct (clos-predicate-check-8 (:type list) :predicate)))

(deftest clos-predicate-check.9
  (defstruct (clos-predicate-check-9 (:type list) (:predicate nil)))
  clos-predicate-check-9)

(deftest-error clos-predicate-check.10
  (defstruct (clos-predicate-check-10 (:type list) (:predicate hello))))

(deftest clos-predicate-check.11
  (defstruct (clos-predicate-check-11 (:type list) :named))
  clos-predicate-check-11)

(deftest clos-predicate-check.12
  (defstruct (clos-predicate-check-12 (:type list) :named :predicate))
  clos-predicate-check-12)

(deftest clos-predicate-check.13
  (defstruct (clos-predicate-check-13 (:type list) :named (:predicate nil)))
  clos-predicate-check-13)

(deftest clos-predicate-check.14
  (defstruct (clos-predicate-check-14 (:type list) :named (:predicate hello)))
  clos-predicate-check-14)


;;  list
(deftest-error list-predicate-check.1
  (defstruct (list-predicate-check-1 :predicate (:type list))))

(deftest list-predicate-check.2
  (defstruct (list-predicate-check-2 :predicate (:type list) :named))
  list-predicate-check-2)

(deftest list-predicate-check.3
  (progn
    (defstruct (list-predicate-check-3 :predicate (:type list) :named))
    (list-predicate-check-3-p 10))
  nil)

(deftest list-predicate-check.4
  (progn
    (defstruct (list-predicate-check-4 :predicate (:type list) :named))
    (list-predicate-check-4-p
      (make-list-predicate-check-4)))
  t)

(deftest list-predicate-check.5
  (progn
    (defstruct (list-predicate-check-5 :predicate (:type list) :named)
      aaa (bbb 100))
    (values
      (list-predicate-check-5-p (make-list-predicate-check-5))
      (list-predicate-check-5-p (list 'list-predicate-check-5 10))
      (list-predicate-check-5-p (list 'list-predicate-check-5 20 30))
      (list-predicate-check-5-p (list 'list-predicate-check-5 40 50 60)) ;; t
      (list-predicate-check-5-p (list 'list-predicate-check-4 20 30))))
  t nil t t nil)

(deftest list-predicate-check.6
  (progn
    (defstruct (list-predicate-check-6 (:predicate) (:type list) :named) aaa)
    (list-predicate-check-6-p
      (make-list-predicate-check-6)))
  t)

(deftest list-predicate-check.7
  (progn
    (defstruct (list-predicate-check-7
                 (:predicate hello-list-predicate-check-7)
                 (:type list) :named))
    (hello-list-predicate-check-7
      (make-list-predicate-check-7)))
  t)

(deftest list-predicate-check.8
  (progn
    (defstruct (list-predicate-check-8 (:predicate nil) (:type list) :named))
    (fboundp 'list-predicate-check-8-p))
  nil)


;;  vector
(deftest-error vector-predicate-check.1
  (defstruct (vector-predicate-check-1 :predicate (:type vector))))

(deftest vector-predicate-check.2
  (defstruct (vector-predicate-check-2 :predicate (:type vector) :named))
  vector-predicate-check-2)

(deftest vector-predicate-check.3
  (progn
    (defstruct (vector-predicate-check-3 :predicate (:type vector) :named))
    (vector-predicate-check-3-p 10))
  nil)

(deftest vector-predicate-check.4
  (progn
    (defstruct (vector-predicate-check-4 :predicate (:type vector) :named))
    (vector-predicate-check-4-p
      (make-vector-predicate-check-4)))
  t)

(deftest vector-predicate-check.5
  (progn
    (defstruct (vector-predicate-check-5 :predicate (:type vector) :named)
      aaa (bbb 100))
    (values
      (vector-predicate-check-5-p (make-vector-predicate-check-5))
      (vector-predicate-check-5-p (vector 'vector-predicate-check-5 10))
      (vector-predicate-check-5-p (vector 'vector-predicate-check-5 20 30))
      (vector-predicate-check-5-p (vector 'vector-predicate-check-5 40 50 60)) ;; t
      (vector-predicate-check-5-p (vector 'vector-predicate-check-4 20 30))))
  t nil t t nil)

(deftest vector-predicate-check.6
  (progn
    (defstruct (vector-predicate-check-6 (:predicate) (:type vector) :named) aaa)
    (vector-predicate-check-6-p
      (make-vector-predicate-check-6)))
  t)

(deftest vector-predicate-check.7
  (progn
    (defstruct (vector-predicate-check-7
                 (:predicate hello-vector-predicate-check-7)
                 (:type vector) :named))
    (hello-vector-predicate-check-7
      (make-vector-predicate-check-7)))
  t)

(deftest vector-predicate-check.8
  (progn
    (defstruct (vector-predicate-check-8 (:predicate nil) (:type vector) :named))
    (fboundp 'vector-predicate-check-8-p))
  nil)


;;
;;  predicate
;;

;;  clos

(defstruct clos-predicate-1)
(deftest clos-predicate.1
  (fboundp 'clos-predicate-1-p)
  t)

(defstruct (clos-predicate-2 :predicate) aaa)
(deftest clos-predicate.2
  (fboundp 'clos-predicate-2-p)
  t)

(defstruct (clos-predicate-3 (:predicate)) aaa)
(deftest clos-predicate.3
  (fboundp 'clos-predicate-3-p)
  t)

(defstruct (clos-predicate-4 (:predicate nil)) aaa)
(deftest clos-predicate.4
  (fboundp 'clos-predicate-4-p)
  nil)

(defstruct (clos-predicate-5 (:predicate clos-predicate-5-hello)) aaa)
(deftest clos-predicate.5
  (fboundp 'clos-predicate-5-hello)
  t)

(defstruct clos-predicate-6 aaa bbb ccc)
(deftest clos-predicate.6
  (let ((x (make-clos-predicate-6 :aaa 10 :bbb 20 :ccc 30))
        (y (make-clos-predicate-5)))
    (values
      (clos-predicate-6-p x)
      (clos-predicate-6-p y)))
  t nil)

(deftest clos-predicate.7
  (clos-predicate-6-p 100)
  nil)


;; list
(defstruct (list-predicate-1 (:type list)))
(deftest list-predicate.1
  (fboundp 'list-predicate-1-p)
  nil)

(defstruct (list-predicate-2 (:type list) :named))
(deftest list-predicate.2
  (fboundp 'list-predicate-2-p)
  t)

(defstruct (list-predicate-3 (:type list) :named :predicate) aaa)
(deftest list-predicate.3
  (fboundp 'list-predicate-3-p)
  t)

(defstruct (list-predicate-4 (:type list) :named (:predicate)) aaa)
(deftest list-predicate.4
  (fboundp 'list-predicate-4-p)
  t)

(defstruct (list-predicate-5 (:type list) :named (:predicate nil)) aaa)
(deftest list-predicate.5
  (fboundp 'list-predicate-5-p)
  nil)

(defstruct (list-predicate-6
             (:type list) :named
             (:predicate list-predicate-6-hello)) aaa)
(deftest list-predicate.6
  (fboundp 'list-predicate-6-hello)
  t)

(defstruct (list-predicate-7 (:type list) :named) aaa bbb ccc)
(deftest list-predicate.7
  (let ((x (make-list-predicate-7 :aaa 10 :bbb 20 :ccc 30))
        (y (make-list-predicate-6)))
    (values
      (list-predicate-7-p x)
      (list-predicate-7-p y)))
  t nil)

(deftest list-predicate.8
  (list-predicate-7-p 100)
  nil)


;;  vector
(defstruct (vector-predicate-1 (:type vector)))
(deftest vector-predicate.1
  (fboundp 'vector-predicate-1-p)
  nil)

(defstruct (vector-predicate-2 (:type vector) :named))
(deftest vector-predicate.2
  (fboundp 'vector-predicate-2-p)
  t)

(defstruct (vector-predicate-3 (:type vector) :named :predicate) aaa)
(deftest vector-predicate.3
  (fboundp 'vector-predicate-3-p)
  t)

(defstruct (vector-predicate-4 (:type vector) :named (:predicate)) aaa)
(deftest vector-predicate.4
  (fboundp 'vector-predicate-4-p)
  t)

(defstruct (vector-predicate-5 (:type vector) :named (:predicate nil)) aaa)
(deftest vector-predicate.5
  (fboundp 'vector-predicate-5-p)
  nil)

(defstruct (vector-predicate-6
             (:type vector) :named
             (:predicate vector-predicate-6-hello)) aaa)
(deftest vector-predicate.6
  (fboundp 'vector-predicate-6-hello)
  t)

(defstruct (vector-predicate-7 (:type vector) :named) aaa bbb ccc)
(deftest vector-predicate.7
  (let ((x (make-vector-predicate-7 :aaa 10 :bbb 20 :ccc 30))
        (y (make-vector-predicate-6)))
    (values
      (vector-predicate-7-p x)
      (vector-predicate-7-p y)))
  t nil)

(deftest vector-predicate.8
  (vector-predicate-7-p 100)
  nil)


;;
;;  change
;;

;;  clos
(deftest change-clos-predicate.1
  (progn
    (defstruct change-clos-predicate-1)
    (change-clos-predicate-1-p
      (make-change-clos-predicate-1)))
  t)

(deftest change-clos-predicate.2
  (progn
    (defstruct change-clos-predicate-2)
    (defstruct change-clos-predicate-2)
    (change-clos-predicate-2-p
      (make-change-clos-predicate-2)))
  t)

(deftest change-clos-predicate.3
  (progn
    (defstruct (change-clos-predicate-3 (:predicate change-clos-predicate-4)))
    (defstruct (change-clos-predicate-3 (:predicate change-clos-predicate-5)))
    (values
      (fboundp 'change-clos-predicate-4)
      (fboundp 'change-clos-predicate-5)
      (change-clos-predicate-5
        (make-change-clos-predicate-3))))
  nil t t)

(deftest change-clos-predicate.4
  (progn
    (defstruct (change-clos-predicate-6 (:predicate change-clos-predicate-7)))
    (defstruct (change-clos-predicate-6 (:predicate nil)))
    (values
      (fboundp 'change-clos-predicate-7)
      (fboundp 'change-clos-predicate-6-p)))
  nil nil)


;;  list
(deftest change-list-predicate.1
  (progn
    (defstruct (change-list-predicate-1 (:type list) :named))
    (change-list-predicate-1-p
      (make-change-list-predicate-1)))
  t)

(deftest change-list-predicate.2
  (progn
    (defstruct (change-list-predicate-2 (:type list) :named))
    (defstruct (change-list-predicate-2 (:type list) :named))
    (change-list-predicate-2-p
      (make-change-list-predicate-2)))
  t)

(deftest change-list-predicate.3
  (progn
    (defstruct (change-list-predicate-3
                 (:predicate change-list-predicate-4)
                 (:type list) :named))
    (defstruct (change-list-predicate-3
                 (:predicate change-list-predicate-5)
                 (:type list) :named))
    (values
      (fboundp 'change-list-predicate-4)
      (fboundp 'change-list-predicate-5)
      (change-list-predicate-5
        (make-change-list-predicate-3))))
  nil t t)

(deftest change-list-predicate.4
  (progn
    (defstruct (change-list-predicate-6
                 (:predicate change-list-predicate-7)
                 (:type list) :named))
    (defstruct (change-list-predicate-6
                 (:predicate nil)
                 (:type list) :named))
    (values
      (fboundp 'change-list-predicate-7)
      (fboundp 'change-list-predicate-6-p)))
  nil nil)


;;  vector
(deftest change-vector-predicate.1
  (progn
    (defstruct (change-vector-predicate-1 (:type vector) :named))
    (change-vector-predicate-1-p
      (make-change-vector-predicate-1)))
  t)

(deftest change-vector-predicate.2
  (progn
    (defstruct (change-vector-predicate-2 (:type vector) :named))
    (defstruct (change-vector-predicate-2 (:type vector) :named))
    (change-vector-predicate-2-p
      (make-change-vector-predicate-2)))
  t)

(deftest change-vector-predicate.3
  (progn
    (defstruct (change-vector-predicate-3
                 (:predicate change-vector-predicate-4)
                 (:type vector) :named))
    (defstruct (change-vector-predicate-3
                 (:predicate change-vector-predicate-5)
                 (:type vector) :named))
    (values
      (fboundp 'change-vector-predicate-4)
      (fboundp 'change-vector-predicate-5)
      (change-vector-predicate-5
        (make-change-vector-predicate-3))))
  nil t t)

(deftest change-vector-predicate.4
  (progn
    (defstruct (change-vector-predicate-6
                 (:predicate change-vector-predicate-7)
                 (:type vector) :named))
    (defstruct (change-vector-predicate-6
                 (:predicate nil)
                 (:type vector) :named))
    (values
      (fboundp 'change-vector-predicate-7)
      (fboundp 'change-vector-predicate-6-p)))
  nil nil)

