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
  (defstruct clos-empty-1)  ;; change
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
(deftest change-clos-empty.1
  (progn
    (defstruct change-clos-empty-1)
    (defstruct change-clos-empty-1))
  change-clos-empty-1)

(deftest change-clos-empty.2
  (progn
    (defstruct change-clos-empty-2 aaa bbb ccc)
    (defstruct change-clos-empty-2 aaa bbb ccc))
  change-clos-empty-2)

(deftest-error change-clos-empty.3
  (progn
    (defstruct change-clos-empty-3 aaa bbb ccc)
    (defstruct change-clos-empty-3 aaa bbb)))

(deftest-error change-clos-empty.4
  (progn
    (defstruct change-clos-empty-4 aaa bbb)
    (defstruct change-clos-empty-4 aaa bbb ccc)))

(deftest change-clos-empty.5
  (let (x y)
    (defstruct change-clos-empty-5)
    (setq x (find-class 'change-clos-empty-5))
    (when (lisp-system:sysctl 'structure 'check 'change-clos-empty-5)
      (error "sysctl error, 1"))
    (defstruct change-clos-empty-5)
    (setq y (find-class 'change-clos-empty-5))
    (when (lisp-system:sysctl 'structure 'check 'change-clos-empty-5)
      (error "sysctl error, 2"))
    (eq x y))
  t)

(deftest change-clos-empty.6
  (let (x y)
    (defstruct change-clos-empty-6 aaa)
    (setq x (find-class 'change-clos-empty-6))
    (when (lisp-system:sysctl 'structure 'check 'change-clos-empty-6)
      (error "sysctl error, 1"))
    (defstruct change-clos-empty-6 :aaa)
    (setq y (find-class 'change-clos-empty-6))
    (when (lisp-system:sysctl 'structure 'check 'change-clos-empty-6)
      (error "sysctl error, 2"))
    (eq x y))
  t)

(deftest change-list-empty.1
  (progn
    (defstruct (change-list-empty-1 (:type list) :named))
    (defstruct (change-list-empty-1 (:type list) :named)))
  change-list-empty-1)

(deftest change-list-empty.2
  (progn
    (defstruct (change-list-empty-2 (:type list) :named) aaa bbb ccc)
    (defstruct (change-list-empty-2 (:type list) :named) aaa bbb ccc))
  change-list-empty-2)

(deftest-error change-list-empty.3
  (progn
    (defstruct (change-list-empty-3 (:type list) :named) aaa bbb ccc)
    (defstruct (change-list-empty-3 (:type list) :named) aaa bbb)))

(deftest-error change-list-empty.4
  (progn
    (defstruct (change-list-empty-4 (:type list) :named) aaa bbb)
    (defstruct (change-list-empty-4 (:type list) :named) aaa bbb ccc)))

(deftest change-list-empty.5
  (progn
    (defstruct (change-list-empty-5 (:type list) :named))
    (when (find-class 'change-list-empty-5 nil)
      (error "find-class error, 1"))
    (unless (lisp-system:sysctl 'structure 'check 'change-list-empty-5)
      (error "sysctl error, 2"))
    (defstruct (change-list-empty-5 (:type list) :named))
    (when (find-class 'change-list-empty-5 nil)
      (error "find-class error, 3"))
    (unless (lisp-system:sysctl 'structure 'check 'change-list-empty-5)
      (error "sysctl error, 4"))
    t)
  t)

(deftest change-list-empty.6
  (progn
    (defstruct (change-list-empty-6 (:type list) :named) aaa)
    (when (find-class 'change-list-empty-6 nil)
      (error "find-class error, 1"))
    (unless (lisp-system:sysctl 'structure 'check 'change-list-empty-6)
      (error "sysctl error, 2"))
    (defstruct (change-list-empty-6 (:type list) :named) :aaa)
    (when (find-class 'change-list-empty-6 nil)
      (error "find-class error, 3"))
    (unless (lisp-system:sysctl 'structure 'check 'change-list-empty-6)
      (error "sysctl error, 4"))
    t)
  t)

(deftest change-vector-empty.1
  (progn
    (defstruct (change-vector-empty-1 (:type vector) :named))
    (defstruct (change-vector-empty-1 (:type vector) :named)))
  change-vector-empty-1)

(deftest change-vector-empty.2
  (progn
    (defstruct (change-vector-empty-2 (:type vector) :named) aaa bbb ccc)
    (defstruct (change-vector-empty-2 (:type vector) :named) aaa bbb ccc))
  change-vector-empty-2)

(deftest-error change-vector-empty.3
  (progn
    (defstruct (change-vector-empty-3 (:type vector) :named) aaa bbb ccc)
    (defstruct (change-vector-empty-3 (:type vector) :named) aaa bbb)))

(deftest-error change-vector-empty.4
  (progn
    (defstruct (change-vector-empty-4 (:type vector) :named) aaa bbb)
    (defstruct (change-vector-empty-4 (:type vector) :named) aaa bbb ccc)))

(deftest change-vector-empty.5
  (progn
    (defstruct (change-vector-empty-5 (:type vector) :named))
    (when (find-class 'change-vector-empty-5 nil)
      (error "find-class error, 1"))
    (unless (lisp-system:sysctl 'structure 'check 'change-vector-empty-5)
      (error "sysctl error, 2"))
    (defstruct (change-vector-empty-5 (:type vector) :named))
    (when (find-class 'change-vector-empty-5 nil)
      (error "find-class error, 3"))
    (unless (lisp-system:sysctl 'structure 'check 'change-vector-empty-5)
      (error "sysctl error, 4"))
    t)
  t)

(deftest change-vector-empty.6
  (progn
    (defstruct (change-vector-empty-6 (:type vector) :named) aaa)
    (when (find-class 'change-vector-empty-6 nil)
      (error "find-class error, 1"))
    (unless (lisp-system:sysctl 'structure 'check 'change-vector-empty-6)
      (error "sysctl error, 2"))
    (defstruct (change-vector-empty-6 (:type vector) :named) :aaa)
    (when (find-class 'change-vector-empty-6 nil)
      (error "find-class error, 3"))
    (unless (lisp-system:sysctl 'structure 'check 'change-vector-empty-6)
      (error "sysctl error, 4"))
    t)
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


;;
;;  read-only
;;
;;  If a slot is read-only in the included structure,
;;  then it must also be so in the including structure.
;;
(deftest clos-read-only.1
  (progn
    (defstruct clos-read-only-1a
      (aaa 10 :read-only t))
    (defstruct (clos-read-only-1b (:include clos-read-only-1a)))
    (values
      (fboundp '(setf clos-read-only-1a-aaa))
      (fboundp '(setf clos-read-only-1b-aaa))))
  nil nil)

(deftest clos-read-only.2
  (progn
    (defstruct clos-read-only-2a
      (aaa 10 :read-only t))
    (defstruct (clos-read-only-2b
                 (:include clos-read-only-2a aaa)))
    (values
      (fboundp '(setf clos-read-only-2a-aaa))
      (fboundp '(setf clos-read-only-2b-aaa))))
  nil nil)

(deftest-error clos-read-only.3
  (progn
    (defstruct clos-read-only-3a
      (aaa 10 :read-only t))
    (defstruct (clos-read-only-3b
                 (:include clos-read-only-3a
                           (aaa 20 :read-only nil))))))

(deftest clos-read-only.4
  (progn
    (defstruct clos-read-only-4a
      (aaa 10 :read-only t))
    (defstruct (clos-read-only-4b
                 (:include clos-read-only-4a
                           (aaa 20 :read-only t))))
    (values
      (fboundp '(setf clos-read-only-4a-aaa))
      (fboundp '(setf clos-read-only-4b-aaa))))
  nil nil)

;;  list
(deftest list-read-only.1
  (progn
    (defstruct (list-read-only-1a (:type list))
      (aaa 10 :read-only t))
    (defstruct (list-read-only-1b
                 (:type list)
                 (:include list-read-only-1a)))
    (values
      (fboundp '(setf list-read-only-1a-aaa))
      (fboundp '(setf list-read-only-1b-aaa))))
  nil nil)

(deftest list-read-only.2
  (progn
    (defstruct (list-read-only-2a (:type list))
      (aaa 10 :read-only t))
    (defstruct (list-read-only-2b
                 (:type list)
                 (:include list-read-only-2a aaa)))
    (values
      (fboundp '(setf list-read-only-2a-aaa))
      (fboundp '(setf list-read-only-2b-aaa))))
  nil nil)

(deftest-error list-read-only.3
  (progn
    (defstruct (list-read-only-3a (:type list))
      (aaa 10 :read-only t))
    (defstruct (list-read-only-3b
                 (:type list)
                 (:include list-read-only-3a
                           (aaa 20 :read-only nil))))))

(deftest list-read-only.4
  (progn
    (defstruct (list-read-only-4a (:type list))
      (aaa 10 :read-only t))
    (defstruct (list-read-only-4b
                 (:type list)
                 (:include list-read-only-4a
                           (aaa 20 :read-only t))))
    (values
      (fboundp '(setf list-read-only-4a-aaa))
      (fboundp '(setf list-read-only-4b-aaa))))
  nil nil)

;;  vector
(deftest vector-read-only.1
  (progn
    (defstruct (vector-read-only-1a (:type vector))
      (aaa 10 :read-only t))
    (defstruct (vector-read-only-1b
                 (:type vector)
                 (:include vector-read-only-1a)))
    (values
      (fboundp '(setf vector-read-only-1a-aaa))
      (fboundp '(setf vector-read-only-1b-aaa))))
  nil nil)

(deftest vector-read-only.2
  (progn
    (defstruct (vector-read-only-2a (:type vector))
      (aaa 10 :read-only t))
    (defstruct (vector-read-only-2b
                 (:type vector)
                 (:include vector-read-only-2a aaa)))
    (values
      (fboundp '(setf vector-read-only-2a-aaa))
      (fboundp '(setf vector-read-only-2b-aaa))))
  nil nil)

(deftest-error vector-read-only.3
  (progn
    (defstruct (vector-read-only-3a (:type vector))
      (aaa 10 :read-only t))
    (defstruct (vector-read-only-3b
                 (:type vector)
                 (:include vector-read-only-3a
                           (aaa 20 :read-only nil))))))

(deftest vector-read-only.4
  (progn
    (defstruct (vector-read-only-4a (:type vector))
      (aaa 10 :read-only t))
    (defstruct (vector-read-only-4b
                 (:type vector)
                 (:include vector-read-only-4a
                           (aaa 20 :read-only t))))
    (values
      (fboundp '(setf vector-read-only-4a-aaa))
      (fboundp '(setf vector-read-only-4b-aaa))))
  nil nil)

