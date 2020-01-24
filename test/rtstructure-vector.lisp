;;
;;  ANSI COMMON LISP: 8. Structures
;;

;; empty
(deftest vector-empty.1
  (defstruct (vector-empty-1 (:type vector)))
  vector-empty-1)

(deftest vector-empty.2
  (handler-case
    (defstruct (vector-empty-1 (:type vector)))
    (warning () :warning))
  :warning)

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

;; include-check
(deftest-error vector-include-check.1
  (eval '(defstruct (vector-include-check-1 (:type vector) :include))))

(deftest-error vector-include-check.2
  (eval '(defstruct (vector-include-check-2 (:type vector) (:include)))))

(deftest vector-include-check.3
  (defstruct (vector-include-check-3 (:type vector) (:include vector-empty-1)))
  vector-include-check-3)

(deftest vector-include-check.4
  (defstruct (vector-include-check-4
               (:type vector)
               (:include vector-empty-3 aaa bbb))
    ddd eee)
  vector-include-check-4)

(deftest-error vector-include-check.5
  (defstruct (vector-include-check-5
               (:type vector)
               (:include vector-empty-3 aaa bbb aaa))
    ddd eee))

(deftest-error vector-include-check.6
  (defstruct (vector-include-check-6
               (:type vector)
               (:include vector-empty-3 aaa bbb))
    ccc ddd eee))

(deftest vector-include-check.7
  (defstruct (vector-include-check-7
               (:type vector)
               :named
               (:include vector-empty-3 aaa bbb))
    ddd eee)
  vector-include-check-7)

(deftest vector-include-check.8
  (defstruct (vector-include-check-8
               (:type vector)
               :named
               (:include vector-empty-named aaa bbb))
    ddd eee)
  vector-include-check-8)

(defstruct vector-include-error-clos aaa)
(defstruct (vector-include-error-list (:type list)) aaa)
(defstruct (vector-include-error-vector (:type vector)) aaa)

(deftest vector-include-error.1
  (defstruct (vector-include-error-1
               (:include vector-include-error-clos))
    bbb ccc)
  vector-include-error-1)

(deftest-error vector-include-error.2
  (defstruct (vector-include-error-2
               (:type list)
               (:include vector-include-error-clos))
    bbb ccc))

(deftest-error vector-include-error.3
  (defstruct (vector-include-error-3
               (:type vector)
               (:include vector-include-error-clos))
    bbb ccc))

(deftest-error vector-include-error.4
  (defstruct (vector-include-error-4
               (:include vector-include-error-list))))

(deftest vector-include-error.5
  (defstruct (vector-include-error-5
               (:type list)
               (:include vector-include-error-list)))
  vector-include-error-5)

(deftest-error vector-include-error.6
  (defstruct (vector-include-error-6
               (:type vector)
               (:include vector-include-error-list))))

(deftest-error vector-include-error.7
  (defstruct (vector-include-error-7
               (:include vector-include-error-vector))))

(deftest-error vector-include-error.8
  (defstruct (vector-include-error-8
               (:type list)
               (:include vector-include-error-vector))))

(deftest vector-include-error.9
  (defstruct (vector-include-error-9
               (:type vector)
               (:include vector-include-error-vector)))
  vector-include-error-9)

;; predicate-check
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

;; include
(deftest vector-include.1
  (defstruct (vector-include-1 (:include vector-empty-1) (:type vector)))
  vector-include-1)

(deftest-error vector-include.2
  (defstruct (vector-include-2 (:include no-such-structure) (:type vector))))

(deftest-error vector-include.3
  (defstruct (vector-include-2 (:include standard-class) (:type vector))))

;; include-slot
(deftest vector-check-include-slots.1
  (defstruct (vector-check-include-slots-1 (:include vector-empty-1) (:type vector))
    aaa bbb ccc)
  vector-check-include-slots-1)

(deftest vector-check-include-slots.2
  (defstruct (vector-check-include-slots-2
               (:include vector-check-include-slots-1)
               (:type vector))
    ddd eee fff)
  vector-check-include-slots-2)

(deftest-error vector-check-include-slots.3
  (defstruct (vector-check-include-slots-3
               (:include vector-check-include-slots-1) (:type vector))
    ddd aaa fff))

;; include-arguemnts
(defstruct (vector-check-include-arguments (:type vector))
  aaa bbb
  (ccc 10 :type integer)
  (ddd 20.0 :type float :read-only t))

(deftest vector-check-include-arguments.1
  (defstruct (vector-check-include-arguments-1
               (:include vector-check-include-arguments)
               (:type vector)))
  vector-check-include-arguments-1)

(deftest vector-check-include-arguments.2
  (defstruct (vector-check-include-arguments-2
               (:include vector-check-include-arguments
                         aaa bbb ccc ddd)
               (:type vector))
    eee fff ggg)
  vector-check-include-arguments-2)

(deftest vector-check-include-arguments.3
  (defstruct (vector-check-include-arguments-3
               (:include vector-check-include-arguments
                         (aaa 10 :type integer :read-only t))
               (:type vector)))
  vector-check-include-arguments-3)

(deftest vector-check-include-arguments.4
  (defstruct (vector-check-include-arguments-4
               (:include vector-check-include-arguments
                         (ccc 10 :type integer))
               (:type vector)))
  vector-check-include-arguments-4)

(deftest vector-check-include-arguments.5
  (defstruct (vector-check-include-arguments-5
               (:include vector-check-include-arguments
                         (ccc 10 :type fixnum))
               (:type vector)))
  vector-check-include-arguments-5)

(deftest-error vector-check-include-arguments.6
  (defstruct (vector-check-include-arguments-6
               (:include vector-check-include-arguments
                         (ccc 10 :type real))
               (:type vector))))

(deftest vector-check-include-arguments.7
  (defstruct (vector-check-include-arguments-7
               (:include vector-check-include-arguments
                         (ddd 20))
               (:type vector)))
  vector-check-include-arguments-7)

(deftest vector-check-include-arguments.8
  (defstruct (vector-check-include-arguments-8
               (:include vector-check-include-arguments
                         (ddd 20 :read-only t))
               (:type vector)))
  vector-check-include-arguments-8)

(deftest-error vector-check-include-arguments.9
  (defstruct (vector-check-include-arguments-9
               (:include vector-check-include-arguments
                         (ddd 20 :read-only nil))
               (:type vector))))

;; named
(defstruct (vector-include-named-a (:type vector))
  (aa 10) (bb 20))
(defstruct (vector-include-named-b
             (:include vector-include-named-a)
             (:type vector) :named)
  (cc 30) (dd 40) (ee 50))

(defstruct (vector-include-named-1
             (:include vector-include-named-b)
             (:type vector))
  (ff 111))
(deftest vector-include-named.1
  (make-vector-include-named-1)
  #(10 20 nil 30 40 50 111))

(deftest vector-include-named.2
  (let ((x (make-vector-include-named-1)))
    (values
      (vector-include-named-1-aa x)
      (vector-include-named-1-bb x)
      (vector-include-named-1-cc x)
      (vector-include-named-1-dd x)
      (vector-include-named-1-ee x)
      (vector-include-named-1-ff x)))
  10 20 30 40 50 111)

(defstruct (vector-include-named-3
             (:include vector-include-named-b)
             (:type vector) :named)
  (ff 111) (gg 222) (hh 333) (ii 444))
(deftest vector-include-named.3
  (make-vector-include-named-3)
  #(10 20 nil 30 40 50 vector-include-named-3 111 222 333 444))

(deftest vector-include-named.4
  (let ((x (make-vector-include-named-3)))
    (values
      (vector-include-named-3-aa x)
      (vector-include-named-3-bb x)
      (vector-include-named-3-cc x)
      (vector-include-named-3-dd x)
      (vector-include-named-3-ee x)
      (vector-include-named-3-ff x)
      (vector-include-named-3-gg x)
      (vector-include-named-3-hh x)
      (vector-include-named-3-ii x)))
  10 20 30 40 50 111 222 333 444)

;; slots-value
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

;; instance
(defstruct (vector-structure1 (:type vector)))
(defstruct (vector-structure2 (:type vector) (:include vector-structure1)))
(defstruct (vector-structure3 (:type vector) (:include vector-structure2) :named))

(deftest vector-structure.1
  (null (find-class 'vector-structure1))
  nil)

(deftest vector-structure.2
  (typep (find-class 'vector-structure1) 'structure-class)
  t)

(deftest vector-structure.3
  (typep (find-class 'vector-structure1) 'structure-object)
  nil)

(deftest vector-structure.4
  (typep (find-class 'vector-structure1) t)
  t)

(deftest vector-structure.5
  (typep (find-class 'vector-structure1) 'vector-structure1)
  nil)

(deftest vector-structure.6
  (typep (find-class 'vector-structure1) 'vector-structure2)
  nil)

(deftest vector-structure.7
  (typep (find-class 'vector-structure2) 'vector-structure1)
  nil)

(deftest vector-structure.8
  (subtypep 'vector-structure1 'vector-structure1)
  t t)

(deftest vector-structure.9
  (subtypep 'vector-structure2 'vector-structure1)
  t t)

(deftest vector-structure.10
  (subtypep 'vector-structure1 'vector-structure2)
  nil t)

(deftest vector-structure.11
  (subtypep 'vector-structure3 'vector-structure1)
  t t)

(deftest vector-structure.12
  (subtypep 'vector-structure1 'vector-structure3)
  nil t)

(deftest vector-structure.13
  (subtypep 'vector-structure1 'structure-class)
  nil t)

(deftest vector-structure.14
  (subtypep 'vector-structure1 'structure-object)
  t t)

(deftest vector-structure.15
  (subtypep 'vector-structure1 t)
  t t)

(deftest vector-instance.1
  (typep (make-vector-structure1) 'vector)
  t)

;; slot-access
(defstruct (vector-slot-access-1 (:type vector)) aaa bbb)
(deftest vector-slot-access.1
  (let ((inst (make-vector-slot-access-1)))
    (vector-slot-access-1-aaa inst))
  nil)

(deftest vector-slot-access.2
  (let ((inst (make-vector-slot-access-1)))
    (setf (vector-slot-access-1-aaa inst) 100)
    (vector-slot-access-1-aaa inst))
  100)

(deftest vector-slot-access.3
  (let ((inst (make-vector-slot-access-1)))
    (setf (vector-slot-access-1-aaa inst) 100)
    (setf (vector-slot-access-1-bbb inst) 200)
    (values
      (vector-slot-access-1-aaa inst)
      (vector-slot-access-1-bbb inst)))
  100 200)

(defstruct (vector-slot-access-4 (:type vector) :named) aaa bbb)
(deftest vector-slot-access.4
  (let ((inst (make-vector-slot-access-4)))
    (vector-slot-access-4-aaa inst))
  nil)

(deftest vector-slot-access.5
  (let ((inst (make-vector-slot-access-4)))
    (setf (vector-slot-access-4-aaa inst) 100)
    (vector-slot-access-4-aaa inst))
  100)

(deftest vector-slot-access.6
  (let ((inst (make-vector-slot-access-4)))
    (setf (vector-slot-access-4-aaa inst) 100)
    (setf (vector-slot-access-4-bbb inst) 200)
    (values
      (vector-slot-access-4-aaa inst)
      (vector-slot-access-4-bbb inst)))
  100 200)

(defstruct (vector-slot-access-2
             (:type vector) (:include vector-slot-access-1))
  ccc ddd)
(deftest vector-slot-access-include.1
  (let ((inst (make-vector-slot-access-2)))
    (values
      (vector-slot-access-2-aaa inst)
      (vector-slot-access-2-bbb inst)
      (vector-slot-access-2-ccc inst)
      (vector-slot-access-2-ddd inst)))
  nil nil nil nil)

(deftest vector-slot-access-include.2
  (let ((inst (make-vector-slot-access-2)))
    (setf (vector-slot-access-2-aaa inst) 100)
    (setf (vector-slot-access-2-bbb inst) 200)
    (setf (vector-slot-access-2-ccc inst) 300)
    (setf (vector-slot-access-2-ddd inst) 400)
    (values
      (vector-slot-access-2-aaa inst)
      (vector-slot-access-2-bbb inst)
      (vector-slot-access-2-ccc inst)
      (vector-slot-access-2-ddd inst)))
  100 200 300 400)

(deftest vector-slot-access-include.4
  (let ((inst (make-vector-slot-access-2)))
    (setf (vector-slot-access-2-aaa inst) 100)
    (setf (vector-slot-access-2-bbb inst) 200)
    (setf (vector-slot-access-2-ccc inst) 300)
    (setf (vector-slot-access-2-ddd inst) 400)
    (values
      (vector-slot-access-1-aaa inst)  ;; 1
      (vector-slot-access-1-bbb inst)  ;; 1
      (vector-slot-access-2-ccc inst)
      (vector-slot-access-2-ddd inst)))
  100 200 300 400)

(deftest-error vector-slot-access-include.5
  (let ((inst (make-vector-slot-access-1)))
    (vector-slot-access-2-aaa inst)))

(defstruct (vector-slot-access-readonly-1 (:type vector))
  (aaa 100 :read-only t)
  (bbb 200))

(deftest vector-slot-access-readonly.1
  (let ((inst (make-vector-slot-access-readonly-1)))
    (values
      (vector-slot-access-readonly-1-aaa inst)
      (fboundp '(setf vector-slot-access-readonly-1-aaa))))
  100 nil)

(defstruct (vector-slot-access-readonly-2
             (:type vector) (:include vector-slot-access-readonly-1)))
(deftest vector-slot-access-readonly.2
  (let ((inst (make-vector-slot-access-readonly-2)))
    (values
      (vector-slot-access-readonly-2-aaa inst)
      (fboundp '(setf vector-slot-access-readonly-2-aaa))))
  100 nil)

(defstruct (vector-slot-access-readonly-3
             (:type vector)
             (:include vector-slot-access-readonly-1
                       (aaa 300 :read-only t))))
(deftest vector-slot-access-readonly.3
  (let ((inst (make-vector-slot-access-readonly-3)))
    (values
      (vector-slot-access-readonly-3-aaa inst)
      (fboundp '(setf vector-slot-access-readonly-3-aaa))))
  300 nil)

(deftest-error vector-slot-access-readonly.4
  (defstruct (vector-slot-access-readonly-4
               (:type vector) :named
               (:include vector-slot-access-readonly-1
                         (aaa 300 :read-only nil)))))

(defstruct (vector-slot-access-readonly-5
             (:type vector)
             (:include vector-slot-access-readonly-1
                       (bbb 400 :read-only t))))
(deftest vector-slot-access-readonly.5
  (let ((inst (make-vector-slot-access-readonly-5)))
    (values
      (vector-slot-access-readonly-5-bbb inst)
      (fboundp '(setf vector-slot-access-readonly-5-bbb))))
  400 nil)

(defstruct (vector-slot-access-type-1 (:type vector))
  (aaa 100 :type integer)
  (bbb "Hello" :type string))

(deftest vector-slot-access-type.1
  (let ((inst (make-vector-slot-access-type-1)))
    (setf (vector-slot-access-type-1-aaa inst) 200)
    (setf (vector-slot-access-type-1-bbb inst) "ZZZ")
    (values
      (vector-slot-access-type-1-aaa inst)
      (vector-slot-access-type-1-bbb inst)))
  200 "ZZZ")

(defstruct (vector-slot-access-type-2
             (:type vector) (:include vector-slot-access-type-1 aaa)))
(deftest vector-slot-access-type.2
  (let ((inst (make-vector-slot-access-type-2)))
    (setf (vector-slot-access-type-2-aaa inst) 200)
    (setf (vector-slot-access-type-2-bbb inst) "ZZZ")
    (values
      (vector-slot-access-type-2-aaa inst)
      (vector-slot-access-type-2-bbb inst)))
  200 "ZZZ")

(defstruct (vector-slot-access-type-3
             (:type vector)
             (:include vector-slot-access-type-1
                       (aaa 300 :type fixnum))))
(deftest vector-slot-access-type.3
  (let ((inst (make-vector-slot-access-type-3)))
    (setf (vector-slot-access-type-3-aaa inst) 400)
    (setf (vector-slot-access-type-3-bbb inst) "ZZZ")
    (values
      (vector-slot-access-type-3-aaa inst)
      (vector-slot-access-type-3-bbb inst)))
  400 "ZZZ")

(deftest-error vector-slot-access-type.4
  (defstruct (vector-slot-access-type-4
               (:type vector)
               (:include vector-slot-access-type-1
                         (aaa 300 :type real)))))

;; conc-name
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

;; copier
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

;; predicate
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

;; constructor
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

;; boa
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

;; print-object
(deftest-error vector-print-object.1
  (defstruct (vector-print-object-1 (:type vector) :named :print-object)))

(deftest-error vector-print-object.2
  (defstruct (vector-print-object-2
               (:type vector) :named
               (:print-object vector-print-object-2-call))))

;; print-function
(deftest-error vector-print-function.1
  (defstruct (vector-print-function-1 (:type vector) :named :print-function)))

(deftest-error vector-print-function.2
  (defstruct (vector-print-function-2
               (:type vector) :named
               (:print-function vector-print-function-2-call))))

;; make-instance
(defstruct (vector-make-instance-structure (:type vector)) aaa bbb)
(deftest vector-make-instance-structure.1
  (let ((inst (make-instance 'vector-make-instance-structure :aaa 100)))
    (values
      (vector-make-instance-structure-aaa inst)
      (vector-make-instance-structure-bbb inst)))
  100 nil)

;; vector-type
(defstruct (vector-type-a (:type (vector symbol))) aaa)
(deftest-error vector-type.1
  (defstruct (vector-type-1 (:type (vector integer))
                            (:include vector-type-a))))

(defstruct (vector-type-b (:type (vector integer))) aaa)
(deftest-error vector-type.2
  (defstruct (vector-type-2 (:type (vector real))
                            (:include vector-type-b))))

(deftest vector-type.3
  (defstruct (vector-type-3 (:type (vector fixnum))
                            (:include vector-type-b)))
  vector-type-3)

(deftest vector-type.4
  (let ((inst (make-vector-type-a)))
    (setf (vector-type-a-aaa inst) 'hello)
    (vector-type-a-aaa inst))
  hello)

(deftest-error vector-type.5
  (let ((inst (make-vector-type-a)))
    (setf (vector-type-a-aaa inst) 100)))

;; slot-value
(defstruct (vector-slot-value (:type vector) :named) aaa bbb)
(deftest-error vector-slot-value.1
  (slot-value (make-vector-slot-value) 'aaa))

(deftest-error vector-slot-value.2
  (slot-boundp (make-vector-slot-value) 'aaa))

(deftest-error vector-slot-value.3
  (slot-exists-p (make-vector-slot-value) 'aaa))

(deftest-error vector-slot-value.4
  (slot-makunbound (make-vector-slot-value) 'aaa))

;; array access
(defstruct (vector-array-access1 (:type vector)) aaa bbb)
(deftest vector-array-access.1
  (let ((x (make-vector-array-access1 :aaa 10 :bbb 20)))
    (values
      (vector-array-access1-aaa x)
      (vector-array-access1-bbb x)))
  10 20)

(deftest vector-array-access.2
  (let ((x #(30 40)))
    (values
      (vector-array-access1-aaa x)
      (vector-array-access1-bbb x)))
  30 40)

(deftest vector-array-access.3
  (let ((x (make-array 2 :initial-contents '(50 60))))
    (values
      (vector-array-access1-aaa x)
      (vector-array-access1-bbb x)))
  50 60)

(defstruct (vector-array-access2 (:type vector) :named) aaa bbb)
(deftest vector-array-access.4
  (let ((x (make-vector-array-access2 :aaa 10 :bbb 20)))
    (values
      (vector-array-access2-aaa x)
      (vector-array-access2-bbb x)))
  10 20)

(deftest vector-array-access.5
  (let ((x #(vector-array-access2 30 40)))
    (values
      (vector-array-access2-aaa x)
      (vector-array-access2-bbb x)))
  30 40)

(deftest vector-array-access.6
  (let ((x (make-array 3 :initial-contents '(vector-array-access2 50 60))))
    (values
      (vector-array-access2-aaa x)
      (vector-array-access2-bbb x)))
  50 60)

(deftest vector-array-access.7
  (let ((x (make-array 2 :initial-contents '(50 60))))
    (setf (vector-array-access1-aaa x) 70)
    (setf (vector-array-access1-bbb x) 80)
    (values
      (vector-array-access1-aaa x)
      (vector-array-access1-bbb x)))
  70 80)

(deftest vector-array-access.8
  (let ((x (make-array 3 :initial-contents '(vector-array-access2 50 60))))
    (setf (vector-array-access2-aaa x) 70)
    (setf (vector-array-access2-bbb x) 80)
    (values
      (vector-array-access2-aaa x)
      (vector-array-access2-bbb x)))
  70 80)

