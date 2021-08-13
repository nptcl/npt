;;
;;  ANSI COMMON LISP: 8. Structures
;;

;; instance
(defstruct (list-structure1 (:type list)))
(defstruct (list-structure2 (:type list) (:include list-structure1)))
(defstruct (list-structure3 (:type list) (:include list-structure2) :named))

(deftest list-structure.1
  (null (find-class 'list-structure1))
  nil)

(deftest list-structure.2
  (typep (find-class 'list-structure1) 'structure-class)
  t)

(deftest list-structure.3
  (typep (find-class 'list-structure1) 'structure-object)
  nil)

(deftest list-structure.4
  (typep (find-class 'list-structure1) t)
  t)

(deftest list-structure.5
  (typep (find-class 'list-structure1) 'list-structure1)
  nil)

(deftest list-structure.6
  (typep (find-class 'list-structure1) 'list-structure2)
  nil)

(deftest list-structure.7
  (typep (find-class 'list-structure2) 'list-structure1)
  nil)

(deftest list-structure.8
  (subtypep 'list-structure1 'list-structure1)
  t t)

(deftest list-structure.9
  (subtypep 'list-structure2 'list-structure1)
  t t)

(deftest list-structure.10
  (subtypep 'list-structure1 'list-structure2)
  nil t)

(deftest list-structure.11
  (subtypep 'list-structure3 'list-structure1)
  t t)

(deftest list-structure.12
  (subtypep 'list-structure1 'list-structure3)
  nil t)

(deftest list-structure.13
  (subtypep 'list-structure1 'structure-class)
  nil t)

(deftest list-structure.14
  (subtypep 'list-structure1 'structure-object)
  t t)

(deftest list-structure.15
  (subtypep 'list-structure1 t)
  t t)

(deftest list-instance.1
  (typep (make-list-structure1) 'list)
  t)

;; slot-access
(defstruct (list-slot-access-1 (:type list)) aaa bbb)
(deftest list-slot-access.1
  (let ((inst (make-list-slot-access-1)))
    (list-slot-access-1-aaa inst))
  nil)

(deftest list-slot-access.2
  (let ((inst (make-list-slot-access-1)))
    (setf (list-slot-access-1-aaa inst) 100)
    (list-slot-access-1-aaa inst))
  100)

(deftest list-slot-access.3
  (let ((inst (make-list-slot-access-1)))
    (setf (list-slot-access-1-aaa inst) 100)
    (setf (list-slot-access-1-bbb inst) 200)
    (values
      (list-slot-access-1-aaa inst)
      (list-slot-access-1-bbb inst)))
  100 200)

(defstruct (list-slot-access-4 (:type list) :named) aaa bbb)
(deftest list-slot-access.4
  (let ((inst (make-list-slot-access-4)))
    (list-slot-access-4-aaa inst))
  nil)

(deftest list-slot-access.5
  (let ((inst (make-list-slot-access-4)))
    (setf (list-slot-access-4-aaa inst) 100)
    (list-slot-access-4-aaa inst))
  100)

(deftest list-slot-access.6
  (let ((inst (make-list-slot-access-4)))
    (setf (list-slot-access-4-aaa inst) 100)
    (setf (list-slot-access-4-bbb inst) 200)
    (values
      (list-slot-access-4-aaa inst)
      (list-slot-access-4-bbb inst)))
  100 200)

(defstruct (list-slot-access-2
             (:type list) (:include list-slot-access-1))
  ccc ddd)
(deftest list-slot-access-include.1
  (let ((inst (make-list-slot-access-2)))
    (values
      (list-slot-access-2-aaa inst)
      (list-slot-access-2-bbb inst)
      (list-slot-access-2-ccc inst)
      (list-slot-access-2-ddd inst)))
  nil nil nil nil)

(deftest list-slot-access-include.2
  (let ((inst (make-list-slot-access-2)))
    (setf (list-slot-access-2-aaa inst) 100)
    (setf (list-slot-access-2-bbb inst) 200)
    (setf (list-slot-access-2-ccc inst) 300)
    (setf (list-slot-access-2-ddd inst) 400)
    (values
      (list-slot-access-2-aaa inst)
      (list-slot-access-2-bbb inst)
      (list-slot-access-2-ccc inst)
      (list-slot-access-2-ddd inst)))
  100 200 300 400)

(deftest list-slot-access-include.4
  (let ((inst (make-list-slot-access-2)))
    (setf (list-slot-access-2-aaa inst) 100)
    (setf (list-slot-access-2-bbb inst) 200)
    (setf (list-slot-access-2-ccc inst) 300)
    (setf (list-slot-access-2-ddd inst) 400)
    (values
      (list-slot-access-1-aaa inst)  ;; 1
      (list-slot-access-1-bbb inst)  ;; 1
      (list-slot-access-2-ccc inst)
      (list-slot-access-2-ddd inst)))
  100 200 300 400)

(deftest-error list-slot-access-include.5
  (let ((inst (make-list-slot-access-1)))
    (list-slot-access-2-aaa inst)))

(defstruct (list-slot-access-readonly-1 (:type list))
  (aaa 100 :read-only t)
  (bbb 200))

(deftest list-slot-access-readonly.1
  (let ((inst (make-list-slot-access-readonly-1)))
    (values
      (list-slot-access-readonly-1-aaa inst)
      (fboundp '(setf list-slot-access-readonly-1-aaa))))
  100 nil)

(defstruct (list-slot-access-readonly-2
             (:type list) (:include list-slot-access-readonly-1)))
(deftest list-slot-access-readonly.2
  (let ((inst (make-list-slot-access-readonly-2)))
    (values
      (list-slot-access-readonly-2-aaa inst)
      (fboundp '(setf list-slot-access-readonly-2-aaa))))
  100 nil)

(defstruct (list-slot-access-readonly-3
             (:type list)
             (:include list-slot-access-readonly-1
                       (aaa 300 :read-only t))))
(deftest list-slot-access-readonly.3
  (let ((inst (make-list-slot-access-readonly-3)))
    (values
      (list-slot-access-readonly-3-aaa inst)
      (fboundp '(setf list-slot-access-readonly-3-aaa))))
  300 nil)

(deftest-error list-slot-access-readonly.4
  (defstruct (list-slot-access-readonly-4
               (:type list) :named
               (:include list-slot-access-readonly-1
                         (aaa 300 :read-only nil)))))

(defstruct (list-slot-access-readonly-5
             (:type list)
             (:include list-slot-access-readonly-1
                       (bbb 400 :read-only t))))
(deftest list-slot-access-readonly.5
  (let ((inst (make-list-slot-access-readonly-5)))
    (values
      (list-slot-access-readonly-5-bbb inst)
      (fboundp '(setf list-slot-access-readonly-5-bbb))))
  400 nil)

(defstruct (list-slot-access-type-1 (:type list))
  (aaa 100 :type integer)
  (bbb "Hello" :type string))

(deftest list-slot-access-type.1
  (let ((inst (make-list-slot-access-type-1)))
    (setf (list-slot-access-type-1-aaa inst) 200)
    (setf (list-slot-access-type-1-bbb inst) "ZZZ")
    (values
      (list-slot-access-type-1-aaa inst)
      (list-slot-access-type-1-bbb inst)))
  200 "ZZZ")

(defstruct (list-slot-access-type-2
             (:type list) (:include list-slot-access-type-1 aaa)))
(deftest list-slot-access-type.2
  (let ((inst (make-list-slot-access-type-2)))
    (setf (list-slot-access-type-2-aaa inst) 200)
    (setf (list-slot-access-type-2-bbb inst) "ZZZ")
    (values
      (list-slot-access-type-2-aaa inst)
      (list-slot-access-type-2-bbb inst)))
  200 "ZZZ")

(defstruct (list-slot-access-type-3
             (:type list)
             (:include list-slot-access-type-1
                       (aaa 300 :type fixnum))))
(deftest list-slot-access-type.3
  (let ((inst (make-list-slot-access-type-3)))
    (setf (list-slot-access-type-3-aaa inst) 400)
    (setf (list-slot-access-type-3-bbb inst) "ZZZ")
    (values
      (list-slot-access-type-3-aaa inst)
      (list-slot-access-type-3-bbb inst)))
  400 "ZZZ")

(deftest-error list-slot-access-type.4
  (defstruct (list-slot-access-type-4
               (:type list)
               (:include list-slot-access-type-1
                         (aaa 300 :type real)))))

;; conc-name
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

;; copier
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

;; constructor
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

;; boa
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

;; print-object
(deftest-error list-print-object.1
  (defstruct (list-print-object-1 (:type list) :named :print-object)))

(deftest-error list-print-object.2
  (defstruct (list-print-object-2
               (:type list) :named
               (:print-object list-print-object-2-call))))

;; print-function
(deftest-error list-print-function.1
  (defstruct (list-print-function-1 (:type list) :named :print-function)))

(deftest-error list-print-function.2
  (defstruct (list-print-function-2
               (:type list) :named
               (:print-function list-print-function-2-call))))

;; make-instance
(defstruct (list-make-instance-structure (:type list)) aaa bbb)
(deftest list-make-instance-structure.1
  (let ((inst (make-instance 'list-make-instance-structure :aaa 100)))
    (values
      (list-make-instance-structure-aaa inst)
      (list-make-instance-structure-bbb inst)))
  100 nil)

;; slot-value
(defstruct (list-slot-value (:type list) :named) aaa bbb)
(deftest-error list-slot-value.1
  (slot-value (make-list-slot-value) 'aaa))

(deftest-error list-slot-value.2
  (slot-boundp (make-list-slot-value) 'aaa))

(deftest-error list-slot-value.3
  (slot-exists-p (make-list-slot-value) 'aaa))

(deftest-error list-slot-value.4
  (slot-makunbound (make-list-slot-value) 'aaa))

