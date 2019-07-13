;;
;;  ANSI COMMON LISP: 8. Structures
;;

;; empty
(deftest empty.1
  (defstruct empty-1)
  empty-1)

(deftest empty.2
  (handler-case
    (defstruct empty-1)
    (warning () :warning))
  :warning)

(deftest empty.3
  (defstruct empty-3 aaa bbb ccc)
  empty-3)

(deftest-error empty.4
  (defstruct empty-4 aaa bbb ccc bbb))

(deftest-error empty.5
  (defstruct empty-5 aaa bbb ccc :aaa))

;; include-check
(deftest-error include-check.1
  (eval '(defstruct (include-check-1 :include))))

(deftest-error include-check.2
  (eval '(defstruct (include-check-2 (:include)))))

(deftest include-check.3
  (defstruct (include-check-3 (:include empty-1)))
  include-check-3)

(deftest include-check.4
  (defstruct (include-check-4
               (:include empty-3 aaa bbb))
    ddd eee)
  include-check-4)

(deftest-error include-check.5
  (defstruct (include-check-5
               (:include empty-3 aaa bbb aaa))
    ddd eee))

(deftest-error include-check.6
  (defstruct (include-check-6
               (:include empty-3 aaa bbb))
    ccc ddd eee))

;; predicate-check
(deftest predicate-check.1
  (defstruct (predicate-check-1 :predicate))
  predicate-check-1)

(deftest predicate-check.2
  (progn
    (defstruct (predicate-check-2 :predicate))
    (predicate-check-2-p 10))
  nil)

(deftest predicate-check.3
  (progn
    (defstruct (predicate-check-3 :predicate))
    (predicate-check-3-p
      (make-predicate-check-3)))
  t)

(deftest predicate-check.4
  (progn
    (defstruct (predicate-check-4 (:predicate)))
    (predicate-check-4-p
      (make-predicate-check-4)))
  t)

(deftest predicate-check.5
  (progn
    (defstruct (predicate-check-5
                 (:predicate hello-predicate-check-5)))
    (hello-predicate-check-5
      (make-predicate-check-5)))
  t)

(deftest predicate-check.6
  (progn
    (defstruct (predicate-check-6 (:predicate nil)))
    (fboundp 'predicate-check-6-p))
  nil)

(deftest predicate-check.7
  (defstruct (predicate-check-7 (:type list)))
  predicate-check-7)

(deftest-error predicate-check.8
  (defstruct (predicate-check-8 (:type list) :predicate)))

(deftest predicate-check.9
  (defstruct (predicate-check-9 (:type list) (:predicate nil)))
  predicate-check-9)

(deftest-error predicate-check.10
  (defstruct (predicate-check-10 (:type list) (:predicate hello))))

(deftest predicate-check.11
  (defstruct (predicate-check-11 (:type list) :named))
  predicate-check-11)

(deftest predicate-check.12
  (defstruct (predicate-check-12 (:type list) :named :predicate))
  predicate-check-12)

(deftest predicate-check.13
  (defstruct (predicate-check-13 (:type list) :named (:predicate nil)))
  predicate-check-13)

(deftest predicate-check.14
  (defstruct (predicate-check-14 (:type list) :named (:predicate hello)))
  predicate-check-14)

;; include
(deftest include.1
  (defstruct (include-1 (:include empty-1)))
  include-1)

(deftest-error include.2
  (defstruct (include-2 (:include no-such-structure))))

(deftest-error include.3
  (defstruct (include-2 (:include standard-class))))

;; include-slot
(deftest check-include-slots.1
  (defstruct (check-include-slots-1 (:include empty-1))
    aaa bbb ccc)
  check-include-slots-1)

(deftest check-include-slots.2
  (defstruct (check-include-slots-2
               (:include check-include-slots-1))
    ddd eee fff)
  check-include-slots-2)

(deftest-error check-include-slots.3
  (defstruct (check-include-slots-3
               (:include check-include-slots-1))
    ddd aaa fff))

;; include-arguemnts
(defstruct check-include-arguments
  aaa bbb
  (ccc 10 :type integer)
  (ddd 20.0 :type float :read-only t))

(deftest check-include-arguments.1
  (defstruct (check-include-arguments-1
               (:include check-include-arguments)))
  check-include-arguments-1)

(deftest check-include-arguments.2
  (defstruct (check-include-arguments-2
               (:include check-include-arguments
                         aaa bbb ccc ddd))
    eee fff ggg)
  check-include-arguments-2)

(deftest check-include-arguments.3
  (defstruct (check-include-arguments-3
               (:include check-include-arguments
                         (aaa 10 :type integer :read-only t))))
  check-include-arguments-3)

(deftest check-include-arguments.4
  (defstruct (check-include-arguments-4
               (:include check-include-arguments
                         (ccc 10 :type integer))))
  check-include-arguments-4)

(deftest check-include-arguments.5
  (defstruct (check-include-arguments-5
               (:include check-include-arguments
                         (ccc 10 :type fixnum))))
  check-include-arguments-5)

(deftest-error check-include-arguments.6
  (defstruct (check-include-arguments-6
               (:include check-include-arguments
                         (ccc 10 :type real)))))

(deftest check-include-arguments.7
  (defstruct (check-include-arguments-7
               (:include check-include-arguments
                         (ddd 20))))
  check-include-arguments-7)

(deftest check-include-arguments.8
  (defstruct (check-include-arguments-8
               (:include check-include-arguments
                         (ddd 20 :read-only t))))
  check-include-arguments-8)

(deftest-error check-include-arguments.9
  (defstruct (check-include-arguments-9
               (:include check-include-arguments
                         (ddd 20 :read-only nil)))))

;; slots-value
(deftest initialize-slot-value.1
  (defstruct initialize-slot-value-1
    aaa)
  initialize-slot-value-1)

(deftest initialize-slot-value.2
  (defstruct initialize-slot-value-2
    (aaa))
  initialize-slot-value-2)

(deftest initialize-slot-value.3
  (defstruct initialize-slot-value-3
    (ccc 10))
  initialize-slot-value-3)

(deftest initialize-slot-value.4
  (defstruct initialize-slot-value-4
    (ddd 20 :type integer))
  initialize-slot-value-4)

(deftest initialize-slot-value.5
  (defstruct initialize-slot-value-5
    (eee 30 :read-only t))
  initialize-slot-value-5)

(deftest initialize-slot-value.6
  (defstruct initialize-slot-value-6
    (fff 40 :read-only nil :type integer :type string :read-only t))
  initialize-slot-value-6)

;; instance
(defstruct structure1)
(defstruct (structure2 (:include structure1)))
(defstruct (structure3 (:include structure2)))

(deftest structure.1
  (null (find-class 'structure1))
  nil)

(deftest structure.2
  (typep (find-class 'structure1) 'structure-class)
  t)

(deftest structure.3
  (typep (find-class 'structure1) 'structure-object)
  nil)

(deftest structure.4
  (typep (find-class 'structure1) t)
  t)

(deftest structure.5
  (typep (find-class 'structure1) 'structure1)
  nil)

(deftest structure.6
  (typep (find-class 'structure1) 'structure2)
  nil)

(deftest structure.7
  (typep (find-class 'structure2) 'structure1)
  nil)

(deftest structure.8
  (subtypep 'structure1 'structure1)
  t t)

(deftest structure.9
  (subtypep 'structure2 'structure1)
  t t)

(deftest structure.10
  (subtypep 'structure1 'structure2)
  nil t)

(deftest structure.11
  (subtypep 'structure3 'structure1)
  t t)

(deftest structure.12
  (subtypep 'structure1 'structure3)
  nil t)

(deftest structure.13
  (subtypep 'structure1 'structure-class)
  nil t)

(deftest structure.14
  (subtypep 'structure1 'structure-object)
  t t)

(deftest structure.15
  (subtypep 'structure1 t)
  t t)

(defvar instance1 (make-structure1))
(defvar instance2 (make-structure2))
(defvar instance3 (make-structure3))

(deftest instance.1
  (typep instance1 'structure-object)
  t)

(deftest instance.2
  (typep instance1 'standard-object)
  nil)

(deftest instance.3
  (typep instance1 t)
  t)

(deftest instance.4
  (typep instance1 'structure1)
  t)

(deftest instance.5
  (typep instance1 'structure2)
  nil)

(deftest instance.6
  (typep instance2 'structure-object)
  t)

(deftest instance.7
  (typep instance2 't)
  t)

(deftest instance.8
  (typep instance2 'structure1)
  t)

(deftest instance.9
  (typep instance2 'structure2)
  t)

(deftest instance.10
  (typep instance2 'structure3)
  nil)

(deftest instance.11
  (typep instance3 'structure3)
  t)

(deftest instance.12
  (typep instance3 'structure1)
  t)

(deftest instance.13
  (typep instance1 'structure3)
  nil)

(deftest instance.14
  (< 3 (length (lisp-clos::class-precedence-list
                 (find-class 'structure3))))
  t)

(deftest instance.15
  (lisp-clos::class-name
    (find-class 'structure1))
  structure1)

;; slot-access
(defstruct slot-access-1 aaa bbb)
(deftest slot-access.1
  (let ((inst (make-slot-access-1)))
    (slot-access-1-aaa inst))
  nil)

(deftest slot-access.2
  (let ((inst (make-slot-access-1)))
    (setf (slot-access-1-aaa inst) 100)
    (slot-access-1-aaa inst))
  100)

(deftest slot-access.3
  (let ((inst (make-slot-access-1)))
    (setf (slot-access-1-aaa inst) 100)
    (setf (slot-access-1-bbb inst) 200)
    (values
      (slot-access-1-aaa inst)
      (slot-access-1-bbb inst)))
  100 200)

(deftest slot-access.4
  (let ((inst (make-slot-access-1)))
    (slot-value inst 'aaa))
  nil)

(deftest slot-access.5
  (let ((inst (make-slot-access-1)))
    (setf (slot-access-1-aaa inst) 100)
    (slot-value inst 'aaa))
  100)

(deftest slot-access.6
  (let ((inst (make-slot-access-1)))
    (setf (slot-value inst 'aaa) 200)
    (slot-access-1-aaa inst))
  200)

(deftest slot-access.7
  (let ((inst (make-slot-access-1)))
    (setf (slot-value inst 'aaa) 100)
    (setf (slot-value inst 'bbb) 200)
    (values
      (slot-access-1-aaa inst)
      (slot-access-1-bbb inst)
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  100 200 100 200)

(defstruct (slot-access-2 (:include slot-access-1)) ccc ddd)
(deftest slot-access-include.1
  (let ((inst (make-slot-access-2)))
    (values
      (slot-access-2-aaa inst)
      (slot-access-2-bbb inst)
      (slot-access-2-ccc inst)
      (slot-access-2-ddd inst)))
  nil nil nil nil)

(deftest slot-access-include.2
  (let ((inst (make-slot-access-2)))
    (setf (slot-access-2-aaa inst) 100)
    (setf (slot-access-2-bbb inst) 200)
    (setf (slot-access-2-ccc inst) 300)
    (setf (slot-access-2-ddd inst) 400)
    (values
      (slot-access-2-aaa inst)
      (slot-access-2-bbb inst)
      (slot-access-2-ccc inst)
      (slot-access-2-ddd inst)))
  100 200 300 400)

(deftest slot-access-include.3
  (let ((inst (make-slot-access-2)))
    (setf (slot-value inst 'aaa) 100)
    (setf (slot-access-2-bbb inst) 200)
    (setf (slot-value inst 'ccc) 300)
    (setf (slot-access-2-ddd inst) 400)
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-access-2-ccc inst)
      (slot-access-2-ddd inst)))
  100 200 300 400)

(deftest slot-access-include.4
  (let ((inst (make-slot-access-2)))
    (setf (slot-access-2-aaa inst) 100)
    (setf (slot-access-2-bbb inst) 200)
    (setf (slot-access-2-ccc inst) 300)
    (setf (slot-access-2-ddd inst) 400)
    (values
      (slot-access-1-aaa inst)  ;; 1
      (slot-access-1-bbb inst)  ;; 1
      (slot-access-2-ccc inst)
      (slot-access-2-ddd inst)))
  100 200 300 400)

(deftest-error slot-access-include.5
  (let ((inst (make-slot-access-1)))
    (slot-access-2-aaa inst)))

(defstruct slot-access-readonly-1
  (aaa 100 :read-only t)
  (bbb 200))

(deftest slot-access-readonly.1
  (let ((inst (make-slot-access-readonly-1)))
    (values
      (slot-access-readonly-1-aaa inst)
      (fboundp '(setf slot-access-readonly-1-aaa))))
  100 nil)

(defstruct (slot-access-readonly-2 (:include slot-access-readonly-1)))
(deftest slot-access-readonly.2
  (let ((inst (make-slot-access-readonly-2)))
    (values
      (slot-access-readonly-2-aaa inst)
      (fboundp '(setf slot-access-readonly-2-aaa))))
  100 nil)

(defstruct (slot-access-readonly-3
             (:include slot-access-readonly-1
                       (aaa 300 :read-only t))))
(deftest slot-access-readonly.3
  (let ((inst (make-slot-access-readonly-3)))
    (values
      (slot-access-readonly-3-aaa inst)
      (fboundp '(setf slot-access-readonly-3-aaa))))
  300 nil)

(deftest-error slot-access-readonly.4
  (defstruct (slot-access-readonly-4
               (:include slot-access-readonly-1
                         (aaa 300 :read-only nil)))))

(defstruct (slot-access-readonly-5
             (:include slot-access-readonly-1
                       (bbb 400 :read-only t))))
(deftest slot-access-readonly.5
  (let ((inst (make-slot-access-readonly-5)))
    (values
      (slot-access-readonly-5-bbb inst)
      (fboundp '(setf slot-access-readonly-5-bbb))))
  400 nil)

(defstruct slot-access-type-1
  (aaa 100 :type integer)
  (bbb "Hello" :type string))

(deftest slot-access-type.1
  (let ((inst (make-slot-access-type-1)))
    (setf (slot-access-type-1-aaa inst) 200)
    (setf (slot-access-type-1-bbb inst) "ZZZ")
    (values
      (slot-access-type-1-aaa inst)
      (slot-access-type-1-bbb inst)))
  200 "ZZZ")

(defstruct (slot-access-type-2 (:include slot-access-type-1 aaa)))
(deftest slot-access-type.2
  (let ((inst (make-slot-access-type-2)))
    (setf (slot-access-type-2-aaa inst) 200)
    (setf (slot-access-type-2-bbb inst) "ZZZ")
    (values
      (slot-access-type-2-aaa inst)
      (slot-access-type-2-bbb inst)))
  200 "ZZZ")

(defstruct (slot-access-type-3
             (:include slot-access-type-1
                       (aaa 300 :type fixnum))))
(deftest slot-access-type.3
  (let ((inst (make-slot-access-type-3)))
    (setf (slot-access-type-3-aaa inst) 400)
    (setf (slot-access-type-3-bbb inst) "ZZZ")
    (values
      (slot-access-type-3-aaa inst)
      (slot-access-type-3-bbb inst)))
  400 "ZZZ")

(deftest-error slot-access-type.4
  (defstruct (slot-access-type-4
               (:include slot-access-type-1
                         (aaa 300 :type real)))))

;; conc-name
(defstruct (conc-name-1 :conc-name) aaa)
(deftest conc-name.1
  (values
    (fboundp 'conc-name-1-aaa)
    (fboundp '(setf conc-name-1-aaa)))
  nil nil)

(defstruct (conc-name-2 (:conc-name)) aaa)
(deftest conc-name.2
  (values
    (fboundp 'conc-name-2-aaa)
    (fboundp '(setf conc-name-2-aaa)))
  nil nil)

(defstruct (conc-name-3 (:conc-name nil)) aaa)
(deftest conc-name.3
  (values
    (fboundp 'conc-name-3-aaa)
    (fboundp '(setf conc-name-3-aaa)))
  nil nil)

(defstruct (conc-name-4 (:conc-name hello-conc-name-4-)) aaa)
(deftest conc-name.4
  (values
    (fboundp 'conc-name-4-aaa)
    (fboundp 'hello-conc-name-4-aaa)
    (fboundp '(setf conc-name-4-aaa))
    (fboundp '(setf hello-conc-name-4-aaa)))
  nil t nil t)

(defstruct (conc-name-5 (:conc-name hello-conc-name-5-)
                        (:include conc-name-4))
  bbb ccc)
(deftest conc-name.5
  (let ((inst (make-conc-name-5 :aaa 10 :bbb 20 :ccc 30)))
    (values
      (hello-conc-name-5-aaa inst)
      (hello-conc-name-5-bbb inst)
      (hello-conc-name-5-ccc inst)))
  10 20 30)

(deftest conc-name.6
  (let ((inst (make-conc-name-5 :aaa 10 :bbb 20 :ccc 30)))
    (setf (hello-conc-name-5-aaa inst) 40)
    (setf (hello-conc-name-5-bbb inst) 50)
    (setf (hello-conc-name-5-ccc inst) 60)
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (slot-value inst 'ccc)))
  40 50 60)

(defstruct (conc-name-7) aaa)
(deftest conc-name.7
  (values
    (fboundp 'conc-name-7-aaa)
    (fboundp '(setf conc-name-7-aaa)))
  t t)

;; copier
(defstruct copier-1)
(deftest copier.1
  (fboundp 'copy-copier-1)
  t)

(defstruct (copier-2 :copier) aaa)
(deftest copier.2
  (fboundp 'copy-copier-2)
  t)

(defstruct (copier-3 (:copier)) aaa)
(deftest copier.3
  (fboundp 'copy-copier-3)
  t)

(defstruct (copier-4 (:copier nil)) aaa)
(deftest copier.4
  (fboundp 'copy-copier-4)
  nil)

(defstruct (copier-5 (:copier copier-5-hello)) aaa)
(deftest copier.5
  (fboundp 'copier-5-hello)
  t)

(defstruct copier-6 aaa bbb ccc)
(deftest copier.6
  (let* ((x (make-copier-6 :aaa 10 :bbb 20 :ccc 30))
         (y (copy-copier-6 x)))
    (values
      (eq x y)
      (copier-6-aaa y)
      (copier-6-bbb y)
      (copier-6-ccc y)))
  nil 10 20 30)

(deftest-error copier.7
  (copy-copier-6
    (make-copier-5)))

;; predicate
(defstruct predicate-1)
(deftest predicate.1
  (fboundp 'predicate-1-p)
  t)

(defstruct (predicate-2 :predicate) aaa)
(deftest predicate.2
  (fboundp 'predicate-2-p)
  t)

(defstruct (predicate-3 (:predicate)) aaa)
(deftest predicate.3
  (fboundp 'predicate-3-p)
  t)

(defstruct (predicate-4 (:predicate nil)) aaa)
(deftest predicate.4
  (fboundp 'predicate-4-p)
  nil)

(defstruct (predicate-5 (:predicate predicate-5-hello)) aaa)
(deftest predicate.5
  (fboundp 'predicate-5-hello)
  t)

(defstruct predicate-6 aaa bbb ccc)
(deftest predicate.6
  (let ((x (make-predicate-6 :aaa 10 :bbb 20 :ccc 30))
        (y (make-predicate-5)))
    (values
      (predicate-6-p x)
      (predicate-6-p y)))
  t nil)

(deftest predicate.7
  (predicate-6-p 100)
  nil)

;; constructor
(defstruct constructor-1)
(deftest constructor.1
  (fboundp 'make-constructor-1)
  t)

(defstruct (constructor-2 :constructor) aaa)
(deftest constructor.2
  (fboundp 'make-constructor-2)
  t)

(defstruct (constructor-3 (:constructor)) aaa)
(deftest constructor.3
  (fboundp 'make-constructor-3)
  t)

(defstruct (constructor-4 (:constructor nil)) aaa)
(deftest constructor.4
  (fboundp 'make-constructor-4)
  nil)

(defstruct (constructor-5 (:constructor hello-constructor-5)) aaa)
(deftest constructor.5
  (fboundp 'hello-constructor-5)
  t)

(defstruct constructor-6 aaa bbb)
(deftest constructor.6
  (let ((inst (make-constructor-6 :aaa 10 :bbb 20)))
    (values
      (constructor-6-aaa inst)
      (constructor-6-bbb inst)))
  10 20)

(defstruct (constructor-7 (:include constructor-6)) ccc)
(deftest constructor.7
  (let ((inst (make-constructor-7 :aaa 10 :bbb 20 :ccc 30 :bbb 40)))
    (values
      (constructor-7-aaa inst)
      (constructor-7-bbb inst)
      (constructor-7-ccc inst)))
  10 20 30)

;; boa
(defstruct (boa-1 (:constructor make-boa-1 ())) aaa bbb)
(deftest boa.1
  (let ((inst (make-boa-1)))
    (values
      (boa-1-aaa inst)
      (boa-1-bbb inst)))
  nil nil)

(defstruct (boa-2 (:constructor make-boa-2 (aaa bbb))) aaa bbb)
(deftest boa.2
  (let ((inst (make-boa-2 10 20)))
    (values
      (boa-2-aaa inst)
      (boa-2-bbb inst)))
  10 20)

(defstruct (boa-3 (:constructor make-boa-3 (aaa bbb ccc))) aaa bbb)
(deftest boa.3
  (let ((inst (make-boa-3 10 20 30)))
    (values
      (boa-3-aaa inst)
      (boa-3-bbb inst)))
  10 20)

(defstruct (boa-4 (:constructor make-boa-4 (aaa)))
  (aaa 999) (bbb 888))
(deftest boa.4
  (let ((inst (make-boa-4 10)))
    (values
      (boa-4-aaa inst)
      (boa-4-bbb inst)))
  10 888)

(defstruct (boa-5 (:constructor make-boa-5 (aaa &optional bbb)))
  (aaa 999) (bbb 888))
(deftest boa.5
  (let ((inst (make-boa-5 10)))
    (values
      (boa-5-aaa inst)
      (boa-5-bbb inst)))
  10 888)

(deftest boa.6
  (let ((inst (make-boa-5 10 20)))
    (values
      (boa-5-aaa inst)
      (boa-5-bbb inst)))
  10 20)

(defstruct (boa-7 (:constructor make-boa-7 (&key (aaa 200) ((:hello bbb)))))
  (aaa 999) (bbb 888))
(deftest boa.7
  (let ((inst (make-boa-7)))
    (values
      (boa-7-aaa inst)
      (boa-7-bbb inst)))
  200 888)

(deftest boa.8
  (let ((inst (make-boa-7 :aaa 123)))
    (values
      (boa-7-aaa inst)
      (boa-7-bbb inst)))
  123 888)

(deftest boa.9
  (let ((inst (make-boa-7 :hello 123)))
    (values
      (boa-7-aaa inst)
      (boa-7-bbb inst)))
  200 123)

(defstruct (boa-10 (:constructor make-boa-10 (&rest aaa &aux bbb)))
  (aaa 999) (bbb 888))
(deftest boa.10
  (let ((inst (make-boa-10)))
    (values
      (boa-10-aaa inst)
      (boa-10-bbb inst)))
  nil nil)

(deftest boa.11
  (let ((inst (make-boa-10 10 20 30)))
    (values
      (boa-10-aaa inst)
      (boa-10-bbb inst)))
  (10 20 30) nil)

(defstruct (boa-12 (:constructor make-boa-12 (&rest aaa &aux (bbb 777))))
  (aaa 999) (bbb 888))
(deftest boa.12
  (let ((inst (make-boa-12)))
    (values
      (boa-12-aaa inst)
      (boa-12-bbb inst)))
  nil 777)

;; print-object
(defstruct print-object-1)
(deftest print-object.1
  (princ-to-string (make-print-object-1))
  "#S(PRINT-OBJECT-1)")

(defun print-object-2-call (value stream)
  (declare (ignore value))
  (princ "Hello" stream))

(defstruct (print-object-2 (:print-object print-object-2-call)))
(deftest print-object.2
  (princ-to-string (make-print-object-2))
  "Hello")

(defstruct (print-object-3 (:include print-object-2)))
(deftest print-object.3
  (princ-to-string (make-print-object-3))
  "Hello")

(defstruct (print-object-4 (:include print-object-2) :print-object))
(deftest print-object.4
  (princ-to-string (make-print-object-4))
  "#S(PRINT-OBJECT-4)")

;; print-function
(defstruct print-function-1)
(deftest print-function.1
  (princ-to-string (make-print-function-1))
  "#S(PRINT-FUNCTION-1)")

(defun print-function-2-call (value stream level)
  (declare (ignore value level))
  (princ "Hello" stream))

(defstruct (print-function-2 (:print-function print-function-2-call)))
(deftest print-function.2
  (princ-to-string (make-print-function-2))
  "Hello")

(defstruct (print-function-3 (:include print-function-2)))
(deftest print-function.3
  (princ-to-string (make-print-function-3))
  "Hello")

(defstruct (print-function-4 (:include print-function-2) :print-function))
(deftest print-function.4
  (princ-to-string (make-print-function-4))
  "#S(PRINT-FUNCTION-4)")

;; make-instance
(defstruct make-instance-structure aaa bbb)
(deftest make-instance-structure.1
  (let ((inst (make-instance 'make-instance-structure :aaa 100)))
    (values
      (make-instance-structure-aaa inst)
      (make-instance-structure-bbb inst)))
  100 nil)

;; slot-value
(deftest slot-value-structure.1
  (let ((inst (make-instance 'make-instance-structure :aaa 100)))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  100 nil)

;; slot-boundp
(deftest slot-boundp-structure.1
  (let ((inst (make-instance 'make-instance-structure :aaa 100)))
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  t t)

;; slot-exists-p
(deftest slot-exists-p-structure.1
  (let ((inst (make-instance 'make-instance-structure :aaa 100)))
    (values
      (slot-exists-p inst 'aaa)
      (slot-exists-p inst 'bbb)
      (slot-exists-p inst 'ccc)))
  t t nil)

;; slot-makunbound
(deftest slot-makunbound-structure.1
  (let ((inst (make-instance 'make-instance-structure :aaa 100)))
    (slot-makunbound inst 'aaa)
    (values
      (slot-boundp inst 'aaa)
      (slot-boundp inst 'bbb)))
  nil t)

;; #S()
(deftest dispatch-structure.1
  (let ((inst (read-from-string "#S(make-instance-structure :aaa 100)")))
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  100 nil)

