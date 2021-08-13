;;
;;  ANSI COMMON LISP: 8. Structures
;;

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

