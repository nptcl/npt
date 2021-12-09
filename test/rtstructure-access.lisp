;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  slot-access
;;

;;  clos
(defstruct clos-slot-access-1 aaa bbb)
(deftest clos-slot-access.1
  (let ((inst (make-clos-slot-access-1)))
    (clos-slot-access-1-aaa inst))
  nil)

(deftest clos-slot-access.2
  (let ((inst (make-clos-slot-access-1)))
    (setf (clos-slot-access-1-aaa inst) 100)
    (clos-slot-access-1-aaa inst))
  100)

(deftest clos-slot-access.3
  (let ((inst (make-clos-slot-access-1)))
    (setf (clos-slot-access-1-aaa inst) 100)
    (setf (clos-slot-access-1-bbb inst) 200)
    (values
      (clos-slot-access-1-aaa inst)
      (clos-slot-access-1-bbb inst)))
  100 200)

(deftest clos-slot-access.4
  (let ((inst (make-clos-slot-access-1)))
    (slot-value inst 'aaa))
  nil)

(deftest clos-slot-access.5
  (let ((inst (make-clos-slot-access-1)))
    (setf (clos-slot-access-1-aaa inst) 100)
    (slot-value inst 'aaa))
  100)

(deftest clos-slot-access.6
  (let ((inst (make-clos-slot-access-1)))
    (setf (slot-value inst 'aaa) 200)
    (clos-slot-access-1-aaa inst))
  200)

(deftest clos-slot-access.7
  (let ((inst (make-clos-slot-access-1)))
    (setf (slot-value inst 'aaa) 100)
    (setf (slot-value inst 'bbb) 200)
    (values
      (clos-slot-access-1-aaa inst)
      (clos-slot-access-1-bbb inst)
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)))
  100 200 100 200)

(defstruct (clos-slot-access-2 (:include clos-slot-access-1)) ccc ddd)
(deftest clos-slot-access-include.1
  (let ((inst (make-clos-slot-access-2)))
    (values
      (clos-slot-access-2-aaa inst)
      (clos-slot-access-2-bbb inst)
      (clos-slot-access-2-ccc inst)
      (clos-slot-access-2-ddd inst)))
  nil nil nil nil)

(deftest clos-slot-access-include.2
  (let ((inst (make-clos-slot-access-2)))
    (setf (clos-slot-access-2-aaa inst) 100)
    (setf (clos-slot-access-2-bbb inst) 200)
    (setf (clos-slot-access-2-ccc inst) 300)
    (setf (clos-slot-access-2-ddd inst) 400)
    (values
      (clos-slot-access-2-aaa inst)
      (clos-slot-access-2-bbb inst)
      (clos-slot-access-2-ccc inst)
      (clos-slot-access-2-ddd inst)))
  100 200 300 400)

(deftest clos-slot-access-include.3
  (let ((inst (make-clos-slot-access-2)))
    (setf (slot-value inst 'aaa) 100)
    (setf (clos-slot-access-2-bbb inst) 200)
    (setf (slot-value inst 'ccc) 300)
    (setf (clos-slot-access-2-ddd inst) 400)
    (values
      (slot-value inst 'aaa)
      (slot-value inst 'bbb)
      (clos-slot-access-2-ccc inst)
      (clos-slot-access-2-ddd inst)))
  100 200 300 400)

(deftest clos-slot-access-include.4
  (let ((inst (make-clos-slot-access-2)))
    (setf (clos-slot-access-2-aaa inst) 100)
    (setf (clos-slot-access-2-bbb inst) 200)
    (setf (clos-slot-access-2-ccc inst) 300)
    (setf (clos-slot-access-2-ddd inst) 400)
    (values
      (clos-slot-access-1-aaa inst)  ;; 1
      (clos-slot-access-1-bbb inst)  ;; 1
      (clos-slot-access-2-ccc inst)
      (clos-slot-access-2-ddd inst)))
  100 200 300 400)

(deftest-error clos-slot-access-include.5
  (let ((inst (make-clos-slot-access-1)))
    (clos-slot-access-2-aaa inst)))

(defstruct clos-slot-access-readonly-1
  (aaa 100 :read-only t)
  (bbb 200))

(deftest clos-slot-access-readonly.1
  (let ((inst (make-clos-slot-access-readonly-1)))
    (values
      (clos-slot-access-readonly-1-aaa inst)
      (fboundp '(setf clos-slot-access-readonly-1-aaa))))
  100 nil)

(defstruct (clos-slot-access-readonly-2 (:include clos-slot-access-readonly-1)))
(deftest clos-slot-access-readonly.2
  (let ((inst (make-clos-slot-access-readonly-2)))
    (values
      (clos-slot-access-readonly-2-aaa inst)
      (fboundp '(setf clos-slot-access-readonly-2-aaa))))
  100 nil)

(defstruct (clos-slot-access-readonly-3
             (:include clos-slot-access-readonly-1
                       (aaa 300 :read-only t))))
(deftest clos-slot-access-readonly.3
  (let ((inst (make-clos-slot-access-readonly-3)))
    (values
      (clos-slot-access-readonly-3-aaa inst)
      (fboundp '(setf clos-slot-access-readonly-3-aaa))))
  300 nil)

(deftest-error clos-slot-access-readonly.4
  (defstruct (clos-slot-access-readonly-4
               (:include clos-slot-access-readonly-1
                         (aaa 300 :read-only nil)))))

(defstruct (clos-slot-access-readonly-5
             (:include clos-slot-access-readonly-1
                       (bbb 400 :read-only t))))
(deftest clos-slot-access-readonly.5
  (let ((inst (make-clos-slot-access-readonly-5)))
    (values
      (clos-slot-access-readonly-5-bbb inst)
      (fboundp '(setf clos-slot-access-readonly-5-bbb))))
  400 nil)

(defstruct clos-slot-access-type-1
  (aaa 100 :type integer)
  (bbb "Hello" :type string))

(deftest clos-slot-access-type.1
  (let ((inst (make-clos-slot-access-type-1)))
    (setf (clos-slot-access-type-1-aaa inst) 200)
    (setf (clos-slot-access-type-1-bbb inst) "ZZZ")
    (values
      (clos-slot-access-type-1-aaa inst)
      (clos-slot-access-type-1-bbb inst)))
  200 "ZZZ")

(defstruct (clos-slot-access-type-2 (:include clos-slot-access-type-1 aaa)))
(deftest clos-slot-access-type.2
  (let ((inst (make-clos-slot-access-type-2)))
    (setf (clos-slot-access-type-2-aaa inst) 200)
    (setf (clos-slot-access-type-2-bbb inst) "ZZZ")
    (values
      (clos-slot-access-type-2-aaa inst)
      (clos-slot-access-type-2-bbb inst)))
  200 "ZZZ")

(defstruct (clos-slot-access-type-3
             (:include clos-slot-access-type-1
                       (aaa 300 :type fixnum))))
(deftest clos-slot-access-type.3
  (let ((inst (make-clos-slot-access-type-3)))
    (setf (clos-slot-access-type-3-aaa inst) 400)
    (setf (clos-slot-access-type-3-bbb inst) "ZZZ")
    (values
      (clos-slot-access-type-3-aaa inst)
      (clos-slot-access-type-3-bbb inst)))
  400 "ZZZ")

(deftest-error clos-slot-access-type.4
  (defstruct (clos-slot-access-type-4
               (:include clos-slot-access-type-1
                         (aaa 300 :type real)))))


;;  list
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


;;  vector
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


;;
;;  array access
;;
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


;;  change
(deftest change-slot-access.1
  (progn
    (defstruct change-slot-access-1 aaa)
    (defstruct change-slot-access-1 aaa)
    (let ((x (make-change-slot-access-1)))
      (setf (change-slot-access-1-aaa x) 100)
      (change-slot-access-1-aaa x)))
  100)

(deftest change-slot-access.2
  (progn
    (defstruct (change-slot-access-2 (:type list)) aaa)
    (defstruct (change-slot-access-2 (:type list)) aaa)
    (let ((x (make-change-slot-access-2)))
      (setf (change-slot-access-2-aaa x) 200)
      (change-slot-access-2-aaa x)))
  200)

(deftest change-slot-access.3
  (progn
    (defstruct (change-slot-access-3 (:type vector)) aaa)
    (defstruct (change-slot-access-3 (:type vector)) aaa)
    (let ((x (make-change-slot-access-3)))
      (setf (change-slot-access-3-aaa x) 300)
      (change-slot-access-3-aaa x)))
  300)


;;
;;  type error
;;
(deftest-error defstruct-type-error.1
  (progn
    (defstruct defstruct-type-error-1
      (aaa 100 :type integer))
    (let ((x (make-defstruct-type-error-1)))
      (setf (defstruct-type-error-1-aaa x) :hello)))
  type-error)

(deftest-error defstruct-type-error.2
  (progn
    (defstruct (defstruct-type-error-2 (:type list))
      (aaa 100 :type integer))
    (let ((x (make-defstruct-type-error-2)))
      (setf (defstruct-type-error-2-aaa x) :hello)))
  type-error)

(deftest-error defstruct-type-error.3
  (progn
    (defstruct (defstruct-type-error-3 (:type vector))
      (aaa 100 :type integer))
    (let ((x (make-defstruct-type-error-3)))
      (setf (defstruct-type-error-3-aaa x) :hello)))
  type-error)

