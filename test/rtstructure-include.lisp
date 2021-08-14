;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  include-check
;;

;;  clos
(deftest-error clos-include-check.1
  (eval '(defstruct (clos-include-check-1 :include))))

(deftest-error clos-include-check.2
  (eval '(defstruct (clos-include-check-2 (:include)))))

(deftest clos-include-check.3
  (defstruct (clos-include-check-3 (:include clos-empty-1)))
  clos-include-check-3)

(deftest clos-include-check.4
  (defstruct (clos-include-check-4
               (:include clos-empty-3 aaa bbb))
    ddd eee)
  clos-include-check-4)

(deftest-error clos-include-check.5
  (defstruct (clos-include-check-5
               (:include clos-empty-3 aaa bbb aaa))
    ddd eee))

(deftest-error clos-include-check.6
  (defstruct (clos-include-check-6
               (:include clos-empty-3 aaa bbb))
    ccc ddd eee))


;;  list
(deftest-error list-include-check.1
  (eval '(defstruct (list-include-check-1 (:type list) :include))))

(deftest-error list-include-check.2
  (eval '(defstruct (list-include-check-2 (:type list) (:include)))))

(deftest list-include-check.3
  (defstruct (list-include-check-3 (:type list) (:include list-empty-1)))
  list-include-check-3)

(deftest list-include-check.4
  (defstruct (list-include-check-4
               (:type list)
               (:include list-empty-3 aaa bbb))
    ddd eee)
  list-include-check-4)

(deftest-error list-include-check.5
  (defstruct (list-include-check-5
               (:type list)
               (:include list-empty-3 aaa bbb aaa))
    ddd eee))

(deftest-error list-include-check.6
  (defstruct (list-include-check-6
               (:type list)
               (:include list-empty-3 aaa bbb))
    ccc ddd eee))

(deftest list-include-check.7
  (defstruct (list-include-check-7
               (:type list)
               :named
               (:include list-empty-3 aaa bbb))
    ddd eee)
  list-include-check-7)

(deftest list-include-check.8
  (defstruct (list-include-check-8
               (:type list)
               :named
               (:include list-empty-named aaa bbb))
    ddd eee)
  list-include-check-8)

(defstruct list-include-error-clos aaa)
(defstruct (list-include-error-list (:type list)) aaa)
(defstruct (list-include-error-vector (:type vector)) aaa)

(deftest list-include-error.1
  (defstruct (list-include-error-1
               (:include list-include-error-clos))
    bbb ccc)
  list-include-error-1)

(deftest-error list-include-error.2
  (defstruct (list-include-error-2
               (:type list)
               (:include list-include-error-clos))
    bbb ccc))

(deftest-error list-include-error.3
  (defstruct (list-include-error-3
               (:type vector)
               (:include list-include-error-clos))
    bbb ccc))

(deftest-error list-include-error.4
  (defstruct (list-include-error-4
               (:include list-include-error-list))))

(deftest list-include-error.5
  (defstruct (list-include-error-5
               (:type list)
               (:include list-include-error-list)))
  list-include-error-5)

(deftest-error list-include-error.6
  (defstruct (list-include-error-6
               (:type vector)
               (:include list-include-error-list))))

(deftest-error list-include-error.7
  (defstruct (list-include-error-7
               (:include list-include-error-vector))))

(deftest-error list-include-error.8
  (defstruct (list-include-error-8
               (:type list)
               (:include list-include-error-vector))))

(deftest list-include-error.9
  (defstruct (list-include-error-9
               (:type vector)
               (:include list-include-error-vector)))
  list-include-error-9)


;;  vector
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


;;
;;  include
;;

;;  clos
(deftest clos-include.1
  (defstruct (clos-include-1 (:include clos-empty-1)))
  clos-include-1)

(deftest-error clos-include.2
  (defstruct (clos-include-2 (:include no-such-structure))))

(deftest-error clos-include.3
  (defstruct (clos-include-2 (:include standard-class))))


;;  list
(deftest list-include.1
  (defstruct (list-include-1 (:include list-empty-1) (:type list)))
  list-include-1)

(deftest-error list-include.2
  (defstruct (list-include-2 (:include no-such-structure) (:type list))))

(deftest-error list-include.3
  (defstruct (list-include-2 (:include standard-class) (:type list))))


;;  vector
(deftest vector-include.1
  (defstruct (vector-include-1 (:include vector-empty-1) (:type vector)))
  vector-include-1)

(deftest-error vector-include.2
  (defstruct (vector-include-2 (:include no-such-structure) (:type vector))))

(deftest-error vector-include.3
  (defstruct (vector-include-2 (:include standard-class) (:type vector))))


;;
;;  include-slots
;;

;;  clos
(deftest check-clos-include-slots.1
  (defstruct (check-clos-include-slots-1 (:include clos-empty-1))
    aaa bbb ccc)
  check-clos-include-slots-1)

(deftest check-clos-include-slots.2
  (defstruct (check-clos-include-slots-2
               (:include check-clos-include-slots-1))
    ddd eee fff)
  check-clos-include-slots-2)

(deftest-error check-clos-include-slots.3
  (defstruct (check-clos-include-slots-3
               (:include check-clos-include-slots-1))
    ddd aaa fff))


;;  list
(deftest list-check-include-slots.1
  (defstruct (list-check-include-slots-1 (:include list-empty-1) (:type list))
    aaa bbb ccc)
  list-check-include-slots-1)

(deftest list-check-include-slots.2
  (defstruct (list-check-include-slots-2
               (:include list-check-include-slots-1)
               (:type list))
    ddd eee fff)
  list-check-include-slots-2)

(deftest-error list-check-include-slots.3
  (defstruct (list-check-include-slots-3
               (:include list-check-include-slots-1) (:type list))
    ddd aaa fff))


;;  vector
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


;;
;;  include-arguemnts
;;

;;  clos
(defstruct check-clos-include-arguments
  aaa bbb
  (ccc 10 :type integer)
  (ddd 20.0 :type float :read-only t))

(deftest check-clos-include-arguments.1
  (defstruct (check-clos-include-arguments-1
               (:include check-clos-include-arguments)))
  check-clos-include-arguments-1)

(deftest check-clos-include-arguments.2
  (defstruct (check-clos-include-arguments-2
               (:include check-clos-include-arguments
                         aaa bbb ccc ddd))
    eee fff ggg)
  check-clos-include-arguments-2)

(deftest check-clos-include-arguments.3
  (defstruct (check-clos-include-arguments-3
               (:include check-clos-include-arguments
                         (aaa 10 :type integer :read-only t))))
  check-clos-include-arguments-3)

(deftest check-clos-include-arguments.4
  (defstruct (check-clos-include-arguments-4
               (:include check-clos-include-arguments
                         (ccc 10 :type integer))))
  check-clos-include-arguments-4)

(deftest check-clos-include-arguments.5
  (defstruct (check-clos-include-arguments-5
               (:include check-clos-include-arguments
                         (ccc 10 :type fixnum))))
  check-clos-include-arguments-5)

(deftest-error check-clos-include-arguments.6
  (defstruct (check-clos-include-arguments-6
               (:include check-clos-include-arguments
                         (ccc 10 :type real)))))

(deftest check-clos-include-arguments.7
  (defstruct (check-clos-include-arguments-7
               (:include check-clos-include-arguments
                         (ddd 20))))
  check-clos-include-arguments-7)

(deftest check-clos-include-arguments.8
  (defstruct (check-clos-include-arguments-8
               (:include check-clos-include-arguments
                         (ddd 20 :read-only t))))
  check-clos-include-arguments-8)

(deftest-error check-clos-include-arguments.9
  (defstruct (check-clos-include-arguments-9
               (:include check-clos-include-arguments
                         (ddd 20 :read-only nil)))))


;;  list
(defstruct (list-check-include-arguments (:type list))
  aaa bbb
  (ccc 10 :type integer)
  (ddd 20.0 :type float :read-only t))

(deftest list-check-include-arguments.1
  (defstruct (list-check-include-arguments-1
               (:include list-check-include-arguments)
               (:type list)))
  list-check-include-arguments-1)

(deftest list-check-include-arguments.2
  (defstruct (list-check-include-arguments-2
               (:include list-check-include-arguments
                         aaa bbb ccc ddd)
               (:type list))
    eee fff ggg)
  list-check-include-arguments-2)

(deftest list-check-include-arguments.3
  (defstruct (list-check-include-arguments-3
               (:include list-check-include-arguments
                         (aaa 10 :type integer :read-only t))
               (:type list)))
  list-check-include-arguments-3)

(deftest list-check-include-arguments.4
  (defstruct (list-check-include-arguments-4
               (:include list-check-include-arguments
                         (ccc 10 :type integer))
               (:type list)))
  list-check-include-arguments-4)

(deftest list-check-include-arguments.5
  (defstruct (list-check-include-arguments-5
               (:include list-check-include-arguments
                         (ccc 10 :type fixnum))
               (:type list)))
  list-check-include-arguments-5)

(deftest-error list-check-include-arguments.6
  (defstruct (list-check-include-arguments-6
               (:include list-check-include-arguments
                         (ccc 10 :type real))
               (:type list))))

(deftest list-check-include-arguments.7
  (defstruct (list-check-include-arguments-7
               (:include list-check-include-arguments
                         (ddd 20))
               (:type list)))
  list-check-include-arguments-7)

(deftest list-check-include-arguments.8
  (defstruct (list-check-include-arguments-8
               (:include list-check-include-arguments
                         (ddd 20 :read-only t))
               (:type list)))
  list-check-include-arguments-8)

(deftest-error list-check-include-arguments.9
  (defstruct (list-check-include-arguments-9
               (:include list-check-include-arguments
                         (ddd 20 :read-only nil))
               (:type list))))

;; named
(defstruct (list-include-named-a (:type list))
  (aa 10) (bb 20))
(defstruct (list-include-named-b
             (:include list-include-named-a)
             (:type list) :named)
  (cc 30) (dd 40) (ee 50))

(defstruct (list-include-named-1
             (:include list-include-named-b)
             (:type list))
  (ff 111))
(deftest list-include-named.1
  (make-list-include-named-1)
  (10 20 nil 30 40 50 111))

(deftest list-include-named.2
  (let ((x (make-list-include-named-1)))
    (values
      (list-include-named-1-aa x)
      (list-include-named-1-bb x)
      (list-include-named-1-cc x)
      (list-include-named-1-dd x)
      (list-include-named-1-ee x)
      (list-include-named-1-ff x)))
  10 20 30 40 50 111)

(defstruct (list-include-named-3
             (:include list-include-named-b)
             (:type list) :named)
  (ff 111) (gg 222) (hh 333) (ii 444))
(deftest list-include-named.3
  (make-list-include-named-3)
  (10 20 nil 30 40 50 list-include-named-3 111 222 333 444))

(deftest list-include-named.4
  (let ((x (make-list-include-named-3)))
    (values
      (list-include-named-3-aa x)
      (list-include-named-3-bb x)
      (list-include-named-3-cc x)
      (list-include-named-3-dd x)
      (list-include-named-3-ee x)
      (list-include-named-3-ff x)
      (list-include-named-3-gg x)
      (list-include-named-3-hh x)
      (list-include-named-3-ii x)))
  10 20 30 40 50 111 222 333 444)


;;  vector
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



;;
;;  change
;;

;;  clos
(defstruct change-clos-include-1 aaa)
(defstruct change-clos-include-2 bbb ccc)

(deftest-error change-clos-include.1
  (eval '(defstruct (change-clos-include-3
                      (:include change-clos-include-1)
                      (:include change-clos-include-2)))))

(deftest-error change-clos-include.2
  (progn
    (defstruct change-clos-include-3)
    (defstruct (change-clos-include-3 (:include change-clos-include-2)))))

(deftest-error change-clos-include.3
  (progn
    (defstruct (change-clos-include-4 (:include change-clos-include-1)) ddd eee)
    (defstruct change-clos-include-4)))

(deftest-error change-clos-include.4
  (progn
    (defstruct (change-clos-include-5 (:include change-clos-include-1)))
    (defstruct (change-clos-include-5 (:include change-clos-include-2)))))

(deftest change-clos-include.5
  (progn
    (defstruct (change-clos-include-6 (:include change-clos-include-1)))
    (defstruct (change-clos-include-6 (:include change-clos-include-1))))
  change-clos-include-6)

(deftest change-clos-include.6
  (progn
    (defstruct (change-clos-include-7 (:include change-clos-include-2)))
    (defstruct (change-clos-include-7 (:include change-clos-include-2 (bbb 100)))))
  change-clos-include-7)


;;  list
(defstruct (change-list-include-1 (:type list) :named) aaa)
(defstruct (change-list-include-2 (:type list) :named) bbb ccc)

(deftest-error change-list-include.1
  (eval '(defstruct (change-list-include-3
                      (:include change-list-include-1)
                      (:include change-list-include-2)
                      (:type list)
                      :named))))

(deftest-error change-list-include.2
  (progn
    (defstruct (change-list-include-3
                 (:type list) :named))
    (defstruct (change-list-include-3
                 (:include change-list-include-2)
                 (:type list) :named))))

(deftest-error change-list-include.3
  (progn
    (defstruct (change-list-include-4
                 (:include change-list-include-1)
                 (:type list) :named)
      ddd eee)
    (defstruct (change-list-include-4
                 (:type list) :named))))

(deftest-error change-list-include.4
  (progn
    (defstruct (change-list-include-5
                 (:include change-list-include-1)
                 (:type list) :named))
    (defstruct (change-list-include-5
                 (:include change-list-include-2)
                 (:type list) :named))))

(deftest change-list-include.5
  (progn
    (defstruct (change-list-include-6
                 (:include change-list-include-1)
                 (:type list) :named))
    (defstruct (change-list-include-6
                 (:include change-list-include-1)
                 (:type list) :named)))
  change-list-include-6)

(deftest change-list-include.6
  (progn
    (defstruct (change-list-include-7
                 (:include change-list-include-2)
                 (:type list) :named))
    (defstruct (change-list-include-7
                 (:include change-list-include-2 (bbb 100))
                 (:type list) :named)))
  change-list-include-7)


;;  vector
(defstruct (change-vector-include-1 (:type vector) :named) aaa)
(defstruct (change-vector-include-2 (:type vector) :named) bbb ccc)

(deftest-error change-vector-include.1
  (eval '(defstruct (change-vector-include-3
                      (:include change-vector-include-1)
                      (:include change-vector-include-2)
                      (:type vector)
                      :named))))

(deftest-error change-vector-include.2
  (progn
    (defstruct (change-vector-include-3
                 (:type vector) :named))
    (defstruct (change-vector-include-3
                 (:include change-vector-include-2)
                 (:type vector) :named))))

(deftest-error change-vector-include.3
  (progn
    (defstruct (change-vector-include-4
                 (:include change-vector-include-1)
                 (:type vector) :named)
      ddd eee)
    (defstruct (change-vector-include-4
                 (:type vector) :named))))

(deftest-error change-vector-include.4
  (progn
    (defstruct (change-vector-include-5
                 (:include change-vector-include-1)
                 (:type vector) :named))
    (defstruct (change-vector-include-5
                 (:include change-vector-include-2)
                 (:type vector) :named))))

(deftest change-vector-include.5
  (progn
    (defstruct (change-vector-include-6
                 (:include change-vector-include-1)
                 (:type vector) :named))
    (defstruct (change-vector-include-6
                 (:include change-vector-include-1)
                 (:type vector) :named)))
  change-vector-include-6)

(deftest change-vector-include.6
  (progn
    (defstruct (change-vector-include-7
                 (:include change-vector-include-2)
                 (:type vector) :named))
    (defstruct (change-vector-include-7
                 (:include change-vector-include-2 (bbb 100))
                 (:type vector) :named)))
  change-vector-include-7)

