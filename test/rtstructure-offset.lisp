;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  list-offset
;;
(deftest-error defstruct-list-offset.1
  (eval '(defstruct (defstruct-list-offset-1 (:initial-offset 2)))))

(deftest defstruct-list-offset.2
  (progn
    (defstruct (defstruct-list-offset-2
                 (:initial-offset 2)
                 (:type list))
      aaa bbb)
    (make-defstruct-list-offset-2 :aaa 10 :bbb 20))
  (nil nil 10 20))

(deftest defstruct-list-offset.3
  (progn
    (defstruct (defstruct-list-offset-3
                 (:initial-offset 2)
                 (:type list)
                 :named)
      aaa bbb)
    (make-defstruct-list-offset-3 :aaa 10 :bbb 20))
  (nil nil defstruct-list-offset-3 10 20))

(deftest defstruct-list-offset.4
  (progn
    (defstruct (defstruct-list-offset-4
                 (:initial-offset 2)
                 (:type list)
                 :named)
      (bbb 100)
      (ccc 200))
    (defstruct (defstruct-list-offset-5
                 (:include defstruct-list-offset-4)
                 (:initial-offset 3)
                 (:type list))
      (eee 300)
      (fff 400))
    (values
      (make-defstruct-list-offset-4)
      (make-defstruct-list-offset-5)))
  (nil nil defstruct-list-offset-4 100 200)
  (nil nil defstruct-list-offset-4 100 200 nil nil nil 300 400))

(deftest defstruct-list-offset.5
  (progn
    (defstruct (defstruct-list-offset-6
                 (:initial-offset 1)
                 (:type list)
                 :named)
      (bbb 100) (ccc 200))
    (defstruct (defstruct-list-offset-7
                 (:include defstruct-list-offset-6)
                 (:initial-offset 2)
                 (:type list)
                 :named)
      (ddd 300) (eee 400))
    (defstruct (defstruct-list-offset-8
                 (:include defstruct-list-offset-7)
                 (:initial-offset 3)
                 (:type list)
                 :named)
      (fff 500) (ggg 600))
    (values
      (make-defstruct-list-offset-6)
      (make-defstruct-list-offset-7)
      (make-defstruct-list-offset-8)))
  (nil defstruct-list-offset-6 100 200)
  (nil defstruct-list-offset-6 100 200
       nil nil defstruct-list-offset-7 300 400)
  (nil defstruct-list-offset-6 100 200
       nil nil defstruct-list-offset-7 300 400
       nil nil nil defstruct-list-offset-8 500 600))


;;
;;  vector
;;
(deftest defstruct-vector-offset.2
  (progn
    (defstruct (defstruct-vector-offset-2
                 (:initial-offset 2)
                 (:type vector))
      aaa bbb)
    (make-defstruct-vector-offset-2 :aaa 10 :bbb 20))
  #(nil nil 10 20))

(deftest defstruct-vector-offset.3
  (progn
    (defstruct (defstruct-vector-offset-3
                 (:initial-offset 2)
                 (:type vector)
                 :named)
      aaa bbb)
    (make-defstruct-vector-offset-3 :aaa 10 :bbb 20))
  #(nil nil defstruct-vector-offset-3 10 20))

(deftest defstruct-vector-offset.4
  (progn
    (defstruct (defstruct-vector-offset-4
                 (:initial-offset 2)
                 (:type vector)
                 :named)
      (bbb 100)
      (ccc 200))
    (defstruct (defstruct-vector-offset-5
                 (:include defstruct-vector-offset-4)
                 (:initial-offset 3)
                 (:type vector))
      (eee 300)
      (fff 400))
    (values
      (make-defstruct-vector-offset-4)
      (make-defstruct-vector-offset-5)))
  #(nil nil defstruct-vector-offset-4 100 200)
  #(nil nil defstruct-vector-offset-4 100 200 nil nil nil 300 400))

(deftest defstruct-vector-offset.5
  (progn
    (defstruct (defstruct-vector-offset-6
                 (:initial-offset 1)
                 (:type vector)
                 :named)
      (bbb 100) (ccc 200))
    (defstruct (defstruct-vector-offset-7
                 (:include defstruct-vector-offset-6)
                 (:initial-offset 2)
                 (:type vector)
                 :named)
      (ddd 300) (eee 400))
    (defstruct (defstruct-vector-offset-8
                 (:include defstruct-vector-offset-7)
                 (:initial-offset 3)
                 (:type vector)
                 :named)
      (fff 500) (ggg 600))
    (values
      (make-defstruct-vector-offset-6)
      (make-defstruct-vector-offset-7)
      (make-defstruct-vector-offset-8)))
  #(nil defstruct-vector-offset-6 100 200)
  #(nil defstruct-vector-offset-6 100 200
        nil nil defstruct-vector-offset-7 300 400)
  #(nil defstruct-vector-offset-6 100 200
        nil nil defstruct-vector-offset-7 300 400
        nil nil nil defstruct-vector-offset-8 500 600))


;;
;;  error
;;
(deftest-error defstruct-offset-error.1
  (eval '(defstruct (defstruct-offset-error-1
                      (:type list)
                      (:initial-offset -1)))))

(deftest defstruct-offset-error.2
  (progn
    (defstruct (defstruct-offset-error-2
                 (:type list)
                 (:initial-offset 0))
      (aaa 10))
    (make-defstruct-offset-error-2))
  (10))

