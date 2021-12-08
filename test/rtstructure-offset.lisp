;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  initial-offset
;;
(deftest-error defstruct-initial-offset.1
  (eval '(defstruct (defstruct-initial-offset-1 (:initial-offset 2)))))

(deftest defstruct-initial-offset.2
  (progn
    (defstruct (defstruct-initial-offset-2
                 (:initial-offset 2)
                 (:type list))
      aaa bbb)
    (make-defstruct-initial-offset-2 :aaa 10 :bbb 20))
  (nil nil 10 20))

(deftest defstruct-initial-offset.3
  (progn
    (defstruct (defstruct-initial-offset-3
                 (:initial-offset 2)
                 (:type list)
                 :named)
      aaa bbb)
    (make-defstruct-initial-offset-3 :aaa 10 :bbb 20))
  (nil nil defstruct-initial-offset-3 10 20))

(deftest defstruct-initial-offset.4
  (progn
    (defstruct (defstruct-initial-offset-4
                 (:initial-offset 2)
                 (:type list)
                 :named)
      (bbb 100)
      (ccc 200))
    (defstruct (defstruct-initial-offset-5
                 (:include defstruct-initial-offset-4)
                 (:initial-offset 3)
                 (:type list))
      (eee 300)
      (fff 400))
    (values
      (make-defstruct-initial-offset-4)
      (make-defstruct-initial-offset-5)))
  (nil nil defstruct-initial-offset-4 100 200)
  (nil nil defstruct-initial-offset-4 100 200 nil nil nil 300 400))

(deftest defstruct-initial-offset.5
  (progn
    (defstruct (defstruct-initial-offset-6
                 (:initial-offset 1)
                 (:type list)
                 :named)
      (bbb 100) (ccc 200))
    (defstruct (defstruct-initial-offset-7
                 (:include defstruct-initial-offset-6)
                 (:initial-offset 2)
                 (:type list)
                 :named)
      (ddd 300) (eee 400))
    (defstruct (defstruct-initial-offset-8
                 (:include defstruct-initial-offset-7)
                 (:initial-offset 3)
                 (:type list)
                 :named)
      (fff 500) (ggg 600))
    (values
      (make-defstruct-initial-offset-6)
      (make-defstruct-initial-offset-7)
      (make-defstruct-initial-offset-8)))
  (nil defstruct-initial-offset-6 100 200)
  (nil defstruct-initial-offset-6 100 200
       nil nil defstruct-initial-offset-7 300 400)
  (nil defstruct-initial-offset-6 100 200
       nil nil defstruct-initial-offset-7 300 400
       nil nil nil defstruct-initial-offset-8 500 600))

