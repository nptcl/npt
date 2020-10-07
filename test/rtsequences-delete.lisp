;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Functiono DELETE
;;
(deftest delete-list.1
  (delete 10 nil)
  nil)

(deftest delete-list.2
  (delete 4 '(1 3 4 5 9))
  (1 3 5 9))

(deftest delete-list.3
  (delete 4 '(1 2 4 1 3 4 5 1))
  (1 2 1 3 5 1))

(deftest delete-list.4
  (delete 4 '(1 2 4 1 3 4 5 1) :count 1)
  (1 2 1 3 4 5 1))

(deftest delete-list.5
  (delete 4 '(1 2 4 1 3 4 5 1) :count 1 :from-end t)
  (1 2 4 1 3 5 1))

(deftest delete-list.6
  (delete 3 '(1 2 4 1 3 4 5 1) :test #'>)
  (4 3 4 5))

(deftest delete-list.7
  (delete 3 '(1 2 4 1 3 4 5 1) :test-not #'>)
  (1 2 1 1))

(deftest delete-list.8
  (delete 3 '((1) (4) (5) (1)) :key #'car :test #'>)
  ((4) (5)))

(deftest delete-vector.1
  (delete 10 #())
  #())

(deftest delete-vector.2
  (delete 4 #(1 3 4 5 9))
  #(1 3 5 9))

(deftest delete-vector.3
  (delete 4 #(1 2 4 1 3 4 5 1))
  #(1 2 1 3 5 1))

(deftest delete-vector.4
  (delete 4 #(1 2 4 1 3 4 5 1) :count 1)
  #(1 2 1 3 4 5 1))

(deftest delete-vector.5
  (delete 4 #(1 2 4 1 3 4 5 1) :count 1 :from-end t)
  #(1 2 4 1 3 5 1))

(deftest delete-vector.6
  (delete 3 #(1 2 4 1 3 4 5 1) :test #'>)
  #(4 3 4 5))

(deftest delete-vector.7
  (delete 3 #(1 2 4 1 3 4 5 1) :test-not #'>)
  #(1 2 1 1))

(deftest delete-vector.8
  (delete 3 #((1) (4) (5) (1)) :key #'car :test #'>)
  #((4) (5)))

(deftest delete-start-list.1
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  (4 5 9 8 8 4))

(deftest delete-start-list.2
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  (4 5 9 8 8 4))

(deftest delete-start-list.3
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  (3 4 5 9 8 8 4))

(deftest delete-start-list.4
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  (3 4 5 9 8 8 4))

(deftest delete-start-list.5
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  (3 3 4 5 9 8 8 4))

(deftest delete-start-list.6
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  (3 3 4 5 9 8 8 4))

(deftest delete-start-list.7
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-start-list.8
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-start-list.9
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-list.10
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-start-list.11
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest-error delete-start-list.12
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest delete-start-vector.1
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  #(4 5 9 8 8 4))

(deftest delete-start-vector.2
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-start-vector.3
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  #(3 4 5 9 8 8 4))

(deftest delete-start-vector.4
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  #(3 4 5 9 8 8 4))

(deftest delete-start-vector.5
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  #(3 3 4 5 9 8 8 4))

(deftest delete-start-vector.6
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  #(3 3 4 5 9 8 8 4))

(deftest delete-start-vector.7
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-start-vector.8
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-start-vector.9
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-vector.10
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-start-vector.11
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest-error delete-start-vector.12
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest delete-end-list.1
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil)
  (4 5 9 8 8 4))

(deftest delete-end-list.2
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil :from-end t)
  (4 5 9 8 8 4))

(deftest delete-end-list.3
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  (4 5 9 8 8 4))

(deftest delete-end-list.4
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest delete-end-list.5
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  (4 5 9 8 8 4 3))

(deftest delete-end-list.6
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  (4 5 9 8 8 4 3))

(deftest delete-end-list.7
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end-list.8
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end-list.9
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end-list.10
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end-list.11
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end-list.12
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-end-list.13
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest-error delete-end-list.14
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest delete-end-vector.1
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil)
  #(4 5 9 8 8 4))

(deftest delete-end-vector.2
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-end-vector.3
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  #(4 5 9 8 8 4))

(deftest delete-end-vector.4
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-end-vector.5
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  #(4 5 9 8 8 4 3))

(deftest delete-end-vector.6
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  #(4 5 9 8 8 4 3))

(deftest delete-end-vector.7
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end-vector.8
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end-vector.9
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end-vector.10
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end-vector.11
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end-vector.12
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-end-vector.13
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest-error delete-end-vector.14
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest delete-start-end-list.1
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil)
  (4 5 9 8 8 4))

(deftest delete-start-end-list.2
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil :from-end t)
  (4 5 9 8 8 4))

(deftest delete-start-end-list.3
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil :count 4)
  (4 5 9 8 8 4 3 3 3))

(deftest delete-start-end-list.4
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil :from-end t :count 4)
  (3 3 4 5 9 8 3 8 4))

(deftest delete-start-end-list.5
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13)
  (4 5 9 8 8 4))

(deftest delete-start-end-list.6
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest delete-start-end-list.7
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :count 4)
  (4 5 9 8 8 4 3 3 3))

(deftest delete-start-end-list.8
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :from-end t :count 4)
  (3 3 4 5 9 8 3 8 4))

(deftest delete-start-end-list.9
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11)
  (3 4 5 9 8 8 4 3 3))

(deftest delete-start-end-list.10
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :from-end t)
  (3 4 5 9 8 8 4 3 3))

(deftest delete-start-end-list.11
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :count 2)
  (3 4 5 9 8 8 4 3 3 3 3))

(deftest delete-start-end-list.12
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :from-end t :count 2)
  (3 3 4 5 9 8 3 8 4 3 3))

(deftest delete-start-end-list.13
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-end-list.14
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-end-list.15
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :count 2)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-end-list.16
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :from-end t :count 2)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-end-list.17
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-start-end-list.18
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :from-end t)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-start-end-list.19
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :count 2)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-start-end-list.20
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :from-end t :count 2)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest-error delete-start-end-list.21
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4))

(deftest-error delete-start-end-list.22
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4 :from-end t))

(deftest delete-start-end-vector.1
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil)
  #(4 5 9 8 8 4))

(deftest delete-start-end-vector.2
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-start-end-vector.3
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil :count 4)
  #(4 5 9 8 8 4 3 3 3))

(deftest delete-start-end-vector.4
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil :from-end t :count 4)
  #(3 3 4 5 9 8 3 8 4))

(deftest delete-start-end-vector.5
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13)
  #(4 5 9 8 8 4))

(deftest delete-start-end-vector.6
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-start-end-vector.7
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :count 4)
  #(4 5 9 8 8 4 3 3 3))

(deftest delete-start-end-vector.8
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :from-end t :count 4)
  #(3 3 4 5 9 8 3 8 4))

(deftest delete-start-end-vector.9
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11)
  #(3 4 5 9 8 8 4 3 3))

(deftest delete-start-end-vector.10
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :from-end t)
  #(3 4 5 9 8 8 4 3 3))

(deftest delete-start-end-vector.11
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :count 2)
  #(3 4 5 9 8 8 4 3 3 3 3))

(deftest delete-start-end-vector.12
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :from-end t :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3))

(deftest delete-start-end-vector.13
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-end-vector.14
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-end-vector.15
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-end-vector.16
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :from-end t :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-end-vector.17
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-start-end-vector.18
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :from-end t)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-start-end-vector.19
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :count 2)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-start-end-vector.20
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :from-end t :count 2)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest-error delete-start-end-vector.21
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4))

(deftest-error delete-start-end-vector.22
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4 :from-end t))

(deftest-error delete-error.1
  (eval '(delete 10 20)))

(deftest-error! delete-error.2
  (eval '(delete 10)))

(deftest-error delete-error.3
  (eval '(delete 10 nil nil)))

(deftest-error delete-error.4
  (eval '(delete 10 nil :key)))

(deftest-error delete-error.5
  (eval '(delete 10 nil :key 30)))

(deftest-error delete-error.6
  (eval '(delete 10 nil :hello 30)))

(deftest-error delete-error.7
  (eval '(delete 10 nil :test (constantly t) :test-not (constantly t))))

(deftest-error delete-error.8
  (eval '(delete 10 '(a b c) :start 4)))

(deftest-error delete-error.9
  (eval '(delete 10 '(a b c) :end 4)))

(deftest-error delete-error.10
  (eval '(delete 10 #(a b c) :start 4)))

(deftest-error delete-error.11
  (eval '(delete 10 #(a b c) :end 4)))

(deftest-error delete-error.12
  (eval '(delete 10 #(a b c) :start 3 :end 1)))


;;
;;  Function DELETE-IF
;;
(deftest delete-if-list.1
  (delete-if (constantly t) nil)
  nil)

(deftest delete-if-list.2
  (delete-if (eqlf 4) '(1 3 4 5 9))
  (1 3 5 9))

(deftest delete-if-list.3
  (delete-if (eqlf 4) '(1 2 4 1 3 4 5 1))
  (1 2 1 3 5 1))

(deftest delete-if-list.4
  (delete-if (eqlf 4) '(1 2 4 1 3 4 5 1) :count 1)
  (1 2 1 3 4 5 1))

(deftest delete-if-list.5
  (delete-if (eqlf 4) '(1 2 4 1 3 4 5 1) :count 1 :from-end t)
  (1 2 4 1 3 5 1))

(deftest delete-if-list.6
  (delete-if (lambda (x) (> 3 x)) '((1) (4) (5) (1)) :key #'car)
  ((4) (5)))

(deftest delete-if-vector.1
  (delete-if (constantly t) #())
  #())

(deftest delete-if-vector.2
  (delete-if (eqlf 4) #(1 3 4 5 9))
  #(1 3 5 9))

(deftest delete-if-vector.3
  (delete-if (eqlf 4) #(1 2 4 1 3 4 5 1))
  #(1 2 1 3 5 1))

(deftest delete-if-vector.4
  (delete-if (eqlf 4) #(1 2 4 1 3 4 5 1) :count 1)
  #(1 2 1 3 4 5 1))

(deftest delete-if-vector.5
  (delete-if (eqlf 4) #(1 2 4 1 3 4 5 1) :count 1 :from-end t)
  #(1 2 4 1 3 5 1))

(deftest delete-if-vector.6
  (delete-if (lambda (x) (> 3 x)) #((1) (4) (5) (1)) :key #'car)
  #((4) (5)))

(deftest delete-if-start-list.1
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  (4 5 9 8 8 4))

(deftest delete-if-start-list.2
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  (4 5 9 8 8 4))

(deftest delete-if-start-list.3
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  (3 4 5 9 8 8 4))

(deftest delete-if-start-list.4
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  (3 4 5 9 8 8 4))

(deftest delete-if-start-list.5
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  (3 3 4 5 9 8 8 4))

(deftest delete-if-start-list.6
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  (3 3 4 5 9 8 8 4))

(deftest delete-if-start-list.7
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-if-start-list.8
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-if-start-list.9
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-start-list.10
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-if-start-list.11
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest-error delete-if-start-list.12
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest delete-if-start-vector.1
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  #(4 5 9 8 8 4))

(deftest delete-if-start-vector.2
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-if-start-vector.3
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  #(3 4 5 9 8 8 4))

(deftest delete-if-start-vector.4
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  #(3 4 5 9 8 8 4))

(deftest delete-if-start-vector.5
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  #(3 3 4 5 9 8 8 4))

(deftest delete-if-start-vector.6
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  #(3 3 4 5 9 8 8 4))

(deftest delete-if-start-vector.7
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-if-start-vector.8
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-if-start-vector.9
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-start-vector.10
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-if-start-vector.11
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest-error delete-if-start-vector.12
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest delete-if-end-list.1
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil)
  (4 5 9 8 8 4))

(deftest delete-if-end-list.2
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil :from-end t)
  (4 5 9 8 8 4))

(deftest delete-if-end-list.3
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  (4 5 9 8 8 4))

(deftest delete-if-end-list.4
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest delete-if-end-list.5
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  (4 5 9 8 8 4 3))

(deftest delete-if-end-list.6
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  (4 5 9 8 8 4 3))

(deftest delete-if-end-list.7
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-end-list.8
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-end-list.9
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-end-list.10
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-end-list.11
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-end-list.12
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-if-end-list.13
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest-error delete-if-end-list.14
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest delete-if-end-vector.1
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil)
  #(4 5 9 8 8 4))

(deftest delete-if-end-vector.2
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-if-end-vector.3
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  #(4 5 9 8 8 4))

(deftest delete-if-end-vector.4
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-if-end-vector.5
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  #(4 5 9 8 8 4 3))

(deftest delete-if-end-vector.6
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  #(4 5 9 8 8 4 3))

(deftest delete-if-end-vector.7
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-end-vector.8
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-end-vector.9
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-end-vector.10
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-end-vector.11
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-end-vector.12
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-if-end-vector.13
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest-error delete-if-end-vector.14
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest delete-if-start-end-list.1
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil)
  (4 5 9 8 8 4))

(deftest delete-if-start-end-list.2
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil :from-end t)
  (4 5 9 8 8 4))

(deftest delete-if-start-end-list.3
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil :count 4)
  (4 5 9 8 8 4 3 3 3))

(deftest delete-if-start-end-list.4
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil :from-end t :count 4)
  (3 3 4 5 9 8 3 8 4))

(deftest delete-if-start-end-list.5
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13)
  (4 5 9 8 8 4))

(deftest delete-if-start-end-list.6
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest delete-if-start-end-list.7
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13 :count 4)
  (4 5 9 8 8 4 3 3 3))

(deftest delete-if-start-end-list.8
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13 :from-end t :count 4)
  (3 3 4 5 9 8 3 8 4))

(deftest delete-if-start-end-list.9
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11)
  (3 4 5 9 8 8 4 3 3))

(deftest delete-if-start-end-list.10
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11 :from-end t)
  (3 4 5 9 8 8 4 3 3))

(deftest delete-if-start-end-list.11
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11 :count 2)
  (3 4 5 9 8 8 4 3 3 3 3))

(deftest delete-if-start-end-list.12
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11 :from-end t :count 2)
  (3 3 4 5 9 8 3 8 4 3 3))

(deftest delete-if-start-end-list.13
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-start-end-list.14
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-start-end-list.15
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1 :count 2)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-start-end-list.16
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1 :from-end t :count 2)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-start-end-list.17
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-if-start-end-list.18
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10 :from-end t)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-if-start-end-list.19
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10 :count 2)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-if-start-end-list.20
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10 :from-end t :count 2)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest-error delete-if-start-end-list.21
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 5 :end 4))

(deftest-error delete-if-start-end-list.22
  (delete-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 5 :end 4 :from-end t))

(deftest delete-if-start-end-vector.1
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil)
  #(4 5 9 8 8 4))

(deftest delete-if-start-end-vector.2
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-if-start-end-vector.3
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil :count 4)
  #(4 5 9 8 8 4 3 3 3))

(deftest delete-if-start-end-vector.4
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil :from-end t :count 4)
  #(3 3 4 5 9 8 3 8 4))

(deftest delete-if-start-end-vector.5
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13)
  #(4 5 9 8 8 4))

(deftest delete-if-start-end-vector.6
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-if-start-end-vector.7
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13 :count 4)
  #(4 5 9 8 8 4 3 3 3))

(deftest delete-if-start-end-vector.8
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13 :from-end t :count 4)
  #(3 3 4 5 9 8 3 8 4))

(deftest delete-if-start-end-vector.9
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11)
  #(3 4 5 9 8 8 4 3 3))

(deftest delete-if-start-end-vector.10
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11 :from-end t)
  #(3 4 5 9 8 8 4 3 3))

(deftest delete-if-start-end-vector.11
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11 :count 2)
  #(3 4 5 9 8 8 4 3 3 3 3))

(deftest delete-if-start-end-vector.12
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11 :from-end t :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3))

(deftest delete-if-start-end-vector.13
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-start-end-vector.14
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-start-end-vector.15
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1 :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-start-end-vector.16
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1 :from-end t :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-start-end-vector.17
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-if-start-end-vector.18
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10 :from-end t)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-if-start-end-vector.19
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10 :count 2)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-if-start-end-vector.20
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10 :from-end t :count 2)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest-error delete-if-start-end-vector.21
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 5 :end 4))

(deftest-error delete-if-start-end-vector.22
  (delete-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 5 :end 4 :from-end t))

(deftest-error delete-if-error.1
  (eval '(delete-if (eqlf 10) 20)))

(deftest-error! delete-if-error.2
  (eval '(delete-if (eqlf 10))))

(deftest-error delete-if-error.3
  (eval '(delete-if (eqlf 10) nil nil)))

(deftest-error delete-if-error.4
  (eval '(delete-if (eqlf 10) nil :key)))

(deftest-error delete-if-error.5
  (eval '(delete-if (eqlf 10) nil :key 30)))

(deftest-error delete-if-error.6
  (eval '(delete-if (eqlf 10) nil :hello 30)))

(deftest-error delete-if-error.7
  (eval '(delete-if (eqlf 10) '(a b c) :start 4)))

(deftest-error delete-if-error.8
  (eval '(delete-if (eqlf 10) '(a b c) :end 4)))

(deftest-error delete-if-error.9
  (eval '(delete-if (eqlf 10) #(a b c) :start 4)))

(deftest-error delete-if-error.10
  (eval '(delete-if (eqlf 10) #(a b c) :end 4)))

(deftest-error delete-if-error.11
  (eval '(delete-if (eqlf 10) #(a b c) :start 3 :end 1)))


;;
;;  Function DELETE-IF-NOT
;;
(deftest delete-if-not-list.1
  (delete-if-not (constantly t) nil)
  nil)

(deftest delete-if-not-list.2
  (delete-if-not (noteqlf 4) '(1 3 4 5 9))
  (1 3 5 9))

(deftest delete-if-not-list.3
  (delete-if-not (noteqlf 4) '(1 2 4 1 3 4 5 1))
  (1 2 1 3 5 1))

(deftest delete-if-not-list.4
  (delete-if-not (noteqlf 4) '(1 2 4 1 3 4 5 1) :count 1)
  (1 2 1 3 4 5 1))

(deftest delete-if-not-list.5
  (delete-if-not (noteqlf 4) '(1 2 4 1 3 4 5 1) :count 1 :from-end t)
  (1 2 4 1 3 5 1))

(deftest delete-if-not-list.6
  (delete-if-not (lambda (x) (not (> 3 x))) '((1) (4) (5) (1)) :key #'car)
  ((4) (5)))

(deftest delete-if-not-vector.1
  (delete-if-not (constantly t) #())
  #())

(deftest delete-if-not-vector.2
  (delete-if-not (noteqlf 4) #(1 3 4 5 9))
  #(1 3 5 9))

(deftest delete-if-not-vector.3
  (delete-if-not (noteqlf 4) #(1 2 4 1 3 4 5 1))
  #(1 2 1 3 5 1))

(deftest delete-if-not-vector.4
  (delete-if-not (noteqlf 4) #(1 2 4 1 3 4 5 1) :count 1)
  #(1 2 1 3 4 5 1))

(deftest delete-if-not-vector.5
  (delete-if-not (noteqlf 4) #(1 2 4 1 3 4 5 1) :count 1 :from-end t)
  #(1 2 4 1 3 5 1))

(deftest delete-if-not-vector.6
  (delete-if-not (lambda (x) (not (> 3 x))) #((1) (4) (5) (1)) :key #'car)
  #((4) (5)))

(deftest delete-if-not-start-list.1
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  (4 5 9 8 8 4))

(deftest delete-if-not-start-list.2
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  (4 5 9 8 8 4))

(deftest delete-if-not-start-list.3
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  (3 4 5 9 8 8 4))

(deftest delete-if-not-start-list.4
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  (3 4 5 9 8 8 4))

(deftest delete-if-not-start-list.5
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  (3 3 4 5 9 8 8 4))

(deftest delete-if-not-start-list.6
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  (3 3 4 5 9 8 8 4))

(deftest delete-if-not-start-list.7
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-if-not-start-list.8
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-if-not-start-list.9
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-start-list.10
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-if-not-start-list.11
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest-error delete-if-not-start-list.12
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest delete-if-not-start-vector.1
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  #(4 5 9 8 8 4))

(deftest delete-if-not-start-vector.2
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-if-not-start-vector.3
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  #(3 4 5 9 8 8 4))

(deftest delete-if-not-start-vector.4
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  #(3 4 5 9 8 8 4))

(deftest delete-if-not-start-vector.5
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  #(3 3 4 5 9 8 8 4))

(deftest delete-if-not-start-vector.6
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  #(3 3 4 5 9 8 8 4))

(deftest delete-if-not-start-vector.7
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-if-not-start-vector.8
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-if-not-start-vector.9
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-start-vector.10
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-if-not-start-vector.11
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest-error delete-if-not-start-vector.12
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest delete-if-not-end-list.1
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil)
  (4 5 9 8 8 4))

(deftest delete-if-not-end-list.2
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil :from-end t)
  (4 5 9 8 8 4))

(deftest delete-if-not-end-list.3
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  (4 5 9 8 8 4))

(deftest delete-if-not-end-list.4
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest delete-if-not-end-list.5
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  (4 5 9 8 8 4 3))

(deftest delete-if-not-end-list.6
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  (4 5 9 8 8 4 3))

(deftest delete-if-not-end-list.7
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-end-list.8
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-end-list.9
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-end-list.10
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-end-list.11
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-end-list.12
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-if-not-end-list.13
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest-error delete-if-not-end-list.14
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest delete-if-not-end-vector.1
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil)
  #(4 5 9 8 8 4))

(deftest delete-if-not-end-vector.2
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-if-not-end-vector.3
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  #(4 5 9 8 8 4))

(deftest delete-if-not-end-vector.4
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-if-not-end-vector.5
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  #(4 5 9 8 8 4 3))

(deftest delete-if-not-end-vector.6
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  #(4 5 9 8 8 4 3))

(deftest delete-if-not-end-vector.7
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-end-vector.8
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-end-vector.9
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-end-vector.10
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-end-vector.11
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-end-vector.12
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-if-not-end-vector.13
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest-error delete-if-not-end-vector.14
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest delete-if-not-start-end-list.1
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil)
  (4 5 9 8 8 4))

(deftest delete-if-not-start-end-list.2
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil :from-end t)
  (4 5 9 8 8 4))

(deftest delete-if-not-start-end-list.3
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil :count 4)
  (4 5 9 8 8 4 3 3 3))

(deftest delete-if-not-start-end-list.4
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil :from-end t :count 4)
  (3 3 4 5 9 8 3 8 4))

(deftest delete-if-not-start-end-list.5
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13)
  (4 5 9 8 8 4))

(deftest delete-if-not-start-end-list.6
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest delete-if-not-start-end-list.7
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13 :count 4)
  (4 5 9 8 8 4 3 3 3))

(deftest delete-if-not-start-end-list.8
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13 :from-end t :count 4)
  (3 3 4 5 9 8 3 8 4))

(deftest delete-if-not-start-end-list.9
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11)
  (3 4 5 9 8 8 4 3 3))

(deftest delete-if-not-start-end-list.10
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11 :from-end t)
  (3 4 5 9 8 8 4 3 3))

(deftest delete-if-not-start-end-list.11
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11 :count 2)
  (3 4 5 9 8 8 4 3 3 3 3))

(deftest delete-if-not-start-end-list.12
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11 :from-end t :count 2)
  (3 3 4 5 9 8 3 8 4 3 3))

(deftest delete-if-not-start-end-list.13
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-start-end-list.14
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-start-end-list.15
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1 :count 2)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-start-end-list.16
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1 :from-end t :count 2)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-start-end-list.17
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-if-not-start-end-list.18
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10 :from-end t)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-if-not-start-end-list.19
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10 :count 2)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-if-not-start-end-list.20
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10 :from-end t :count 2)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest-error delete-if-not-start-end-list.21
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 5 :end 4))

(deftest-error delete-if-not-start-end-list.22
  (delete-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 5 :end 4 :from-end t))

(deftest delete-if-not-start-end-vector.1
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil)
  #(4 5 9 8 8 4))

(deftest delete-if-not-start-end-vector.2
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-if-not-start-end-vector.3
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil :count 4)
  #(4 5 9 8 8 4 3 3 3))

(deftest delete-if-not-start-end-vector.4
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil :from-end t :count 4)
  #(3 3 4 5 9 8 3 8 4))

(deftest delete-if-not-start-end-vector.5
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13)
  #(4 5 9 8 8 4))

(deftest delete-if-not-start-end-vector.6
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-if-not-start-end-vector.7
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13 :count 4)
  #(4 5 9 8 8 4 3 3 3))

(deftest delete-if-not-start-end-vector.8
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13 :from-end t :count 4)
  #(3 3 4 5 9 8 3 8 4))

(deftest delete-if-not-start-end-vector.9
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11)
  #(3 4 5 9 8 8 4 3 3))

(deftest delete-if-not-start-end-vector.10
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11 :from-end t)
  #(3 4 5 9 8 8 4 3 3))

(deftest delete-if-not-start-end-vector.11
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11 :count 2)
  #(3 4 5 9 8 8 4 3 3 3 3))

(deftest delete-if-not-start-end-vector.12
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11 :from-end t :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3))

(deftest delete-if-not-start-end-vector.13
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-start-end-vector.14
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-start-end-vector.15
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1 :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-start-end-vector.16
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1 :from-end t :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-if-not-start-end-vector.17
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-if-not-start-end-vector.18
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10 :from-end t)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-if-not-start-end-vector.19
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10 :count 2)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest delete-if-not-start-end-vector.20
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10 :from-end t :count 2)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest-error delete-if-not-start-end-vector.21
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 5 :end 4))

(deftest-error delete-if-not-start-end-vector.22
  (delete-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 5 :end 4 :from-end t))

(deftest-error delete-if-not-error.1
  (eval '(delete-if-not (noteqlf 10) 20)))

(deftest-error! delete-if-not-error.2
  (eval '(delete-if-not (noteqlf 10))))

(deftest-error delete-if-not-error.3
  (eval '(delete-if-not (noteqlf 10) nil nil)))

(deftest-error delete-if-not-error.4
  (eval '(delete-if-not (noteqlf 10) nil :key)))

(deftest-error delete-if-not-error.5
  (eval '(delete-if-not (noteqlf 10) nil :key 30)))

(deftest-error delete-if-not-error.6
  (eval '(delete-if-not (noteqlf 10) nil :hello 30)))

(deftest-error delete-if-not-error.7
  (eval '(delete-if-not (noteqlf 10) '(a b c) :start 4)))

(deftest-error delete-if-not-error.8
  (eval '(delete-if-not (noteqlf 10) '(a b c) :end 4)))

(deftest-error delete-if-not-error.9
  (eval '(delete-if-not (noteqlf 10) #(a b c) :start 4)))

(deftest-error delete-if-not-error.10
  (eval '(delete-if-not (noteqlf 10) #(a b c) :end 4)))

(deftest-error delete-if-not-error.11
  (eval '(delete-if-not (noteqlf 10) #(a b c) :start 3 :end 1)))

