;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Functiono REMOVE
;;
(deftest remove-list.1
  (remove 10 nil)
  nil)

(deftest remove-list.2
  (remove 4 '(1 3 4 5 9))
  (1 3 5 9))

(deftest remove-list.3
  (remove 4 '(1 2 4 1 3 4 5 1))
  (1 2 1 3 5 1))

(deftest remove-list.4
  (remove 4 '(1 2 4 1 3 4 5 1) :count 1)
  (1 2 1 3 4 5 1))

(deftest remove-list.5
  (remove 4 '(1 2 4 1 3 4 5 1) :count 1 :from-end t)
  (1 2 4 1 3 5 1))

(deftest remove-list.6
  (remove 3 '(1 2 4 1 3 4 5 1) :test #'>)
  (4 3 4 5))

(deftest remove-list.7
  (remove 3 '(1 2 4 1 3 4 5 1) :test-not #'>)
  (1 2 1 1))

(deftest remove-list.8
  (remove 3 '((1) (4) (5) (1)) :key #'car :test #'>)
  ((4) (5)))

(deftest remove-vector.1
  (remove 10 #())
  #())

(deftest remove-vector.2
  (remove 4 #(1 3 4 5 9))
  #(1 3 5 9))

(deftest remove-vector.3
  (remove 4 #(1 2 4 1 3 4 5 1))
  #(1 2 1 3 5 1))

(deftest remove-vector.4
  (remove 4 #(1 2 4 1 3 4 5 1) :count 1)
  #(1 2 1 3 4 5 1))

(deftest remove-vector.5
  (remove 4 #(1 2 4 1 3 4 5 1) :count 1 :from-end t)
  #(1 2 4 1 3 5 1))

(deftest remove-vector.6
  (remove 3 #(1 2 4 1 3 4 5 1) :test #'>)
  #(4 3 4 5))

(deftest remove-vector.7
  (remove 3 #(1 2 4 1 3 4 5 1) :test-not #'>)
  #(1 2 1 1))

(deftest remove-vector.8
  (remove 3 #((1) (4) (5) (1)) :key #'car :test #'>)
  #((4) (5)))

(deftest remove-start-list.1
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  (4 5 9 8 8 4))

(deftest remove-start-list.2
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  (4 5 9 8 8 4))

(deftest remove-start-list.3
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  (3 4 5 9 8 8 4))

(deftest remove-start-list.4
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  (3 4 5 9 8 8 4))

(deftest remove-start-list.5
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  (3 3 4 5 9 8 8 4))

(deftest remove-start-list.6
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  (3 3 4 5 9 8 8 4))

(deftest remove-start-list.7
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-start-list.8
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-start-list.9
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-list.10
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-start-list.11
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest-error remove-start-list.12
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest remove-start-vector.1
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  #(4 5 9 8 8 4))

(deftest remove-start-vector.2
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-start-vector.3
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  #(3 4 5 9 8 8 4))

(deftest remove-start-vector.4
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  #(3 4 5 9 8 8 4))

(deftest remove-start-vector.5
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  #(3 3 4 5 9 8 8 4))

(deftest remove-start-vector.6
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  #(3 3 4 5 9 8 8 4))

(deftest remove-start-vector.7
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-start-vector.8
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-start-vector.9
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-vector.10
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-start-vector.11
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest-error remove-start-vector.12
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest remove-end-list.1
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil)
  (4 5 9 8 8 4))

(deftest remove-end-list.2
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil :from-end t)
  (4 5 9 8 8 4))

(deftest remove-end-list.3
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  (4 5 9 8 8 4))

(deftest remove-end-list.4
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest remove-end-list.5
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  (4 5 9 8 8 4 3))

(deftest remove-end-list.6
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  (4 5 9 8 8 4 3))

(deftest remove-end-list.7
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end-list.8
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end-list.9
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end-list.10
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end-list.11
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end-list.12
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-end-list.13
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest-error remove-end-list.14
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest remove-end-vector.1
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil)
  #(4 5 9 8 8 4))

(deftest remove-end-vector.2
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-end-vector.3
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  #(4 5 9 8 8 4))

(deftest remove-end-vector.4
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-end-vector.5
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  #(4 5 9 8 8 4 3))

(deftest remove-end-vector.6
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  #(4 5 9 8 8 4 3))

(deftest remove-end-vector.7
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end-vector.8
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end-vector.9
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end-vector.10
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end-vector.11
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end-vector.12
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-end-vector.13
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest-error remove-end-vector.14
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest remove-start-end-list.1
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil)
  (4 5 9 8 8 4))

(deftest remove-start-end-list.2
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil :from-end t)
  (4 5 9 8 8 4))

(deftest remove-start-end-list.3
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil :count 4)
  (4 5 9 8 8 4 3 3 3))

(deftest remove-start-end-list.4
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil :from-end t :count 4)
  (3 3 4 5 9 8 3 8 4))

(deftest remove-start-end-list.5
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13)
  (4 5 9 8 8 4))

(deftest remove-start-end-list.6
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest remove-start-end-list.7
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :count 4)
  (4 5 9 8 8 4 3 3 3))

(deftest remove-start-end-list.8
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :from-end t :count 4)
  (3 3 4 5 9 8 3 8 4))

(deftest remove-start-end-list.9
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11)
  (3 4 5 9 8 8 4 3 3))

(deftest remove-start-end-list.10
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :from-end t)
  (3 4 5 9 8 8 4 3 3))

(deftest remove-start-end-list.11
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :count 2)
  (3 4 5 9 8 8 4 3 3 3 3))

(deftest remove-start-end-list.12
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :from-end t :count 2)
  (3 3 4 5 9 8 3 8 4 3 3))

(deftest remove-start-end-list.13
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-end-list.14
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-end-list.15
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :count 2)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-end-list.16
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :from-end t :count 2)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-end-list.17
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-start-end-list.18
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :from-end t)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-start-end-list.19
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :count 2)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-start-end-list.20
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :from-end t :count 2)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest-error remove-start-end-list.21
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4))

(deftest-error remove-start-end-list.22
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4 :from-end t))

(deftest remove-start-end-vector.1
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil)
  #(4 5 9 8 8 4))

(deftest remove-start-end-vector.2
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-start-end-vector.3
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil :count 4)
  #(4 5 9 8 8 4 3 3 3))

(deftest remove-start-end-vector.4
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end nil :from-end t :count 4)
  #(3 3 4 5 9 8 3 8 4))

(deftest remove-start-end-vector.5
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13)
  #(4 5 9 8 8 4))

(deftest remove-start-end-vector.6
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-start-end-vector.7
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :count 4)
  #(4 5 9 8 8 4 3 3 3))

(deftest remove-start-end-vector.8
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :from-end t :count 4)
  #(3 3 4 5 9 8 3 8 4))

(deftest remove-start-end-vector.9
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11)
  #(3 4 5 9 8 8 4 3 3))

(deftest remove-start-end-vector.10
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :from-end t)
  #(3 4 5 9 8 8 4 3 3))

(deftest remove-start-end-vector.11
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :count 2)
  #(3 4 5 9 8 8 4 3 3 3 3))

(deftest remove-start-end-vector.12
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :from-end t :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3))

(deftest remove-start-end-vector.13
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-end-vector.14
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-end-vector.15
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-end-vector.16
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :from-end t :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-end-vector.17
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-start-end-vector.18
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :from-end t)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-start-end-vector.19
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :count 2)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-start-end-vector.20
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :from-end t :count 2)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest-error remove-start-end-vector.21
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4))

(deftest-error remove-start-end-vector.22
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4 :from-end t))

(deftest-error remove-error.1
  (eval '(remove 10 20)))

(deftest-error! remove-error.2
  (eval '(remove 10)))

(deftest-error remove-error.3
  (eval '(remove 10 nil nil)))

(deftest-error remove-error.4
  (eval '(remove 10 nil :key)))

(deftest-error remove-error.5
  (eval '(remove 10 nil :key 30)))

(deftest-error remove-error.6
  (eval '(remove 10 nil :hello 30)))

(deftest-error remove-error.7
  (eval '(remove 10 nil :test (constantly t) :test-not (constantly t))))

(deftest-error remove-error.8
  (eval '(remove 10 '(a b c) :start 4)))

(deftest-error remove-error.9
  (eval '(remove 10 '(a b c) :end 4)))

(deftest-error remove-error.10
  (eval '(remove 10 #(a b c) :start 4)))

(deftest-error remove-error.11
  (eval '(remove 10 #(a b c) :end 4)))

(deftest-error remove-error.12
  (eval '(remove 10 #(a b c) :start 3 :end 1)))


;;
;;  Function REMOVE-IF
;;
(deftest remove-if-list.1
  (remove-if (constantly t) nil)
  nil)

(deftest remove-if-list.2
  (remove-if (eqlf 4) '(1 3 4 5 9))
  (1 3 5 9))

(deftest remove-if-list.3
  (remove-if (eqlf 4) '(1 2 4 1 3 4 5 1))
  (1 2 1 3 5 1))

(deftest remove-if-list.4
  (remove-if (eqlf 4) '(1 2 4 1 3 4 5 1) :count 1)
  (1 2 1 3 4 5 1))

(deftest remove-if-list.5
  (remove-if (eqlf 4) '(1 2 4 1 3 4 5 1) :count 1 :from-end t)
  (1 2 4 1 3 5 1))

(deftest remove-if-list.6
  (remove-if (lambda (x) (> 3 x)) '((1) (4) (5) (1)) :key #'car)
  ((4) (5)))

(deftest remove-if-vector.1
  (remove-if (constantly t) #())
  #())

(deftest remove-if-vector.2
  (remove-if (eqlf 4) #(1 3 4 5 9))
  #(1 3 5 9))

(deftest remove-if-vector.3
  (remove-if (eqlf 4) #(1 2 4 1 3 4 5 1))
  #(1 2 1 3 5 1))

(deftest remove-if-vector.4
  (remove-if (eqlf 4) #(1 2 4 1 3 4 5 1) :count 1)
  #(1 2 1 3 4 5 1))

(deftest remove-if-vector.5
  (remove-if (eqlf 4) #(1 2 4 1 3 4 5 1) :count 1 :from-end t)
  #(1 2 4 1 3 5 1))

(deftest remove-if-vector.6
  (remove-if (lambda (x) (> 3 x)) #((1) (4) (5) (1)) :key #'car)
  #((4) (5)))

(deftest remove-if-start-list.1
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  (4 5 9 8 8 4))

(deftest remove-if-start-list.2
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  (4 5 9 8 8 4))

(deftest remove-if-start-list.3
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  (3 4 5 9 8 8 4))

(deftest remove-if-start-list.4
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  (3 4 5 9 8 8 4))

(deftest remove-if-start-list.5
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  (3 3 4 5 9 8 8 4))

(deftest remove-if-start-list.6
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  (3 3 4 5 9 8 8 4))

(deftest remove-if-start-list.7
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-if-start-list.8
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-if-start-list.9
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-start-list.10
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-if-start-list.11
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest-error remove-if-start-list.12
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest remove-if-start-vector.1
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  #(4 5 9 8 8 4))

(deftest remove-if-start-vector.2
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-if-start-vector.3
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  #(3 4 5 9 8 8 4))

(deftest remove-if-start-vector.4
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  #(3 4 5 9 8 8 4))

(deftest remove-if-start-vector.5
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  #(3 3 4 5 9 8 8 4))

(deftest remove-if-start-vector.6
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  #(3 3 4 5 9 8 8 4))

(deftest remove-if-start-vector.7
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-if-start-vector.8
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-if-start-vector.9
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-start-vector.10
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-if-start-vector.11
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest-error remove-if-start-vector.12
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest remove-if-end-list.1
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil)
  (4 5 9 8 8 4))

(deftest remove-if-end-list.2
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil :from-end t)
  (4 5 9 8 8 4))

(deftest remove-if-end-list.3
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  (4 5 9 8 8 4))

(deftest remove-if-end-list.4
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest remove-if-end-list.5
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  (4 5 9 8 8 4 3))

(deftest remove-if-end-list.6
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  (4 5 9 8 8 4 3))

(deftest remove-if-end-list.7
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-end-list.8
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-end-list.9
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-end-list.10
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-end-list.11
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-end-list.12
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-if-end-list.13
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest-error remove-if-end-list.14
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest remove-if-end-vector.1
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil)
  #(4 5 9 8 8 4))

(deftest remove-if-end-vector.2
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-if-end-vector.3
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  #(4 5 9 8 8 4))

(deftest remove-if-end-vector.4
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-if-end-vector.5
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  #(4 5 9 8 8 4 3))

(deftest remove-if-end-vector.6
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  #(4 5 9 8 8 4 3))

(deftest remove-if-end-vector.7
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-end-vector.8
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-end-vector.9
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-end-vector.10
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-end-vector.11
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-end-vector.12
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-if-end-vector.13
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest-error remove-if-end-vector.14
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest remove-if-start-end-list.1
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil)
  (4 5 9 8 8 4))

(deftest remove-if-start-end-list.2
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil :from-end t)
  (4 5 9 8 8 4))

(deftest remove-if-start-end-list.3
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil :count 4)
  (4 5 9 8 8 4 3 3 3))

(deftest remove-if-start-end-list.4
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil :from-end t :count 4)
  (3 3 4 5 9 8 3 8 4))

(deftest remove-if-start-end-list.5
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13)
  (4 5 9 8 8 4))

(deftest remove-if-start-end-list.6
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest remove-if-start-end-list.7
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13 :count 4)
  (4 5 9 8 8 4 3 3 3))

(deftest remove-if-start-end-list.8
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13 :from-end t :count 4)
  (3 3 4 5 9 8 3 8 4))

(deftest remove-if-start-end-list.9
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11)
  (3 4 5 9 8 8 4 3 3))

(deftest remove-if-start-end-list.10
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11 :from-end t)
  (3 4 5 9 8 8 4 3 3))

(deftest remove-if-start-end-list.11
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11 :count 2)
  (3 4 5 9 8 8 4 3 3 3 3))

(deftest remove-if-start-end-list.12
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11 :from-end t :count 2)
  (3 3 4 5 9 8 3 8 4 3 3))

(deftest remove-if-start-end-list.13
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-start-end-list.14
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-start-end-list.15
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1 :count 2)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-start-end-list.16
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1 :from-end t :count 2)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-start-end-list.17
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-if-start-end-list.18
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10 :from-end t)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-if-start-end-list.19
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10 :count 2)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-if-start-end-list.20
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10 :from-end t :count 2)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest-error remove-if-start-end-list.21
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 5 :end 4))

(deftest-error remove-if-start-end-list.22
  (remove-if (eqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 5 :end 4 :from-end t))

(deftest remove-if-start-end-vector.1
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil)
  #(4 5 9 8 8 4))

(deftest remove-if-start-end-vector.2
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-if-start-end-vector.3
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil :count 4)
  #(4 5 9 8 8 4 3 3 3))

(deftest remove-if-start-end-vector.4
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end nil :from-end t :count 4)
  #(3 3 4 5 9 8 3 8 4))

(deftest remove-if-start-end-vector.5
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13)
  #(4 5 9 8 8 4))

(deftest remove-if-start-end-vector.6
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-if-start-end-vector.7
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13 :count 4)
  #(4 5 9 8 8 4 3 3 3))

(deftest remove-if-start-end-vector.8
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 0 :end 13 :from-end t :count 4)
  #(3 3 4 5 9 8 3 8 4))

(deftest remove-if-start-end-vector.9
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11)
  #(3 4 5 9 8 8 4 3 3))

(deftest remove-if-start-end-vector.10
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11 :from-end t)
  #(3 4 5 9 8 8 4 3 3))

(deftest remove-if-start-end-vector.11
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11 :count 2)
  #(3 4 5 9 8 8 4 3 3 3 3))

(deftest remove-if-start-end-vector.12
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 11 :from-end t :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3))

(deftest remove-if-start-end-vector.13
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-start-end-vector.14
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-start-end-vector.15
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1 :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-start-end-vector.16
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 1 :end 1 :from-end t :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-start-end-vector.17
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-if-start-end-vector.18
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10 :from-end t)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-if-start-end-vector.19
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10 :count 2)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-if-start-end-vector.20
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 4 :end 10 :from-end t :count 2)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest-error remove-if-start-end-vector.21
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 5 :end 4))

(deftest-error remove-if-start-end-vector.22
  (remove-if (eqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
             :start 5 :end 4 :from-end t))

(deftest-error remove-if-error.1
  (eval '(remove-if (eqlf 10) 20)))

(deftest-error! remove-if-error.2
  (eval '(remove-if (eqlf 10))))

(deftest-error remove-if-error.3
  (eval '(remove-if (eqlf 10) nil nil)))

(deftest-error remove-if-error.4
  (eval '(remove-if (eqlf 10) nil :key)))

(deftest-error remove-if-error.5
  (eval '(remove-if (eqlf 10) nil :key 30)))

(deftest-error remove-if-error.6
  (eval '(remove-if (eqlf 10) nil :hello 30)))

(deftest-error remove-if-error.7
  (eval '(remove-if (eqlf 10) '(a b c) :start 4)))

(deftest-error remove-if-error.8
  (eval '(remove-if (eqlf 10) '(a b c) :end 4)))

(deftest-error remove-if-error.9
  (eval '(remove-if (eqlf 10) #(a b c) :start 4)))

(deftest-error remove-if-error.10
  (eval '(remove-if (eqlf 10) #(a b c) :end 4)))

(deftest-error remove-if-error.11
  (eval '(remove-if (eqlf 10) #(a b c) :start 3 :end 1)))


;;
;;  Function REMOVE-IF-NOT
;;
(deftest remove-if-not-list.1
  (remove-if-not (constantly t) nil)
  nil)

(deftest remove-if-not-list.2
  (remove-if-not (noteqlf 4) '(1 3 4 5 9))
  (1 3 5 9))

(deftest remove-if-not-list.3
  (remove-if-not (noteqlf 4) '(1 2 4 1 3 4 5 1))
  (1 2 1 3 5 1))

(deftest remove-if-not-list.4
  (remove-if-not (noteqlf 4) '(1 2 4 1 3 4 5 1) :count 1)
  (1 2 1 3 4 5 1))

(deftest remove-if-not-list.5
  (remove-if-not (noteqlf 4) '(1 2 4 1 3 4 5 1) :count 1 :from-end t)
  (1 2 4 1 3 5 1))

(deftest remove-if-not-list.6
  (remove-if-not (lambda (x) (not (> 3 x))) '((1) (4) (5) (1)) :key #'car)
  ((4) (5)))

(deftest remove-if-not-vector.1
  (remove-if-not (constantly t) #())
  #())

(deftest remove-if-not-vector.2
  (remove-if-not (noteqlf 4) #(1 3 4 5 9))
  #(1 3 5 9))

(deftest remove-if-not-vector.3
  (remove-if-not (noteqlf 4) #(1 2 4 1 3 4 5 1))
  #(1 2 1 3 5 1))

(deftest remove-if-not-vector.4
  (remove-if-not (noteqlf 4) #(1 2 4 1 3 4 5 1) :count 1)
  #(1 2 1 3 4 5 1))

(deftest remove-if-not-vector.5
  (remove-if-not (noteqlf 4) #(1 2 4 1 3 4 5 1) :count 1 :from-end t)
  #(1 2 4 1 3 5 1))

(deftest remove-if-not-vector.6
  (remove-if-not (lambda (x) (not (> 3 x))) #((1) (4) (5) (1)) :key #'car)
  #((4) (5)))

(deftest remove-if-not-start-list.1
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  (4 5 9 8 8 4))

(deftest remove-if-not-start-list.2
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  (4 5 9 8 8 4))

(deftest remove-if-not-start-list.3
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  (3 4 5 9 8 8 4))

(deftest remove-if-not-start-list.4
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  (3 4 5 9 8 8 4))

(deftest remove-if-not-start-list.5
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  (3 3 4 5 9 8 8 4))

(deftest remove-if-not-start-list.6
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  (3 3 4 5 9 8 8 4))

(deftest remove-if-not-start-list.7
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-if-not-start-list.8
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-if-not-start-list.9
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-start-list.10
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-if-not-start-list.11
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest-error remove-if-not-start-list.12
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest remove-if-not-start-vector.1
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  #(4 5 9 8 8 4))

(deftest remove-if-not-start-vector.2
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-if-not-start-vector.3
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  #(3 4 5 9 8 8 4))

(deftest remove-if-not-start-vector.4
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  #(3 4 5 9 8 8 4))

(deftest remove-if-not-start-vector.5
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  #(3 3 4 5 9 8 8 4))

(deftest remove-if-not-start-vector.6
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  #(3 3 4 5 9 8 8 4))

(deftest remove-if-not-start-vector.7
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-if-not-start-vector.8
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-if-not-start-vector.9
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-start-vector.10
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-if-not-start-vector.11
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest-error remove-if-not-start-vector.12
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest remove-if-not-end-list.1
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil)
  (4 5 9 8 8 4))

(deftest remove-if-not-end-list.2
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil :from-end t)
  (4 5 9 8 8 4))

(deftest remove-if-not-end-list.3
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  (4 5 9 8 8 4))

(deftest remove-if-not-end-list.4
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest remove-if-not-end-list.5
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  (4 5 9 8 8 4 3))

(deftest remove-if-not-end-list.6
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  (4 5 9 8 8 4 3))

(deftest remove-if-not-end-list.7
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-end-list.8
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-end-list.9
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-end-list.10
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-end-list.11
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-end-list.12
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-if-not-end-list.13
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest-error remove-if-not-end-list.14
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest remove-if-not-end-vector.1
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil)
  #(4 5 9 8 8 4))

(deftest remove-if-not-end-vector.2
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end nil :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-if-not-end-vector.3
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  #(4 5 9 8 8 4))

(deftest remove-if-not-end-vector.4
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-if-not-end-vector.5
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  #(4 5 9 8 8 4 3))

(deftest remove-if-not-end-vector.6
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  #(4 5 9 8 8 4 3))

(deftest remove-if-not-end-vector.7
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-end-vector.8
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-end-vector.9
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-end-vector.10
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-end-vector.11
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-end-vector.12
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-if-not-end-vector.13
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest-error remove-if-not-end-vector.14
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest remove-if-not-start-end-list.1
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil)
  (4 5 9 8 8 4))

(deftest remove-if-not-start-end-list.2
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil :from-end t)
  (4 5 9 8 8 4))

(deftest remove-if-not-start-end-list.3
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil :count 4)
  (4 5 9 8 8 4 3 3 3))

(deftest remove-if-not-start-end-list.4
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil :from-end t :count 4)
  (3 3 4 5 9 8 3 8 4))

(deftest remove-if-not-start-end-list.5
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13)
  (4 5 9 8 8 4))

(deftest remove-if-not-start-end-list.6
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest remove-if-not-start-end-list.7
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13 :count 4)
  (4 5 9 8 8 4 3 3 3))

(deftest remove-if-not-start-end-list.8
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13 :from-end t :count 4)
  (3 3 4 5 9 8 3 8 4))

(deftest remove-if-not-start-end-list.9
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11)
  (3 4 5 9 8 8 4 3 3))

(deftest remove-if-not-start-end-list.10
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11 :from-end t)
  (3 4 5 9 8 8 4 3 3))

(deftest remove-if-not-start-end-list.11
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11 :count 2)
  (3 4 5 9 8 8 4 3 3 3 3))

(deftest remove-if-not-start-end-list.12
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11 :from-end t :count 2)
  (3 3 4 5 9 8 3 8 4 3 3))

(deftest remove-if-not-start-end-list.13
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-start-end-list.14
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-start-end-list.15
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1 :count 2)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-start-end-list.16
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1 :from-end t :count 2)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-start-end-list.17
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-if-not-start-end-list.18
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10 :from-end t)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-if-not-start-end-list.19
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10 :count 2)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-if-not-start-end-list.20
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10 :from-end t :count 2)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest-error remove-if-not-start-end-list.21
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 5 :end 4))

(deftest-error remove-if-not-start-end-list.22
  (remove-if-not (noteqlf 3) '(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 5 :end 4 :from-end t))

(deftest remove-if-not-start-end-vector.1
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil)
  #(4 5 9 8 8 4))

(deftest remove-if-not-start-end-vector.2
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-if-not-start-end-vector.3
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil :count 4)
  #(4 5 9 8 8 4 3 3 3))

(deftest remove-if-not-start-end-vector.4
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end nil :from-end t :count 4)
  #(3 3 4 5 9 8 3 8 4))

(deftest remove-if-not-start-end-vector.5
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13)
  #(4 5 9 8 8 4))

(deftest remove-if-not-start-end-vector.6
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-if-not-start-end-vector.7
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13 :count 4)
  #(4 5 9 8 8 4 3 3 3))

(deftest remove-if-not-start-end-vector.8
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 0 :end 13 :from-end t :count 4)
  #(3 3 4 5 9 8 3 8 4))

(deftest remove-if-not-start-end-vector.9
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11)
  #(3 4 5 9 8 8 4 3 3))

(deftest remove-if-not-start-end-vector.10
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11 :from-end t)
  #(3 4 5 9 8 8 4 3 3))

(deftest remove-if-not-start-end-vector.11
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11 :count 2)
  #(3 4 5 9 8 8 4 3 3 3 3))

(deftest remove-if-not-start-end-vector.12
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 11 :from-end t :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3))

(deftest remove-if-not-start-end-vector.13
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-start-end-vector.14
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-start-end-vector.15
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1 :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-start-end-vector.16
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 1 :end 1 :from-end t :count 2)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-if-not-start-end-vector.17
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-if-not-start-end-vector.18
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10 :from-end t)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-if-not-start-end-vector.19
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10 :count 2)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest remove-if-not-start-end-vector.20
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 4 :end 10 :from-end t :count 2)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest-error remove-if-not-start-end-vector.21
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 5 :end 4))

(deftest-error remove-if-not-start-end-vector.22
  (remove-if-not (noteqlf 3) #(3 3 4 5 9 8 3 8 4 3 3 3 3)
                 :start 5 :end 4 :from-end t))

(deftest-error remove-if-not-error.1
  (eval '(remove-if-not (noteqlf 10) 20)))

(deftest-error! remove-if-not-error.2
  (eval '(remove-if-not (noteqlf 10))))

(deftest-error remove-if-not-error.3
  (eval '(remove-if-not (noteqlf 10) nil nil)))

(deftest-error remove-if-not-error.4
  (eval '(remove-if-not (noteqlf 10) nil :key)))

(deftest-error remove-if-not-error.5
  (eval '(remove-if-not (noteqlf 10) nil :key 30)))

(deftest-error remove-if-not-error.6
  (eval '(remove-if-not (noteqlf 10) nil :hello 30)))

(deftest-error remove-if-not-error.7
  (eval '(remove-if-not (noteqlf 10) '(a b c) :start 4)))

(deftest-error remove-if-not-error.8
  (eval '(remove-if-not (noteqlf 10) '(a b c) :end 4)))

(deftest-error remove-if-not-error.9
  (eval '(remove-if-not (noteqlf 10) #(a b c) :start 4)))

(deftest-error remove-if-not-error.10
  (eval '(remove-if-not (noteqlf 10) #(a b c) :end 4)))

(deftest-error remove-if-not-error.11
  (eval '(remove-if-not (noteqlf 10) #(a b c) :start 3 :end 1)))

