;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function POSITION
;;
(deftest position.1
  (position 10 nil)
  nil)

(deftest position.2
  (position 10 #())
  nil)

(deftest position.3
  (position 'a '(a b c d e))
  0)

(deftest position.4
  (position 'd '(a b c d e))
  3)

(deftest position.5
  (position 'z '(a b c d e))
  nil)

(deftest position.6
  (position 'a #(a b c d e))
  0)

(deftest position.7
  (position 'd #(a b c d e))
  3)

(deftest position.8
  (position 'z #(a b c d e))
  nil)

(deftest position.9
  (position 4 '(1 2 3 4 5 6 7) :key #'1+)
  2)

(deftest position.10
  (position '(4) '((1) (2) (3) (4) (5)))
  nil)

(deftest position.11
  (position '(4) '((1) (2) (3) (4) (5)) :test #'equal)
  3)

(deftest position.12
  (position '(4) '((1) (2) (3) (4) (5)) :test-not #'equal)
  0)

(deftest position.13
  (position '(4) '((4) (4) (3) (4) (5)) :test-not #'equal)
  2)

(deftest position.14
  (position 3 '(1 2 3 1 2 3 1 2) :from-end t)
  5)

(deftest position-range-list.1
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9))
  3)

(deftest position-range-list.2
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :start 3)
  3)

(deftest position-range-list.3
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :start 4)
  6)

(deftest position-range-list.4
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :end 3)
  nil)

(deftest position-range-list.5
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :end 4)
  3)

(deftest position-range-list.6
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :start 4 :end 6)
  nil)

(deftest position-range-list.7
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :start 4 :end 7)
  6)

(deftest position-range-list.8
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :start 4 :end nil)
  6)

(deftest position-range-list.9
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t)
  10)

(deftest position-range-list.10
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :start 11)
  nil)

(deftest position-range-list.11
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :start 10)
  10)

(deftest position-range-list.12
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :end 12)
  10)

(deftest position-range-list.13
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :end 11)
  10)

(deftest position-range-list.14
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :end 10)
  7)

(deftest position-range-list.15
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :start 8 :end 12)
  10)

(deftest position-range-list.16
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :start 8 :end nil)
  10)

(deftest position-range-list.17
  (position 3 '(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :start 8 :end 9)
  nil)

(deftest position-range-vector.1
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9))
  3)

(deftest position-range-vector.2
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :start 3)
  3)

(deftest position-range-vector.3
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :start 4)
  6)

(deftest position-range-vector.4
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :end 3)
  nil)

(deftest position-range-vector.5
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :end 4)
  3)

(deftest position-range-vector.6
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :start 4 :end 6)
  nil)

(deftest position-range-vector.7
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :start 4 :end 7)
  6)

(deftest position-range-vector.8
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :start 4 :end nil)
  6)

(deftest position-range-vector.9
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t)
  10)

(deftest position-range-vector.10
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :start 11)
  nil)

(deftest position-range-vector.11
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :start 10)
  10)

(deftest position-range-vector.12
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :end 12)
  10)

(deftest position-range-vector.13
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :end 11)
  10)

(deftest position-range-vector.14
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :end 10)
  7)

(deftest position-range-vector.15
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :start 8 :end 12)
  10)

(deftest position-range-vector.16
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :start 8 :end nil)
  10)

(deftest position-range-vector.17
  (position 3 #(0 1 2 3 4 5 3 3 6 7 3 9) :from-end t :start 8 :end 9)
  nil)

(deftest-error position-error.1
  (eval '(position 3 10))
  type-error)

(deftest-error! position-error.2
  (eval '(position 3)))

(deftest-error position-error.3
  (eval '(position 3 nil nil)))

(deftest-error position-error.4
  (eval '(position 3 nil :key)))

(deftest-error position-error.5
  (eval '(position 3 nil :key 10)))

(deftest-error position-error.6
  (eval '(position 3 nil :hello 10)))

(deftest-error position-error.7
  (eval '(position 3 nil :test (constantly t) :test-not (constantly t))))

(deftest position-error.8
  (eval '(position 3 '(a b c) :start 3))
  nil)

(deftest-error position-error.9
  (eval '(position 3 '(a b c) :start 4)))

(deftest position-error.10
  (eval '(position 3 '(a b c) :end 3))
  nil)

(deftest-error position-error.11
  (eval '(position 3 '(a b c) :end 4)))

(deftest-error position-error.12
  (eval '(position 3 '(a b c) :start 2 :end 1)))


;;
;;  Function POSITION-IF
;;
(deftest position-if.1
  (position-if #'evenp '(1 3 5 4 9))
  3)

(deftest position-if.2
  (position-if (lambda (x) (eql x 10)) nil)
  nil)

(deftest position-if.3
  (position-if (lambda (x) (eql x 10)) #())
  nil)

(deftest position-if.4
  (position-if (lambda (x) (eq x 'a)) '(a b c d e))
  0)

(deftest position-if.5
  (position-if (lambda (x) (eq x 'd)) '(a b c d e))
  3)

(deftest position-if.6
  (position-if (lambda (x) (eq x 'z)) '(a b c d e))
  nil)

(deftest position-if.7
  (position-if (lambda (x) (eq x 'a)) #(a b c d e))
  0)

(deftest position-if.8
  (position-if (lambda (x) (eq x 'd)) #(a b c d e))
  3)

(deftest position-if.9
  (position-if (lambda (x) (eq x 'z)) #(a b c d e))
  nil)

(deftest position-if.10
  (position-if (lambda (x) (eql x 4)) '(1 2 3 4 5 6 7) :key #'1+)
  2)

(deftest position-if.11
  (position-if (lambda (x) (equal x '(4))) '((1) (2) (3) (4) (5)))
  3)

(deftest position-if.12
  (position-if (lambda (x) (eql x 3)) '(1 2 3 1 2 3 1 2) :from-end t)
  5)

(deftest position-if-range-list.1
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9))
  3)

(deftest position-if-range-list.2
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :start 3)
  3)

(deftest position-if-range-list.3
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :start 4)
  6)

(deftest position-if-range-list.4
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :end 3)
  nil)

(deftest position-if-range-list.5
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :end 4)
  3)

(deftest position-if-range-list.6
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :start 4 :end 6)
  nil)

(deftest position-if-range-list.7
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :start 4 :end 7)
  6)

(deftest position-if-range-list.8
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :start 4 :end nil)
  6)

(deftest position-if-range-list.9
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t)
  10)

(deftest position-if-range-list.10
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :start 11)
  nil)

(deftest position-if-range-list.11
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :start 10)
  10)

(deftest position-if-range-list.12
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :end 12)
  10)

(deftest position-if-range-list.13
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :end 11)
  10)

(deftest position-if-range-list.14
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :end 10)
  7)

(deftest position-if-range-list.15
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :start 8 :end 12)
  10)

(deftest position-if-range-list.16
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :start 8 :end nil)
  10)

(deftest position-if-range-list.17
  (position-if (lambda (x) (eql x 3)) '(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :start 8 :end 9)
  nil)

(deftest position-if-range-vector.1
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9))
  3)

(deftest position-if-range-vector.2
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :start 3)
  3)

(deftest position-if-range-vector.3
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :start 4)
  6)

(deftest position-if-range-vector.4
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :end 3)
  nil)

(deftest position-if-range-vector.5
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :end 4)
  3)

(deftest position-if-range-vector.6
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :start 4 :end 6)
  nil)

(deftest position-if-range-vector.7
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :start 4 :end 7)
  6)

(deftest position-if-range-vector.8
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :start 4 :end nil)
  6)

(deftest position-if-range-vector.9
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t)
  10)

(deftest position-if-range-vector.10
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :start 11)
  nil)

(deftest position-if-range-vector.11
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :start 10)
  10)

(deftest position-if-range-vector.12
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :end 12)
  10)

(deftest position-if-range-vector.13
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :end 11)
  10)

(deftest position-if-range-vector.14
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :end 10)
  7)

(deftest position-if-range-vector.15
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :start 8 :end 12)
  10)

(deftest position-if-range-vector.16
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :start 8 :end nil)
  10)

(deftest position-if-range-vector.17
  (position-if (lambda (x) (eql x 3)) #(0 1 2 3 4 5 3 3 6 7 3 9)
               :from-end t :start 8 :end 9)
  nil)

(deftest-error position-if-error.1
  (eval '(position-if 10 nil))
  type-error)

(deftest-error position-if-error.2
  (eval '(position-if (constantly t) 20))
  type-error)

(deftest-error! position-if-error.3
  (eval '(position-if (constantly t))))

(deftest-error position-if-error.4
  (eval '(position-if (constantly t) nil nil)))

(deftest-error position-if-error.5
  (eval '(position-if (constantly t) nil :key)))

(deftest-error position-if-error.6
  (eval '(position-if (constantly t) nil :key 10)))

(deftest-error position-if-error.7
  (eval '(position-if (constantly t) nil :hello 10)))

(deftest position-if-error.8
  (eval '(position-if (constantly t) '(a b c) :start 3))
  nil)

(deftest-error position-if-error.9
  (eval '(position-if (constantly t) '(a b c) :start 4)))

(deftest position-if-error.10
  (eval '(position-if (lambda (x) (eql x 'z)) '(a b c) :end 3))
  nil)

(deftest-error position-if-error.11
  (eval '(position-if (constantly nil) '(a b c) :end 4)))

(deftest-error position-if-error.12
  (eval '(position-if (constantly t) '(a b c) :start 2 :end 1)))


;;
;;  Function POSITION-IF-NOT
;;
(deftest position-if-not.1
  (position-if-not #'oddp #(1 3 5 4 9))
  3)

(deftest position-if-not.2
  (position-if-not (lambda (x) (not (eql x 10))) nil)
  nil)

(deftest position-if-not.3
  (position-if-not (lambda (x) (not (eql x 10))) #())
  nil)

(deftest position-if-not.4
  (position-if-not (lambda (x) (not (eq x 'a))) '(a b c d e))
  0)

(deftest position-if-not.5
  (position-if-not (lambda (x) (not (eq x 'd))) '(a b c d e))
  3)

(deftest position-if-not.6
  (position-if-not (lambda (x) (not (eq x 'z))) '(a b c d e))
  nil)

(deftest position-if-not.7
  (position-if-not (lambda (x) (not (eq x 'a))) #(a b c d e))
  0)

(deftest position-if-not.8
  (position-if-not (lambda (x) (not (eq x 'd))) #(a b c d e))
  3)

(deftest position-if-not.9
  (position-if-not (lambda (x) (not (eq x 'z))) #(a b c d e))
  nil)

(deftest position-if-not.10
  (position-if-not (lambda (x) (not (eql x 4))) '(1 2 3 4 5 6 7) :key #'1+)
  2)

(deftest position-if-not.11
  (position-if-not (lambda (x) (not (equal x '(4)))) '((1) (2) (3) (4) (5)))
  3)

(deftest position-if-not.12
  (position-if-not (lambda (x) (not (eql x 3))) '(1 2 3 1 2 3 1 2) :from-end t)
  5)

(deftest position-if-not-range-list.1
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9))
  3)

(deftest position-if-not-range-list.2
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :start 3)
  3)

(deftest position-if-not-range-list.3
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :start 4)
  6)

(deftest position-if-not-range-list.4
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :end 3)
  nil)

(deftest position-if-not-range-list.5
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :end 4)
  3)

(deftest position-if-not-range-list.6
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :start 4 :end 6)
  nil)

(deftest position-if-not-range-list.7
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :start 4 :end 7)
  6)

(deftest position-if-not-range-list.8
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :start 4 :end nil)
  6)

(deftest position-if-not-range-list.9
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t)
  10)

(deftest position-if-not-range-list.10
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :start 11)
  nil)

(deftest position-if-not-range-list.11
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :start 10)
  10)

(deftest position-if-not-range-list.12
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :end 12)
  10)

(deftest position-if-not-range-list.13
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :end 11)
  10)

(deftest position-if-not-range-list.14
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :end 10)
  7)

(deftest position-if-not-range-list.15
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :start 8 :end 12)
  10)

(deftest position-if-not-range-list.16
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :start 8 :end nil)
  10)

(deftest position-if-not-range-list.17
  (position-if-not (lambda (x) (not (eql x 3))) '(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :start 8 :end 9)
  nil)

(deftest position-if-not-range-vector.1
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9))
  3)

(deftest position-if-not-range-vector.2
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :start 3)
  3)

(deftest position-if-not-range-vector.3
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :start 4)
  6)

(deftest position-if-not-range-vector.4
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :end 3)
  nil)

(deftest position-if-not-range-vector.5
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :end 4)
  3)

(deftest position-if-not-range-vector.6
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :start 4 :end 6)
  nil)

(deftest position-if-not-range-vector.7
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :start 4 :end 7)
  6)

(deftest position-if-not-range-vector.8
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :start 4 :end nil)
  6)

(deftest position-if-not-range-vector.9
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t)
  10)

(deftest position-if-not-range-vector.10
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :start 11)
  nil)

(deftest position-if-not-range-vector.11
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :start 10)
  10)

(deftest position-if-not-range-vector.12
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :end 12)
  10)

(deftest position-if-not-range-vector.13
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :end 11)
  10)

(deftest position-if-not-range-vector.14
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :end 10)
  7)

(deftest position-if-not-range-vector.15
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :start 8 :end 12)
  10)

(deftest position-if-not-range-vector.16
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :start 8 :end nil)
  10)

(deftest position-if-not-range-vector.17
  (position-if-not (lambda (x) (not (eql x 3))) #(0 1 2 3 4 5 3 3 6 7 3 9)
                   :from-end t :start 8 :end 9)
  nil)

(deftest-error position-if-not-error.1
  (eval '(position-if-not 10 nil))
  type-error)

(deftest-error position-if-not-error.2
  (eval '(position-if-not (constantly nil) 20))
  type-error)

(deftest-error! position-if-not-error.3
  (eval '(position-if-not (constantly nil))))

(deftest-error position-if-not-error.4
  (eval '(position-if-not (constantly nil) nil nil)))

(deftest-error position-if-not-error.5
  (eval '(position-if-not (constantly nil) nil :key)))

(deftest-error position-if-not-error.6
  (eval '(position-if-not (constantly nil) nil :key 10)))

(deftest-error position-if-not-error.7
  (eval '(position-if-not (constantly nil) nil :hello 10)))

(deftest position-if-not-error.8
  (eval '(position-if-not (constantly nil) '(a b c) :start 3))
  nil)

(deftest-error position-if-not-error.9
  (eval '(position-if-not (constantly nil) '(a b c) :start 4)))

(deftest position-if-not-error.10
  (eval '(position-if-not (constantly t) '(a b c) :end 3))
  nil)

(deftest-error position-if-not-error.11
  (eval '(position-if-not (constantly t) '(a b c) :end 4)))

(deftest-error position-if-not-error.12
  (eval '(position-if-not (constantly nil) '(a b c) :start 2 :end 1)))


;;  ANSI Common Lisp
(deftest position-test.1
  (position #\a "baobab" :from-end t)
  4)

(deftest position-test.2
  (position-if #'oddp '((1) (2) (3) (4)) :start 1 :key #'car)
  2)

(deftest position-test.3
  (position 595 '())
  nil)

(deftest position-test.4
  (position-if-not #'integerp '(1 2 3 4 5.0))
  4)

