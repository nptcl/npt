;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function FIND
;;
(deftest find.1
  (find #\c nil)
  nil)

(deftest find.2
  (find #\c #())
  nil)

(deftest find.3
  (find #\c "abcdefg")
  #\c)

(deftest find.4
  (find #\z "abcdefg")
  nil)

(deftest find.5
  (find 20 '(10 20 30 40))
  20)

(deftest find.6
  (find 100 '(10 20 30 40))
  nil)

(deftest find.7
  (find 31 '(10 20 30 40) :key #'1+)
  30)

(deftest find.8
  (find '(a) #((z) (a a) (b) (c) (a)) :test #'equal)
  (a))

(deftest find.9
  (find '(a) #((z) (a a) (b) (c) (a)) :test-not #'equal)
  (z))

(deftest find.10
  (find 20 '(10 20 30 40) :from-end t)
  20)

(deftest find.11
  (find 20 #(10 20 30 40) :from-end t)
  20)

(deftest find-range-list.1
  (find 3 '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9)) :key #'car)
  (3 4))

(deftest find-range-list.2
  (find 3 '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9)) :key #'car :start 0)
  (3 4))

(deftest find-range-list.3
  (find 3 '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9)) :key #'car :start 2)
  (3 5))

(deftest find-range-list.4
  (find 3 '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9)) :key #'car :start 2 :end nil)
  (3 5))

(deftest find-range-list.5
  (find 3 '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9)) :key #'car :start 2 :end 2)
  nil)

(deftest find-range-list.6
  (find 3 '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9)) :key #'car :start 2 :end 3)
  (3 5))

(deftest find-range-list.7
  (find 3 '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :from-end t)
  (3 6))

(deftest find-range-list.8
  (find 3 '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :start 0 :from-end t)
  (3 6))

(deftest find-range-list.9
  (find 3 '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :start 2 :from-end t)
  (3 6))

(deftest find-range-list.10
  (find 3 '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :start 2 :end nil :from-end t)
  (3 6))

(deftest find-range-list.11
  (find 3 '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :start 2 :end 2 :from-end t)
  nil)

(deftest find-range-list.12
  (find 3 '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :start 1 :end 2 :from-end t)
  (3 4))

(deftest find-range-list.13
  (find 3 '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :start 1 :end 3 :from-end t)
  (3 5))

(deftest find-range-vector.1
  (find 3 #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9)) :key #'car)
  (3 4))

(deftest find-range-vector.2
  (find 3 #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9)) :key #'car :start 0)
  (3 4))

(deftest find-range-vector.3
  (find 3 #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9)) :key #'car :start 2)
  (3 5))

(deftest find-range-vector.4
  (find 3 #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9)) :key #'car :start 2 :end nil)
  (3 5))

(deftest find-range-vector.5
  (find 3 #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9)) :key #'car :start 2 :end 2)
  nil)

(deftest find-range-vector.6
  (find 3 #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9)) :key #'car :start 2 :end 3)
  (3 5))

(deftest find-range-vector.7
  (find 3 #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :from-end t)
  (3 6))

(deftest find-range-vector.8
  (find 3 #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :start 0 :from-end t)
  (3 6))

(deftest find-range-vector.9
  (find 3 #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :start 2 :from-end t)
  (3 6))

(deftest find-range-vector.10
  (find 3 #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :start 2 :end nil :from-end t)
  (3 6))

(deftest find-range-vector.11
  (find 3 #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :start 2 :end 2 :from-end t)
  nil)

(deftest find-range-vector.12
  (find 3 #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :start 1 :end 2 :from-end t)
  (3 4))

(deftest find-range-vector.13
  (find 3 #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
        :key #'car :start 1 :end 3 :from-end t)
  (3 5))

(deftest-error find-error.1
  (eval '(find 10 20))
  type-error)

(deftest-error! find-error.2
  (eval '(find 10)))

(deftest-error find-error.3
  (eval '(find 10 nil nil)))

(deftest-error find-error.4
  (eval '(find 10 nil :key)))

(deftest-error find-error.5
  (eval '(find 10 nil :key 10)))

(deftest-error find-error.6
  (eval '(find 10 nil :hello 10)))

(deftest-error find-error.7
  (eval '(find 10 nil :test (constantly t) :test-not (constantly t))))

(deftest find-error.8
  (find 10 '(a b c) :start 3)
  nil)

(deftest-error find-error.9
  (find 10 '(a b c) :start 4))

(deftest find-error.10
  (find 10 '(a b c) :end 3)
  nil)

(deftest-error find-error.11
  (find 10 '(a b c) :end 4))

(deftest find-error.12
  (find 10 '(a b c) :start 2 :end 2)
  nil)

(deftest-error find-error.13
  (find 10 '(a b c) :start 2 :end 1))


;;
;;  Function FIND-IF
;;
(deftest find-if.1
  (find-if (lambda (x) (equal x "aa")) '("zz" "aaa" "bb" "aa" "cc"))
  "aa")

(deftest find-if.2
  (find-if (lambda (x) (equal x "ba")) '("zz" "aaa" "bb" "aa" "cc"))
  nil)

(deftest find-if.3
  (find-if (lambda (x) (equal x "aa")) #("zz" "aaa" "bb" "aa" "cc"))
  "aa")

(deftest find-if.4
  (find-if (lambda (x) (equal x "ba")) #("zz" "aaa" "bb" "aa" "cc"))
  nil)

(deftest find-if.5
  (find-if (lambda (x) (eql x #\c)) nil)
  nil)

(deftest find-if.6
  (find-if (lambda (x) (eql x #\c)) #())
  nil)

(deftest find-if.7
  (find-if (lambda (x) (eql x #\c)) "abcdefg")
  #\c)

(deftest find-if.8
  (find-if (lambda (x) (eql x #\z)) "abcdefg")
  nil)

(deftest find-if.9
  (find-if (lambda (x) (eql x 20)) '(10 20 30 40))
  20)

(deftest find-if.10
  (find-if (lambda (x) (eql x 100)) '(10 20 30 40))
  nil)

(deftest find-if.11
  (find-if (lambda (x) (eql x 31)) '(10 20 30 40) :key #'1+)
  30)

(deftest find-if.12
  (find-if (lambda (x) (eql x 20)) '(10 20 30 40) :from-end t)
  20)

(deftest find-if.13
  (find-if (lambda (x) (eql x 20)) #(10 20 30 40) :from-end t)
  20)

(deftest find-if-range-list.1
  (find-if (lambda (x) (eql x 3)) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car)
  (3 4))

(deftest find-if-range-list.2
  (find-if (lambda (x) (eql x 3)) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 0)
  (3 4))

(deftest find-if-range-list.3
  (find-if (lambda (x) (eql x 3)) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2)
  (3 5))

(deftest find-if-range-list.4
  (find-if (lambda (x) (eql x 3)) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2 :end nil)
  (3 5))

(deftest find-if-range-list.5
  (find-if (lambda (x) (eql x 3)) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2 :end 2)
  nil)

(deftest find-if-range-list.6
  (find-if (lambda (x) (eql x 3)) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2 :end 3)
  (3 5))

(deftest find-if-range-list.7
  (find-if (lambda (x) (eql x 3)) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :from-end t)
  (3 6))

(deftest find-if-range-list.8
  (find-if (lambda (x) (eql x 3)) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 0 :from-end t)
  (3 6))

(deftest find-if-range-list.9
  (find-if (lambda (x) (eql x 3)) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2 :from-end t)
  (3 6))

(deftest find-if-range-list.10
  (find-if (lambda (x) (eql x 3)) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2 :end nil :from-end t)
  (3 6))

(deftest find-if-range-list.11
  (find-if (lambda (x) (eql x 3)) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2 :end 2 :from-end t)
  nil)

(deftest find-if-range-list.12
  (find-if (lambda (x) (eql x 3)) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 1 :end 2 :from-end t)
  (3 4))

(deftest find-if-range-list.13
  (find-if (lambda (x) (eql x 3)) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 1 :end 3 :from-end t)
  (3 5))

(deftest find-if-range-vector.1
  (find-if (lambda (x) (eql x 3)) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car)
  (3 4))

(deftest find-if-range-vector.2
  (find-if (lambda (x) (eql x 3)) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 0)
  (3 4))

(deftest find-if-range-vector.3
  (find-if (lambda (x) (eql x 3)) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2)
  (3 5))

(deftest find-if-range-vector.4
  (find-if (lambda (x) (eql x 3)) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2 :end nil)
  (3 5))

(deftest find-if-range-vector.5
  (find-if (lambda (x) (eql x 3)) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2 :end 2)
  nil)

(deftest find-if-range-vector.6
  (find-if (lambda (x) (eql x 3)) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2 :end 3)
  (3 5))

(deftest find-if-range-vector.7
  (find-if (lambda (x) (eql x 3)) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :from-end t)
  (3 6))

(deftest find-if-range-vector.8
  (find-if (lambda (x) (eql x 3)) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 0 :from-end t)
  (3 6))

(deftest find-if-range-vector.9
  (find-if (lambda (x) (eql x 3)) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2 :from-end t)
  (3 6))

(deftest find-if-range-vector.10
  (find-if (lambda (x) (eql x 3)) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2 :end nil :from-end t)
  (3 6))

(deftest find-if-range-vector.11
  (find-if (lambda (x) (eql x 3)) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 2 :end 2 :from-end t)
  nil)

(deftest find-if-range-vector.12
  (find-if (lambda (x) (eql x 3)) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 1 :end 2 :from-end t)
  (3 4))

(deftest find-if-range-vector.13
  (find-if (lambda (x) (eql x 3)) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
           :key #'car :start 1 :end 3 :from-end t)
  (3 5))

(deftest-error find-if-error.1
  (eval '(find-if 10 nil))
  type-error)

(deftest-error find-if-error.2
  (eval '(find-if (constantly t) 20))
  type-error)

(deftest-error! find-if-error.3
  (eval '(find-if (constantly t))))

(deftest-error find-if-error.4
  (eval '(find-if (constantly t) nil nil)))

(deftest-error find-if-error.5
  (eval '(find-if (constantly t) nil :key)))

(deftest-error find-if-error.6
  (eval '(find-if (constantly t) nil :key 10)))

(deftest-error find-if-error.7
  (eval '(find-if (constantly t) nil :hello 10)))

(deftest-error find-if-error.8
  (eval '(find-if (constantly t) nil :test (constantly t) :test-not (constantly t))))

(deftest find-if-error.9
  (find-if (constantly nil) '(a b c) :start 3)
  nil)

(deftest-error find-if-error.10
  (find-if (constantly nil) '(a b c) :start 4))

(deftest find-if-error.11
  (find-if (constantly nil) '(a b c) :end 3)
  nil)

(deftest-error find-if-error.12
  (find-if (constantly nil) '(a b c) :end 4))

(deftest find-if-error.13
  (find-if (constantly nil) '(a b c) :start 2 :end 2)
  nil)

(deftest-error find-if-error.14
  (find (constantly nil) '(a b c) :start 2 :end 1))


;;
;;  Function FIND-IF-NOT
;;
(deftest find-if-not.1
  (find-if-not (lambda (x) (equal x "zz")) '("zz" "aaa" "bb" "aa" "cc"))
  "aaa")

(deftest find-if-not.2
  (find-if-not (constantly t) '("zz" "aaa" "bb" "aa" "cc"))
  nil)

(deftest find-if-not.3
  (find-if-not (lambda (x) (equal x "zz")) #("zz" "aaa" "bb" "aa" "cc"))
  "aaa")

(deftest find-if-not.4
  (find-if-not (constantly t) #("zz" "aaa" "bb" "aa" "cc"))
  nil)

(deftest find-if-not.5
  (find-if-not (lambda (x) (eql x #\c)) nil)
  nil)

(deftest find-if-not.6
  (find-if-not (lambda (x) (eql x #\c)) #())
  nil)

(deftest find-if-not.7
  (find-if-not (lambda (x) (not (eql x #\c))) "abcdefg")
  #\c)

(deftest find-if-not.8
  (find-if-not (lambda (x) (not (eql x #\z))) "abcdefg")
  nil)

(deftest find-if-not.9
  (find-if-not (lambda (x) (not (eql x 20))) '(10 20 30 40))
  20)

(deftest find-if-not.10
  (find-if-not (lambda (x) (not (eql x 100))) '(10 20 30 40))
  nil)

(deftest find-if-not.11
  (find-if-not (lambda (x) (not (eql x 31))) '(10 20 30 40) :key #'1+)
  30)

(deftest find-if-not.12
  (find-if-not (lambda (x) (not (eql x 20))) '(10 20 30 40) :from-end t)
  20)

(deftest find-if-not.13
  (find-if-not (lambda (x) (not (eql x 20))) #(10 20 30 40) :from-end t)
  20)

(deftest find-if-not-range-list.1
  (find-if-not (lambda (x) (not (eql x 3))) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car)
  (3 4))

(deftest find-if-not-range-list.2
  (find-if-not (lambda (x) (not (eql x 3))) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 0)
  (3 4))

(deftest find-if-not-range-list.3
  (find-if-not (lambda (x) (not (eql x 3))) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2)
  (3 5))

(deftest find-if-not-range-list.4
  (find-if-not (lambda (x) (not (eql x 3))) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2 :end nil)
  (3 5))

(deftest find-if-not-range-list.5
  (find-if-not (lambda (x) (not (eql x 3))) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2 :end 2)
  nil)

(deftest find-if-not-range-list.6
  (find-if-not (lambda (x) (not (eql x 3))) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2 :end 3)
  (3 5))

(deftest find-if-not-range-list.7
  (find-if-not (lambda (x) (not (eql x 3))) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :from-end t)
  (3 6))

(deftest find-if-not-range-list.8
  (find-if-not (lambda (x) (not (eql x 3))) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 0 :from-end t)
  (3 6))

(deftest find-if-not-range-list.9
  (find-if-not (lambda (x) (not (eql x 3))) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2 :from-end t)
  (3 6))

(deftest find-if-not-range-list.10
  (find-if-not (lambda (x) (not (eql x 3))) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2 :end nil :from-end t)
  (3 6))

(deftest find-if-not-range-list.11
  (find-if-not (lambda (x) (not (eql x 3))) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2 :end 2 :from-end t)
  nil)

(deftest find-if-not-range-list.12
  (find-if-not (lambda (x) (not (eql x 3))) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 1 :end 2 :from-end t)
  (3 4))

(deftest find-if-not-range-list.13
  (find-if-not (lambda (x) (not (eql x 3))) '((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 1 :end 3 :from-end t)
  (3 5))

(deftest find-if-not-range-vector.1
  (find-if-not (lambda (x) (not (eql x 3))) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car)
  (3 4))

(deftest find-if-not-range-vector.2
  (find-if-not (lambda (x) (not (eql x 3))) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 0)
  (3 4))

(deftest find-if-not-range-vector.3
  (find-if-not (lambda (x) (not (eql x 3))) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2)
  (3 5))

(deftest find-if-not-range-vector.4
  (find-if-not (lambda (x) (not (eql x 3))) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2 :end nil)
  (3 5))

(deftest find-if-not-range-vector.5
  (find-if-not (lambda (x) (not (eql x 3))) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2 :end 2)
  nil)

(deftest find-if-not-range-vector.6
  (find-if-not (lambda (x) (not (eql x 3))) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2 :end 3)
  (3 5))

(deftest find-if-not-range-vector.7
  (find-if-not (lambda (x) (not (eql x 3))) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :from-end t)
  (3 6))

(deftest find-if-not-range-vector.8
  (find-if-not (lambda (x) (not (eql x 3))) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 0 :from-end t)
  (3 6))

(deftest find-if-not-range-vector.9
  (find-if-not (lambda (x) (not (eql x 3))) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2 :from-end t)
  (3 6))

(deftest find-if-not-range-vector.10
  (find-if-not (lambda (x) (not (eql x 3))) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2 :end nil :from-end t)
  (3 6))

(deftest find-if-not-range-vector.11
  (find-if-not (lambda (x) (not (eql x 3))) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 2 :end 2 :from-end t)
  nil)

(deftest find-if-not-range-vector.12
  (find-if-not (lambda (x) (not (eql x 3))) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 1 :end 2 :from-end t)
  (3 4))

(deftest find-if-not-range-vector.13
  (find-if-not (lambda (x) (not (eql x 3))) #((1 2) (3 4) (3 5) (4 5) (3 6) (9 9))
               :key #'car :start 1 :end 3 :from-end t)
  (3 5))

(deftest-error find-if-not-error.1
  (eval '(find-if-not 10 nil))
  type-error)

(deftest-error find-if-not-error.2
  (eval '(find-if-not (constantly t) 20))
  type-error)

(deftest-error! find-if-not-error.3
  (eval '(find-if-not (constantly t))))

(deftest-error find-if-not-error.4
  (eval '(find-if-not (constantly t) nil nil)))

(deftest-error find-if-not-error.5
  (eval '(find-if-not (constantly t) nil :key)))

(deftest-error find-if-not-error.6
  (eval '(find-if-not (constantly t) nil :key 10)))

(deftest-error find-if-not-error.7
  (eval '(find-if-not (constantly t) nil :hello 10)))

(deftest-error find-if-not-error.8
  (eval '(find-if-not (constantly t) nil :test (constantly t) :test-not (constantly t))))

(deftest find-if-not-error.9
  (find-if-not (constantly t) '(a b c) :start 3)
  nil)

(deftest-error find-if-not-error.10
  (find-if-not (constantly t) '(a b c) :start 4))

(deftest find-if-not-error.11
  (find-if-not (constantly t) '(a b c) :end 3)
  nil)

(deftest-error find-if-not-error.12
  (find-if-not (constantly t) '(a b c) :end 4))

(deftest find-if-not-error.13
  (find-if-not (constantly t) '(a b c) :start 2 :end 2)
  nil)

(deftest-error find-if-not-error.14
  (find (constantly t) '(a b c) :start 2 :end 1))


;;  ANSI Common Lisp
(deftest find-test.1
  (find #\d "here are some letters that can be looked at" :test #'char>)
  #\Space)

(deftest find-test.2
  (find-if #'oddp '(1 2 3 4 5) :end 3 :from-end t)
  3)

(deftest find-test.3
  (find-if-not #'complexp '#(3.5 2 #C(1.0 0.0) #C(0.0 1.0)) :start 2)
  nil)

