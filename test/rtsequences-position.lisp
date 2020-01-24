;;
;;  ANSI COMMON LISP: 17. Sequences
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
  (position 'a #(a b c d e))
  0)

(deftest position.6
  (position 'd #(a b c d e))
  3)

(deftest position-start.1
  (position 3 '(0 1 2 3 4 5 6 7 8 9 0))
  3)

(deftest position-start.2
  (position 3 '(0 1 2 3 4 5 6 7 8 9 0) :start 2)
  3)

(deftest position-start.3
  (position 3 '(0 1 2 3 4 5 6 7 8 9 0) :end 6)
  3)

(deftest position-start.4
  (position 3 '(0 1 2 3 4 5 6 7 8 9 0) :start 2 :end 6)
  3)

(deftest position-start.5
  (position 3 '(0 1 2 3 4 5 6 7 8 9 0) :from-end t)
  3)

(deftest position-start.6
  (position 3 '(0 1 2 3 4 5 6 7 8 9 0) :from-end t :start 2)
  3)

(deftest position-start.7
  (position 3 '(0 1 2 3 4 5 6 7 8 9 0) :from-end t :end 6)
  3)

(deftest position-start.8
  (position 3 '(0 1 2 3 4 5 6 7 8 9 0) :from-end t :start 2 :end 6)
  3)

(deftest position-start.9
  (position 3 #(0 1 2 3 4 5 6 7 8 9 0))
  3)

(deftest position-start.10
  (position 3 #(0 1 2 3 4 5 6 7 8 9 0) :start 2)
  3)

(deftest position-start.11
  (position 3 #(0 1 2 3 4 5 6 7 8 9 0) :end 6)
  3)

(deftest position-start.12
  (position 3 #(0 1 2 3 4 5 6 7 8 9 0) :start 2 :end 6)
  3)

(deftest position-start.13
  (position 3 #(0 1 2 3 4 5 6 7 8 9 0) :from-end t)
  3)

(deftest position-start.14
  (position 3 #(0 1 2 3 4 5 6 7 8 9 0) :from-end t :start 2)
  3)

(deftest position-start.15
  (position 3 #(0 1 2 3 4 5 6 7 8 9 0) :from-end t :end 6)
  3)

(deftest position-start.16
  (position 3 #(0 1 2 3 4 5 6 7 8 9 0) :from-end t :start 2 :end 6)
  3)

(deftest position-if.1
  (position-if #'evenp '(1 3 5 4 9))
  3)

(deftest position-if.2
  (position-if-not #'oddp #(1 3 5 4 9))
  3)

(deftest position-if-start.1
  (position-if
    (lambda (x) (= 3 x)) '(0 1 2 3 4 5 6 7 8 9 0))
  3)

(deftest position-if-start.2
  (position-if
    (lambda (x) (= 3 x)) '(0 1 2 3 4 5 6 7 8 9 0) :start 2)
  3)

(deftest position-if-start.3
  (position-if
    (lambda (x) (= 3 x)) '(0 1 2 3 4 5 6 7 8 9 0) :end 6)
  3)

(deftest position-if-start.4
  (position-if
    (lambda (x) (= 3 x)) '(0 1 2 3 4 5 6 7 8 9 0) :start 2 :end 6)
  3)

(deftest position-if-start.5
  (position-if
    (lambda (x) (= 3 x)) '(0 1 2 3 4 5 6 7 8 9 0) :from-end t)
  3)

(deftest position-if-start.6
  (position-if
    (lambda (x) (= 3 x)) '(0 1 2 3 4 5 6 7 8 9 0) :from-end t :start 2)
  3)

(deftest position-if-start.7
  (position-if
    (lambda (x) (= 3 x)) '(0 1 2 3 4 5 6 7 8 9 0) :from-end t :end 6)
  3)

(deftest position-if-start.8
  (position-if
    (lambda (x) (= 3 x)) '(0 1 2 3 4 5 6 7 8 9 0) :from-end t :start 2 :end 6)
  3)

(deftest position-if-start.9
  (position-if
    (lambda (x) (= 3 x)) #(0 1 2 3 4 5 6 7 8 9 0))
  3)

(deftest position-if-start.10
  (position-if
    (lambda (x) (= 3 x)) #(0 1 2 3 4 5 6 7 8 9 0) :start 2)
  3)

(deftest position-if-start.11
  (position-if
    (lambda (x) (= 3 x)) #(0 1 2 3 4 5 6 7 8 9 0) :end 6)
  3)

(deftest position-if-start.12
  (position-if
    (lambda (x) (= 3 x)) #(0 1 2 3 4 5 6 7 8 9 0) :start 2 :end 6)
  3)

(deftest position-if-start.13
  (position-if
    (lambda (x) (= 3 x)) #(0 1 2 3 4 5 6 7 8 9 0) :from-end t)
  3)

(deftest position-if-start.14
  (position-if
    (lambda (x) (= 3 x)) #(0 1 2 3 4 5 6 7 8 9 0) :from-end t :start 2)
  3)

(deftest position-if-start.15
  (position-if
    (lambda (x) (= 3 x)) #(0 1 2 3 4 5 6 7 8 9 0) :from-end t :end 6)
  3)

(deftest position-if-start.16
  (position-if
    (lambda (x) (= 3 x)) #(0 1 2 3 4 5 6 7 8 9 0) :from-end t :start 2 :end 6)
  3)


(deftest search-list.1
  (search nil '(a b c d))
  0)

(deftest search-list.2
  (search '(a) '(a b c d))
  0)

(deftest search-list.3
  (search '(b) '(a b c d))
  1)

(deftest search-list.4
  (search '(z) '(a b c d))
  nil)

(deftest search-list.5
  (search '(a b c d e) '(a b c d))
  nil)

(deftest search-list.6
  (search '(b c) '(a b c d))
  1)

(deftest search-list.7
  (search '(b d) '(a b c d))
  nil)

(deftest search-vector.1
  (search "" "abcd")
  0)

(deftest search-vector.2
  (search "a" "abcd")
  0)

(deftest search-vector.3
  (search "b" "abcd")
  1)

(deftest search-vector.4
  (search "z" "abcd")
  nil)

(deftest search-vector.5
  (search "abcde" "abcd")
  nil)

(deftest search-vector.6
  (search "bc" "abcd")
  1)

(deftest search-vector.7
  (search "bd" "abcd")
  nil)

(deftest search-list-end.1
  (search nil '(a b c d) :from-end t)
  0)

(deftest search-list-end.2
  (search '(a) '(a b c d) :from-end t)
  0)

(deftest search-list-end.3
  (search '(b) '(a b c d) :from-end t)
  1)

(deftest search-list-end.4
  (search '(z) '(a b c d) :from-end t)
  nil)

(deftest search-list-end.5
  (search '(a b c d e) '(a b c d) :from-end t)
  nil)

(deftest search-list-end.6
  (search '(b c) '(a b c d) :from-end t)
  1)

(deftest search-list-end.7
  (search '(b d) '(a b c d) :from-end t)
  nil)

(deftest search-vector-end.1
  (search "" "abcd" :from-end t)
  0)

(deftest search-vector-end.2
  (search "a" "abcd" :from-end t)
  0)

(deftest search-vector-end.3
  (search "b" "abcd" :from-end t)
  1)

(deftest search-vector-end.4
  (search "z" "abcd" :from-end t)
  nil)

(deftest search-vector-end.5
  (search "abcde" "abcd" :from-end t)
  nil)

(deftest search-vector-end.6
  (search "bc" "abcd" :from-end t)
  1)

(deftest search-vector-end.7
  (search "bd" "abcd" :from-end t)
  nil)


(deftest search-start2.1
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0))
  3)

(deftest search-start2.2
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 2)
  3)

(deftest search-start2.3
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :end2 6)
  3)

(deftest search-start2.4
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 2 :end2 6)
  3)

(deftest search-start2.5
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :from-end t)
  3)

(deftest search-start2.6
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :from-end t :start2 2)
  3)

(deftest search-start2.7
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :from-end t :end2 6)
  3)

(deftest search-start2.8
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :from-end t :start2 2 :end2 6)
  3)

(deftest search-start2.9
  (search '(3 4) #(0 1 2 3 4 5 6 7 8 9 0))
  3)

(deftest search-start2.10
  (search '(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 2)
  3)

(deftest search-start2.11
  (search '(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :end2 6)
  3)

(deftest search-start2.12
  (search '(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 2 :end2 6)
  3)

(deftest search-start2.13
  (search '(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :from-end t)
  3)

(deftest search-start2.14
  (search '(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :from-end t :start2 2)
  3)

(deftest search-start2.15
  (search '(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :from-end t :end2 6)
  3)

(deftest search-start2.16
  (search '(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :from-end t :start2 2 :end2 6)
  3)


(deftest search-start.1
  (search '(1 1 7 8 9 9 9 10) '(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4)
  7)

(deftest search-start.2
  (search '(1 1 7 8 9 9 9 10) '(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :start2 2)
  7)

(deftest search-start.3
  (search '(1 1 7 8 9 9 9 10) '(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :end2 9)
  7)

(deftest search-start.4
  (search '(1 1 7 8 9 9 9 10) '(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :start2 2 :end2 9)
  7)

(deftest search-start.5
  (search '(1 1 7 8 9 9 9 10) '(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :from-end t)
  7)

(deftest search-start.6
  (search '(1 1 7 8 9 9 9 10) '(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :from-end t :start2 2)
  7)

(deftest search-start.7
  (search '(1 1 7 8 9 9 9 10) '(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :from-end t :end2 9)
  7)

(deftest search-start.8
  (search '(1 1 7 8 9 9 9 10) '(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :from-end t :start2 2 :end2 9)
  7)

(deftest search-start.9
  (search '(1 1 7 8 9 9 9 10) #(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4)
  7)

(deftest search-start.10
  (search '(1 1 7 8 9 9 9 10) #(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :start2 2)
  7)

(deftest search-start.11
  (search '(1 1 7 8 9 9 9 10) #(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :end2 9)
  7)

(deftest search-start.12
  (search '(1 1 7 8 9 9 9 10) #(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :start2 2 :end2 9)
  7)

(deftest search-start.13
  (search '(1 1 7 8 9 9 9 10) #(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :from-end t)
  7)

(deftest search-start.14
  (search '(1 1 7 8 9 9 9 10) #(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :from-end t :start2 2)
  7)

(deftest search-start.15
  (search '(1 1 7 8 9 9 9 10) #(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :from-end t :end2 9)
  7)

(deftest search-start.16
  (search '(1 1 7 8 9 9 9 10) #(0 1 2 3 4 5 6 7 8 9 0)
          :start1 2 :end1 4 :from-end t :start2 2 :end2 9)
  7)

