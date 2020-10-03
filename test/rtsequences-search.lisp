;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function SEARCH
;;
(deftest search.1
  (search nil nil)
  0)

(deftest search.2
  (search '(a b c) nil)
  nil)

(deftest search.3
  (search nil '(a b c))
  0)

(deftest search.4
  (search '(c d) '(a b c d e f g h))
  2)

(deftest search.5
  (search #(c d) '(a b c d e f g h))
  2)

(deftest search.6
  (search '(c d) #(a b c d e f g h))
  2)

(deftest search.7
  (search #(c d) #(a b c d e f g h))
  2)

(deftest search.8
  (search "hello" "abcdhelloefghellohijk")
  4)

(deftest search.9
  (search '(4 5) '(1 2 3 4 5 6 7 8 9) :key #'1+)
  3)

(deftest search.10
  (search '(4 5) '(1 2 3 4 5 6 7 8 9)
          :test (lambda (x y)
                  (eql (1+ x) (1+ y))))
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

