;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function SEARCH
;;
(deftest search-list-list.1
  (search nil nil)
  0)

(deftest search-list-list.2
  (search '(a b c) nil)
  nil)

(deftest search-list-list.3
  (search nil '(a b c))
  0)

(deftest search-list-list.4
  (search '(a) '(a b c d))
  0)

(deftest search-list-list.5
  (search '(b) '(a b c d))
  1)

(deftest search-list-list.6
  (search '(z) '(a b c d))
  nil)

(deftest search-list-list.7
  (search '(a b c d e) '(a b c d))
  nil)

(deftest search-list-list.8
  (search '(b c) '(a b c d))
  1)

(deftest search-list-list.9
  (search '(b d) '(a b c d))
  nil)

(deftest search-list-list.10
  (search '(c d) '(a b c d e f g h))
  2)

(deftest search-list-list.11
  (search '(4 5) '(1 2 3 4 5 6 7 8 9) :key #'1+)
  3)

(deftest search-list-list.12
  (search '((4) 5) '(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test #'equal)
  3)

(deftest search-list-list.13
  (search '((4) 5) '(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test-not (lambda (x y) (not (equal x y))))
  3)

(deftest search-list-list.14
  (search '((4) 5) '(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test #'equal :from-end t)
  6)

(deftest search-list-list.15
  (search '((4) 5) '(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test-not (complement #'equal) :from-end t)
  6)

(deftest search-vector-list.1
  (search #() nil)
  0)

(deftest search-vector-list.2
  (search #(a b c) nil)
  nil)

(deftest search-vector-list.3
  (search #() '(a b c))
  0)

(deftest search-vector-list.4
  (search #(a) '(a b c d))
  0)

(deftest search-vector-list.5
  (search #(b) '(a b c d))
  1)

(deftest search-vector-list.6
  (search #(z) '(a b c d))
  nil)

(deftest search-vector-list.7
  (search #(a b c d e) '(a b c d))
  nil)

(deftest search-vector-list.8
  (search #(b c) '(a b c d))
  1)

(deftest search-vector-list.9
  (search #(b d) '(a b c d))
  nil)

(deftest search-vector-list.10
  (search #(c d) '(a b c d e f g h))
  2)

(deftest search-vector-list.11
  (search #(4 5) '(1 2 3 4 5 6 7 8 9) :key #'1+)
  3)

(deftest search-vector-list.12
  (search #((4) 5) '(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test #'equal)
  3)

(deftest search-vector-list.13
  (search #((4) 5) '(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test-not (lambda (x y) (not (equal x y))))
  3)

(deftest search-vector-list.14
  (search #((4) 5) '(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test #'equal :from-end t)
  6)

(deftest search-vector-list.15
  (search #((4) 5) '(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test-not (complement #'equal) :from-end t)
  6)

(deftest search-list-vector.1
  (search nil #())
  0)

(deftest search-list-vector.2
  (search '(a b c) #())
  nil)

(deftest search-list-vector.3
  (search nil #(a b c))
  0)

(deftest search-list-vector.4
  (search '(a) #(a b c d))
  0)

(deftest search-list-vector.5
  (search '(b) #(a b c d))
  1)

(deftest search-list-vector.6
  (search '(z) #(a b c d))
  nil)

(deftest search-list-vector.7
  (search '(a b c d e) #(a b c d))
  nil)

(deftest search-list-vector.8
  (search '(b c) #(a b c d))
  1)

(deftest search-list-vector.9
  (search '(b d) #(a b c d))
  nil)

(deftest search-list-vector.10
  (search '(c d) #(a b c d e f g h))
  2)

(deftest search-list-vector.11
  (search '(4 5) #(1 2 3 4 5 6 7 8 9) :key #'1+)
  3)

(deftest search-list-vector.12
  (search '((4) 5) #(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test #'equal)
  3)

(deftest search-list-vector.13
  (search '((4) 5) #(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test-not (lambda (x y) (not (equal x y))))
  3)

(deftest search-list-vector.14
  (search '((4) 5) #(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test #'equal :from-end t)
  6)

(deftest search-list-vector.15
  (search '((4) 5) #(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test-not (complement #'equal) :from-end t)
  6)

(deftest search-vector-vector.1
  (search #() #())
  0)

(deftest search-vector-vector.2
  (search #(a b c) #())
  nil)

(deftest search-vector-vector.3
  (search #() #(a b c))
  0)

(deftest search-vector-vector.4
  (search #(a) #(a b c d))
  0)

(deftest search-vector-vector.5
  (search #(b) #(a b c d))
  1)

(deftest search-vector-vector.6
  (search #(z) #(a b c d))
  nil)

(deftest search-vector-vector.7
  (search #(a b c d e) #(a b c d))
  nil)

(deftest search-vector-vector.8
  (search #(b c) #(a b c d))
  1)

(deftest search-vector-vector.9
  (search #(b d) #(a b c d))
  nil)

(deftest search-vector-vector.10
  (search #(c d) #(a b c d e f g h))
  2)

(deftest search-vector-vector.11
  (search #(4 5) #(1 2 3 4 5 6 7 8 9) :key #'1+)
  3)

(deftest search-vector-vector.12
  (search #((4) 5) #(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test #'equal)
  3)

(deftest search-vector-vector.13
  (search #((4) 5) #(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test-not (lambda (x y) (not (equal x y))))
  3)

(deftest search-vector-vector.14
  (search #((4) 5) #(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test #'equal :from-end t)
  6)

(deftest search-vector-vector.15
  (search #((4) 5) #(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test-not (complement #'equal) :from-end t)
  6)

(deftest search-string.1
  (search "" "abcd")
  0)

(deftest search-string.2
  (search "a" "abcd")
  0)

(deftest search-string.3
  (search "b" "abcd")
  1)

(deftest search-string.4
  (search "z" "abcd")
  nil)

(deftest search-string.5
  (search "abcde" "abcd")
  nil)

(deftest search-string.6
  (search "bc" "abcd")
  1)

(deftest search-string.7
  (search "bd" "abcd")
  nil)

(deftest search-string.8
  (search "hello" "abcdhelloefghellohijk")
  4)

(deftest search-from-end-list.1
  (search nil nil :from-end t)
  0)

(deftest search-from-end-list.2
  (search '(a b c) nil :from-end t)
  nil)

(deftest search-from-end-list.3
  (search nil '(a b c) :from-end t)
  0)

(deftest search-from-end-list.4
  (search '(a) '(a b c d) :from-end t)
  0)

(deftest search-from-end-list.5
  (search '(b) '(a b c d) :from-end t)
  1)

(deftest search-from-end-list.6
  (search '(z) '(a b c d) :from-end t)
  nil)

(deftest search-from-end-list.7
  (search '(a b c d e) '(a b c d) :from-end t)
  nil)

(deftest search-from-end-list.8
  (search '(b c) '(a b c d) :from-end t)
  1)

(deftest search-from-end-list.9
  (search '(b d) '(a b c d) :from-end t)
  nil)

(deftest search-from-end-list.10
  (search '(c d) '(a b c d e f g h) :from-end t)
  2)

(deftest search-from-end-list.11
  (search '(4 5) '(1 2 3 4 5 6 7 8 9) :key #'1+ :from-end t)
  3)

(deftest search-from-end-list.12
  (search '((4) 5) '(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test #'equal :from-end t)
  6)

(deftest search-from-end-list.13
  (search '((4) 5) '(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test-not (lambda (x y) (not (equal x y))) :from-end t)
  6)

(deftest search-from-end-vector.1
  (search #() #() :from-end t)
  0)

(deftest search-from-end-vector.2
  (search #(a b c) #() :from-end t)
  nil)

(deftest search-from-end-vector.3
  (search #() #(a b c) :from-end t)
  0)

(deftest search-from-end-vector.4
  (search #(a) #(a b c d) :from-end t)
  0)

(deftest search-from-end-vector.5
  (search #(b) #(a b c d) :from-end t)
  1)

(deftest search-from-end-vector.6
  (search #(z) #(a b c d) :from-end t)
  nil)

(deftest search-from-end-vector.7
  (search #(a b c d e) #(a b c d) :from-end t)
  nil)

(deftest search-from-end-vector.8
  (search #(b c) #(a b c d) :from-end t)
  1)

(deftest search-from-end-vector.9
  (search #(b d) #(a b c d) :from-end t)
  nil)

(deftest search-from-end-vector.10
  (search #(c d) #(a b c d e f g h) :from-end t)
  2)

(deftest search-from-end-vector.11
  (search #(4 5) #(1 2 3 4 5 6 7 8 9) :key #'1+ :from-end t)
  3)

(deftest search-from-end-vector.12
  (search #((4) 5) #(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test #'equal :from-end t)
  6)

(deftest search-from-end-vector.13
  (search #((4) 5) #(1 2 3 (4) 5 6 (4) 5 7 8 9)
          :test-not (lambda (x y) (not (equal x y))) :from-end t)
  6)

(deftest search-start2-list.1
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0))
  3)

(deftest search-start2-list.2
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 0)
  3)

(deftest search-start2-list.3
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 3)
  3)

(deftest search-start2-list.4
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 4)
  nil)

(deftest search-start2-list.5
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 11)
  nil)

(deftest search-start2-list.6
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :end2 nil)
  3)

(deftest search-start2-list.7
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :end2 6)
  3)

(deftest search-start2-list.8
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :end2 5)
  3)

(deftest search-start2-list.9
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :end2 4)
  nil)

(deftest search-start2-list.10
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 0 :end2 nil)
  3)

(deftest search-start2-list.11
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 2 :end2 6)
  3)

(deftest search-start2-list.12
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :from-end t)
  3)

(deftest search-start2-list.13
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 0 :from-end t)
  3)

(deftest search-start2-list.14
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 3 :from-end t)
  3)

(deftest search-start2-list.15
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 4 :from-end t)
  nil)

(deftest search-start2-list.16
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 11 :from-end t)
  nil)

(deftest search-start2-list.17
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :end2 nil :from-end t)
  3)

(deftest search-start2-list.18
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :end2 6 :from-end t)
  3)

(deftest search-start2-list.19
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :end2 5 :from-end t)
  3)

(deftest search-start2-list.20
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :end2 4 :from-end t)
  nil)

(deftest search-start2-list.21
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 0 :end2 nil :from-end t)
  3)

(deftest search-start2-list.22
  (search '(3 4) '(0 1 2 3 4 5 6 7 8 9 0) :start2 2 :end2 6 :from-end t)
  3)

(deftest search-start2-list.23
  (search '(3 4) '(0 1 2 3 4 3 4 7 8 9 0) :start2 2 :end2 9 :from-end t)
  5)

(deftest search-start2-vector.1
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0))
  3)

(deftest search-start2-vector.2
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 0)
  3)

(deftest search-start2-vector.3
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 3)
  3)

(deftest search-start2-vector.4
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 4)
  nil)

(deftest search-start2-vector.5
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 11)
  nil)

(deftest search-start2-vector.6
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :end2 nil)
  3)

(deftest search-start2-vector.7
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :end2 6)
  3)

(deftest search-start2-vector.8
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :end2 5)
  3)

(deftest search-start2-vector.9
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :end2 4)
  nil)

(deftest search-start2-vector.10
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 0 :end2 nil)
  3)

(deftest search-start2-vector.11
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 2 :end2 6)
  3)

(deftest search-start2-vector.12
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :from-end t)
  3)

(deftest search-start2-vector.13
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 0 :from-end t)
  3)

(deftest search-start2-vector.14
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 3 :from-end t)
  3)

(deftest search-start2-vector.15
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 4 :from-end t)
  nil)

(deftest search-start2-vector.16
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 11 :from-end t)
  nil)

(deftest search-start2-vector.17
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :end2 nil :from-end t)
  3)

(deftest search-start2-vector.18
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :end2 6 :from-end t)
  3)

(deftest search-start2-vector.19
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :end2 5 :from-end t)
  3)

(deftest search-start2-vector.20
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :end2 4 :from-end t)
  nil)

(deftest search-start2-vector.21
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 0 :end2 nil :from-end t)
  3)

(deftest search-start2-vector.22
  (search #(3 4) #(0 1 2 3 4 5 6 7 8 9 0) :start2 2 :end2 6 :from-end t)
  3)

(deftest search-start2-vector.23
  (search #(3 4) #(0 1 2 3 4 3 4 7 8 9 0) :start2 2 :end2 9 :from-end t)
  5)

(deftest search-start1-list.1
  (search '(2 3 4 5 6) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 0)
  1)

(deftest search-start1-list.2
  (search '(2 3 4 5 6) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 1)
  2)

(deftest search-start1-list.3
  (search '(2 3 4 5 6) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 5)
  0)

(deftest search-start1-list.4
  (search '(2 3 4 5 6) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 nil)
  1)

(deftest search-start1-list.5
  (search '(2 3 4 5 6) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 1)
  1)

(deftest search-start1-list.6
  (search '(2 3 4 5 6) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 0)
  0)

(deftest search-start1-list.7
  (search '(2 3 9 9 9) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 2)
  1)

(deftest search-start1-list.8
  (search '(9 9 3 4 9) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 2 :end1 4)
  2)

(deftest search-start1-list.9
  (search '(2 3 4 5 6) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 0 :from-end t)
  9)

(deftest search-start1-list.10
  (search '(2 3 4 5 6) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 1 :from-end t)
  10)

(deftest search-start1-list.11
  (search '(2 3 4 5 6) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 5 :from-end t)
  0)

(deftest search-start1-list.12
  (search '(2 3 4 5 6) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 nil :from-end t)
  9)

(deftest search-start1-list.13
  (search '(2 3 4 5 6) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 1 :from-end t)
  9)

(deftest search-start1-list.14
  (search '(2 3 4 5 6) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 0 :from-end t)
  0)

(deftest search-start1-list.15
  (search '(2 3 9 9 9) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 2 :from-end t)
  9)

(deftest search-start1-list.16
  (search '(9 9 3 4 9) '(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 2 :end1 4 :from-end t)
  10)

(deftest search-start1-vector.1
  (search #(2 3 4 5 6) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 0)
  1)

(deftest search-start1-vector.2
  (search #(2 3 4 5 6) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 1)
  2)

(deftest search-start1-vector.3
  (search #(2 3 4 5 6) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 5)
  0)

(deftest search-start1-vector.4
  (search #(2 3 4 5 6) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 nil)
  1)

(deftest search-start1-vector.5
  (search #(2 3 4 5 6) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 1)
  1)

(deftest search-start1-vector.6
  (search #(2 3 4 5 6) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 0)
  0)

(deftest search-start1-vector.7
  (search #(2 3 9 9 9) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 2)
  1)

(deftest search-start1-vector.8
  (search #(9 9 3 4 9) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 2 :end1 4)
  2)

(deftest search-start1-vector.9
  (search #(2 3 4 5 6) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 0 :from-end t)
  9)

(deftest search-start1-vector.10
  (search #(2 3 4 5 6) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 1 :from-end t)
  10)

(deftest search-start1-vector.11
  (search #(2 3 4 5 6) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 5 :from-end t)
  0)

(deftest search-start1-vector.12
  (search #(2 3 4 5 6) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 nil :from-end t)
  9)

(deftest search-start1-vector.13
  (search #(2 3 4 5 6) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 1 :from-end t)
  9)

(deftest search-start1-vector.14
  (search #(2 3 4 5 6) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 0 :from-end t)
  0)

(deftest search-start1-vector.15
  (search #(2 3 9 9 9) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :end1 2 :from-end t)
  9)

(deftest search-start1-vector.16
  (search #(9 9 3 4 9) #(1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
          :start1 2 :end1 4 :from-end t)
  10)

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

(deftest-error search-error.1
  (eval '(search 10 nil)))

(deftest-error search-error.2
  (eval '(search nil 20)))

(deftest-error! search-error.3
  (eval '(search nil)))

(deftest-error search-error.4
  (eval '(search nil nil :key)))

(deftest-error search-error.5
  (eval '(search nil nil :key 10)))

(deftest-error search-error.6
  (eval '(search nil nil :hello 10)))

(deftest-error search-error.7
  (eval '(search nil nil :test (constantly t) :test-not (constantly t))))

(deftest search-error.8
  (search '(1 2 3) nil :start1 3)
  0)

(deftest-error search-error.9
  (search '(1 2 3) nil :start1 4))

(deftest search-error.10
  (search #(1 2 3) nil :start1 3)
  0)

(deftest-error search-error.11
  (search #(1 2 3) nil :start1 4))

(deftest search-error.12
  (search '(1 2 3) nil :end1 3)
  nil)

(deftest-error search-error.13
  (search '(1 2 3) nil :end1 4))

(deftest search-error.14
  (search #(1 2 3) nil :end1 3)
  nil)

(deftest-error search-error.15
  (search #(1 2 3) nil :end1 4))

(deftest search-error.16
  (search '(z) '(1 2 3) :start2 3)
  nil)

(deftest-error search-error.17
  (search '(z) '(1 2 3) :start2 4))

(deftest search-error.18
  (search '(z) '(1 2 3) :end2 3)
  nil)

(deftest-error search-error.19
  (search '(z) '(1 2 3) :end2 4))

(deftest search-error.20
  (search #(z) #(1 2 3) :start2 3)
  nil)

(deftest-error search-error.21
  (search #(z) #(1 2 3) :start2 4))

(deftest search-error.22
  (search #(z) #(1 2 3) :end2 3)
  nil)

(deftest-error search-error.23
  (search #(z) #(1 2 3) :end2 4))

;;  ANSI Common Lisp
(deftest search-test.1
  (search "dog" "it's a dog's life")
  7)

(deftest search-test.2
  (search '(0 1) '(2 4 6 1 3 5) :key #'oddp)
  2)

