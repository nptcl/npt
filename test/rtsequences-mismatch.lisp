;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function MISMATCH
;;
(deftest mismatch-list.1
  (mismatch nil nil)
  nil)

(deftest mismatch-list.2
  (mismatch nil nil :from-end t)
  nil)

(deftest mismatch-list.3
  (mismatch '(a b c) nil)
  0)

(deftest mismatch-list.4
  (mismatch '(a b c) nil :from-end t)
  3)

(deftest mismatch-list.5
  (mismatch nil '(a b c))
  0)

(deftest mismatch-list.6
  (mismatch nil '(a b c) :from-end t)
  0)

(deftest mismatch-list.7
  (mismatch '(a b c) '(a b c))
  nil)

(deftest mismatch-list.8
  (mismatch '(a b c) '(a b c) :from-end t)
  nil)

(deftest mismatch-list.9
  (mismatch '(a b c) '(d e f))
  0)

(deftest mismatch-list.10
  (mismatch '(a b c) '(d e f) :from-end t)
  3)

(deftest mismatch-list.11
  (mismatch '(a b c d f) '(a b c))
  3)

(deftest mismatch-list.12
  (mismatch '(a b c d f) '(a b c) :from-end t)
  5)

(deftest mismatch-list.13
  (mismatch '(a b c) '(a b c d e))
  3)

(deftest mismatch-list.14
  (mismatch '(a b c) '(a b c d e) :from-end t)
  3)

(deftest mismatch-list.15
  (mismatch '(a b c e f) '(a b c d e))
  3)

(deftest mismatch-list.16
  (mismatch '(a b c e f) '(a b c d e) :from-end t)
  5)

(deftest mismatch-list.17
  (mismatch '(a b c d e) '(c d e))
  0)

(deftest mismatch-list.18
  (mismatch '(a b c d e) '(c d e) :from-end t)
  2)

(deftest mismatch-list.19
  (mismatch '(c d e) '(a b c d e))
  0)

(deftest mismatch-list.20
  (mismatch '(c d e) '(a b c d e) :from-end t)
  0)

(deftest mismatch-list.21
  (mismatch '(x d e) '(a b c d e) :from-end t)
  1)

(deftest mismatch-list.22
  (mismatch '(a b c d e) '(x d e) :from-end t)
  3)

(deftest mismatch-vector.1
  (mismatch #() #())
  nil)

(deftest mismatch-vector.2
  (mismatch #() #() :from-end t)
  nil)

(deftest mismatch-vector.3
  (mismatch #(a b c) #())
  0)

(deftest mismatch-vector.4
  (mismatch #(a b c) #() :from-end t)
  3)

(deftest mismatch-vector.5
  (mismatch #() #(a b c))
  0)

(deftest mismatch-vector.6
  (mismatch #() #(a b c) :from-end t)
  0)

(deftest mismatch-vector.7
  (mismatch #(a b c) #(a b c))
  nil)

(deftest mismatch-vector.8
  (mismatch #(a b c) #(a b c) :from-end t)
  nil)

(deftest mismatch-vector.9
  (mismatch #(a b c) #(d e f))
  0)

(deftest mismatch-vector.10
  (mismatch #(a b c) #(d e f) :from-end t)
  3)

(deftest mismatch-vector.11
  (mismatch #(a b c d f) #(a b c))
  3)

(deftest mismatch-vector.12
  (mismatch #(a b c d f) #(a b c) :from-end t)
  5)

(deftest mismatch-vector.13
  (mismatch #(a b c) #(a b c d e))
  3)

(deftest mismatch-vector.14
  (mismatch #(a b c) #(a b c d e) :from-end t)
  3)

(deftest mismatch-vector.15
  (mismatch #(a b c e f) #(a b c d e))
  3)

(deftest mismatch-vector.16
  (mismatch #(a b c e f) #(a b c d e) :from-end t)
  5)

(deftest mismatch-vector.17
  (mismatch #(a b c d e) #(c d e))
  0)

(deftest mismatch-vector.18
  (mismatch #(a b c d e) #(c d e) :from-end t)
  2)

(deftest mismatch-vector.19
  (mismatch #(c d e) #(a b c d e))
  0)

(deftest mismatch-vector.20
  (mismatch #(c d e) #(a b c d e) :from-end t)
  0)

(deftest mismatch-vector.21
  (mismatch #(x d e) #(a b c d e) :from-end t)
  1)

(deftest mismatch-vector.22
  (mismatch #(a b c d e) #(x d e) :from-end t)
  3)

(deftest mismatch-key-list.1
  (mismatch '(3 4 5) '(13 24 35 46 57)
            :key (lambda (x) (mod x 10))
            :test #'eql)
  3)

(deftest mismatch-key-list.2
  (mismatch '((3) 4 5) '((3) 4 5 6 7) :test #'eql)
  0)

(deftest mismatch-key-list.3
  (mismatch '((3) 4 5) '((3) 4 5 6 7) :test #'equal)
  3)

(deftest mismatch-key-list.4
  (mismatch '((3) 4 5) '((3) 4 5 6 7) :test-not #'equal)
  0)

(deftest mismatch-key-list.5
  (mismatch '((3) 4 5) '((3) 4 5 6 7) :test-not (complement #'equal))
  3)

(deftest mismatch-key-vector.1
  (mismatch #(3 4 5) #(13 24 35 46 57)
            :key (lambda (x) (mod x 10))
            :test #'eql)
  3)

(deftest mismatch-key-vector.2
  (mismatch #((3) 4 5) #((3) 4 5 6 7) :test #'eql)
  0)

(deftest mismatch-key-vector.3
  (mismatch #((3) 4 5) #((3) 4 5 6 7) :test #'equal)
  3)

(deftest mismatch-key-vector.4
  (mismatch #((3) 4 5) #((3) 4 5 6 7) :test-not #'equal)
  0)

(deftest mismatch-key-vector.5
  (mismatch #((3) 4 5) #((3) 4 5 6 7) :test-not (complement #'equal))
  3)

(deftest mismatch-start1-list.1
  (mismatch '(a b c d e) '(a b c d e f g) :start1 0)
  5)

(deftest mismatch-start1-list.2
  (mismatch '(a b c d e) '(a b c d e f g) :start1 0 :from-end t)
  5)

(deftest mismatch-start1-list.3
  (mismatch '(a b c d e) '(a b c d e f g) :start1 1)
  1)

(deftest mismatch-start1-list.4
  (mismatch '(a b c d e) '(a b c d e f g) :start1 1 :from-end t)
  5)

(deftest mismatch-start1-list.5
  (mismatch '(a b c d e) '(a b c d e f g) :start1 2)
  2)

(deftest mismatch-start1-list.6
  (mismatch '(a b c d e) '(a b c d e f g) :start1 2 :from-end t)
  5)

(deftest mismatch-start1-list.7
  (mismatch '(a b a d e) '(a b c d e f g) :start1 2)
  3)

(deftest mismatch-start1-list.8
  (mismatch '(a b c d e) '(a b z z c d e) :start1 2 :from-end t)
  2)

(deftest mismatch-start1-list.9
  (mismatch '(a b c d e) '(c d e) :start1 2)
  nil)

(deftest mismatch-start1-list.10
  (mismatch '(a b c d e) '(c d e) :start1 2 :from-end t)
  nil)

(deftest mismatch-end1-list.1
  (mismatch '(a b c) '(a b c d e) :end1 nil)
  3)

(deftest mismatch-end1-list.2
  (mismatch '(a b c) '(a b c d e) :end1 nil :from-end t)
  3)

(deftest mismatch-end1-list.3
  (mismatch '(a b c) '(a b c) :end1 3)
  nil)

(deftest mismatch-end1-list.4
  (mismatch '(a b c) '(a b c) :end1 3 :from-end t)
  nil)

(deftest mismatch-end1-list.5
  (mismatch '(a b c) '(a b c d b) :end1 2)
  2)

(deftest mismatch-end1-list.6
  (mismatch '(a b c) '(a b c d b) :end1 2 :from-end t)
  1)

(deftest mismatch-end1-list.7
  (mismatch '(a b c) '(a b c a b) :end1 2 :from-end t)
  0)

(deftest mismatch-start2-list.1
  (mismatch '(a b c) '(a b c a b) :start2 0)
  3)

(deftest mismatch-start2-list.2
  (mismatch '(a b c) '(a b c a b) :start2 0 :from-end t)
  3)

(deftest mismatch-start2-list.3
  (mismatch '(a b c) '(a b c a b) :start2 2)
  0)

(deftest mismatch-start2-list.4
  (mismatch '(a b c) '(a b c a b) :start2 2 :from-end t)
  3)

(deftest mismatch-start2-list.5
  (mismatch '(a b) '(a b c a b) :start2 2 :from-end t)
  0)

(deftest mismatch-start2-list.6
  (mismatch '(a b c) '(a b c a b) :start2 5)
  0)

(deftest mismatch-start2-list.7
  (mismatch '(a b c) '(a b c a b) :start2 5 :from-end t)
  3)

(deftest mismatch-end2-list.1
  (mismatch '(a b c) '(a b c a b) :end2 nil)
  3)

(deftest mismatch-end2-list.2
  (mismatch '(a b c) '(a b c a b) :end2 nil :from-end t)
  3)

(deftest mismatch-end2-list.3
  (mismatch '(a b c) '(a b c a b) :end2 5)
  3)

(deftest mismatch-end2-list.4
  (mismatch '(a b c) '(a b c a b) :end2 5 :from-end t)
  3)

(deftest mismatch-end2-list.5
  (mismatch '(a b c) '(a b c a b) :end2 1)
  1)

(deftest mismatch-end2-list.6
  (mismatch '(a b c) '(a b c a b) :end2 1 :from-end t)
  3)

(deftest mismatch-start1-vector.1
  (mismatch #(a b c d e) #(a b c d e f g) :start1 0)
  5)

(deftest mismatch-start1-vector.2
  (mismatch #(a b c d e) #(a b c d e f g) :start1 0 :from-end t)
  5)

(deftest mismatch-start1-vector.3
  (mismatch #(a b c d e) #(a b c d e f g) :start1 1)
  1)

(deftest mismatch-start1-vector.4
  (mismatch #(a b c d e) #(a b c d e f g) :start1 1 :from-end t)
  5)

(deftest mismatch-start1-vector.5
  (mismatch #(a b c d e) #(a b c d e f g) :start1 2)
  2)

(deftest mismatch-start1-vector.6
  (mismatch #(a b c d e) #(a b c d e f g) :start1 2 :from-end t)
  5)

(deftest mismatch-start1-vector.7
  (mismatch #(a b a d e) #(a b c d e f g) :start1 2)
  3)

(deftest mismatch-start1-vector.8
  (mismatch #(a b c d e) #(a b z z c d e) :start1 2 :from-end t)
  2)

(deftest mismatch-start1-vector.9
  (mismatch #(a b c d e) #(c d e) :start1 2)
  nil)

(deftest mismatch-start1-vector.10
  (mismatch #(a b c d e) #(c d e) :start1 2 :from-end t)
  nil)

(deftest mismatch-end1-vector.1
  (mismatch #(a b c) #(a b c d e) :end1 nil)
  3)

(deftest mismatch-end1-vector.2
  (mismatch #(a b c) #(a b c d e) :end1 nil :from-end t)
  3)

(deftest mismatch-end1-vector.3
  (mismatch #(a b c) #(a b c) :end1 3)
  nil)

(deftest mismatch-end1-vector.4
  (mismatch #(a b c) #(a b c) :end1 3 :from-end t)
  nil)

(deftest mismatch-end1-vector.5
  (mismatch #(a b c) #(a b c d b) :end1 2)
  2)

(deftest mismatch-end1-vector.6
  (mismatch #(a b c) #(a b c d b) :end1 2 :from-end t)
  1)

(deftest mismatch-end1-vector.7
  (mismatch #(a b c) #(a b c a b) :end1 2 :from-end t)
  0)

(deftest mismatch-start2-vector.1
  (mismatch #(a b c) #(a b c a b) :start2 0)
  3)

(deftest mismatch-start2-vector.2
  (mismatch #(a b c) #(a b c a b) :start2 0 :from-end t)
  3)

(deftest mismatch-start2-vector.3
  (mismatch #(a b c) #(a b c a b) :start2 2)
  0)

(deftest mismatch-start2-vector.4
  (mismatch #(a b c) #(a b c a b) :start2 2 :from-end t)
  3)

(deftest mismatch-start2-vector.5
  (mismatch #(a b) #(a b c a b) :start2 2 :from-end t)
  0)

(deftest mismatch-start2-vector.6
  (mismatch #(a b c) #(a b c a b) :start2 5)
  0)

(deftest mismatch-start2-vector.7
  (mismatch #(a b c) #(a b c a b) :start2 5 :from-end t)
  3)

(deftest mismatch-end2-vector.1
  (mismatch #(a b c) #(a b c a b) :end2 nil)
  3)

(deftest mismatch-end2-vector.2
  (mismatch #(a b c) #(a b c a b) :end2 nil :from-end t)
  3)

(deftest mismatch-end2-vector.3
  (mismatch #(a b c) #(a b c a b) :end2 5)
  3)

(deftest mismatch-end2-vector.4
  (mismatch #(a b c) #(a b c a b) :end2 5 :from-end t)
  3)

(deftest mismatch-end2-vector.5
  (mismatch #(a b c) #(a b c a b) :end2 1)
  1)

(deftest mismatch-end2-vector.6
  (mismatch #(a b c) #(a b c a b) :end2 1 :from-end t)
  3)

(deftest mismatch-start-end-list.1
  (mismatch '(a b   c d e   f g a b c)
            '(z x c v z z x x q w    c d e   z a b c)
            :start1 2 :end1 5
            :start2 10 :end2 13)
  nil)

(deftest mismatch-start-end-list.2
  (mismatch '(a b   c d e f   g a b c)
            '(z x c v z z x x q w   c d e z   a b c)
            :start1 2 :end1 6
            :start2 10 :end2 14)
  5)

(deftest mismatch-start-end-list.3
  (mismatch '(a b   c d e   f g a b c)
            '(z x c v z z x x q w    c d e   z a b c)
            :start1 2 :end1 5
            :start2 10 :end2 13
            :from-end t)
  nil)

(deftest mismatch-start-end-list.4
  (mismatch '(a b   c d e f   g a b c)
            '(z x c v z z x x q w   c d e z   a b c)
            :start1 2 :end1 6
            :start2 10 :end2 14
            :from-end t)
  6)

(deftest mismatch-start-end-list.5
  (mismatch '(a b   c d e f   g a b c)
            '(z x c v z z x x q w   c z e f   a b c)
            :start1 2 :end1 6
            :start2 10 :end2 14
            :from-end t)
  4)

(deftest mismatch-start-end-vector.1
  (mismatch #(a b   c d e   f g a b c)
            #(z x c v z z x x q w    c d e   z a b c)
            :start1 2 :end1 5
            :start2 10 :end2 13)
  nil)

(deftest mismatch-start-end-vector.2
  (mismatch #(a b   c d e f   g a b c)
            #(z x c v z z x x q w   c d e z   a b c)
            :start1 2 :end1 6
            :start2 10 :end2 14)
  5)

(deftest mismatch-start-end-vector.3
  (mismatch #(a b   c d e   f g a b c)
            #(z x c v z z x x q w    c d e   z a b c)
            :start1 2 :end1 5
            :start2 10 :end2 13
            :from-end t)
  nil)

(deftest mismatch-start-end-vector.4
  (mismatch #(a b   c d e f   g a b c)
            #(z x c v z z x x q w   c d e z   a b c)
            :start1 2 :end1 6
            :start2 10 :end2 14
            :from-end t)
  6)

(deftest mismatch-start-end-vector.5
  (mismatch #(a b   c d e f   g a b c)
            #(z x c v z z x x q w   c z e f   a b c)
            :start1 2 :end1 6
            :start2 10 :end2 14
            :from-end t)
  4)

(deftest-error mismatch-error.1
  (eval '(mismatch 10 nil)))

(deftest-error mismatch-error.2
  (eval '(mismatch nil 20)))

(deftest-error! mismatch-error.3
  (eval '(mismatch nil)))

(deftest-error mismatch-error.4
  (eval '(mismatch nil nil :key)))

(deftest-error mismatch-error.5
  (eval '(mismatch nil nil :key 10)))

(deftest-error mismatch-error.6
  (eval '(mismatch nil nil :hello 10)))

(deftest-error mismatch-error.7
  (eval '(mismatch nil nil :test (constantly t) :test-not (constantly t))))

(deftest mismatch-error.8
  (mismatch '(1 2 3) nil :start1 3)
  nil)

(deftest-error mismatch-error.9
  (mismatch '(1 2 3) nil :start1 4))

(deftest mismatch-error.10
  (mismatch #(1 2 3) nil :start1 3)
  nil)

(deftest-error mismatch-error.11
  (mismatch #(1 2 3) nil :start1 4))

(deftest mismatch-error.12
  (mismatch '(1 2 3) nil :end1 3)
  0)

(deftest-error mismatch-error.13
  (mismatch '(1 2 3) nil :end1 4))

(deftest mismatch-error.14
  (mismatch #(1 2 3) nil :end1 3)
  0)

(deftest-error mismatch-error.15
  (mismatch #(1 2 3) nil :end1 4))

(deftest mismatch-error.16
  (mismatch '(z) '(1 2 3) :start2 3)
  0)

(deftest-error mismatch-error.17
  (mismatch '(z) '(1 2 3) :start2 4))

(deftest mismatch-error.18
  (mismatch '(z) '(1 2 3) :end2 3)
  0)

(deftest-error mismatch-error.19
  (mismatch '(z) '(1 2 3) :end2 4))

(deftest mismatch-error.20
  (mismatch #(z) #(1 2 3) :start2 3)
  0)

(deftest-error mismatch-error.21
  (mismatch #(z) #(1 2 3) :start2 4))

(deftest mismatch-error.22
  (mismatch #(z) #(1 2 3) :end2 3)
  0)

(deftest-error mismatch-error.23
  (mismatch #(z) #(1 2 3) :end2 4))

;;  ANSI Common Lisp
(deftest mismatch-test.1
  (mismatch "abcd" "ABCDE" :test #'char-equal)
  4)

(deftest mismatch-test.2
  (mismatch '(3 2 1 1 2 3) '(1 2 3) :from-end t)
  3)

(deftest mismatch-test.3
  (mismatch '(1 2 3) '(2 3 4) :test-not #'eq :key #'oddp)
  nil)

(deftest mismatch-test.4
  (mismatch '(1 2 3 4 5 6) '(3 4 5 6 7) :start1 2 :end2 4)
  nil)

