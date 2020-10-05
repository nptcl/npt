;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function REPLACE
;;
(deftest replace.1
  (replace nil nil)
  nil)

(deftest replace.2
  (replace #() #())
  #())

(deftest replace.3
  (replace nil '(a b c))
  nil)

(deftest replace.4
  (replace '(a b c) nil)
  (a b c))

(deftest replace.5
  (replace #() #(a b c))
  #())

(deftest replace.6
  (replace #(a b c) #())
  #(a b c))

(deftest replace.7
  (replace '(a b c) '(1 2 3))
  (1 2 3))

(deftest replace.8
  (let ((x '(a b c)))
    (replace x '(1 2 3))
    x)
  (1 2 3))

(deftest replace.9
  (let ((x '(a b c)))
    (eq x (replace x '(1 2 3))))
  t)

(deftest replace.10
  (replace #(a b c) '(1 2 3))
  #(1 2 3))

(deftest replace.11
  (replace '(a b c) #(1 2 3))
  (1 2 3))

(deftest replace.12
  (replace #(a b c) #(1 2 3))
  #(1 2 3))

(deftest replace.13
  (replace '(a b c) '(1 2))
  (1 2 c))

(deftest replace.14
  (replace '(a b c) '(1 2 3 4))
  (1 2 3))

(deftest replace.15
  (replace #(a b c) #(1 2))
  #(1 2 c))

(deftest replace.16
  (replace #(a b c) #(1 2 3 4))
  #(1 2 3))

(deftest replace-start1-list.1
  (replace '(a b c d e f) '(1 2 3) :start1 0)
  (1 2 3 d e f))

(deftest replace-start1-list.2
  (replace '(a b c d e f) '(1 2 3) :start1 2)
  (a b 1 2 3 f))

(deftest replace-start1-list.3
  (replace '(a b c d e f) '(1 2 3) :start1 5)
  (a b c d e 1))

(deftest replace-start1-list.4
  (replace '(a b c d e f) '(1 2 3) :start1 6)
  (a b c d e f))

(deftest replace-end1-list.1
  (replace '(a b c d e f) '(1 2 3) :end1 0)
  (a b c d e f))

(deftest replace-end1-list.2
  (replace '(a b c d e f) '(1 2 3) :end1 2)
  (1 2 c d e f))

(deftest replace-end1-list.3
  (replace '(a b c d e f) '(1 2 3) :end1 6)
  (1 2 3 d e f))

(deftest replace-end1-list.4
  (replace '(a b c d e f) '(1 2 3) :end1 nil)
  (1 2 3 d e f))

(deftest replace-start2-list.1
  (replace '(a b c d e f) '(1 2 3) :start2 0)
  (1 2 3 d e f))

(deftest replace-start2-list.2
  (replace '(a b c d e f) '(1 2 3) :start2 1)
  (2 3 c d e f))

(deftest replace-start2-list.3
  (replace '(a b c d e f) '(1 2 3) :start2 3)
  (a b c d e f))

(deftest replace-end2-list.1
  (replace '(a b c d e f) '(1 2 3) :end2 0)
  (a b c d e f))

(deftest replace-end2-list.2
  (replace '(a b c d e f) '(1 2 3) :end2 1)
  (1 b c d e f))

(deftest replace-end2-list.3
  (replace '(a b c d e f) '(1 2 3) :end2 3)
  (1 2 3 d e f))

(deftest replace-end2-list.4
  (replace '(a b c d e f) '(1 2 3) :end2 nil)
  (1 2 3 d e f))

(deftest replace-start-end-list.1
  (replace '(a b c d e f) '(1 2 3 4 5 6 7 8 9)
           :start1 1 :end1 3
           :start2 4 :end2 8)
  (a 5 6 d e f))

(deftest replace-start-end-list.2
  (let ((x '(a b c d e f g h i j k l)))
    (replace x x :start1 2 :end1 8 :start2 4 :end2 10))
  (a b e f g h i j i j k l))

(deftest replace-start-end-list.3
  (let ((x '(a b c d e f g h i j k l)))
    (replace x x :start1 4 :end1 10 :start2 2 :end2 8))
  (a b c d c d e f g h k l))

(deftest replace-start1-vector.1
  (replace #(a b c d e f) #(1 2 3) :start1 0)
  #(1 2 3 d e f))

(deftest replace-start1-vector.2
  (replace #(a b c d e f) #(1 2 3) :start1 2)
  #(a b 1 2 3 f))

(deftest replace-start1-vector.3
  (replace #(a b c d e f) #(1 2 3) :start1 5)
  #(a b c d e 1))

(deftest replace-start1-vector.4
  (replace #(a b c d e f) #(1 2 3) :start1 6)
  #(a b c d e f))

(deftest replace-end1-vector.1
  (replace #(a b c d e f) #(1 2 3) :end1 0)
  #(a b c d e f))

(deftest replace-end1-vector.2
  (replace #(a b c d e f) #(1 2 3) :end1 2)
  #(1 2 c d e f))

(deftest replace-end1-vector.3
  (replace #(a b c d e f) #(1 2 3) :end1 6)
  #(1 2 3 d e f))

(deftest replace-start2-vector.1
  (replace #(a b c d e f) #(1 2 3) :start2 0)
  #(1 2 3 d e f))

(deftest replace-start2-vector.2
  (replace #(a b c d e f) #(1 2 3) :start2 1)
  #(2 3 c d e f))

(deftest replace-start2-vector.3
  (replace #(a b c d e f) #(1 2 3) :start2 3)
  #(a b c d e f))

(deftest replace-end2-vector.1
  (replace #(a b c d e f) #(1 2 3) :end2 0)
  #(a b c d e f))

(deftest replace-end2-vector.2
  (replace #(a b c d e f) #(1 2 3) :end2 1)
  #(1 b c d e f))

(deftest replace-end2-vector.3
  (replace #(a b c d e f) #(1 2 3) :end2 3)
  #(1 2 3 d e f))

(deftest replace-start-end-vector.1
  (replace #(a b c d e f) #(1 2 3 4 5 6 7 8 9)
           :start1 1 :end1 3
           :start2 4 :end2 8)
  #(a 5 6 d e f))

(deftest replace-start-end-vector.2
  (let ((x #(a b c d e f g h i j k l)))
    (replace x x :start1 2 :end1 8 :start2 4 :end2 10))
  #(a b e f g h i j i j k l))

(deftest replace-start-end-vector.3
  (let ((x #(a b c d e f g h i j k l)))
    (replace x x :start1 4 :end1 10 :start2 2 :end2 8))
  #(a b c d c d e f g h k l))

(deftest-error replace-error.1
  (eval '(replace 10 nil)))

(deftest-error replace-error.2
  (eval '(replace nil 20)))

(deftest-error! replace-error.3
  (eval '(replace nil)))

(deftest-error replace-error.4
  (eval '(replace nil nil :start1)))

(deftest-error replace-error.5
  (eval '(replace nil nil :start1 :aaa)))

(deftest-error replace-error.6
  (eval '(replace nil nil :hello 10)))

(deftest-error replace-error.7
  (replace '(a b c) nil :start1 4))

(deftest-error replace-error.8
  (replace #(a b c) nil :start1 4))

(deftest-error replace-error.9
  (replace '(a b c) nil :end1 4))

(deftest-error replace-error.10
  (replace #(a b c) nil :end1 4))

(deftest-error replace-error.11
  (replace nil '(a b c) :start2 4))

(deftest-error replace-error.12
  (replace nil #(a b c) :start2 4))

(deftest-error replace-error.13
  (replace nil '(a b c) :end2 4))

(deftest-error replace-error.14
  (replace nil #(a b c) :end2 4))

(deftest-error replace-error.15
  (replace '(a b c) '(a b c) :start1 2 :end1 1))

(deftest-error replace-error.16
  (replace '(a b c) '(a b c) :start2 2 :end2 1))

;;  ANSI Common Lisp
(deftest replace-test.1
  (replace "abcdefghij" "0123456789" :start1 4 :end1 7 :start2 4)
  "abcd456hij")

(setq *replace-object* "012345678")

(deftest replace-test.2
  (replace *replace-object* *replace-object* :start1 2 :start2 0)
  "010123456")

(deftest replace-test.3
  *replace-object*
  "010123456")

(deftest replace-test.4
  (replace '(a b c d e f g h i j) "0123456789" :start1 4 :end1 7 :start2 4)
  (a b c d #\4 #\5 #\6 h i j))

(deftest replace-test.5
  (replace "abcdefghij" '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           :start1 4 :end1 7 :start2 4)
  "abcd456hij")

(deftest replace-test.6
  (replace '(a b c d e f g h i j) '(0 1 2 3 4 5 6 7 8 9) :start1 4 :end1 7 :start2 4)
  (a b c d 4 5 6 h i j))

