;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function REMOVE-DUPLICATES
;;
(deftest remove-duplicates-list.1
  (remove-duplicates nil)
  nil)

(deftest remove-duplicates-list.2
  (remove-duplicates '(a a))
  (a))

(deftest remove-duplicates-list.3
  (remove-duplicates '(a a b a))
  (b a))

(deftest remove-duplicates-list.4
  (remove-duplicates '(a a b a c b d d))
  (a c b d))

(deftest remove-duplicates-list.5
  (remove-duplicates '(a a b a c b d d) :from-end t)
  (a b c d))

(deftest remove-duplicates-list.6
  (remove-duplicates '(a b c d e f))
  (a b c d e f))

(deftest remove-duplicates-list.7
  (remove-duplicates '(a b c d e f) :from-end t)
  (a b c d e f))

(deftest remove-duplicates-list.8
  (remove-duplicates '((a) (a) (b) (a) (c) (b) (d) (d)) :key #'car)
  ((a) (c) (b) (d)))

(deftest remove-duplicates-list.9
  (remove-duplicates '((a) (a) (b) (a) (c) (b) (d) (d)) :test #'equal)
  ((a) (c) (b) (d)))

(deftest remove-duplicates-list.10
  (remove-duplicates '((a) (a) (b) (a) (c) (b) (d) (d)) :test-not (complement #'equal))
  ((a) (c) (b) (d)))

(deftest remove-duplicates-vector.1
  (remove-duplicates #())
  #())

(deftest remove-duplicates-vector.2
  (remove-duplicates #(a a))
  #(a))

(deftest remove-duplicates-vector.3
  (remove-duplicates #(a a b a))
  #(b a))

(deftest remove-duplicates-vector.4
  (remove-duplicates #(a a b a c b d d))
  #(a c b d))

(deftest remove-duplicates-vector.5
  (remove-duplicates #(a a b a c b d d) :from-end t)
  #(a b c d))

(deftest remove-duplicates-vector.6
  (remove-duplicates #(a b c d e f))
  #(a b c d e f))

(deftest remove-duplicates-vector.7
  (remove-duplicates #(a b c d e f) :from-end t)
  #(a b c d e f))

(deftest remove-duplicates-vector.8
  (remove-duplicates #((a) (a) (b) (a) (c) (b) (d) (d)) :key #'car)
  #((a) (c) (b) (d)))

(deftest remove-duplicates-vector.9
  (remove-duplicates #((a) (a) (b) (a) (c) (b) (d) (d)) :test #'equal)
  #((a) (c) (b) (d)))

(deftest remove-duplicates-vector.10
  (remove-duplicates #((a) (a) (b) (a) (c) (b) (d) (d)) :test-not (complement #'equal))
  #((a) (c) (b) (d)))

(deftest remove-duplicates-start-list.1
  (remove-duplicates '(a a b c d a a) :start 0)
  (b c d a))

(deftest remove-duplicates-start-list.2
  (remove-duplicates '(a a b c d a a) :start 0 :from-end t)
  (a b c d))

(deftest remove-duplicates-start-list.3
  (remove-duplicates '(a a b c d a a) :start 1)
  (a b c d a))

(deftest remove-duplicates-start-list.4
  (remove-duplicates '(a a b c d a a) :start 1 :from-end t)
  (a a b c d))

(deftest remove-duplicates-start-list.5
  (remove-duplicates '(a a b c d a a) :start 7)
  (a a b c d a a))

(deftest remove-duplicates-start-list.6
  (remove-duplicates '(a a b c d a a) :start 7 :from-end t)
  (a a b c d a a))

(deftest-error remove-duplicates-start-list.7
  (remove-duplicates '(a a b c d a a) :start 8))

(deftest remove-duplicates-start-vector.1
  (remove-duplicates #(a a b c d a a) :start 0)
  #(b c d a))

(deftest remove-duplicates-start-vector.2
  (remove-duplicates #(a a b c d a a) :start 0 :from-end t)
  #(a b c d))

(deftest remove-duplicates-start-vector.3
  (remove-duplicates #(a a b c d a a) :start 1)
  #(a b c d a))

(deftest remove-duplicates-start-vector.4
  (remove-duplicates #(a a b c d a a) :start 1 :from-end t)
  #(a a b c d))

(deftest remove-duplicates-start-vector.5
  (remove-duplicates #(a a b c d a a) :start 7)
  #(a a b c d a a))

(deftest remove-duplicates-start-vector.6
  (remove-duplicates #(a a b c d a a) :start 7 :from-end t)
  #(a a b c d a a))

(deftest-error remove-duplicates-start-vector.7
  (remove-duplicates #(a a b c d a a) :start 8))

(deftest remove-duplicates-end-list.1
  (remove-duplicates '(a a b c d a a) :end 0)
  (a a b c d a a))

(deftest remove-duplicates-end-list.2
  (remove-duplicates '(a a b c d a a) :end 0 :from-end t)
  (a a b c d a a))

(deftest remove-duplicates-end-list.3
  (remove-duplicates '(a a b c d a a) :end 2)
  (a b c d a a))

(deftest remove-duplicates-end-list.4
  (remove-duplicates '(a a b c d a a) :end 2 :from-end t)
  (a b c d a a))

(deftest remove-duplicates-end-list.5
  (remove-duplicates '(a a b c d a a) :end 7)
  (b c d a))

(deftest remove-duplicates-end-list.6
  (remove-duplicates '(a a b c d a a) :end 7 :from-end t)
  (a b c d))

(deftest remove-duplicates-end-list.7
  (remove-duplicates '(a a b c d a a) :end nil)
  (b c d a))

(deftest remove-duplicates-end-list.8
  (remove-duplicates '(a a b c d a a) :end nil :from-end t)
  (a b c d))

(deftest-error remove-duplicates-end-list.9
  (remove-duplicates '(a a b c d a a) :end 8))

(deftest remove-duplicates-end-vector.1
  (remove-duplicates #(a a b c d a a) :end 0)
  #(a a b c d a a))

(deftest remove-duplicates-end-vector.2
  (remove-duplicates #(a a b c d a a) :end 0 :from-end t)
  #(a a b c d a a))

(deftest remove-duplicates-end-vector.3
  (remove-duplicates #(a a b c d a a) :end 2)
  #(a b c d a a))

(deftest remove-duplicates-end-vector.4
  (remove-duplicates #(a a b c d a a) :end 2 :from-end t)
  #(a b c d a a))

(deftest remove-duplicates-end-vector.5
  (remove-duplicates #(a a b c d a a) :end 7)
  #(b c d a))

(deftest remove-duplicates-end-vector.6
  (remove-duplicates #(a a b c d a a) :end 7 :from-end t)
  #(a b c d))

(deftest remove-duplicates-end-vector.7
  (remove-duplicates #(a a b c d a a) :end nil)
  #(b c d a))

(deftest remove-duplicates-end-vector.8
  (remove-duplicates #(a a b c d a a) :end nil :from-end t)
  #(a b c d))

(deftest-error remove-duplicates-end-vector.9
  (remove-duplicates #(a a b c d a a) :end 8))

(deftest remove-duplicates-start-end-list.1
  (remove-duplicates '(a a b c d a a) :start 1 :end 6)
  (a b c d a a))

(deftest remove-duplicates-start-end-list.2
  (remove-duplicates '(a a b c d a a) :start 1 :end 6 :from-end t)
  (a a b c d a))

(deftest remove-duplicates-start-end-list.3
  (remove-duplicates '(a a b c d a a) :start 1 :end 5)
  (a a b c d a a))

(deftest remove-duplicates-start-end-list.4
  (remove-duplicates '(a a b c d a a) :start 1 :end 5 :from-end t)
  (a a b c d a a))

(deftest remove-duplicates-start-end-vector.1
  (remove-duplicates #(a a b c d a a) :start 1 :end 6)
  #(a b c d a a))

(deftest remove-duplicates-start-end-vector.2
  (remove-duplicates #(a a b c d a a) :start 1 :end 6 :from-end t)
  #(a a b c d a))

(deftest remove-duplicates-start-end-vector.3
  (remove-duplicates #(a a b c d a a) :start 1 :end 5)
  #(a a b c d a a))

(deftest remove-duplicates-start-end-vector.4
  (remove-duplicates #(a a b c d a a) :start 1 :end 5 :from-end t)
  #(a a b c d a a))

(deftest-error remove-duplicates-error.1
  (eval '(remove-duplicates 10)))

(deftest-error! remove-duplicates-error.2
  (eval '(remove-duplicates)))

(deftest-error remove-duplicates-error.3
  (eval '(remove-duplicates nil nil)))

(deftest-error remove-duplicates-error.4
  (eval '(remove-duplicates nil :key)))

(deftest-error remove-duplicates-error.5
  (eval '(remove-duplicates nil :key 20)))

(deftest-error remove-duplicates-error.6
  (eval '(remove-duplicates nil :hello 20)))

(deftest-error remove-duplicates-error.7
  (eval '(remove-duplicates nil :test (constantly t) :test-not (constantly t))))

(deftest-error remove-duplicates-error.8
  (eval '(remove-duplicates '(a b c) :start 4)))

(deftest-error remove-duplicates-error.9
  (eval '(remove-duplicates #(a b c) :start 4)))

(deftest-error remove-duplicates-error.10
  (eval '(remove-duplicates '(a b c) :end 4)))

(deftest-error remove-duplicates-error.11
  (eval '(remove-duplicates #(a b c) :end 4)))

(deftest-error remove-duplicates-error.12
  (eval '(remove-duplicates '(a b c) :start 3 :end 1)))

(deftest-error remove-duplicates-error.13
  (eval '(remove-duplicates #(a b c) :start 3 :end 1)))


;;
;;  Function DELETE-DUPLICATES
;;
(deftest delete-duplicates-list.1
  (delete-duplicates nil)
  nil)

(deftest delete-duplicates-list.2
  (delete-duplicates '(a a))
  (a))

(deftest delete-duplicates-list.3
  (delete-duplicates '(a a b a))
  (b a))

(deftest delete-duplicates-list.4
  (delete-duplicates '(a a b a c b d d))
  (a c b d))

(deftest delete-duplicates-list.5
  (delete-duplicates '(a a b a c b d d) :from-end t)
  (a b c d))

(deftest delete-duplicates-list.6
  (delete-duplicates '(a b c d e f))
  (a b c d e f))

(deftest delete-duplicates-list.7
  (delete-duplicates '(a b c d e f) :from-end t)
  (a b c d e f))

(deftest delete-duplicates-list.8
  (delete-duplicates '((a) (a) (b) (a) (c) (b) (d) (d)) :key #'car)
  ((a) (c) (b) (d)))

(deftest delete-duplicates-list.9
  (delete-duplicates '((a) (a) (b) (a) (c) (b) (d) (d)) :test #'equal)
  ((a) (c) (b) (d)))

(deftest delete-duplicates-list.10
  (delete-duplicates '((a) (a) (b) (a) (c) (b) (d) (d)) :test-not (complement #'equal))
  ((a) (c) (b) (d)))

(deftest delete-duplicates-vector.1
  (delete-duplicates #())
  #())

(deftest delete-duplicates-vector.2
  (delete-duplicates #(a a))
  #(a))

(deftest delete-duplicates-vector.3
  (delete-duplicates #(a a b a))
  #(b a))

(deftest delete-duplicates-vector.4
  (delete-duplicates #(a a b a c b d d))
  #(a c b d))

(deftest delete-duplicates-vector.5
  (delete-duplicates #(a a b a c b d d) :from-end t)
  #(a b c d))

(deftest delete-duplicates-vector.6
  (delete-duplicates #(a b c d e f))
  #(a b c d e f))

(deftest delete-duplicates-vector.7
  (delete-duplicates #(a b c d e f) :from-end t)
  #(a b c d e f))

(deftest delete-duplicates-vector.8
  (delete-duplicates #((a) (a) (b) (a) (c) (b) (d) (d)) :key #'car)
  #((a) (c) (b) (d)))

(deftest delete-duplicates-vector.9
  (delete-duplicates #((a) (a) (b) (a) (c) (b) (d) (d)) :test #'equal)
  #((a) (c) (b) (d)))

(deftest delete-duplicates-vector.10
  (delete-duplicates #((a) (a) (b) (a) (c) (b) (d) (d)) :test-not (complement #'equal))
  #((a) (c) (b) (d)))

(deftest delete-duplicates-start-list.1
  (delete-duplicates '(a a b c d a a) :start 0)
  (b c d a))

(deftest delete-duplicates-start-list.2
  (delete-duplicates '(a a b c d a a) :start 0 :from-end t)
  (a b c d))

(deftest delete-duplicates-start-list.3
  (delete-duplicates '(a a b c d a a) :start 1)
  (a b c d a))

(deftest delete-duplicates-start-list.4
  (delete-duplicates '(a a b c d a a) :start 1 :from-end t)
  (a a b c d))

(deftest delete-duplicates-start-list.5
  (delete-duplicates '(a a b c d a a) :start 7)
  (a a b c d a a))

(deftest delete-duplicates-start-list.6
  (delete-duplicates '(a a b c d a a) :start 7 :from-end t)
  (a a b c d a a))

(deftest-error delete-duplicates-start-list.7
  (delete-duplicates '(a a b c d a a) :start 8))

(deftest delete-duplicates-start-vector.1
  (delete-duplicates #(a a b c d a a) :start 0)
  #(b c d a))

(deftest delete-duplicates-start-vector.2
  (delete-duplicates #(a a b c d a a) :start 0 :from-end t)
  #(a b c d))

(deftest delete-duplicates-start-vector.3
  (delete-duplicates #(a a b c d a a) :start 1)
  #(a b c d a))

(deftest delete-duplicates-start-vector.4
  (delete-duplicates #(a a b c d a a) :start 1 :from-end t)
  #(a a b c d))

(deftest delete-duplicates-start-vector.5
  (delete-duplicates #(a a b c d a a) :start 7)
  #(a a b c d a a))

(deftest delete-duplicates-start-vector.6
  (delete-duplicates #(a a b c d a a) :start 7 :from-end t)
  #(a a b c d a a))

(deftest-error delete-duplicates-start-vector.7
  (delete-duplicates #(a a b c d a a) :start 8))

(deftest delete-duplicates-end-list.1
  (delete-duplicates '(a a b c d a a) :end 0)
  (a a b c d a a))

(deftest delete-duplicates-end-list.2
  (delete-duplicates '(a a b c d a a) :end 0 :from-end t)
  (a a b c d a a))

(deftest delete-duplicates-end-list.3
  (delete-duplicates '(a a b c d a a) :end 2)
  (a b c d a a))

(deftest delete-duplicates-end-list.4
  (delete-duplicates '(a a b c d a a) :end 2 :from-end t)
  (a b c d a a))

(deftest delete-duplicates-end-list.5
  (delete-duplicates '(a a b c d a a) :end 7)
  (b c d a))

(deftest delete-duplicates-end-list.6
  (delete-duplicates '(a a b c d a a) :end 7 :from-end t)
  (a b c d))

(deftest delete-duplicates-end-list.7
  (delete-duplicates '(a a b c d a a) :end nil)
  (b c d a))

(deftest delete-duplicates-end-list.8
  (delete-duplicates '(a a b c d a a) :end nil :from-end t)
  (a b c d))

(deftest-error delete-duplicates-end-list.9
  (delete-duplicates '(a a b c d a a) :end 8))

(deftest delete-duplicates-end-vector.1
  (delete-duplicates #(a a b c d a a) :end 0)
  #(a a b c d a a))

(deftest delete-duplicates-end-vector.2
  (delete-duplicates #(a a b c d a a) :end 0 :from-end t)
  #(a a b c d a a))

(deftest delete-duplicates-end-vector.3
  (delete-duplicates #(a a b c d a a) :end 2)
  #(a b c d a a))

(deftest delete-duplicates-end-vector.4
  (delete-duplicates #(a a b c d a a) :end 2 :from-end t)
  #(a b c d a a))

(deftest delete-duplicates-end-vector.5
  (delete-duplicates #(a a b c d a a) :end 7)
  #(b c d a))

(deftest delete-duplicates-end-vector.6
  (delete-duplicates #(a a b c d a a) :end 7 :from-end t)
  #(a b c d))

(deftest delete-duplicates-end-vector.7
  (delete-duplicates #(a a b c d a a) :end nil)
  #(b c d a))

(deftest delete-duplicates-end-vector.8
  (delete-duplicates #(a a b c d a a) :end nil :from-end t)
  #(a b c d))

(deftest-error delete-duplicates-end-vector.9
  (delete-duplicates #(a a b c d a a) :end 8))

(deftest delete-duplicates-start-end-list.1
  (delete-duplicates '(a a b c d a a) :start 1 :end 6)
  (a b c d a a))

(deftest delete-duplicates-start-end-list.2
  (delete-duplicates '(a a b c d a a) :start 1 :end 6 :from-end t)
  (a a b c d a))

(deftest delete-duplicates-start-end-list.3
  (delete-duplicates '(a a b c d a a) :start 1 :end 5)
  (a a b c d a a))

(deftest delete-duplicates-start-end-list.4
  (delete-duplicates '(a a b c d a a) :start 1 :end 5 :from-end t)
  (a a b c d a a))

(deftest delete-duplicates-start-end-vector.1
  (delete-duplicates #(a a b c d a a) :start 1 :end 6)
  #(a b c d a a))

(deftest delete-duplicates-start-end-vector.2
  (delete-duplicates #(a a b c d a a) :start 1 :end 6 :from-end t)
  #(a a b c d a))

(deftest delete-duplicates-start-end-vector.3
  (delete-duplicates #(a a b c d a a) :start 1 :end 5)
  #(a a b c d a a))

(deftest delete-duplicates-start-end-vector.4
  (delete-duplicates #(a a b c d a a) :start 1 :end 5 :from-end t)
  #(a a b c d a a))

(deftest-error delete-duplicates-error.1
  (eval '(delete-duplicates 10)))

(deftest-error! delete-duplicates-error.2
  (eval '(delete-duplicates)))

(deftest-error delete-duplicates-error.3
  (eval '(delete-duplicates nil nil)))

(deftest-error delete-duplicates-error.4
  (eval '(delete-duplicates nil :key)))

(deftest-error delete-duplicates-error.5
  (eval '(delete-duplicates nil :key 20)))

(deftest-error delete-duplicates-error.6
  (eval '(delete-duplicates nil :hello 20)))

(deftest-error delete-duplicates-error.7
  (eval '(delete-duplicates nil :test (constantly t) :test-not (constantly t))))

(deftest-error delete-duplicates-error.8
  (eval '(delete-duplicates '(a b c) :start 4)))

(deftest-error delete-duplicates-error.9
  (eval '(delete-duplicates #(a b c) :start 4)))

(deftest-error delete-duplicates-error.10
  (eval '(delete-duplicates '(a b c) :end 4)))

(deftest-error delete-duplicates-error.11
  (eval '(delete-duplicates #(a b c) :end 4)))

(deftest-error delete-duplicates-error.12
  (eval '(delete-duplicates '(a b c) :start 3 :end 1)))

(deftest-error delete-duplicates-error.13
  (eval '(delete-duplicates #(a b c) :start 3 :end 1)))


;;  ANSI Common Lisp
(deftest remove-duplicates-test.1
  (remove-duplicates "aBcDAbCd" :test #'char-equal :from-end t)
  "aBcD")

(deftest remove-duplicates-test.2
  (remove-duplicates '(a b c b d d e))
  (a c b d e))

(deftest remove-duplicates-test.3
  (remove-duplicates '(a b c b d d e) :from-end t)
  (a b c d e))

(deftest remove-duplicates-test.4
  (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
                     :test #'char-equal :key #'cadr)
  ((bar #\%) (baz #\A)))

(deftest remove-duplicates-test.5
  (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
                     :test #'char-equal :key #'cadr :from-end t)
  ((foo #\a) (bar #\%)))

(deftest remove-duplicates-test.6
  (let ((tester (list 0 1 2 3 4 5 6)))
    (delete-duplicates tester :key #'oddp :start 1 :end 6))
  (0 4 5 6))

