;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function SUBSTITUTE
;;
(deftest substitute-list.1
  (substitute 99 3 '(1 2 3 4 5 3 4 5 3))
  (1 2 99 4 5 99 4 5 99))

(deftest substitute-list.2
  (substitute 99 3 '(1 2 3 4 5 3 4 5 3) :count 0)
  (1 2 3 4 5 3 4 5 3))

(deftest substitute-list.3
  (substitute 99 3 '(1 2 3 4 5 3 4 5 3) :count 100)
  (1 2 99 4 5 99 4 5 99))

(deftest substitute-list.4
  (substitute 99 3 '(1 2 3 4 5 3 4 5 3) :count 2)
  (1 2 99 4 5 99 4 5 3))

(deftest substitute-list.5
  (substitute 99 3 '(1 2 3 4 5 3 4 5 3) :count 2 :from-end t)
  (1 2 3 4 5 99 4 5 99))

(deftest substitute-list.6
  (substitute 99 '(3) '(1 2 (3) 4 5 3 4 5 (3)) :test #'equal)
  (1 2 99 4 5 3 4 5 99))

(deftest substitute-list.7
  (substitute 99 '(3) '(1 2 (3) 4 5 3 4 5 (3)) :test-not (complement #'equal))
  (1 2 99 4 5 3 4 5 99))

(deftest substitute-list.8
  (substitute 99 3 '(1 2 (3) 4 5 3 4 5 (3))
              :key (lambda (x)
                     (if (consp x)
                       (car x)
                       x)))
  (1 2 99 4 5 99 4 5 99))

(deftest substitute-list.9
  (let ((x '(1 2 3 4 5)))
    (eq x (substitute 99 3 x)))
  nil)

(deftest substitute-vector.1
  (substitute 99 3 #(1 2 3 4 5 3 4 5 3))
  #(1 2 99 4 5 99 4 5 99))

(deftest substitute-vector.2
  (substitute 99 3 #(1 2 3 4 5 3 4 5 3) :count 0)
  #(1 2 3 4 5 3 4 5 3))

(deftest substitute-vector.3
  (substitute 99 3 #(1 2 3 4 5 3 4 5 3) :count 100)
  #(1 2 99 4 5 99 4 5 99))

(deftest substitute-vector.4
  (substitute 99 3 #(1 2 3 4 5 3 4 5 3) :count 2)
  #(1 2 99 4 5 99 4 5 3))

(deftest substitute-vector.5
  (substitute 99 3 #(1 2 3 4 5 3 4 5 3) :count 2 :from-end t)
  #(1 2 3 4 5 99 4 5 99))

(deftest substitute-vector.6
  (substitute 99 '(3) #(1 2 (3) 4 5 3 4 5 (3)) :test #'equal)
  #(1 2 99 4 5 3 4 5 99))

(deftest substitute-vector.7
  (substitute 99 '(3) #(1 2 (3) 4 5 3 4 5 (3)) :test-not (complement #'equal))
  #(1 2 99 4 5 3 4 5 99))

(deftest substitute-vector.8
  (substitute 99 3 '(1 2 (3) 4 5 3 4 5 (3))
              :key (lambda (x)
                     (if (consp x)
                       (car x)
                       x)))
  (1 2 99 4 5 99 4 5 99))

(deftest substitute-vector.9
  (let ((x #(1 2 3 4 5)))
    (eq x (substitute 99 3 x)))
  nil)

(deftest substitute-start-list.1
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 0)
  (1 99 4 99 2 4 99 99))

(deftest substitute-start-list.2
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 0 :from-end t)
  (1 99 4 99 2 4 99 99))

(deftest substitute-start-list.3
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 1)
  (1 99 4 99 2 4 99 99))

(deftest substitute-start-list.4
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 1 :from-end t)
  (1 99 4 99 2 4 99 99))

(deftest substitute-start-list.5
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 2)
  (1 3 4 99 2 4 99 99))

(deftest substitute-start-list.6
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 2 :from-end t)
  (1 3 4 99 2 4 99 99))

(deftest substitute-start-list.7
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 7)
  (1 3 4 3 2 4 3 99))

(deftest substitute-start-list.8
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 7 :from-end t)
  (1 3 4 3 2 4 3 99))

(deftest substitute-start-list.9
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 8)
  (1 3 4 3 2 4 3 3))

(deftest substitute-start-list.10
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 8 :from-end t)
  (1 3 4 3 2 4 3 3))

(deftest-error substitute-start-list.11
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 9))

(deftest-error substitute-start-list.12
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 9 :from-end t))

(deftest substitute-start-vector.1
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 0)
  #(1 99 4 99 2 4 99 99))

(deftest substitute-start-vector.2
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 0 :from-end t)
  #(1 99 4 99 2 4 99 99))

(deftest substitute-start-vector.3
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 1)
  #(1 99 4 99 2 4 99 99))

(deftest substitute-start-vector.4
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 1 :from-end t)
  #(1 99 4 99 2 4 99 99))

(deftest substitute-start-vector.5
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 2)
  #(1 3 4 99 2 4 99 99))

(deftest substitute-start-vector.6
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 2 :from-end t)
  #(1 3 4 99 2 4 99 99))

(deftest substitute-start-vector.7
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 7)
  #(1 3 4 3 2 4 3 99))

(deftest substitute-start-vector.8
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 7 :from-end t)
  #(1 3 4 3 2 4 3 99))

(deftest substitute-start-vector.9
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 8)
  #(1 3 4 3 2 4 3 3))

(deftest substitute-start-vector.10
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 8 :from-end t)
  #(1 3 4 3 2 4 3 3))

(deftest-error substitute-start-vector.11
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 9))

(deftest-error substitute-start-vector.12
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 9 :from-end t))

(deftest substitute-end-list.1
  (substitute 99 3 '(3 3 4 3 2 4 3 3) :end nil)
  (99 99 4 99 2 4 99 99))

(deftest substitute-end-list.2
  (substitute 99 3 '(3 3 7 3 2 4 3 3) :end nil :from-end t)
  (99 99 7 99 2 4 99 99))

(deftest substitute-end-list.3
  (substitute 99 3 '(3 3 4 3 2 4 3 3) :end 8)
  (99 99 4 99 2 4 99 99))

(deftest substitute-end-list.4
  (substitute 99 3 '(3 3 7 3 2 4 3 3) :end 8 :from-end t)
  (99 99 7 99 2 4 99 99))

(deftest substitute-end-list.5
  (substitute 99 3 '(3 3 4 3 2 4 3 3) :end 7)
  (99 99 4 99 2 4 99 3))

(deftest substitute-end-list.6
  (substitute 99 3 '(3 3 7 3 2 4 3 3) :end 7 :from-end t)
  (99 99 7 99 2 4 99 3))

(deftest substitute-end-list.7
  (substitute 99 3 '(3 3 4 3 2 4 3 3) :end 6)
  (99 99 4 99 2 4 3 3))

(deftest substitute-end-list.8
  (substitute 99 3 '(3 3 7 3 2 4 3 3) :end 6 :from-end t)
  (99 99 7 99 2 4 3 3))

(deftest substitute-end-list.9
  (substitute 99 3 '(3 3 4 3 2 4 3 3) :end 1)
  (99 3 4 3 2 4 3 3))

(deftest substitute-end-list.10
  (substitute 99 3 '(3 3 7 3 2 4 3 3) :end 1 :from-end t)
  (99 3 7 3 2 4 3 3))

(deftest substitute-end-list.11
  (substitute 99 3 '(3 3 4 3 2 4 3 3) :end 0)
  (3 3 4 3 2 4 3 3))

(deftest substitute-end-list.12
  (substitute 99 3 '(3 3 7 3 2 4 3 3) :end 0 :from-end t)
  (3 3 7 3 2 4 3 3))

(deftest-error substitute-end-list.13
  (substitute 99 3 '(3 3 4 3 2 4 3 3) :end 9))

(deftest-error substitute-end-list.14
  (substitute 99 3 '(3 3 7 3 2 4 3 3) :end 9 :from-end t))

(deftest substitute-end-vector.1
  (substitute 99 3 #(3 3 4 3 2 4 3 3) :end nil)
  #(99 99 4 99 2 4 99 99))

(deftest substitute-end-vector.2
  (substitute 99 3 #(3 3 7 3 2 4 3 3) :end nil :from-end t)
  #(99 99 7 99 2 4 99 99))

(deftest substitute-end-vector.3
  (substitute 99 3 #(3 3 4 3 2 4 3 3) :end 8)
  #(99 99 4 99 2 4 99 99))

(deftest substitute-end-vector.4
  (substitute 99 3 #(3 3 7 3 2 4 3 3) :end 8 :from-end t)
  #(99 99 7 99 2 4 99 99))

(deftest substitute-end-vector.5
  (substitute 99 3 #(3 3 4 3 2 4 3 3) :end 7)
  #(99 99 4 99 2 4 99 3))

(deftest substitute-end-vector.6
  (substitute 99 3 #(3 3 7 3 2 4 3 3) :end 7 :from-end t)
  #(99 99 7 99 2 4 99 3))

(deftest substitute-end-vector.7
  (substitute 99 3 #(3 3 4 3 2 4 3 3) :end 6)
  #(99 99 4 99 2 4 3 3))

(deftest substitute-end-vector.8
  (substitute 99 3 #(3 3 7 3 2 4 3 3) :end 6 :from-end t)
  #(99 99 7 99 2 4 3 3))

(deftest substitute-end-vector.9
  (substitute 99 3 #(3 3 4 3 2 4 3 3) :end 1)
  #(99 3 4 3 2 4 3 3))

(deftest substitute-end-vector.10
  (substitute 99 3 #(3 3 7 3 2 4 3 3) :end 1 :from-end t)
  #(99 3 7 3 2 4 3 3))

(deftest substitute-end-vector.11
  (substitute 99 3 #(3 3 4 3 2 4 3 3) :end 0)
  #(3 3 4 3 2 4 3 3))

(deftest substitute-end-vector.12
  (substitute 99 3 #(3 3 7 3 2 4 3 3) :end 0 :from-end t)
  #(3 3 7 3 2 4 3 3))

(deftest-error substitute-end-vector.13
  (substitute 99 3 #(3 3 4 3 2 4 3 3) :end 9))

(deftest-error substitute-end-vector.14
  (substitute 99 3 #(3 3 7 3 2 4 3 3) :end 9 :from-end t))

(deftest substitute-start-end-list.1
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 0)
  (1 2 3 3 6 7 3 3 8))

(deftest substitute-start-end-list.2
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 0 :from-end t)
  (1 2 3 3 6 7 3 3 8))

(deftest substitute-start-end-list.3
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 9)
  (1 2 99 99 6 7 99 99 8))

(deftest substitute-start-end-list.4
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 9 :from-end t)
  (1 2 99 99 6 7 99 99 8))

(deftest substitute-start-end-list.5
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 3 :end 9)
  (1 2 3 99 6 7 99 99 8))

(deftest substitute-start-end-list.6
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 3 :end 9 :from-end t)
  (1 2 3 99 6 7 99 99 8))

(deftest substitute-start-end-list.7
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 6)
  (1 2 99 99 6 7 3 3 8))

(deftest substitute-start-end-list.8
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 6 :from-end t)
  (1 2 99 99 6 7 3 3 8))

(deftest substitute-start-end-list.9
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 7)
  (1 2 99 99 6 7 99 3 8))

(deftest substitute-start-end-list.10
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 7 :from-end t)
  (1 2 99 99 6 7 99 3 8))

(deftest substitute-start-end-list.11
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 7 :end 7)
  (1 2 3 3 6 7 3 3 8))

(deftest substitute-start-end-list.12
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 7 :end 7 :from-end t)
  (1 2 3 3 6 7 3 3 8))

(deftest-error substitute-start-end-list.13
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 10))

(deftest-error substitute-start-end-list.14
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 10 :from-end t))

(deftest substitute-start-end-vector.1
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 0)
  #(1 2 3 3 6 7 3 3 8))

(deftest substitute-start-end-vector.2
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 0 :from-end t)
  #(1 2 3 3 6 7 3 3 8))

(deftest substitute-start-end-vector.3
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 9)
  #(1 2 99 99 6 7 99 99 8))

(deftest substitute-start-end-vector.4
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 9 :from-end t)
  #(1 2 99 99 6 7 99 99 8))

(deftest substitute-start-end-vector.5
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 3 :end 9)
  #(1 2 3 99 6 7 99 99 8))

(deftest substitute-start-end-vector.6
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 3 :end 9 :from-end t)
  #(1 2 3 99 6 7 99 99 8))

(deftest substitute-start-end-vector.7
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 6)
  #(1 2 99 99 6 7 3 3 8))

(deftest substitute-start-end-vector.8
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 6 :from-end t)
  #(1 2 99 99 6 7 3 3 8))

(deftest substitute-start-end-vector.9
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 7)
  #(1 2 99 99 6 7 99 3 8))

(deftest substitute-start-end-vector.10
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 7 :from-end t)
  #(1 2 99 99 6 7 99 3 8))

(deftest substitute-start-end-vector.11
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 7 :end 7)
  #(1 2 3 3 6 7 3 3 8))

(deftest substitute-start-end-vector.12
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 7 :end 7 :from-end t)
  #(1 2 3 3 6 7 3 3 8))

(deftest-error substitute-start-end-vector.13
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 10))

(deftest-error substitute-start-end-vector.14
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 10 :from-end t))

(deftest substitute-count-list.1
  (substitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 0)
  (1 3 3 3 5 6 7 3 3 3 8 9))

(deftest substitute-count-list.2
  (substitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 1)
  (1 99 3 3 5 6 7 3 3 3 8 9))

(deftest substitute-count-list.3
  (substitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 4)
  (1 99 99 99 5 6 7 99 3 3 8 9))

(deftest substitute-count-list.4
  (substitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 0 :from-end t)
  (1 3 3 3 5 6 7 3 3 3 8 9))

(deftest substitute-count-list.5
  (substitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 1 :from-end t)
  (1 3 3 3 5 6 7 3 3 99 8 9))

(deftest substitute-count-list.6
  (substitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 4 :from-end t)
  (1 3 3 99 5 6 7 99 99 99 8 9))

(deftest substitute-count-vector.1
  (substitute 99 3 #(1 3 3 3 5 6 7 3 3 3 8 9) :count 0)
  #(1 3 3 3 5 6 7 3 3 3 8 9))

(deftest substitute-count-vector.2
  (substitute 99 3 #(1 3 3 3 5 6 7 3 3 3 8 9) :count 1)
  #(1 99 3 3 5 6 7 3 3 3 8 9))

(deftest substitute-count-vector.3
  (substitute 99 3 #(1 3 3 3 5 6 7 3 3 3 8 9) :count 4)
  #(1 99 99 99 5 6 7 99 3 3 8 9))

(deftest substitute-count-vector.4
  (substitute 99 3 #(1 3 3 3 5 6 7 3 3 3 8 9) :count 0 :from-end t)
  #(1 3 3 3 5 6 7 3 3 3 8 9))

(deftest substitute-count-vector.5
  (substitute 99 3 #(1 3 3 3 5 6 7 3 3 3 8 9) :count 1 :from-end t)
  #(1 3 3 3 5 6 7 3 3 99 8 9))

(deftest substitute-count-vector.6
  (substitute 99 3 #(1 3 3 3 5 6 7 3 3 3 8 9) :count 4 :from-end t)
  #(1 3 3 99 5 6 7 99 99 99 8 9))

(deftest-error substitute-error.1
  (eval '(substitute 10 20 30)))

(deftest-error! substitute-error.2
  (eval '(substitute 10 20)))

(deftest-error substitute-error.3
  (eval '(substitute 10 20 nil nil)))

(deftest-error substitute-error.4
  (eval '(substitute 10 20 nil :key)))

(deftest-error substitute-error.5
  (eval '(substitute 10 20 nil :key 10)))

(deftest-error substitute-error.6
  (eval '(substitute 10 20 nil :hello 10)))

(deftest-error substitute-error.7
  (eval '(substitute 10 20 nil :test (constantly t) :test-not (constantly t))))

(deftest-error substitute-error.8
  (eval '(substitute 10 20 '(a b c) :start 4)))

(deftest-error substitute-error.9
  (eval '(substitute 10 20 '(a b c) :end 4)))

(deftest-error substitute-error.10
  (eval '(substitute 10 20 #(a b c) :start 4)))

(deftest-error substitute-error.11
  (eval '(substitute 10 20 #(a b c) :end 4)))


;;
;;  Function NSUBSTITUTE
;;
(deftest nsubstitute-list.1
  (nsubstitute 99 3 '(1 2 3 4 5 3 4 5 3))
  (1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-list.2
  (nsubstitute 99 3 '(1 2 3 4 5 3 4 5 3) :count 0)
  (1 2 3 4 5 3 4 5 3))

(deftest nsubstitute-list.3
  (nsubstitute 99 3 '(1 2 3 4 5 3 4 5 3) :count 100)
  (1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-list.4
  (nsubstitute 99 3 '(1 2 3 4 5 3 4 5 3) :count 2)
  (1 2 99 4 5 99 4 5 3))

(deftest nsubstitute-list.5
  (nsubstitute 99 3 '(1 2 3 4 5 3 4 5 3) :count 2 :from-end t)
  (1 2 3 4 5 99 4 5 99))

(deftest nsubstitute-list.6
  (nsubstitute 99 '(3) '(1 2 (3) 4 5 3 4 5 (3)) :test #'equal)
  (1 2 99 4 5 3 4 5 99))

(deftest nsubstitute-list.7
  (nsubstitute 99 '(3) '(1 2 (3) 4 5 3 4 5 (3)) :test-not (complement #'equal))
  (1 2 99 4 5 3 4 5 99))

(deftest nsubstitute-list.8
  (nsubstitute 99 3 '(1 2 (3) 4 5 3 4 5 (3))
               :key (lambda (x)
                      (if (consp x)
                        (car x)
                        x)))
  (1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-list.9
  (let ((x '(1 2 3 4 5)))
    (eq x (nsubstitute 99 3 x)))
  t)

(deftest nsubstitute-vector.1
  (nsubstitute 99 3 #(1 2 3 4 5 3 4 5 3))
  #(1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-vector.2
  (nsubstitute 99 3 #(1 2 3 4 5 3 4 5 3) :count 0)
  #(1 2 3 4 5 3 4 5 3))

(deftest nsubstitute-vector.3
  (nsubstitute 99 3 #(1 2 3 4 5 3 4 5 3) :count 100)
  #(1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-vector.4
  (nsubstitute 99 3 #(1 2 3 4 5 3 4 5 3) :count 2)
  #(1 2 99 4 5 99 4 5 3))

(deftest nsubstitute-vector.5
  (nsubstitute 99 3 #(1 2 3 4 5 3 4 5 3) :count 2 :from-end t)
  #(1 2 3 4 5 99 4 5 99))

(deftest nsubstitute-vector.6
  (nsubstitute 99 '(3) #(1 2 (3) 4 5 3 4 5 (3)) :test #'equal)
  #(1 2 99 4 5 3 4 5 99))

(deftest nsubstitute-vector.7
  (nsubstitute 99 '(3) #(1 2 (3) 4 5 3 4 5 (3)) :test-not (complement #'equal))
  #(1 2 99 4 5 3 4 5 99))

(deftest nsubstitute-vector.8
  (nsubstitute 99 3 '(1 2 (3) 4 5 3 4 5 (3))
               :key (lambda (x)
                      (if (consp x)
                        (car x)
                        x)))
  (1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-vector.9
  (let ((x #(1 2 3 4 5)))
    (eq x (nsubstitute 99 3 x)))
  t)

(deftest nsubstitute-start-list.1
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 0)
  (1 99 4 99 2 4 99 99))

(deftest nsubstitute-start-list.2
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 0 :from-end t)
  (1 99 4 99 2 4 99 99))

(deftest nsubstitute-start-list.3
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 1)
  (1 99 4 99 2 4 99 99))

(deftest nsubstitute-start-list.4
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 1 :from-end t)
  (1 99 4 99 2 4 99 99))

(deftest nsubstitute-start-list.5
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 2)
  (1 3 4 99 2 4 99 99))

(deftest nsubstitute-start-list.6
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 2 :from-end t)
  (1 3 4 99 2 4 99 99))

(deftest nsubstitute-start-list.7
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 7)
  (1 3 4 3 2 4 3 99))

(deftest nsubstitute-start-list.8
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 7 :from-end t)
  (1 3 4 3 2 4 3 99))

(deftest nsubstitute-start-list.9
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 8)
  (1 3 4 3 2 4 3 3))

(deftest nsubstitute-start-list.10
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 8 :from-end t)
  (1 3 4 3 2 4 3 3))

(deftest-error nsubstitute-start-list.11
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 9))

(deftest-error nsubstitute-start-list.12
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 9 :from-end t))

(deftest nsubstitute-start-vector.1
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 0)
  #(1 99 4 99 2 4 99 99))

(deftest nsubstitute-start-vector.2
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 0 :from-end t)
  #(1 99 4 99 2 4 99 99))

(deftest nsubstitute-start-vector.3
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 1)
  #(1 99 4 99 2 4 99 99))

(deftest nsubstitute-start-vector.4
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 1 :from-end t)
  #(1 99 4 99 2 4 99 99))

(deftest nsubstitute-start-vector.5
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 2)
  #(1 3 4 99 2 4 99 99))

(deftest nsubstitute-start-vector.6
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 2 :from-end t)
  #(1 3 4 99 2 4 99 99))

(deftest nsubstitute-start-vector.7
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 7)
  #(1 3 4 3 2 4 3 99))

(deftest nsubstitute-start-vector.8
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 7 :from-end t)
  #(1 3 4 3 2 4 3 99))

(deftest nsubstitute-start-vector.9
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 8)
  #(1 3 4 3 2 4 3 3))

(deftest nsubstitute-start-vector.10
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 8 :from-end t)
  #(1 3 4 3 2 4 3 3))

(deftest-error nsubstitute-start-vector.11
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 9))

(deftest-error nsubstitute-start-vector.12
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 9 :from-end t))

(deftest nsubstitute-end-list.1
  (nsubstitute 99 3 '(3 3 4 3 2 4 3 3) :end nil)
  (99 99 4 99 2 4 99 99))

(deftest nsubstitute-end-list.2
  (nsubstitute 99 3 '(3 3 7 3 2 4 3 3) :end nil :from-end t)
  (99 99 7 99 2 4 99 99))

(deftest nsubstitute-end-list.3
  (nsubstitute 99 3 '(3 3 4 3 2 4 3 3) :end 8)
  (99 99 4 99 2 4 99 99))

(deftest nsubstitute-end-list.4
  (nsubstitute 99 3 '(3 3 7 3 2 4 3 3) :end 8 :from-end t)
  (99 99 7 99 2 4 99 99))

(deftest nsubstitute-end-list.5
  (nsubstitute 99 3 '(3 3 4 3 2 4 3 3) :end 7)
  (99 99 4 99 2 4 99 3))

(deftest nsubstitute-end-list.6
  (nsubstitute 99 3 '(3 3 7 3 2 4 3 3) :end 7 :from-end t)
  (99 99 7 99 2 4 99 3))

(deftest nsubstitute-end-list.7
  (nsubstitute 99 3 '(3 3 4 3 2 4 3 3) :end 6)
  (99 99 4 99 2 4 3 3))

(deftest nsubstitute-end-list.8
  (nsubstitute 99 3 '(3 3 7 3 2 4 3 3) :end 6 :from-end t)
  (99 99 7 99 2 4 3 3))

(deftest nsubstitute-end-list.9
  (nsubstitute 99 3 '(3 3 4 3 2 4 3 3) :end 1)
  (99 3 4 3 2 4 3 3))

(deftest nsubstitute-end-list.10
  (nsubstitute 99 3 '(3 3 7 3 2 4 3 3) :end 1 :from-end t)
  (99 3 7 3 2 4 3 3))

(deftest nsubstitute-end-list.11
  (nsubstitute 99 3 '(3 3 4 3 2 4 3 3) :end 0)
  (3 3 4 3 2 4 3 3))

(deftest nsubstitute-end-list.12
  (nsubstitute 99 3 '(3 3 7 3 2 4 3 3) :end 0 :from-end t)
  (3 3 7 3 2 4 3 3))

(deftest-error nsubstitute-end-list.13
  (nsubstitute 99 3 '(3 3 4 3 2 4 3 3) :end 9))

(deftest-error nsubstitute-end-list.14
  (nsubstitute 99 3 '(3 3 7 3 2 4 3 3) :end 9 :from-end t))

(deftest nsubstitute-end-vector.1
  (nsubstitute 99 3 #(3 3 4 3 2 4 3 3) :end nil)
  #(99 99 4 99 2 4 99 99))

(deftest nsubstitute-end-vector.2
  (nsubstitute 99 3 #(3 3 7 3 2 4 3 3) :end nil :from-end t)
  #(99 99 7 99 2 4 99 99))

(deftest nsubstitute-end-vector.3
  (nsubstitute 99 3 #(3 3 4 3 2 4 3 3) :end 8)
  #(99 99 4 99 2 4 99 99))

(deftest nsubstitute-end-vector.4
  (nsubstitute 99 3 #(3 3 7 3 2 4 3 3) :end 8 :from-end t)
  #(99 99 7 99 2 4 99 99))

(deftest nsubstitute-end-vector.5
  (nsubstitute 99 3 #(3 3 4 3 2 4 3 3) :end 7)
  #(99 99 4 99 2 4 99 3))

(deftest nsubstitute-end-vector.6
  (nsubstitute 99 3 #(3 3 7 3 2 4 3 3) :end 7 :from-end t)
  #(99 99 7 99 2 4 99 3))

(deftest nsubstitute-end-vector.7
  (nsubstitute 99 3 #(3 3 4 3 2 4 3 3) :end 6)
  #(99 99 4 99 2 4 3 3))

(deftest nsubstitute-end-vector.8
  (nsubstitute 99 3 #(3 3 7 3 2 4 3 3) :end 6 :from-end t)
  #(99 99 7 99 2 4 3 3))

(deftest nsubstitute-end-vector.9
  (nsubstitute 99 3 #(3 3 4 3 2 4 3 3) :end 1)
  #(99 3 4 3 2 4 3 3))

(deftest nsubstitute-end-vector.10
  (nsubstitute 99 3 #(3 3 7 3 2 4 3 3) :end 1 :from-end t)
  #(99 3 7 3 2 4 3 3))

(deftest nsubstitute-end-vector.11
  (nsubstitute 99 3 #(3 3 4 3 2 4 3 3) :end 0)
  #(3 3 4 3 2 4 3 3))

(deftest nsubstitute-end-vector.12
  (nsubstitute 99 3 #(3 3 7 3 2 4 3 3) :end 0 :from-end t)
  #(3 3 7 3 2 4 3 3))

(deftest-error nsubstitute-end-vector.13
  (nsubstitute 99 3 #(3 3 4 3 2 4 3 3) :end 9))

(deftest-error nsubstitute-end-vector.14
  (nsubstitute 99 3 #(3 3 7 3 2 4 3 3) :end 9 :from-end t))

(deftest nsubstitute-start-end-list.1
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 0)
  (1 2 3 3 6 7 3 3 8))

(deftest nsubstitute-start-end-list.2
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 0 :from-end t)
  (1 2 3 3 6 7 3 3 8))

(deftest nsubstitute-start-end-list.3
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 9)
  (1 2 99 99 6 7 99 99 8))

(deftest nsubstitute-start-end-list.4
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 9 :from-end t)
  (1 2 99 99 6 7 99 99 8))

(deftest nsubstitute-start-end-list.5
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 3 :end 9)
  (1 2 3 99 6 7 99 99 8))

(deftest nsubstitute-start-end-list.6
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 3 :end 9 :from-end t)
  (1 2 3 99 6 7 99 99 8))

(deftest nsubstitute-start-end-list.7
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 6)
  (1 2 99 99 6 7 3 3 8))

(deftest nsubstitute-start-end-list.8
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 6 :from-end t)
  (1 2 99 99 6 7 3 3 8))

(deftest nsubstitute-start-end-list.9
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 7)
  (1 2 99 99 6 7 99 3 8))

(deftest nsubstitute-start-end-list.10
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 7 :from-end t)
  (1 2 99 99 6 7 99 3 8))

(deftest nsubstitute-start-end-list.11
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 7 :end 7)
  (1 2 3 3 6 7 3 3 8))

(deftest nsubstitute-start-end-list.12
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 7 :end 7 :from-end t)
  (1 2 3 3 6 7 3 3 8))

(deftest-error nsubstitute-start-end-list.13
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 10))

(deftest-error nsubstitute-start-end-list.14
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 10 :from-end t))

(deftest nsubstitute-start-end-vector.1
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 0)
  #(1 2 3 3 6 7 3 3 8))

(deftest nsubstitute-start-end-vector.2
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 0 :from-end t)
  #(1 2 3 3 6 7 3 3 8))

(deftest nsubstitute-start-end-vector.3
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 9)
  #(1 2 99 99 6 7 99 99 8))

(deftest nsubstitute-start-end-vector.4
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 9 :from-end t)
  #(1 2 99 99 6 7 99 99 8))

(deftest nsubstitute-start-end-vector.5
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 3 :end 9)
  #(1 2 3 99 6 7 99 99 8))

(deftest nsubstitute-start-end-vector.6
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 3 :end 9 :from-end t)
  #(1 2 3 99 6 7 99 99 8))

(deftest nsubstitute-start-end-vector.7
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 6)
  #(1 2 99 99 6 7 3 3 8))

(deftest nsubstitute-start-end-vector.8
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 6 :from-end t)
  #(1 2 99 99 6 7 3 3 8))

(deftest nsubstitute-start-end-vector.9
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 7)
  #(1 2 99 99 6 7 99 3 8))

(deftest nsubstitute-start-end-vector.10
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 7 :from-end t)
  #(1 2 99 99 6 7 99 3 8))

(deftest nsubstitute-start-end-vector.11
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 7 :end 7)
  #(1 2 3 3 6 7 3 3 8))

(deftest nsubstitute-start-end-vector.12
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 7 :end 7 :from-end t)
  #(1 2 3 3 6 7 3 3 8))

(deftest-error nsubstitute-start-end-vector.13
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 10))

(deftest-error nsubstitute-start-end-vector.14
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 10 :from-end t))

(deftest nsubstitute-count-list.1
  (nsubstitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 0)
  (1 3 3 3 5 6 7 3 3 3 8 9))

(deftest nsubstitute-count-list.2
  (nsubstitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 1)
  (1 99 3 3 5 6 7 3 3 3 8 9))

(deftest nsubstitute-count-list.3
  (nsubstitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 4)
  (1 99 99 99 5 6 7 99 3 3 8 9))

(deftest nsubstitute-count-list.4
  (nsubstitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 0 :from-end t)
  (1 3 3 3 5 6 7 3 3 3 8 9))

(deftest nsubstitute-count-list.5
  (nsubstitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 1 :from-end t)
  (1 3 3 3 5 6 7 3 3 99 8 9))

(deftest nsubstitute-count-list.6
  (nsubstitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 4 :from-end t)
  (1 3 3 99 5 6 7 99 99 99 8 9))

(deftest nsubstitute-count-vector.1
  (nsubstitute 99 3 #(1 3 3 3 5 6 7 3 3 3 8 9) :count 0)
  #(1 3 3 3 5 6 7 3 3 3 8 9))

(deftest nsubstitute-count-vector.2
  (nsubstitute 99 3 #(1 3 3 3 5 6 7 3 3 3 8 9) :count 1)
  #(1 99 3 3 5 6 7 3 3 3 8 9))

(deftest nsubstitute-count-vector.3
  (nsubstitute 99 3 #(1 3 3 3 5 6 7 3 3 3 8 9) :count 4)
  #(1 99 99 99 5 6 7 99 3 3 8 9))

(deftest nsubstitute-count-vector.4
  (nsubstitute 99 3 #(1 3 3 3 5 6 7 3 3 3 8 9) :count 0 :from-end t)
  #(1 3 3 3 5 6 7 3 3 3 8 9))

(deftest nsubstitute-count-vector.5
  (nsubstitute 99 3 #(1 3 3 3 5 6 7 3 3 3 8 9) :count 1 :from-end t)
  #(1 3 3 3 5 6 7 3 3 99 8 9))

(deftest nsubstitute-count-vector.6
  (nsubstitute 99 3 #(1 3 3 3 5 6 7 3 3 3 8 9) :count 4 :from-end t)
  #(1 3 3 99 5 6 7 99 99 99 8 9))

(deftest-error nsubstitute-error.1
  (eval '(nsubstitute 10 20 30)))

(deftest-error! nsubstitute-error.2
  (eval '(nsubstitute 10 20)))

(deftest-error nsubstitute-error.3
  (eval '(nsubstitute 10 20 nil nil)))

(deftest-error nsubstitute-error.4
  (eval '(nsubstitute 10 20 nil :key)))

(deftest-error nsubstitute-error.5
  (eval '(nsubstitute 10 20 nil :key 10)))

(deftest-error nsubstitute-error.6
  (eval '(nsubstitute 10 20 nil :hello 10)))

(deftest-error nsubstitute-error.7
  (eval '(nsubstitute 10 20 nil :test (constantly t) :test-not (constantly t))))

(deftest-error nsubstitute-error.8
  (eval '(nsubstitute 10 20 '(a b c) :start 4)))

(deftest-error nsubstitute-error.9
  (eval '(nsubstitute 10 20 '(a b c) :end 4)))

(deftest-error nsubstitute-error.10
  (eval '(nsubstitute 10 20 #(a b c) :start 4)))

(deftest-error nsubstitute-error.11
  (eval '(nsubstitute 10 20 #(a b c) :end 4)))


;;
;;  Function SUBSTITUTE-IF
;;
(deftest substitute-if-list.1
  (substitute-if 99 (lambda (x) (eql x 3))
                 '(1 2 3 4 5 3 4 5 3))
  (1 2 99 4 5 99 4 5 99))

(deftest substitute-if-list.2
  (substitute-if 99 (lambda (x) (eql x 3))
                 '(1 2 3 4 5 3 4 5 3) :count 2)
  (1 2 99 4 5 99 4 5 3))

(deftest substitute-if-list.3
  (substitute-if 99 (lambda (x) (eql x 3))
                 '(1 2 3 4 5 3 4 5 3) :count 2 :from-end t)
  (1 2 3 4 5 99 4 5 99))

(deftest substitute-if-list.4
  (substitute-if 99 (lambda (x) (eql x 3))
                 '(1 2 (3) 4 5 3 4 5 (3))
                 :key (lambda (x)
                        (if (consp x)
                          (car x)
                          x)))
  (1 2 99 4 5 99 4 5 99))

(deftest substitute-if-list.5
  (let ((x '(1 2 3 4 5)))
    (eq x (substitute-if 99 (lambda (x) (eql x 3)) x)))
  nil)

(deftest substitute-if-vector.1
  (substitute-if 99 (lambda (x) (eql x 3))
                 #(1 2 3 4 5 3 4 5 3))
  #(1 2 99 4 5 99 4 5 99))

(deftest substitute-if-vector.2
  (substitute-if 99 (lambda (x) (eql x 3))
                 #(1 2 3 4 5 3 4 5 3) :count 2)
  #(1 2 99 4 5 99 4 5 3))

(deftest substitute-if-vector.3
  (substitute-if 99 (lambda (x) (eql x 3))
                 #(1 2 3 4 5 3 4 5 3) :count 2 :from-end t)
  #(1 2 3 4 5 99 4 5 99))

(deftest substitute-if-vector.4
  (substitute-if 99 (lambda (x) (eql x 3))
                 #(1 2 (3) 4 5 3 4 5 (3))
                 :key (lambda (x)
                        (if (consp x)
                          (car x)
                          x)))
  #(1 2 99 4 5 99 4 5 99))

(deftest substitute-if-vector.5
  (let ((x #(1 2 3 4 5)))
    (eq x (substitute-if 99 (lambda (x) (eql x 3)) x)))
  nil)

(deftest substitute-if-start.1
  (substitute-if 99 (lambda (x) (eql x 3))
                 '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :start 0)
  (99 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest substitute-if-start.2
  (substitute-if 99 (lambda (x) (eql x 3))
                 '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :start 1)
  (3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest substitute-if-start.3
  (substitute-if 99 (lambda (x) (eql x 3))
                 '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :start 5 :end nil)
  (3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest substitute-if-start.4
  (substitute-if 99 (lambda (x) (eql x 3))
                 '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :start 6 :end 13)
  (3 4 5 1 2 3 4 5 1 2 99 4 5 3))

(deftest substitute-if-start.5
  (substitute-if 99 (lambda (x) (eql x 3))
                 '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :end 11)
  (99 4 5 1 2 99 4 5 1 2 99 4 5 3))

(deftest substitute-if-start.6
  (substitute-if 99 (lambda (x) (eql x 3))
                 #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :start 0)
  #(99 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest substitute-if-start.7
  (substitute-if 99 (lambda (x) (eql x 3))
                 #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :start 1)
  #(3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest substitute-if-start.8
  (substitute-if 99 (lambda (x) (eql x 3))
                 #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :start 5 :end nil)
  #(3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest substitute-if-start.9
  (substitute-if 99 (lambda (x) (eql x 3))
                 #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :start 6 :end 13)
  #(3 4 5 1 2 3 4 5 1 2 99 4 5 3))

(deftest substitute-if-start.10
  (substitute-if 99 (lambda (x) (eql x 3))
                 #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :end 11)
  #(99 4 5 1 2 99 4 5 1 2 99 4 5 3))

(deftest substitute-if-count.1
  (substitute-if 99 (lambda (x) (eql x 3))
                 '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :count 0)
  (3 4 5 1 2 3 4 5 1 2 3 4 5 3))

(deftest substitute-if-count.2
  (substitute-if 99 (lambda (x) (eql x 3))
                 '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :count 2)
  (99 4 5 1 2 99 4 5 1 2 3 4 5 3))

(deftest substitute-if-count.3
  (substitute-if 99 (lambda (x) (eql x 3))
                 '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :count 2 :from-end t)
  (3 4 5 1 2 3 4 5 1 2 99 4 5 99))

(deftest substitute-if-count.4
  (substitute-if 99 (lambda (x) (eql x 3))
                 #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :count 0)
  #(3 4 5 1 2 3 4 5 1 2 3 4 5 3))

(deftest substitute-if-count.5
  (substitute-if 99 (lambda (x) (eql x 3))
                 #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :count 2)
  #(99 4 5 1 2 99 4 5 1 2 3 4 5 3))

(deftest substitute-if-count.6
  (substitute-if 99 (lambda (x) (eql x 3))
                 #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                 :count 2 :from-end t)
  #(3 4 5 1 2 3 4 5 1 2 99 4 5 99))

(deftest-error substitute-if-error.1
  (eval '(substitute-if 10 (constantly nil) 30)))

(deftest-error! substitute-if-error.2
  (eval '(substitute-if 10 (constantly nil))))

(deftest-error substitute-if-error.3
  (eval '(substitute-if 10 (constantly nil) nil nil)))

(deftest-error substitute-if-error.4
  (eval '(substitute-if 10 (constantly nil) nil :key)))

(deftest-error substitute-if-error.5
  (eval '(substitute-if 10 (constantly nil) nil :key 10)))

(deftest-error substitute-if-error.6
  (eval '(substitute-if 10 (constantly nil) nil :hello 10)))

(deftest-error substitute-if-error.7
  (eval '(substitute-if 10 (constantly nil) '(a b c) :start 4)))

(deftest-error substitute-if-error.8
  (eval '(substitute-if 10 (constantly nil) '(a b c) :end 4)))

(deftest-error substitute-if-error.9
  (eval '(substitute-if 10 (constantly nil) #(a b c) :start 4)))

(deftest-error substitute-if-error.10
  (eval '(substitute-if 10 (constantly nil) #(a b c) :end 4)))


;;
;;  Function NSUBSTITUTE-IF
;;
(deftest nsubstitute-if-list.1
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  '(1 2 3 4 5 3 4 5 3))
  (1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-if-list.2
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  '(1 2 3 4 5 3 4 5 3) :count 2)
  (1 2 99 4 5 99 4 5 3))

(deftest nsubstitute-if-list.3
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  '(1 2 3 4 5 3 4 5 3) :count 2 :from-end t)
  (1 2 3 4 5 99 4 5 99))

(deftest nsubstitute-if-list.4
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  '(1 2 (3) 4 5 3 4 5 (3))
                  :key (lambda (x)
                         (if (consp x)
                           (car x)
                           x)))
  (1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-if-list.5
  (let ((x '(1 2 3 4 5)))
    (eq x (nsubstitute-if 99 (lambda (x) (eql x 3)) x)))
  t)

(deftest nsubstitute-if-vector.1
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  #(1 2 3 4 5 3 4 5 3))
  #(1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-if-vector.2
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  #(1 2 3 4 5 3 4 5 3) :count 2)
  #(1 2 99 4 5 99 4 5 3))

(deftest nsubstitute-if-vector.3
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  #(1 2 3 4 5 3 4 5 3) :count 2 :from-end t)
  #(1 2 3 4 5 99 4 5 99))

(deftest nsubstitute-if-vector.4
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  #(1 2 (3) 4 5 3 4 5 (3))
                  :key (lambda (x)
                         (if (consp x)
                           (car x)
                           x)))
  #(1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-if-vector.5
  (let ((x #(1 2 3 4 5)))
    (eq x (nsubstitute-if 99 (lambda (x) (eql x 3)) x)))
  t)

(deftest nsubstitute-if-start.1
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :start 0)
  (99 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-start.2
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :start 1)
  (3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-start.3
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :start 5 :end nil)
  (3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-start.4
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :start 6 :end 13)
  (3 4 5 1 2 3 4 5 1 2 99 4 5 3))

(deftest nsubstitute-if-start.5
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :end 11)
  (99 4 5 1 2 99 4 5 1 2 99 4 5 3))

(deftest nsubstitute-if-start.6
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :start 0)
  #(99 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-start.7
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :start 1)
  #(3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-start.8
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :start 5 :end nil)
  #(3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-start.9
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :start 6 :end 13)
  #(3 4 5 1 2 3 4 5 1 2 99 4 5 3))

(deftest nsubstitute-if-start.10
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :end 11)
  #(99 4 5 1 2 99 4 5 1 2 99 4 5 3))

(deftest nsubstitute-if-count.1
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :count 0)
  (3 4 5 1 2 3 4 5 1 2 3 4 5 3))

(deftest nsubstitute-if-count.2
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :count 2)
  (99 4 5 1 2 99 4 5 1 2 3 4 5 3))

(deftest nsubstitute-if-count.3
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :count 2 :from-end t)
  (3 4 5 1 2 3 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-count.4
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :count 0)
  #(3 4 5 1 2 3 4 5 1 2 3 4 5 3))

(deftest nsubstitute-if-count.5
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :count 2)
  #(99 4 5 1 2 99 4 5 1 2 3 4 5 3))

(deftest nsubstitute-if-count.6
  (nsubstitute-if 99 (lambda (x) (eql x 3))
                  #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                  :count 2 :from-end t)
  #(3 4 5 1 2 3 4 5 1 2 99 4 5 99))

(deftest-error nsubstitute-if-error.1
  (eval '(nsubstitute-if 10 (constantly nil) 30)))

(deftest-error! nsubstitute-if-error.2
  (eval '(nsubstitute-if 10 (constantly nil))))

(deftest-error nsubstitute-if-error.3
  (eval '(nsubstitute-if 10 (constantly nil) nil nil)))

(deftest-error nsubstitute-if-error.4
  (eval '(nsubstitute-if 10 (constantly nil) nil :key)))

(deftest-error nsubstitute-if-error.5
  (eval '(nsubstitute-if 10 (constantly nil) nil :key 10)))

(deftest-error nsubstitute-if-error.6
  (eval '(nsubstitute-if 10 (constantly nil) nil :hello 10)))

(deftest-error nsubstitute-if-error.7
  (eval '(nsubstitute-if 10 (constantly nil) '(a b c) :start 4)))

(deftest-error nsubstitute-if-error.8
  (eval '(nsubstitute-if 10 (constantly nil) '(a b c) :end 4)))

(deftest-error nsubstitute-if-error.9
  (eval '(nsubstitute-if 10 (constantly nil) #(a b c) :start 4)))

(deftest-error nsubstitute-if-error.10
  (eval '(nsubstitute-if 10 (constantly nil) #(a b c) :end 4)))


;;
;;  Function SUBSTITUTE-IF-NOT
;;
(deftest substitute-if-not-list.1
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     '(1 2 3 4 5 3 4 5 3))
  (1 2 99 4 5 99 4 5 99))

(deftest substitute-if-not-list.2
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     '(1 2 3 4 5 3 4 5 3) :count 2)
  (1 2 99 4 5 99 4 5 3))

(deftest substitute-if-not-list.3
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     '(1 2 3 4 5 3 4 5 3) :count 2 :from-end t)
  (1 2 3 4 5 99 4 5 99))

(deftest substitute-if-not-list.4
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     '(1 2 (3) 4 5 3 4 5 (3))
                     :key (lambda (x)
                            (if (consp x)
                              (car x)
                              x)))
  (1 2 99 4 5 99 4 5 99))

(deftest substitute-if-not-list.5
  (let ((x '(1 2 3 4 5)))
    (eq x (substitute-if-not 99 (lambda (x) (not (eql x 3))) x)))
  nil)

(deftest substitute-if-not-vector.1
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     #(1 2 3 4 5 3 4 5 3))
  #(1 2 99 4 5 99 4 5 99))

(deftest substitute-if-not-vector.2
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     #(1 2 3 4 5 3 4 5 3) :count 2)
  #(1 2 99 4 5 99 4 5 3))

(deftest substitute-if-not-vector.3
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     #(1 2 3 4 5 3 4 5 3) :count 2 :from-end t)
  #(1 2 3 4 5 99 4 5 99))

(deftest substitute-if-not-vector.4
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     #(1 2 (3) 4 5 3 4 5 (3))
                     :key (lambda (x)
                            (if (consp x)
                              (car x)
                              x)))
  #(1 2 99 4 5 99 4 5 99))

(deftest substitute-if-not-vector.5
  (let ((x #(1 2 3 4 5)))
    (eq x (substitute-if-not 99 (lambda (x) (not (eql x 3))) x)))
  nil)

(deftest substitute-if-not-start.1
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :start 0)
  (99 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest substitute-if-not-start.2
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :start 1)
  (3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest substitute-if-not-start.3
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :start 5 :end nil)
  (3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest substitute-if-not-start.4
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :start 6 :end 13)
  (3 4 5 1 2 3 4 5 1 2 99 4 5 3))

(deftest substitute-if-not-start.5
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :end 11)
  (99 4 5 1 2 99 4 5 1 2 99 4 5 3))

(deftest substitute-if-not-start.6
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :start 0)
  #(99 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest substitute-if-not-start.7
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :start 1)
  #(3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest substitute-if-not-start.8
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :start 5 :end nil)
  #(3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest substitute-if-not-start.9
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :start 6 :end 13)
  #(3 4 5 1 2 3 4 5 1 2 99 4 5 3))

(deftest substitute-if-not-start.10
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :end 11)
  #(99 4 5 1 2 99 4 5 1 2 99 4 5 3))

(deftest substitute-if-not-count.1
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :count 0)
  (3 4 5 1 2 3 4 5 1 2 3 4 5 3))

(deftest substitute-if-not-count.2
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :count 2)
  (99 4 5 1 2 99 4 5 1 2 3 4 5 3))

(deftest substitute-if-not-count.3
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :count 2 :from-end t)
  (3 4 5 1 2 3 4 5 1 2 99 4 5 99))

(deftest substitute-if-not-count.4
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :count 0)
  #(3 4 5 1 2 3 4 5 1 2 3 4 5 3))

(deftest substitute-if-not-count.5
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :count 2)
  #(99 4 5 1 2 99 4 5 1 2 3 4 5 3))

(deftest substitute-if-not-count.6
  (substitute-if-not 99 (lambda (x) (not (eql x 3)))
                     #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                     :count 2 :from-end t)
  #(3 4 5 1 2 3 4 5 1 2 99 4 5 99))

(deftest-error substitute-if-not-error.1
  (eval '(substitute-if-not 10 (constantly nil) 30)))

(deftest-error! substitute-if-not-error.2
  (eval '(substitute-if-not 10 (constantly nil))))

(deftest-error substitute-if-not-error.3
  (eval '(substitute-if-not 10 (constantly nil) nil nil)))

(deftest-error substitute-if-not-error.4
  (eval '(substitute-if-not 10 (constantly nil) nil :key)))

(deftest-error substitute-if-not-error.5
  (eval '(substitute-if-not 10 (constantly nil) nil :key 10)))

(deftest-error substitute-if-not-error.6
  (eval '(substitute-if-not 10 (constantly nil) nil :hello 10)))

(deftest-error substitute-if-not-error.7
  (eval '(substitute-if-not 10 (constantly nil) '(a b c) :start 4)))

(deftest-error substitute-if-not-error.8
  (eval '(substitute-if-not 10 (constantly nil) '(a b c) :end 4)))

(deftest-error substitute-if-not-error.9
  (eval '(substitute-if-not 10 (constantly nil) #(a b c) :start 4)))

(deftest-error substitute-if-not-error.10
  (eval '(substitute-if-not 10 (constantly nil) #(a b c) :end 4)))


;;
;;  Function NSUBSTITUTE-IF
;;
(deftest nsubstitute-if-not-list.1
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      '(1 2 3 4 5 3 4 5 3))
  (1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-if-not-list.2
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      '(1 2 3 4 5 3 4 5 3) :count 2)
  (1 2 99 4 5 99 4 5 3))

(deftest nsubstitute-if-not-list.3
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      '(1 2 3 4 5 3 4 5 3) :count 2 :from-end t)
  (1 2 3 4 5 99 4 5 99))

(deftest nsubstitute-if-not-list.4
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      '(1 2 (3) 4 5 3 4 5 (3))
                      :key (lambda (x)
                             (if (consp x)
                               (car x)
                               x)))
  (1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-if-not-list.5
  (let ((x '(1 2 3 4 5)))
    (eq x (nsubstitute-if-not 99 (lambda (x) (not (eql x 3))) x)))
  t)

(deftest nsubstitute-if-not-vector.1
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      #(1 2 3 4 5 3 4 5 3))
  #(1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-if-not-vector.2
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      #(1 2 3 4 5 3 4 5 3) :count 2)
  #(1 2 99 4 5 99 4 5 3))

(deftest nsubstitute-if-not-vector.3
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      #(1 2 3 4 5 3 4 5 3) :count 2 :from-end t)
  #(1 2 3 4 5 99 4 5 99))

(deftest nsubstitute-if-not-vector.4
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      #(1 2 (3) 4 5 3 4 5 (3))
                      :key (lambda (x)
                             (if (consp x)
                               (car x)
                               x)))
  #(1 2 99 4 5 99 4 5 99))

(deftest nsubstitute-if-not-vector.5
  (let ((x #(1 2 3 4 5)))
    (eq x (nsubstitute-if-not 99 (lambda (x) (not (eql x 3))) x)))
  t)

(deftest nsubstitute-if-not-start.1
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :start 0)
  (99 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-not-start.2
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :start 1)
  (3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-not-start.3
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :start 5 :end nil)
  (3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-not-start.4
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :start 6 :end 13)
  (3 4 5 1 2 3 4 5 1 2 99 4 5 3))

(deftest nsubstitute-if-not-start.5
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :end 11)
  (99 4 5 1 2 99 4 5 1 2 99 4 5 3))

(deftest nsubstitute-if-not-start.6
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :start 0)
  #(99 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-not-start.7
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :start 1)
  #(3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-not-start.8
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :start 5 :end nil)
  #(3 4 5 1 2 99 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-not-start.9
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :start 6 :end 13)
  #(3 4 5 1 2 3 4 5 1 2 99 4 5 3))

(deftest nsubstitute-if-not-start.10
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :end 11)
  #(99 4 5 1 2 99 4 5 1 2 99 4 5 3))

(deftest nsubstitute-if-not-count.1
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :count 0)
  (3 4 5 1 2 3 4 5 1 2 3 4 5 3))

(deftest nsubstitute-if-not-count.2
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :count 2)
  (99 4 5 1 2 99 4 5 1 2 3 4 5 3))

(deftest nsubstitute-if-not-count.3
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      '(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :count 2 :from-end t)
  (3 4 5 1 2 3 4 5 1 2 99 4 5 99))

(deftest nsubstitute-if-not-count.4
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :count 0)
  #(3 4 5 1 2 3 4 5 1 2 3 4 5 3))

(deftest nsubstitute-if-not-count.5
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :count 2)
  #(99 4 5 1 2 99 4 5 1 2 3 4 5 3))

(deftest nsubstitute-if-not-count.6
  (nsubstitute-if-not 99 (lambda (x) (not (eql x 3)))
                      #(3 4 5 1 2 3 4 5 1 2 3 4 5 3)
                      :count 2 :from-end t)
  #(3 4 5 1 2 3 4 5 1 2 99 4 5 99))

(deftest-error nsubstitute-if-not-error.1
  (eval '(nsubstitute-if-not 10 (constantly nil) 30)))

(deftest-error! nsubstitute-if-not-error.2
  (eval '(nsubstitute-if-not 10 (constantly nil))))

(deftest-error nsubstitute-if-not-error.3
  (eval '(nsubstitute-if-not 10 (constantly nil) nil nil)))

(deftest-error nsubstitute-if-not-error.4
  (eval '(nsubstitute-if-not 10 (constantly nil) nil :key)))

(deftest-error nsubstitute-if-not-error.5
  (eval '(nsubstitute-if-not 10 (constantly nil) nil :key 10)))

(deftest-error nsubstitute-if-not-error.6
  (eval '(nsubstitute-if-not 10 (constantly nil) nil :hello 10)))

(deftest-error nsubstitute-if-not-error.7
  (eval '(nsubstitute-if-not 10 (constantly nil) '(a b c) :start 4)))

(deftest-error nsubstitute-if-not-error.8
  (eval '(nsubstitute-if-not 10 (constantly nil) '(a b c) :end 4)))

(deftest-error nsubstitute-if-not-error.9
  (eval '(nsubstitute-if-not 10 (constantly nil) #(a b c) :start 4)))

(deftest-error nsubstitute-if-not-error.10
  (eval '(nsubstitute-if-not 10 (constantly nil) #(a b c) :end 4)))


;;  ANSI Common Lisp
(deftest substitute-test.1
  (substitute #\. #\SPACE "0 2 4 6")
  "0.2.4.6")

(deftest substitute-test.2
  (substitute 'b 'a '(0 a 2 a 4 a 6))
  (0 b 2 b 4 b 6))

(deftest substitute-test.3
  (substitute 9 4 '(1 2 4 1 3 4 5))
  (1 2 9 1 3 9 5))

(deftest substitute-test.4
  (substitute 9 4 #(1 2 4 1 3 4 5))
  #(1 2 9 1 3 9 5))

(deftest substitute-test.5
  (substitute 9 4 '(1 2 4 1 3 4 5) :count 1)
  (1 2 9 1 3 4 5))

(deftest substitute-test.6
  (substitute 9 4 #(1 2 4 1 3 4 5) :count 1)
  #(1 2 9 1 3 4 5))

(deftest substitute-test.7
  (substitute 9 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))

(deftest substitute-test.8
  (substitute 9 4 #(1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 9 5))

(deftest substitute-test.9
  (substitute 9 3 '(1 2 4 1 3 4 5) :test #'>)
  (9 9 4 9 3 4 5))

(deftest substitute-test.10
  (substitute 9 3 #(1 2 4 1 3 4 5) :test #'>)
  #(9 9 4 9 3 4 5))

(deftest substitute-if-test.1
  (substitute-if 0 #'evenp '((1) (2) (3) (4)) :start 2 :key #'car)
  ((1) (2) (3) 0))

(deftest substitute-if-test.2
  (substitute-if 0 #'evenp #((1) (2) (3) (4)) :start 2 :key #'car)
  #((1) (2) (3) 0))

(deftest substitute-if-test.3
  (substitute-if 9 #'oddp '(1 2 4 1 3 4 5))
  (9 2 4 9 9 4 9))

(deftest substitute-if-test.4
  (substitute-if 9 #'oddp #(1 2 4 1 3 4 5))
  #(9 2 4 9 9 4 9))

(deftest substitute-if-test.5
  (substitute-if 9 #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))

(deftest substitute-if-test.6
  (substitute-if 9 #'evenp #(1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 9 5))

(deftest nsubstitute-test.1
  (nsubstitute #\. #\SPACE "0 2 4 6")
  "0.2.4.6")

(deftest nsubstitute-test.2
  (nsubstitute 'b 'a '(0 a 2 a 4 a 6))
  (0 b 2 b 4 b 6))

(deftest nsubstitute-test.3
  (nsubstitute 9 4 '(1 2 4 1 3 4 5))
  (1 2 9 1 3 9 5))

(deftest nsubstitute-test.4
  (nsubstitute 9 4 #(1 2 4 1 3 4 5))
  #(1 2 9 1 3 9 5))

(deftest nsubstitute-test.5
  (nsubstitute 9 4 '(1 2 4 1 3 4 5) :count 1)
  (1 2 9 1 3 4 5))

(deftest nsubstitute-test.6
  (nsubstitute 9 4 #(1 2 4 1 3 4 5) :count 1)
  #(1 2 9 1 3 4 5))

(deftest nsubstitute-test.7
  (nsubstitute 9 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))

(deftest nsubstitute-test.8
  (nsubstitute 9 4 #(1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 9 5))

(deftest nsubstitute-test.9
  (nsubstitute 9 3 '(1 2 4 1 3 4 5) :test #'>)
  (9 9 4 9 3 4 5))

(deftest nsubstitute-test.10
  (nsubstitute 9 3 #(1 2 4 1 3 4 5) :test #'>)
  #(9 9 4 9 3 4 5))

(deftest nsubstitute-if-test.1
  (nsubstitute-if 0 #'evenp '((1) (2) (3) (4)) :start 2 :key #'car)
  ((1) (2) (3) 0))

(deftest nsubstitute-if-test.2
  (nsubstitute-if 0 #'evenp #((1) (2) (3) (4)) :start 2 :key #'car)
  #((1) (2) (3) 0))

(deftest nsubstitute-if-test.3
  (nsubstitute-if 9 #'oddp '(1 2 4 1 3 4 5))
  (9 2 4 9 9 4 9))

(deftest nsubstitute-if-test.4
  (nsubstitute-if 9 #'oddp #(1 2 4 1 3 4 5))
  #(9 2 4 9 9 4 9))

(deftest nsubstitute-if-test.5
  (nsubstitute-if 9 #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))

(deftest nsubstitute-if-test.6
  (nsubstitute-if 9 #'evenp #(1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 9 5))

