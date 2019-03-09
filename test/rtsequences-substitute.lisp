;;
;;  ANSI COMMON LISP: 17. Sequences
;;
(deftest substitute.1
  (substitute #\. #\SPACE "0 2 4 6")
  "0.2.4.6")

(deftest substitute.2
  (substitute 'b 'a '(0 a 2 a 4 a 6))
  (0 b 2 b 4 b 6))

(deftest substitute.3
  (substitute 9 4 '(1 2 4 1 3 4 5))
  (1 2 9 1 3 9 5))

(deftest substitute.4
  (substitute 9 4 #(1 2 4 1 3 4 5))
  #(1 2 9 1 3 9 5))

(deftest substitute.5
  (substitute 9 4 '(1 2 4 1 3 4 5) :count 1)
  (1 2 9 1 3 4 5))

(deftest substitute.6
  (substitute 9 4 #(1 2 4 1 3 4 5) :count 1)
  #(1 2 9 1 3 4 5))

(deftest substitute.7
  (substitute 9 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))

(deftest substitute.8
  (substitute 9 4 #(1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 9 5))

(deftest substitute.9
  (substitute 9 3 '(1 2 4 1 3 4 5) :test #'>)
  (9 9 4 9 3 4 5))

(deftest substitute.10
  (substitute 9 3 #(1 2 4 1 3 4 5) :test #'>)
  #(9 9 4 9 3 4 5))

(deftest substitute-if.1
  (substitute-if 0 #'evenp '((1) (2) (3) (4)) :start 2 :key #'car)
  ((1) (2) (3) 0))

(deftest substitute-if.2
  (substitute-if 0 #'evenp #((1) (2) (3) (4)) :start 2 :key #'car)
  #((1) (2) (3) 0))

(deftest substitute-if.3
  (substitute-if 9 #'oddp '(1 2 4 1 3 4 5))
  (9 2 4 9 9 4 9))

(deftest substitute-if.4
  (substitute-if 9 #'oddp #(1 2 4 1 3 4 5))
  #(9 2 4 9 9 4 9))

(deftest substitute-if.5
  (substitute-if 9 #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))

(deftest substitute-if.6
  (substitute-if 9 #'evenp #(1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 9 5))

(deftest substitute-start.1
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 0)
  (1 99 4 99 2 4 99 99))

(deftest substitute-start.2
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 1)
  (1 99 4 99 2 4 99 99))

(deftest substitute-start.3
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 2)
  (1 3 4 99 2 4 99 99))

(deftest substitute-start.4
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 7)
  (1 3 4 3 2 4 3 99))

(deftest substitute-start.5
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 8)
  (1 3 4 3 2 4 3 3))

(deftest-error substitute-start.6
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 9))

(deftest substitute-start.7
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 0)
  #(1 99 4 99 2 4 99 99))

(deftest substitute-start.8
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 1)
  #(1 99 4 99 2 4 99 99))

(deftest substitute-start.9
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 2)
  #(1 3 4 99 2 4 99 99))

(deftest substitute-start.10
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 7)
  #(1 3 4 3 2 4 3 99))

(deftest substitute-start.11
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 8)
  #(1 3 4 3 2 4 3 3))

(deftest-error substitute-start.12
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 9))

(deftest substitute-start.1e
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 0 :from-end t)
  (1 99 4 99 2 4 99 99))

(deftest substitute-start.2e
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 1 :from-end t)
  (1 99 4 99 2 4 99 99))

(deftest substitute-start.3e
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 2 :from-end t)
  (1 3 4 99 2 4 99 99))

(deftest substitute-start.4e
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 7 :from-end t)
  (1 3 4 3 2 4 3 99))

(deftest substitute-start.5e
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 8 :from-end t)
  (1 3 4 3 2 4 3 3))

(deftest-error substitute-start.6e
  (substitute 99 3 '(1 3 4 3 2 4 3 3) :start 9 :from-end t))

(deftest substitute-start.7e
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 0 :from-end t)
  #(1 99 4 99 2 4 99 99))

(deftest substitute-start.8e
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 1 :from-end t)
  #(1 99 4 99 2 4 99 99))

(deftest substitute-start.9e
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 2 :from-end t)
  #(1 3 4 99 2 4 99 99))

(deftest substitute-start.10e
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 7 :from-end t)
  #(1 3 4 3 2 4 3 99))

(deftest substitute-start.11e
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 8 :from-end t)
  #(1 3 4 3 2 4 3 3))

(deftest-error substitute-start.12e
  (substitute 99 3 #(1 3 4 3 2 4 3 3) :start 9 :from-end t))

(deftest substitute-end.1
  (substitute 99 3 '(3 3 4 3 2 4 3 3) :end 8)
  (99 99 4 99 2 4 99 99))

(deftest substitute-end.2
  (substitute 99 3 '(3 3 4 3 2 4 3 3) :end 7)
  (99 99 4 99 2 4 99 3))

(deftest substitute-end.3
  (substitute 99 3 '(3 3 4 3 2 4 3 3) :end 6)
  (99 99 4 99 2 4 3 3))

(deftest substitute-end.4
  (substitute 99 3 '(3 3 4 3 2 4 3 3) :end 1)
  (99 3 4 3 2 4 3 3))

(deftest substitute-end.5
  (substitute 99 3 '(3 3 4 3 2 4 3 3) :end 0)
  (3 3 4 3 2 4 3 3))

(deftest-error substitute-end.6
  (substitute 99 3 '(3 3 4 3 2 4 3 3) :end 9))

(deftest substitute-end.7
  (substitute 99 3 #(3 3 4 3 2 4 3 3) :end 8)
  #(99 99 4 99 2 4 99 99))

(deftest substitute-end.8
  (substitute 99 3 #(3 3 4 3 2 4 3 3) :end 7)
  #(99 99 4 99 2 4 99 3))

(deftest substitute-end.9
  (substitute 99 3 #(3 3 4 3 2 4 3 3) :end 6)
  #(99 99 4 99 2 4 3 3))

(deftest substitute-end.10
  (substitute 99 3 #(3 3 4 3 2 4 3 3) :end 1)
  #(99 3 4 3 2 4 3 3))

(deftest substitute-end.11
  (substitute 99 3 #(3 3 4 3 2 4 3 3) :end 0)
  #(3 3 4 3 2 4 3 3))

(deftest-error substitute-end.12
  (substitute 99 3 #(3 3 4 3 2 4 3 3) :end 9))

(deftest substitute-end.1e
  (substitute 99 3 '(3 3 7 3 2 4 3 3) :end 8 :from-end t)
  (99 99 7 99 2 4 99 99))

(deftest substitute-end.2e
  (substitute 99 3 '(3 3 7 3 2 4 3 3) :end 7 :from-end t)
  (99 99 7 99 2 4 99 3))

(deftest substitute-end.3e
  (substitute 99 3 '(3 3 7 3 2 4 3 3) :end 6 :from-end t)
  (99 99 7 99 2 4 3 3))

(deftest substitute-end.4e
  (substitute 99 3 '(3 3 7 3 2 4 3 3) :end 1 :from-end t)
  (99 3 7 3 2 4 3 3))

(deftest substitute-end.5e
  (substitute 99 3 '(3 3 7 3 2 4 3 3) :end 0 :from-end t)
  (3 3 7 3 2 4 3 3))

(deftest-error substitute-end.6e
  (substitute 99 3 '(3 3 7 3 2 4 3 3) :end 9 :from-end t))

(deftest substitute-end.7e
  (substitute 99 3 #(3 3 7 3 2 4 3 3) :end 8 :from-end t)
  #(99 99 7 99 2 4 99 99))

(deftest substitute-end.8e
  (substitute 99 3 #(3 3 7 3 2 4 3 3) :end 7 :from-end t)
  #(99 99 7 99 2 4 99 3))

(deftest substitute-end.9e
  (substitute 99 3 #(3 3 7 3 2 4 3 3) :end 6 :from-end t)
  #(99 99 7 99 2 4 3 3))

(deftest substitute-end.10e
  (substitute 99 3 #(3 3 7 3 2 4 3 3) :end 1 :from-end t)
  #(99 3 7 3 2 4 3 3))

(deftest substitute-end.11e
  (substitute 99 3 #(3 3 7 3 2 4 3 3) :end 0 :from-end t)
  #(3 3 7 3 2 4 3 3))

(deftest-error substitute-end.12e
  (substitute 99 3 #(3 3 7 3 2 4 3 3) :end 9 :from-end t))

(deftest substitute-start-end.1
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 0)
  (1 2 3 3 6 7 3 3 8))

(deftest substitute-start-end.2
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 9)
  (1 2 99 99 6 7 99 99 8))

(deftest substitute-start-end.3
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 3 :end 9)
  (1 2 3 99 6 7 99 99 8))

(deftest substitute-start-end.4
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 6)
  (1 2 99 99 6 7 3 3 8))

(deftest substitute-start-end.5
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 7)
  (1 2 99 99 6 7 99 3 8))

(deftest substitute-start-end.6
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 7 :end 7)
  (1 2 3 3 6 7 3 3 8))

(deftest-error substitute-start-end.7
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 10))

(deftest substitute-start-end.8
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 0)
  #(1 2 3 3 6 7 3 3 8))

(deftest substitute-start-end.9
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 9)
  #(1 2 99 99 6 7 99 99 8))

(deftest substitute-start-end.10
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 3 :end 9)
  #(1 2 3 99 6 7 99 99 8))

(deftest substitute-start-end.11
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 6)
  #(1 2 99 99 6 7 3 3 8))

(deftest substitute-start-end.12
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 7)
  #(1 2 99 99 6 7 99 3 8))

(deftest substitute-start-end.13
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 7 :end 7)
  #(1 2 3 3 6 7 3 3 8))

(deftest-error substitute-start-end.14
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 10))

(deftest substitute-start-end.1e
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 0 :from-end t)
  (1 2 3 3 6 7 3 3 8))

(deftest substitute-start-end.2e
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 9 :from-end t)
  (1 2 99 99 6 7 99 99 8))

(deftest substitute-start-end.3e
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 3 :end 9 :from-end t)
  (1 2 3 99 6 7 99 99 8))

(deftest substitute-start-end.4e
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 6 :from-end t)
  (1 2 99 99 6 7 3 3 8))

(deftest substitute-start-end.5e
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 7 :from-end t)
  (1 2 99 99 6 7 99 3 8))

(deftest substitute-start-end.6e
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 7 :end 7 :from-end t)
  (1 2 3 3 6 7 3 3 8))

(deftest-error substitute-start-end.7e
  (substitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 10 :from-end t))

(deftest substitute-start-end.8e
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 0 :from-end t)
  #(1 2 3 3 6 7 3 3 8))

(deftest substitute-start-end.9e
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 9 :from-end t)
  #(1 2 99 99 6 7 99 99 8))

(deftest substitute-start-end.10e
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 3 :end 9 :from-end t)
  #(1 2 3 99 6 7 99 99 8))

(deftest substitute-start-end.11e
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 6 :from-end t)
  #(1 2 99 99 6 7 3 3 8))

(deftest substitute-start-end.12e
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 7 :from-end t)
  #(1 2 99 99 6 7 99 3 8))

(deftest substitute-start-end.13e
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 7 :end 7 :from-end t)
  #(1 2 3 3 6 7 3 3 8))

(deftest-error substitute-start-end.14e
  (substitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 10 :from-end t))

(deftest substitute-count.1
  (substitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 0)
  (1 3 3 3 5 6 7 3 3 3 8 9))

(deftest substitute-count.2
  (substitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 1)
  (1 99 3 3 5 6 7 3 3 3 8 9))

(deftest substitute-count.3
  (substitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 4)
  (1 99 99 99 5 6 7 99 3 3 8 9))

(deftest substitute-count.4
  (substitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 0 :from-end t)
  (1 3 3 3 5 6 7 3 3 3 8 9))

(deftest substitute-count.5
  (substitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 1 :from-end t)
  (1 3 3 3 5 6 7 3 3 99 8 9))

(deftest substitute-count.6
  (substitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 4 :from-end t)
  (1 3 3 99 5 6 7 99 99 99 8 9))

(deftest nsubstitute.1
  (nsubstitute #\. #\SPACE "0 2 4 6")
  "0.2.4.6")

(deftest nsubstitute.2
  (nsubstitute 'b 'a '(0 a 2 a 4 a 6))
  (0 b 2 b 4 b 6))

(deftest nsubstitute.3
  (nsubstitute 9 4 '(1 2 4 1 3 4 5))
  (1 2 9 1 3 9 5))

(deftest nsubstitute.4
  (nsubstitute 9 4 #(1 2 4 1 3 4 5))
  #(1 2 9 1 3 9 5))

(deftest nsubstitute.5
  (nsubstitute 9 4 '(1 2 4 1 3 4 5) :count 1)
  (1 2 9 1 3 4 5))

(deftest nsubstitute.6
  (nsubstitute 9 4 #(1 2 4 1 3 4 5) :count 1)
  #(1 2 9 1 3 4 5))

(deftest nsubstitute.7
  (nsubstitute 9 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))

(deftest nsubstitute.8
  (nsubstitute 9 4 #(1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 9 5))

(deftest nsubstitute.9
  (nsubstitute 9 3 '(1 2 4 1 3 4 5) :test #'>)
  (9 9 4 9 3 4 5))

(deftest nsubstitute.10
  (nsubstitute 9 3 #(1 2 4 1 3 4 5) :test #'>)
  #(9 9 4 9 3 4 5))

(deftest nsubstitute-if.1
  (nsubstitute-if 0 #'evenp '((1) (2) (3) (4)) :start 2 :key #'car)
  ((1) (2) (3) 0))

(deftest nsubstitute-if.2
  (nsubstitute-if 0 #'evenp #((1) (2) (3) (4)) :start 2 :key #'car)
  #((1) (2) (3) 0))

(deftest nsubstitute-if.3
  (nsubstitute-if 9 #'oddp '(1 2 4 1 3 4 5))
  (9 2 4 9 9 4 9))

(deftest nsubstitute-if.4
  (nsubstitute-if 9 #'oddp #(1 2 4 1 3 4 5))
  #(9 2 4 9 9 4 9))

(deftest nsubstitute-if.5
  (nsubstitute-if 9 #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))

(deftest nsubstitute-if.6
  (nsubstitute-if 9 #'evenp #(1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 9 5))

(deftest nsubstitute-start.1
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 0)
  (1 99 4 99 2 4 99 99))

(deftest nsubstitute-start.2
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 1)
  (1 99 4 99 2 4 99 99))

(deftest nsubstitute-start.3
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 2)
  (1 3 4 99 2 4 99 99))

(deftest nsubstitute-start.4
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 7)
  (1 3 4 3 2 4 3 99))

(deftest nsubstitute-start.5
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 8)
  (1 3 4 3 2 4 3 3))

(deftest-error nsubstitute-start.6
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 9))

(deftest nsubstitute-start.7
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 0)
  #(1 99 4 99 2 4 99 99))

(deftest nsubstitute-start.8
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 1)
  #(1 99 4 99 2 4 99 99))

(deftest nsubstitute-start.9
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 2)
  #(1 3 4 99 2 4 99 99))

(deftest nsubstitute-start.10
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 7)
  #(1 3 4 3 2 4 3 99))

(deftest nsubstitute-start.11
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 8)
  #(1 3 4 3 2 4 3 3))

(deftest-error nsubstitute-start.12
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 9))

(deftest nsubstitute-start.1e
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 0 :from-end t)
  (1 99 4 99 2 4 99 99))

(deftest nsubstitute-start.2e
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 1 :from-end t)
  (1 99 4 99 2 4 99 99))

(deftest nsubstitute-start.3e
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 2 :from-end t)
  (1 3 4 99 2 4 99 99))

(deftest nsubstitute-start.4e
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 7 :from-end t)
  (1 3 4 3 2 4 3 99))

(deftest nsubstitute-start.5e
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 8 :from-end t)
  (1 3 4 3 2 4 3 3))

(deftest-error nsubstitute-start.6e
  (nsubstitute 99 3 '(1 3 4 3 2 4 3 3) :start 9 :from-end t))

(deftest nsubstitute-start.7e
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 0 :from-end t)
  #(1 99 4 99 2 4 99 99))

(deftest nsubstitute-start.8e
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 1 :from-end t)
  #(1 99 4 99 2 4 99 99))

(deftest nsubstitute-start.9e
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 2 :from-end t)
  #(1 3 4 99 2 4 99 99))

(deftest nsubstitute-start.10e
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 7 :from-end t)
  #(1 3 4 3 2 4 3 99))

(deftest nsubstitute-start.11e
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 8 :from-end t)
  #(1 3 4 3 2 4 3 3))

(deftest-error nsubstitute-start.12e
  (nsubstitute 99 3 #(1 3 4 3 2 4 3 3) :start 9 :from-end t))

(deftest nsubstitute-end.1
  (nsubstitute 99 3 '(3 3 4 3 2 4 3 3) :end 8)
  (99 99 4 99 2 4 99 99))

(deftest nsubstitute-end.2
  (nsubstitute 99 3 '(3 3 4 3 2 4 3 3) :end 7)
  (99 99 4 99 2 4 99 3))

(deftest nsubstitute-end.3
  (nsubstitute 99 3 '(3 3 4 3 2 4 3 3) :end 6)
  (99 99 4 99 2 4 3 3))

(deftest nsubstitute-end.4
  (nsubstitute 99 3 '(3 3 4 3 2 4 3 3) :end 1)
  (99 3 4 3 2 4 3 3))

(deftest nsubstitute-end.5
  (nsubstitute 99 3 '(3 3 4 3 2 4 3 3) :end 0)
  (3 3 4 3 2 4 3 3))

(deftest-error nsubstitute-end.6
  (nsubstitute 99 3 '(3 3 4 3 2 4 3 3) :end 9))

(deftest nsubstitute-end.7
  (nsubstitute 99 3 #(3 3 4 3 2 4 3 3) :end 8)
  #(99 99 4 99 2 4 99 99))

(deftest nsubstitute-end.8
  (nsubstitute 99 3 #(3 3 4 3 2 4 3 3) :end 7)
  #(99 99 4 99 2 4 99 3))

(deftest nsubstitute-end.9
  (nsubstitute 99 3 #(3 3 4 3 2 4 3 3) :end 6)
  #(99 99 4 99 2 4 3 3))

(deftest nsubstitute-end.10
  (nsubstitute 99 3 #(3 3 4 3 2 4 3 3) :end 1)
  #(99 3 4 3 2 4 3 3))

(deftest nsubstitute-end.11
  (nsubstitute 99 3 #(3 3 4 3 2 4 3 3) :end 0)
  #(3 3 4 3 2 4 3 3))

(deftest-error nsubstitute-end.12
  (nsubstitute 99 3 #(3 3 4 3 2 4 3 3) :end 9))

(deftest nsubstitute-end.1e
  (nsubstitute 99 3 '(3 3 7 3 2 4 3 3) :end 8 :from-end t)
  (99 99 7 99 2 4 99 99))

(deftest nsubstitute-end.2e
  (nsubstitute 99 3 '(3 3 7 3 2 4 3 3) :end 7 :from-end t)
  (99 99 7 99 2 4 99 3))

(deftest nsubstitute-end.3e
  (nsubstitute 99 3 '(3 3 7 3 2 4 3 3) :end 6 :from-end t)
  (99 99 7 99 2 4 3 3))

(deftest nsubstitute-end.4e
  (nsubstitute 99 3 '(3 3 7 3 2 4 3 3) :end 1 :from-end t)
  (99 3 7 3 2 4 3 3))

(deftest nsubstitute-end.5e
  (nsubstitute 99 3 '(3 3 7 3 2 4 3 3) :end 0 :from-end t)
  (3 3 7 3 2 4 3 3))

(deftest-error nsubstitute-end.6e
  (nsubstitute 99 3 '(3 3 7 3 2 4 3 3) :end 9 :from-end t))

(deftest nsubstitute-end.7e
  (nsubstitute 99 3 #(3 3 7 3 2 4 3 3) :end 8 :from-end t)
  #(99 99 7 99 2 4 99 99))

(deftest nsubstitute-end.8e
  (nsubstitute 99 3 #(3 3 7 3 2 4 3 3) :end 7 :from-end t)
  #(99 99 7 99 2 4 99 3))

(deftest nsubstitute-end.9e
  (nsubstitute 99 3 #(3 3 7 3 2 4 3 3) :end 6 :from-end t)
  #(99 99 7 99 2 4 3 3))

(deftest nsubstitute-end.10e
  (nsubstitute 99 3 #(3 3 7 3 2 4 3 3) :end 1 :from-end t)
  #(99 3 7 3 2 4 3 3))

(deftest nsubstitute-end.11e
  (nsubstitute 99 3 #(3 3 7 3 2 4 3 3) :end 0 :from-end t)
  #(3 3 7 3 2 4 3 3))

(deftest-error nsubstitute-end.12e
  (nsubstitute 99 3 #(3 3 7 3 2 4 3 3) :end 9 :from-end t))

(deftest nsubstitute-start-end.1
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 0)
  (1 2 3 3 6 7 3 3 8))

(deftest nsubstitute-start-end.2
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 9)
  (1 2 99 99 6 7 99 99 8))

(deftest nsubstitute-start-end.3
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 3 :end 9)
  (1 2 3 99 6 7 99 99 8))

(deftest nsubstitute-start-end.4
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 6)
  (1 2 99 99 6 7 3 3 8))

(deftest nsubstitute-start-end.5
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 7)
  (1 2 99 99 6 7 99 3 8))

(deftest nsubstitute-start-end.6
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 7 :end 7)
  (1 2 3 3 6 7 3 3 8))

(deftest-error nsubstitute-start-end.7
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 10))

(deftest nsubstitute-start-end.8
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 0)
  #(1 2 3 3 6 7 3 3 8))

(deftest nsubstitute-start-end.9
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 9)
  #(1 2 99 99 6 7 99 99 8))

(deftest nsubstitute-start-end.10
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 3 :end 9)
  #(1 2 3 99 6 7 99 99 8))

(deftest nsubstitute-start-end.11
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 6)
  #(1 2 99 99 6 7 3 3 8))

(deftest nsubstitute-start-end.12
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 7)
  #(1 2 99 99 6 7 99 3 8))

(deftest nsubstitute-start-end.13
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 7 :end 7)
  #(1 2 3 3 6 7 3 3 8))

(deftest-error nsubstitute-start-end.14
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 10))

(deftest nsubstitute-start-end.1e
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 0 :from-end t)
  (1 2 3 3 6 7 3 3 8))

(deftest nsubstitute-start-end.2e
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 9 :from-end t)
  (1 2 99 99 6 7 99 99 8))

(deftest nsubstitute-start-end.3e
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 3 :end 9 :from-end t)
  (1 2 3 99 6 7 99 99 8))

(deftest nsubstitute-start-end.4e
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 6 :from-end t)
  (1 2 99 99 6 7 3 3 8))

(deftest nsubstitute-start-end.5e
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 7 :from-end t)
  (1 2 99 99 6 7 99 3 8))

(deftest nsubstitute-start-end.6e
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 7 :end 7 :from-end t)
  (1 2 3 3 6 7 3 3 8))

(deftest-error nsubstitute-start-end.7e
  (nsubstitute 99 3 '(1 2 3 3 6 7 3 3 8) :start 0 :end 10 :from-end t))

(deftest nsubstitute-start-end.8e
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 0 :from-end t)
  #(1 2 3 3 6 7 3 3 8))

(deftest nsubstitute-start-end.9e
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 9 :from-end t)
  #(1 2 99 99 6 7 99 99 8))

(deftest nsubstitute-start-end.10e
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 3 :end 9 :from-end t)
  #(1 2 3 99 6 7 99 99 8))

(deftest nsubstitute-start-end.11e
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 6 :from-end t)
  #(1 2 99 99 6 7 3 3 8))

(deftest nsubstitute-start-end.12e
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 7 :from-end t)
  #(1 2 99 99 6 7 99 3 8))

(deftest nsubstitute-start-end.13e
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 7 :end 7 :from-end t)
  #(1 2 3 3 6 7 3 3 8))

(deftest-error nsubstitute-start-end.14e
  (nsubstitute 99 3 #(1 2 3 3 6 7 3 3 8) :start 0 :end 10 :from-end t))

(deftest nsubstitute-count.1
  (nsubstitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 0)
  (1 3 3 3 5 6 7 3 3 3 8 9))

(deftest nsubstitute-count.2
  (nsubstitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 1)
  (1 99 3 3 5 6 7 3 3 3 8 9))

(deftest nsubstitute-count.3
  (nsubstitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 4)
  (1 99 99 99 5 6 7 99 3 3 8 9))

(deftest nsubstitute-count.4
  (nsubstitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 0 :from-end t)
  (1 3 3 3 5 6 7 3 3 3 8 9))

(deftest nsubstitute-count.5
  (nsubstitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 1 :from-end t)
  (1 3 3 3 5 6 7 3 3 99 8 9))

(deftest nsubstitute-count.6
  (nsubstitute 99 3 '(1 3 3 3 5 6 7 3 3 3 8 9) :count 4 :from-end t)
  (1 3 3 99 5 6 7 99 99 99 8 9))

