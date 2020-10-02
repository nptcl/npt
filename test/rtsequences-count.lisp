;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function COUNT
;;
(deftest count.1
  (count #\a "aabbccaabbcca")
  5)

(deftest count.2
  (count 'a '(b c d e a b c d a b c d))
  2)

(deftest count.3
  (count 3 #(1 2 3 4 5 a b c d e 1 2 3 4 5 6 3 3 3))
  5)

(deftest count.4
  (count 10 '(8 8 9 9 9 10 10 10 10 11) :key #'1+)
  3)

(deftest count.5
  (count 3 '(2 3 4 5 6 7 8 9 10 31 32 33)
         :test (lambda (x y)
                 (zerop (mod (max x y) (min x y)))))
  4)

(deftest count.6
  (count 3 '(2 3 4 5 6 7 8 9 10 31 32 33)
         :test-not (lambda (x y)
                     (not (zerop (mod (max x y) (min x y))))))
  4)

(deftest count.7
  (count #\a "aabbccaabbcca" :from-end t)
  5)

(deftest count.8
  (let (list)
    (count 3 '((1 . 2) (3 . 4) (5 . 6) (3 . 7))
           :test (lambda (x y)
                   (let ((a (if (consp x) x y))
                         (b (if (consp x) y x)))
                     (when (eql (car a) b)
                       (push (cdr a) list)
                       t))))
    (nreverse list))
  (4 7))

(deftest count.9
  (let (list)
    (count 3 '((1 . 2) (3 . 4) (5 . 6) (3 . 7))
           :test (lambda (x y)
                   (let ((a (if (consp x) x y))
                         (b (if (consp x) y x)))
                     (when (eql (car a) b)
                       (push (cdr a) list)
                       t)))
           :from-end t)
    (nreverse list))
  (7 4))

(deftest count-range-list.1
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 3)
  5)

(deftest count-range-list.2
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8)
  4)

(deftest count-range-list.3
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :end nil)
  4)

(deftest count-range-list.4
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :end 15)
  4)

(deftest count-range-list.5
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :end 14)
  3)

(deftest count-range-list.6
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :end 14)
  5)

(deftest-error count-range-list.7
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 16))

(deftest-error count-range-list.8
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :end 16))

(deftest count-range-list.9
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 3 :from-end t)
  5)

(deftest count-range-list.10
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :from-end t)
  4)

(deftest count-range-list.11
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :end nil :from-end t)
  4)

(deftest count-range-list.12
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :end 15 :from-end t)
  4)

(deftest count-range-list.13
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :end 14 :from-end t)
  3)

(deftest count-range-list.14
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :end 14 :from-end t)
  5)

(deftest-error count-range-list.15
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 16 :from-end t))

(deftest-error count-range-list.16
  (count 3 '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :end 16 :from-end t))

(deftest count-range-vector.1
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 3)
  5)

(deftest count-range-vector.2
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8)
  4)

(deftest count-range-vector.3
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :end nil)
  4)

(deftest count-range-vector.4
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :end 15)
  4)

(deftest count-range-vector.5
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :end 14)
  3)

(deftest count-range-vector.6
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :end 14)
  5)

(deftest-error count-range-vector.7
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 16))

(deftest-error count-range-vector.8
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :end 16))

(deftest count-range-vector.9
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 3 :from-end t)
  5)

(deftest count-range-vector.10
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :from-end t)
  4)

(deftest count-range-vector.11
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :end nil :from-end t)
  4)

(deftest count-range-vector.12
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :end 15 :from-end t)
  4)

(deftest count-range-vector.13
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 8 :end 14 :from-end t)
  3)

(deftest count-range-vector.14
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :end 14 :from-end t)
  5)

(deftest-error count-range-vector.15
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :start 16 :from-end t))

(deftest-error count-range-vector.16
  (count 3 #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3) :end 16 :from-end t))

(deftest-error count-error.1
  (eval '(count 1 20)))

(deftest-error! count-error.2
  (eval '(count 1)))

(deftest-error count-error.3
  (eval '(count 1 nil nil)))

(deftest-error count-error.4
  (eval '(count 1 nil :key)))

(deftest-error count-error.5
  (eval '(count 1 nil :key 10)))

(deftest-error count-error.6
  (eval '(count 1 nil :hello 10)))

(deftest-error count-error.7
  (eval '(count 1 nil :test (constantly t) :test-not (constantly t))))


;;
;;  Function COUNT-IF
;;
(deftest count-if.1
  (count-if #'evenp '(1 2 3 3 3 3 3 3 3 3 3 5 5 5 10 10 1))
  3)

(deftest count-if.2
  (count-if (lambda (x) (eql x #\a)) "aabbccaabbcca")
  5)

(deftest count-if.3
  (count-if (lambda (x) (eq x 'a)) '(b c d e a b c d a b c d))
  2)

(deftest count-if.4
  (count-if (lambda (x) (equal x 3)) #(1 2 3 4 5 a b c d e 1 2 3 4 5 6 3 3 3))
  5)

(deftest count-if.5
  (count-if (lambda (x) (= x 10)) '(8 8 9 9 9 10 10 10 10 11) :key #'1+)
  3)

(deftest count-if.6
  (count-if (lambda (y)
              (let ((x 3))
                (zerop (mod (max x y) (min x y)))))
            '(2 3 4 5 6 7 8 9 10 31 32 33))
  4)

(deftest count-if.7
  (count-if (lambda (x) (char= x #\a)) "aabbccaabbcca" :from-end t)
  5)

(deftest count-if.8
  (let (list)
    (count-if
      (lambda (y)
        (let* ((x 3)
               (a (if (consp x) x y))
               (b (if (consp x) y x)))
          (when (eql (car a) b)
            (push (cdr a) list)
            t)))
      '((1 . 2) (3 . 4) (5 . 6) (3 . 7)))
    (nreverse list))
  (4 7))

(deftest count-if.9
  (let (list)
    (count-if
      (lambda (y)
        (let* ((x 3)
               (a (if (consp x) x y))
               (b (if (consp x) y x)))
          (when (eql (car a) b)
            (push (cdr a) list)
            t)))
      '((1 . 2) (3 . 4) (5 . 6) (3 . 7))
      :from-end t)
    (nreverse list))
  (7 4))

(deftest count-if-range-list.1
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 3)
  5)

(deftest count-if-range-list.2
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8)
  4)

(deftest count-if-range-list.3
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :end nil)
  4)

(deftest count-if-range-list.4
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :end 15)
  4)

(deftest count-if-range-list.5
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :end 14)
  3)

(deftest count-if-range-list.6
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :end 14)
  5)

(deftest-error count-if-range-list.7
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 16))

(deftest-error count-if-range-list.8
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :end 16))

(deftest count-if-range-list.9
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 3 :from-end t)
  5)

(deftest count-if-range-list.10
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :from-end t)
  4)

(deftest count-if-range-list.11
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :end nil :from-end t)
  4)

(deftest count-if-range-list.12
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :end 15 :from-end t)
  4)

(deftest count-if-range-list.13
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :end 14 :from-end t)
  3)

(deftest count-if-range-list.14
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :end 14 :from-end t)
  5)

(deftest-error count-if-range-list.15
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 16 :from-end t))

(deftest-error count-if-range-list.16
  (count-if (lambda (x) (eql x 3)) '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :end 16 :from-end t))

(deftest count-if-range-vector.1
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 3)
  5)

(deftest count-if-range-vector.2
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8)
  4)

(deftest count-if-range-vector.3
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :end nil)
  4)

(deftest count-if-range-vector.4
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :end 15)
  4)

(deftest count-if-range-vector.5
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :end 14)
  3)

(deftest count-if-range-vector.6
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :end 14)
  5)

(deftest-error count-if-range-vector.7
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 16))

(deftest-error count-if-range-vector.8
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :end 16))

(deftest count-if-range-vector.9
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 3 :from-end t)
  5)

(deftest count-if-range-vector.10
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :from-end t)
  4)

(deftest count-if-range-vector.11
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :end nil :from-end t)
  4)

(deftest count-if-range-vector.12
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :end 15 :from-end t)
  4)

(deftest count-if-range-vector.13
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 8 :end 14 :from-end t)
  3)

(deftest count-if-range-vector.14
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :end 14 :from-end t)
  5)

(deftest-error count-if-range-vector.15
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :start 16 :from-end t))

(deftest-error count-if-range-vector.16
  (count-if (lambda (x) (eql x 3)) #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
            :end 16 :from-end t))

(deftest-error count-if-error.1
  (eval '(count-if 1 nil)))

(deftest-error! count-if-error.2
  (eval '(count-if (constantly t) 20)))

(deftest-error count-if-error.3
  (eval '(count-if (constantly t) nil nil)))

(deftest-error count-if-error.4
  (eval '(count-if (constantly t) nil :key)))

(deftest-error count-if-error.5
  (eval '(count-if (constantly t) nil :key 10)))

(deftest-error count-if-error.6
  (eval '(count-if (constantly t) nil :hello 10)))


;;
;;  Function COUNT-IF-NOT
;;
(deftest count-if-not.1
  (count-if-not #'evenp '(1 2 3 3 3 3 3 3 3 3 3 5 5 5 10 10 1))
  14)

(deftest count-if-not.2
  (count-if-not (lambda (x) (not (eql x #\a))) "aabbccaabbcca")
  5)

(deftest count-if-not.3
  (count-if-not (lambda (x) (not (eq x 'a))) '(b c d e a b c d a b c d))
  2)

(deftest count-if-not.4
  (count-if-not (lambda (x) (not (equal x 3)))
                #(1 2 3 4 5 a b c d e 1 2 3 4 5 6 3 3 3))
  5)

(deftest count-if-not.5
  (count-if-not (lambda (x) (not (= x 10)))
                '(8 8 9 9 9 10 10 10 10 11) :key #'1+)
  3)

(deftest count-if-not.6
  (count-if-not (lambda (y)
                  (let ((x 3))
                    (not (zerop (mod (max x y) (min x y))))))
                '(2 3 4 5 6 7 8 9 10 31 32 33))
  4)

(deftest count-if-not.7
  (count-if-not (lambda (x) (not (char= x #\a)))
                "aabbccaabbcca" :from-end t)
  5)

(deftest count-if-not.8
  (let (list)
    (count-if-not
      (lambda (y)
        (let* ((x 3)
               (a (if (consp x) x y))
               (b (if (consp x) y x)))
          (when (eql (car a) b)
            (push (cdr a) list)
            t)))
      '((1 . 2) (3 . 4) (5 . 6) (3 . 7)))
    (nreverse list))
  (4 7))

(deftest count-if-not.9
  (let (list)
    (count-if-not
      (lambda (y)
        (let* ((x 3)
               (a (if (consp x) x y))
               (b (if (consp x) y x)))
          (when (eql (car a) b)
            (push (cdr a) list)
            t)))
      '((1 . 2) (3 . 4) (5 . 6) (3 . 7))
      :from-end t)
    (nreverse list))
  (7 4))

(deftest count-if-not-range-list.1
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 3)
  5)

(deftest count-if-not-range-list.2
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8)
  4)

(deftest count-if-not-range-list.3
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :end nil)
  4)

(deftest count-if-not-range-list.4
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :end 15)
  4)

(deftest count-if-not-range-list.5
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :end 14)
  3)

(deftest count-if-not-range-list.6
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :end 14)
  5)

(deftest-error count-if-not-range-list.7
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 16))

(deftest-error count-if-not-range-list.8
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :end 16))

(deftest count-if-not-range-list.9
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 3 :from-end t)
  5)

(deftest count-if-not-range-list.10
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :from-end t)
  4)

(deftest count-if-not-range-list.11
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :end nil :from-end t)
  4)

(deftest count-if-not-range-list.12
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :end 15 :from-end t)
  4)

(deftest count-if-not-range-list.13
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :end 14 :from-end t)
  3)

(deftest count-if-not-range-list.14
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :end 14 :from-end t)
  5)

(deftest-error count-if-not-range-list.15
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 16 :from-end t))

(deftest-error count-if-not-range-list.16
  (count-if-not (lambda (x) (not (eql x 3)))
                '(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :end 16 :from-end t))

(deftest count-if-not-range-vector.1
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 3)
  5)

(deftest count-if-not-range-vector.2
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8)
  4)

(deftest count-if-not-range-vector.3
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :end nil)
  4)

(deftest count-if-not-range-vector.4
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :end 15)
  4)

(deftest count-if-not-range-vector.5
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :end 14)
  3)

(deftest count-if-not-range-vector.6
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :end 14)
  5)

(deftest-error count-if-not-range-vector.7
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 16))

(deftest-error count-if-not-range-vector.8
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :end 16))

(deftest count-if-not-range-vector.9
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 3 :from-end t)
  5)

(deftest count-if-not-range-vector.10
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :from-end t)
  4)

(deftest count-if-not-range-vector.11
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :end nil :from-end t)
  4)

(deftest count-if-not-range-vector.12
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :end 15 :from-end t)
  4)

(deftest count-if-not-range-vector.13
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 8 :end 14 :from-end t)
  3)

(deftest count-if-not-range-vector.14
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :end 14 :from-end t)
  5)

(deftest-error count-if-not-range-vector.15
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :start 16 :from-end t))

(deftest-error count-if-not-range-vector.16
  (count-if-not (lambda (x) (not (eql x 3)))
                #(1 2 3 4 5 1 2 3 3 3 4 5 6 3 3)
                :end 16 :from-end t))

(deftest-error count-if-not-error.1
  (eval '(count-if-not 1 nil)))

(deftest-error! count-if-not-error.2
  (eval '(count-if-not (constantly t) 20)))

(deftest-error count-if-not-error.3
  (eval '(count-if-not (constantly t) nil nil)))

(deftest-error count-if-not-error.4
  (eval '(count-if-not (constantly t) nil :key)))

(deftest-error count-if-not-error.5
  (eval '(count-if-not (constantly t) nil :key 10)))

(deftest-error count-if-not-error.6
  (eval '(count-if-not (constantly t) nil :hello 10)))

;; ANSI Common Lisp
(deftest count-test.1
  (count #\a "how many A's are there in here?")
  2)

(deftest count-test.2
  (count-if-not #'oddp '((1) (2) (3) (4)) :key #'car)
  2)

(deftest count-test.3
  (count-if #'upper-case-p "The Crying of Lot 49" :start 4)
  2)

