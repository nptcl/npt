;;
;;  ANSI COMMON LISP: 6. Iteration
;;

;;
;;  initially
;;
(deftest loop-initially.1
  (let (list)
    (values
      (loop
        initially (push 10 list)
        do (push 20 list)
        (return 30))
      (nreverse list)))
  30 (10 20))

(deftest loop-initially.2
  (let (list)
    (values
      (loop
        do (push 20 list)
        (return 30)
        initially (push 10 list))
      (nreverse list)))
  30 (10 20))


(deftest loop-initially.3
  (let (list)
    (values
      (loop
        initially (push 10 list)
        initially (push 20 list)
        do (push 30 list)
        (return 40))
      (nreverse list)))
  40 (10 20 30))

(deftest loop-initially.4
  (let (list)
    (values
      (loop
        initially (push 10 list)
        initially (push 20 list)
        do (push 30 list)
        (return 40)
        initially (push 50 list)
        initially (push 60 list))
      (nreverse list)))
  40 (10 20 50 60 30))

(deftest loop-initially.5
  (let (list)
    (values
      (loop
        initially
        (push 10 list)
        (push 20 list)
        initially
        (push 30 list)
        (push 40 list)
        do (push 50 list)
        (return 60)
        initially
        (push 70 list)
        (push 80 list)
        initially
        (push 90 list)
        (push 100 list))
      (nreverse list)))
  60 (10 20 30 40 70 80 90 100 50))


;;
;;  finally
;;
(deftest loop-finally.1
  (let (list)
    (values
      (loop
        finally (push 10 list)
        do (push 20 list)
        (return 30))
      (nreverse list)))
  30 (20))

(deftest loop-finally.2
  (let (list)
    (values
      (loop
        finally (push 10 list)
        do (push 20 list)
        (loop-finish))
      (nreverse list)))
  nil (20 10))

(deftest loop-finally.3
  (let (list)
    (values
      (loop
        do (push 20 list)
        (return 30)
        finally (push 10 list))
      (nreverse list)))
  30 (20))

(deftest loop-finally.4
  (let (list)
    (values
      (loop
        do (push 20 list)
        (loop-finish)
        finally (push 10 list))
      (nreverse list)))
  nil (20 10))

(deftest loop-finally.5
  (let (list)
    (values
      (loop
        finally (push 10 list)
        finally (push 20 list)
        do (push 30 list)
        (return 40))
      (nreverse list)))
  40 (30))

(deftest loop-finally.6
  (let (list)
    (values
      (loop
        finally (push 10 list)
        finally (push 20 list)
        do (push 30 list)
        (loop-finish))
      (nreverse list)))
  nil (30 10 20))

(deftest loop-finally.7
  (let (list)
    (values
      (loop
        finally (push 10 list)
        finally (push 20 list)
        do (push 30 list)
        (return 40)
        finally (push 50 list)
        finally (push 60 list))
      (nreverse list)))
  40 (30))

(deftest loop-finally.8
  (let (list)
    (values
      (loop
        finally (push 10 list)
        finally (push 20 list)
        do (push 30 list)
        (loop-finish)
        finally (push 50 list)
        finally (push 60 list))
      (nreverse list)))
  nil (30 10 20 50 60))

(deftest loop-finally.9
  (let (list)
    (values
      (loop
        finally
        (push 10 list)
        (push 20 list)
        finally
        (push 30 list)
        (push 40 list)
        do (push 50 list)
        (return 60)
        finally
        (push 70 list)
        (push 80 list)
        finally
        (push 90 list)
        (push 100 list))
      (nreverse list)))
  60 (50))

(deftest loop-finally.10
  (let (list)
    (values
      (loop
        finally
        (push 10 list)
        (push 20 list)
        finally
        (push 30 list)
        (push 40 list)
        do (push 50 list)
        (loop-finish)
        finally
        (push 70 list)
        (push 80 list)
        finally
        (push 90 list)
        (push 100 list))
      (nreverse list)))
  nil (50 10 20 30 40 70 80 90 100))


;;
;;  with
;;
(deftest loop-with.1
  (loop with a
        do (return a))
  nil)

(deftest loop-with.2
  (loop with a = 10
        do (return a))
  10)

(deftest loop-with.3
  (loop with a integer = 10
        do (return a))
  10)

(deftest loop-with.4
  (loop with a integer
        do (return a))
  0)

(deftest loop-with.5
  (loop with a float
        do (return a))
  0.0f0)

(deftest loop-with.6
  (loop with a = 10
        with b = (+ a 9000)
        do (return (values a b)))
  10 9010)

(deftest loop-with.7
  (loop with (a b) = '(10 20)
        do (return (values a b)))
  10 20)

(deftest loop-with.8
  (loop with (a b) = '(10 20)
        with (c d) = (list (+ a b) (* a b))
        do (return (values a b c d)))
  10 20 30 200)

(deftest loop-with.9
  (let ((a 10))
    (loop with a = 20 and b = a
          do (return (values a b))))
  20 10)

(deftest loop-with.10
  (let ((a 10) (b 20))
    (loop with (a b) = (list 30 40)
          and (c d) = (list (+ a b) (* a b))
          do (return (values a b c d))))
  30 40 30 200)

(deftest loop-with.11
  (let ((a 10) (b 20))
    (loop with (a b) = (list 30 40)
          and (c d) = (list (+ a b) (* a b))
          with (e f) = (list 100 200)
          and g = a
          do (return (values a b c d e f g))))
  30 40 30 200 100 200 30)

(deftest loop-with.12
  (loop with (a b) integer
        and (c d) integer
        with (e f) float
        and g float = 1.0f0
        do (return (values a b c d e f g)))
  0 0 0 0 0.0f0 0.0f0 1.0f0)


;;
;;  for-as-arithmetic-up
;;
(deftest loop-for-as-up.1
  (loop with list
        for x from 5 to 10
        do (push x list)
        finally (return (nreverse list)))
  (5 6 7 8 9 10))

(deftest loop-for-as-up.2
  (loop with list
        for x from 5 below 10
        do (push x list)
        finally (return (nreverse list)))
  (5 6 7 8 9))

(deftest loop-for-as-up.3
  (loop with list
        for x from 5 to 3
        do (push x list)
        finally (return (nreverse list)))
  nil)

(deftest loop-for-as-up.4
  (loop with list
        for x from 5 below 3
        do (push x list)
        finally (return (nreverse list)))
  nil)

(deftest loop-for-as-up.5
  (loop with list
        for x upfrom 5 to 10
        do (push x list)
        finally (return (nreverse list)))
  (5 6 7 8 9 10))

(deftest loop-for-as-up.6
  (loop with list
        for x from 5 upto 10
        do (push x list)
        finally (return (nreverse list)))
  (5 6 7 8 9 10))

(deftest loop-for-as-up.7
  (loop with list
        for x from 5 to 10 by 2
        do (push x list)
        finally (return (nreverse list)))
  (5 7 9))

(deftest loop-for-as-up.8
  (loop with list
        for x from 5 below 10 by 2
        do (push x list)
        finally (return (nreverse list)))
  (5 7 9))

(deftest loop-for-as-up.9
  (loop with list
        for x from 5 to 9 by 2
        do (push x list)
        finally (return (nreverse list)))
  (5 7 9))

(deftest loop-for-as-up.10
  (loop with list
        for x from 5 below 9 by 2
        do (push x list)
        finally (return (nreverse list)))
  (5 7))

(deftest loop-for-as-up.11
  (loop with list
        for x to 5
        do (push x list)
        finally (return (nreverse list)))
  (0 1 2 3 4 5))


(deftest loop-for-as-up.12
  (loop with list
        for x by 2
        do (push x list)
        (if (<= 10 x)
          (return (nreverse list))))
  (0 2 4 6 8 10))


;;
;;  for-as-arithmetic-downto
;;
(deftest loop-for-as-downto.1
  (loop with list
        for x from 10 downto 5
        do (push x list)
        finally (return (nreverse list)))
  (10 9 8 7 6 5))

(deftest loop-for-as-downto.2
  (loop with list
        for x from 10 above 5
        do (push x list)
        finally (return (nreverse list)))
  (10 9 8 7 6))

(deftest loop-for-as-downto.3
  (loop with list
        for x from 10 downto 5 by 2
        do (push x list)
        finally (return (nreverse list)))
  (10 8 6))

(deftest loop-for-as-downto.4
  (loop with list
        for x from 10 above 5 by 2
        do (push x list)
        finally (return (nreverse list)))
  (10 8 6))

(deftest loop-for-as-downto.5
  (loop with list
        for x from 10 downto 6 by 2
        do (push x list)
        finally (return (nreverse list)))
  (10 8 6))

(deftest loop-for-as-downto.6
  (loop with list
        for x from 10 above 6 by 2
        do (push x list)
        finally (return (nreverse list)))
  (10 8))


;;
;;  for-as-arithmetic-downfrom
;;
(deftest loop-for-as-downfrom.1
  (loop with list
        for x downfrom 10 to 5
        do (push x list)
        finally (return (nreverse list)))
  (10 9 8 7 6 5))

(deftest loop-for-as-downfrom.2
  (loop with list
        for x downfrom 10 above 5
        do (push x list)
        finally (return (nreverse list)))
  (10 9 8 7 6))

(deftest loop-for-as-downfrom.3
  (loop with list
        for x downfrom 10 downto 5 by 2
        do (push x list)
        finally (return (nreverse list)))
  (10 8 6))

(deftest loop-for-as-downfrom.4
  (loop with list
        for x downfrom 10 above 5 by 2
        do (push x list)
        finally (return (nreverse list)))
  (10 8 6))

(deftest loop-for-as-downfrom.5
  (loop with list
        for x downfrom 10 downto 6 by 2
        do (push x list)
        finally (return (nreverse list)))
  (10 8 6))

(deftest loop-for-as-downfrom.6
  (loop with list
        for x downfrom 10 above 6 by 2
        do (push x list)
        finally (return (nreverse list)))
  (10 8))

(deftest loop-for-as-downfrom.7
  (loop with list
        for x downfrom 10
        do (push x list)
        (if (<= x 5)
          (return (nreverse list))))
  (10 9 8 7 6 5))

(deftest loop-for-as-downfrom.8
  (loop with list
        for x downfrom 10 by 2
        do (push x list)
        (if (<= x 5)
          (return (nreverse list))))
  (10 8 6 4))


;;
;;  for 2
;;
(deftest loop-for-as.1
  (loop with list
        for x from 1 to 5
        as y from 6 to 9
        do (push (list x y) list)
        finally (return (nreverse list)))
  ((1 6) (2 7) (3 8) (4 9)))


;;
;;  in-list
;;
(deftest loop-for-in.1
  (loop with list
        for x in '(a b c d)
        do (push x list)
        finally (return (nreverse list)))
  (a b c d))

(deftest loop-for-in.2
  (loop with list
        for x in nil
        do (push x list)
        finally (return (nreverse list)))
  nil)

(deftest loop-for-in.3
  (loop with list
        for x integer in '(1 2 3)
        do (push x list)
        finally (return (nreverse list)))
  (1 2 3))

(deftest loop-for-in.4
  (loop with list
        for (x y) in '((1 2) (3 4) (5 6))
        do (push (list y x) list)
        finally (return (nreverse list)))
  ((2 1) (4 3) (6 5)))


;;
;;  on-list
;;
(deftest loop-for-on.1
  (loop with list
        for x on '(a b c d)
        do (push x list)
        finally (return (nreverse list)))
  ((a b c d) (b c d) (c d) (d)))

(deftest loop-for-on.2
  (loop with list
        for x on nil
        do (push x list)
        finally (return (nreverse list)))
  nil)

(deftest loop-for-on.3
  (loop with list
        for x list on '(1 2 3)
        do (push x list)
        finally (return (nreverse list)))
  ((1 2 3) (2 3) (3)))

(deftest loop-for-on.4
  (loop with list
        for (x y) on '(1 2 3)
        do (push (list y x) list)
        finally (return (nreverse list)))
  ((2 1) (3 2) (nil 3)))


;;
;;  equals-then
;;
(deftest loop-for-equals.1
  (loop for x = 1
        do (return x))
  1)

(deftest loop-for-equals.2
  (loop for x integer = 99
        do (return x))
  99)

(deftest loop-for-equals.3
  (loop with list
        for x = 1 then (+ x 1)
        do (push x list)
        (if (<= 5 x)
          (return (nreverse list))))
  (1 2 3 4 5))

(deftest loop-for-equals.4
  (let ((count 0))
    (flet ((counter-test () (incf count)))
      (loop with list
            for x = (counter-test)
            do (push x list)
            (if (<= 5 x)
              (return (nreverse list))))))
  (1 2 3 4 5))

(deftest loop-for-equals.5
  (let ((count 0))
    (flet ((counter-test () (list (incf count) (1+ count))))
      (loop with list
            for (x y) = (counter-test)
            do (push (list y x) list)
            (if (<= 5 x)
              (return (nreverse list))))))
  ((2 1) (3 2) (4 3) (5 4) (6 5)))


;;
;;  across
;;
(deftest loop-for-across.1
  (loop with list
        for x across #(a b c d)
        do (push x list)
        finally (return (nreverse list)))
  (a b c d))

(deftest loop-for-across.2
  (loop with list
        for x across '(a b c d)
        do (push x list)
        finally (return (nreverse list)))
  (a b c d))

(deftest loop-for-across.3
  (loop with list
        for x across #()
        do (push x list)
        finally (return (nreverse list)))
  nil)

(deftest loop-for-across.4
  (loop with list
        for (x y) across #1a((a b) (c d))
        do (push (list y x) list)
        finally (return (nreverse list)))
  ((b a) (d c)))


;;
;;  hash
;;
(deftest loop-for-hash.1
  (let ((table (make-hash-table)))
    (setf (gethash 'aaa table) :bbb)
    (setf (gethash 'ccc table) :ddd)
    (setf (gethash 'eee table) :fff)
    (loop with list
          for x being each hash-key of table
          do (push x list)
          finally (return (sort list #'string< :key #'symbol-name))))
  (aaa ccc eee))

(deftest loop-for-hash.2
  (let ((table (make-hash-table)))
    (setf (gethash 'aaa table) :bbb)
    (setf (gethash 'ccc table) :ddd)
    (setf (gethash 'eee table) :fff)
    (loop with list
          for x being the hash-keys in table
          do (push x list)
          finally (return (sort list #'string< :key #'symbol-name))))
  (aaa ccc eee))

(deftest loop-for-hash.3
  (let ((table (make-hash-table)))
    (setf (gethash 'aaa table) :bbb)
    (setf (gethash 'ccc table) :ddd)
    (setf (gethash 'eee table) :fff)
    (loop with list
          for x being each hash-key in table using (hash-value y)
          do (push (list x y) list)
          finally (return (sort list #'string<
                                :key (lambda (x) (symbol-name (car x)))))))
  ((aaa :bbb) (ccc :ddd) (eee :fff)))

(deftest loop-for-hash.4
  (let ((table (make-hash-table)))
    (setf (gethash 'aaa table) :bbb)
    (setf (gethash 'ccc table) :ddd)
    (setf (gethash 'eee table) :fff)
    (loop with list
          for x being each hash-value of table
          do (push x list)
          finally (return (sort list #'string< :key #'symbol-name))))
  (:bbb :ddd :fff))

(deftest loop-for-hash.5
  (let ((table (make-hash-table)))
    (setf (gethash 'aaa table) :bbb)
    (setf (gethash 'ccc table) :ddd)
    (setf (gethash 'eee table) :fff)
    (loop with list
          for x being the hash-values in table
          do (push x list)
          finally (return (sort list #'string< :key #'symbol-name))))
  (:bbb :ddd :fff))

(deftest loop-for-hash.6
  (let ((table (make-hash-table)))
    (setf (gethash 'aaa table) :bbb)
    (setf (gethash 'ccc table) :ddd)
    (setf (gethash 'eee table) :fff)
    (loop with list
          for x being each hash-value in table using (hash-key y)
          do (push (list y x) list)
          finally (return (sort list #'string<
                                :key (lambda (x) (symbol-name (car x)))))))
  ((aaa :bbb) (ccc :ddd) (eee :fff)))

(deftest loop-for-hash.7
  (let ((table (make-hash-table)))
    (setf (gethash 'aaa table) (list 10 20))
    (setf (gethash 'ccc table) (list 30 40))
    (setf (gethash 'eee table) (list 50 60))
    (loop with list
          for (x y) being each hash-value in table
          do (push (list y x) list)
          finally (return (sort list #'< :key #'car))))
  ((20 10) (40 30) (60 50)))


;;
;;  package
;;
(make-package 'loop-package1 :use nil)
(make-package 'loop-package2 :use nil)
(intern "AAA" 'loop-package1)
(intern "BBB" 'loop-package1)
(intern "CCC" 'loop-package1)
(intern "DDD" 'loop-package2)
(intern "EEE" 'loop-package2)
(intern "FFF" 'loop-package2)
(export 'loop-package2::ddd 'loop-package2)
(export 'loop-package2::eee 'loop-package2)
(use-package 'loop-package2 'loop-package1)
(export 'loop-package1::aaa 'loop-package1)

(deftest loop-for-as-package-present.1
  (loop with list
        for x being each present-symbol in 'loop-package1
        do (push x list)
        finally (return (sort list #'string< :key #'symbol-name)))
  (loop-package1::aaa loop-package1::bbb loop-package1::ccc))

(deftest loop-for-as-package-symbol.1
  (loop with list
        for x being each symbol in 'loop-package1
        do (push x list)
        finally (return (sort list #'string< :key #'symbol-name)))
  (loop-package1::aaa loop-package1::bbb loop-package1::ccc
                      loop-package2::ddd loop-package2::eee))

(deftest loop-for-as-package-external.1
  (loop with list
        for x being each external-symbol in 'loop-package1
        do (push x list)
        finally (return (sort list #'string< :key #'symbol-name)))
  (loop-package1::aaa))

