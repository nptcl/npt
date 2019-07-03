;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;  simple-sort
(deftest simple-sort.1
  (lisp-system::simple-sort () #'<)
  nil)

(deftest simple-sort.2
  (lisp-system::simple-sort #() #'<)
  #())

(deftest simple-sort.3
  (lisp-system::simple-sort '(3 4 8 5 1 2 9 8 7) #'<)
  (1 2 3 4 5 7 8 8 9))

(deftest simple-sort.4
  (lisp-system::simple-sort '(3 4 8 5 1 2 9 8 7) #'>)
  (9 8 8 7 5 4 3 2 1))

(deftest simple-sort.5
  (lisp-system::simple-sort #(3 4 8 5 1 2 9 8 7) #'<)
  #(1 2 3 4 5 7 8 8 9))

(deftest simple-sort.6
  (lisp-system::simple-sort #(3 4 8 5 1 2 9 8 7) #'>)
  #(9 8 8 7 5 4 3 2 1))


;;  bubble-sort
(deftest bubble-sort.1
  (lisp-system::bubble-sort () #'<)
  nil)

(deftest bubble-sort.2
  (lisp-system::bubble-sort #() #'<)
  #())

(deftest bubble-sort.3
  (lisp-system::bubble-sort '(3 4 8 5 1 2 9 8 7) #'<)
  (1 2 3 4 5 7 8 8 9))

(deftest bubble-sort.4
  (lisp-system::bubble-sort '(3 4 8 5 1 2 9 7 8) #'<)
  (1 2 3 4 5 7 8 8 9))

(deftest bubble-sort.5
  (lisp-system::bubble-sort '(3 4 8 5 1 2 9 8 7) #'>)
  (9 8 8 7 5 4 3 2 1))

(deftest bubble-sort.6
  (lisp-system::bubble-sort '(3 4 8 5 1 2 9 7 8) #'>)
  (9 8 8 7 5 4 3 2 1))

(deftest bubble-sort.7
  (lisp-system::bubble-sort #(3 4 8 5 1 2 9 8 7) #'<)
  #(1 2 3 4 5 7 8 8 9))

(deftest bubble-sort.8
  (lisp-system::bubble-sort #(3 4 8 5 1 2 9 8 7) #'>)
  #(9 8 8 7 5 4 3 2 1))

(deftest bubble-sort.9
  (lisp-system::bubble-sort
    '((1 5) (3 6) (3 8) (9 2) (1 4) (8 4))
    #'< :key #'car)
  ((1 5) (1 4) (3 6) (3 8) (8 4) (9 2)))

(deftest bubble-sort.10
  (lisp-system::bubble-sort
    #((1 5) (3 6) (3 8) (9 2) (1 4) (8 4))
    #'< :key #'car)
  #((1 5) (1 4) (3 6) (3 8) (8 4) (9 2)))


;;
;;  common-lisp
;;
(deftest sort.1
  (sort () #'<)
  nil)

(deftest sort.2
  (sort #() #'<)
  #())

(deftest sort.3
  (sort '(3 4 8 5 1 2 9 8 7) #'<)
  (1 2 3 4 5 7 8 8 9))

(deftest sort.4
  (sort '(3 4 8 5 1 2 9 8 7) #'>)
  (9 8 8 7 5 4 3 2 1))

(deftest sort.5
  (sort #(3 4 8 5 1 2 9 8 7) #'<)
  #(1 2 3 4 5 7 8 8 9))

(deftest sort.6
  (sort #(3 4 8 5 1 2 9 8 7) #'>)
  #(9 8 8 7 5 4 3 2 1))

(deftest stable-sort.1
  (stable-sort () #'<)
  nil)

(deftest stable-sort.2
  (stable-sort #() #'<)
  #())

(deftest stable-sort.3
  (stable-sort '(3 4 8 5 1 2 9 8 7) #'<)
  (1 2 3 4 5 7 8 8 9))

(deftest stable-sort.4
  (stable-sort '(3 4 8 5 1 2 9 8 7) #'>)
  (9 8 8 7 5 4 3 2 1))

(deftest stable-sort.5
  (stable-sort #(3 4 8 5 1 2 9 8 7) #'<)
  #(1 2 3 4 5 7 8 8 9))

(deftest stable-sort.6
  (stable-sort #(3 4 8 5 1 2 9 8 7) #'>)
  #(9 8 8 7 5 4 3 2 1))

(deftest stable-sort.7
  (stable-sort
    '((1 5) (3 6) (3 8) (9 2) (1 4) (8 4))
    #'< :key #'car)
  ((1 5) (1 4) (3 6) (3 8) (8 4) (9 2)))

(deftest stable-sort.8
  (stable-sort
    '((1 5) (3 6) (3 8) (9 2) (1 4) (8 4))
    #'< :key #'car)
  ((1 5) (1 4) (3 6) (3 8) (8 4) (9 2)))

