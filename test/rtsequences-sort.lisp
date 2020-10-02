;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function SIMPLE-SORT
;;
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

(deftest simple-sort.7
  (lisp-system::simple-sort
    '((5 3) (2 9) (8 4) (4 8))
    #'< :key #'car)
  ((2 9) (4 8) (5 3) (8 4)))

(deftest simple-sort.8
  (lisp-system::simple-sort
    '((5 3) (2 9) (8 4) (4 8))
    #'< :key #'cadr)
  ((5 3) (8 4) (4 8) (2 9)))


;;
;;  Function BUBBLE-SORT
;;
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
;;  Function SORT
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

(deftest-error sort-error.1
  (eval '(sort 10)))

(deftest-error! sort-error.2
  (eval '(sort)))

(deftest-error sort-error.3
  (eval '(sort nil #'< nil)))

(deftest-error sort-error.4
  (eval '(sort nil #'< :key)))

(deftest-error sort-error.5
  (eval '(sort nil #'< :key 10)))

(deftest-error sort-error.6
  (eval '(sort nil #'< :hello 10)))


;;
;;  Function STABLE-SORT
;;
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

(deftest-error stable-sort-error.1
  (eval '(stable-sort 10)))

(deftest-error! stable-sort-error.2
  (eval '(stable-sort)))

(deftest-error stable-sort-error.3
  (eval '(stable-sort nil #'< nil)))

(deftest-error stable-sort-error.4
  (eval '(stable-sort nil #'< :key)))

(deftest-error stable-sort-error.5
  (eval '(stable-sort nil #'< :key 10)))

(deftest-error stable-sort-error.6
  (eval '(stable-sort nil #'< :hello 10)))


;;  ANSI Common Lisp
(defvar *sort-data*)

(deftest sort-test.1
  (progn
    (setq *sort-data* (copy-seq "lkjashd"))
    (sort *sort-data* #'char-lessp))
  "adhjkls")

(deftest sort-test.2
  (progn
    (setq *sort-data* (list '(1 2 3) '(4 5 6) '(7 8 9)))
    (sort *sort-data* #'> :key #'car))
  ((7 8 9) (4 5 6) (1 2 3)))

(deftest sort-test.3
  (progn
    (setq *sort-data* (list 1 2 3 4 5 6 7 8 9 0))
    (stable-sort *sort-data* #'(lambda (x y) (and (oddp x) (evenp y)))))
  (1 3 5 7 9 2 4 6 8 0))

(deftest sort-test.4
  (sort (setq *sort-data*
              (vector (list (list "JonL" "White") "Iteration")
                      (list (list "Dick" "Waters") "Iteration")
                      (list (list "Dick" "Gabriel") "Objects")
                      (list (list "Kent" "Pitman") "Conditions")
                      (list (list "Gregor" "Kiczales") "Objects")
                      (list (list "David" "Moon") "Objects")
                      (list (list "Kathy" "Chapman") "Editorial")
                      (list (list "Larry" "Masinter") "Cleanup")
                      (list (list "Sandra" "Loosemore") "Compiler")))
        #'string-lessp :key #'cadar)
  #((("Kathy" "Chapman") "Editorial")
    (("Dick" "Gabriel") "Objects")
    (("Gregor" "Kiczales") "Objects")
    (("Sandra" "Loosemore") "Compiler")
    (("Larry" "Masinter") "Cleanup")
    (("David" "Moon") "Objects")
    (("Kent" "Pitman") "Conditions")
    (("Dick" "Waters") "Iteration")
    (("JonL" "White") "Iteration")))

;; Note that individual alphabetical order within `committees'
;; is preserved.
(deftest sort-test.5
  (setq *sort-data*
        (stable-sort *sort-data* #'string-lessp :key #'cadr))
  #((("Larry" "Masinter") "Cleanup")
    (("Sandra" "Loosemore") "Compiler")
    (("Kent" "Pitman") "Conditions")
    (("Kathy" "Chapman") "Editorial")
    (("Dick" "Waters") "Iteration")
    (("JonL" "White") "Iteration")
    (("Dick" "Gabriel") "Objects")
    (("Gregor" "Kiczales") "Objects")
    (("David" "Moon") "Objects")))

