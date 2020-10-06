;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function MERGE
;;

;;  list, cons
(deftest merge-list.1
  (merge 'list nil nil #'<)
  nil)

(deftest-error merge-list.2
  (merge 'cons nil nil #'<))

(deftest merge-list.3
  (merge 'list #() #() #'<)
  nil)

(deftest-error merge-list.4
  (merge 'cons #() #() #'<))

(deftest merge-list.5
  (merge 'list '(1 2 3) nil #'<)
  (1 2 3))

(deftest merge-list.6
  (merge 'cons nil #(1 2 3) #'<)
  (1 2 3))

(deftest merge-list.7
  (merge 'list '(1 3 4 6 8) '(2 5 9) #'<)
  (1 2 3 4 5 6 8 9))

(deftest merge-list.8
  (merge 'cons '(1 3 4 6 8) '(2 5 9) #'<)
  (1 2 3 4 5 6 8 9))

(deftest merge-list.9
  (merge 'list '(1 3 4 6 8) #(2 5 9) #'<)
  (1 2 3 4 5 6 8 9))

(deftest merge-list.10
  (merge 'list #(1 3 4 6 8) nil #'<)
  (1 3 4 6 8))

(deftest merge-list.11
  (merge 'list #() '(1 3 4 6 8) #'<)
  (1 3 4 6 8))

(deftest merge-list.12
  (merge 'list
         '((1) (2) (3) (6) (7) (8))
         '((3) (4) (5) (6) (7) (9))
         #'< :key #'car)
  ((1) (2) (3) (3) (4) (5) (6) (6) (7) (7) (8) (9)))

(deftest merge-list.13
  (merge 'list '(1 3) '(2 4 5 6 7 8 9) #'<)
  (1 2 3 4 5 6 7 8 9))

(deftest merge-list.14
  (merge 'list '(2 4 5 6 7 8 9) '(1 3) #'<)
  (1 2 3 4 5 6 7 8 9))


;;  vector
(deftest merge-vector.1
  (merge 'vector nil nil #'<)
  #())

(deftest merge-vector.2
  (merge 'vector '(1 2 3) #() #'<)
  #(1 2 3))

(deftest merge-vector.3
  (merge 'vector nil #(1 2 3) #'<)
  #(1 2 3))

(deftest merge-vector.4
  (merge 'vector '(1 3 4 6 8) '(2 5 9) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest merge-vector.5
  (merge 'vector '(1 3 4 6 8) '(2 5 9) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest merge-vector.6
  (merge 'vector #(2 5 9) #(1 3 4 6 8) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest merge-vector.7
  (merge 'list
         '((1) (2) (3) (6) (7) (8))
         '((3) (4) (5) (6) (7) (9))
         #'< :key #'car)
  ((1) (2) (3) (3) (4) (5) (6) (6) (7) (7) (8) (9)))

(deftest merge-vector.8
  (merge '(vector (signed-byte 8) 8) '(1 3 4 6 8) '(2 5 9) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest-error merge-vector.9
  (merge '(vector character 8) '(1 3 4 6 8) '(2 5 9) #'<))

(deftest-error merge-vector.10
  (merge '(vector (signed-byte 8) 7) '(1 3 4 6 8) '(2 5 9) #'<))

(deftest merge-vector.11
  (merge '(vector pathname) '(1 2 3) nil #'<)
  #(1 2 3))


;;  simple-vector
(deftest merge-simple-vector.1
  (merge 'simple-vector nil nil #'<)
  #())

(deftest merge-simple-vector.2
  (merge 'simple-vector '(1 2 3) #() #'<)
  #(1 2 3))

(deftest merge-simple-vector.3
  (merge 'simple-vector nil #(1 2 3) #'<)
  #(1 2 3))

(deftest merge-simple-vector.4
  (merge 'simple-vector '(1 3 4 6 8) '(2 5 9) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest merge-simple-vector.5
  (merge 'simple-vector '(1 3 4 6 8) '(2 5 9) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest merge-simple-vector.6
  (merge 'simple-vector #(2 5 9) #(1 3 4 6 8) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest merge-simple-vector.7
  (merge 'list
         '((1) (2) (3) (6) (7) (8))
         '((3) (4) (5) (6) (7) (9))
         #'< :key #'car)
  ((1) (2) (3) (3) (4) (5) (6) (6) (7) (7) (8) (9)))

(deftest merge-simple-vector.8
  (merge '(simple-vector 8) '(1 3 4 6 8) '(2 5 9) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest-error merge-simple-vector.9
  (merge '(simple-vector 7) '(1 3 4 6 8) '(2 5 9) #'<))

(deftest merge-simple-vector.10
  (merge '(simple-vector 8) #1a(1 3 4 6 8) '(2 5 9) #'<)
  #(1 2 3 4 5 6 8 9))


;;  string
(deftest merge-string.1
  (merge 'string nil nil #'char<)
  "")

(deftest merge-string.2
  (merge 'string "abc" nil #'char<)
  "abc")

(deftest merge-string.3
  (merge 'string "" '(#\d #\e #\f) #'char<)
  "def")

(deftest merge-string.4
  (merge 'string "ABEGHI" "CDF" #'char<)
  "ABCDEFGHI")

(deftest merge-string.5
  (merge 'string "ABE" "CDFGHI" #'char<)
  "ABCDEFGHI")

(deftest merge-string.6
  (merge 'string '(#\1 #\3 #\4 #\6 #\8) "259" #'char<)
  "12345689")

(deftest merge-string.7
  (merge '(string 3) "abc" nil #'char<)
  "abc")

(deftest-error merge-string.8
  (merge '(string 4) "abc" nil #'char<))


;;  array
(deftest merge-array.1
  (merge 'array nil nil #'<)
  #())

(deftest merge-array.2
  (merge 'array '(1 2 3) #() #'<)
  #(1 2 3))

(deftest merge-array.3
  (merge 'array nil #(1 2 3) #'<)
  #(1 2 3))

(deftest merge-array.4
  (merge 'array '(1 3 4 6 8) '(2 5 9) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest merge-array.5
  (merge 'array '(1 3 4 6 8) '(2 5 9) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest merge-array.6
  (merge 'array #(2 5 9) #(1 3 4 6 8) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest merge-array.7
  (merge 'list
         '((1) (2) (3) (6) (7) (8))
         '((3) (4) (5) (6) (7) (9))
         #'< :key #'car)
  ((1) (2) (3) (3) (4) (5) (6) (6) (7) (7) (8) (9)))

(deftest merge-array.8
  (merge '(array (signed-byte 8) (8)) '(1 3 4 6 8) '(2 5 9) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest-error merge-array.9
  (merge '(array character (8)) '(1 3 4 6 8) '(2 5 9) #'<))

(deftest-error merge-array.10
  (merge '(array (signed-byte 8) (7)) '(1 3 4 6 8) '(2 5 9) #'<))

(deftest merge-array.11
  (merge '(array pathname) '(1 2 3) nil #'<)
  #(1 2 3))


;;  bit-vector
(deftest merge-bit-vector.1
  (merge 'bit-vector nil nil #'=)
  #*)

(deftest merge-bit-vector.2
  (merge 'bit-vector '(1 1 0) nil #'=)
  #*110)

(deftest merge-bit-vector.3
  (merge 'bit-vector nil #(1 1 0) #'=)
  #*110)

(deftest merge-bit-vector.4
  (merge 'bit-vector '(1 1 1 0) #(1 0) #'=)
  #*111100)

(deftest merge-bit-vector.5
  (merge 'bit-vector #*10 #*1110 #'=)
  #*111100)

(deftest merge-bit-vector.6
  (merge '(bit-vector 6) #*10 #*1110 #'=)
  #*111100)

(deftest-error merge-bit-vector.7
  (merge '(bit-vector 3) #*10 #*1110 #'=))


;;  simple-bit-vector
(deftest merge-simple-bit-vector.1
  (merge 'simple-bit-vector nil nil #'=)
  #*)

(deftest merge-simple-bit-vector.2
  (merge 'simple-bit-vector '(1 1 0) nil #'=)
  #*110)

(deftest merge-simple-bit-vector.3
  (merge 'simple-bit-vector nil #(1 1 0) #'=)
  #*110)

(deftest merge-simple-bit-vector.4
  (merge 'simple-bit-vector '(1 1 1 0) #(1 0) #'=)
  #*111100)

(deftest merge-simple-bit-vector.5
  (merge 'simple-bit-vector #*10 #*1110 #'=)
  #*111100)

(deftest merge-simple-bit-vector.6
  (merge '(simple-bit-vector 6) #*10 #*1110 #'=)
  #*111100)

(deftest-error merge-simple-bit-vector.7
  (merge '(simple-bit-vector 3) #*10 #*1110 #'=))


;;  error
(deftest-error! merge-error.1
  (eval '(mereg 10 nil nil)))

(deftest-error merge-error.2
  (eval '(mereg 10 nil nil #'<)))

(deftest-error merge-error.3
  (eval '(mereg t 20 nil #'<)))

(deftest-error merge-error.4
  (eval '(mereg t nil 30 #'<)))

(deftest-error merge-error.5
  (eval '(mereg t nil nil 40)))

(deftest-error merge-error.6
  (eval '(mereg t nil nil #'< nil)))

(deftest-error merge-error.7
  (eval '(mereg t nil nil #'< :key)))

(deftest-error merge-error.8
  (eval '(mereg t nil nil #'< :key 10)))

(deftest-error merge-error.9
  (eval '(mereg t nil nil #'< :hello 10)))


;;  ANSI Common Lisp
(defparameter *merge-list1* nil)
(defparameter *merge-list2* nil)

(deftest merge-test.1
  (progn
    (setq *merge-list1* (list 1 3 4 6 7))
    (setq *merge-list2* (list 2 5 8))
    (merge 'list *merge-list1* *merge-list2* #'<))
  (1 2 3 4 5 6 7 8))

(deftest merge-test.2
  (progn
    (setq *merge-list1* (copy-seq "BOY"))
    (setq *merge-list2* (copy-seq "nosy"))
    (merge 'string *merge-list1* *merge-list2* #'char-lessp))
  "BnOosYy")

(deftest merge-test.3
  (progn
    (setq *merge-list1* (vector '(red . 1) '(blue . 4)))
    (setq *merge-list2* (vector '(yellow . 2) '(green . 7)))
    (merge 'vector *merge-list1* *merge-list2* #'< :key #'cdr))
  #((red . 1) (yellow . 2) (blue . 4) (green . 7)))

(deftest-error merge-test.4
  (merge '(vector * 4) '(1 5) '(2 4 6) #'<))

