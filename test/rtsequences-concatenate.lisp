;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function CONCATENATE
;;

;;  list, cons
(deftest concatenate-list.1
  (concatenate 'list)
  nil)

(deftest-error concatenate-list.2
  (concatenate 'cons))

(deftest concatenate-list.3
  (concatenate 'list nil)
  nil)

(deftest-error concatenate-list.4
  (concatenate 'cons nil))

(deftest concatenate-list.5
  (concatenate 'list '(a))
  (a))

(deftest concatenate-list.6
  (concatenate 'cons '(a))
  (a))

(deftest concatenate-list.7
  (concatenate 'list '(a b c) #(d e f) '(g))
  (a b c d e f g))

(deftest concatenate-list.8
  (concatenate 'cons #(a b c) '(d e f) #(g))
  (a b c d e f g))

(deftest-error concatenate-list.9
  (concatenate 'list '(a b c) #(d e f) '(g . h)))


;;  vector
(deftest concatenate-vector.1
  (concatenate 'vector)
  #())

(deftest-error concatenate-vector.2
  (concatenate '(vector t 3)))

(deftest concatenate-vector.3
  (concatenate 'vector '(a b c))
  #(a b c))

(deftest concatenate-vector.4
  (concatenate '(vector symbol 3) '(a b c))
  #(a b c))

(deftest-error concatenate-vector.5
  (concatenate '(vector character 3) '(a b c)))

(deftest concatenate-vector.6
  (concatenate '(vector integer 3) '(a b c))
  #(a b c))

(deftest-error concatenate-vector.7
  (concatenate '(vector t 2) '(a b c)))

(deftest concatenate-vector.8
  (concatenate 'vector '(a b c) #*1011 "DEF")
  #(a b c 1 0 1 1 #\D #\E #\F))


;;  simple-vector
(deftest concatenate-simple-vector.1
  (concatenate 'simple-vector)
  #())

(deftest concatenate-simple-vector.2
  (concatenate 'simple-vector nil)
  #())

(deftest concatenate-simple-vector.3
  (concatenate 'simple-vector '(a b c) #(d e f))
  #(a b c d e f))

(deftest concatenate-simple-vector.4
  (concatenate '(simple-vector 6) '(a b c) #(d e f))
  #(a b c d e f))

(deftest-error concatenate-simple-vector.5
  (concatenate '(simple-vector 10) '(a b c) #(d e f)))

(deftest concatenate-simple-vector.6
  (concatenate 'simple-vector '(a b c) #*1011 "DEF")
  #(a b c 1 0 1 1 #\D #\E #\F))


;;  string
(deftest concatenate-string.1
  (concatenate 'string)
  "")

(deftest concatenate-string.2
  (stringp
    (concatenate 'string "Hello"))
  t)

(deftest concatenate-string.3
  (concatenate 'string '(#\a #\B #\C) "DEf")
  "aBCDEf")

(deftest concatenate-string.4
  (concatenate '(string 6) '(#\a #\B #\C) "DEf")
  "aBCDEf")

(deftest-error concatenate-string.5
  (concatenate '(string 2) '(#\a #\B #\C) "DEf"))


;;  array
(deftest concatenate-array.1
  (concatenate 'array)
  #())

(deftest-error concatenate-array.2
  (concatenate '(array t (3))))

(deftest concatenate-array.3
  (concatenate 'array '(a b c))
  #(a b c))

(deftest concatenate-array.4
  (concatenate '(array symbol (3)) '(a b c))
  #(a b c))

(deftest-error concatenate-array.5
  (concatenate '(array character (3)) '(a b c)))

(deftest concatenate-array.6
  (concatenate '(array integer (3)) '(a b c))
  #(a b c))

(deftest-error concatenate-array.7
  (concatenate '(array t (2)) '(a b c)))

(deftest concatenate-array.8
  (concatenate 'array '(a b c) #*1011 "DEF")
  #(a b c 1 0 1 1 #\D #\E #\F))


;;  bit-vector
(deftest concatenate-bit-vector.1
  (concatenate 'bit-vector)
  #*)

(deftest concatenate-bit-vector.2
  (concatenate 'bit-vector nil)
  #*)

(deftest concatenate-bit-vector.3
  (concatenate 'bit-vector #(1 0 0 1) #*1110)
  #*10011110)

(deftest concatenate-bit-vector.4
  (concatenate '(bit-vector 8) #(1 0 0 1) #*1110)
  #*10011110)

(deftest-error concatenate-bit-vector.5
  (concatenate '(bit-vector 7) #(1 0 0 1) #*1110))

(deftest-error concatenate-bit-vector.6
  (concatenate 'bit-vector #(1 0 2 1) #*1110))


;;  simple-bit-vector
(deftest concatenate-simple-bit-vector.1
  (concatenate 'simple-bit-vector)
  #*)

(deftest concatenate-simple-bit-vector.2
  (concatenate 'simple-bit-vector nil)
  #*)

(deftest concatenate-simple-bit-vector.3
  (concatenate 'simple-bit-vector #(1 0 0 1) #*1110)
  #*10011110)

(deftest concatenate-simple-bit-vector.4
  (concatenate '(simple-bit-vector 8) #(1 0 0 1) #*1110)
  #*10011110)

(deftest-error concatenate-simple-bit-vector.5
  (concatenate '(simple-bit-vector 7) #(1 0 0 1) #*1110))

(deftest-error concatenate-simple-bit-vector.6
  (concatenate 'simple-bit-vector #(1 0 2 1) #*1110))


;;  error
(deftest-error concatenate-error.1
  (eval '(concatenate 10)))

(deftest-error! concatenate-error.2
  (eval '(concatenate)))

(deftest-error concatenate-error.3
  (eval '(concatenate t 20)))


;;  ANSI Common Lisp
(deftest concatenate-test.1
  (concatenate 'string "all" " " "together" " " "now")
  "all together now")

(deftest concatenate-test.2
  (concatenate 'list "ABC" '(d e f) #(1 2 3) #*1011)
  (#\A #\B #\C D E F 1 2 3 1 0 1 1))

(deftest concatenate-test.3
  (concatenate 'list)
  nil)

(deftest-error concatenate-test.4
  (concatenate '(vector * 2) "a" "bc"))

