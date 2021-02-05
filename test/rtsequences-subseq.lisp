;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Accessor SUBSEQ
;;

;;  list
(deftest subseq-list.1
  (subseq nil 0)
  nil)

(deftest subseq-list.2
  (subseq nil 0 0)
  nil)

(deftest subseq-list.3
  (subseq nil 0 nil)
  nil)

(deftest subseq-list.4
  (subseq '(a b c) 0 0)
  nil)

(deftest subseq-list.5
  (subseq '(a b c d e) 3)
  (d e))

(deftest subseq-list.6
  (subseq '(a b c d e) 3 nil)
  (d e))

(deftest subseq-list.7
  (subseq '(a b c d e) 3 3)
  nil)

(deftest subseq-list.8
  (subseq '(a b c d e) 0 5)
  (a b c d e))

(deftest subseq-list.9
  (subseq '(a b c d e) 1 3)
  (b c))

(deftest subseq-list.10
  (subseq '(a b c d e) 5)
  nil)

(deftest subseq-list.11
  (subseq '(a b c d e) 5 5)
  nil)

(deftest-error subseq-list.12
  (subseq '(a b c d e) 6))

(deftest-error subseq-list.13
  (subseq '(a b c d e) 2 6))

(deftest-error subseq-list.14
  (subseq '(a b c d e . z) 3))

(deftest subseq-list.15
  (subseq nil 0 0)
  nil)

(deftest-error subseq-list.16
  (subseq nil 1))

(deftest-error subseq-list.17
  (subseq nil 0 1))

(deftest-error subseq-list.18
  (subseq nil 7 7))

(deftest-error subseq-list.19
  (subseq '(a) 7 7))


;;  vector
(deftest subseq-vector.1
  (subseq #() 0)
  #())

(deftest subseq-vector.2
  (subseq #() 0 nil)
  #())

(deftest subseq-vector.3
  (subseq #() 0 0)
  #())

(deftest-error subseq-vector.4
  (subseq #() 1))

(deftest-error subseq-vector.5
  (subseq #() 0 1))

(deftest subseq-vector.6
  (subseq #(a b c) 0 0)
  #())

(deftest subseq-vector.7
  (subseq #(a b c) 1 1)
  #())

(deftest subseq-vector.8
  (subseq #(a b c d e) 3)
  #(d e))

(deftest subseq-vector.9
  (subseq #(a b c d e) 3 nil)
  #(d e))

(deftest subseq-vector.10
  (subseq #(a b c d e) 3 3)
  #())

(deftest subseq-vector.11
  (subseq #(a b c d e) 0 5)
  #(a b c d e))

(deftest subseq-vector.12
  (subseq #(a b c d e) 1 3)
  #(b c))

(deftest subseq-vector.13
  (subseq #(a b c d e) 5)
  #())

(deftest subseq-vector.14
  (subseq #(a b c d e) 5 5)
  #())

(deftest-error subseq-vector.15
  (subseq #(a b c d e) 6))

(deftest-error subseq-vector.16
  (subseq #(a b c d e) 0 6))

(deftest-error subseq-vector.17
  (subseq #() 7 7))

(deftest-error subseq-vector.18
  (subseq #(a) 7 7))


;;  string
(deftest subseq-string.1
  (subseq "" 0)
  "")

(deftest subseq-string.2
  (subseq "" 0 nil)
  "")

(deftest subseq-string.3
  (subseq "" 0 0)
  "")

(deftest-error subseq-string.4
  (subseq "" 1))

(deftest-error subseq-string.5
  (subseq "" 0 1))

(deftest subseq-string.6
  (subseq "abc" 0 0)
  "")

(deftest subseq-string.7
  (subseq "abc" 1 1)
  "")

(deftest subseq-string.8
  (subseq "abcde" 3)
  "de")

(deftest subseq-string.9
  (subseq "abcde" 3 nil)
  "de")

(deftest subseq-string.10
  (subseq "abcde" 3 3)
  "")

(deftest subseq-string.11
  (subseq "abcde" 0 5)
  "abcde")

(deftest subseq-string.12
  (subseq "abcde" 1 3)
  "bc")

(deftest subseq-string.13
  (subseq "abcde" 5)
  "")

(deftest subseq-string.14
  (subseq "abcde" 5 5)
  "")

(deftest-error subseq-string.15
  (subseq "abcde" 6))

(deftest-error subseq-string.16
  (subseq "abcde" 0 6))


;;  array
(deftest subseq-array.1
  (subseq #1a() 0)
  #())

(deftest subseq-array.2
  (subseq #1a() 0 nil)
  #())

(deftest subseq-array.3
  (subseq #1a() 0 0)
  #())

(deftest-error subseq-array.4
  (subseq #1a() 1))

(deftest-error subseq-array.5
  (subseq #1a() 0 1))

(deftest subseq-array.6
  (subseq #1a(a b c) 0 0)
  #())

(deftest subseq-array.7
  (subseq #1a(a b c) 1 1)
  #())

(deftest subseq-array.8
  (subseq #1a(a b c d e) 3)
  #(d e))

(deftest subseq-array.9
  (subseq #1a(a b c d e) 3 nil)
  #(d e))

(deftest subseq-array.10
  (subseq #1a(a b c d e) 3 3)
  #())

(deftest subseq-array.11
  (subseq #1a(a b c d e) 0 5)
  #(a b c d e))

(deftest subseq-array.12
  (subseq #1a(a b c d e) 1 3)
  #(b c))

(deftest subseq-array.13
  (subseq #1a(a b c d e) 5)
  #())

(deftest subseq-array.14
  (subseq #1a(a b c d e) 5 5)
  #())

(deftest-error subseq-array.15
  (subseq #1a(a b c d e) 6))

(deftest-error subseq-array.16
  (subseq #1a(a b c d e) 0 6))

(deftest-error subseq-array.17
  (subseq #1a() 7 7))

(deftest-error subseq-array.18
  (subseq #1a(a) 7 7))


;; general array
(deftest subseq-general.1
  (subseq #1a() 0)
  #())

(deftest subseq-general.2
  (subseq #1a() 0 0)
  #())

(deftest subseq-general.3
  (subseq #1a(a b 10) 0 0)
  #())

(deftest subseq-general.4
  (subseq #1a(a b 10 20 #\c) 3)
  #(20 #\c))

(deftest subseq-general.5
  (subseq #1a(a b 10 20 #\c) 3 3)
  #())

(deftest subseq-general.6
  (subseq #1a(a b 10 20 #\c) 0 5)
  #1a(a b 10 20 #\c))

(deftest subseq-general.7
  (subseq #1a(a b 10 20 #\c) 1 3)
  #(b 10))

(deftest subseq-general.8
  (subseq #1a(a b 10 20 #\c) 5 5)
  #())

(deftest-error subseq-general.9
  (eval '(subseq #2a((a b) (c d) (e f) (g h)) 0))
  type-error)


;; general array
(defun subseq-arrayspec (type args)
  (make-array (length args) :element-type type :initial-contents args))

(deftest subseq-arrayspec.1
  (subseq-arrayspec '(unsigned-byte 16) nil)
  #())

(deftest subseq-arrayspec.2
  (subseq-arrayspec '(unsigned-byte 16) '(10 20 30))
  #(10 20 30))

(deftest subseq-arrayspec.3
  (lisp-system::array-specialized-p
    (subseq-arrayspec '(unsigned-byte 16) '(10 20 30)))
  t)

(defun subseq-unsigned16 (&rest args)
  (subseq-arrayspec '(unsigned-byte 16) args))

(deftest subseq-specialized.1
  (subseq (subseq-unsigned16) 0)
  #())

(deftest subseq-specialized.2
  (subseq (subseq-unsigned16) 0 0)
  #())

(deftest subseq-specialized.3
  (subseq (subseq-unsigned16 10 20 30) 0 0)
  #())

(deftest subseq-specialized.4
  (subseq (subseq-unsigned16 5 6 7 8 9) 3)
  #(8 9))

(deftest subseq-specialized.5
  (subseq (subseq-unsigned16 5 6 7 8 9) 3 3)
  #())

(deftest subseq-specialized.6
  (subseq (subseq-unsigned16 5 6 7 8 9) 0 5)
  #(5 6 7 8 9))

(deftest subseq-specialized.7
  (subseq (subseq-unsigned16 5 6 7 8 9) 1 3)
  #(6 7))

(deftest subseq-specialized.8
  (subseq (subseq-unsigned16 5 6 7 8 9) 5 5)
  #())


;;  array character
(defun subseq-character (&rest args)
  (subseq-arrayspec 'character args))

(deftest subseq-character.1
  (subseq (subseq-character) 0)
  "")

(deftest subseq-character.2
  (subseq (subseq-character) 0 0)
  "")

(deftest subseq-character.3
  (subseq (subseq-character #\A #\b #\c) 0 0)
  "")

(deftest subseq-character.4
  (subseq (subseq-character #\A #\B #\C #\d #\e) 3)
  "de")

(deftest subseq-character.5
  (subseq (subseq-character #\A #\B #\C #\d #\e) 3 3)
  "")

(deftest subseq-character.6
  (subseq (subseq-character #\A #\B #\C #\d #\e) 0 5)
  "ABCde")

(deftest subseq-character.7
  (subseq (subseq-character #\A #\B #\C #\d #\e) 1 3)
  "BC")

(deftest subseq-character.8
  (subseq (subseq-character #\A #\B #\C #\d #\e) 5 5)
  "")

(deftest subseq-character.9
  (subseq
    (make-array 5 :element-type 'character :initial-element #\A)
    2)
  "AAA")


;;  array bit
(defun subseq-bit (&rest args)
  (subseq-arrayspec 'bit  args))

(deftest subseq-bit.1
  (subseq (subseq-bit) 0)
  #*)

(deftest subseq-bit.2
  (subseq (subseq-bit) 0 0)
  #*)

(deftest subseq-bit.3
  (subseq (subseq-bit 1 1 0) 0 0)
  #*)

(deftest subseq-bit.4
  (subseq (subseq-bit 1 1 0 0 1) 3)
  #*01)

(deftest subseq-bit.5
  (subseq (subseq-bit 1 1 0 0 1) 3 3)
  #*)

(deftest subseq-bit.6
  (subseq (subseq-bit 1 1 0 0 1) 0 5)
  #*11001)

(deftest subseq-bit.7
  (subseq (subseq-bit 1 1 0 0 1) 1 3)
  #*10)

(deftest subseq-bit.8
  (subseq (subseq-bit 1 1 0 0 1) 5 5)
  #*)

(deftest subseq-bit.9
  (subseq
    (make-array 5 :element-type 'bit :initial-element 1)
    2)
  #*111)


;;  error
(deftest-error subseq-error.1
  (eval '(subseq 10 0)))

(deftest-error! subseq-error.2
  (eval '(subseq nil)))

(deftest-error! subseq-error.3
  (eval '(subseq nil 0 0 0)))

(deftest-error subseq-error.4
  (eval '(subseq nil "A")))

(deftest-error subseq-error.5
  (eval '(subseq nil 0 "B")))


;;  ANSI Common Lisp
(defparameter *subseq* "012345")

(deftest subseq-test.1
  (subseq *subseq* 2)
  "2345")

(deftest subseq-test.2
  (subseq *subseq* 3 5)
  "34")

(deftest subseq-test.3
  (values
    (setf (subseq *subseq* 4) "abc")
    *subseq*)
  "abc" "0123ab")

(deftest subseq-test.4
  (values
    (setf (subseq *subseq* 0 2) "A")
    *subseq*)
  "A" "A123ab")


;;
;;  Accessor (SETF SUBSEQ)
;;
(deftest setf-subseq.1
  (let ((x #()))
    (setf (subseq x 0) '(a b c))
    x)
  #())

(deftest setf-subseq.2
  (let ((x #()))
    (setf (subseq x 0) '(a b c)))
  (a b c))

(deftest setf-subseq.3
  (let ((x #()))
    (setf (subseq x 0 nil) '(a b c))
    x)
  #())

(deftest setf-subseq.4
  (let ((x #()))
    (setf (subseq #() 0 0) '(a b c))
    x)
  #())

(deftest-error setf-subseq.5
  (let ((x #()))
    (setf (subseq x 1) '(a b c))))

(deftest-error setf-subseq.6
  (let ((x #()))
    (setf (subseq x 0 1) '(a b c))))

(deftest setf-subseq.7
  (let ((x #(a b c)))
    (setf (subseq x 0 0) '(1 2 3 4 5))
    x)
  #(a b c))

(deftest setf-subseq.8
  (let ((x #(a b c)))
    (setf (subseq x 1 1) '(1 2 3 4 5))
    x)
  #(a b c))

(deftest setf-subseq.9
  (let ((x #(a b c d e)))
    (setf (subseq x 3) '(1 2 3 4 5 6))
    x)
  #(a b c 1 2))

(deftest setf-subseq.10
  (let ((x #(a b c d e)))
    (setf (subseq x 2) '(1 2))
    x)
  #(a b 1 2 e))

(deftest setf-subseq.11
  (let ((x #(a b c d e)))
    (setf (subseq x 2) nil)
    x)
  #(a b c d e))

(deftest setf-subseq.12
  (let ((x #(a b c d e)))
    (setf (subseq x 3 nil) '(1 2 3 4 5))
    x)
  #(a b c 1 2))

(deftest setf-subseq.13
  (let ((x #(a b c d e)))
    (setf (subseq x 3 3) '(1 2 3 4 5))
    x)
  #(a b c d e))

(deftest setf-subseq.14
  (let ((x #(a b c d e)))
    (setf (subseq x 0 5) '(1 2 3 4 5 6))
    x)
  #(1 2 3 4 5))

(deftest setf-subseq.15
  (let ((x #(a b c d e)))
    (setf (subseq x 0 5) '(1 2))
    x)
  #(1 2 c d e))

(deftest setf-subseq.16
  (let ((x #(a b c d e)))
    (setf (subseq x 1 3) '(1 2 3 4 5))
    x)
  #(a 1 2 d e))

(deftest setf-subseq.17
  (let ((x #(a b c d e)))
    (setf (subseq x 1 3) '(1))
    x)
  #(a 1 c d e))

(deftest setf-subseq.18
  (let ((x #(a b c d e)))
    (setf (subseq x 5) '(1 2 3 4 5))
    x)
  #(a b c d e))

(deftest setf-subseq.19
  (let ((x #(a b c d e)))
    (setf (subseq x 5 5) '(1 2 3))
    x)
  #(a b c d e))

(deftest-error setf-subseq.20
  (let ((x #(a b c d e)))
    (setf (subseq x 6) '(1 2 3))))

(deftest-error setf-subseq.21
  (let ((x #(a b c d e)))
    (setf (subseq x 0 6) '(1 2 3))))

(deftest-error setf-subseq.22
  (let ((x #(a b c d e)))
    (setf (subseq x 7 7) '(1 2 3))))

(deftest-error setf-subseq.23
  (let ((x #(a)))
    (setf (subseq x 7 7) '(1 2 3))))


;;  sequence type
(deftest setf-subseq-ll.1
  (let ((a '(a b c d e)))
    (setf (subseq a 0) '(x y z)))
  (x y z))

(deftest setf-subseq-ll.2
  (let ((a '(a b c d e)))
    (setf (subseq a 0) '(x y z))
    a)
  (x y z d e))

(deftest setf-subseq-ll.3
  (let ((a '()))
    (setf (subseq a 0) '(x y z))
    a)
  nil)

(deftest setf-subseq-ll.4
  (let ((a '(a b c d e f g)))
    (setf (subseq a 2) '(x y z))
    a)
  (a b x y z f g))

(deftest setf-subseq-ll.5
  (let ((a '(a b c d e f g)))
    (setf (subseq a 2 4) '(x y z))
    a)
  (a b x y e f g))

(deftest setf-subseq-ll.6
  (let ((a '(a b c d e f g)))
    (setf (subseq a 2 7) '(x y z))
    a)
  (a b x y z f g))

(deftest setf-subseq-ll.7
  (let ((a '(a b)))
    (setf (subseq a 0) '(x y z))
    a)
  (x y))

(deftest setf-subseq-ls.1
  (let ((a '(a b c d e)))
    (setf (subseq a 0) #(x y z)))
  #(x y z))

(deftest setf-subseq-ls.2
  (let ((a '(a b c d e)))
    (setf (subseq a 0) #(x y z))
    a)
  (x y z d e))

(deftest setf-subseq-ls.3
  (let ((a '()))
    (setf (subseq a 0) #(x y z))
    a)
  nil)

(deftest setf-subseq-ls.4
  (let ((a '(a b c d e f g)))
    (setf (subseq a 2) #(x y z))
    a)
  (a b x y z f g))

(deftest setf-subseq-ls.5
  (let ((a '(a b c d e f g)))
    (setf (subseq a 2 4) #(x y z))
    a)
  (a b x y e f g))

(deftest setf-subseq-ls.6
  (let ((a '(a b c d e f g)))
    (setf (subseq a 2 7) #(x y z))
    a)
  (a b x y z f g))

(deftest setf-subseq-ls.7
  (let ((a '(a b)))
    (setf (subseq a 0) #(x y z))
    a)
  (x y))

(deftest setf-subseq-sl.1
  (let ((a #(a b c d e)))
    (setf (subseq a 0) '(x y z)))
  (x y z))

(deftest setf-subseq-sl.2
  (let ((a #(a b c d e)))
    (setf (subseq a 0) '(x y z))
    a)
  #(x y z d e))

(deftest setf-subseq-sl.3
  (let ((a #()))
    (setf (subseq a 0) '(x y z))
    a)
  #())

(deftest setf-subseq-sl.4
  (let ((a #(a b c d e f g)))
    (setf (subseq a 2) '(x y z))
    a)
  #(a b x y z f g))

(deftest setf-subseq-sl.5
  (let ((a #(a b c d e f g)))
    (setf (subseq a 2 4) '(x y z))
    a)
  #(a b x y e f g))

(deftest setf-subseq-sl.6
  (let ((a #(a b c d e f g)))
    (setf (subseq a 2 7) '(x y z))
    a)
  #(a b x y z f g))

(deftest setf-subseq-sl.7
  (let ((a #(a b)))
    (setf (subseq a 0) '(x y z))
    a)
  #(x y))

(deftest setf-subseq-ss.1
  (let ((a #(a b c d e)))
    (setf (subseq a 0) #(x y z)))
  #(x y z))

(deftest setf-subseq-ss.2
  (let ((a #(a b c d e)))
    (setf (subseq a 0) #(x y z))
    a)
  #(x y z d e))

(deftest setf-subseq-ss.3
  (let ((a #()))
    (setf (subseq a 0) #(x y z))
    a)
  #())

(deftest setf-subseq-ss.4
  (let ((a #(a b c d e f g)))
    (setf (subseq a 2) #(x y z))
    a)
  #(a b x y z f g))

(deftest setf-subseq-ss.5
  (let ((a #(a b c d e f g)))
    (setf (subseq a 2 4) #(x y z))
    a)
  #(a b x y e f g))

(deftest setf-subseq-ss.6
  (let ((a #(a b c d e f g)))
    (setf (subseq a 2 7) #(x y z))
    a)
  #(a b x y z f g))

(deftest setf-subseq-ss.7
  (let ((a #(a b)))
    (setf (subseq a 0) #(x y z))
    a)
  #(x y))


;;  error
(deftest-error setf-subseq-error.1
  (eval '(setf (subseq 10 0) '(a b c))))

(deftest-error! setf-subseq-error.2
  (eval '(let ((x #())) (setf (subseq x) '(a b c)))))

(deftest-error! setf-subseq-error.3
  (eval '(let ((x #())) (setf (subseq x 0 0 0) '(a b c)))))

(deftest-error! setf-subseq-error.4
  (eval '(let ((x #())) (setf (subseq x "A") '(a b c)))))

(deftest-error! setf-subseq-error.5
  (eval '(let ((x #())) (setf (subseq x 0 "B") '(a b c)))))

