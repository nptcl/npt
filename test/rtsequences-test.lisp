;;
;;  ANSI COMMON LISP: 17. Sequences
;;
#|
(deftest copy-seq.1
  (copy-seq nil)
  nil)

(deftest copy-seq.2
  (copy-seq '(10 20 30))
  (10 20 30))

(deftest copy-seq.3
  (copy-seq #(a b c))
  #(a b c))

(deftest copy-seq.4
  (copy-seq #1a(10 20 30))
  #1a(10 20 30))

(deftest copy-seq.5
  (copy-seq #*1100110000)
  #*1100110000)

(deftest copy-seq.6
  (copy-seq "Hello")
  "Hello")

(deftest copy-seq.7
  (let ((a '(10 20 30)))
    (eq (copy-seq '(10 20 30)) a))
  nil)

(deftest elt.1
  (elt '(10 20 30 40 50) 3)
  40)

(deftest elt.2
  (elt '(10 20 30 40 50) 0)
  10)

(deftest elt.3
  (elt '(10 20 30 40 50) 4)
  50)

(deftest-error elt.4
  (elt '(10 20 30 40 50) 5))

(deftest elt.5
  (elt #(a b c d e) 1)
  b)

(deftest elt.6
  (elt #1a(a b c d e) 1)
  b)

(deftest elt.7
  (elt "Hello" 1)
  #\e)

(deftest elt.8
  (elt #*110011011 1)
  1)

(deftest elt.9
  (elt (make-array 10 :element-type 'character
                   :initial-element #\a
                   :fill-pointer 3)
       2)
  #\a)

(deftest-error elt.10
  (elt (make-array 10 :element-type 'character
                   :initial-element #\a
                   :fill-pointer 3)
       3))

(deftest fill.1
  (fill nil 10)
  nil)

(deftest fill.2
  (fill '(a b c) 10)
  (10 10 10))

(deftest fill.3
  (fill '(a b c d e) 9 :start 2)
  (a b 9 9 9))

(deftest fill.4
  (fill '(a b c d e) 9 :start 2 :end 4)
  (a b 9 9 e))

(deftest fill.5
  (let* ((a '(a b c d e))
         (b (fill a 9 :start 2 :end 4)))
    (eq a b))
  t)

(deftest fill.6
  (fill #() 10)
  #())

(deftest fill.7
  (fill #(10 20 30) 'a)
  #(a a a))


(deftest fill.8
  (fill #(a b c d e) 9 :start 2)
  #(a b 9 9 9))

(deftest fill.9
  (fill #(a b c d e) 9 :start 2 :end 4)
  #(a b 9 9 e))

(deftest fill.10
  (let* ((a #(a b c d e))
         (b (fill a 9 :start 2 :end 4)))
    (eq a b))
  t)

(deftest fill.11
  (fill "" #\a)
  "")

(deftest fill.12
  (fill "xyz" #\a)
  "aaa")

(deftest fill.13
  (fill "abcde" #\9 :start 2)
  "ab999")

(deftest fill.14
  (fill "abcde" #\9 :start 2 :end 4)
  "ab99e")

(deftest fill.15
  (let* ((a "abcde")
         (b (fill a #\9 :start 2 :end 4)))
    (eq a b))
  t)

(deftest fill.16
  (fill #1a() 'a)
  #1a())

(deftest fill.17
  (fill #1a(10 20 :hello) 'a)
  #1a(a a a))

(deftest fill.18
  (fill #1a(a b c d e) 9 :start 2)
  #1a(a b 9 9 9))

(deftest fill.19
  (fill #1a(a b c d e) 9 :start 2 :end 4)
  #1a(a b 9 9 e))

(deftest fill.20
  (let* ((a #1a(a b c d e))
         (b (fill a 9 :start 2 :end 4)))
    (eq a b))
  t)

(deftest fill.21
  (fill #* 1)
  #*)

(deftest fill.22
  (fill #*1101 0)
  #*0000)

(deftest fill.23
  (fill #*1101 1)
  #*1111)

(deftest fill.24
  (fill #*110111100010111 0 :start 5)
  #*110110000000000)

(deftest fill.25
  (fill #*110001100001111 1 :start 4 :end 8)
  #*110011110001111)

(deftest fill.26
  (let* ((a #*110111101)
         (b (fill a 1 :start 2 :end 4)))
    (eq a b))
  t)

(deftest make-sequence-list.1
  (make-sequence 'list 0)
  nil)

(deftest make-sequence-list.2
  (make-sequence 'list 5)
  (nil nil nil nil nil))

(deftest make-sequence-list.3
  (make-sequence 'cons 5 :initial-element 'a)
  (a a a a a))

(deftest make-sequence-vector.1
  (make-sequence 'vector 5)
  #(nil nil nil nil nil))

(deftest make-sequence-vector.2
  (make-sequence 'vector 5 :initial-element 'a)
  #(a a a a a))

(deftest make-sequence-vector.3
  (make-sequence '(vector t 5) 5)
  #(nil nil nil nil nil))

(deftest-error make-sequence-vector.4
  (make-sequence '(vector t 6) 5))

(deftest make-sequence-vector.5
  (let ((a (make-sequence '(vector character *) 5)))
    (values (stringp a) (length a)))
  t 5)

(deftest make-sequence-vector.6
  (make-sequence '(vector character *) 5 :initial-element #\a)
  "aaaaa")

(deftest make-sequence-vector.7
  (make-sequence '(vector bit *) 5)
  #*00000)

(deftest make-sequence-vector.8
  (make-sequence '(vector bit *) 5 :initial-element 1)
  #*11111)

(deftest make-sequence-vector.9
  (make-sequence '(vector (unsigned-byte 8) *) 5)
  #(0 0 0 0 0))

(deftest make-sequence-vector.10
  (make-sequence '(vector (unsigned-byte 32) *) 5 :initial-element 1)
  #(1 1 1 1 1))

(deftest make-sequence-vector.11
  (make-sequence '(vector long-float *) 5 :initial-element 1.25l0)
  #(1.25l0 1.25l0 1.25l0 1.25l0 1.25l0))

(deftest make-sequence-simple-vector.1
  (make-sequence 'simple-vector 3 :initial-element nil)
  #(nil nil nil))

(deftest make-sequence-simple-vector.2
  (make-sequence '(simple-vector 3) 3)
  #(nil nil nil))

(deftest-error make-sequence-simple-vector.3
  (make-sequence '(simple-vector 3) 4))

(deftest make-sequence-string.1
  (make-sequence 'string 5 :initial-element #\A)
  "AAAAA")

(deftest make-sequence-string.2
  (make-sequence '(string 5) 5 :initial-element #\A)
  "AAAAA")

(deftest-error make-sequence-string.3
  (make-sequence '(string 5) 4 :initial-element #\A))

(deftest make-sequence-array.1
  (make-sequence 'array 5)
  #(nil nil nil nil nil))

(deftest make-sequence-array.2
  (make-sequence '(array * *) 5)
  #(nil nil nil nil nil))

(deftest make-sequence-array.3
  (make-sequence '(array * 1) 5)
  #(nil nil nil nil nil))

(deftest make-sequence-array.4
  (make-sequence '(array * (*)) 5)
  #(nil nil nil nil nil))

(deftest make-sequence-array.5
  (make-sequence '(array * (5)) 5)
  #(nil nil nil nil nil))

(deftest-error make-sequence-array.6
  (make-sequence '(array * 2) 5))

(deftest-error make-sequence-array.7
  (make-sequence '(array * (* *)) 5))

(deftest-error make-sequence-array.8
  (make-sequence '(array * (4)) 5))

(deftest make-sequence-bitvector.1
  (make-sequence 'bit-vector 5 :initial-element 1)
  #*11111)

(deftest make-sequence-bitvector.2
  (make-sequence '(bit-vector 5) 5 :initial-element 0)
  #*00000)

(deftest-error make-sequence-bitvector.3
  (make-sequence '(bit-vector 5) 4))

(deftest make-sequence-bitvector.4
  (make-sequence 'simple-bit-vector 5 :initial-element 1)
  #*11111)

(deftest subseq-list.1
  (subseq nil 0)
  nil)

(deftest subseq-list.2
  (subseq nil 0 0)
  nil)

(deftest subseq-list.3
  (subseq '(a b c) 0 0)
  nil)

(deftest subseq-list.4
  (subseq '(a b c d e) 3)
  (d e))

(deftest subseq-list.5
  (subseq '(a b c d e) 3 3)
  nil)

(deftest subseq-list.6
  (subseq '(a b c d e) 0 5)
  (a b c d e))

(deftest subseq-list.7
  (subseq '(a b c d e) 1 3)
  (b c))

(deftest subseq-vector.1
  (subseq #() 0)
  #())

(deftest subseq-vector.2
  (subseq #() 0 0)
  #())

(deftest subseq-vector.3
  (subseq #(a b c) 0 0)
  #())

(deftest subseq-vector.4
  (subseq #(a b c d e) 3)
  #(d e))

(deftest subseq-vector.5
  (subseq #(a b c d e) 3 3)
  #())

(deftest subseq-vector.6
  (subseq #(a b c d e) 0 5)
  #(a b c d e))

(deftest subseq-vector.7
  (subseq #(a b c d e) 1 3)
  #(b c))

(deftest subseq-vector.8
  (subseq #(a b c d e) 5 5)
  #())

(deftest subseq-string.1
  (subseq "" 0)
  "")

(deftest subseq-string.2
  (subseq "" 0 0)
  "")

(deftest subseq-string.3
  (subseq "abc" 0 0)
  "")

(deftest subseq-string.4
  (subseq "abcde" 3)
  "de")

(deftest subseq-string.5
  (subseq "abcde" 3 3)
  "")

(deftest subseq-string.6
  (subseq "abcde" 0 5)
  "abcde")

(deftest subseq-string.7
  (subseq "abcde" 1 3)
  "bc")

(deftest subseq-string.8
  (subseq "abcde" 5 5)
  "")

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

(defun arrayspec (&rest args)
  (make-array (length args) :element-type '(unsigned-byte 16) :initial-contents args))

(deftest arrayspec.1
  (arrayspec)
  #())

(deftest arrayspec.2
  (arrayspec 10 20 30)
  #(10 20 30))

(deftest arrayspec.3
  (lisp-system::array-specialized-p
    (arrayspec 10 20 30))
  t)

(deftest subseq-specialized.1
  (subseq (arrayspec) 0)
  #())

(deftest subseq-specialized.2
  (subseq (arrayspec) 0 0)
  #())

(deftest subseq-specialized.3
  (subseq (arrayspec 10 20 30) 0 0)
  #())

(deftest subseq-specialized.4
  (subseq (arrayspec 5 6 7 8 9) 3)
  #(8 9))

(deftest subseq-specialized.5
  (subseq (arrayspec 5 6 7 8 9) 3 3)
  #())

(deftest subseq-specialized.6
  (subseq (arrayspec 5 6 7 8 9) 0 5)
  #(5 6 7 8 9))

(deftest subseq-specialized.7
  (subseq (arrayspec 5 6 7 8 9) 1 3)
  #(6 7))

(deftest subseq-specialized.8
  (subseq (arrayspec 5 6 7 8 9) 5 5)
  #())

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

(deftest map-nil.1
  (let (a)
    (map nil (lambda (x) (push x a)) "abc")
    a)
  (#\c #\b #\a))

(deftest map-nil.2
  (let (a)
    (map nil (lambda (x) (push x a)) "abc"))
  nil)

(deftest map-nil.3
  (let (a)
    (map nil (lambda (x y) (push (list x y) a)) "abc" nil)
    a)
  nil)

(deftest map-nil.4
  (let (a)
    (map nil (lambda (x y) (push (list x y) a)) "abc" '(d e))
    a)
  ((#\b e) (#\a d)))

(deftest map-nil.5
  (let (a)
    (map nil (lambda (x y) (push (list x y) a)) "abc" '(d e f g))
    a)
  ((#\c f) (#\b e) (#\a d)))

(defun map-list-test1 (x)
  (code-char
    (1+ (char-code x))))

(defun map-list-test2 (x y)
  (list (map-list-test1 x)
        (map-list-test1 y)))

(deftest map-list.1
  (map 'list #'map-list-test1 "abc")
  (#\b #\c #\d))

(deftest map-list.2
  (map 'list #'map-list-test2 "abc" nil)
  nil)

(deftest map-list.3
  (map 'list #'map-list-test2 "abc" '(#\d #\e))
  ((#\b #\e) (#\c #\f)))

(deftest map-list.4
  (map 'list #'map-list-test2 "abc" '(#\d #\e #\f #\g))
  ((#\b #\e) (#\c #\f) (#\d #\g)))

(defun map-bit-test1 (x)
  (if (eql x 0) 1 0))

(defun map-bit-test2 (x y)
  (if (or (eql x 1) (eql y 1)) 1 0))

(deftest map-vector.1
  (map '(vector bit) #'map-bit-test1 #*1101)
  #*0010)

(deftest map-vector.2
  (map '(vector bit) #'map-bit-test2 #*1001 #*0101)
  #*1101)

(deftest map-vector.3
  (map '(vector bit) #'map-bit-test2 #*100 #*0101)
  #*110)

(deftest map-vector.4
  (map '(vector bit) #'map-bit-test2 #*10011 '(0 1 0 1))
  #*1101)

(deftest map-vector.5
  (map '(vector character) #'map-list-test1 "abc")
  "bcd")

(deftest map-vector.6
  (map '(vector (signed-byte 8)) #'1+ '(10 20 30))
  #(11 21 31))

(deftest-error map-vector.7
  (map '(vector (signed-byte 8)) #'1+ '(10 20 255)))

(deftest map-vector.8
  (map '(vector (signed-byte 8)) #'+ '(10 20 30) '(3 4 5 6))
  #(13 24 35))

(deftest map-vector.9
  (map '(vector double-float) #'1+ '(10d0 20d0 30d0))
  #(11d0 21d0 31d0))

(deftest map-vector.10
  (map '(vector pathname) #'1+ '(10 20 30))
  #(11 21 31))

(deftest map-simple-vector.1
  (map 'simple-vector #'1+ '(10 20 30))
  #(11 21 31))

(deftest map-simple-vector.2
  (map '(simple-vector 3) #'1+ '(10 20 30))
  #(11 21 31))

(deftest-error map-simple-vector.3
  (map '(simple-vector 4) #'1+ '(10 20 30)))

(deftest map-string.1
  (map 'string #'map-list-test1 "abc")
  "bcd")

(deftest map-string.2
  (map '(string 3) #'map-list-test1 "abc")
  "bcd")

(deftest-error map-string.3
  (map '(string 4) #'map-list-test1 "abc"))

(deftest map-array.1
  (map 'array #'1+ #(10 20 30))
  #(11 21 31))

(deftest map-array.2
  (map '(array character) #'map-list-test1 '(#\a #\b #\c))
  "bcd")

(deftest map-bit-vector.1
  (map 'bit-vector #'map-bit-test1 '(1 0 0 1))
  #*0110)

(deftest map-bit-vector.2
  (map '(bit-vector 4) #'map-bit-test1 '(1 0 0 1))
  #*0110)

(deftest-error map-bit-vector.3
  (map '(bit-vector 5) #'map-bit-test1 '(1 0 0 1)))

(deftest map-into.1
  (let ((a '(10 20 30)))
    (map-into a #'+ '(1 2) '(3 4 5 6 7)))
  (4 6 30))

(deftest map-into.2
  (let ((a '(10 20 30)))
    (map-into a #'+ #(1 2 3 4) '(3 4 5 6 7))
    a)
  (4 6 8))

(deftest map-into.3
  (let ((a #(10 20 30)))
    (map-into a #'+ '(1 2) '(3 4 5 6 7)))
  #(4 6 30))

(deftest map-into.4
  (let ((a #(10 20 30)))
    (map-into a #'+ #(1 2 3 4) '(3 4 5 6 7))
    a)
  #(4 6 8))

(deftest map-into.5
  (let ((a (make-array
             10 :element-type 'character
             :fill-pointer nil :initial-contents "Helloaaaaa")))
    (map-into a #'values "abcd"))
  "abcdoaaaaa")

(deftest map-into.6
  (let ((a (make-array
             10 :element-type 'character
             :fill-pointer t :initial-contents "Helloaaaaa")))
    (map-into a #'values "abcd"))
  "abcd")

(deftest reduce-list.1
  (reduce #'+ nil)
  0)

(deftest reduce-list.2
  (reduce #'+ '(1 2 3 4 5))
  15)

(deftest reduce-list.3
  (reduce #'+ nil)
  0)

(deftest reduce-list.4
  (reduce #'+ nil :initial-value 10 :key #'1+)
  10)

(deftest reduce-list.5
  (reduce #'+ '(20) :initial-value 10 :key #'1+)
  31)

(deftest reduce-list.6
  (reduce #'+ '(10) :key #'1+)
  11)

(deftest reduce-list.7
  (reduce #'+ '(10 20) :key #'1+)
  32)

(deftest reduce-list.8
  (reduce #'+ '(10 20) :key #'1+ :start 1)
  21)

(deftest reduce-list.9
  (reduce #'+ '(1 2 3 4 5) :start 2 :end 2)
  0)

(deftest reduce-list.10
  (reduce #'+ '(1 2 3 4 5) :start 2 :end 3 :key #'1+)
  4)

(deftest reduce-list.11
  (reduce #'+ '(1 2 3 4 5) :start 2 :end 4 :key #'1+)
  9)

(deftest reduce-list.12
  (reduce #'+ '(1 2 3 4 5) :start 2 :end 3 :key #'1+ :initial-value 4)
  8)

(deftest reduce-list.13
  (reduce #'- '(10 1 2 3))
  4)

(deftest reduce-list.14
  (reduce #'- '(10 1 2 3 4 5 6) :from-end t :start 1 :end 4)
  2)

(deftest reduce-vector.1
  (reduce #'+ #())
  0)

(deftest reduce-vector.2
  (reduce #'+ #(1 2 3 4 5))
  15)

(deftest reduce-vector.3
  (reduce #'+ #() :initial-value 10 :key #'1+)
  10)

(deftest reduce-vector.4
  (reduce #'+ #(10) :key #'1+)
  11)

(deftest reduce-vector.5
  (reduce #'+ #(20) :initial-value 10 :key #'1+)
  31)

(deftest reduce-vector.6
  (reduce #'+ #(10 20) :key #'1+)
  32)

(deftest reduce-vector.7
  (reduce #'+ #(20 30) :initial-value 10 :key #'1+)
  62)

(deftest reduce-vector.8
  (reduce #'+ #(10 20) :key #'1+ :start 1)
  21)

(deftest reduce-vector.9
  (reduce #'+ #(1 2 3 4 5) :start 2 :end 2)
  0)

(deftest reduce-vector.10
  (reduce #'+ #(1 2 3 4 5) :start 2 :end 3 :key #'1+)
  4)

(deftest reduce-vector.11
  (reduce #'+ #(1 2 3 4 5) :start 2 :end 4 :key #'1+)
  9)

(deftest reduce-vector.12
  (reduce #'+ #(1 2 3 4 5) :start 2 :end 3 :key #'1+ :initial-value 4)
  8)

(deftest reduce-vector.13
  (reduce #'- #(10 1 2 3))
  4)

(deftest reduce-vector.14
  (reduce #'- #(10 1 2 3 4 5 6) :from-end t :start 1 :end 4)
  2)

(deftest count.1
  (count #\a "aabbccaabbcc")
  4)

(deftest count.2
  (count #\a "aabbccaabbcc" :from-end t)
  4)

(deftest count.3
  (count 10 '(8 8 9 9 9 10 10 10 10 11) :key #'1+)
  3)

(deftest count-if.1
  (count-if #'evenp '(1 2 3 3 3 3 3 3 3 3 3 5 5 5 10 10 1))
  3)

(deftest count-if-not.2
  (count-if-not #'evenp '(1 2 3 3 3 3 3 3 3 3 3 5 5 5 10 10 1))
  14)

(deftest length.1
  (length nil)
  0)

(deftest length.2
  (length #())
  0)

(deftest length.3
  (length '(10 20 30))
  3)

(deftest length.4
  (length #(a b c d e))
  5)

(deftest length.5
  (length (make-array 10 :fill-pointer 5 :initial-element :hello))
  5)

(deftest-error length.6
  (length (make-array '(2 3))))

(deftest reverse-list.1
  (reverse nil)
  nil)

(deftest reverse-list.2
  (reverse '(10 20 30 40))
  (40 30 20 10))

(deftest reverse-list.3
  (let ((a '(10 20 30 40)))
    (reverse a)
    (equal a '(10 20 30 40)))
  t)

(deftest nreverse-list.1
  (nreverse nil)
  nil)

(deftest nreverse-list.2
  (nreverse '(10 20 30 40))
  (40 30 20 10))

(deftest nreverse-list.3
  (let ((a '(10 20 30 40)))
    (nreverse a)
    (equal a '(10 20 30 40)))
  nil)

(deftest reverse-vector.1
  (reverse #())
  #())

(deftest reverse-vector.2
  (reverse #(a))
  #(a))

(deftest reverse-vector.3
  (reverse #(a b c d))
  #(d c b a))

(deftest reverse-vector.4
  (reverse #(a b c d e))
  #(e d c b a))

(deftest reverse-vector.5
  (let ((a #(a b c d e)))
    (reverse a)
    (equalrt a #(a b c d e)))
  t)

(deftest nreverse-vector.1
  (nreverse #())
  #())

(deftest nreverse-vector.2
  (nreverse #(a))
  #(a))

(deftest nreverse-vector.3
  (nreverse #(a b c d))
  #(d c b a))

(deftest nreverse-vector.4
  (nreverse #(a b c d e))
  #(e d c b a))

(deftest nreverse-vector.5
  (let ((a #(a b c d e)))
    (nreverse a)
    (equalrt a #(a b c d e)))
  nil)

(deftest reverse-string.1
  (reverse "")
  "")

(deftest reverse-string.2
  (reverse "a")
  "a")

(deftest reverse-string.3
  (reverse "abcd")
  "dcba")

(deftest reverse-string.4
  (reverse "abcde")
  "edcba")

(deftest reverse-string.5
  (let ((a "abcde"))
    (reverse a)
    (equalrt a "abcde"))
  t)

(deftest nreverse-string.1
  (nreverse "")
  "")

(deftest nreverse-string.2
  (nreverse "a")
  "a")

(deftest nreverse-string.3
  (nreverse "abcd")
  "dcba")

(deftest nreverse-string.4
  (nreverse "abcde")
  "edcba")

(deftest nreverse-string.5
  (let ((a "abcde"))
    (nreverse a)
    (equalrt a "abcde"))
  nil)

(deftest reverse-bitvector.1
  (reverse #*)
  #*)

(deftest reverse-bitvector.2
  (reverse #*1)
  #*1)

(deftest reverse-bitvector.3
  (reverse #*1110)
  #*0111)

(deftest reverse-bitvector.4
  (reverse #*11001)
  #*10011)

(deftest reverse-bitvector.5
  (let ((a #*11001))
    (reverse a)
    (equalrt a #*11001))
  t)

(deftest nreverse-bitvector.1
  (nreverse #*)
  #*)

(deftest nreverse-bitvector.2
  (nreverse #*1)
  #*1)

(deftest nreverse-bitvector.3
  (nreverse #*1110)
  #*0111)

(deftest nreverse-bitvector.4
  (nreverse #*11001)
  #*10011)

(deftest nreverse-bitvector.5
  (let ((a #*11001))
    (nreverse a)
    (equalrt a #*11001))
  nil)

(deftest reverse-array.1
  (reverse #1a())
  #())

(deftest reverse-array.2
  (reverse #1a(a))
  #(a))

(deftest reverse-array.3
  (reverse #1a(a b c d))
  #1a(d c b a))

(deftest reverse-array.4
  (reverse #1a(a b c d e))
  #1a(e d c b a))

(deftest reverse-array.5
  (let ((a #1a(a b c d e)))
    (reverse a)
    (equalrt a #1a(a b c d e)))
  t)

(deftest nreverse-array.1
  (nreverse #1a())
  #1a())

(deftest nreverse-array.2
  (nreverse #1a(a))
  #1a(a))

(deftest nreverse-array.3
  (nreverse #1a(a b c d))
  #1a(d c b a))

(deftest nreverse-array.4
  (nreverse #1a(a b c d e))
  #1a(e d c b a))

(deftest nreverse-array.5
  (let ((a #1a(a b c d e)))
    (nreverse a)
    (equalrt a #1a(a b c d e)))
  nil)

(deftest merge-list.1
  (merge 'list '(1 3 4 6 8) '(2 5 9) #'<)
  (1 2 3 4 5 6 8 9))

(deftest merge-list.2
  (merge 'list '(1 3 4 6 8) #(2 5 9) #'<)
  (1 2 3 4 5 6 8 9))

(deftest merge-list.3
  (merge 'list #(1 3 4 6 8) nil #'<)
  (1 3 4 6 8))

(deftest merge-list.4
  (merge 'list #() '(1 3 4 6 8) #'<)
  (1 3 4 6 8))

(deftest merge-vector.1
  (merge 'vector '(1 3 4 6 8) '(2 5 9) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest merge-vector.2
  (merge 'vector '(1 3 4 6 8) #(2 5 9) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest merge-vector.3
  (merge 'vector #(1 3 4 6 8) nil #'<)
  #(1 3 4 6 8))

(deftest merge-vector.4
  (merge 'vector #() '(1 3 4 6 8) #'<)
  #(1 3 4 6 8))

(deftest merge-simple-vector.1
  (merge '(simple-vector 8) #1a(1 3 4 6 8) '(2 5 9) #'<)
  #(1 2 3 4 5 6 8 9))

(deftest merge-string.1
  (merge 'string '(#\1 #\3 #\4 #\6 #\8) "259" #'char<)
  "12345689")

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

(deftest find.1
  (find #\c nil)
  nil)

(deftest find.2
  (find #\c #())
  nil)

(deftest find.3
  (find #\c "abcdefg")
  #\c)

(deftest find.4
  (find #\z "abcdefg")
  nil)

(deftest find.5
  (find 20 '(10 20 30 40))
  20)

(deftest find.6
  (find 100 '(10 20 30 40))
  nil)

(deftest find.7
  (find 31 '(10 20 30 40) :key #'1+)
  30)

(deftest find.8
  (find '(a) #((z) (a a) (b) (c) (a)) :test #'equal)
  (a))

(deftest find.9
  (find '(a) #((z) (a a) (b) (c) (a)) :test-not #'equal)
  (z))

(deftest find.10
  (find 20 '(10 20 30 40) :from-end t)
  20)

(deftest find.11
  (find 20 #(10 20 30 40) :from-end t)
  20)

(deftest find-if.1
  (find-if (lambda (x) (equal x "aa")) '("zz" "aaa" "bb" "aa" "cc"))
  "aa")

(deftest find-if.2
  (find-if (lambda (x) (equal x "ba")) '("zz" "aaa" "bb" "aa" "cc"))
  nil)

(deftest find-if.3
  (find-if (lambda (x) (equal x "aa")) #("zz" "aaa" "bb" "aa" "cc"))
  "aa")

(deftest find-if.4
  (find-if (lambda (x) (equal x "ba")) #("zz" "aaa" "bb" "aa" "cc"))
  nil)

(deftest find-if-not.1
  (find-if-not (lambda (x) (equal x "zz")) '("zz" "aaa" "bb" "aa" "cc"))
  "aaa")

(deftest find-if-not.2
  (find-if-not (constantly t) '("zz" "aaa" "bb" "aa" "cc"))
  nil)

(deftest find-if-not.3
  (find-if-not (lambda (x) (equal x "zz")) #("zz" "aaa" "bb" "aa" "cc"))
  "aaa")

(deftest find-if-not.4
  (find-if-not (constantly t) #("zz" "aaa" "bb" "aa" "cc"))
  nil)

(deftest position.1
  (position 10 nil)
  nil)

(deftest position.2
  (position 10 #())
  nil)

(deftest position.3
  (position 'a '(a b c d e))
  0)

(deftest position.4
  (position 'd '(a b c d e))
  3)

(deftest position.5
  (position 'a #(a b c d e))
  0)

(deftest position.6
  (position 'd #(a b c d e))
  3)

(deftest position-if.1
  (position-if #'evenp '(1 3 5 4 9))
  3)

(deftest position-if.2
  (position-if-not #'oddp #(1 3 5 4 9))
  3)

(deftest search-list.1
  (search nil '(a b c d))
  0)

(deftest search-list.2
  (search '(a) '(a b c d))
  0)

(deftest search-list.3
  (search '(b) '(a b c d))
  1)

(deftest search-list.4
  (search '(z) '(a b c d))
  nil)

(deftest search-list.5
  (search '(a b c d e) '(a b c d))
  nil)

(deftest search-list.6
  (search '(b c) '(a b c d))
  1)

(deftest search-list.7
  (search '(b d) '(a b c d))
  nil)

(deftest search-vector.1
  (search "" "abcd")
  0)

(deftest search-vector.2
  (search "a" "abcd")
  0)

(deftest search-vector.3
  (search "b" "abcd")
  1)

(deftest search-vector.4
  (search "z" "abcd")
  nil)

(deftest search-vector.5
  (search "abcde" "abcd")
  nil)

(deftest search-vector.6
  (search "bc" "abcd")
  1)

(deftest search-vector.7
  (search "bd" "abcd")
  nil)
|#

(deftest search-list-end.1
  (search nil '(a b c d) :from-end t)
  0)

(deftest search-list-end.2
  (search '(a) '(a b c d) :from-end t)
  0)

(deftest search-list-end.3
  (search '(b) '(a b c d) :from-end t)
  1)

(deftest search-list-end.4
  (search '(z) '(a b c d) :from-end t)
  nil)

(deftest search-list-end.5
  (search '(a b c d e) '(a b c d) :from-end t)
  nil)

(deftest search-list-end.6
  (search '(b c) '(a b c d) :from-end t)
  1)

(deftest search-list-end.7
  (search '(b d) '(a b c d) :from-end t)
  nil)

(deftest search-vector-end.1
  (search "" "abcd" :from-end t)
  0)

(deftest search-vector-end.2
  (search "a" "abcd" :from-end t)
  0)

(deftest search-vector-end.3
  (search "b" "abcd" :from-end t)
  1)

(deftest search-vector-end.4
  (search "z" "abcd" :from-end t)
  nil)

(deftest search-vector-end.5
  (search "abcde" "abcd" :from-end t)
  nil)

(deftest search-vector-end.6
  (search "bc" "abcd" :from-end t)
  1)

(deftest search-vector-end.7
  (search "bd" "abcd" :from-end t)
  nil)

#|
(deftest mismatch.1
  (mismatch "abcd" "ABCDE" :test #'char-equal)
  4)

(deftest mismatch.2
  (mismatch '(3 2 1 1 2 3) '(1 2 3) :from-end t)
  3)

(deftest mismatch.3
  (mismatch '(1 2 3) '(2 3 4) :test-not #'eq :key #'oddp)
  nil)

(deftest mismatch.4
  (mismatch '(1 2 3 4 5 6) '(3 4 5 6 7) :start1 2 :end2 4)
  nil)

(deftest replace.1
  (replace "abcdefghij" "0123456789" :start1 4 :end1 7 :start2 4)
  "abcd456hij")

(deftest replace.2
  (replace '(a b c d e f g h i j) "0123456789" :start1 4 :end1 7 :start2 4)
  (a b c d #\4 #\5 #\6 h i j))

(deftest replace.3
  (replace "abcdefghij" '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           :start1 4 :end1 7 :start2 4)
  "abcd456hij")

(deftest replace.4
  (replace '(a b c d e f g h i j) '(0 1 2 3 4 5 6 7 8 9) :start1 4 :end1 7 :start2 4)
  (a b c d 4 5 6 h i j))

(deftest concatenate.1
  (concatenate 'string "all" " " "together" " " "now")
  "all together now")

(deftest concatenate.2
  (concatenate 'list "ABC" '(d e f) #(1 2 3) #*1011)
  (#\A #\B #\C D E F 1 2 3 1 0 1 1))

(deftest concatenate.3
  (concatenate 'list)
  nil)

(deftest-error concatenate.4
  (concatenate '(vector * 2) "a" "bc"))
|#

