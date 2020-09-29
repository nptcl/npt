;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function COPY-SEQ
;;
(deftest copy-seq.1
  (copy-seq nil)
  nil)

(deftest copy-seq.2
  (copy-seq '(10 20 30))
  (10 20 30))

(deftest copy-seq.3
  (let ((x '(10 20 30)))
    (eq x (copy-seq '(10 20 30))))
  nil)

(deftest copy-seq.4
  (copy-seq #(a b c))
  #(a b c))

(deftest copy-seq.5
  (let ((x #(a b c)))
    (eq x (copy-seq x)))
  nil)

(deftest copy-seq.6
  (copy-seq #1a(10 20 30))
  #1a(10 20 30))

(deftest copy-seq.7
  (let ((x #1a(10 20 30)))
    (eq x #1a(10 20 30)))
  nil)

(deftest copy-seq.8
  (copy-seq #*1100110000)
  #*1100110000)

(deftest copy-seq.9
  (let ((x #*1100110000))
    (eq x (copy-seq #*1100110000)))
  nil)

(deftest copy-seq.10
  (let ((x "Hello"))
    (eq x (copy-seq "Hello")))
  nil)

(deftest copy-seq.11
  (let ((str "a string"))
    (values
      (equalp str (copy-seq str))
      (eql str (copy-seq str))))
  t nil)

(deftest-error copy-seq-error.1
  (eval '(copy-seq 10))
  type-error)

(deftest-error! copy-seq-error.2
  (eval '(copy-seq)))

(deftest-error! copy-seq-error.3
  (eval '(copy-seq nil nil)))


;;
;;  Accessor ELT
;;
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

(deftest-error elt.11
  (elt #2a((a b c) (d e f)) 0))

(deftest-error elt-error.1
  (eval '(elt 10 20))
  type-error)

(deftest-error elt-error.2
  (eval '(elt nil "hello"))
  type-error)

(deftest-error! elt-error.3
  (eval '(elt nil)))

(deftest-error! elt-error.4
  (eval '(elt nil 3 4)))


;;
;;  Accessor (SETF ELT)
;;
(deftest setf-elt.1
  (let ((x '(10 20 30 40 50)))
    (values
      (setf (elt x 3) 999)
      x))
  999 (10 20 30 999 50))

(deftest setf-elt.2
  (let ((x '(10 20 30 40 50)))
    (values
      (setf (elt x 0) 999)
      x))
  999 (999 20 30 40 50))

(deftest setf-elt.3
  (let ((x '(10 20 30 40 50)))
    (values
      (setf (elt x 4) 999)
      x))
  999 (10 20 30 40 999))

(deftest-error setf-elt.4
  (let ((x '(10 20 30 40 50)))
    (setf (elt x 5) 999)))

(deftest setf-elt.5
  (let ((x #(a b c d e)))
    (values
      (setf (elt x 1) 'z)
      x))
  z #(a z c d e))

(deftest setf-elt.6
  (let ((x #1a(a b c d e)))
    (values
      (setf (elt x 1) 'z)
      x))
  z #(a z c d e))

(deftest setf-elt.7
  (let ((x "Hello"))
    (values
      (setf (elt x 1) #\a)
      x))
  #\a "Hallo")

(deftest setf-elt.8
  (let ((x #*110011011))
    (values
      (setf (elt x 1) 0)
      x))
  0 #*100011011)

(deftest setf-elt.9
  (let ((x (make-array 10 :element-type 'character
                       :initial-element #\a
                       :fill-pointer 3)))
    (values
      (setf (elt x 2) #\z)
      x))
  #\z "aaz")

(deftest-error setf-elt.10
  (let ((x (make-array 10 :element-type 'character
                       :initial-element #\a
                       :fill-pointer 3)))
    (setf (elt x 3) #\z)))

(deftest-error setf-elt.11
  (setf (elt #2a((a b c) (d e f)) 0) 100))

(deftest-error setf-elt-error.1
  (eval '(setf (elt 10 20) nil))
  type-error)

(deftest-error setf-elt-error.2
  (eval '(setf (elt nil "hello") nil))
  type-error)

(deftest-error! setf-elt-error.3
  (eval '(setf (elt nil) nil)))

(deftest-error! setf-elt-error.4
  (eval '(setf (elt nil 3 4) nil)))

;; ANSI Common Lisp
(deftest elt-test.1
  (let ((str (copy-seq "0123456789")))
    (values
      (elt str 6)
      (setf (elt str 0) #\#)
      str))
  #\6 #\# "#123456789")


;;
;;  Function FILL
;;

;; list
(deftest fill-list.1
  (fill nil 10)
  nil)

(deftest fill-list.2
  (fill '(a b c) 10)
  (10 10 10))

(deftest fill-list.3
  (fill '(a b c d e) 9 :start 2)
  (a b 9 9 9))

(deftest fill-list.4
  (fill '(a b c d e) 9 :start 2 :end 4)
  (a b 9 9 e))

(deftest fill-list.5
  (let* ((a '(a b c d e))
         (b (fill a 9 :start 2 :end 4)))
    (eq a b))
  t)

(deftest-error fill-list.6
  (fill '(a b c d e) 9 :start 6))

(deftest-error fill-list.7
  (fill '(a b c d e) 9 :end 6))

(deftest-error fill-list.8
  (fill '(a b c d e) 9 :start 2 :end 1))

(deftest fill-list.9
  (fill '(a b c d e) 9 :start 2 :end 2)
  (a b c d e))

(deftest fill-list.10
  (fill '(a b c d e) 9 :start 2 :end nil)
  (a b 9 9 9))

;; vector
(deftest fill-vector.1
  (fill #() 10)
  #())

(deftest fill-vector.2
  (fill #(10 20 30) 'a)
  #(a a a))


(deftest fill-vector.3
  (fill #(a b c d e) 9 :start 2)
  #(a b 9 9 9))

(deftest fill-vector.4
  (fill #(a b c d e) 9 :start 2 :end 4)
  #(a b 9 9 e))

(deftest fill-vector.5
  (let* ((a #(a b c d e))
         (b (fill a 9 :start 2 :end 4)))
    (eq a b))
  t)

(deftest-error fill-vector.6
  (fill #(a b c d e) 9 :start 6))

(deftest-error fill-vector.7
  (fill #(a b c d e) 9 :end 6))

(deftest-error fill-vector.8
  (fill #(a b c d e) 9 :start 2 :end 1))

(deftest fill-vector.9
  (fill #(a b c d e) 9 :start 2 :end 2)
  #(a b c d e))

(deftest fill-vector.10
  (fill #(a b c d e) 9 :start 2 :end nil)
  #(a b 9 9 9))


;; string
(deftest fill-string.1
  (fill "" #\a)
  "")

(deftest fill-string.2
  (fill "xyz" #\a)
  "aaa")

(deftest fill-string.3
  (fill "abcde" #\9 :start 2)
  "ab999")

(deftest fill-string.4
  (fill "abcde" #\9 :start 2 :end 4)
  "ab99e")

(deftest fill-string.5
  (let* ((a "abcde")
         (b (fill a #\9 :start 2 :end 4)))
    (eq a b))
  t)

(deftest-error fill-string.6
  (fill "abcde" #\Z :start 6))

(deftest-error fill-string.7
  (fill "abcde" #\Z :end 6))

(deftest-error fill-string.8
  (fill "abcde" #\Z :start 2 :end 1))

(deftest fill-string.9
  (fill "abcde" #\Z :start 2 :end 2)
  "abcde")

(deftest fill-string.10
  (fill "abcde" #\Z :start 2 :end nil)
  "abZZZ")


;; simple array
(deftest fill-array.1
  (fill #1a() 'a)
  #1a())

(deftest fill-array.2
  (fill #1a(10 20 :hello) 'a)
  #1a(a a a))

(deftest fill-array.3
  (fill #1a(a b c d e) 9 :start 2)
  #1a(a b 9 9 9))

(deftest fill-array.4
  (fill #1a(a b c d e) 9 :start 2 :end 4)
  #1a(a b 9 9 e))

(deftest fill-array.5
  (let* ((a #1a(a b c d e))
         (b (fill a 9 :start 2 :end 4)))
    (eq a b))
  t)

(deftest-error fill-array.6
  (fill #1a(a b c d e) 9 :start 6))

(deftest-error fill-array.7
  (fill #1a(a b c d e) 9 :end 6))

(deftest-error fill-array.8
  (fill #1a(a b c d e) 9 :start 2 :end 1))

(deftest fill-array.9
  (fill #1a(a b c d e) 9 :start 2 :end 2)
  #1a(a b c d e))

(deftest fill-array.10
  (fill #1a(a b c d e) 9 :start 2 :end nil)
  #1a(a b 9 9 9))


;; bit-vector
(deftest fill-bit-vector.1
  (fill #* 1)
  #*)

(deftest fill-bit-vector.2
  (fill #*1101 0)
  #*0000)

(deftest fill-bit-vector.3
  (fill #*1101 1)
  #*1111)

(deftest fill-bit-vector.4
  (fill #*110111100010111 0 :start 5)
  #*110110000000000)

(deftest fill-bit-vector.5
  (fill #*110001100001111 1 :start 4 :end 8)
  #*110011110001111)

(deftest fill-bit-vector.6
  (let* ((a #*110111101)
         (b (fill a 1 :start 2 :end 4)))
    (eq a b))
  t)

(deftest-error fill-bit-vector.7
  (fill #*11011 1 :start 6))

(deftest-error fill-bit-vector.8
  (fill #*11011 1 :end 6))

(deftest-error fill-bit-vector.9
  (fill #*11011 1 :start 2 :end 1))

(deftest fill-bit-vector.10
  (fill #*11011 1 :start 2 :end 2)
  #*11011)

(deftest fill-bit-vector.11
  (fill #*11011 1 :start 2 :end nil)
  #*11111)


;; error
(deftest-error fill-error.1
  (eval '(fill 10 20))
  type-error)

(deftest-error! fill-error.2
  (eval '(fill nil)))

(deftest-error! fill-error.3
  (eval '(fill nil 10 nil)))

(deftest-error fill-error.4
  (eval '(fill nil 10 :start)))

(deftest-error fill-error.5
  (eval '(fill nil 10 :start t)))

(deftest-error fill-error.6
  (eval '(fill nil 10 :hello t)))


;; ANSI Common Lisp
(deftest fill-test.1
  (fill (list 0 1 2 3 4 5) '(444))
  ((444) (444) (444) (444) (444) (444)))

(deftest fill-test.2
  (fill (copy-seq "01234") #\e :start 3)
  "012ee")

(deftest fill-test.3
  (let ((x (vector 'a 'b 'c 'd 'e)))
    (values
      (fill x 'z :start 1 :end 3)
      x))
  #(a z z d e)
  #(a z z d e))

(deftest fill-test.4
  (let ((x (vector 'a 'b 'c 'd 'e)))
    (fill x 'z :start 1 :end 3)
    x
    (values
      (fill x 'p)
      x))
  #(p p p p p)
  #(p p p p p))



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

