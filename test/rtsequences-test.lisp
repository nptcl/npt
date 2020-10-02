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


;;
;;  Function REDUCE
;;
(deftest reduce-list.1
  (reduce #'+ nil)
  0)

(deftest reduce-list.2
  (reduce #'+ '(1 2 3 4 5))
  15)

(deftest reduce-list.3
  (reduce #'+ nil :initial-value 10 :key #'1+)
  10)

(deftest reduce-list.4
  (reduce #'+ '(20) :initial-value 10 :key #'1+)
  31)

(deftest reduce-list.5
  (reduce #'+ '(20) :initial-value 10 :key nil)
  30)

(deftest reduce-list.6
  (reduce #'+ '(1 2 3 4 5) :initial-value 1000)
  1015)

(deftest reduce-list.7
  (reduce #'+ '(10) :key #'1+)
  11)

(deftest reduce-list.8
  (reduce #'+ '(1 2 3 4 5) :key #'1+)
  20)

(deftest reduce-list.9
  (reduce #'+ '(1 2 3 4 5) :key #'1+ :start 1)
  18)

(deftest reduce-list.10
  (reduce #'+ '(1 2 3 4 5) :key #'1+ :start 5)
  0)

(deftest-error reduce-list.11
  (reduce #'+ '(1 2 3 4 5) :key #'1+ :start 6))

(deftest reduce-list.12
  (reduce #'+ '(1 2 3 4 5) :start 2 :end 2)
  0)

(deftest reduce-list.13
  (reduce #'+ '(1 2 3 4 5) :start 2 :end 3 :key #'1+)
  4)
(deftest reduce-list.14
  (reduce #'+ '(1 2 3 4 5) :start 2 :end 4 :key #'1+)
  9)

(deftest reduce-list.15
  (reduce #'+ '(1 2 3 4 5) :start 2 :end 5 :key #'1+)
  15)

(deftest reduce-list.16
  (reduce #'+ '(1 2 3 4 5) :start 2 :end nil :key #'1+)
  15)

(deftest-error reduce-list.17
  (reduce #'+ '(1 2 3 4 5) :start 2 :end 6 :key #'1+))

(deftest-error reduce-list.18
  (reduce #'+ '(1 2 3 4 5) :start 2 :end 1 :key #'1+))

(deftest reduce-list.19
  (reduce #'+ '(1 2 3 4 5) :start 2 :end 4 :key #'1+ :initial-value 4)
  13)

(deftest reduce-list.20
  (reduce #'- '(10 1 2 3))
  4)

(deftest reduce-list.21
  (reduce #'- '(10 1 2 3 4 5 6) :from-end t :start 1 :end 4)
  2)

(deftest reduce-list.22
  (reduce #'- '(10 1 2 3 4 5 6) :from-end t)
  13)

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
  (reduce #'+ #(20) :initial-value 10 :key #'1+)
  31)

(deftest reduce-vector.5
  (reduce #'+ #(20) :initial-value 10 :key nil)
  30)

(deftest reduce-vector.6
  (reduce #'+ #(1 2 3 4 5) :initial-value 1000)
  1015)

(deftest reduce-vector.7
  (reduce #'+ #(10) :key #'1+)
  11)

(deftest reduce-vector.8
  (reduce #'+ #(1 2 3 4 5) :key #'1+)
  20)

(deftest reduce-vector.9
  (reduce #'+ #(1 2 3 4 5) :key #'1+ :start 1)
  18)

(deftest reduce-vector.10
  (reduce #'+ #(1 2 3 4 5) :key #'1+ :start 5)
  0)

(deftest-error reduce-vector.11
  (reduce #'+ #(1 2 3 4 5) :key #'1+ :start 6))

(deftest reduce-vector.12
  (reduce #'+ #(1 2 3 4 5) :start 2 :end 2)
  0)

(deftest reduce-vector.13
  (reduce #'+ #(1 2 3 4 5) :start 2 :end 3 :key #'1+)
  4)
(deftest reduce-vector.14
  (reduce #'+ #(1 2 3 4 5) :start 2 :end 4 :key #'1+)
  9)

(deftest reduce-vector.15
  (reduce #'+ #(1 2 3 4 5) :start 2 :end 5 :key #'1+)
  15)

(deftest reduce-vector.16
  (reduce #'+ #(1 2 3 4 5) :start 2 :end nil :key #'1+)
  15)

(deftest-error reduce-vector.17
  (reduce #'+ #(1 2 3 4 5) :start 2 :end 6 :key #'1+))

(deftest-error reduce-vector.18
  (reduce #'+ #(1 2 3 4 5) :start 2 :end 1 :key #'1+))

(deftest reduce-vector.19
  (reduce #'+ #(1 2 3 4 5) :start 2 :end 4 :key #'1+ :initial-value 4)
  13)

(deftest reduce-vector.20
  (reduce #'- #(10 1 2 3))
  4)

(deftest reduce-vector.21
  (reduce #'- #(10 1 2 3 4 5 6) :from-end t :start 1 :end 4)
  2)

(deftest reduce-vector.22
  (reduce #'- #(10 1 2 3 4 5 6) :from-end t)
  13)

(deftest-error reduce-error.1
  (eval '(reduce 10 nil))
  type-error)

(deftest-error reduce-error.2
  (eval '(reduce #'+ 20)))

(deftest-error! reduce-error.3
  (eval '(reduce #'+)))

(deftest-error! reduce-error.4
  (eval '(reduce #'+ nil :key)))

(deftest-error reduce-error.5
  (eval '(reduce #'+ nil :hello 10)))

(deftest-error reduce-error.6
  (eval '(reduce #'+ nil :start 10)))

(deftest-error reduce-error.7
  (eval '(reduce #'+ nil :key 10)))


;;
;;  Function LENGTH
;;
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

(deftest length.6
  (length "ABC")
  3)

(deftest length.7
  (length #*1101000011)
  10)

(deftest length.8
  (length (make-array 10 :fill-pointer 5))
  5)

(deftest-error length-error.1
  (length (make-array '(2 3)))
  type-error)

(deftest-error length-error.2
  (eval '(length 10))
  type-error)

(deftest-error! length-error.3
  (eval '(length)))

(deftest-error! length-error.4
  (eval '(length nil nil)))

;; ANSI Common Lisp
(deftest length-test.1
  (length "abc")
  3)

(defparameter *length-array* (make-array '(3) :element-type 'character
                                         :initial-contents "abc"
                                         :fill-pointer t))

(deftest length-test.2
  (length *length-array*)
  3)

(deftest length-test.3
  (progn
    (setf (fill-pointer *length-array*) 2)
    (length *length-array*))
  2)



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

