;;
;;  ANSI COMMON LISP: 15. Arrays
;;

;;
;;  adjust-array
;;
(deftest adjust-array.1
  (arrayp
    (adjust-array (make-array 3) 10))
  t)

(deftest adjust-array.2
  (arrayp
    (adjust-array (make-array 3 :adjustable t) 10))
  t)


;;
;;  dimension
;;
(deftest adjust-array-dimension.1
  (array-dimensions
    (adjust-array
      (make-array '(3 4))
      '(5 6)))
  (5 6))

(deftest-error adjust-array-dimension.2
  (array-dimensions
    (adjust-array
      (make-array '(3 4))
      '(5 6 7))))

(deftest-error adjust-array-dimension.3
  (array-dimensions
    (adjust-array
      (make-array '(3 4))
      '(5))))


;;
;;  move
;;
(deftest adjust-array-move.1
  (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5)))
         (y (adjust-array x 3)))
    y)
  #(1 2 3))

(deftest adjust-array-move.2
  (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5)))
         (y (adjust-array x 10)))
    (values (length y)
            (subseq y 0 5)))
  10 #(1 2 3 4 5))

(deftest adjust-array-move.3
  (let* ((x (make-array '(4 3) :initial-contents
                        '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
         (y (adjust-array x '(3 2))))
    y)
  #2a((1 2) (4 5) (7 8)))

(deftest adjust-array-move.4
  (let* ((x (make-array '(4 3) :initial-contents
                        '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
         (y (adjust-array x '(5 4))))
    (values (array-dimensions y)
            (aref y 0 0)
            (aref y 1 0)
            (aref y 0 1)
            (aref y 1 1)))
  (5 4) 1 4 2 5)


;;
;;  initial-element
;;
(deftest adjust-array-initial-element.1
  (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5)))
         (y (adjust-array x 10 :initial-element 999)))
    y)
  #(1 2 3 4 5 999 999 999 999 999))

(deftest adjust-array-initial-element.2
  (let* ((x (make-array '(4 3) :initial-contents
                        '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
         (y (adjust-array x '(5 4) :initial-element 888)))
    y)
  #2a((1 2 3 888) (4 5 6 888) (7 8 9 888) (10 11 12 888) (888 888 888 888)))


;;
;;  element-type
;;
(deftest adjust-array-element-type.1
  (let* ((x (make-array 5 :element-type 'character :initial-contents "Hello"))
         (y (adjust-array x 3)))
    (values (stringp y) (array-element-type y) y))
  t character #(#\H #\e #\l))

(deftest-error adjust-array-element-type.2
  (let* ((x (make-array 5 :element-type 'character :initial-contents "Hello"))
         (y (adjust-array x 3 :element-type t)))
    y))

(deftest adjust-array-element-type.3
  (let* ((x (make-array 5 :element-type 'bit :initial-contents '(1 0 0 1 1)))
         (y (adjust-array x 3)))
    (values (array-element-type y) y))
  bit #*100)

(deftest adjust-array-element-type.4
  (let* ((x (make-array 5 :element-type '(signed-byte 16)
                        :initial-contents '(10 20 30 40 50)))
         (y (adjust-array x 3)))
    (values (array-element-type y) y))
  (signed-byte 16) #(10 20 30))

(deftest adjust-array-element-type.5
  (let* ((x (make-array 5 :element-type '(unsigned-byte 32)
                        :initial-contents '(10 20 30 40 50)))
         (y (adjust-array x 3)))
    (values (array-element-type y) y))
  (unsigned-byte 32) #(10 20 30))

(deftest adjust-array-element-type.6
  (let* ((x (make-array 5 :element-type 'double-float
                        :initial-contents '(1.2d0 3.4d0 5.6d0 7.8d0 9.0d0)))
         (y (adjust-array x 3)))
    (values (array-element-type y) y))
  double-float #(1.2d0 3.4d0 5.6d0))

(deftest adjust-array-element-type.7
  (let* ((x (make-array 5 :element-type 'double-float
                        :initial-contents '(1.2d0 3.4d0 5.6d0 7.8d0 9.0d0)))
         (y (adjust-array x 3 :element-type 'double-float)))
    (values (array-element-type y) y))
  double-float #(1.2d0 3.4d0 5.6d0))

(deftest-error adjust-array-element-type.8
  (let* ((x (make-array 5 :element-type 'double-float
                        :initial-contents '(1.2d0 3.4d0 5.6d0 7.8d0 9.0d0)))
         (y (adjust-array x 3 :element-type 'single-float)))
    y))


;;
;;  initial-contents
;;
(deftest adjust-array-initial-contents.1
  (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5)))
         (y (adjust-array x 3 :initial-contents '(6 7 8))))
    (values (eq x y) y))
  nil #(6 7 8))

(deftest adjust-array-initial-contents.2
  (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5) :adjustable t))
         (y (adjust-array x 3 :initial-contents '(6 7 8))))
    (values (eq x y) y))
  t #(6 7 8))



;;
;;  fill-pointer
;;
(deftest adjust-array-fill-pointer.1
  (fill-pointer
    (adjust-array
      (make-array 6 :fill-pointer 4)
      10))
  4)

(deftest adjust-array-fill-pointer.2
  (fill-pointer
    (adjust-array
      (make-array 6 :fill-pointer 4)
      10 :fill-pointer nil))
  4)

(deftest adjust-array-fill-pointer.3
  (fill-pointer
    (adjust-array
      (make-array 6 :fill-pointer 4)
      10 :fill-pointer 5))
  5)

(deftest adjust-array-fill-pointer.4
  (fill-pointer
    (adjust-array
      (make-array 6 :fill-pointer 4)
      4))
  4)

(deftest-error adjust-array-fill-pointer.5
  (fill-pointer
    (adjust-array
      (make-array 6 :fill-pointer 4)
      3)))

(deftest adjust-array-fill-pointer.6
  (fill-pointer
    (adjust-array
      (make-array 6 :fill-pointer 4)
      3 :fill-pointer t))
  3)

(deftest adjust-array-fill-pointer.7
  (fill-pointer
    (adjust-array
      (make-array 6 :fill-pointer 4)
      3 :fill-pointer 3))
  3)

(deftest-error adjust-array-fill-pointer.8
  (fill-pointer
    (adjust-array
      (make-array 6 :fill-pointer 4)
      3 :fill-pointer 4)))

(deftest-error adjust-array-fill-pointer.9
  (fill-pointer
    (adjust-array
      (make-array 6)
      3 :fill-pointer t)))


;;
;;  adjustable
;;
(deftest adjust-array-adjustable.1
  (let* ((x (make-array 3 :adjustable nil))
         (y (adjust-array x 5)))
    (values
      (array-dimensions y)
      (eq x y)))
  (5) nil)

(deftest adjust-array-adjustable.2
  (let* ((x (make-array 3 :adjustable t))
         (y (adjust-array x 5)))
    (values
      (array-dimensions y)
      (eq x y)))
  (5) t)

(deftest adjust-array-adjustable.3
  (let* ((x (make-array 3 :adjustable t))
         (y (adjust-array x 5)))
    (adjustable-array-p y))
  t)

(deftest adjust-array-adjustable.4
  (let* ((x (make-array 3 :adjustable nil))
         (y (adjust-array x 5)))
    (adjustable-array-p y))
  nil)


;;
;;  displaced-to
;;
(deftest adjust-array-displaced-to.1
  (adjust-array
    #2a(( alpha     beta      gamma     delta )
        ( epsilon   zeta      eta       theta )
        ( iota      kappa     lambda    mu    )
        ( nu        xi        omicron   pi    ))
    '(3 5)
    :initial-element 'baz)
  #2a(( alpha     beta      gamma     delta     baz )
      ( epsilon   zeta      eta       theta     baz )
      ( iota      kappa     lambda    mu        baz )))

(deftest adjust-array-displaced-to.2
  (adjust-array
    #2a(( alpha     beta      gamma     delta )
        ( epsilon   zeta      eta       theta )
        ( iota      kappa     lambda    mu    )
        ( nu        xi        omicron   pi    ))
    '(3 5)
    :initial-contents '((1 2 3 4 5) (6 7 8 9 0) (a b c d e)))
  #2a((1 2 3 4 5) (6 7 8 9 0) (a b c d e)))

(deftest adjust-array-displaced-to.3
  (adjust-array
    #2a(( alpha     beta      gamma     delta )
        ( epsilon   zeta      eta       theta )
        ( iota      kappa     lambda    mu    )
        ( nu        xi        omicron   pi    ))
    '(3 5)
    :displaced-to #(1 2 3 4 5 6 7 8 9 0 a b c d e))
  #2a((1 2 3 4 5) (6 7 8 9 0) (a b c d e)))

(deftest adjust-array-displaced-to.4
  (let* ((x #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
         (y (make-array 5 :displaced-to x :displaced-index-offset 5)))
    (adjust-array y 6 :displaced-to x))
  #(1 2 3 4 5 6))

(deftest adjust-array-displaced-to.5
  (let* ((x #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
         (y (make-array 5 :displaced-to x :displaced-index-offset 5)))
    (adjust-array y 4 :displaced-to x :displaced-index-offset 3))
  #(4 5 6 7))

(deftest adjust-array-displaced-to.6
  (let* ((x #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
         (y (make-array 5 :displaced-to x)))
    (adjust-array y 4 :displaced-to x :displaced-index-offset 3))
  #(4 5 6 7))

(deftest adjust-array-displaced-to.7
  (let* ((x #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
         (y (make-array 5 :displaced-to x)))
    (adjust-array y 4))
  #(1 2 3 4))

(deftest adjust-array-displaced-to.8
  (let* ((x #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
         (y (make-array 5 :displaced-to x)))
    (setq y (adjust-array y 4))
    (setf (aref y 0) 999)
    (values x y))
  #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
  #(999 2 3 4))


;;
;;  vector
;;
(deftest adjust-array-vector.1
  (let ((x (adjust-array #() 3)))
    (values
      (vectorp x)
      (length x)
      (typep x 'simple-vector)))
  t 3 t)

(deftest adjust-array-vector.2
  (let ((x (adjust-array #(10 20 30 40) 3)))
    (values
      (vectorp x)
      (length x)
      (typep x 'simple-vector)))
  t 3 t)

(deftest-error adjust-array-vector.3
  (adjust-array #(10 20 30 40) nil))

(deftest-error adjust-array-vector.4
  (adjust-array #(10 20 30 40) '(3 4)))

(deftest adjust-array-vector.5
  (adjust-array #(10 20 30 40) 3 :initial-element 'hello)
  #(10 20 30))

(deftest adjust-array-vector.6
  (adjust-array #(10 20) 3 :initial-element 'hello)
  #(10 20 hello))

(deftest adjust-array-vector.7
  (adjust-array #(10 20) 3 :initial-contents '(22 33 44))
  #(22 33 44))

(deftest adjust-array-vector.8
  (adjust-array #(10 20) 3 :initial-contents #(22 33 44))
  #(22 33 44))

(deftest-error adjust-array-vector.9
  (adjust-array #(10 20) 3 :initial-contents '(22 33 44 55)))

(deftest-error adjust-array-vector.10
  (adjust-array #(10 20) 3 :initial-contents #(22 33 44 55)))

(deftest-error adjust-array-vector.11
  (fill-pointer
    (adjust-array #(10 20 30 40 50) 3 :fill-pointer t)))

(deftest adjust-array-vector.12
  (adjust-array #(10 20 30 40 50) 3 :displaced-to #(20 30 40 50 60))
  #(20 30 40))

(deftest adjust-array-vector.13
  (adjust-array #(10 20 30 40 50) 3
                :displaced-to #(20 30 40 50 60)
                :displaced-index-offset 2)
  #(40 50 60))


;;
;;  string
;;
(deftest adjust-array-string.1
  (let ((x (adjust-array "" 3)))
    (values
      (stringp x)
      (length x)
      (typep x 'simple-string)))
  t 3 t)

(deftest adjust-array-string.2
  (let ((x (adjust-array "ABCDE" 3)))
    (values
      (stringp x)
      (length x)
      (typep x 'simple-string)))
  t 3 t)

(deftest-error adjust-array-string.3
  (adjust-array "ABCDEF" nil))

(deftest-error adjust-array-string.4
  (adjust-array "ABCDEF" '(3 4)))

(deftest adjust-array-string.5
  (adjust-array "ABCDEF" 3 :initial-element #\a)
  "ABC")

(deftest-error adjust-array-string.6
  (adjust-array "ABCDEF" 3 :initial-element 10))

(deftest adjust-array-string.7
  (adjust-array "AB" 3 :initial-element #\Z)
  "ABZ")

(deftest adjust-array-string.8
  (adjust-array "AB" 3 :initial-contents '(#\X #\Y #\Z))
  "XYZ")

(deftest adjust-array-string.9
  (adjust-array "AB" 3 :initial-contents "XYZ")
  "XYZ")

(deftest-error adjust-array-string.10
  (adjust-array "AB" 3 :initial-contents '(#\A #\B #\C #\D)))

(deftest-error adjust-array-string.11
  (adjust-array "AB" 3 :initial-contents "ABCD"))

(deftest-error adjust-array-string.12
  (fill-pointer
    (adjust-array "ABCDEF" 3 :fill-pointer t)))

(deftest adjust-array-string.13
  (adjust-array "ABCDE" 3 :displaced-to "HELLO")
  "HEL")

(deftest adjust-array-string.14
  (adjust-array "ABCDE" 3
                :displaced-to "HelloABCDE"
                :displaced-index-offset 2)
  "llo")


;;
;;  bit-vector
;;
(deftest adjust-array-bitvector.1
  (let ((x (adjust-array #* 3)))
    (values
      (bit-vector-p x)
      (length x)
      (typep x 'simple-bit-vector)
      ))
  t 3 t)

(deftest adjust-array-bitvector.2
  (let ((x (adjust-array #*10110 3)))
    (values
      (bit-vector-p x)
      (length x)
      (typep x 'simple-bit-vector)))
  t 3 t)

(deftest-error adjust-array-bitvector.3
  (adjust-array #*10110 nil))

(deftest-error adjust-array-bitvector.4
  (adjust-array #*10110 '(3 4)))

(deftest adjust-array-bitvector.5
  (adjust-array #*10110 3 :initial-element 1)
  #*101)

(deftest-error adjust-array-bitvector.6
  (adjust-array #*10110 3 :initial-element 10))

(deftest adjust-array-bitvector.7
  (adjust-array #*10 3 :initial-element 1)
  #*101)

(deftest adjust-array-bitvector.8
  (adjust-array #*10 3 :initial-contents '(1 1 0))
  #*110)

(deftest adjust-array-bitvector.9
  (adjust-array #*10 3 :initial-contents #*011)
  #*011)

(deftest-error adjust-array-bitvector.10
  (adjust-array #*10 3 :initial-contents '(1 1 0 0)))

(deftest-error adjust-array-bitvector.11
  (adjust-array #*10 3 :initial-contents #(1 1 0 0)))

(deftest-error adjust-array-bitvector.12
  (fill-pointer
    (adjust-array #*11001 3 :fill-pointer t)))

(deftest adjust-array-bitvector.13
  (adjust-array #*11001 3 :displaced-to #*00110)
  #*001)

(deftest adjust-array-bitvector.14
  (adjust-array #*11001 3
                :displaced-to #*10101100
                :displaced-index-offset 2)
  #*101)

