;;
;;  ANSI COMMON LISP: 15. Arrays
;;

;;
;;  adjustable-array-p
;;
(deftest adjustable-array-p.1
  (adjustable-array-p
    (make-array 10 :adjustable t))
  t)

(deftest adjustable-array-p.2
  (adjustable-array-p
    (make-array 10 :adjustable nil))
  nil)

(deftest adjustable-array-p.3
  (adjustable-array-p
    (make-array 10))
  nil)

(deftest adjustable-array-p.4
  (adjustable-array-p #(10 20 30))
  nil)

(deftest adjustable-array-p.5
  (adjustable-array-p "Hello")
  nil)

(deftest adjustable-array-p.6
  (adjustable-array-p #*11011)
  nil)

(deftest-error adjustable-array-p.7
  (eval '(adjustable-array-p (list 10 20 30))))


;;
;;  aref
;;
(deftest aref.1
  (let ((array (make-array 10)))
    (aref array 0))
  nil)

(deftest aref.2
  (let ((array (make-array 10)))
    (setf (aref array 1) 10)
    (setf (aref array 2) 20)
    (setf (aref array 5) 50)
    (values (aref array 0) (aref array 1) (aref array 2) (aref array 3)
            (aref array 4) (aref array 5) (aref array 6)))
  nil 10 20 nil nil 50 nil)

(deftest aref.3
  (let ((array (make-array nil)))
    (setf (aref array) 10)
    (aref array))
  10)

(deftest aref.4
  (let ((array (make-array '(2 3 4))))
    (setf (aref array 0 0 0) 10)
    (setf (aref array 1 0 0) 20)
    (setf (aref array 0 1 0) 30)
    (setf (aref array 1 2 3) 40)
    (values (aref array 0 0 0)
            (aref array 1 0 0)
            (aref array 0 1 0)
            (aref array 1 2 3)))
  10 20 30 40)

(deftest aref.5
  (aref "Hello" 1)
  #\e)

(deftest aref.6
  (aref #(10 20 30) 1)
  20)

(deftest aref.7
  (aref #*1011011 2)
  1)

(deftest aref.8
  (aref #1a(10 20 30) 1)
  20)

(deftest aref.9
  (aref (make-array nil))
  nil)

(deftest-error aref.10
  (aref (make-array 10) 10))

(deftest-error aref.11
  (aref (make-array '(4 5 6)) 3 4 6))

(deftest-error aref.12
  (aref (make-array nil) 0))

(deftest setf-aref.1
  (let ((pos #(10 20 30 40)))
    (setf (aref pos 2) 999))
  999)

(deftest setf-aref.2
  (let ((pos #(10 20 30 40)))
    (setf (aref pos 2) 999)
    (aref pos 2))
  999)

(deftest setf-aref.3
  (let ((pos "Hello"))
    (setf (aref pos 2) #\L)
    pos)
  "HeLlo")

(deftest setf-aref.4
  (let ((pos #*00110011))
    (setf (aref pos 1) 1)
    (setf (aref pos 2) 0)
    (values
      (aref pos 1)
      (aref pos 2)))
  1 0)

(deftest setf-aref.5
  (let ((x (make-array nil)))
    (setf (aref x) 100)
    (aref x))
  100)

(deftest-error setf-aref.6
  (let ((x (make-array nil)))
    (setf (aref x 0) 100)))

(deftest-error setf-aref.7
  (let ((x (make-array 10)))
    (setf (aref x 10) 100)))

(deftest-error setf-aref.8
  (let ((x (make-array '(10 20 30))))
    (setf (aref x 9 19 30) 100)))


;;
;;  array-dimension
;;
(deftest array-dimension.1
  (array-dimension (make-array 4) 0)
  4)

(deftest array-dimension.2
  (array-dimension (make-array '(2)) 0)
  2)

(deftest array-dimension.3
  (array-dimension (make-array '(2 3)) 1)
  3)

(deftest-error array-dimension.4
  (array-dimension (make-array nil) 0))

(deftest array-dimension.5
  (array-dimension "Hello" 0)
  5)

(deftest array-dimension.6
  (array-dimension #(10 20 30) 0)
  3)

(deftest array-dimension.7
  (array-dimension #*0011001100 0)
  10)

(deftest-error array-dimension.8
  (array-dimension #*0011001100 1))


(deftest-error array-dimension.9
  (array-dimension (make-array '(2 3 4)) 3))


;;
;;  array-dimensions
;;
(deftest array-dimensions.1
  (array-dimensions (make-array 4))
  (4))

(deftest array-dimensions.2
  (array-dimensions (make-array '(10)))
  (10))

(deftest array-dimensions.3
  (array-dimensions (make-array '(2 3)))
  (2 3))

(deftest array-dimensions.4
  (array-dimensions (make-array 4 :fill-pointer 2))
  (4))

(deftest array-dimensions.5
  (array-dimensions (make-array nil))
  nil)

(deftest array-dimensions.6
  (array-dimensions "Hello")
  (5))

(deftest array-dimensions.7
  (array-dimensions #(10 20 30))
  (3))

(deftest array-dimensions.8
  (array-dimensions #*110011)
  (6))


;;
;;  array-element-type
;;
(deftest array-element-type.1
  (array-element-type
    (make-array 10))
  t)

(deftest array-element-type.2
  (array-element-type
    (make-array 10 :element-type 'character))
  character)

(deftest array-element-type.3
  (array-element-type
    (make-array 10 :element-type '(unsigned-byte 18)))
  (unsigned-byte 32))

(deftest array-element-type.4
  (array-element-type "Hello")
  character)

(deftest array-element-type.5
  (array-element-type #(10 20 30))
  t)

(deftest array-element-type.6
  (array-element-type #*101010)
  bit)

(deftest array-element-type.7
  (array-element-type
    (make-array 10 :element-type t))
  t)

(deftest array-element-type.8
  (array-element-type
    (make-array '(3 4) :element-type 'bit))
  bit)

(deftest array-element-type.9
  (array-element-type
    (make-array '(3 4) :element-type 'character))
  character)

(deftest array-element-type.10
  (array-element-type
    (make-array '(3 4) :element-type '(signed-byte 8)))
  (signed-byte 8))

(deftest array-element-type.11
  (array-element-type
    (make-array '(3 4) :element-type '(signed-byte 16)))
  (signed-byte 16))

(deftest array-element-type.12
  (array-element-type
    (make-array '(3 4) :element-type '(signed-byte 32)))
  (signed-byte 32))

(deftest array-element-type.13
  (array-element-type
    (make-array '(3 4) :element-type '(signed-byte 128)))
  t)

(deftest array-element-type.14
  (array-element-type
    (make-array '(3 4) :element-type '(unsigned-byte 8)))
  (unsigned-byte 8))

(deftest array-element-type.15
  (array-element-type
    (make-array '(3 4) :element-type '(unsigned-byte 16)))
  (unsigned-byte 16))

(deftest array-element-type.16
  (array-element-type
    (make-array '(3 4) :element-type '(unsigned-byte 32)))
  (unsigned-byte 32))

(deftest array-element-type.17
  (array-element-type
    (make-array '(3 4) :element-type '(unsigned-byte 128)))
  t)

(deftest array-element-type.18
  (array-element-type
    (make-array '(3 4) :element-type 'single-float))
  single-float)

(deftest array-element-type.19
  (array-element-type
    (make-array '(3 4) :element-type 'double-float))
  double-float)

(deftest array-element-type.20
  (array-element-type
    (make-array '(3 4) :element-type 'long-float))
  long-float)

(deftest array-element-type.21
  (array-element-type
    (make-array '(3 4) :element-type 'long-float))
  long-float)

(deftest array-element-type.22
  (array-element-type
    (make-array '(3 4) :element-type '(integer -300 20000)))
  (signed-byte 16))

(deftest array-element-type.23
  (array-element-type
    (make-array '(3 4) :element-type '(mod 256)))
  (unsigned-byte 8))

(deftest array-element-type.24
  (array-element-type
    (make-array '(3 4) :element-type '(integer 1000000 2000000)))
  (unsigned-byte 32))

(deftest-error array-element-type.25
  (eval '(array-element-type 10))
  type-error)

(deftest array-element-type.26
  (array-element-type
    (make-array '(3 4) :element-type 'cons))
  t)

#+64-bit
(deftest array-element-type-64bit.1
  (array-element-type
    (make-array '(3 4) :element-type '(signed-byte 64)))
  (signed-byte 64))

#+64-bit
(deftest array-element-type-64bit.2
  (array-element-type
    (make-array '(3 4) :element-type '(unsigned-byte 64)))
  (unsigned-byte 64))


;;
;;  array-has-fill-pointer-p
;;
(deftest array-has-fill-pointer-p.1
  (array-has-fill-pointer-p
    (make-array 10))
  nil)

(deftest array-has-fill-pointer-p.2
  (array-has-fill-pointer-p
    (make-array '(10)))
  nil)

(deftest array-has-fill-pointer-p.3
  (array-has-fill-pointer-p
    (make-array nil))
  nil)

(deftest array-has-fill-pointer-p.4
  (array-has-fill-pointer-p
    (make-array '(2 3)))
  nil)

(deftest array-has-fill-pointer-p.5
  (array-has-fill-pointer-p
    (make-array 10 :fill-pointer t))
  t)

(deftest array-has-fill-pointer-p.6
  (array-has-fill-pointer-p "Hello")
  nil)

(deftest array-has-fill-pointer-p.7
  (array-has-fill-pointer-p #(a b c))
  nil)

(deftest array-has-fill-pointer-p.8
  (array-has-fill-pointer-p #*110011)
  nil)

(deftest-error array-has-fill-pointer-p.9
  (eval '(array-has-fill-pointer-p 10))
  type-error)


;;
;;  array-displacement
;;
(deftest array-displacement.1
  (array-displacement (make-array 10))
  nil 0)

(deftest array-displacement.2
  (let ((pos (make-array 10)))
    (multiple-value-bind (check1 check2)
      (array-displacement
        (make-array 10 :displaced-to pos))
      (values
        (arrayp check1)
        check2)))
  t 0)

(deftest array-displacement.3
  (let ((pos (make-array 20)))
    (multiple-value-bind (check1 check2)
      (array-displacement
        (make-array 10 :displaced-to pos :displaced-index-offset 4))
      (values
        (arrayp check1)
        check2)))
  t 4)

(deftest array-displacement.4
  (array-displacement "Hello")
  nil 0)

(deftest array-displacement.5
  (array-displacement #(10 20 30))
  nil 0)

(deftest array-displacement.6
  (array-displacement #*11001)
  nil 0)

(deftest-error array-displacement.7
  (eval '(array-displacement 100))
  type-error)


;;
;;  array-in-bounds-p
;;
(deftest array-in-bounds-p.1
  (array-in-bounds-p
    (make-array nil))
  t)

(deftest-error array-in-bounds-p.2
  (array-in-bounds-p
    (make-array nil)
    0))

(deftest array-in-bounds-p.3
  (array-in-bounds-p (make-array 10) 0)
  t)

(deftest array-in-bounds-p.4
  (array-in-bounds-p (make-array 10) 9)
  t)

(deftest array-in-bounds-p.5
  (array-in-bounds-p (make-array 10) 10)
  nil)

(deftest array-in-bounds-p.6
  (array-in-bounds-p (make-array 10) -1)
  nil)

(deftest-error array-in-bounds-p.7
  (array-in-bounds-p (make-array 10)))

(deftest-error array-in-bounds-p.8
  (array-in-bounds-p (make-array 10) 1 2))

(deftest array-in-bounds-p.9
  (array-in-bounds-p (make-array '(10 20 30)) 1 2 4)
  t)

(deftest array-in-bounds-p.10
  (array-in-bounds-p (make-array '(10 20 30)) -10 2 4)
  nil)

(deftest array-in-bounds-p.11
  (array-in-bounds-p (make-array '(10 20 30)) 1 2 60)
  nil)

(deftest array-in-bounds-p.12
  (array-in-bounds-p (make-array '(10 20 30)) 1 19 5)
  t)

(deftest array-in-bounds-p.13
  (array-in-bounds-p (make-array '(10 20 30)) 1 20 5)
  nil)

(deftest-error array-in-bounds-p.14
  (array-in-bounds-p (make-array '(10 20 30)) 2 4))

(deftest-error array-in-bounds-p.15
  (array-in-bounds-p (make-array '(10 20 30)) 1 2 4 5))

(deftest array-in-bounds-p.16
  (array-in-bounds-p "Hello" 1)
  t)

(deftest array-in-bounds-p.17
  (array-in-bounds-p "Hello" 4)
  t)

(deftest array-in-bounds-p.18
  (array-in-bounds-p "Hello" 5)
  nil)

(deftest-error array-in-bounds-p.19
  (array-in-bounds-p "Hello"))

(deftest array-in-bounds-p.20
  (array-in-bounds-p #(1 2 3 4 5) 1)
  t)

(deftest array-in-bounds-p.21
  (array-in-bounds-p #(1 2 3 4 5) 4)
  t)

(deftest array-in-bounds-p.22
  (array-in-bounds-p #(1 2 3 4 5) 5)
  nil)

(deftest-error array-in-bounds-p.23
  (array-in-bounds-p #(1 2 3 4 5) 1 2 3))

(deftest array-in-bounds-p.24
  (array-in-bounds-p #*11101 1)
  t)

(deftest array-in-bounds-p.25
  (array-in-bounds-p #*11101 4)
  t)

(deftest array-in-bounds-p.26
  (array-in-bounds-p #*11101 5)
  nil)

(deftest-error array-in-bounds-p.27
  (array-in-bounds-p #*00001))

(deftest array-in-bounds-p.28
  (array-in-bounds-p
    (make-array 30 :fill-pointer 20)
    19)
  t)

(deftest array-in-bounds-p.29
  (array-in-bounds-p
    (make-array 30 :fill-pointer 20)
    20)
  t)

(deftest array-in-bounds-p.30
  (array-in-bounds-p
    (make-array 30 :fill-pointer 20)
    29)
  t)

(deftest array-in-bounds-p.31
  (array-in-bounds-p
    (make-array 30 :fill-pointer 20)
    30)
  nil)


;;
;;  array-rank
;;
(deftest array-rank.1
  (array-rank (make-array nil))
  0)

(deftest array-rank.2
  (array-rank (make-array 10))
  1)

(deftest array-rank.3
  (array-rank (make-array '(111)))
  1)

(deftest array-rank.4
  (array-rank (make-array '(1 2 3)))
  3)

(deftest array-rank.5
  (array-rank "Hello")
  1)

(deftest array-rank.6
  (array-rank #(1 2 3))
  1)

(deftest array-rank.7
  (array-rank #*11011)
  1)

(deftest-error array-rank.8
  (eval '(array-rank (list 10 20 30)))
  type-error)


;;
;;  array-row-major-index
;;
(deftest array-row-major-index.1
  (array-row-major-index (make-array nil))
  0)

(deftest array-row-major-index.2
  (array-row-major-index (make-array 10) 7)
  7)

(deftest array-row-major-index.3
  (array-row-major-index (make-array '(10)) 3)
  3)

(deftest array-row-major-index.4
  (array-row-major-index (make-array '(10 20 30)) 3 4 5)
  1925)

(deftest-error array-row-major-index.5
  (array-row-major-index (make-array 10) 10))

(deftest-error array-row-major-index.6
  (array-row-major-index (make-array 10) 4 5))

(deftest-error array-row-major-index.7
  (array-row-major-index (make-array 10)))

(deftest array-row-major-index.8
  (array-row-major-index "Hello" 3)
  3)

(deftest-error array-row-major-index.9
  (array-row-major-index "Hello" 5))

(deftest array-row-major-index.10
  (array-row-major-index #(1 2 3 4 5) 3)
  3)

(deftest-error array-row-major-index.11
  (array-row-major-index #(1 2 3 4 5) 5))

(deftest array-row-major-index.12
  (array-row-major-index #*11011 3)
  3)

(deftest-error array-row-major-index.13
  (array-row-major-index #*11011 5))

(deftest array-row-major-index.14
  (array-row-major-index
    (make-array '(4 7) :element-type '(unsigned-byte 8))
    1 2)
  9)

(deftest array-row-major-index.15
  (array-row-major-index
    (make-array '(2 3 4)
                :element-type '(unsigned-byte 8)
                :displaced-to (make-array '(4 7) :element-type '(unsigned-byte 8))
                :displaced-index-offset 4)
    0 2 1)
  9)


;;
;;  array-total-size
;;
(deftest array-total-size.1
  (array-total-size (make-array nil))
  1)

(deftest array-total-size.2
  (array-total-size (make-array 4))
  4)

(deftest array-total-size.3
  (array-total-size (make-array '(10)))
  10)

(deftest array-total-size.4
  (array-total-size (make-array 4 :fill-pointer 2))
  4)

(deftest array-total-size.5
  (array-total-size (make-array 0))
  0)

(deftest array-total-size.6
  (array-total-size (make-array '(4 2)))
  8)

(deftest array-total-size.7
  (array-total-size (make-array '(4 0)))
  0)

(deftest array-total-size.8
  (array-total-size "Hello")
  5)

(deftest array-total-size.9
  (array-total-size #(1 2 3 4 5))
  5)

(deftest array-total-size.10
  (array-total-size #*11011)
  5)


;;
;;  fill-pointer
;;
(deftest fill-pointer.1
  (fill-pointer
    (make-array 10 :fill-pointer t))
  10)

(deftest fill-pointer.2
  (fill-pointer
    (make-array 10 :fill-pointer 5))
  5)

(deftest-error fill-pointer.3
  (fill-pointer
    (make-array 10 :fill-pointer nil))
  type-error)

(deftest-error fill-pointer.4
  (fill-pointer "Hello")
  type-error)

(deftest-error fill-pointer.5
  (fill-pointer #(1 2 3))
  type-error)

(deftest-error fill-pointer.6
  (fill-pointer #*111)
  type-error)

(deftest setf-fill-pointer.1
  (let ((pos (make-array 10 :fill-pointer t)))
    (setf (fill-pointer pos) 9))
  9)

(deftest setf-fill-pointer.2
  (let ((pos (make-array 10 :fill-pointer t)))
    (setf (fill-pointer pos) 9)
    (fill-pointer pos))
  9)

(deftest-error setf-fill-pointer.3
  (let ((pos (make-array 10 :fill-pointer nil)))
    (setf (fill-pointer pos) 9))
  type-error)

(deftest-error setf-fill-pointer.4
  (let ((pos (make-array 10 :fill-pointer nil)))
    (setf (fill-pointer pos) 10)))

(deftest-error setf-fill-pointer.5
  (setf (fill-pointer "Hello") 1)
  type-error)

(deftest-error setf-fill-pointer.6
  (setf (fill-pointer #(10 20 30)) 1)
  type-error)

(deftest-error setf-fill-pointer.7
  (setf (fill-pointer #*110111) 1)
  type-error)

(deftest setf-fill-pointer.8
  (let ((pos (make-array 5 :fill-pointer 3 :initial-contents '(1 2 3 4 5))))
    (setf (fill-pointer pos) 4)
    pos)
  #(1 2 3 4))


;;
;;  row-major-aref
;;
(deftest row-major-aref.1
  (let ((pos (make-array 10)))
    (setf (aref pos 9) :hello)
    (row-major-aref pos 9))
  :hello)

(deftest row-major-aref.2
  (let ((pos (make-array '(10))))
    (setf (aref pos 9) :hello)
    (row-major-aref pos 9))
  :hello)

(deftest row-major-aref.3
  (let ((pos (make-array nil)))
    (setf (aref pos) :abc)
    (row-major-aref pos 0))
  :abc)

(deftest row-major-aref.4
  (let ((pos (make-array '(5 6))))
    (setf (aref pos 2 4) :hello)
    (row-major-aref pos 16))
  :hello)

(deftest-error row-major-aref.5
  (let ((pos (make-array '(5 6))))
    (row-major-aref pos 30)))

(deftest row-major-aref.6
  (row-major-aref "Hello" 3)
  #\l)

(deftest row-major-aref.7
  (row-major-aref #(10 20 30) 1)
  20)

(deftest row-major-aref.8
  (row-major-aref #*10110011 3)
  1)

(deftest setf-row-major-aref.1
  (let ((pos (make-array 10)))
    (setf (row-major-aref pos 3) :hello))
  :hello)

(deftest setf-row-major-aref.2
  (let ((pos (make-array 10)))
    (setf (row-major-aref pos 3) :hello)
    (aref pos 3))
  :hello)

(deftest setf-row-major-aref.3
  (let ((pos (make-array '(10))))
    (setf (row-major-aref pos 3) :hello)
    (aref pos 3))
  :hello)

(deftest setf-row-major-aref.4
  (let ((pos (make-array nil)))
    (setf (row-major-aref pos 0) :hello)
    (aref pos))
  :hello)

(deftest setf-row-major-aref.5
  (let ((pos (make-array '(3 4))))
    (setf (row-major-aref pos 10) :hello)
    (aref pos 2 2))
  :hello)

(deftest setf-row-major-aref.6
  (let ((pos "Hello"))
    (setf (row-major-aref pos 2) #\a)
    pos)
  "Healo")

(deftest setf-row-major-aref.7
  (let ((pos #(10 20 30)))
    (setf (row-major-aref pos 2) #\a)
    (aref pos 2))
  #\a)

(deftest setf-row-major-aref.8
  (let ((pos #*000000))
    (setf (row-major-aref pos 2) 1)
    (aref pos 2))
  1)


;;
;;  upgraded-array-element-type
;;
(deftest upgraded-array-element-type.1
  (upgraded-array-element-type 'standard-char)
  character)

(deftest upgraded-array-element-type.2
  (upgraded-array-element-type 'package)
  t)

(deftest upgraded-array-element-type.3
  (upgraded-array-element-type '(integer 30 40))
  (unsigned-byte 8))

(deftest upgraded-array-element-type.4
  (upgraded-array-element-type 'bit)
  bit)

(deftest upgraded-array-element-type.5
  (upgraded-array-element-type 'character)
  character)

(deftest upgraded-array-element-type.6
  (upgraded-array-element-type '(signed-byte 8))
  (signed-byte 8))

(deftest upgraded-array-element-type.7
  (upgraded-array-element-type '(signed-byte 16))
  (signed-byte 16))

(deftest upgraded-array-element-type.8
  (upgraded-array-element-type '(signed-byte 32))
  (signed-byte 32))

(deftest upgraded-array-element-type.9
  (upgraded-array-element-type '(unsigned-byte 8))
  (unsigned-byte 8))

(deftest upgraded-array-element-type.10
  (upgraded-array-element-type '(unsigned-byte 16))
  (unsigned-byte 16))

(deftest upgraded-array-element-type.11
  (upgraded-array-element-type '(unsigned-byte 32))
  (unsigned-byte 32))

(deftest upgraded-array-element-type.12
  (upgraded-array-element-type 'single-float)
  single-float)

(deftest upgraded-array-element-type.13
  (upgraded-array-element-type 'double-float)
  double-float)

(deftest upgraded-array-element-type.14
  (upgraded-array-element-type 'long-float)
  long-float)

#+64-bit
(deftest upgraded-array-element-type-64bit.1
  (upgraded-array-element-type '(signed-byte 64))
  (signed-byte 64))

#+64-bit
(deftest upgraded-array-element-type-64bit.2
  (upgraded-array-element-type '(unsigned-byte 64))
  (unsigned-byte 64))


;;
;;  array-dimension-limit
;;
(deftest array-dimension-limit.1
  (integerp array-dimension-limit)
  t)

(deftest array-dimension-limit.2
  (< array-dimension-limit 1024)
  nil)


;;
;;  array-rank-limit
;;
(deftest array-rank-limit.1
  (integerp array-rank-limit)
  t)

(deftest array-rank-limit.2
  (< array-rank-limit 8)
  nil)


;;
;;  array-total-size-limit
;;
(deftest array-total-size-limit.1
  (integerp array-total-size-limit)
  t)

(deftest array-total-size-limit.2
  (< array-total-size-limit 1024)
  nil)

