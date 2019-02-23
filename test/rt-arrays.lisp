;;
;;  ANSI COMMON LISP: 15. Arrays
;;
(deftest make-array.1
  (arrayp
    (make-array 10))
  t)

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

(deftest setf-aref.1
  (let ((pos #(10 20 30 40)))
    (setf (aref pos 2) 999)
    (aref pos 2))
  999)

(deftest setf-aref.2
  (let ((pos "Hello"))
    (setf (aref pos 2) #\L)
    pos)
  "HeLlo")

(deftest setf-aref.3
  (let ((pos #*00110011))
    (setf (aref pos 1) 1)
    (setf (aref pos 2) 0)
    (values
      (aref pos 1)
      (aref pos 2)))
  1 0)

(deftest array-dimension.1
  (array-dimension (make-array 4) 0)
  4)

(deftest array-dimension.2
  (array-dimension (make-array '(2 3)) 1)
  3)

(deftest-error array-dimension.3
  (array-dimension (make-array nil) 0))

(deftest array-dimension.4
  (array-dimension "Hello" 0)
  5)

(deftest array-dimension.5
  (array-dimension #(10 20 30) 0)
  3)

(deftest array-dimension.6
  (array-dimension #*0011001100 0)
  10)

(deftest-error array-dimension.7
  (array-dimension #*0011001100 1))

(deftest array-dimensions.1
  (array-dimensions (make-array 4))
  (4))

(deftest array-dimensions.2
  (array-dimensions (make-array '(2 3)))
  (2 3))

(deftest array-dimensions.3
  (array-dimensions (make-array 4 :fill-pointer 2))
  (4))

(deftest array-dimensions.4
  (array-dimensions (make-array nil))
  nil)

(deftest array-dimensions.5
  (array-dimensions "Hello")
  (5))

(deftest array-dimensions.6
  (array-dimensions #(10 20 30))
  (3))

(deftest array-dimensions.7
  (array-dimensions #*110011)
  (6))

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

(deftest array-has-fill-pointer-p.1
  (array-has-fill-pointer-p
    (make-array 10))
  nil)

(deftest array-has-fill-pointer-p.2
  (array-has-fill-pointer-p
    (make-array '(2 3)))
  nil)

(deftest array-has-fill-pointer-p.3
  (array-has-fill-pointer-p
    (make-array 10 :fill-pointer t))
  t)

(deftest array-has-fill-pointer-p.4
  (array-has-fill-pointer-p "Hello")
  nil)

(deftest array-has-fill-pointer-p.5
  (array-has-fill-pointer-p #(a b c))
  nil)

(deftest array-has-fill-pointer-p.6
  (array-has-fill-pointer-p #*110011)
  nil)

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
  (let ((pos (make-array 10)))
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

(deftest-error array-in-bounds-p.12
  (array-in-bounds-p (make-array '(10 20 30)) 2 4))

(deftest-error array-in-bounds-p.13
  (array-in-bounds-p (make-array '(10 20 30)) 1 2 4 5))

(deftest array-in-bounds-p.14
  (array-in-bounds-p "Hello" 1)
  t)

(deftest array-in-bounds-p.15
  (array-in-bounds-p "Hello" 5)
  nil)

(deftest-error array-in-bounds-p.16
  (array-in-bounds-p "Hello"))

(deftest array-in-bounds-p.17
  (array-in-bounds-p #(1 2 3 4 5) 1)
  t)

(deftest array-in-bounds-p.18
  (array-in-bounds-p #(1 2 3 4 5) 5)
  nil)

(deftest-error array-in-bounds-p.19
  (array-in-bounds-p #(1 2 3 4 5) 1 2 3))

(deftest array-in-bounds-p.20
  (array-in-bounds-p #*11101 1)
  t)

(deftest array-in-bounds-p.21
  (array-in-bounds-p #*11101 5)
  nil)

(deftest-error array-in-bounds-p.22
  (array-in-bounds-p #*00001))

(deftest array-rank.1
  (array-rank (make-array nil))
  0)

(deftest array-rank.2
  (array-rank (make-array 10))
  1)

(deftest array-rank.3
  (array-rank (make-array '(1 2 3)))
  3)

(deftest array-rank.4
  (array-rank "Hello")
  1)

(deftest array-rank.5
  (array-rank #(1 2 3))
  1)

(deftest array-rank.6
  (array-rank #*11011)
  1)

(deftest array-row-major-index.1
  (array-row-major-index (make-array nil))
  0)

(deftest array-row-major-index.2
  (array-row-major-index (make-array 10) 7)
  7)

(deftest array-row-major-index.3
  (array-row-major-index (make-array '(10 20 30)) 3 4 5)
  1925)

(deftest array-row-major-index.4
  (array-row-major-index "Hello" 3)
  3)

(deftest array-row-major-index.5
  (array-row-major-index #(1 2 3 4 5) 3)
  3)

(deftest array-row-major-index.6
  (array-row-major-index #*11011 3)
  3)

(deftest array-total-size.1
  (array-total-size (make-array nil))
  1)

(deftest array-total-size.2
  (array-total-size (make-array 4))
  4)

(deftest array-total-size.3
  (array-total-size (make-array 4 :fill-pointer 2))
  4)

(deftest array-total-size.4
  (array-total-size (make-array '(4 2)))
  8)

(deftest array-total-size.5
  (array-total-size (make-array '(4 0)))
  0)

(deftest array-total-size.6
  (array-total-size "Hello")
  5)

(deftest array-total-size.7
  (array-total-size #(1 2 3 4 5))
  5)

(deftest array-total-size.8
  (array-total-size #*11011)
  5)

(deftest arrayp.1
  (arrayp (make-array nil))
  t)

(deftest arrayp.2
  (arrayp "Hello")
  t)

(deftest arrayp.3
  (arrayp #(1 2 3))
  t)

(deftest arrayp.4
  (arrayp #*11011)
  t)

(deftest arrayp.5
  (arrayp 'hello)
  nil)

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
    (setf (fill-pointer pos) 9)
    (fill-pointer pos))
  9)

(deftest-error setf-fill-pointer.2
  (let ((pos (make-array 10 :fill-pointer nil)))
    (setf (fill-pointer pos) 9))
  type-error)

(deftest-error setf-fill-pointer.3
  (let ((pos (make-array 10 :fill-pointer nil)))
    (setf (fill-pointer pos) 10)))

(deftest-error setf-fill-pointer.4
  (setf (fill-pointer "Hello") 1)
  type-error)

(deftest-error setf-fill-pointer.5
  (setf (fill-pointer #(10 20 30)) 1)
  type-error)

(deftest-error setf-fill-pointer.6
  (setf (fill-pointer #*110111) 1)
  type-error)

(deftest row-major-aref.1
  (let ((pos (make-array 10)))
    (setf (aref pos 9) :hello)
    (row-major-aref pos 9))
  :hello)

(deftest row-major-aref.2
  (let ((pos (make-array '(5 6))))
    (setf (aref pos 2 4) :hello)
    (row-major-aref pos 16))
  :hello)

(deftest-error row-major-aref.3
  (let ((pos (make-array '(5 6))))
    (row-major-aref pos 30)))

(deftest row-major-aref.4
  (row-major-aref "Hello" 3)
  #\l)

(deftest row-major-aref.5
  (row-major-aref #(10 20 30) 1)
  20)

(deftest row-major-aref.6
  (row-major-aref #*10110011 3)
  1)

(deftest setf-row-major-aref.1
  (let ((pos (make-array 10)))
    (setf (row-major-aref pos 3) :hello)
    (aref pos 3))
  :hello)

(deftest setf-row-major-aref.2
  (let ((pos (make-array '(3 4))))
    (setf (row-major-aref pos 10) :hello)
    (aref pos 2 2))
  :hello)

(deftest setf-row-major-aref.3
  (let ((pos "Hello"))
    (setf (row-major-aref pos 2) #\a)
    pos)
  "Healo")

(deftest setf-row-major-aref.4
  (let ((pos #(10 20 30)))
    (setf (row-major-aref pos 2) #\a)
    (aref pos 2))
  #\a)

(deftest setf-row-major-aref.5
  (let ((pos #*000000))
    (setf (row-major-aref pos 2) 1)
    (aref pos 2))
  1)

(deftest upgraded-array-element-type.1
  (upgraded-array-element-type 'standard-char)
  character)

(deftest upgraded-array-element-type.2
  (upgraded-array-element-type 'package)
  t)

(deftest upgraded-array-element-type.3
  (upgraded-array-element-type '(integer 30 40))
  (unsigned-byte 8))

(deftest array-dimension-limit.1
  (< 0 array-dimension-limit)
  t)

(deftest array-rank-limit.1
  (< 0 array-rank-limit)
  t)

(deftest array-total-size-limit.1
  (< 0 array-total-size-limit)
  t)

(deftest simple-vector-p.1
  (simple-vector-p (make-array 6))
  t)

(deftest simple-vector-p.2
  (simple-vector-p "aaa")
  nil)

(deftest simple-vector-p.3
  (simple-vector-p (make-array 6 :fill-pointer t))
  nil)

(deftest simple-vector-p.4
  (simple-vector-p #(10 20 30))
  t)

(deftest svref.1
  (let ((pos #(10 20 30)))
    (svref pos 1))
  20)

(deftest setf-svref.1
  (let ((pos #(10 20 30)))
    (setf (svref pos 1) 999)
    (svref pos 1))
  999)

(deftest vector.1
  (array-total-size
    (vector 10 20 30 40))
  4)

(deftest vector.2
  (arrayp (vector 10 20 30 40))
  t)

(deftest vector.3
  (simple-vector-p (vector 10 20 30 40))
  t)

(deftest vector.4
  (array-total-size (vector))
  0)

(deftest vector-pop.1
  (let ((pos (make-array 5 :initial-contents '(9 8 7 6 5) :fill-pointer t)))
    (values
      (vector-pop pos)
      (vector-pop pos)
      (vector-pop pos)
      (fill-pointer pos)))
  5 6 7 2)

(deftest-error vector-pop.2
  (let ((pos (make-array 5 :initial-contents '(9 8 7 6 5) :fill-pointer nil)))
    (vector-pop pos))
  type-error)

(deftest-error vector-pop.3
  (vector-pop #(10 20 30 40 50))
  type-error)

(deftest-error vector-pop.4
  (let ((pos (make-array 5 :initial-contents '(9 8 7 6 5) :fill-pointer 0)))
    (vector-pop pos))
  type-error)

(deftest vector-push.1
  (let ((pos (make-array 4 :fill-pointer 0)))
    (vector-push 1 pos))
  0)

(deftest vector-push.2
  (let ((pos (make-array 4 :fill-pointer 0)))
    (values
      (vector-push 1 pos)
      (vector-push 1 pos)
      (vector-push 1 pos)
      (vector-push 1 pos)
      (vector-push 1 pos)
      (vector-push 1 pos)))
  0 1 2 3 nil nil)

(deftest vector-push.3
  (let ((pos (make-array 4 :fill-pointer 0)))
    (vector-push 1 pos)
    (vector-push 2 pos)
    (vector-push 3 pos)
    (vector-push 4 pos)
    (values
      (aref pos 0)
      (aref pos 1)
      (aref pos 2)
      (aref pos 3)))
  1 2 3 4)

(deftest-error vector-push.4
  (let ((pos (make-array 4 :fill-pointer nil)))
    (vector-push 1 pos))
  type-error)

(deftest-error vector-push.5
  (vector-push 1 #(10 20 30))
  type-error)

(deftest vector-push-extend.1
  (let ((pos (make-array 5 :fill-pointer 0 :adjustable t)))
    (values
      (vector-push-extend 'x pos)
      pos))
  0 #(x))

(deftest vector-push-extend.2
  (let ((pos (make-array 5 :fill-pointer 4 :adjustable t :initial-element nil)))
    (values
      (vector-push-extend 'x pos)
      pos))
  4 #(nil nil nil nil x))

(deftest vector-push-extend.3
  (let ((pos (make-array 5 :fill-pointer 5 :adjustable t :initial-element nil)))
    (values
      (vector-push-extend 'x pos)
      pos))
  5 #(nil nil nil nil nil x))

(deftest vector-push-extend.4
  (let* ((a (make-array 3 :fill-pointer 3 :adjustable t :initial-element nil))
         (b (make-array 3 :fill-pointer 3 :adjustable t :displaced-to a)))
    (vector-push-extend 'x b)
    (values a b))
  #(nil nil nil)
  #(nil nil nil x))

(deftest vector-push-extend.5
  (let* ((a (make-array 3 :fill-pointer 3 :adjustable t :initial-element nil))
         (b (make-array 3 :fill-pointer 3 :adjustable t :displaced-to a)))
    (vector-push-extend 'x b)
    (vector-push-extend 'y b)
    (vector-push-extend 'z b)
    (vector-push-extend 'w a)
    (values a b))
  #(nil nil nil w)
  #(nil nil nil x y z))

(deftest vectorp.1
  (vectorp "aaaaaa")
  t)

(deftest vectorp.2
  (vectorp (make-array 6 :fill-pointer t))
  t)

(deftest vectorp.3
  (vectorp (make-array '(2 3 4)))
  nil)

(deftest vectorp.4
  (vectorp #*11)
  t)

(deftest vectorp.5
  (vectorp #b11)
  nil)

(deftest bit.1
  (bit #*10011 0)
  1)

(deftest bit.2
  (bit #*10011 1)
  0)

(deftest bit.3
  (bit #*10011 2)
  0)

(deftest bit.4
  (let ((a (make-array 5 :element-type 'bit :initial-contents '(1 0 0 1 1))))
    (values
      (bit a 0)
      (bit a 1)
      (bit a 2)
      (bit a 3)
      (bit a 4)))
  1 0 0 1 1)

(deftest bit.5
  (let ((a (make-array '(2 3) :element-type 'bit
                       :initial-contents '((1 0 0) (0 1 1)))))
    (values
      (bit a 0 0)
      (bit a 0 1)
      (bit a 0 2)
      (bit a 1 0)
      (bit a 1 1)
      (bit a 1 2)))
  1 0 0 0 1 1)

(deftest sbit.1
  (sbit #*10011 0)
  1)

(deftest sbit.2
  (sbit #*10011 1)
  0)

(deftest sbit.3
  (sbit #*10011 2)
  0)

(deftest sbit.4
  (let ((a (make-array 5 :element-type 'bit :initial-contents '(1 0 0 1 1))))
    (values
      (sbit a 0)
      (sbit a 1)
      (sbit a 2)
      (sbit a 3)
      (sbit a 4)))
  1 0 0 1 1)

(deftest sbit.5
  (let ((a (make-array '(2 3) :element-type 'bit
                       :initial-contents '((1 0 0) (0 1 1)))))
    (values
      (sbit a 0 0)
      (sbit a 0 1)
      (sbit a 0 2)
      (sbit a 1 0)
      (sbit a 1 1)
      (sbit a 1 2)))
  1 0 0 0 1 1)

(deftest setf-bit.1
  (let ((a (make-array 5 :element-type 'bit :initial-contents '(1 0 0 1 1))))
    (setf (bit a 0) 0)
    (setf (bit a 1) 1)
    (setf (bit a 2) 1)
    a)
  #*01111)

(deftest setf-bit.2
  (let ((a (make-array '(2 3) :element-type 'bit
                       :initial-contents '((1 0 0) (0 1 1)))))
    (setf (bit a 0 0) 0)
    (setf (bit a 0 1) 1)
    (setf (bit a 1 1) 0)
    (setf (bit a 1 2) 0)
    (values
      (bit a 0 0)
      (bit a 0 1)
      (bit a 0 2)
      (bit a 1 0)
      (bit a 1 1)
      (bit a 1 2)))
  0 1 0 0 0 0)

(deftest setf-sbit.1
  (let ((a (make-array 5 :element-type 'bit :initial-contents '(1 0 0 1 1))))
    (setf (sbit a 0) 0)
    (setf (sbit a 1) 1)
    (setf (sbit a 2) 1)
    a)
  #*01111)

(deftest setf-sbit.2
  (let ((a (make-array '(2 3) :element-type 'bit
                       :initial-contents '((1 0 0) (0 1 1)))))
    (setf (sbit a 0 0) 0)
    (setf (sbit a 0 1) 1)
    (setf (sbit a 1 1) 0)
    (setf (sbit a 1 2) 0)
    (values
      (sbit a 0 0)
      (sbit a 0 1)
      (sbit a 0 2)
      (sbit a 1 0)
      (sbit a 1 1)
      (sbit a 1 2)))
  0 1 0 0 0 0)

(deftest bit-vector-p.1
  (bit-vector-p #*110)
  t)

(deftest bit-vector-p.2
  (bit-vector-p
    (make-array 10 :element-type 'bit :fill-pointer t))
  t)

(deftest bit-vector-p.3
  (bit-vector-p
    (make-array '(2 3) :element-type 'bit))
  nil)

(deftest bit-vector-p.4
  (bit-vector-p #(1 0 1 1))
  nil)

(deftest simple-bit-vector-p.1
  (simple-bit-vector-p #*110)
  t)

(deftest simple-bit-vector-p.2
  (simple-bit-vector-p
    (make-array 10 :element-type 'bit :fill-pointer t))
  nil)

(deftest simple-bit-vector-p.3
  (simple-bit-vector-p
    (make-array '(2 3) :element-type 'bit))
  nil)

(deftest simple-bit-vector-p.4
  (simple-bit-vector-p #(1 0 1 1))
  nil)

(deftest bit-and.1
  (let ((a #*11011)
        (b #*01110))
    (bit-and a b))
  #*01010)

(deftest bit-and.2
  (let* ((a #*11011)
         (b #*01110)
         (c (bit-and a b)))
    (values (eq a c) (eq b c)))
  nil nil)

(deftest bit-and.3
  (let* ((a #*11011)
         (b #*01110)
         (c (bit-and a b t)))
    (values c (eq a c) (eq b c)))
  #*01010 t nil)

(deftest bit-and.4
  (let* ((a #*11011)
         (b #*01110)
         (r #*11111)
         (c (bit-and a b r)))
    (values r (eq a c) (eq b c) (eq r c)))
  #*01010 nil nil t)

(deftest bit-and.5
  (let* ((a #*11011)
         (b #*01110)
         (r (make-array 5 :element-type 'bit))
         (c (bit-and a b r)))
    (values r (eq a c) (eq b c) (eq r c) (arrayp c)))
  #*01010 nil nil t t)

(deftest bit-and.6
  (let ((a (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1)))
        (b #*01110))
    (bit-and a b))
  #*01010)

(deftest bit-and.7
  (let* ((a (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1)))
         (b #*01110)
         (c (bit-and a b)))
    (values (eq a c) (eq b c)))
  nil nil)

(deftest bit-and.8
  (let* ((a (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1)))
         (b #*01110)
         (c (bit-and a b t)))
    (values c (eq a c) (eq b c)))
  #*01010 t nil)

(deftest bit-and.9
  (let* ((a (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1)))
         (b #*01110)
         (r #*11111)
         (c (bit-and a b r)))
    (values r (eq a c) (eq b c) (eq r c)))
  #*01010 nil nil t)

(deftest bit-and.10
  (let* ((a (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1)))
         (b #*01110)
         (r (make-array 5 :element-type 'bit))
         (c (bit-and a b r)))
    (values r (eq a c) (eq b c) (eq r c) (arrayp c)))
  #*01010 nil nil t t)

(deftest bit-and.11
  (let ((a #*01110)
        (b (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1))))
    (bit-and a b))
  #*01010)

(deftest bit-and.12
  (let* ((a #*01110)
         (b (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1)))
         (c (bit-and a b)))
    (values (eq a c) (eq b c)))
  nil nil)

(deftest bit-and.13
  (let* ((a #*01110)
         (b (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1)))
         (c (bit-and a b t)))
    (values c (eq a c) (eq b c)))
  #*01010 t nil)

(deftest bit-and.14
  (let* ((a #*01110)
         (b (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1)))
         (r #*11111)
         (c (bit-and a b r)))
    (values r (eq a c) (eq b c) (eq r c)))
  #*01010 nil nil t)

(deftest bit-and.15
  (let* ((a #*01110)
         (b (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1)))
         (r (make-array 5 :element-type 'bit))
         (c (bit-and a b r)))
    (values r (eq a c) (eq b c) (eq r c) (arrayp c)))
  #*01010 nil nil t t)

(deftest bit-and.16
  (let ((a (make-array 5 :element-type 'bit :initial-contents '(0 1 1 1 0)))
        (b (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1))))
    (bit-and a b))
  #*01010)

(deftest bit-and.17
  (let* ((a (make-array 5 :element-type 'bit :initial-contents '(0 1 1 1 0)))
         (b (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1)))
         (c (bit-and a b)))
    (values (eq a c) (eq b c)))
  nil nil)

(deftest bit-and.18
  (let* ((a (make-array 5 :element-type 'bit :initial-contents '(0 1 1 1 0)))
         (b (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1)))
         (c (bit-and a b t)))
    (values c (eq a c) (eq b c)))
  #*01010 t nil)

(deftest bit-and.19
  (let* ((a (make-array 5 :element-type 'bit :initial-contents '(0 1 1 1 0)))
         (b (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1)))
         (r #*11111)
         (c (bit-and a b r)))
    (values r (eq a c) (eq b c) (eq r c)))
  #*01010 nil nil t)

(deftest bit-and.20
  (let* ((a (make-array 5 :element-type 'bit :initial-contents '(0 1 1 1 0)))
         (b (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 1)))
         (r (make-array 5 :element-type 'bit))
         (c (bit-and a b r)))
    (values r (eq a c) (eq b c) (eq r c) (arrayp c)))
  #*01010 nil nil t t)

(deftest bit-andc1.1
  (bit-andc1 #*1100 #*1010)
  #*0010)

(deftest bit-andc2.1
  (bit-andc2 #*1010 #*1100)
  #*0010)

(deftest bit-eqv.1
  (bit-eqv #*1100 #*1010)
  #*0110)

(deftest bit-ior.1
  (bit-ior #*11000001 #*01110011)
  #*11110011)

(deftest bit-nand.1
  (bit-nand #*11000001 #*01110011)
  #*10111110)

(deftest bit-nor.1
  (bit-nor #*11000001 #*01110011)
  #*00001100)

(deftest bit-orc1.1
  (bit-orc1 #*00111110 #*01110011)
  #*11110011)

(deftest bit-orc2.1
  (bit-orc2 #*11000001 #*10001100)
  #*11110011)

(deftest bit-xor.1
  (bit-xor #*1100 #*1010)
  #*0110)

(deftest bit-not.1
  (bit-not #*11001111)
  #*00110000)

(deftest bit-not.2
  (let* ((a #*11001111)
         (b (bit-not a)))
    (values b (eq a b)))
  #*00110000 nil)

(deftest bit-not.3
  (let* ((a #*11001111)
         (b (bit-not a t)))
    (values b (eq a b)))
  #*00110000 t)

(deftest bit-not.4
  (let* ((a #*11001111)
         (r #*11111111)
         (b (bit-not a r)))
    (values b (eq a b) (eq b r)))
  #*00110000 nil t)

(deftest bit-not.5
  (let* ((a #*11001111)
         (r (make-array 8 :element-type 'bit))
         (b (bit-not a r)))
    (values b (eq a b) (eq b r) (arrayp r)))
  #*00110000 nil t t)

(deftest bit-not.6
  (let* ((a (make-array 8 :element-type 'bit :initial-contents '(1 1 0 0 1 1 1 1)))
         (b (bit-not a)))
    (values b (eq a b)))
  #*00110000 nil)

(deftest bit-not.7
  (let* ((a (make-array 8 :element-type 'bit :initial-contents '(1 1 0 0 1 1 1 1)))
         (b (bit-not a t)))
    (values b (eq a b)))
  #*00110000 t)

(deftest bit-not.8
  (let* ((a (make-array 8 :element-type 'bit :initial-contents '(1 1 0 0 1 1 1 1)))
         (r #*11111111)
         (b (bit-not a r)))
    (values b (eq a b) (eq b r)))
  #*00110000 nil t)

(deftest bit-not.9
  (let* ((a (make-array 8 :element-type 'bit :initial-contents '(1 1 0 0 1 1 1 1)))
         (r (make-array 8 :element-type 'bit))
         (b (bit-not a r)))
    (values b (eq a b) (eq b r) (arrayp r)))
  #*00110000 nil t t)

(deftest adjust-array.1
  (adjust-array #() 3)
  #(nil nil nil))

(deftest adjust-array.2
  (let ((a #2a((a b c) (d e f) (g h i) (j k l)))) ;; (4 3)
    (adjust-array a '(3 2)))
  #2a((a b) (d e) (g h)))

(deftest adjust-array.3
  (let ((a #2a((a b c) (d e f) (g h i) (j k l)))) ;; (4 3)
    (adjust-array a '(5 4)))
  #2a((a b c nil) (d e f nil) (g h i nil) (j k l nil) (nil nil nil nil)))

(deftest adjust-array.4
  (let ((a #2a((a b c) (d e f) (g h i) (j k l)))) ;; (4 3)
    (adjust-array a '(2 4) :initial-element 10))
  #2a((a b c 10) (d e f 10)))

(deftest adjust-array.5
  (adjust-array #(10 20 30) 2)
  #(10 20))

(deftest adjust-array.6
  (adjust-array #(10 20 30) 5 :initial-element 999)
  #(10 20 30 999 999))

(deftest adjust-array.7
  (adjust-array "abc" 2)
  "ab")

(deftest adjust-array.8
  (adjust-array "abc" 5 :initial-element #\z)
  "abczz")

(deftest adjust-array.9
  (adjust-array #*101 2)
  #*10)

(deftest adjust-array.10
  (adjust-array #*101 5 :initial-element 1)
  #*10111)

(deftest adjust-array.11
  (adjust-array #(10 20 30) 2 :fill-pointer t)
  #(10 20))

(deftest adjust-array.12
  (adjust-array #(10 20 30) 5 :fill-pointer t)
  #(10 20 30 nil nil))


;;
;;  do-tests
;;
(do-tests :test t)

