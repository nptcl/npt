;;
;;  ANSI COMMON LISP: 15. Arrays
;;

;;
;;  Function ADJUSTABLE-ARRAY-P
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

(deftest adjustable-array-p.7
  (adjustable-array-p
    (make-array 5 :element-type 'character
                :adjustable t
                :fill-pointer 3))
  t)

(deftest adjustable-array-p.8
  (adjustable-array-p
    (make-array 4))
  nil)

(deftest-error adjustable-array-p-error.1
  (eval '(adjustable-array-p (list 10 20 30)))
  type-error)

(deftest-error adjustable-array-p-error.2
  (eval '(adjustable-array-p :hello))
  type-error)

(deftest-error! adjustable-array-p-error.3
  (eval '(adjustable-array-p)))

(deftest-error! adjustable-array-p-error.4
  (eval '(adjustable-array-p #() #())))


;;
;;  Function ARRAY-DIMENSION
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
  (array-dimension (make-array '(2 3)) 2))

(deftest-error array-dimension.5
  (array-dimension (make-array nil) 0))

(deftest array-dimension.6
  (array-dimension "Hello" 0)
  5)

(deftest array-dimension.7
  (array-dimension #(10 20 30) 0)
  3)

(deftest array-dimension.8
  (array-dimension #*0011001100 0)
  10)

(deftest-error array-dimension.9
  (array-dimension #*0011001100 1))


(deftest-error array-dimension.10
  (array-dimension (make-array '(2 3 4)) 3))

(deftest array-dimension.11
  (array-dimension
    (make-array 5 :fill-pointer 2)
    0)
  5)

(deftest-error array-dimension-error.1
  (eval '(array-dimension 10 0))
  type-error)

(deftest-error array-dimension-error.2
  (eval '(array-dimension "Hello" :hello))
  type-error)

(deftest-error! array-dimension-error.3
  (eval '(array-dimension "Hello")))

(deftest-error! array-dimension-error.4
  (eval '(array-dimension "Hello" 0 0)))

;;  ANSI Common Lisp
(deftest array-dimension-test.1
  (array-dimension (make-array 4) 0)
  4)

(deftest array-dimension-test.2
  (array-dimension (make-array '(2 3)) 1)
  3)


;;
;;  Function ARRAY-DIMENSIONS
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

(deftest-error array-dimensions-error.1
  (eval '(array-dimensions 100))
  type-error)

(deftest-error! array-dimensions-error.2
  (eval '(array-dimensions)))

(deftest-error! array-dimensions-error.3
  (eval '(array-dimensions "Hello" nil)))

;;  ANSI Common Lisp
(deftest array-dimensions-test.1
  (array-dimensions (make-array 4))
  (4))

(deftest array-dimensions-test.2
  (array-dimensions (make-array '(2 3)))
  (2 3))

(deftest array-dimensions-test.3
  (array-dimensions (make-array 4 :fill-pointer 2))
  (4))


;;
;;  Function ARRAY-HAS-FILL-POINTER-P
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

(deftest-error array-has-fill-pointer-p-error.1
  (eval '(array-has-fill-pointer-p 10))
  type-error)

(deftest-error! array-has-fill-pointer-p-error.2
  (eval '(array-has-fill-pointer-p)))

(deftest-error! array-has-fill-pointer-p-error.3
  (eval '(array-has-fill-pointer-p "Hello" nil)))

;;  ANSI Common Lisp
(deftest array-has-fill-pointer-p-test.1
  (array-has-fill-pointer-p (make-array 4))
  nil)

(deftest array-has-fill-pointer-p-test.2
  (array-has-fill-pointer-p (make-array '(2 3)))
  nil)

(deftest array-has-fill-pointer-p-test.3
  (array-has-fill-pointer-p
    (make-array 8 :fill-pointer 2
                :initial-element 'filler))
  t)


;;
;;  Function ARRAY-DISPLACEMENT
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

(deftest-error array-displacement-error.1
  (eval '(array-displacement 100))
  type-error)

(deftest-error! array-displacement-error.2
  (eval '(array-displacement)))

(deftest-error! array-displacement-error.3
  (eval '(array-displacement "Hello" nil)))

;;  ANSI Common Lisp
(defvar *array-displacement-1*)
(defvar *array-displacement-2*)
(defvar *array-displacement-3*)

(deftest array-displacement-test.1
  (arrayp
    (setq *array-displacement-1* (make-array 5)))
  t)

(deftest array-displacement-test.2
  (arrayp
    (setq *array-displacement-2*
          (make-array 4 :displaced-to *array-displacement-1*
                      :displaced-index-offset 1)))
  t)

(deftest array-displacement-test.3
  (multiple-value-bind (x y) (array-displacement *array-displacement-2*)
    (values (arrayp x) y))
  t 1)

(deftest array-displacement-test.4
  (arrayp
    (setq *array-displacement-3*
          (make-array 2 :displaced-to *array-displacement-2*
                      :displaced-index-offset 2)))
  t)

(deftest array-displacement-test.5
  (multiple-value-bind (x y) (array-displacement *array-displacement-3*)
    (values (arrayp x) y))
  t 2)


;;
;;  Function ARRAY-IN-BOUNDS-P
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

(deftest-error array-in-bounds-p-error.1
  (eval '(array-in-bounds-p 10 20 30))
  type-error)

(deftest-error array-in-bounds-p-error.2
  (eval '(array-in-bounds-p "Hello" :hello))
  type-error)

(deftest-error! array-in-bounds-p-error.3
  (eval '(array-in-bounds-p)))

;;  ANSI Common Lisp
(defvar *array-in-bounds-p-1*)

(deftest array-in-bounds-p-test.1
  (progn
    (setq *array-in-bounds-p-1*
          (make-array '(7 11) :element-type 'standard-char))
    (array-in-bounds-p *array-in-bounds-p-1* 0  0))
  t)

(deftest array-in-bounds-p-test.2
  (array-in-bounds-p *array-in-bounds-p-1* 6 10)
  t)

(deftest array-in-bounds-p-test.3
  (array-in-bounds-p *array-in-bounds-p-1* 0 -1)
  nil)

(deftest array-in-bounds-p-test.4
  (array-in-bounds-p *array-in-bounds-p-1* 0 11)
  nil)

(deftest array-in-bounds-p-test.5
  (array-in-bounds-p *array-in-bounds-p-1* 7  0)
  nil)


;;
;;  Function ARRAY-RANK
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

(deftest-error array-rank-error.1
  (eval '(array-rank (list 10 20 30)))
  type-error)

(deftest-error! array-rank-error.2
  (eval '(array-rank)))

(deftest-error! array-rank-error.3
  (eval '(array-rank "Hello" nil)))

;;  ANSI Common Lisp
(deftest array-rank-test.1
  (array-rank (make-array '()))
  0)

(deftest array-rank-test.2
  (array-rank (make-array 4))
  1)

(deftest array-rank-test.3
  (array-rank (make-array '(4)))
  1)

(deftest array-rank-test.4
  (array-rank (make-array '(2 3)))
  2)


;;
;;  Function ARRAY-TOTAL-SIZE
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

(deftest-error array-total-size-error.1
  (eval '(array-total-size 100))
  type-error)

(deftest-error! array-total-size-error.2
  (eval '(array-total-size)))

(deftest-error! array-total-size-error.3
  (eval '(array-total-size "Hello" nil)))

;;  ANSI Common Lisp
(deftest array-total-size-test.1
  (array-total-size (make-array 4))
  4)

(deftest array-total-size-test.2
  (array-total-size (make-array 4 :fill-pointer 2))
  4)

(deftest array-total-size-test.3
  (array-total-size (make-array 0))
  0)

(deftest array-total-size-test.4
  (array-total-size (make-array '(4 2)))
  8)

(deftest array-total-size-test.5
  (array-total-size (make-array '(4 0)))
  0)

(deftest array-total-size-test.6
  (array-total-size (make-array '()))
  1)


;;
;;  array-dimension-limit
;;
(deftest array-dimension-limit.1
  (integerp array-dimension-limit)
  t)

(deftest array-dimension-limit.2
  (< array-dimension-limit 1024)
  nil)

(deftest-error array-dimension-limit.3
  (eval '(setq array-dimension-limit 2048)))


;;
;;  array-rank-limit
;;
(deftest array-rank-limit.1
  (integerp array-rank-limit)
  t)

(deftest array-rank-limit.2
  (< array-rank-limit 8)
  nil)

(deftest-error array-rank-limit.3
  (eval '(setq array-rank-limit 16)))


;;
;;  array-total-size-limit
;;
(deftest array-total-size-limit.1
  (integerp array-total-size-limit)
  t)

(deftest array-total-size-limit.2
  (< array-total-size-limit 1024)
  nil)

(deftest-error array-total-size-limit.3
  (eval '(setq array-total-size-limit 2048)))

