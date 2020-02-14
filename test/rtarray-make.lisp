;;
;;  ANSI COMMON LISP: 15. Arrays
;;
(deftest make-array.1
  (arrayp
    (make-array 10))
  t)

(deftest make-array.2
  (length
    (make-array 10))
  10)


;;
;;  dimension
;;
(deftest make-array-dimension.1
  (array-dimensions
    (make-array 10))
  (10))

(deftest make-array-dimension.2
  (array-dimensions
    (make-array '(10)))
  (10))

(deftest make-array-dimension.3
  (array-dimensions
    (make-array '(10 20 30)))
  (10 20 30))

(deftest make-array-dimension.4
  (array-dimensions
    (make-array nil))
  nil)


;;
;;  range
;;
(deftest make-array-range.1
  (let ((x (make-array 5)))
    (setf (aref x 0) 10)
    (setf (aref x 4) 20)
    (values (aref x 0) (aref x 4)))
  10 20)

(deftest-error make-array-range.2
  (let ((x (make-array 5)))
    (setf (aref x 5) 10)))

(deftest-error make-array-range.3
  (let ((x (make-array 5)))
    (aref x 5)))

(deftest-error make-array-range.4
  (let ((x (make-array 5)))
    (setf (aref x -1) 10)))

(deftest make-array-range.5
  (let ((x (make-array '(4 5))))
    (setf (aref x 0 0) 10)
    (setf (aref x 0 4) 20)
    (setf (aref x 3 0) 30)
    (setf (aref x 3 4) 40)
    (values
      (aref x 0 0)
      (aref x 0 4)
      (aref x 3 0)
      (aref x 3 4)))
  10 20 30 40)

(deftest-error make-array-range.6
  (let ((x (make-array '(2 3))))
    (setf (aref x 2 0) 10)))

(deftest-error make-array-range.7
  (let ((x (make-array '(2 3))))
    (aref x 2 0)))

(deftest-error make-array-range.8
  (let ((x (make-array '(2 3))))
    (setf (aref x 0 3) 10)))

(deftest-error make-array-range.9
  (let ((x (make-array '(2 3))))
    (aref x 0 3)))

(deftest-error make-array-range.10
  (let ((x (make-array '(2 3))))
    (aref x 0 0 0)))

(deftest-error make-array-range.11
  (let ((x (make-array '(2 3))))
    (aref x 0)))

(deftest make-array-range.12
  (let ((x (make-array nil)))
    (setf (aref x) 10))
  10)

(deftest make-array-range.13
  (let ((x (make-array nil)))
    (setf (aref x) 10)
    (aref x))
  10)

(deftest-error make-array-range.14
  (let ((x (make-array nil)))
    (setf (aref x 0) 10)))

(deftest-error make-array-range.15
  (let ((x (make-array nil)))
    (aref x 0)))


;;
;;  initial-element
;;
(deftest make-array-initial-element.1
  (make-array 3 :initial-element 44)
  #(44 44 44))

(deftest make-array-initial-element.2
  (make-array '(2 3) :initial-element 44)
  #2a((44 44 44) (44 44 44)))

(deftest make-array-initial-element.3
  (make-array nil :initial-element 44)
  #0a44)


;;
;;  element-type
;;
(deftest make-array-character.1
  (array-element-type
    (make-array 3 :element-type 'character))
  character)

(deftest make-array-character.2
  (let ((x (make-array 3 :element-type 'character)))
    (setf (aref x 0) #\A)
    (setf (aref x 1) #\B)
    (setf (aref x 2) #\C)
    (string= x "ABC"))
  t)

(deftest make-array-character.3
  (stringp
    (make-array 3 :element-type 'character))
  t)

(deftest-error make-array-character.4
  (let ((x (make-array 3 :element-type 'character)))
    (setf (aref x 0) 10)))

(deftest make-array-character.5
  (stringp
    (make-array '(3 4) :element-type 'character))
  nil)

(deftest make-array-bit.1
  (array-element-type
    (make-array 3 :element-type 'bit))
  bit)

(deftest make-array-bit.2
  (let ((x (make-array 3 :element-type 'bit)))
    (setf (aref x 0) 0)
    (setf (aref x 1) 1)
    (setf (aref x 2) 1)
    (equal x #*011))
  t)

(deftest make-array-bit.3
  (typep
    (make-array 3 :element-type 'bit)
    '(vector bit))
  t)

(deftest-error make-array-bit.4
  (let ((x (make-array 3 :element-type 'bit)))
    (setf (aref x 0) #\a)))

(deftest make-array-single-float.1
  (array-element-type
    (make-array 3 :element-type 'single-float))
  single-float)

(deftest make-array-single-float.2
  (let ((x (make-array 3 :element-type 'single-float)))
    (setf (aref x 0) 10.5f0)
    (setf (aref x 1) 1.25f0)
    (setf (aref x 2) 4.125f0)
    x)
  #(10.5f0 1.25f0 4.125f0))

(deftest make-array-single-float.3
  (typep
    (make-array 3 :element-type 'single-float)
    '(vector single-float))
  t)

(deftest-error make-array-single-float.4
  (let ((x (make-array 3 :element-type 'single-float)))
    (setf (aref x 0) #\a)))

(deftest make-array-double-float.1
  (array-element-type
    (make-array 3 :element-type 'double-float))
  double-float)

(deftest make-array-double-float.2
  (let ((x (make-array 3 :element-type 'double-float)))
    (setf (aref x 0) 10.5d0)
    (setf (aref x 1) 1.25d0)
    (setf (aref x 2) 4.125d0)
    x)
  #(10.5d0 1.25d0 4.125d0))

(deftest make-array-double-float.3
  (typep
    (make-array 3 :element-type 'double-float)
    '(vector double-float))
  t)

(deftest-error make-array-double-float.4
  (let ((x (make-array 3 :element-type 'double-float)))
    (setf (aref x 0) #\a)))

(deftest make-array-long-float.1
  (array-element-type
    (make-array 3 :element-type 'long-float))
  long-float)

(deftest make-array-long-float.2
  (let ((x (make-array 3 :element-type 'long-float)))
    (setf (aref x 0) 10.5L0)
    (setf (aref x 1) 1.25L0)
    (setf (aref x 2) 4.125L0)
    x)
  #(10.5L0 1.25L0 4.125L0))

(deftest make-array-long-float.3
  (typep
    (make-array 3 :element-type 'long-float)
    '(vector long-float))
  t)

(deftest-error make-array-long-float.4
  (let ((x (make-array 3 :element-type 'long-float)))
    (setf (aref x 0) #\a)))

(deftest make-array-signed-byte.1
  (values
    (array-element-type (make-array 3 :element-type '(signed-byte 8)))
    (array-element-type (make-array 3 :element-type '(signed-byte 16)))
    (array-element-type (make-array 3 :element-type '(signed-byte 24)))
    (array-element-type (make-array 3 :element-type '(signed-byte 32))))
  (signed-byte 8)
  (signed-byte 16)
  (signed-byte 32)
  (signed-byte 32))

(deftest make-array-signed-byte.2
  (let ((x (make-array 3 :element-type '(signed-byte 8))))
    (setf (aref x 1) 20)
    (aref x 1))
  20)

(deftest make-array-signed-byte.3
  (let ((x (make-array 3 :element-type '(signed-byte 8))))
    (setf (aref x 1) #x7F)
    (aref x 1))
  #x7F)

(deftest make-array-signed-byte.4
  (let ((x (make-array 3 :element-type '(signed-byte 8))))
    (setf (aref x 1) #x-80)
    (aref x 1))
  #x-80)

(deftest-error make-array-signed-byte.5
  (let ((x (make-array 3 :element-type '(signed-byte 8))))
    (setf (aref x 1) #x80)))

(deftest-error make-array-signed-byte.6
  (let ((x (make-array 3 :element-type '(signed-byte 8))))
    (setf (aref x 1) #x-81)))

(deftest-error make-array-signed-byte.7
  (let ((x (make-array 3 :element-type '(signed-byte 8))))
    (setf (aref x 0) #\a)))

(deftest make-array-signed-byte.8
  (let ((x (make-array 3 :element-type '(signed-byte 16))))
    (setf (aref x 1) #x7FFF)
    (aref x 1))
  #x7FFF)

(deftest make-array-signed-byte.9
  (let ((x (make-array 3 :element-type '(signed-byte 16))))
    (setf (aref x 1) #x-8000)
    (aref x 1))
  #x-8000)

(deftest-error make-array-signed-byte.10
  (let ((x (make-array 3 :element-type '(signed-byte 16))))
    (setf (aref x 1) #x8000)))

(deftest-error make-array-signed-byte.11
  (let ((x (make-array 3 :element-type '(signed-byte 16))))
    (setf (aref x 1) #x-8001)))

(deftest-error make-array-signed-byte.12
  (let ((x (make-array 3 :element-type '(signed-byte 16))))
    (setf (aref x 0) #\a)))

(deftest make-array-signed-byte.13
  (let ((x (make-array 3 :element-type '(signed-byte 32))))
    (setf (aref x 1) #x7FFFFFFF)
    (aref x 1))
  #x7FFFFFFF)

(deftest make-array-signed-byte.14
  (let ((x (make-array 3 :element-type '(signed-byte 32))))
    (setf (aref x 1) #x-80000000)
    (aref x 1))
  #x-80000000)

(deftest-error make-array-signed-byte.15
  (let ((x (make-array 3 :element-type '(signed-byte 32))))
    (setf (aref x 1) #x80000000)))

(deftest-error make-array-signed-byte.16
  (let ((x (make-array 3 :element-type '(signed-byte 32))))
    (setf (aref x 1) #x-80000001)))

(deftest-error make-array-signed-byte.17
  (let ((x (make-array 3 :element-type '(signed-byte 32))))
    (setf (aref x 0) #\a)))

#+64-bit
(deftest make-array-signed-byte-64bit.1
  (array-element-type
    (make-array 3 :element-type '(signed-byte 64)))
  (signed-byte 64))

#+64-bit
(deftest make-array-signed-byte-64bit.2
  (let ((x (make-array 3 :element-type '(signed-byte 64))))
    (setf (aref x 1) #x7FFFFFFFFFFFFFFF)
    (aref x 1))
  #x7FFFFFFFFFFFFFFF)

#+64-bit
(deftest make-array-signed-byte-64bit.3
  (let ((x (make-array 3 :element-type '(signed-byte 64))))
    (setf (aref x 1) #x-8000000000000000)
    (aref x 1))
  #x-8000000000000000)

#+64-bit
(deftest-error make-array-signed-byte-64bit.4
  (let ((x (make-array 3 :element-type '(signed-byte 64))))
    (setf (aref x 1) #x8000000000000000)))

#+64-bit
(deftest-error make-array-signed-byte-64bit.5
  (let ((x (make-array 3 :element-type '(signed-byte 64))))
    (setf (aref x 1) #x-8000000000000001)))

#+64-bit
(deftest-error make-array-signed-byte-64bit.6
  (let ((x (make-array 3 :element-type '(signed-byte 64))))
    (setf (aref x 0) #\a)))

(deftest make-array-unsigned-byte.1
  (values
    (array-element-type (make-array 3 :element-type '(unsigned-byte 8)))
    (array-element-type (make-array 3 :element-type '(unsigned-byte 16)))
    (array-element-type (make-array 3 :element-type '(unsigned-byte 24)))
    (array-element-type (make-array 3 :element-type '(unsigned-byte 32))))
  (unsigned-byte 8)
  (unsigned-byte 16)
  (unsigned-byte 32)
  (unsigned-byte 32))

(deftest make-array-unsigned-byte.2
  (let ((x (make-array 3 :element-type '(unsigned-byte 8))))
    (setf (aref x 1) 20)
    (aref x 1))
  20)

(deftest make-array-unsigned-byte.3
  (let ((x (make-array 3 :element-type '(unsigned-byte 8))))
    (setf (aref x 1) #xFF)
    (aref x 1))
  #xFF)

(deftest make-array-unsigned-byte.4
  (let ((x (make-array 3 :element-type '(unsigned-byte 8))))
    (setf (aref x 1) 0)
    (aref x 1))
  0)

(deftest-error make-array-unsigned-byte.5
  (let ((x (make-array 3 :element-type '(unsigned-byte 8))))
    (setf (aref x 1) #x0100)))

(deftest-error make-array-unsigned-byte.6
  (let ((x (make-array 3 :element-type '(unsigned-byte 8))))
    (setf (aref x 1) -1)))

(deftest-error make-array-unsigned-byte.7
  (let ((x (make-array 3 :element-type '(unsigned-byte 8))))
    (setf (aref x 0) #\a)))

(deftest make-array-unsigned-byte.8
  (let ((x (make-array 3 :element-type '(unsigned-byte 16))))
    (setf (aref x 1) #xFFFF)
    (aref x 1))
  #xFFFF)

(deftest make-array-unsigned-byte.9
  (let ((x (make-array 3 :element-type '(unsigned-byte 16))))
    (setf (aref x 1) 0)
    (aref x 1))
  0)

(deftest-error make-array-unsigned-byte.10
  (let ((x (make-array 3 :element-type '(unsigned-byte 16))))
    (setf (aref x 1) #x010000)))

(deftest-error make-array-unsigned-byte.11
  (let ((x (make-array 3 :element-type '(unsigned-byte 16))))
    (setf (aref x 1) -1)))

(deftest-error make-array-unsigned-byte.12
  (let ((x (make-array 3 :element-type '(unsigned-byte 16))))
    (setf (aref x 0) #\a)))

(deftest make-array-unsigned-byte.13
  (let ((x (make-array 3 :element-type '(unsigned-byte 32))))
    (setf (aref x 1) #xFFFFFFFF)
    (aref x 1))
  #xFFFFFFFF)

(deftest make-array-unsigned-byte.14
  (let ((x (make-array 3 :element-type '(unsigned-byte 32))))
    (setf (aref x 1) 0)
    (aref x 1))
  0)

(deftest-error make-array-unsigned-byte.15
  (let ((x (make-array 3 :element-type '(unsigned-byte 32))))
    (setf (aref x 1) #x0100000000)))

(deftest-error make-array-unsigned-byte.16
  (let ((x (make-array 3 :element-type '(unsigned-byte 32))))
    (setf (aref x 1) -1)))

(deftest-error make-array-unsigned-byte.17
  (let ((x (make-array 3 :element-type '(unsigned-byte 32))))
    (setf (aref x 0) #\a)))

#+64-bit
(deftest make-array-unsigned-byte-64bit.1
  (array-element-type
    (make-array 3 :element-type '(unsigned-byte 64)))
  (unsigned-byte 64))

#+64-bit
(deftest make-array-unsigned-byte-64bit.2
  (let ((x (make-array 3 :element-type '(unsigned-byte 64))))
    (setf (aref x 1) #xFFFFFFFFFFFFFFFF)
    (aref x 1))
  #xFFFFFFFFFFFFFFFF)

#+64-bit
(deftest make-array-unsigned-byte-64bit.3
  (let ((x (make-array 3 :element-type '(unsigned-byte 64))))
    (setf (aref x 1) 0)
    (aref x 1))
  0)

#+64-bit
(deftest-error make-array-unsigned-byte-64bit.4
  (let ((x (make-array 3 :element-type '(unsigned-byte 64))))
    (setf (aref x 1) #x010000000000000000)))

#+64-bit
(deftest-error make-array-unsigned-byte-64bit.5
  (let ((x (make-array 3 :element-type '(unsigned-byte 64))))
    (setf (aref x 1) -1)))

#+64-bit
(deftest-error make-array-unsigned-byte-64bit.6
  (let ((x (make-array 3 :element-type '(unsigned-byte 64))))
    (setf (aref x 0) #\a)))


;;
;;  initial-contents
;;
(deftest make-array-initial-contents.1
  (make-array 3 :initial-contents '(10 20 30))
  #(10 20 30))

(deftest make-array-initial-contents.2
  (make-array 3 :initial-contents #(10 20 30))
  #(10 20 30))

(deftest-error make-array-initial-contents.3
  (make-array 3 :initial-contents '(10 20)))

(deftest-error make-array-initial-contents.4
  (make-array 3 :initial-contents '(10 20 30 40)))

(deftest make-array-initial-contents.5
  (make-array 3 :initial-contents '((10) (20) (30)))
  #((10) (20) (30)))

(deftest make-array-initial-contents.6
  (make-array nil :initial-contents 10)
  #0a10)

(deftest make-array-initial-contents.7
  (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))
  #2a((1 2 3) (4 5 6)))

(deftest-error make-array-initial-contents.8
  (make-array '(2 3) :initial-contents '((1 2) (3 4) (5 6))))

(deftest make-array-initial-contents.9
  (make-array '(5) :initial-contents "Hello")
  #(#\H #\e #\l #\l #\o))

(deftest make-array-initial-contents.10
  (make-array '(5) :element-type 'character :initial-contents "Hello")
  "Hello")

(deftest-error make-array-initial-contents.11
  (make-array 3 :element-type 'character :initial-contents '(10 20 30)))

(deftest-error make-array-initial-contents.12
  (make-array 3 :element-type 'single-float :initial-contents '(10 20 30)))


;;
;;  fill-pointer
;;
(deftest make-array-fill-pointer.1
  (fill-pointer
    (make-array 3 :fill-pointer t))
  3)

(deftest-error make-array-fill-pointer.2
  (fill-pointer
    (make-array 3 :fill-pointer nil)))

(deftest make-array-fill-pointer.3
  (values
    (array-has-fill-pointer-p (make-array 3 :fill-pointer nil))
    (array-has-fill-pointer-p (make-array 3 :fill-pointer t))
    (array-has-fill-pointer-p (make-array 3 :fill-pointer 2)))
  nil t t)

(deftest make-array-fill-pointer.4
  (values
    (array-total-size (make-array 3 :fill-pointer nil))
    (array-total-size (make-array 3 :fill-pointer t))
    (array-total-size (make-array 3 :fill-pointer 2)))
  3 3 3)

(deftest make-array-fill-pointer.5
  (fill-pointer
    (make-array 3 :fill-pointer 2))
  2)

(deftest make-array-fill-pointer.6
  (fill-pointer
    (make-array 3 :fill-pointer 3))
  3)

(deftest-error make-array-fill-pointer.7
  (make-array 3 :fill-pointer 4))

(deftest-error make-array-fill-pointer.8
  (make-array 3 :fill-pointer -1))

(deftest-error make-array-fill-pointer.9
  (make-array nil :fill-pointer t))

(deftest-error make-array-fill-pointer.10
  (make-array '(2 3) :fill-pointer t))

(deftest make-array-fill-pointer.11
  (fill-pointer
    (make-array '(2) :fill-pointer t))
  2)

(deftest make-array-fill-pointer.12
  (make-array 3 :fill-pointer 2 :initial-contents '(1 2 3))
  #(1 2))

(deftest-error make-array-fill-pointer.13
  (make-array 3 :fill-pointer 2 :initial-contents '(1 2)))

(deftest make-array-fill-pointer.14
  (let ((x (make-array 10 :element-type 'character
                       :fill-pointer 5
                       :initial-contents "HelloABCDE")))
    (values
      (string= x "Hello")
      (string-equal x "HeLLO")
      (equal x "Hello")
      (equalp x "HeLLO")))
  t t t t)

(deftest make-array-fill-pointer.15
  (let ((x (make-array 100 :element-type 'character :fill-pointer 0)))
    (vector-push #\A x)
    (vector-push #\B x)
    (vector-push #\C x)
    (values x (length x)))
  "ABC" 3)

(deftest make-array-fill-pointer.16
  (let ((x (make-array 100 :element-type 'character :fill-pointer 0)))
    (vector-push #\A x)
    (vector-push #\B x)
    (vector-push #\C x)
    (array-total-size x))
  100)

(deftest make-array-fill-pointer.17
  (let ((x (make-array 100 :element-type 'character :fill-pointer 0)))
    (format x "Hello: ~S" 200)
    x)
  "Hello: 200")

(deftest-error make-array-fill-pointer.18
  (let ((x (make-array 4 :element-type 'character :fill-pointer 0)))
    (format x "Hello: ~S" 200)))

(deftest-error make-array-fill-pointer.19
  (let ((x (make-array 4 :element-type 'character :fill-pointer 0)))
    (format x "Hello")))

(deftest make-array-fill-pointer.20
  (let ((x (make-array 4 :element-type 'character :fill-pointer 0)))
    (format x "Hell")
    x)
  "Hell")


;;
;;  adjustable
;;
(deftest make-array-adjustable.1
  (arrayp
    (make-array 10 :adjustable t))
  t)

(deftest make-array-adjustable.2
  (arrayp
    (make-array 10 :adjustable nil))
  t)

(deftest make-array-adjustable.3
  (arrayp
    (make-array 10 :adjustable 10))
  t)

(deftest make-array-adjustable.4
  (let ((x (make-array 2 :fill-pointer 0 :adjustable t)))
    (vector-push-extend 10 x)
    (vector-push-extend 20 x)
    (vector-push-extend 30 x)
    x)
  #(10 20 30))

(deftest-error make-array-adjustable.5
  (let ((x (make-array 2 :fill-pointer 0 :adjustable nil)))
    (vector-push-extend 10 x)
    (vector-push-extend 20 x)
    (vector-push-extend 30 x)))

(deftest make-array-adjustable.6
  (let ((x (make-array 4 :element-type 'character :fill-pointer 0 :adjustable t)))
    (format x "Hello: ~S" 200)
    x)
  "Hello: 200")


;;
;;  displaced-to
;;
(deftest make-array-displaced.1
  (let* ((x (make-array 10 :initial-element 99))
         (y (make-array 5 :displaced-to x)))
    (values x y))
  #(99 99 99 99 99  99 99 99 99 99)
  #(99 99 99 99 99))

(deftest make-array-displaced.2
  (let* ((x (make-array 10 :initial-element 99))
         (y (make-array 5 :displaced-to x)))
    (setf (aref x 3) 111)
    (values x y))
  #(99 99 99 111 99  99 99 99 99 99)
  #(99 99 99 111 99))

(deftest-error make-array-displaced.3
  (let* ((x (make-array 5 :initial-element 99))
         (y (make-array 6 :displaced-to x)))
    y))

(deftest make-array-displaced.4
  (let* ((x (make-array 5 :initial-element 99))
         (y (make-array 5 :displaced-to x)))
    y)
  #(99 99 99 99 99))

(deftest make-array-displaced.5
  (let* ((x (make-array 5 :initial-element 99))
         (y (make-array 5 :displaced-to x)))
    y)
  #(99 99 99 99 99))

(deftest make-array-displaced.6
  (let* ((x (make-array 10 :initial-contents '(1 2 3 4 5 6 7 8 9 0)))
         (y (make-array '(2 3) :displaced-to x)))
    y)
  #2a((1 2 3) (4 5 6)))

(deftest make-array-displaced.7
  (let* ((x (make-array 10 :initial-contents '(1 2 3 4 5 6 7 8 9 0)))
         (y (make-array '(2 3) :displaced-to x)))
    (setf (aref x 4) 100)
    y)
  #2a((1 2 3) (4 100 6)))

(deftest-error make-array-displaced.8
  (let* ((x (make-array 10 :element-type '(unsigned-byte 8)
                        :initial-contents '(1 2 3 4 5 6 7 8 9 0)))
         (y (make-array '(2 3) :displaced-to x)))
    y))

(deftest make-array-displaced.9
  (let* ((x (make-array 10 :initial-element 99))
         (y (make-array 5 :displaced-to x :displaced-index-offset 2)))
    (values x y))
  #(99 99 99 99 99  99 99 99 99 99)
  #(99 99 99 99 99))

(deftest make-array-displaced.10
  (let* ((x (make-array 10 :initial-element 99))
         (y (make-array 5 :displaced-to x :displaced-index-offset 5)))
    (values x y))
  #(99 99 99 99 99  99 99 99 99 99)
  #(99 99 99 99 99))

(deftest-error make-array-displaced.11
  (let* ((x (make-array 10 :initial-element 99))
         (y (make-array 5 :displaced-to x :displaced-index-offset 6)))
    (values x y)))

(deftest make-array-displaced.12
  (let* ((x (make-array 10 :initial-contents '(1 2 3 4 5 6 7 8 9 0)))
         (y (make-array 5 :displaced-to x :displaced-index-offset 2)))
    (values x y))
  #(1 2 3 4 5 6 7 8 9 0)
  #(3 4 5 6 7))

(deftest make-array-displaced.13
  (let* ((x (make-array 10 :initial-contents '(1 2 3 4 5 6 7 8 9 0)))
         (y (make-array '(2 3) :displaced-to x :displaced-index-offset 2)))
    (setf (aref x 4) 100)
    y)
  #2a((3 4 100) (6 7 8)))

(deftest make-array-displaced.14
  (let* ((x (make-array 10 :element-type '(unsigned-byte 16)
                        :initial-contents '(1 2 3 4 5 6 7 8 9 0)))
         (y (make-array '(2 3) :element-type '(unsigned-byte 16)
                        :displaced-to x :displaced-index-offset 2)))
    (setf (aref x 4) 100)
    y)
  #2a((3 4 100) (6 7 8)))


;;
;;  displaced-to vector
;;
(deftest make-array-displaced-vector.1
  (let* ((x #(1 2 3 4 5 6 7 8 0 0))
         (y (make-array '(2 3) :displaced-to x)))
    y)
  #2a((1 2 3) (4 5 6)))

(deftest-error make-array-displaced-vector.2
  (let* ((x #(1 1 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1))
         (y (make-array '(2 3) :element-type 'bit :displaced-to x)))
    y))


;;
;;  element-type not
;;
(deftest make-array-not.1
  (array-element-type
    (make-array 3 :element-type '(not character)))
  t)

(deftest make-array-not.2
  (array-element-type
    (make-array 3 :element-type '(not bit)))
  t)

(deftest make-array-not.3
  (array-element-type
    (make-array 3 :element-type '(not single-float)))
  t)

(deftest make-array-not.4
  (array-element-type
    (make-array 3 :element-type '(not double-float)))
  t)

(deftest make-array-not.5
  (array-element-type
    (make-array 3 :element-type '(not long-float)))
  t)

(deftest make-array-not.6
  (array-element-type
    (make-array 3 :element-type '(not (signed-byte 8))))
  t)

(deftest make-array-not.7
  (array-element-type
    (make-array 3 :element-type '(not (unsigned-byte 8))))
  t)

