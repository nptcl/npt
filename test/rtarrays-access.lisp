;;
;;  ANSI COMMON LISP: 15. Arrays
;;

;;
;;  Accessor AREF
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

(deftest aref.10
  (aref (make-array 3 :element-type 'character
                    :initial-contents "ABC")
        1)
  #\B)

(deftest aref.11
  (aref (make-array 5 :initial-contents "ABCDE" :fill-pointer 2) 1)
  #\B)

(deftest aref.12
  (aref (make-array 5 :initial-contents "ABCDE" :fill-pointer 2) 2)
  #\C)

(deftest-error aref-error.1
  (aref (make-array 10) 10))

(deftest-error aref-error.2
  (aref (make-array '(4 5 6)) 3 4 6))

(deftest-error aref-error.3
  (aref (make-array nil) 0))

(deftest-error aref-error.4
  (eval '(make-array :hello))
  type-error)

(deftest-error! aref-error.5
  (eval '(make-array)))


;;
;;  Accessor (SETF AREF)
;;
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


(deftest setf-aref.6
  (let ((x (make-array 3 :element-type 'character :initial-contents "ABC")))
    (setf (aref x 1) #\Z)
    x)
  "AZC")

(deftest setf-aref.7
  (let ((x (make-array 5 :initial-contents "ABCDE" :fill-pointer 2)))
    (setf (aref x 1) #\Z)
    x)
  #(#\A #\Z))

(deftest setf-aref.8
  (let ((x (make-array 5 :initial-contents "ABCDE" :fill-pointer 2)))
    (setf (aref x 2) #\Z)
    x)
  #(#\A #\B))

(deftest setf-aref.9
  (let ((x (make-array 5 :initial-contents "ABCDE" :fill-pointer 2)))
    (setf (aref x 2) #\Z)
    (setf (fill-pointer x) 3)
    x)
  #(#\A #\B #\Z))

(deftest-error setf-aref-error.1
  (let ((x (make-array nil)))
    (setf (aref x 0) 100)))

(deftest-error setf-aref-error.2
  (let ((x (make-array 10)))
    (setf (aref x 10) 100)))

(deftest-error setf-aref-error.3
  (let ((x (make-array '(10 20 30))))
    (setf (aref x 9 19 30) 100)))

(deftest-error setf-aref-error.4
  (eval '(setf (aref :hello) 10)))

(deftest-error! setf-aref-error.5
  (eval '(setf (aref) 10)))


;;  ANSI Common Lisp
(defvar *aref-test-1*)
(defvar *aref-test-2*)
(defvar *aref-test-3*)

(deftest aref-test.1
  (aref (setq *aref-test-1* (make-array 4)) 3)
  nil)

(deftest aref-test.2
  (setf (aref *aref-test-1* 3) 'sirens)
  sirens)

(deftest aref-test.3
  (aref *aref-test-1* 3)
  sirens)

(deftest aref-test.4
  (aref (setq *aref-test-2*
              (make-array '(2 4) :element-type '(unsigned-byte 2)
                          :initial-contents '((0 1 2 3) (3 2 1 0))))
        1 2)
  1)

(deftest aref-test.5
  (progn
    (setq *aref-test-3* '(0 2))
    (apply #'aref *aref-test-2* *aref-test-3*))
  2)

(deftest aref-test.6
  (setf (apply #'aref *aref-test-2* *aref-test-3*) 3)
  3)

(deftest aref-test.7
  (apply #'aref *aref-test-2* *aref-test-3*)
  3)

(deftest aref-test.8
  (aref *aref-test-2* 0 2)
  3)


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

;;  ANSI Common Lisp
(defvar *array-row-major-index-test-1*)

(deftest array-row-major-index-test.1
  (progn
    (setq *array-row-major-index-test-1*
          (make-array '(4 7) :element-type '(unsigned-byte 8)))
    (array-row-major-index *array-row-major-index-test-1* 1 2))
  9)

(deftest array-row-major-index-test.2
  (array-row-major-index
    (make-array '(2 3 4)
                :element-type '(unsigned-byte 8)
                :displaced-to *array-row-major-index-test-1*
                :displaced-index-offset 4)
    0 2 1)
  9)


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

