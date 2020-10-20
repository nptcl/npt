;;
;;  ANSI COMMON LISP: 15. Arrays
;;

;;
;;  Accessor BIT
;;
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

(deftest bit.6
  (let ((a (make-array 5 :element-type 'bit
                       :initial-contents '(1 0 0 1 1)
                       :fill-pointer 2)))
    (values
      (bit a 0)
      (bit a 1)
      (bit a 2)
      (bit a 3)
      (bit a 4)))
  1 0 0 1 1)

(deftest bit.7
  (let ((a (make-array nil :element-type 'bit :initial-contents 1)))
    (bit a))
  1)

(deftest-error bit-error.1
  (eval '(bit "Hello" 1))
  type-error)

(deftest-error bit-error.2
  (eval '(bit #(10 20 30) 1))
  type-error)

(deftest-error bit-error.3
  (eval '(bit (make-array 3) 1))
  type-error)

(deftest-error bit-error.4
  (eval '(bit 10 1))
  type-error)

(deftest-error bit-error.5
  (eval '(bit #*10110 -1))
  type-error)

(deftest-error bit-error.6
  (eval '(bit #*10110 5)))

(deftest-error bit-error.7
  (eval '(bit #*10110 :hello))
  type-error)

(deftest-error bit-error.8
  (eval '(bit #*10110 1 2)))

(deftest-error bit-error.9
  (eval '(bit #*10110)))

(deftest-error bit-error.10
  (let ((a (make-array '(2 3) :element-type 'bit
                       :initial-contents '((1 0 0) (0 1 1)))))
    (bit a 0 3)))

(deftest-error bit-error.11
  (let ((a (make-array '(2 3) :element-type 'bit
                       :initial-contents '((1 0 0) (0 1 1)))))
    (bit a 2 0)))

(deftest-error! bit-error.12
  (eval '(bit)))


;;
;;  Accessor (SETF BIT)
;;
(deftest setf-bit.1
  (let ((a (make-array 5 :element-type 'bit :initial-contents '(1 0 0 1 1))))
    (setf (bit a 1) 1))
  1)

(deftest setf-bit.2
  (let ((a (make-array 5 :element-type 'bit :initial-contents '(1 0 0 1 1))))
    (setf (bit a 0) 0)
    (setf (bit a 1) 1)
    (setf (bit a 2) 1)
    a)
  #*01111)

(deftest setf-bit.3
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

(deftest setf-bit.4
  (let ((a (make-array 5 :element-type 'bit
                       :initial-contents '(1 0 0 1 1)
                       :fill-pointer 2)))
    (setf (bit a 1) 1)
    (setf (bit a 2) 1)
    (setf (bit a 3) 0)
    (setf (bit a 4) 0)
    (values
      (bit a 0)
      (bit a 1)
      (bit a 2)
      (bit a 3)
      (bit a 4)
      a))
  1 1 1 0 0 #*11)

(deftest setf-bit.5
  (let ((a (make-array nil :element-type 'bit :initial-contents 1)))
    (setf (bit a) 0)
    (bit a))
  0)

(deftest-error setf-bit-error.1
  (eval '(setf (bit "Hello" 1) #\A))
  type-error)

(deftest-error setf-bit-error.2
  (eval '(setf (bit #(10 20 30) 1) 99))
  type-error)

(deftest-error setf-bit-error.3
  (eval '(setf (bit (make-array 3) 1) :hello))
  type-error)

(deftest-error setf-bit-error.4
  (eval '(setf (bit 10 1) 20))
  type-error)

(deftest-error setf-bit-error.5
  (eval '(setf (bit #*10110 -1) 1))
  type-error)

(deftest-error setf-bit-error.6
  (eval '(setf (bit #*10110 5) 1)))

(deftest-error setf-bit-error.7
  (eval '(setf (bit #*10110 :hello) 1))
  type-error)

(deftest-error setf-bit-error.8
  (eval '(setf (bit #*10110 1 2) 1)))

(deftest-error setf-bit-error.9
  (eval '(setf (bit #*10110 1) :hello))
  type-error)

(deftest-error setf-bit-error.10
  (eval '(setf (bit #*10110) 1)))

(deftest-error setf-bit-error.11
  (let ((a (make-array '(2 3) :element-type 'bit
                       :initial-contents '((1 0 0) (0 1 1)))))
    (setf (bit a 0 3) 1)))

(deftest-error setf-bit-error.12
  (let ((a (make-array '(2 3) :element-type 'bit
                       :initial-contents '((1 0 0) (0 1 1)))))
    (setf (bit a 2 0) 0)))

(deftest-error! setf-bit-error.13
  (eval '(setf (bit) 1)))


;;
;;  Accessor SBIT
;;
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

(deftest-error sbit.6
  (let ((a (make-array 5 :element-type 'bit
                       :initial-contents '(1 0 0 1 1)
                       :fill-pointer 2)))
    (values
      (sbit a 0)
      (sbit a 1)
      (sbit a 2)
      (sbit a 3)
      (sbit a 4)))
  type-error)

(deftest sbit.7
  (let ((a (make-array nil :element-type 'bit :initial-contents 1)))
    (sbit a))
  1)

(deftest-error sbit-error.1
  (eval '(sbit "Hello" 1))
  type-error)

(deftest-error sbit-error.2
  (eval '(sbit #(10 20 30) 1))
  type-error)

(deftest-error sbit-error.3
  (eval '(sbit (make-array 3) 1))
  type-error)

(deftest-error sbit-error.4
  (eval '(sbit 10 1))
  type-error)

(deftest-error sbit-error.5
  (eval '(sbit #*10110 -1))
  type-error)

(deftest-error sbit-error.6
  (eval '(sbit #*10110 5)))

(deftest-error sbit-error.7
  (eval '(sbit #*10110 :hello))
  type-error)

(deftest-error sbit-error.8
  (eval '(sbit #*10110 1 2)))

(deftest-error sbit-error.9
  (eval '(sbit #*10110)))

(deftest-error sbit-error.10
  (let ((a (make-array '(2 3) :element-type 'bit
                       :initial-contents '((1 0 0) (0 1 1)))))
    (sbit a 0 3)))

(deftest-error sbit-error.11
  (let ((a (make-array '(2 3) :element-type 'bit
                       :initial-contents '((1 0 0) (0 1 1)))))
    (sbit a 2 0)))

(deftest-error! sbit-error.12
  (eval '(sbit)))


;;
;;  Accessor (SETF SBIT)
;;
(deftest setf-sbit.1
  (let ((a (make-array 5 :element-type 'bit :initial-contents '(1 0 0 1 1))))
    (setf (sbit a 1) 1))
  1)

(deftest setf-sbit.2
  (let ((a (make-array 5 :element-type 'bit :initial-contents '(1 0 0 1 1))))
    (setf (sbit a 0) 0)
    (setf (sbit a 1) 1)
    (setf (sbit a 2) 1)
    a)
  #*01111)

(deftest setf-sbit.3
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

(deftest-error setf-sbit.4
  (let ((a (make-array 5 :element-type 'bit
                       :initial-contents '(1 0 0 1 1)
                       :fill-pointer 2)))
    (setf (sbit a 1) 1)
    (setf (sbit a 2) 1)
    (setf (sbit a 3) 0)
    (setf (sbit a 4) 0)
    (values
      (sbit a 0)
      (sbit a 1)
      (sbit a 2)
      (sbit a 3)
      (sbit a 4)
      a))
  type-error)

(deftest setf-sbit.5
  (let ((a (make-array nil :element-type 'bit :initial-contents 1)))
    (setf (sbit a) 0)
    (sbit a))
  0)

(deftest-error setf-sbit-error.1
  (eval '(setf (sbit "Hello" 1) #\A))
  type-error)

(deftest-error setf-sbit-error.2
  (eval '(setf (sbit #(10 20 30) 1) 99))
  type-error)

(deftest-error setf-sbit-error.3
  (eval '(setf (sbit (make-array 3) 1) :hello))
  type-error)

(deftest-error setf-sbit-error.4
  (eval '(setf (sbit 10 1) 20))
  type-error)

(deftest-error setf-sbit-error.5
  (eval '(setf (sbit #*10110 -1) 1))
  type-error)

(deftest-error setf-sbit-error.6
  (eval '(setf (sbit #*10110 5) 1)))

(deftest-error setf-sbit-error.7
  (eval '(setf (sbit #*10110 :hello) 1))
  type-error)

(deftest-error setf-sbit-error.8
  (eval '(setf (sbit #*10110 1 2) 1)))

(deftest-error setf-sbit-error.9
  (eval '(setf (sbit #*10110 1) :hello))
  type-error)

(deftest-error setf-sbit-error.10
  (eval '(setf (sbit #*10110) 1)))

(deftest-error setf-sbit-error.11
  (let ((a (make-array '(2 3) :element-type 'bit
                       :initial-contents '((1 0 0) (0 1 1)))))
    (setf (sbit a 0 3) 1)))

(deftest-error setf-sbit-error.12
  (let ((a (make-array '(2 3) :element-type 'bit
                       :initial-contents '((1 0 0) (0 1 1)))))
    (setf (sbit a 2 0) 0)))

(deftest-error! setf-sbit-error.13
  (eval '(setf (sbit) 1)))

;;  ANSI Common Lisp
(deftest bit-test.1
  (bit (setq *bit-1* (make-array 8 :element-type 'bit
                                 :initial-element 1))
       3)
  1)

(deftest bit-test.2
  (setf (bit *bit-1* 3) 0)
  0)

(deftest bit-test.3
  (bit *bit-1* 3)
  0)

(deftest bit-test.4
  (sbit *bit-1* 5)
  1)

(deftest bit-test.5
  (setf (sbit *bit-1* 5) 1)
  1)

(deftest bit-test.6
  (sbit *bit-1* 5)
  1)


;;
;;  BIT-OPERATION
;;
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


;;  error
(deftest-error bit-and-error.1
  (eval '(bit-and 10 #*1100))
  type-error)

(deftest-error bit-and-error.2
  (eval '(bit-and #*1100 20))
  type-error)

(deftest-error bit-and-error.3
  (eval '(bit-and #*1100 #*1010 30))
  type-error)

(deftest-error! bit-and-error.4
  (eval '(bit-and #*1100)))

(deftest-error! bit-and-error.5
  (eval '(bit-and #*1100 #*1010 t nil)))

(deftest-error bit-not-error.1
  (eval '(bit-not 10))
  type-error)

(deftest-error bit-not-error.2
  (eval '(bit-not #*1010 20))
  type-error)

(deftest-error! bit-not-error.3
  (eval '(bit-not)))

(deftest-error! bit-not-error.4
  (eval '(bit-not #*1010 nil nil)))


;;  ANSI Common Lisp
(defvar *bit-and-test-1*)
(defvar *bit-and-test-2*)
(defvar *bit-and-test-3*)

(deftest bit-and-test.1
  (bit-and (setq *bit-and-test-1* #*11101010) #*01101011)
  #*01101010)

(deftest bit-and-test.2
  (bit-and #*1100 #*1010)
  #*1000)

(deftest bit-and-test.3
  (bit-andc1 #*1100 #*1010)
  #*0010)

(deftest bit-and-test.4
  (setq *bit-and-test-2*
        (bit-andc2 *bit-and-test-1* #*00110011 t))
  #*11001000)

(deftest bit-and-test.5
  (eq *bit-and-test-2* *bit-and-test-1*)
  t)

(deftest bit-and-test.6
  (bit-not (setq *bit-and-test-1* #*11101010))
  #*00010101)

(deftest bit-and-test.7
  (setq *bit-and-test-2*
        (bit-not *bit-and-test-1*
                 (setq *bit-and-test-3* (make-array 8 :element-type 'bit))))
  #*00010101)

(deftest bit-and-test.8
  (equal *bit-and-test-2* *bit-and-test-3*)
  t)

(deftest bit-and-test.9
  (bit-xor #*1100 #*1010)
  #*0110)

