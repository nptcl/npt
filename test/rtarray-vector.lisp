;;
;;  ANSI COMMON LISP: 15. Arrays
;;

;;
;;  simple-vector-p
;;
(deftest simple-vector-p.1
  (simple-vector-p
    (make-array 6))
  t)

(deftest simple-vector-p.2
  (simple-vector-p
    (make-array '(10)))
  t)

(deftest simple-vector-p.3
  (simple-vector-p
    (make-array nil))
  nil)

(deftest simple-vector-p.4
  (simple-vector-p
    (make-array '(4 5)))
  nil)

(deftest simple-vector-p.5
  (simple-vector-p
    (make-array 10 :fill-pointer t))
  nil)

(deftest simple-vector-p.6
  (simple-vector-p
    (make-array 10 :adjustable t))
  nil)

(deftest simple-vector-p.7
  (simple-vector-p
    (make-array 10 :displaced-to (make-array 10)))
  nil)

(deftest simple-vector-p.8
  (simple-vector-p "aaa")
  nil)

(deftest simple-vector-p.10
  (simple-vector-p #(10 20 30))
  t)

(deftest simple-vector-p.11
  (simple-vector-p
    (make-array 10 :element-type 'single-float))
  nil)

(deftest simple-vector-p.12
  (simple-vector-p 100)
  nil)


;;
;;  svref
;;
(deftest svref.1
  (let ((pos #(10 20 30)))
    (svref pos 1))
  20)

(deftest svref.2
  (let ((pos (make-array 10 :initial-contents '(10 20 30 40 50 0 1 2 3 4)
                         :fill-pointer nil
                         :adjustable nil
                         :displaced-to nil)))
    (svref pos 2))
  30)

(deftest setf-svref.1
  (let ((pos #(10 20 30)))
    (setf (svref pos 1) 999))
  999)

(deftest setf-svref.2
  (let ((pos #(10 20 30)))
    (setf (svref pos 1) 999)
    (svref pos 1))
  999)

(deftest setf-svref.3
  (let ((pos (make-array 10 :initial-contents '(10 20 30 40 50 0 1 2 3 4)
                         :fill-pointer nil
                         :adjustable nil
                         :displaced-to nil)))
    (setf (svref pos 2) 999)
    (svref pos 2))
  999)


;;
;;  vector
;;
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


;;
;;  vector-pop
;;
(deftest vector-pop.1
  (let ((pos (make-array 5 :initial-contents '(9 8 7 6 5) :fill-pointer t)))
    (values
      (vector-pop pos)
      (vector-pop pos)
      (vector-pop pos)
      (fill-pointer pos)
      pos))
  5 6 7 2 #(9 8))

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

(deftest vector-pop.5
  (let ((pos (make-array 5
                         :element-type 'character
                         :initial-contents "Hello"
                         :fill-pointer t)))
    (values
      (vector-pop pos)
      (vector-pop pos)
      (vector-pop pos)
      (fill-pointer pos)
      pos))
  #\o #\l #\l 2 "He")


;;
;;  vector-push
;;
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


;;
;;  vector-push-extend
;;
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


;;
;;  vectorp
;;
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

(deftest vectorp.6
  (vectorp "hello")
  t)

(deftest vectorp.7
  (vectorp #(10 20 30))
  t)

(deftest vectorp.8
  (vectorp '(10 20 30))
  nil)


;;
;;  bit
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

(deftest setf-sbit.1
  (let ((a (make-array 5 :element-type 'bit :initial-contents '(1 0 0 1 1))))
    (setf (sbit a 2) 0))
  0)

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


;;
;;  bit-vecotr-p
;;
(deftest bit-vector-p.1
  (bit-vector-p
    (make-array 10 :element-type 'bit :fill-pointer t))
  t)

(deftest bit-vector-p.2
  (bit-vector-p #*)
  t)

(deftest bit-vector-p.3
  (bit-vector-p #*110)
  t)

(deftest bit-vector-p.4
  (bit-vector-p
    (make-array '(2 3) :element-type 'bit))
  nil)

(deftest bit-vector-p.5
  (bit-vector-p #(1 0 1 1))
  nil)

(deftest bit-vector-p.6
  (bit-vector-p (make-array 6))
  nil)

(deftest bit-vector-p.7
  (bit-vector-p 100)
  nil)


;;
;;  simple-bit-vector-p
;;
(deftest simple-bit-vector-p.1
  (simple-bit-vector-p #*110)
  t)

(deftest simple-bit-vector-p.2
  (simple-bit-vector-p
    (make-array 10 :element-type 'bit :fill-pointer t))
  nil)

(deftest simple-bit-vector-p.3
  (simple-bit-vector-p
    (make-array 10 :element-type 'bit
                :fill-pointer nil
                :adjustable nil
                :displaced-to nil))
  t)

(deftest simple-bit-vector-p.4
  (simple-bit-vector-p
    (make-array '(2 3) :element-type 'bit))
  nil)

(deftest simple-bit-vector-p.5
  (simple-bit-vector-p #(1 0 1 1))
  nil)

(deftest simple-bit-vector-p.6
  (simple-bit-vector-p 100)
  nil)


;;
;;  bit-operation
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

