;;
;;  ANSI COMMON LISP: 15. Arrays
;;

;;
;;  System Class VECTOR
;;
(deftest vector-type.1
  (lisp-system:closp
    (find-class 'vector))
  t)

(deftest vector-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'vector)))
  (vector array sequence t))

(deftest vector-type.3
  (typep "Hello" 'vector)
  t)

(deftest vector-type.4
  (typep #*10110 'vector)
  t)

(deftest vector-type.5
  (typep '(10 20 30) 'vector)
  nil)

(deftest vector-type.6
  (typep (make-array 5 :initial-contents "Hello") 'vector)
  t)

(deftest vector-type.7
  (typep (make-array 5 :initial-contents "Hello"
                     :fill-pointer t
                     :adjustable t)
         'vector)
  t)

(deftest vector-type.8
  (typep (make-array '(2 5) :initial-contents '("Hello" "ABCDE"))
         'vector)
  nil)

(deftest vector-type.9
  (typep "Hello" '(vector character))
  t)

(deftest vector-type.10
  (typep "Hello" '(vector character 5))
  t)

(deftest vector-type.11
  (typep "Hello" '(vector t 5))
  nil)

(deftest vector-type.12
  (typep "Hello" '(vector * 5))
  t)

(deftest vector-type.13
  (typep "Hello" '(vector character 6))
  nil)

(deftest vector-type.14
  (typep "Hello" '(vector))
  t)


;;
;;  Type SIMPLE-VECTOR
;;
(deftest simple-vector-type.1
  (lisp-system:closp
    (find-class 'simple-vector))
  t)

(deftest simple-vector-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'simple-vector)))
  (simple-vector vector simple-array array sequence t))

(deftest simple-vector-type.3
  (typep "Hello" 'simple-vector)
  nil)

(deftest simple-vector-type.4
  (typep #*10110 'simple-vector)
  nil)

(deftest simple-vector-type.5
  (typep '(10 20 30) 'simple-vector)
  nil)

(deftest simple-vector-type.6
  (typep (make-array 5 :initial-contents "Hello") 'simple-vector)
  t)

(deftest simple-vector-type.7
  (typep (make-array 5 :initial-contents "Hello"
                     :fill-pointer t
                     :adjustable t)
         'simple-vector)
  nil)

(deftest simple-vector-type.8
  (typep (make-array '(2 5) :initial-contents '("Hello" "ABCDE"))
         'simple-vector)
  nil)

(deftest simple-vector-type.9
  (typep #(1 2 3 4 5) '(simple-vector *))
  t)

(deftest simple-vector-type.10
  (typep #(1 2 3 4 5) '(simple-vector *))
  t)

(deftest simple-vector-type.11
  (typep #(1 2 3 4 5) '(simple-vector 5))
  t)

(deftest simple-vector-type.12
  (typep #(1 2 3 4 5) '(simple-vector 6))
  nil)


;;
;;  System Class BIT-VECTOR
;;
(deftest bit-vector-type.1
  (lisp-system:closp
    (find-class 'bit-vector))
  t)

(deftest bit-vector-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'bit-vector)))
  (bit-vector vector array sequence t))

(deftest bit-vector-type.4
  (typep #*10110 'bit-vector)
  t)

(deftest bit-vector-type.3
  (typep "Hello" 'bit-vector)
  nil)

(deftest bit-vector-type.5
  (typep '(10 20 30) 'bit-vector)
  nil)

(deftest bit-vector-type.6
  (typep (make-array 5 :element-type 'bit :initial-contents '(1 1 0 0 1))
         'bit-vector)
  t)

(deftest bit-vector-type.7
  (typep (make-array 5 :element-type 'bit :initial-contents '(1 1 0 0 1)
                     :fill-pointer t
                     :adjustable t)
         'bit-vector)
  t)

(deftest bit-vector-type.8
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11011 #*11110))
         'bit-vector)
  nil)

(deftest bit-vector-type.9
  (typep #*11001 '(bit-vector *))
  t)

(deftest bit-vector-type.10
  (typep #*11001 '(bit-vector *))
  t)

(deftest bit-vector-type.11
  (typep #*11001 '(bit-vector 5))
  t)

(deftest bit-vector-type.12
  (typep #*11001 '(bit-vector 6))
  nil)


;;
;;  Type SIMPLE-BIT-VECTOR
;;
(deftest simple-bit-vector-type.1
  (lisp-system:closp
    (find-class 'simple-bit-vector))
  t)

(deftest simple-bit-vector-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'simple-bit-vector)))
  (simple-bit-vector bit-vector vector simple-array array sequence t))

(deftest simple-bit-vector-type.4
  (typep #*10110 'simple-bit-vector)
  t)

(deftest simple-bit-vector-type.3
  (typep "Hello" 'simple-bit-vector)
  nil)

(deftest simple-bit-vector-type.5
  (typep '(10 20 30) 'simple-bit-vector)
  nil)

(deftest simple-bit-vector-type.6
  (typep (make-array 5 :element-type 'bit :initial-contents '(1 1 0 0 1))
         'simple-bit-vector)
  t)

(deftest simple-bit-vector-type.7
  (typep (make-array 5 :element-type 'bit :initial-contents '(1 1 0 0 1)
                     :fill-pointer t
                     :adjustable t)
         'simple-bit-vector)
  nil)

(deftest simple-bit-vector-type.8
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11011 #*11110))
         'simple-bit-vector)
  nil)

(deftest simple-bit-vector-type.9
  (typep #*11001 '(simple-bit-vector *))
  t)

(deftest simple-bit-vector-type.10
  (typep #*11001 '(simple-bit-vector *))
  t)

(deftest simple-bit-vector-type.11
  (typep #*11001 '(simple-bit-vector 5))
  t)

(deftest simple-bit-vector-type.12
  (typep #*11001 '(simple-bit-vector 6))
  nil)


;;
;;  Function ARRAYP
;;
(deftest arrayp.1
  (arrayp #(10 20 30))
  t)

(deftest arrayp.2
  (arrayp #*11011)
  t)

(deftest arrayp.3
  (arrayp #2a((1 2 3) (:a :b :c)))
  t)

(deftest arrayp.4
  (arrayp
    (make-array 5 :initial-contents "Hello"
                :fill-pointer t
                :adjustable t))
  t)

(deftest arrayp.5
  (arrayp
    (make-array '(2 5) :initial-contents '("Hello" "ABCDE")))
  t)

(deftest arrayp.6
  (arrayp "Hello")
  t)

(deftest arrayp.7
  (arrayp '(10 20 30))
  nil)

(deftest-error! arrayp-error.1
  (eval '(arrayp)))

(deftest-error! arrayp-error.2
  (eval '(arrayp 10 20)))


;;  ANSI Common Lisp
(deftest arrayp-test.1
  (arrayp (make-array '(2 3 4) :adjustable t))
  t)

(deftest arrayp-test.2
  (arrayp (make-array 6))
  t)

(deftest arrayp-test.3
  (arrayp #*1011)
  t)

(deftest arrayp-test.4
  (arrayp "hi")
  t)

(deftest arrayp-test.5
  (arrayp 'hi)
  nil)

(deftest arrayp-test.6
  (arrayp 12)
  nil)


;;
;;  Function VECTORP
;;
(deftest vectorp.1
  (vectorp #(10 20 30))
  t)

(deftest vectorp.2
  (vectorp #*11011)
  t)

(deftest vectorp.3
  (vectorp #2a((1 2 3) (:a :b :c)))
  nil)

(deftest vectorp.4
  (vectorp
    (make-array 5 :initial-contents "Hello"
                :fill-pointer t
                :adjustable t))
  t)

(deftest vectorp.5
  (vectorp
    (make-array '(2 5) :initial-contents '("Hello" "ABCDE")))
  nil)

(deftest vectorp.6
  (vectorp "Hello")
  t)

(deftest vectorp.7
  (vectorp '(10 20 30))
  nil)

(deftest-error! vectorp-error.1
  (eval '(vectorp)))

(deftest-error! vectorp-error.2
  (eval '(vectorp 10 20)))


;;  ANSI Common Lisp
(deftest vectorp-test.1
  (vectorp "aaaaaa")
  t)

(deftest vectorp-test.2
  (vectorp (make-array 6 :fill-pointer t))
  t)

(deftest vectorp-test.3
  (vectorp (make-array '(2 3 4)))
  nil)

(deftest vectorp-test.4
  (vectorp #*11)
  t)

(deftest vectorp-test.5
  (vectorp #b11)
  nil)


;;
;;  Function SIMPLE-VECTOR-P
;;
(deftest simple-vector-p.1
  (simple-vector-p #(10 20 30))
  t)

(deftest simple-vector-p.2
  (simple-vector-p #*11011)
  nil)

(deftest simple-vector-p.3
  (simple-vector-p #2a((1 2 3) (:a :b :c)))
  nil)

(deftest simple-vector-p.4
  (simple-vector-p
    (make-array 5 :initial-contents "Hello"))
  t)

(deftest simple-vector-p.5
  (simple-vector-p
    (make-array 5 :initial-contents "Hello"
                :fill-pointer t
                :adjustable t))
  nil)

(deftest simple-vector-p.6
  (simple-vector-p
    (make-array '(2 5) :initial-contents '("Hello" "ABCDE")))
  nil)

(deftest simple-vector-p.7
  (simple-vector-p "Hello")
  nil)

(deftest simple-vector-p.8
  (simple-vector-p '(10 20 30))
  nil)

(deftest simple-vector-p.9
  (simple-vector-p nil)
  nil)

(deftest simple-vector-p.10
  (simple-vector-p (make-array 6 :adjustable t))
  nil)

(deftest simple-vector-p.11
  (simple-vector-p
    (make-array 10 :displaced-to (make-array 10)))
  nil)


(deftest simple-vector-p.12
  (simple-vector-p
    (make-array 10 :element-type 'single-float))
  nil)

(deftest simple-vector-p.13
  (simple-vector-p 100)
  nil)

(deftest-error! simple-vector-p-error.1
  (eval '(simple-vector-p)))

(deftest-error! simple-vector-p-error.2
  (eval '(simple-vector-p 10 20)))


;;  ANSI Common Lisp
(deftest simple-vector-p-test.1
  (simple-vector-p (make-array 6))
  t)

(deftest simple-vector-p-test.2
  (simple-vector-p "aaaaaa")
  nil)

(deftest simple-vector-p-test.3
  (simple-vector-p (make-array 6 :fill-pointer t))
  nil)


;;
;;  Function BIT-VECTOR-P
;;
(deftest bit-vector-p.1
  (bit-vector-p #*1011)
  t)

(deftest bit-vector-p.2
  (bit-vector-p #*)
  t)

(deftest bit-vector-p.3
  (bit-vector-p #(1 0 1 1 0))
  nil)

(deftest bit-vector-p.4
  (bit-vector-p "Hello")
  nil)

(deftest bit-vector-p.5
  (bit-vector-p '(1 1 1 1))
  nil)

(deftest bit-vector-p.6
  (bit-vector-p
    (make-array 5 :element-type 'bit :initial-contents '(1 1 0 0 1)))
  t)

(deftest bit-vector-p.7
  (bit-vector-p
    (make-array 5 :element-type 'bit :initial-contents '(1 1 0 0 1)
                :fill-pointer t
                :adjustable t))
  t)

(deftest bit-vector-p.8
  (bit-vector-p #b10111)
  nil)

(deftest bit-vector-p.9
  (bit-vector-p
    (make-array '(2 3) :element-type 'bit))
  nil)

(deftest bit-vector-p.10
  (bit-vector-p (make-array 6))
  nil)

(deftest bit-vector-p.11
  (bit-vector-p 100)
  nil)

(deftest-error! bit-vector-p-error.1
  (eval '(bit-vector-p)))

(deftest-error! bit-vector-p-error.2
  (eval '(bit-vector-p 10 20)))


;;  ANSI Common Lisp
(deftest bit-vector-p-test.1
  (bit-vector-p (make-array 6 :element-type 'bit :fill-pointer t))
  t)

(deftest bit-vector-p-test.2
  (bit-vector-p #*)
  t)

(deftest bit-vector-p-test.3
  (bit-vector-p (make-array 6))
  nil)


;;
;;  Function SIMPLE-BIT-VECTOR-P
;;
(deftest simple-bit-vector-p.1
  (simple-bit-vector-p #*1011)
  t)

(deftest simple-bit-vector-p.2
  (simple-bit-vector-p #*)
  t)

(deftest simple-bit-vector-p.3
  (simple-bit-vector-p #(1 0 1 1 0))
  nil)

(deftest simple-bit-vector-p.4
  (simple-bit-vector-p "Hello")
  nil)

(deftest simple-bit-vector-p.5
  (simple-bit-vector-p '(1 1 1 1))
  nil)

(deftest simple-bit-vector-p.6
  (simple-bit-vector-p
    (make-array 5 :element-type 'bit :initial-contents '(1 1 0 0 1)))
  t)

(deftest simple-bit-vector-p.7
  (simple-bit-vector-p
    (make-array 5 :element-type 'bit :initial-contents '(1 1 0 0 1)
                :fill-pointer t
                :adjustable t))
  nil)

(deftest simple-bit-vector-p.8
  (simple-bit-vector-p #b10111)
  nil)

(deftest simple-bit-vector-p.9
  (simple-bit-vector-p
    (make-array '(2 3) :element-type 'bit))
  nil)

(deftest simple-bit-vector-p.10
  (simple-bit-vector-p 100)
  nil)

(deftest-error! simple-bit-vector-p-error.1
  (eval '(simple-bit-vector-p)))

(deftest-error! simple-bit-vector-p-error.2
  (eval '(simple-bit-vector-p 10 20)))


;;  ANSI Common Lisp
(deftest simple-bit-vector-p-test.1
  (simple-bit-vector-p (make-array 6))
  nil)

(deftest simple-bit-vector-p-test.2
  (simple-bit-vector-p #*)
  t)

