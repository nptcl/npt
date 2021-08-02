;;
;;  ANSI COMMON LISP: 15. Arrays
;;

;;
;;  System Class ARRAY
;;
(deftest array-type.1
  (lisp-system:closp
    (find-class 'array))
  t)

(deftest array-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'array)))
  (array t))

(deftest array-type.3
  (typep "Hello" 'array)
  t)

(deftest array-type.4
  (typep "Hello" '(array))
  t)

(deftest array-type.5
  (typep #(10 20 30) 'array)
  t)

(deftest array-type.6
  (typep #*11011 'array)
  t)

(deftest array-type.7
  (typep 20 'array)
  nil)

(deftest array-type.8
  (typep '(10 20 30) 'array)
  nil)

(deftest array-type.9
  (typep (make-array 5 :initial-contents "Hello"
                     :fill-pointer t
                     :adjustable t)
         'array)
  t)

(deftest array-type.10
  (typep (make-array 5 :initial-contents "Hello"
                     :fill-pointer t
                     :adjustable t)
         '(array * (5)))
  t)


;;
;;  array - character
;;
(deftest array-type-character.1
  (upgraded-array-element-type 'character)
  character)

(deftest array-type-character.2
  (typep "Hello" '(array character))
  t)

(deftest array-type-character.3
  (typep "Hello" '(array t))
  nil)

(deftest array-type-character.4
  (typep "Hello" '(array *))
  t)

(deftest array-type-character.5
  (typep "Hello" '(array character (5)))
  t)

(deftest array-type-character.6
  (typep "Hello" '(array character (6)))
  nil)

(deftest array-type-character.7
  (typep "Hello" '(array character 5))
  nil)

(deftest array-type-character.8
  (typep "Hello" '(array character 1))
  t)

(deftest array-type-character.9
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         '(array character (2 5)))
  t)

(deftest array-type-character.10
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         '(array character (3 5)))
  nil)

(deftest array-type-character.11
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         '(array character (2 4)))
  nil)

(deftest array-type-character.12
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         '(array character (2 *)))
  t)

(deftest array-type-character.13
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         '(array character 2))
  t)

(deftest array-type-character.14
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         '(array character 3))
  nil)

(deftest array-type-character.15
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         'array)
  t)

(deftest array-type-character.16
  (typep #(#\A #\B #\C) '(array character))
  nil)


;;
;;  array - bit
;;
(deftest array-type-bit.1
  (upgraded-array-element-type 'bit)
  bit)

(deftest array-type-bit.2
  (typep #*11001 '(array bit))
  t)

(deftest array-type-bit.3
  (typep #*11001 '(array t))
  nil)

(deftest array-type-bit.4
  (typep #*11001 '(array *))
  t)

(deftest array-type-bit.5
  (typep #*11001 '(array bit (5)))
  t)

(deftest array-type-bit.6
  (typep #*11001 '(array bit (6)))
  nil)

(deftest array-type-bit.7
  (typep #*11001 '(array bit 5))
  nil)

(deftest array-type-bit.8
  (typep #*11001 '(array bit 1))
  t)

(deftest array-type-bit.9
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         '(array bit (2 5)))
  t)

(deftest array-type-bit.10
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         '(array bit (3 5)))
  nil)

(deftest array-type-bit.11
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         '(array bit (2 4)))
  nil)

(deftest array-type-bit.12
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         '(array bit (2 *)))
  t)

(deftest array-type-bit.13
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         '(array bit 2))
  t)

(deftest array-type-bit.14
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         '(array bit 3))
  nil)

(deftest array-type-bit.15
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         'array)
  t)

(deftest array-type-bit.16
  (typep #(1 1 0 0 1) '(array bit))
  nil)

(deftest array-type-bit.17
  (typep #b11001 '(array bit))
  nil)


;;
;;  array - (signed-byte 8)
;;
(deftest array-type-signed-byte-8.1
  (typep #(1 2 3) '(array (signed-byte 8)))
  nil)

(deftest array-type-signed-byte-8.2
  (typep (make-array 3 :element-type '(signed-byte 8)
                     :initial-contents '(1 2 3))
         '(array (signed-byte 8)))
  t)

(deftest array-type-signed-byte-8.3
  (typep (make-array 3 :element-type '(signed-byte 8)
                     :initial-contents '(1 2 3))
         '(array t))
  nil)


;;
;;  array - (signed-byte 16)
;;
(deftest array-type-signed-byte-16.1
  (typep #(1 2 3) '(array (signed-byte 16)))
  nil)

(deftest array-type-signed-byte-16.2
  (typep (make-array 3 :element-type '(signed-byte 16)
                     :initial-contents '(1 2 3))
         '(array (signed-byte 16)))
  t)

(deftest array-type-signed-byte-16.3
  (typep (make-array 3 :element-type '(signed-byte 16)
                     :initial-contents '(1 2 3))
         '(array t))
  nil)


;;
;;  array - (signed-byte 32)
;;
(deftest array-type-signed-byte-32.1
  (typep #(1 2 3) '(array (signed-byte 32)))
  nil)

(deftest array-type-signed-byte-32.2
  (typep (make-array 3 :element-type '(signed-byte 32)
                     :initial-contents '(1 2 3))
         '(array (signed-byte 32)))
  t)

(deftest array-type-signed-byte-32.3
  (typep (make-array 3 :element-type '(signed-byte 32)
                     :initial-contents '(1 2 3))
         '(array t))
  nil)


;;
;;  array - (signed-byte 64)  64bit only
;;
#+fixnum-64
(deftest array-type-signed-byte-64.1
  (typep #(1 2 3) '(array (signed-byte 64)))
  nil)

#+fixnum-64
(deftest array-type-signed-byte-64.2
  (typep (make-array 3 :element-type '(signed-byte 64)
                     :initial-contents '(1 2 3))
         '(array (signed-byte 64)))
  t)

#+fixnum-64
(deftest array-type-signed-byte-64.3
  (typep (make-array 3 :element-type '(signed-byte 64)
                     :initial-contents '(1 2 3))
         '(array t))
  nil)


;;
;;  array - (unsigned-byte 8)
;;
(deftest array-type-unsigned-byte-8.1
  (typep #(1 2 3) '(array (unsigned-byte 8)))
  nil)

(deftest array-type-unsigned-byte-8.2
  (typep (make-array 3 :element-type '(unsigned-byte 8)
                     :initial-contents '(1 2 3))
         '(array (unsigned-byte 8)))
  t)

(deftest array-type-unsigned-byte-8.3
  (typep (make-array 3 :element-type '(unsigned-byte 8)
                     :initial-contents '(1 2 3))
         '(array t))
  nil)


;;
;;  array - (unsigned-byte 16)
;;
(deftest array-type-unsigned-byte-16.1
  (typep #(1 2 3) '(array (unsigned-byte 16)))
  nil)

(deftest array-type-unsigned-byte-16.2
  (typep (make-array 3 :element-type '(unsigned-byte 16)
                     :initial-contents '(1 2 3))
         '(array (unsigned-byte 16)))
  t)

(deftest array-type-unsigned-byte-16.3
  (typep (make-array 3 :element-type '(unsigned-byte 16)
                     :initial-contents '(1 2 3))
         '(array t))
  nil)


;;
;;  array - (unsigned-byte 32)
;;
(deftest array-type-unsigned-byte-32.1
  (typep #(1 2 3) '(array (unsigned-byte 32)))
  nil)

(deftest array-type-unsigned-byte-32.2
  (typep (make-array 3 :element-type '(unsigned-byte 32)
                     :initial-contents '(1 2 3))
         '(array (unsigned-byte 32)))
  t)

(deftest array-type-unsigned-byte-32.3
  (typep (make-array 3 :element-type '(unsigned-byte 32)
                     :initial-contents '(1 2 3))
         '(array t))
  nil)


;;
;;  array - (unsigned-byte 64)  64bit only
;;
#+fixnum-64
(deftest array-type-unsigned-byte-64.1
  (typep #(1 2 3) '(array (unsigned-byte 64)))
  nil)

#+fixnum-64
(deftest array-type-unsigned-byte-64.2
  (typep (make-array 3 :element-type '(unsigned-byte 64)
                     :initial-contents '(1 2 3))
         '(array (unsigned-byte 64)))
  t)

#+fixnum-64
(deftest array-type-unsigned-byte-64.3
  (typep (make-array 3 :element-type '(unsigned-byte 64)
                     :initial-contents '(1 2 3))
         '(array t))
  nil)


;;
;;  array - single-float
;;
(deftest array-type-single-float.1
  (typep #(1 2 3) '(array single-float))
  nil)

(deftest array-type-single-float.2
  (typep (make-array 3 :element-type 'single-float
                     :initial-contents '(1.2f0 2.3f0 3.4f0))
         '(array single-float))
  t)

(deftest array-type-single-float.3
  (typep (make-array 3 :element-type 'single-float
                     :initial-contents '(1.2f0 2.3f0 3.4f0))
         '(array t))
  nil)


;;
;;  array - double-float
;;
(deftest array-type-double-float.1
  (typep #(1 2 3) '(array double-float))
  nil)

(deftest array-type-double-float.2
  (typep (make-array 3 :element-type 'double-float
                     :initial-contents '(1.2d0 2.3d0 3.4d0))
         '(array double-float))
  t)

(deftest array-type-double-float.3
  (typep (make-array 3 :element-type 'double-float
                     :initial-contents '(1.2d0 2.3d0 3.4d0))
         '(array t))
  nil)


;;
;;  array - long-float
;;
(deftest array-type-long-float.1
  (typep #(1 2 3) '(array long-float))
  nil)

(deftest array-type-long-float.2
  (typep (make-array 3 :element-type 'long-float
                     :initial-contents '(1.2L0 2.3L0 3.4L0))
         '(array long-float))
  t)

(deftest array-type-long-float.3
  (typep (make-array 3 :element-type 'long-float
                     :initial-contents '(1.2L0 2.3L0 3.4L0))
         '(array t))
  nil)


;;
;;  Type SIMPLE-ARRAY
;;
(deftest simple-array-type.1
  (lisp-system:closp
    (find-class 'simple-array))
  t)

(deftest simple-array-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'simple-array)))
  (simple-array array t))

(deftest simple-array-type.3
  (typep "Hello" 'simple-array)
  t)

(deftest simple-array-type.4
  (typep "Hello" '(simple-array))
  t)

(deftest simple-array-type.5
  (typep #(10 20 30) 'simple-array)
  t)

(deftest simple-array-type.6
  (typep #*11011 'simple-array)
  t)

(deftest simple-array-type.7
  (typep 20 'simple-array)
  nil)

(deftest simple-array-type.8
  (typep '(10 20 30) 'simple-array)
  nil)

(deftest simple-array-type.9
  (typep (make-array 5 :initial-contents "Hello"
                     :fill-pointer t
                     :adjustable t)
         'simple-array)
  nil)

(deftest simple-array-type.10
  (typep (make-array 5 :initial-contents "Hello"
                     :fill-pointer t
                     :adjustable t)
         '(simple-array * (5)))
  nil)


;;
;;  simple-array - character
;;
(deftest simple-array-type-character.1
  (upgraded-array-element-type 'character)
  character)

(deftest simple-array-type-character.2
  (typep "Hello" '(simple-array character))
  t)

(deftest simple-array-type-character.3
  (typep "Hello" '(simple-array t))
  nil)

(deftest simple-array-type-character.4
  (typep "Hello" '(simple-array *))
  t)

(deftest simple-array-type-character.5
  (typep "Hello" '(simple-array character (5)))
  t)

(deftest simple-array-type-character.6
  (typep "Hello" '(simple-array character (6)))
  nil)

(deftest simple-array-type-character.7
  (typep "Hello" '(simple-array character 5))
  nil)

(deftest simple-array-type-character.8
  (typep "Hello" '(simple-array character 1))
  t)

(deftest simple-array-type-character.9
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         '(simple-array character (2 5)))
  t)

(deftest simple-array-type-character.10
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         '(simple-array character (3 5)))
  nil)

(deftest simple-array-type-character.11
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         '(simple-array character (2 4)))
  nil)

(deftest simple-array-type-character.12
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         '(simple-array character (2 *)))
  t)

(deftest simple-array-type-character.13
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         '(simple-array character 2))
  t)

(deftest simple-array-type-character.14
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         '(simple-array character 3))
  nil)

(deftest simple-array-type-character.15
  (typep (make-array '(2 5) :element-type 'character
                     :initial-contents '("Hello" "ABCDE"))
         'simple-array)
  t)

(deftest simple-array-type-character.16
  (typep #(#\A #\B #\C) '(simple-array character))
  nil)


;;
;;  simple-array - bit
;;
(deftest simple-array-type-bit.1
  (upgraded-array-element-type 'bit)
  bit)

(deftest simple-array-type-bit.2
  (typep #*11001 '(simple-array bit))
  t)

(deftest simple-array-type-bit.3
  (typep #*11001 '(simple-array t))
  nil)

(deftest simple-array-type-bit.4
  (typep #*11001 '(simple-array *))
  t)

(deftest simple-array-type-bit.5
  (typep #*11001 '(simple-array bit (5)))
  t)

(deftest simple-array-type-bit.6
  (typep #*11001 '(simple-array bit (6)))
  nil)

(deftest simple-array-type-bit.7
  (typep #*11001 '(simple-array bit 5))
  nil)

(deftest simple-array-type-bit.8
  (typep #*11001 '(simple-array bit 1))
  t)

(deftest simple-array-type-bit.9
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         '(simple-array bit (2 5)))
  t)

(deftest simple-array-type-bit.10
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         '(simple-array bit (3 5)))
  nil)

(deftest simple-array-type-bit.11
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         '(simple-array bit (2 4)))
  nil)

(deftest simple-array-type-bit.12
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         '(simple-array bit (2 *)))
  t)

(deftest simple-array-type-bit.13
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         '(simple-array bit 2))
  t)

(deftest simple-array-type-bit.14
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         '(simple-array bit 3))
  nil)

(deftest simple-array-type-bit.15
  (typep (make-array '(2 5) :element-type 'bit
                     :initial-contents '(#*11001 #*00011))
         'simple-array)
  t)

(deftest simple-array-type-bit.16
  (typep #(1 1 0 0 1) '(simple-array bit))
  nil)

(deftest simple-array-type-bit.17
  (typep #b11001 '(simple-array bit))
  nil)


;;
;;  simple-array - (signed-byte 8)
;;
(deftest simple-array-type-signed-byte-8.1
  (typep #(1 2 3) '(simple-array (signed-byte 8)))
  nil)

(deftest simple-array-type-signed-byte-8.2
  (typep (make-array 3 :element-type '(signed-byte 8)
                     :initial-contents '(1 2 3))
         '(simple-array (signed-byte 8)))
  t)

(deftest simple-array-type-signed-byte-8.3
  (typep (make-array 3 :element-type '(signed-byte 8)
                     :initial-contents '(1 2 3))
         '(simple-array t))
  nil)


;;
;;  simple-array - (signed-byte 16)
;;
(deftest simple-array-type-signed-byte-16.1
  (typep #(1 2 3) '(simple-array (signed-byte 16)))
  nil)

(deftest simple-array-type-signed-byte-16.2
  (typep (make-array 3 :element-type '(signed-byte 16)
                     :initial-contents '(1 2 3))
         '(simple-array (signed-byte 16)))
  t)

(deftest simple-array-type-signed-byte-16.3
  (typep (make-array 3 :element-type '(signed-byte 16)
                     :initial-contents '(1 2 3))
         '(simple-array t))
  nil)


;;
;;  simple-array - (signed-byte 32)
;;
(deftest simple-array-type-signed-byte-32.1
  (typep #(1 2 3) '(simple-array (signed-byte 32)))
  nil)

(deftest simple-array-type-signed-byte-32.2
  (typep (make-array 3 :element-type '(signed-byte 32)
                     :initial-contents '(1 2 3))
         '(simple-array (signed-byte 32)))
  t)

(deftest simple-array-type-signed-byte-32.3
  (typep (make-array 3 :element-type '(signed-byte 32)
                     :initial-contents '(1 2 3))
         '(simple-array t))
  nil)


;;
;;  simple-array - (signed-byte 64)  64bit only
;;
#+fixnum-64
(deftest simple-array-type-signed-byte-64.1
  (typep #(1 2 3) '(simple-array (signed-byte 64)))
  nil)

#+fixnum-64
(deftest simple-array-type-signed-byte-64.2
  (typep (make-array 3 :element-type '(signed-byte 64)
                     :initial-contents '(1 2 3))
         '(simple-array (signed-byte 64)))
  t)

#+fixnum-64
(deftest simple-array-type-signed-byte-64.3
  (typep (make-array 3 :element-type '(signed-byte 64)
                     :initial-contents '(1 2 3))
         '(simple-array t))
  nil)


;;
;;  simple-array - (unsigned-byte 8)
;;
(deftest simple-array-type-unsigned-byte-8.1
  (typep #(1 2 3) '(simple-array (unsigned-byte 8)))
  nil)

(deftest simple-array-type-unsigned-byte-8.2
  (typep (make-array 3 :element-type '(unsigned-byte 8)
                     :initial-contents '(1 2 3))
         '(simple-array (unsigned-byte 8)))
  t)

(deftest simple-array-type-unsigned-byte-8.3
  (typep (make-array 3 :element-type '(unsigned-byte 8)
                     :initial-contents '(1 2 3))
         '(simple-array t))
  nil)


;;
;;  simple-array - (unsigned-byte 16)
;;
(deftest simple-array-type-unsigned-byte-16.1
  (typep #(1 2 3) '(simple-array (unsigned-byte 16)))
  nil)

(deftest simple-array-type-unsigned-byte-16.2
  (typep (make-array 3 :element-type '(unsigned-byte 16)
                     :initial-contents '(1 2 3))
         '(simple-array (unsigned-byte 16)))
  t)

(deftest simple-array-type-unsigned-byte-16.3
  (typep (make-array 3 :element-type '(unsigned-byte 16)
                     :initial-contents '(1 2 3))
         '(simple-array t))
  nil)


;;
;;  simple-array - (unsigned-byte 32)
;;
(deftest simple-array-type-unsigned-byte-32.1
  (typep #(1 2 3) '(simple-array (unsigned-byte 32)))
  nil)

(deftest simple-array-type-unsigned-byte-32.2
  (typep (make-array 3 :element-type '(unsigned-byte 32)
                     :initial-contents '(1 2 3))
         '(simple-array (unsigned-byte 32)))
  t)

(deftest simple-array-type-unsigned-byte-32.3
  (typep (make-array 3 :element-type '(unsigned-byte 32)
                     :initial-contents '(1 2 3))
         '(simple-array t))
  nil)


;;
;;  simple-array - (unsigned-byte 64)  64bit only
;;
#+fixnum-64
(deftest simple-array-type-unsigned-byte-64.1
  (typep #(1 2 3) '(simple-array (unsigned-byte 64)))
  nil)

#+fixnum-64
(deftest simple-array-type-unsigned-byte-64.2
  (typep (make-array 3 :element-type '(unsigned-byte 64)
                     :initial-contents '(1 2 3))
         '(simple-array (unsigned-byte 64)))
  t)

#+fixnum-64
(deftest simple-array-type-unsigned-byte-64.3
  (typep (make-array 3 :element-type '(unsigned-byte 64)
                     :initial-contents '(1 2 3))
         '(simple-array t))
  nil)


;;
;;  simple-array - single-float
;;
(deftest simple-array-type-single-float.1
  (typep #(1 2 3) '(simple-array single-float))
  nil)

(deftest simple-array-type-single-float.2
  (typep (make-array 3 :element-type 'single-float
                     :initial-contents '(1.2f0 2.3f0 3.4f0))
         '(simple-array single-float))
  t)

(deftest simple-array-type-single-float.3
  (typep (make-array 3 :element-type 'single-float
                     :initial-contents '(1.2f0 2.3f0 3.4f0))
         '(simple-array t))
  nil)


;;
;;  simple-array - double-float
;;
(deftest simple-array-type-double-float.1
  (typep #(1 2 3) '(simple-array double-float))
  nil)

(deftest simple-array-type-double-float.2
  (typep (make-array 3 :element-type 'double-float
                     :initial-contents '(1.2d0 2.3d0 3.4d0))
         '(simple-array double-float))
  t)

(deftest simple-array-type-double-float.3
  (typep (make-array 3 :element-type 'double-float
                     :initial-contents '(1.2d0 2.3d0 3.4d0))
         '(simple-array t))
  nil)


;;
;;  simple-array - long-float
;;
(deftest simple-array-type-long-float.1
  (typep #(1 2 3) '(simple-array long-float))
  nil)

(deftest simple-array-type-long-float.2
  (typep (make-array 3 :element-type 'long-float
                     :initial-contents '(1.2L0 2.3L0 3.4L0))
         '(simple-array long-float))
  t)

(deftest simple-array-type-long-float.3
  (typep (make-array 3 :element-type 'long-float
                     :initial-contents '(1.2L0 2.3L0 3.4L0))
         '(simple-array t))
  nil)

