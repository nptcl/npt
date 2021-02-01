;;
;;  typep
;;

;;
;;  vector
;;
(deftest typep-vector.1
  (typep 100 'vector)
  nil)

;;  object vector
(deftest typep-vector-vector.1
  (typep #() 'vector)
  t)

(deftest typep-vector-vector.2
  (typep #(1 2 3) '(vector))
  t)

(deftest typep-vector-vector.3
  (typep #(1 2 3) '(vector *))
  t)

(deftest typep-vector-vector.4
  (typep #(1 2 3) '(vector t))
  t)

(deftest typep-vector-vector.5
  (typep #(1 2 3) '(vector integer))  ;; upgraded -> t
  t)

(deftest typep-vector-vector.6
  (typep #(1 2 3) '(vector (integer 0 10)))  ;; specialized (unsigned-byte 8)
  nil)

(deftest typep-vector-vector.7
  (typep #(1 2 3) '(vector character))
  nil)

(deftest typep-vector-vector.8
  (typep #(1 2 3) '(vector * *))
  t)

(deftest typep-vector-vector.9
  (typep #(1 2 3) '(vector * 3))
  t)

(deftest typep-vector-vector.10
  (typep #(1 2 3) '(vector * 4))
  nil)

;;  object string
(deftest typep-vector-string.1
  (typep "" 'vector)
  t)

(deftest typep-vector-string.2
  (typep "ABC" '(vector))
  t)

(deftest typep-vector-string.3
  (typep "ABC" '(vector *))
  t)

(deftest typep-vector-string.4
  (typep "ABC" '(vector t))
  nil)

(deftest typep-vector-string.5
  (typep "ABC" '(vector integer))  ;; upgraded -> t
  nil)

(deftest typep-vector-string.6
  (typep "ABC" '(vector (integer 0 10)))  ;; specialized (unsigned-byte 8)
  nil)

(deftest typep-vector-string.7
  (typep "ABC" '(vector character))
  t)

(deftest typep-vector-string.8
  (typep "ABC" '(vector * *))
  t)

(deftest typep-vector-string.9
  (typep "ABC" '(vector * 3))
  t)

(deftest typep-vector-string.10
  (typep "ABC" '(vector * 4))
  nil)

;;  object (array character)
(defun make-array-string (x)
  (make-array (length x) :element-type 'character :initial-contents x))

(deftest typep-vector-strarray.1
  (typep (make-array-string "") 'vector)
  t)

(deftest typep-vector-strarray.2
  (typep (make-array-string "ABC") '(vector))
  t)

(deftest typep-vector-strarray.3
  (typep (make-array-string "ABC") '(vector *))
  t)

(deftest typep-vector-strarray.4
  (typep (make-array-string "ABC") '(vector t))
  nil)

(deftest typep-vector-strarray.5
  (typep (make-array-string "ABC") '(vector integer))  ;; upgraded -> t
  nil)

(deftest typep-vector-strarray.6
  (typep (make-array-string "ABC")
         '(vector (integer 0 10)))  ;; specialized (unsigned-byte 8)
  nil)

(deftest typep-vector-strarray.7
  (typep (make-array-string "ABC") '(vector character))
  t)

(deftest typep-vector-strarray.8
  (typep (make-array-string "ABC") '(vector * *))
  t)

(deftest typep-vector-strarray.9
  (typep (make-array-string "ABC") '(vector * 3))
  t)

(deftest typep-vector-strarray.10
  (typep (make-array-string "ABC") '(vector * 4))
  nil)

;;  object array
(deftest typep-vector-array.1
  (typep #1a() 'vector)
  t)

(deftest typep-vector-array.2
  (typep #1a(1 2 3) '(vector))
  t)

(deftest typep-vector-array.3
  (typep #1a(1 2 3) '(vector *))
  t)

(deftest typep-vector-array.4
  (typep #1a(1 2 3) '(vector t))
  t)

(deftest typep-vector-array.5
  (typep #1a(1 2 3) '(vector integer))  ;; upgraded -> t
  t)

(deftest typep-vector-array.6
  (typep #1a(1 2 3) '(vector (integer 0 10)))  ;; specialized (unsigned-byte 8)
  nil)

(deftest typep-vector-array.7
  (typep #1a(1 2 3) '(vector character))
  nil)

(deftest typep-vector-array.8
  (typep #1a(1 2 3) '(vector * *))
  t)

(deftest typep-vector-array.9
  (typep #1a(1 2 3) '(vector * 3))
  t)

(deftest typep-vector-array.10
  (typep #1a(1 2 3) '(vector * 4))
  nil)

(deftest typep-vector-array.11
  (typep (make-array 3 :element-type '(unsigned-byte 16)) '(vector (integer 0 10)))
  nil)

(deftest typep-vector-array.12
  (typep (make-array '(2 3)) 'vector)
  nil)

;;  object bit-vector
(deftest typep-vector-bit-vector.1
  (typep #* 'vector)
  t)

(deftest typep-vector-bit-vector.2
  (typep #*110 '(vector))
  t)

(deftest typep-vector-bit-vector.3
  (typep #*110 '(vector *))
  t)

(deftest typep-vector-bit-vector.4
  (typep #*110 '(vector t))
  nil)

(deftest typep-vector-bit-vector.5
  (typep #*110 '(vector integer))  ;; upgraded -> t
  nil)

(deftest typep-vector-bit-vector.6
  (typep #*110 '(vector (integer 0 10)))  ;; specialized (unsigned-byte 8)
  nil)

(deftest typep-vector-bit-vector.7
  (typep #*110 '(vector bit))
  t)

(deftest typep-vector-bit-vector.8
  (typep #*110 '(vector * *))
  t)

(deftest typep-vector-bit-vector.9
  (typep #*110 '(vector * 3))
  t)

(deftest typep-vector-bit-vector.10
  (typep #*110 '(vector * 4))
  nil)


;;
;;  simple-vector
;;
(deftest typep-simple-vector.1
  (typep #(10 20 30) 'simple-vector)
  t)

(deftest typep-simple-vector.2
  (typep #(10 20 30) '(simple-vector))
  t)

(deftest typep-simple-vector.3
  (typep "Hello" 'simple-vector)
  nil)

(deftest typep-simple-vector.4
  (typep #*10111 'simple-vector)
  nil)

(deftest typep-simple-vector.5
  (typep (make-array 5 :adjustable t) 'simple-vector)
  nil)

(deftest typep-simple-vector.6
  (typep 100 'simple-vector)
  nil)

;;  object vector
(deftest typep-simple-vector-vector.1
  (typep #() 'simple-vector)
  t)

(deftest typep-simple-vector-vector.2
  (typep #(1 2 3) '(simple-vector))
  t)

(deftest typep-simple-vector-vector.3
  (typep #(1 2 3) '(simple-vector *))
  t)

(deftest typep-simple-vector-vector.4
  (typep #(1 2 3) '(simple-vector 3))
  t)

(deftest typep-simple-vector-vector.5
  (typep #(1 2 3) '(simple-vector 4))
  nil)

;;  object array
(deftest typep-simple-vector-array.1
  (typep (make-array 3 :element-type t) 'simple-vector)
  t)

(deftest typep-simple-vector-array.2
  (typep (make-array 3 :element-type t) '(simple-vector *))
  t)

(deftest typep-simple-vector-array.3
  (typep (make-array 3 :element-type 'character) '(simple-vector *))
  nil)

(deftest typep-simple-vector-array.4
  (typep (make-array 3) '(simple-vector 3))
  t)

(deftest typep-simple-vector-array.5
  (typep (make-array 3) '(simple-vector 4))
  nil)

(deftest typep-simple-vector-array.6
  (typep (make-array '(3 4)) 'simple-vector)
  nil)


;;
;;  bit-vector
;;
(deftest typep-bit-vector.1
  (typep #*11011 'bit-vector)
  t)

(deftest typep-bit-vector.2
  (typep #*11011 '(bit-vector))
  t)

(deftest typep-bit-vector.3
  (typep "Hello" 'bit-vector)
  nil)

(deftest typep-bit-vector.4
  (typep #(1 0 1) 'bit-vector)
  nil)

(deftest typep-bit-vector.5
  (typep (make-array 5 :element-type 'bit :adjustable t) 'bit-vector)
  t)

(deftest typep-bit-vector.6
  (typep (make-array '(2 3) :element-type 'bit) 'bit-vector)
  nil)

(deftest typep-bit-vector.7
  (typep 100 'bit-vector)
  nil)

;;  object array
(deftest typep-bit-vector-array.1
  (typep (make-array 3 :element-type 'bit) 'bit-vector)
  t)

(deftest typep-bit-vector-array.2
  (typep (make-array 3 :element-type 'bit) '(bit-vector *))
  t)

(deftest typep-bit-vector-array.3
  (typep (make-array 3 :element-type 't) '(bit-vector *))
  nil)

(deftest typep-bit-vector-array.4
  (typep (make-array 3 :element-type 'bit) '(bit-vector 3))
  t)

(deftest typep-bit-vector-array.5
  (typep (make-array 3 :element-type 'bit) '(bit-vector 4))
  nil)

(deftest typep-bit-vector-array.6
  (typep (make-array '(3 4) :element-type 'bit) 'bit-vector)
  nil)

;;  object vector
(deftest typep-bit-vector-vector.1
  (typep #* 'bit-vector)
  t)

(deftest typep-bit-vector-vector.2
  (typep #*110 '(bit-vector))
  t)

(deftest typep-bit-vector-vector.3
  (typep #*110 '(bit-vector *))
  t)

(deftest typep-bit-vector-vector.4
  (typep #*110 '(bit-vector 3))
  t)

(deftest typep-bit-vector-vector.5
  (typep #*110 '(bit-vector 4))
  nil)


;;
;;  simple-bit-vector
;;
(deftest typep-simple-bit-vector.1
  (typep #*11011 'simple-bit-vector)
  t)

(deftest typep-simple-bit-vector.2
  (typep #*11011 '(simple-bit-vector))
  t)

(deftest typep-simple-bit-vector.3
  (typep "Hello" 'simple-bit-vector)
  nil)

(deftest typep-simple-bit-vector.4
  (typep #(1 0 1) 'simple-bit-vector)
  nil)

(deftest typep-simple-bit-vector.5
  (typep (make-array 5 :element-type 'bit :adjustable t) 'simple-bit-vector)
  nil)

(deftest typep-simple-bit-vector.6
  (typep (make-array '(2 3) :element-type 'bit) 'simple-bit-vector)
  nil)

(deftest typep-simple-bit-vector.7
  (typep 100 'simple-bit-vector)
  nil)

;;  object array
(deftest typep-simple-bit-vector-array.1
  (typep (make-array 3 :element-type 'bit) 'simple-bit-vector)
  t)

(deftest typep-simple-bit-vector-array.2
  (typep (make-array 3 :element-type 'bit) '(simple-bit-vector *))
  t)

(deftest typep-simple-bit-vector-array.3
  (typep (make-array 3 :element-type 't) '(simple-bit-vector *))
  nil)

(deftest typep-simple-bit-vector-array.4
  (typep (make-array 3 :element-type 'bit) '(simple-bit-vector 3))
  t)

(deftest typep-simple-bit-vector-array.5
  (typep (make-array 3 :element-type 'bit) '(simple-bit-vector 4))
  nil)

(deftest typep-simple-bit-vector-array.6
  (typep (make-array '(3 4) :element-type 'bit) 'simple-bit-vector)
  nil)

;;  object vector
(deftest typep-simple-bit-vector-vector.1
  (typep #* 'simple-bit-vector)
  t)

(deftest typep-simple-bit-vector-vector.2
  (typep #*110 '(simple-bit-vector))
  t)

(deftest typep-simple-bit-vector-vector.3
  (typep #*110 '(simple-bit-vector *))
  t)

(deftest typep-simple-bit-vector-vector.4
  (typep #*110 '(simple-bit-vector 3))
  t)

(deftest typep-simple-bit-vector-vector.5
  (typep #*110 '(simple-bit-vector 4))
  nil)


;;
;;  string
;;
(deftest typep-string.1
  (typep "Hello" 'string)
  t)

(deftest typep-string.2
  (typep 10 'string)
  nil)

(deftest typep-string.3
  (typep "Hello" '(string))
  t)

(deftest typep-string.4
  (typep "Hello" '(string *))
  t)

(deftest typep-string.5
  (typep "Hello" '(string 5))
  t)

(deftest typep-string.6
  (typep "Hello" '(string 6))
  nil)

(deftest typep-string.7
  (typep (make-array-string "Hello") '(string 5))
  t)

(deftest typep-string.8
  (typep (make-array-string "Hello") '(string 6))
  nil)

(deftest typep-string.9
  (typep (make-array '(2 3) :element-type 'character) 'string)
  nil)

(deftest typep-string.10
  (typep #(#\a #\b #\c) 'string)
  nil)

(deftest typep-string.11
  (typep (make-array 3 :element-type 'character :adjustable t) 'string)
  t)


;;
;;  base-string
;;
(deftest typep-base-string.1
  (typep "Hello" 'base-string)
  t)

(deftest typep-base-string.2
  (typep 10 'base-string)
  nil)

(deftest typep-base-string.3
  (typep "Hello" '(base-string))
  t)

(deftest typep-base-string.4
  (typep "Hello" '(base-string *))
  t)

(deftest typep-base-string.5
  (typep "Hello" '(base-string 5))
  t)

(deftest typep-base-string.6
  (typep "Hello" '(base-string 6))
  nil)

(deftest typep-base-string.7
  (typep (make-array-string "Hello") '(base-string 5))
  t)

(deftest typep-base-string.8
  (typep (make-array-string "Hello") '(base-string 6))
  nil)

(deftest typep-base-string.9
  (typep (make-array '(2 3) :element-type 'character) 'base-string)
  nil)

(deftest typep-base-string.10
  (typep #(#\a #\b #\c) 'base-string)
  nil)

(deftest typep-base-string.11
  (typep (make-array 3 :element-type 'character :adjustable t) 'base-string)
  t)


;;
;;  simple-string
;;
(deftest typep-simple-string.1
  (typep "Hello" 'simple-string)
  t)

(deftest typep-simple-string.2
  (typep 10 'simple-string)
  nil)

(deftest typep-simple-string.3
  (typep "Hello" '(simple-string))
  t)

(deftest typep-simple-string.4
  (typep "Hello" '(simple-string *))
  t)

(deftest typep-simple-string.5
  (typep "Hello" '(simple-string 5))
  t)

(deftest typep-simple-string.6
  (typep "Hello" '(simple-string 6))
  nil)

(deftest typep-simple-string.7
  (typep (make-array-string "Hello") '(simple-string 5))
  t)

(deftest typep-simple-string.8
  (typep (make-array-string "Hello") '(simple-string 6))
  nil)

(deftest typep-simple-string.9
  (typep (make-array '(2 3) :element-type 'character) 'simple-string)
  nil)

(deftest typep-simple-string.10
  (typep #(#\a #\b #\c) 'simple-string)
  nil)

(deftest typep-simple-string.11
  (typep (make-array 3 :element-type 'character :adjustable t) 'simple-string)
  nil)


;;
;;  simple-base-string
;;
(deftest typep-simple-base-string.1
  (typep "Hello" 'simple-base-string)
  t)

(deftest typep-simple-base-string.2
  (typep 10 'simple-base-string)
  nil)

(deftest typep-simple-base-string.3
  (typep "Hello" '(simple-base-string))
  t)

(deftest typep-simple-base-string.4
  (typep "Hello" '(simple-base-string *))
  t)

(deftest typep-simple-base-string.5
  (typep "Hello" '(simple-base-string 5))
  t)

(deftest typep-simple-base-string.6
  (typep "Hello" '(simple-base-string 6))
  nil)

(deftest typep-simple-base-string.7
  (typep (make-array-string "Hello") '(simple-base-string 5))
  t)

(deftest typep-simple-base-string.8
  (typep (make-array-string "Hello") '(simple-base-string 6))
  nil)

(deftest typep-simple-base-string.9
  (typep (make-array '(2 3) :element-type 'character) 'simple-base-string)
  nil)

(deftest typep-simple-base-string.10
  (typep #(#\a #\b #\c) 'simple-base-string)
  nil)

(deftest typep-simple-base-string.11
  (typep (make-array 3 :element-type 'character :adjustable t) 'simple-base-string)
  nil)


;;
;;  sequence
;;
(deftest typep-sequence.1
  (typep nil 'sequence)
  t)

(deftest typep-sequence.2
  (typep (list 10 20 30) 'sequence)
  t)

(deftest typep-sequence.3
  (typep #(a b c) 'sequence)
  t)

(deftest typep-sequence.4
  (typep "Hello" 'sequence)
  t)

(deftest typep-sequence.5
  (typep (make-array 5 :element-type 'character) 'sequence)
  t)

(deftest typep-sequence.6
  (typep #*10111 'sequence)
  t)

(deftest typep-sequence.7
  (typep (make-array '(3 4) :element-type 'bit) 'sequence)
  nil)

(deftest typep-sequence.8
  (typep (make-array 5 :element-type '(signed-byte 8)) 'sequence)
  t)


;;
;;  array
;;
(deftest typep-array.1
  (typep 10 'array)
  nil)

(deftest typep-array.2
  (typep #1a(1 2 3) 'array)
  t)

(deftest typep-array.3
  (typep #1a(1 2 3) '(array))
  t)

(deftest typep-array.4
  (typep #1a(1 2 3) '(array *))
  t)

(deftest typep-array.5
  (typep #1a(1 2 3) '(array * *))
  t)

;;  object array
(deftest typep-array-array.1
  (typep #1a(1 2 3) '(array t))
  t)

(deftest typep-array-array.2
  (typep #1a(1 2 3) '(array character))
  nil)

(deftest typep-array-array.3
  (typep #1a(1 2 3) '(array t 1))
  t)

(deftest typep-array-array.4
  (typep #1a(1 2 3) '(array t 3))
  nil)

(deftest typep-array-array.5
  (typep #1a(1 2 3) '(array t (3)))
  t)

(deftest typep-array-array.6
  (typep #1a(1 2 3) '(array t (4)))
  nil)

(deftest typep-array-array.7
  (typep (make-array '(2 3) :element-type 'character :adjustable t)
         '(array character (2 3)))
  t)

(deftest typep-array-array.8
  (typep (make-array '(2 3) :element-type 'bit) '(array bit (2 3)))
  t)

(deftest typep-array-array.9
  (typep (make-array '(2 3) :element-type 'bit) '(array bit (2 *)))
  t)

(deftest typep-array-array.10
  (typep (make-array 10 :fill-pointer 5) '(array * (10)))
  t)

(deftest typep-array-array.11
  (typep #0a() '(array * ()))
  t)

(deftest typep-array-array.12
  (typep #0a() '(array * (4)))
  nil)

;;  object vector
(deftest typep-array-vector.1
  (typep #(1 2 3) '(array t))
  t)

(deftest typep-array-vector.2
  (typep #(1 2 3) '(array *))
  t)

(deftest typep-array-vector.3
  (typep #(1 2 3) '(array character))
  nil)

(deftest typep-array-vector.4
  (typep #(1 2 3) '(array t *))
  t)

(deftest typep-array-vector.5
  (typep #(1 2 3) '(array t 1))
  t)

(deftest typep-array-vector.6
  (typep #(1 2 3) '(array t 3))
  nil)

(deftest typep-array-vector.7
  (typep #(1 2 3) '(array t (3)))
  t)

(deftest typep-array-vector.8
  (typep #(1 2 3) '(array t (*)))
  t)

(deftest typep-array-vector.9
  (typep #(1 2 3) '(array t (4)))
  nil)

(deftest typep-array-vector.10
  (typep #(1 2 3) '(array t (3 3 3)))
  nil)

;;  object string
(deftest typep-array-string.1
  (typep "ABC" '(array character))
  t)

(deftest typep-array-string.2
  (typep "ABC" '(array *))
  t)

(deftest typep-array-string.3
  (typep "ABC" '(array t))
  nil)

(deftest typep-array-string.4
  (typep "ABC" '(array character *))
  t)

(deftest typep-array-string.5
  (typep "ABC" '(array character 1))
  t)

(deftest typep-array-string.6
  (typep "ABC" '(array character 3))
  nil)

(deftest typep-array-string.7
  (typep "ABC" '(array character (3)))
  t)

(deftest typep-array-string.8
  (typep "ABC" '(array character (*)))
  t)

(deftest typep-array-string.9
  (typep "ABC" '(array character (4)))
  nil)

(deftest typep-array-string.10
  (typep "ABC" '(array character (3 3 3)))
  nil)

;;  object bit-vector
(deftest typep-array-bit-vector.1
  (typep #*110 '(array bit))
  t)

(deftest typep-array-bit-vector.2
  (typep #*110 '(array *))
  t)

(deftest typep-array-bit-vector.3
  (typep #*110 '(array t))
  nil)

(deftest typep-array-bit-vector.4
  (typep #*110 '(array bit *))
  t)

(deftest typep-array-bit-vector.5
  (typep #*110 '(array bit 1))
  t)

(deftest typep-array-bit-vector.6
  (typep #*110 '(array bit 3))
  nil)

(deftest typep-array-bit-vector.7
  (typep #*110 '(array bit (3)))
  t)

(deftest typep-array-bit-vector.8
  (typep #*110 '(array bit (*)))
  t)

(deftest typep-array-bit-vector.9
  (typep #*110 '(array bit (4)))
  nil)

(deftest typep-array-bit-vector.10
  (typep #*110 '(array bit (3 3 3)))
  nil)


;;
;;  simple-array
;;
(deftest typep-simple-array.1
  (typep 10 'simple-array)
  nil)

(deftest typep-simple-array.2
  (typep #1a(1 2 3) 'simple-array)
  t)

(deftest typep-simple-array.3
  (typep #1a(1 2 3) '(simple-array))
  t)

(deftest typep-simple-array.4
  (typep #1a(1 2 3) '(simple-array *))
  t)

(deftest typep-simple-array.5
  (typep #1a(1 2 3) '(simple-array * *))
  t)

;;  object array
(deftest typep-simple-array-array.1
  (typep #1a(1 2 3) '(simple-array t))
  t)

(deftest typep-simple-array-array.2
  (typep #1a(1 2 3) '(simple-array character))
  nil)

(deftest typep-simple-array-array.3
  (typep #1a(1 2 3) '(simple-array t 1))
  t)

(deftest typep-simple-array-array.4
  (typep #1a(1 2 3) '(simple-array t 3))
  nil)

(deftest typep-simple-array-array.5
  (typep #1a(1 2 3) '(simple-array t (3)))
  t)

(deftest typep-simple-array-array.6
  (typep #1a(1 2 3) '(simple-array t (4)))
  nil)

(deftest typep-simple-array-array.7
  (typep (make-array '(2 3) :element-type 'character :adjustable t)
         '(simple-array character (2 3)))
  nil)

(deftest typep-simple-array-array.8
  (typep (make-array '(2 3) :element-type 'bit) '(simple-array bit (2 3)))
  t)

(deftest typep-simple-array-array.9
  (typep (make-array '(2 3) :element-type 'bit) '(simple-array bit (2 *)))
  t)

(deftest typep-simple-array-array.10
  (typep (make-array 10 :fill-pointer 5) '(simple-array * (10)))
  nil)

(deftest typep-simple-array-array.11
  (typep #0a() '(simple-array * ()))
  t)

(deftest typep-simple-array-array.12
  (typep #0a() '(simple-array * (4)))
  nil)

;;  object vector
(deftest typep-simple-array-vector.1
  (typep #(1 2 3) '(simple-array t))
  t)

(deftest typep-simple-array-vector.2
  (typep #(1 2 3) '(simple-array *))
  t)

(deftest typep-simple-array-vector.3
  (typep #(1 2 3) '(simple-array character))
  nil)

(deftest typep-simple-array-vector.4
  (typep #(1 2 3) '(simple-array t *))
  t)

(deftest typep-simple-array-vector.5
  (typep #(1 2 3) '(simple-array t 1))
  t)

(deftest typep-simple-array-vector.6
  (typep #(1 2 3) '(simple-array t 3))
  nil)

(deftest typep-simple-array-vector.7
  (typep #(1 2 3) '(simple-array t (3)))
  t)

(deftest typep-simple-array-vector.8
  (typep #(1 2 3) '(simple-array t (*)))
  t)

(deftest typep-simple-array-vector.9
  (typep #(1 2 3) '(simple-array t (4)))
  nil)

(deftest typep-simple-array-vector.10
  (typep #(1 2 3) '(simple-array t (3 3 3)))
  nil)

;;  object string
(deftest typep-simple-array-string.1
  (typep "ABC" '(simple-array character))
  t)

(deftest typep-simple-array-string.2
  (typep "ABC" '(simple-array *))
  t)

(deftest typep-simple-array-string.3
  (typep "ABC" '(simple-array t))
  nil)

(deftest typep-simple-array-string.4
  (typep "ABC" '(simple-array character *))
  t)

(deftest typep-simple-array-string.5
  (typep "ABC" '(simple-array character 1))
  t)

(deftest typep-simple-array-string.6
  (typep "ABC" '(simple-array character 3))
  nil)

(deftest typep-simple-array-string.7
  (typep "ABC" '(simple-array character (3)))
  t)

(deftest typep-simple-array-string.8
  (typep "ABC" '(simple-array character (*)))
  t)

(deftest typep-simple-array-string.9
  (typep "ABC" '(simple-array character (4)))
  nil)

(deftest typep-simple-array-string.10
  (typep "ABC" '(simple-array character (3 3 3)))
  nil)

;;  object bit-vector
(deftest typep-simple-array-bit-vector.1
  (typep #*110 '(simple-array bit))
  t)

(deftest typep-simple-array-bit-vector.2
  (typep #*110 '(simple-array *))
  t)

(deftest typep-simple-array-bit-vector.3
  (typep #*110 '(simple-array t))
  nil)

(deftest typep-simple-array-bit-vector.4
  (typep #*110 '(simple-array bit *))
  t)

(deftest typep-simple-array-bit-vector.5
  (typep #*110 '(simple-array bit 1))
  t)

(deftest typep-simple-array-bit-vector.6
  (typep #*110 '(simple-array bit 3))
  nil)

(deftest typep-simple-array-bit-vector.7
  (typep #*110 '(simple-array bit (3)))
  t)

(deftest typep-simple-array-bit-vector.8
  (typep #*110 '(simple-array bit (*)))
  t)

(deftest typep-simple-array-bit-vector.9
  (typep #*110 '(simple-array bit (4)))
  nil)

(deftest typep-simple-array-bit-vector.10
  (typep #*110 '(simple-array bit (3 3 3)))
  nil)

