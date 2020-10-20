;;
;;  ANSI COMMON LISP: 15. Arrays
;;

;;
;;  Function ARRAY-ELEMENT-TYPE
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

;;  error
(deftest-error array-element-type-error.1
  (eval '(array-element-type 100))
  type-error)

(deftest-error! array-element-type-error.2
  (eval '(array-element-type)))

(deftest-error! array-element-type-error.3
  (eval '(array-element-type "Hello" nil)))

;;  ANSI Common Lisp
(deftest array-element-type-test.1
  (array-element-type (make-array 4))
  t)

(deftest array-element-type-test.2
  (array-element-type (make-array 12 :element-type '(unsigned-byte 8)))
  (unsigned-byte 8))

(deftest array-element-type-test.3
  (array-element-type (make-array 12 :element-type '(unsigned-byte 5)))
  (unsigned-byte 8))

(deftest array-element-type-test.4
  (array-element-type (make-array 5 :element-type '(mod 5)))
  (unsigned-byte 8))


;;
;;  Function UPGRADED-ARRAY-ELEMENT-TYPE
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

(deftest-error upgraded-array-element-type-error.1
  (eval '(upgraded-array-element-type 10))
  type-error)

(deftest-error upgraded-array-element-type-error.2
  (eval '(upgraded-array-element-type 'integer 20))
  type-error)

(deftest-error! upgraded-array-element-type-error.3
  (eval '(upgraded-array-element-type)))

(deftest-error! upgraded-array-element-type-error.4
  (eval '(upgraded-array-element-type 'integer nil nil)))

