;;
;;  ANSI COMMON LISP: 4. Types and Classes
;;

;;
;;  Function COERCE
;;
(deftest coerce.1
  (coerce 10 'integer)
  10)

(deftest coerce-test-sequence.1
  (coerce '(10 20 30) 'sequence)
  (10 20 30))

(deftest coerce-test-sequence.2
  (coerce #(10 20 30) 'sequence)
  #(10 20 30))

(deftest coerce-test-sequence.3
  (coerce "Hello" 'sequence)
  "Hello")

(deftest coerce-test-character.1
  (coerce #\A 'character)
  #\A)

(deftest coerce-test-character.2
  (coerce 'a 'character)
  #\A)

(deftest coerce-test-character.3
  (coerce "z" 'character)
  #\z)

(deftest-error coerce-test-character.4
  (coerce "12" 'character)
  type-error)

(deftest coerce-test-complex.1
  (coerce 12.5 'complex)
  #c(12.5 0.0))

(deftest coerce-test-complex.2
  (coerce 12.5 '(complex double-float))
  #c(12.50d0 0.00d0))

(deftest coerce-test-complex.3
  (coerce 3 'complex)
  3)

(deftest coerce-test-float.1
  (coerce 3 'double-float)
  3.0d0)

(deftest coerce-test-float.2
  (coerce 3.0d0 'long-float)
  3.0L0)

(deftest coerce-test-function.1
  (eq #'car
      (coerce #'car 'function))
  t)

(deftest coerce-test-function.2
  (eq #'car
      (coerce 'car 'function))
  t)

(deftest-error coerce-test-function.3
  (coerce 'dotimes 'function)
  undefined-function)

(deftest-error! coerce-error.1
  (eval '(coerce 10)))

(deftest-error! coerce-error.2
  (eval '(coerce 10 'integer nil)))

(deftest-error coerce-error.3
  (eval '(coerce 10 20))
  type-error)


;;  coerce
(deftest-error coerce-nil.1
  (coerce 12.3f0 'nil)
  type-error)

(deftest coerce-t.1
  (coerce 123 't)
  123)

(deftest-error coerce-asterisk.1
  (coerce 123 '*))


;;  float
(deftest coerce-float.1
  (coerce 12.3f0 'float)
  12.3f0)

(deftest coerce-float.2
  (coerce 12.3d0 'float)
  12.3d0)

(deftest coerce-float.3
  (coerce 12.3L0 'float)
  12.3L0)

(deftest coerce-float.4
  (coerce 10 'float)
  10.0f0)

(deftest coerce-float.5
  (coerce (make-bignum 11) 'float)
  11.0f0)

(deftest coerce-float.6
  (coerce (make-ratio 12 1) 'float)
  12.0f0)

(deftest-error coerce-float.7
  (coerce "Hello" 'float)
  type-error)


;;  single-float
(deftest coerce-single-float.1
  (coerce 12.3f0 'single-float)
  12.3f0)

(deftest coerce-single-float.2
  (coerce 12.3d0 'single-float)
  12.3f0)

(deftest coerce-single-float.3
  (coerce 12.3L0 'single-float)
  12.3f0)

(deftest coerce-single-float.4
  (coerce 10 'single-float)
  10.0f0)

(deftest coerce-single-float.5
  (coerce (make-bignum 11) 'single-float)
  11.0f0)

(deftest coerce-single-float.6
  (coerce (make-ratio 12 1) 'single-float)
  12.0f0)

(deftest-error coerce-single-float.7
  (coerce "Hello" 'single-float)
  type-error)


;;  double-float
(deftest-double coerce-double-float.1
  (coerce 12.3f0 'double-float)
  12.3d0 0.0d0 1e-6)

(deftest coerce-double-float.2
  (coerce 12.3d0 'double-float)
  12.3d0)

(deftest coerce-double-float.3
  (coerce 12.3L0 'double-float)
  12.3d0)

(deftest coerce-double-float.4
  (coerce 10 'double-float)
  10.0d0)

(deftest coerce-double-float.5
  (coerce (make-bignum 11) 'double-float)
  11.0d0)

(deftest coerce-double-float.6
  (coerce (make-ratio 12 1) 'double-float)
  12.0d0)

(deftest-error coerce-double-float.7
  (coerce "Hello" 'double-float)
  type-error)

;;  long-float
(deftest-long coerce-long-float.1
  (coerce 12.3f0 'long-float)
  12.3L0 0.0L0 1e-6)

(deftest-long coerce-long-float.2
  (coerce 12.3d0 'long-float)
  12.3L0 0.0L0 1.0L-14)

(deftest coerce-long-float.3
  (coerce 12.3L0 'long-float)
  12.3L0)

(deftest coerce-long-float.4
  (coerce 10 'long-float)
  10.0L0)

(deftest coerce-long-float.5
  (coerce (make-bignum 11) 'long-float)
  11.0L0)

(deftest coerce-long-float.6
  (coerce (make-ratio 12 1) 'long-float)
  12.0L0)

(deftest-error coerce-long-float.7
  (coerce "Hello" 'long-float)
  type-error)

;;  complex
(deftest coerce-complex.1
  (coerce 10 'complex)
  10)

(deftest coerce-complex.2
  (coerce 12.3 'complex)
  #c(12.3 0.0))

(deftest coerce-complex.3
  (coerce 12.3L0 '(complex double-float))
  #c(12.3d0 0.0d0))

(deftest coerce-complex.4
  (coerce #c(1.2d3 4.5d6) '(complex single-float))
  #c(1.2f3 4.5f6))

(deftest coerce-complex.5
  (coerce 12 '(complex long-float))
  #c(12.0L0 0.0L0))

(deftest-error coerce-complex.6
  (coerce 'car 'complex)
  type-error)


;;  character
(deftest coerce-character.1
  (coerce #\a 'character)
  #\a)

(deftest coerce-character.2
  (coerce "Z" 'character)
  #\Z)

(deftest coerce-character.3
  (coerce 'q 'character)
  #\Q)

(deftest-error coerce-character.4
  (coerce "ABC" 'character)
  type-error)

(deftest-error coerce-character.5
  (coerce 'hello 'character)
  type-error)

(deftest coerce-character.6
  (coerce #\u2000 'base-char)
  #\u2000)

(deftest-error coerce-character.7
  (coerce #\u2000 'standard-char)
  type-error)

(deftest-error coerce-character.8
  (coerce 100 'character)
  type-error)


;;  function
(deftest coerce-function.1
  (functionp
    (coerce 'car 'function))
  t)

(deftest-error coerce-function.2
  (coerce 'no-such-function 'function))
;; undefined-function

(deftest-error coerce-function.3
  (coerce 200 'function)
  type-error)

(deftest coerce-compiled-function.1
  (functionp
    (coerce 'car 'compiled-function))
  t)

(deftest-error coerce-compiled-function.2
  (coerce 'no-such-function 'compiled-function))
;; undefined-function

(deftest-error coerce-compiled-function.3
  (coerce 200 'compiled-function)
  type-error)


;;  list
(deftest coerce-list.1
  (coerce nil 'list)
  nil)

(deftest coerce-list.2
  (coerce '(10 20 30) 'list)
  (10 20 30))

(deftest coerce-list.3
  (coerce "Hello" 'list)
  (#\H #\e #\l #\l #\o))

(deftest coerce-list.4
  (coerce #(10 20 30) 'list)
  (10 20 30))

(deftest coerce-list.5
  (coerce #*10011 'list)
  (1 0 0 1 1))

(deftest coerce-list.6
  (coerce #1a(10 20 "Hello" #\a) 'list)
  (10 20 "Hello" #\a))

(deftest-error coerce-list.7
  (coerce #2a((10 20) ("Hello" #\a)) 'list)
  type-error)

(deftest-error coerce-list.8
  (coerce 100 'list)
  type-error)

(deftest coerce-list.9
  (coerce "Hello" 'cons)
  (#\H #\e #\l #\l #\o))

(deftest-error coerce-list.10
  (coerce "Hello" '(cons integer string))
  type-error)


;;  (coerce vector '(array t))
(deftest coerce-av.1
  (let* ((array #(10 20 30))
         (result (coerce array '(array t))))
    (values
      (vectorp result)
      result))
  t
  #(10 20 30))

(deftest coerce-av.2
  (let* ((array #(1 1 0 0 1))
         (result (coerce array '(array bit))))
    (values
      (bit-vector-p result)
      result))
  t
  #*11001)

(deftest coerce-av.3
  (let* ((array #(#\H #\e #\l #\l #\o))
         (result (coerce array '(array character))))
    (values
      (stringp result)
      result))
  t
  "Hello")

(deftest coerce-av.4a
  (let* ((array #(10 20 30 40 50))
         (result (coerce array '(array (signed-byte 8)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8) t #(10 20 30 40 50))

(deftest coerce-av.4b
  (let* ((array #(10 20 30 40 50))
         (result (coerce array '(array (signed-byte 16)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16) t #(10 20 30 40 50))

(deftest coerce-av.4c
  (let* ((array #(10 20 30 40 50))
         (result (coerce array '(array (signed-byte 32)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-av.4d
  (let* ((array #(10 20 30 40 50))
         (result (coerce array '(array (signed-byte 64)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64) t #(10 20 30 40 50))

(deftest coerce-av.5a
  (let* ((array #(10 20 30 40 50))
         (result (coerce array '(array (unsigned-byte 8)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8) t #(10 20 30 40 50))

(deftest coerce-av.5b
  (let* ((array #(10 20 30 40 50))
         (result (coerce array '(array (unsigned-byte 16)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16) t #(10 20 30 40 50))

(deftest coerce-av.5c
  (let* ((array #(10 20 30 40 50))
         (result (coerce array '(array (unsigned-byte 32)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-av.5d
  (let* ((array #(10 20 30 40 50))
         (result (coerce array '(array (unsigned-byte 64)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64) t #(10 20 30 40 50))

(deftest coerce-av.6
  (let* ((array #(12.0f0 0.25d0 300.0L0))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  single-float t #(12.0f0 0.25f0 300.0f0))

(deftest coerce-av.7
  (let* ((array #(12.0f0 0.25d0 300.0L0))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  double-float t #(12.0d0 0.25d0 300.0d0))

(deftest coerce-av.8
  (let* ((array #(12.0f0 0.25d0 300.0L0))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  long-float t #(12.0L0 0.25L0 300.0L0))


;;  (coerce string '(array t))
(deftest coerce-as.1
  (let* ((array "Hello")
         (result (coerce array '(array t))))
    (values
      (vectorp result)
      result))
  t
  #(#\H #\e #\l #\l #\o))

;;  (coerce string '(array character))
(deftest coerce-as.2
  (coerce "Hello" '(array character))
  "Hello")

;;  (coerce bit-vector '(array t))
(deftest coerce-ab.1
  (let* ((array #*11001)
         (result (coerce array '(array t))))
    (values
      (vectorp result)
      result))
  t
  #(1 1 0 0 1))

(deftest coerce-ab.2a
  (let* ((array #*11001)
         (result (coerce array '(array (signed-byte 8)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8)
  t
  #(1 1 0 0 1))

(deftest coerce-ab.2b
  (let* ((array #*11001)
         (result (coerce array '(array (signed-byte 16)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16)
  t
  #(1 1 0 0 1))

(deftest coerce-ab.2c
  (let* ((array #*11001)
         (result (coerce array '(array (signed-byte 32)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32)
  t
  #(1 1 0 0 1))

#+64-bit
(deftest coerce-ab.2d
  (let* ((array #*11001)
         (result (coerce array '(array (signed-byte 64)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64)
  t
  #(1 1 0 0 1))

(deftest coerce-ab.3a
  (let* ((array #*11001)
         (result (coerce array '(array (unsigned-byte 8)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8)
  t
  #(1 1 0 0 1))

(deftest coerce-ab.3b
  (let* ((array #*11001)
         (result (coerce array '(array (unsigned-byte 16)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16)
  t
  #(1 1 0 0 1))

(deftest coerce-ab.3c
  (let* ((array #*11001)
         (result (coerce array '(array (unsigned-byte 32)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32)
  t
  #(1 1 0 0 1))

#+64-bit
(deftest coerce-ab.3d
  (let* ((array #*11001)
         (result (coerce array '(array (unsigned-byte 64)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64)
  t
  #(1 1 0 0 1))


;;  (coerce list '(array t))
(deftest coerce-al.1
  (let* ((array '(10 20 30))
         (result (coerce array '(array t))))
    (values
      (vectorp result)
      result))
  t
  #(10 20 30))

(deftest coerce-al.2
  (let* ((array '(1 1 0 0 1))
         (result (coerce array '(array bit))))
    (values
      (bit-vector-p result)
      result))
  t
  #*11001)

(deftest coerce-al.3
  (let* ((array '(#\H #\e #\l #\l #\o))
         (result (coerce array '(array character))))
    (values
      (stringp result)
      result))
  t
  "Hello")

(deftest coerce-al.4a
  (let* ((array '(10 20 30 40 50))
         (result (coerce array '(array (signed-byte 8)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8) t #(10 20 30 40 50))

(deftest coerce-al.4b
  (let* ((array '(10 20 30 40 50))
         (result (coerce array '(array (signed-byte 16)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16) t #(10 20 30 40 50))

(deftest coerce-al.4c
  (let* ((array '(10 20 30 40 50))
         (result (coerce array '(array (signed-byte 32)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-al.4d
  (let* ((array '(10 20 30 40 50))
         (result (coerce array '(array (signed-byte 64)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64) t #(10 20 30 40 50))

(deftest coerce-al.5a
  (let* ((array '(10 20 30 40 50))
         (result (coerce array '(array (unsigned-byte 8)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8) t #(10 20 30 40 50))

(deftest coerce-al.5b
  (let* ((array '(10 20 30 40 50))
         (result (coerce array '(array (unsigned-byte 16)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16) t #(10 20 30 40 50))

(deftest coerce-al.5c
  (let* ((array '(10 20 30 40 50))
         (result (coerce array '(array (unsigned-byte 32)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-al.5d
  (let* ((array '(10 20 30 40 50))
         (result (coerce array '(array (unsigned-byte 64)))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64) t #(10 20 30 40 50))

(deftest coerce-al.6
  (let* ((array '(12.0f0 0.25d0 300.0L0))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  single-float t #(12.0f0 0.25f0 300.0f0))

(deftest coerce-al.7
  (let* ((array '(12.0f0 0.25d0 300.0L0))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  double-float t #(12.0d0 0.25d0 300.0d0))

(deftest coerce-al.8
  (let* ((array '(12.0f0 0.25d0 300.0L0))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  long-float t #(12.0L0 0.25L0 300.0L0))

;;  ANSI Common Lisp
(deftest coerce-test.1
  (coerce '(a b c) 'vector)
  #(a b c))

(deftest coerce-test.2
  (coerce 'a 'character)
  #\A)

(deftest coerce-test.3
  (coerce 4.56 'complex)
  #c(4.56 0.0))

(deftest coerce-test.4
  (coerce 4.5s0 'complex)
  #c(4.5s0 0.0s0))

(deftest coerce-test.5
  (coerce 7/2 'complex)
  7/2)

(deftest coerce-test.6
  (coerce 0 'short-float)
  0.0s0)

(deftest coerce-test.7
  (coerce 3.5L0 'float)
  3.5L0)

(deftest coerce-test.8
  (coerce 7/2 'float)
  3.5)

(deftest coerce-test.9
  (coerce (cons 1 2) t)
  (1 . 2))

(deftest-error coerce-test.10
  (coerce '(a b c) '(vector * 4))
  type-error)

(deftest-error coerce-test.11
  (coerce #(a b c) '(vector * 4))
  type-error)

(deftest-error coerce-test.12
  (coerce '(a b c) '(vector * 2))
  type-error)

(deftest-error coerce-test.13
  (coerce #(a b c) '(vector * 2))
  type-error)

(deftest-error coerce-test.14
  (coerce "foo" '(string 2))
  type-error)

(deftest-error coerce-test.15
  (coerce #(#\a #\b #\c) '(string 2))
  type-error)

(deftest-error coerce-test.16
  (coerce '(0 1) '(simple-bit-vector 3))
  type-error)

(deftest-error coerce-test.17
  (coerce 10 'nil)
  type-error)

(deftest-error coerce-test.18
  (coerce #(1 2 3) '(array (4)))
  type-error)

(deftest-error coerce-test.19
  (coerce 0.25 'ratio)
  type-error)


;;
;;  bugfix
;;
(deftest coerce-bugfix.1
  (coerce '(10 20 30) 'vector)
  #(10 20 30))

(deftest coerce-bugfix.2
  (coerce #(10 20 30) 'list)
  (10 20 30))

(deftest coerce-bugfix.3
  (coerce #*11001 'vector)
  #(1 1 0 0 1))

(deftest-error coerce-bugfix-not.1
  (coerce 12.3f0 '(not float)))

(deftest coerce-bugfix-not.2
  (coerce 'car '(not function))
  car)

(deftest coerce-bugfix-not.3
  (let* ((array #(10 20 30 40 50))
         (result (coerce array '(not (array (signed-byte 8))))))
    (values
      (array-element-type result)
      (array-specialized-p result)
      result))
  t nil #(10 20 30 40 50))

