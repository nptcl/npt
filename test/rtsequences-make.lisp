;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function MAKE-SEQUENCE
;;

;;  list
(deftest make-sequence-list.1
  (make-sequence 'list 0)
  nil)

(deftest make-sequence-list.2
  (make-sequence 'list 5)
  (nil nil nil nil nil))

(deftest make-sequence-list.3
  (make-sequence 'list 5 :initial-element 10)
  (10 10 10 10 10))


;;  cons
(deftest-error make-sequence-cons.1
  (make-sequence 'cons 0))

(deftest make-sequence-cons.2
  (make-sequence 'cons 5)
  (nil nil nil nil nil))

(deftest make-sequence-cons.3
  (make-sequence 'cons 5 :initial-element 10)
  (10 10 10 10 10))


;;  vector
(deftest make-sequence-vector.1
  (make-sequence 'vector 5)
  #(nil nil nil nil nil))

(deftest make-sequence-vector.2
  (make-sequence 'vector 5 :initial-element 'a)
  #(a a a a a))

(deftest make-sequence-vector.3
  (make-sequence '(vector t 5) 5)
  #(nil nil nil nil nil))

(deftest-error make-sequence-vector.4
  (make-sequence '(vector t 6) 5))

(deftest make-sequence-vector.5
  (let ((a (make-sequence '(vector character *) 5)))
    (values (stringp a) (length a)))
  t 5)

(deftest make-sequence-vector.6
  (make-sequence '(vector character *) 5 :initial-element #\a)
  "aaaaa")

(deftest make-sequence-vector.7
  (make-sequence '(vector bit *) 5)
  #*00000)

(deftest make-sequence-vector.8
  (make-sequence '(vector bit *) 5 :initial-element 1)
  #*11111)

(deftest make-sequence-vector.9
  (make-sequence '(vector (unsigned-byte 8) *) 5)
  #(0 0 0 0 0))

(deftest make-sequence-vector.10
  (make-sequence '(vector (unsigned-byte 32) *) 5 :initial-element 1)
  #(1 1 1 1 1))

(deftest make-sequence-vector.11
  (make-sequence '(vector long-float *) 5 :initial-element 1.25l0)
  #(1.25l0 1.25l0 1.25l0 1.25l0 1.25l0))

(deftest-error make-sequence-vector.13
  (make-sequence '(vector character) 3 :initial-element 10))


;;  vector-bit
(deftest make-sequence-vector-bit.1
  (make-sequence '(vector bit) 4)
  #*0000)

(deftest make-sequence-vector-bit.2
  (make-sequence '(vector bit) 4 :initial-element 1)
  #*1111)

(deftest-error make-sequence-vector-bit.3
  (make-sequence '(vector bit) 4 :initial-element 3))


;;  vector-character
(deftest make-sequence-character.1
  (make-sequence '(vector character) 3)
  #(#\u0 #\u0 #\u0))

(deftest make-sequence-character.2
  (make-sequence '(vector character) 3 :initial-element #\A)
  #(#\A #\A #\A))

(deftest-error make-sequence-character.3
  (make-sequence '(vector character) 3 :initial-element 10))


;;  vector-signed
(deftest make-sequence-signed.1
  (make-sequence '(vector signed-byte) 3)
  #(nil nil nil))  ;; #(0 0 0)

(deftest make-sequence-signed.2
  (make-sequence '(vector (signed-byte 8)) 3)
  #(0 0 0))

(deftest make-sequence-signed.3
  (make-sequence '(vector (signed-byte 8)) 3 :initial-element -2)
  #(-2 -2 -2))

(deftest-error make-sequence-signed.4
  (make-sequence '(vector (signed-byte 8)) 3 :initial-element 128))

(deftest-error make-sequence-signed.5
  (make-sequence '(vector (signed-byte 8)) 3 :initial-element -129))

(deftest make-sequence-signed.6
  (make-sequence '(vector (signed-byte 32)) 4 :initial-element 128)
  #(128 128 128 128))


;;  vector-unsigned
(deftest make-sequence-unsigned.1
  (make-sequence '(vector unsigned-byte) 3)
  #(nil nil nil))  ;; #(0 0 0)

(deftest make-sequence-unsigned.2
  (make-sequence '(vector (unsigned-byte 8)) 3)
  #(0 0 0))

(deftest make-sequence-unsigned.3
  (make-sequence '(vector (unsigned-byte 8)) 3 :initial-element 2)
  #(2 2 2))

(deftest-error make-sequence-unsigned.4
  (make-sequence '(vector (unsigned-byte 8)) 3 :initial-element -2))

(deftest-error make-sequence-unsigned.5
  (make-sequence '(vector (unsigned-byte 8)) 3 :initial-element 256))

(deftest make-sequence-unsigned.6
  (make-sequence '(vector (unsigned-byte 32)) 4 :initial-element 256)
  #(256 256 256 256))


;;  simple-vector
(deftest make-sequence-simple-vector.1
  (make-sequence 'simple-vector 3 :initial-element nil)
  #(nil nil nil))

(deftest make-sequence-simple-vector.2
  (make-sequence '(simple-vector 3) 3)
  #(nil nil nil))

(deftest-error make-sequence-simple-vector.3
  (make-sequence '(simple-vector 3) 4))

(deftest make-sequence-simple-vector.4
  (make-sequence 'simple-vector 5 :initial-element 20)
  #(20 20 20 20 20))


;;  string
(deftest make-sequence-string.1
  (make-sequence 'string 5 :initial-element #\A)
  "AAAAA")

(deftest make-sequence-string.2
  (make-sequence '(string 5) 5 :initial-element #\A)
  "AAAAA")

(deftest-error make-sequence-string.3
  (make-sequence '(string 5) 4 :initial-element #\A))

(deftest make-sequence-string.4
  (stringp
    (make-sequence 'string 5))
  t)


;;  array
(deftest make-sequence-array.1
  (make-sequence 'array 5)
  #(nil nil nil nil nil))

(deftest make-sequence-array.2
  (make-sequence '(array * *) 5)
  #(nil nil nil nil nil))

(deftest make-sequence-array.3
  (make-sequence '(array * 1) 5)
  #(nil nil nil nil nil))

(deftest make-sequence-array.4
  (make-sequence '(array * (*)) 5)
  #(nil nil nil nil nil))

(deftest make-sequence-array.5
  (make-sequence '(array * (5)) 5)
  #(nil nil nil nil nil))

(deftest-error make-sequence-array.6
  (make-sequence '(array * 2) 5))

(deftest-error make-sequence-array.7
  (make-sequence '(array * (* *)) 5))

(deftest-error make-sequence-array.8
  (make-sequence '(array * (4)) 5))

(deftest make-sequence-array.9
  (make-sequence '(array character) 5 :initial-element #\A)
  "AAAAA")


;;  bit-vector
(deftest make-sequence-bit-vector.1
  (make-sequence 'bit-vector 5 :initial-element 1)
  #*11111)

(deftest make-sequence-bit-vector.2
  (make-sequence '(bit-vector 5) 5 :initial-element 0)
  #*00000)

(deftest-error make-sequence-bit-vector.3
  (make-sequence '(bit-vector 5) 4))

(deftest make-sequence-bit-vector.4
  (make-sequence 'simple-bit-vector 5 :initial-element 1)
  #*11111)


;;  not
(deftest-error make-sequence-not.1
  (make-sequence '(not list) 5))

(deftest-error make-sequence-not.2
  (make-sequence '(not cons) 5 :initial-element 'a))

(deftest-error make-sequence-not.3
  (make-sequence '(not vector) 5 :initial-element 'a))

(deftest-error make-sequence-not.4
  (make-sequence '(not (simple-vector 3)) 3))

(deftest-error make-sequence-not.5
  (make-sequence '(not string) 5 :initial-element #\A))

(deftest-error make-sequence-not.6
  (make-sequence '(not array) 5))

(deftest-error make-sequence-not.7
  (make-sequence '(not bit-vector) 5 :initial-element 1))


;;  error
(deftest-error make-sequence-error.1
  (eval '(make-sequence 10 20))
  type-error)

(deftest-error make-sequence-error.2
  (eval '(make-sequence t :hello))
  type-error)

(deftest-error! make-sequence-error.3
  (eval '(make-sequence t)))

(deftest-error! make-sequence-error.4
  (eval '(make-sequence t 5 nil)))

(deftest-error! make-sequence-error.5
  (eval '(make-sequence t 5 :initial-element)))

(deftest-error! make-sequence-error.6
  (eval '(make-sequence t 5 :hello 10)))

;;  ANSI Common Lisp
(deftest make-sequence-test.1
  (make-sequence 'list 0)
  ())

(deftest make-sequence-test.2
  (make-sequence 'string 26 :initial-element #\.)
  "..........................")

(deftest make-sequence-test.3
  (make-sequence '(vector double-float) 2 :initial-element 1d0)
  #(1.0d0 1.0d0))

(deftest-error make-sequence-test.4
  (make-sequence '(vector * 2) 3))

(deftest-error make-sequence-test.5
  (make-sequence '(vector * 4) 3))

