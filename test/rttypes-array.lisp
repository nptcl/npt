;;
;;  ANSI COMMON LISP: 4. Types and Classes
;;

;;  (coerce array '(array *))
(deftest coerce-aa-asterisk.1
  (coerce #2a((10 20) (30 40)) '(array t))
  #2a((10 20) (30 40)))

;;  (coerce array '(array t))
(deftest coerce-aa-t.1
  (let* ((array (make-array '(2 3) :element-type t
                            :initial-contents '((10 20 30) (#\a #\b #\c))))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      array
      (array-element-type result)
      result))
  t
  #2a((10 20 30) (#\a #\b #\c))
  t
  #2a((10 20 30) (#\a #\b #\c)))

(deftest coerce-aa-t.2
  (let* ((array (make-array '5 :element-type 'bit
                            :initial-contents '(1 0 0 1 1)))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  bit t #*10011
  t t #(1 0 0 1 1))

(deftest coerce-aa-t.3
  (let* ((array (make-array '5 :element-type 'character
                            :initial-contents '(#\H #\e #\l #\l #\o)))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  character t "Hello"
  t t #(#\H #\e #\l #\l #\o))

(deftest coerce-aa-t.4a
  (let* ((array (make-array '5 :element-type '(signed-byte 8)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  (signed-byte 8) t #(10 -20 30 40 50)
  t t #(10 -20 30 40 50))

(deftest coerce-aa-t.4b
  (let* ((array (make-array '5 :element-type '(signed-byte 16)
                            :initial-contents '(10 -20 30 40 1000)))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  (signed-byte 16) t #(10 -20 30 40 1000)
  t t #(10 -20 30 40 1000))

(deftest coerce-aa-t.4c
  (let* ((array (make-array '5 :element-type '(signed-byte 32)
                            :initial-contents '(10 -20 30 40 1000)))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  (signed-byte 32) t #(10 -20 30 40 1000)
  t t #(10 -20 30 40 1000))

#+64-bit
(deftest coerce-aa-t.4d
  (let* ((array (make-array '5 :element-type '(signed-byte 64)
                            :initial-contents '(10 -20 30 40 1000)))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  (signed-byte 64) t #(10 -20 30 40 1000)
  t t #(10 -20 30 40 1000))

(deftest coerce-aa-t.5a
  (let* ((array (make-array '5 :element-type '(unsigned-byte 8)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  (unsigned-byte 8) t #(10 20 30 40 50)
  t t #(10 20 30 40 50))

(deftest coerce-aa-t.5b
  (let* ((array (make-array '5 :element-type '(unsigned-byte 16)
                            :initial-contents '(10 20 30 40 1000)))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  (unsigned-byte 16) t #(10 20 30 40 1000)
  t t #(10 20 30 40 1000))

(deftest coerce-aa-t.5c
  (let* ((array (make-array '5 :element-type '(unsigned-byte 32)
                            :initial-contents '(10 20 30 40 1000)))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  (unsigned-byte 32) t #(10 20 30 40 1000)
  t t #(10 20 30 40 1000))

#+64-bit
(deftest coerce-aa-t.5d
  (let* ((array (make-array '5 :element-type '(unsigned-byte 64)
                            :initial-contents '(10 20 30 40 1000)))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  (unsigned-byte 64) t #(10 20 30 40 1000)
  t t #(10 20 30 40 1000))

(deftest coerce-aa-t.6
  (let* ((array (make-array '3 :element-type 'single-float
                            :initial-contents '(1.2f0 2.3f0 -3.4f0)))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  single-float t #(1.2f0 2.3f0 -3.4f0)
  t t #(1.2f0 2.3f0 -3.4f0))

(deftest coerce-aa-t.7
  (let* ((array (make-array '3 :element-type 'double-float
                            :initial-contents '(1.2d0 2.3d0 -3.4d0)))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  double-float t #(1.2d0 2.3d0 -3.4d0)
  t t #(1.2d0 2.3d0 -3.4d0))

(deftest coerce-aa-t.8
  (let* ((array (make-array '3 :element-type 'long-float
                            :initial-contents '(1.2L0 2.3L0 -3.4L0)))
         (result (coerce array '(array t))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  long-float t #(1.2L0 2.3L0 -3.4L0)
  t t #(1.2L0 2.3L0 -3.4L0))

(deftest coerce-aa-t.9a
  (let* ((array (make-array '(2 3) :element-type 'single-float
                            :initial-contents
                            '((1.2f0 2.3f0 -3.4f0) (5.6f0 7.8f0 -9.0f0))))
         (result (coerce array '(array t (2 3)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-general-p result)
      result))
  single-float t #2a((1.2f0 2.3f0 -3.4f0) (5.6f0 7.8f0 -9.0f0))
  t t #2a((1.2f0 2.3f0 -3.4f0) (5.6f0 7.8f0 -9.0f0)))

(deftest-error coerce-aa-t.9b
  (let ((array (make-array '(2 3) :element-type 'single-float
                           :initial-contents
                           '((1.2f0 2.3f0 -3.4f0) (5.6f0 7.8f0 -9.0f0)))))
    (coerce array '(array t (2 4))))
  type-error)

;;  (coerce array '(array bit))
(deftest coerce-aa-bit.1
  (let* ((array (make-array '5 :element-type t
                            :initial-contents '(1 0 0 1 1)))
         (result (coerce array '(array bit))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      (typep result 'bit-vector)
      result))
  t t #(1 0 0 1 1)
  bit nil t #*10011)

(deftest coerce-aa-bit.2
  (let* ((array (make-array '5 :element-type 'bit
                            :initial-contents '(1 0 0 1 1)))
         (result (coerce array '(array bit))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      (typep result 'bit-vector)
      result))
  bit t #(1 0 0 1 1)
  bit nil t #*10011)

(deftest coerce-aa-bit.3a
  (let* ((array (make-array '5 :element-type '(signed-byte 8)
                            :initial-contents '(1 0 0 1 1)))
         (result (coerce array '(array bit))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      (typep result 'bit-vector)
      result))
  (signed-byte 8) t #(1 0 0 1 1)
  bit nil t #*10011)

(deftest coerce-aa-bit.3b
  (let* ((array (make-array '5 :element-type '(signed-byte 16)
                            :initial-contents '(1 0 0 1 1)))
         (result (coerce array '(array bit))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      (typep result 'bit-vector)
      result))
  (signed-byte 16) t #(1 0 0 1 1)
  bit nil t #*10011)

(deftest coerce-aa-bit.3c
  (let* ((array (make-array '5 :element-type '(signed-byte 32)
                            :initial-contents '(1 0 0 1 1)))
         (result (coerce array '(array bit))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      (typep result 'bit-vector)
      result))
  (signed-byte 32) t #(1 0 0 1 1)
  bit nil t #*10011)

#+64-bit
(deftest coerce-aa-bit.3d
  (let* ((array (make-array '5 :element-type '(signed-byte 64)
                            :initial-contents '(1 0 0 1 1)))
         (result (coerce array '(array bit))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      (typep result 'bit-vector)
      result))
  (signed-byte 64) t #(1 0 0 1 1)
  bit nil t #*10011)

(deftest coerce-aa-bit.4a
  (let* ((array (make-array '5 :element-type '(unsigned-byte 8)
                            :initial-contents '(1 0 0 1 1)))
         (result (coerce array '(array bit))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      (typep result 'bit-vector)
      result))
  (unsigned-byte 8) t #(1 0 0 1 1)
  bit nil t #*10011)

(deftest coerce-aa-bit.4b
  (let* ((array (make-array '5 :element-type '(unsigned-byte 16)
                            :initial-contents '(1 0 0 1 1)))
         (result (coerce array '(array bit))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      (typep result 'bit-vector)
      result))
  (unsigned-byte 16) t #(1 0 0 1 1)
  bit nil t #*10011)

(deftest coerce-aa-bit.4c
  (let* ((array (make-array '5 :element-type '(unsigned-byte 32)
                            :initial-contents '(1 0 0 1 1)))
         (result (coerce array '(array bit))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      (typep result 'bit-vector)
      result))
  (unsigned-byte 32) t #(1 0 0 1 1)
  bit nil t #*10011)

#+64-bit
(deftest coerce-aa-bit.4d
  (let* ((array (make-array '5 :element-type '(unsigned-byte 64)
                            :initial-contents '(1 0 0 1 1)))
         (result (coerce array '(array bit))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      (typep result 'bit-vector)
      result))
  (unsigned-byte 64) t #(1 0 0 1 1)
  bit nil t #*10011)

(deftest coerce-aa-bit.5a
  (let* ((array (make-array '(2 3) :element-type t
                            :initial-contents '((1 0 0) (1 1 1))))
         (result (coerce array '(array bit (2 3)))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      (typep result 'bit-vector)
      result))
  t t #2a((1 0 0) (1 1 1))
  bit t nil #2a((1 0 0) (1 1 1)))

(deftest-error coerce-aa-bit.5b
  (let ((array (make-array '(2 3) :element-type t
                           :initial-contents '((1 0 0) (1 1 1)))))
    (coerce array '(array bit (2 4))))
  type-error)

(deftest-error coerce-aa-bit.6
  (let ((array (make-array '2 :element-type 'single-float
                           :initial-contents '(1.0 0.0))))
    (coerce array '(array bit)))
  type-error)

;;  (coerce array '(array character))
(deftest coerce-aa-character.1
  (let* ((array (make-array '5 :element-type t
                            :initial-contents "Hello"))
         (result (coerce array '(array character))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      (typep result 'string)
      result))
  t t "Hello"
  character nil t "Hello")

(deftest coerce-aa-character.2
  (let* ((array (make-array '(2 3) :element-type t
                            :initial-contents '((#\a #\b #\c) (#\X #\Y #\Z))))
         (result (coerce array '(array character))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      (typep result 'string)
      result))
  t t #2a((#\a #\b #\c) (#\X #\Y #\Z))
  character t nil #2a((#\a #\b #\c) (#\X #\Y #\Z)))

(deftest-error coerce-aa-character.3
  (let ((array (make-array '(2 3) :element-type t
                           :initial-contents '((#\a #\b #\c) (#\X #\Y #\Z)))))
    (coerce array '(array character (2 4))))
  type-error)

(deftest coerce-aa-character.4
  (let ((array (make-array 5 :element-type t
                           :initial-contents '(#\a #\b #\u1000 #\X #\Y))))
    (coerce array '(array standard-char))) ;; upgraded -> character
  #(#\a #\b #\u1000 #\X #\Y))


;;  (coerce array '(array (signed-byte 8)))
(deftest coerce-aa-signed8.1
  (let* ((array (make-array '5 :element-type t
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 8)))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  t t #(10 -20 30 40 50)
  (signed-byte 8) t #(10 -20 30 40 50))

(deftest coerce-aa-signed8.2
  (let* ((array (make-array '5 :element-type 'bit :initial-contents #*10011))
         (result (coerce array '(array (signed-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  bit t #*10011
  (signed-byte 8) t #(1 0 0 1 1))

(deftest coerce-aa-signed8.3a
  (let* ((array (make-array '5 :element-type '(signed-byte 8)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8) t #(10 -20 30 40 50)
  (signed-byte 8) t #(10 -20 30 40 50))

(deftest coerce-aa-signed8.3b
  (let* ((array (make-array '5 :element-type '(signed-byte 16)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16) t #(10 -20 30 40 50)
  (signed-byte 8) t #(10 -20 30 40 50))

(deftest coerce-aa-signed8.3c
  (let* ((array (make-array '5 :element-type '(signed-byte 32)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32) t #(10 -20 30 40 50)
  (signed-byte 8) t #(10 -20 30 40 50))

#+64-bit
(deftest coerce-aa-signed8.3d
  (let* ((array (make-array '5 :element-type '(signed-byte 64)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64) t #(10 -20 30 40 50)
  (signed-byte 8) t #(10 -20 30 40 50))

(deftest coerce-aa-signed8.4a
  (let* ((array (make-array '5 :element-type '(unsigned-byte 8)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8) t #(10 20 30 40 50)
  (signed-byte 8) t #(10 20 30 40 50))

(deftest coerce-aa-signed8.4b
  (let* ((array (make-array '5 :element-type '(unsigned-byte 16)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16) t #(10 20 30 40 50)
  (signed-byte 8) t #(10 20 30 40 50))

(deftest coerce-aa-signed8.4c
  (let* ((array (make-array '5 :element-type '(unsigned-byte 32)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #(10 20 30 40 50)
  (signed-byte 8) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-signed8.4d
  (let* ((array (make-array '5 :element-type '(unsigned-byte 64)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64) t #(10 20 30 40 50)
  (signed-byte 8) t #(10 20 30 40 50))

(deftest coerce-aa-signed8.5
  (let* ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                            :initial-contents '((10 20 30) (30 40 50))))
         (result (coerce array '(array (signed-byte 8) (2 3)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #2a((10 20 30) (30  40 50))
  (signed-byte 8) t #2a((10 20 30) (30 40 50)))

(deftest-error coerce-aa-signed8.6
  (let ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                           :initial-contents '((10 20 30) (30 40 50)))))
    (coerce array '(array (signed-byte 8) (2 4))))
  type-error)


;;  (coerce array '(array (signed-byte 16)))
(deftest coerce-aa-signed16.1
  (let* ((array (make-array '5 :element-type t
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 16)))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  t t #(10 -20 30 40 50)
  (signed-byte 16) t #(10 -20 30 40 50))

(deftest coerce-aa-signed16.2
  (let* ((array (make-array '5 :element-type 'bit :initial-contents #*10011))
         (result (coerce array '(array (signed-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  bit t #*10011
  (signed-byte 16) t #(1 0 0 1 1))

(deftest coerce-aa-signed16.3a
  (let* ((array (make-array '5 :element-type '(signed-byte 8)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8) t #(10 -20 30 40 50)
  (signed-byte 16) t #(10 -20 30 40 50))

(deftest coerce-aa-signed16.3b
  (let* ((array (make-array '5 :element-type '(signed-byte 16)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16) t #(10 -20 30 40 50)
  (signed-byte 16) t #(10 -20 30 40 50))

(deftest coerce-aa-signed16.3c
  (let* ((array (make-array '5 :element-type '(signed-byte 32)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32) t #(10 -20 30 40 50)
  (signed-byte 16) t #(10 -20 30 40 50))

#+64-bit
(deftest coerce-aa-signed16.3d
  (let* ((array (make-array '5 :element-type '(signed-byte 64)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64) t #(10 -20 30 40 50)
  (signed-byte 16) t #(10 -20 30 40 50))

(deftest coerce-aa-signed16.4a
  (let* ((array (make-array '5 :element-type '(unsigned-byte 8)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8) t #(10 20 30 40 50)
  (signed-byte 16) t #(10 20 30 40 50))

(deftest coerce-aa-signed16.4b
  (let* ((array (make-array '5 :element-type '(unsigned-byte 16)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16) t #(10 20 30 40 50)
  (signed-byte 16) t #(10 20 30 40 50))

(deftest coerce-aa-signed16.4c
  (let* ((array (make-array '5 :element-type '(unsigned-byte 32)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #(10 20 30 40 50)
  (signed-byte 16) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-signed16.4d
  (let* ((array (make-array '5 :element-type '(unsigned-byte 64)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64) t #(10 20 30 40 50)
  (signed-byte 16) t #(10 20 30 40 50))

(deftest coerce-aa-signed16.5
  (let* ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                            :initial-contents '((10 20 30) (30 40 50))))
         (result (coerce array '(array (signed-byte 16) (2 3)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #2a((10 20 30) (30  40 50))
  (signed-byte 16) t #2a((10 20 30) (30 40 50)))

(deftest-error coerce-aa-signed16.6
  (let ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                           :initial-contents '((10 20 30) (30 40 50)))))
    (coerce array '(array (signed-byte 16) (2 4))))
  type-error)


;;  (coerce array '(array (signed-byte 32)))
(deftest coerce-aa-signed32.1
  (let* ((array (make-array '5 :element-type t
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 32)))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  t t #(10 -20 30 40 50)
  (signed-byte 32) t #(10 -20 30 40 50))

(deftest coerce-aa-signed32.2
  (let* ((array (make-array '5 :element-type 'bit :initial-contents #*10011))
         (result (coerce array '(array (signed-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  bit t #*10011
  (signed-byte 32) t #(1 0 0 1 1))

(deftest coerce-aa-signed32.3a
  (let* ((array (make-array '5 :element-type '(signed-byte 8)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8) t #(10 -20 30 40 50)
  (signed-byte 32) t #(10 -20 30 40 50))

(deftest coerce-aa-signed32.3b
  (let* ((array (make-array '5 :element-type '(signed-byte 16)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16) t #(10 -20 30 40 50)
  (signed-byte 32) t #(10 -20 30 40 50))

(deftest coerce-aa-signed32.3c
  (let* ((array (make-array '5 :element-type '(signed-byte 32)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32) t #(10 -20 30 40 50)
  (signed-byte 32) t #(10 -20 30 40 50))

#+64-bit
(deftest coerce-aa-signed32.3d
  (let* ((array (make-array '5 :element-type '(signed-byte 64)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64) t #(10 -20 30 40 50)
  (signed-byte 32) t #(10 -20 30 40 50))

(deftest coerce-aa-signed32.4a
  (let* ((array (make-array '5 :element-type '(unsigned-byte 8)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8) t #(10 20 30 40 50)
  (signed-byte 32) t #(10 20 30 40 50))

(deftest coerce-aa-signed32.4b
  (let* ((array (make-array '5 :element-type '(unsigned-byte 16)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16) t #(10 20 30 40 50)
  (signed-byte 32) t #(10 20 30 40 50))

(deftest coerce-aa-signed32.4c
  (let* ((array (make-array '5 :element-type '(unsigned-byte 32)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #(10 20 30 40 50)
  (signed-byte 32) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-signed32.4d
  (let* ((array (make-array '5 :element-type '(unsigned-byte 64)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64) t #(10 20 30 40 50)
  (signed-byte 32) t #(10 20 30 40 50))

(deftest coerce-aa-signed32.5
  (let* ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                            :initial-contents '((10 20 30) (30 40 50))))
         (result (coerce array '(array (signed-byte 32) (2 3)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #2a((10 20 30) (30  40 50))
  (signed-byte 32) t #2a((10 20 30) (30 40 50)))

(deftest-error coerce-aa-signed32.6
  (let ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                           :initial-contents '((10 20 30) (30 40 50)))))
    (coerce array '(array (signed-byte 32) (2 4))))
  type-error)


;;  (coerce array '(array (signed-byte 64)))
#+64-bit
(deftest coerce-aa-signed64.1
  (let* ((array (make-array '5 :element-type t
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 64)))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  t t #(10 -20 30 40 50)
  (signed-byte 64) t #(10 -20 30 40 50))

#+64-bit
(deftest coerce-aa-signed64.2
  (let* ((array (make-array '5 :element-type 'bit :initial-contents #*10011))
         (result (coerce array '(array (signed-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  bit t #*10011
  (signed-byte 64) t #(1 0 0 1 1))

#+64-bit
(deftest coerce-aa-signed64.3a
  (let* ((array (make-array '5 :element-type '(signed-byte 8)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8) t #(10 -20 30 40 50)
  (signed-byte 64) t #(10 -20 30 40 50))

#+64-bit
(deftest coerce-aa-signed64.3b
  (let* ((array (make-array '5 :element-type '(signed-byte 16)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16) t #(10 -20 30 40 50)
  (signed-byte 64) t #(10 -20 30 40 50))

#+64-bit
(deftest coerce-aa-signed64.3c
  (let* ((array (make-array '5 :element-type '(signed-byte 32)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32) t #(10 -20 30 40 50)
  (signed-byte 64) t #(10 -20 30 40 50))

#+64-bit
(deftest coerce-aa-signed64.3d
  (let* ((array (make-array '5 :element-type '(signed-byte 64)
                            :initial-contents '(10 -20 30 40 50)))
         (result (coerce array '(array (signed-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64) t #(10 -20 30 40 50)
  (signed-byte 64) t #(10 -20 30 40 50))

#+64-bit
(deftest coerce-aa-signed64.4a
  (let* ((array (make-array '5 :element-type '(unsigned-byte 8)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8) t #(10 20 30 40 50)
  (signed-byte 64) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-signed64.4b
  (let* ((array (make-array '5 :element-type '(unsigned-byte 16)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16) t #(10 20 30 40 50)
  (signed-byte 64) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-signed64.4c
  (let* ((array (make-array '5 :element-type '(unsigned-byte 32)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #(10 20 30 40 50)
  (signed-byte 64) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-signed64.4d
  (let* ((array (make-array '5 :element-type '(unsigned-byte 64)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (signed-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64) t #(10 20 30 40 50)
  (signed-byte 64) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-signed64.5
  (let* ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                            :initial-contents '((10 20 30) (30 40 50))))
         (result (coerce array '(array (signed-byte 64) (2 3)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #2a((10 20 30) (30  40 50))
  (signed-byte 64) t #2a((10 20 30) (30 40 50)))

#+64-bit
(deftest-error coerce-aa-signed64.6
  (let ((array (make-array '(2 3) :element-type '(unsigned-byte 64)
                           :initial-contents '((10 20 30) (30 40 50)))))
    (coerce array '(array (signed-byte 64) (2 4))))
  type-error)


;;  (coerce array '(array (unsigned-byte 8)))
(deftest coerce-aa-unsigned8.1
  (let* ((array (make-array '5 :element-type t
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 8)))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  t t #(10 20 30 40 50)
  (unsigned-byte 8) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned8.2
  (let* ((array (make-array '5 :element-type 'bit :initial-contents #*10011))
         (result (coerce array '(array (unsigned-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  bit t #*10011
  (unsigned-byte 8) t #(1 0 0 1 1))

(deftest coerce-aa-unsigned8.3a
  (let* ((array (make-array '5 :element-type '(signed-byte 8)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8) t #(10 20 30 40 50)
  (unsigned-byte 8) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned8.3b
  (let* ((array (make-array '5 :element-type '(signed-byte 16)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16) t #(10 20 30 40 50)
  (unsigned-byte 8) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned8.3c
  (let* ((array (make-array '5 :element-type '(signed-byte 32)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32) t #(10 20 30 40 50)
  (unsigned-byte 8) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned8.3d
  (let* ((array (make-array '5 :element-type '(signed-byte 64)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64) t #(10 20 30 40 50)
  (unsigned-byte 8) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned8.4a
  (let* ((array (make-array '5 :element-type '(unsigned-byte 8)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8) t #(10 20 30 40 50)
  (unsigned-byte 8) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned8.4b
  (let* ((array (make-array '5 :element-type '(unsigned-byte 16)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16) t #(10 20 30 40 50)
  (unsigned-byte 8) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned8.4c
  (let* ((array (make-array '5 :element-type '(unsigned-byte 32)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #(10 20 30 40 50)
  (unsigned-byte 8) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned8.4d
  (let* ((array (make-array '5 :element-type '(unsigned-byte 64)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 8)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64) t #(10 20 30 40 50)
  (unsigned-byte 8) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned8.5
  (let* ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                            :initial-contents '((10 20 30) (30 40 50))))
         (result (coerce array '(array (unsigned-byte 8) (2 3)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #2a((10 20 30) (30  40 50))
  (unsigned-byte 8) t #2a((10 20 30) (30 40 50)))

(deftest-error coerce-aa-unsigned8.6
  (let ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                           :initial-contents '((10 20 30) (30 40 50)))))
    (coerce array '(array (unsigned-byte 8) (2 4))))
  type-error)


;;  (coerce array '(array (unsigned-byte 16)))
(deftest coerce-aa-unsigned16.1
  (let* ((array (make-array '5 :element-type t
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 16)))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  t t #(10 20 30 40 50)
  (unsigned-byte 16) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned16.2
  (let* ((array (make-array '5 :element-type 'bit :initial-contents #*10011))
         (result (coerce array '(array (unsigned-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  bit t #*10011
  (unsigned-byte 16) t #(1 0 0 1 1))

(deftest coerce-aa-unsigned16.3a
  (let* ((array (make-array '5 :element-type '(signed-byte 8)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8) t #(10 20 30 40 50)
  (unsigned-byte 16) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned16.3b
  (let* ((array (make-array '5 :element-type '(signed-byte 16)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16) t #(10 20 30 40 50)
  (unsigned-byte 16) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned16.3c
  (let* ((array (make-array '5 :element-type '(signed-byte 32)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32) t #(10 20 30 40 50)
  (unsigned-byte 16) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned16.3d
  (let* ((array (make-array '5 :element-type '(signed-byte 64)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64) t #(10 20 30 40 50)
  (unsigned-byte 16) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned16.4a
  (let* ((array (make-array '5 :element-type '(unsigned-byte 8)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8) t #(10 20 30 40 50)
  (unsigned-byte 16) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned16.4b
  (let* ((array (make-array '5 :element-type '(unsigned-byte 16)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16) t #(10 20 30 40 50)
  (unsigned-byte 16) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned16.4c
  (let* ((array (make-array '5 :element-type '(unsigned-byte 32)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #(10 20 30 40 50)
  (unsigned-byte 16) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned16.4d
  (let* ((array (make-array '5 :element-type '(unsigned-byte 64)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 16)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64) t #(10 20 30 40 50)
  (unsigned-byte 16) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned16.5
  (let* ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                            :initial-contents '((10 20 30) (30 40 50))))
         (result (coerce array '(array (unsigned-byte 16) (2 3)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #2a((10 20 30) (30  40 50))
  (unsigned-byte 16) t #2a((10 20 30) (30 40 50)))

(deftest-error coerce-aa-unsigned16.6
  (let ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                           :initial-contents '((10 20 30) (30 40 50)))))
    (coerce array '(array (unsigned-byte 16) (2 4))))
  type-error)


;;  (coerce array '(array (unsigned-byte 32)))
(deftest coerce-aa-unsigned32.1
  (let* ((array (make-array '5 :element-type t
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 32)))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  t t #(10 20 30 40 50)
  (unsigned-byte 32) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned32.2
  (let* ((array (make-array '5 :element-type 'bit :initial-contents #*10011))
         (result (coerce array '(array (unsigned-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  bit t #*10011
  (unsigned-byte 32) t #(1 0 0 1 1))

(deftest coerce-aa-unsigned32.3a
  (let* ((array (make-array '5 :element-type '(signed-byte 8)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8) t #(10 20 30 40 50)
  (unsigned-byte 32) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned32.3b
  (let* ((array (make-array '5 :element-type '(signed-byte 16)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16) t #(10 20 30 40 50)
  (unsigned-byte 32) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned32.3c
  (let* ((array (make-array '5 :element-type '(signed-byte 32)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32) t #(10 20 30 40 50)
  (unsigned-byte 32) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned32.3d
  (let* ((array (make-array '5 :element-type '(signed-byte 64)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64) t #(10 20 30 40 50)
  (unsigned-byte 32) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned32.4a
  (let* ((array (make-array '5 :element-type '(unsigned-byte 8)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8) t #(10 20 30 40 50)
  (unsigned-byte 32) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned32.4b
  (let* ((array (make-array '5 :element-type '(unsigned-byte 16)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16) t #(10 20 30 40 50)
  (unsigned-byte 32) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned32.4c
  (let* ((array (make-array '5 :element-type '(unsigned-byte 32)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #(10 20 30 40 50)
  (unsigned-byte 32) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned32.4d
  (let* ((array (make-array '5 :element-type '(unsigned-byte 64)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 32)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64) t #(10 20 30 40 50)
  (unsigned-byte 32) t #(10 20 30 40 50))

(deftest coerce-aa-unsigned32.5
  (let* ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                            :initial-contents '((10 20 30) (30 40 50))))
         (result (coerce array '(array (unsigned-byte 32) (2 3)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #2a((10 20 30) (30  40 50))
  (unsigned-byte 32) t #2a((10 20 30) (30 40 50)))

(deftest-error coerce-aa-unsigned32.6
  (let ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                           :initial-contents '((10 20 30) (30 40 50)))))
    (coerce array '(array (unsigned-byte 32) (2 4))))
  type-error)


;;  (coerce array '(array (unsigned-byte 64)))
#+64-bit
(deftest coerce-aa-unsigned64.1
  (let* ((array (make-array '5 :element-type t
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 64)))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  t t #(10 20 30 40 50)
  (unsigned-byte 64) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned64.2
  (let* ((array (make-array '5 :element-type 'bit :initial-contents #*10011))
         (result (coerce array '(array (unsigned-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  bit t #*10011
  (unsigned-byte 64) t #(1 0 0 1 1))

#+64-bit
(deftest coerce-aa-unsigned64.3a
  (let* ((array (make-array '5 :element-type '(signed-byte 8)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8) t #(10 20 30 40 50)
  (unsigned-byte 64) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned64.3b
  (let* ((array (make-array '5 :element-type '(signed-byte 16)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16) t #(10 20 30 40 50)
  (unsigned-byte 64) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned64.3c
  (let* ((array (make-array '5 :element-type '(signed-byte 32)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32) t #(10 20 30 40 50)
  (unsigned-byte 64) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned64.3d
  (let* ((array (make-array '5 :element-type '(signed-byte 64)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64) t #(10 20 30 40 50)
  (unsigned-byte 64) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned64.4a
  (let* ((array (make-array '5 :element-type '(unsigned-byte 8)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8) t #(10 20 30 40 50)
  (unsigned-byte 64) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned64.4b
  (let* ((array (make-array '5 :element-type '(unsigned-byte 16)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16) t #(10 20 30 40 50)
  (unsigned-byte 64) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned64.4c
  (let* ((array (make-array '5 :element-type '(unsigned-byte 32)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #(10 20 30 40 50)
  (unsigned-byte 64) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned64.4d
  (let* ((array (make-array '5 :element-type '(unsigned-byte 64)
                            :initial-contents '(10 20 30 40 50)))
         (result (coerce array '(array (unsigned-byte 64)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64) t #(10 20 30 40 50)
  (unsigned-byte 64) t #(10 20 30 40 50))

#+64-bit
(deftest coerce-aa-unsigned64.5
  (let* ((array (make-array '(2 3) :element-type '(unsigned-byte 32)
                            :initial-contents '((10 20 30) (30 40 50))))
         (result (coerce array '(array (unsigned-byte 64) (2 3)))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #2a((10 20 30) (30  40 50))
  (unsigned-byte 64) t #2a((10 20 30) (30 40 50)))

#+64-bit
(deftest-error coerce-aa-unsigned64.6
  (let ((array (make-array '(2 3) :element-type '(unsigned-byte 64)
                           :initial-contents '((10 20 30) (30 40 50)))))
    (coerce array '(array (unsigned-byte 64) (2 4))))
  type-error)


;;  (coerce array '(array single-float))
(deftest coerce-aa-single.1
  (let* ((array (make-array '5 :element-type t
                            :initial-contents '(1 1/2 3.4 5.6d0 7.8L0)))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  t t #(1 1/2 3.4 5.6d0 7.8L0)
  single-float t #(1.0f0 0.5f0 3.4f0 5.6f0 7.8f0))

(deftest coerce-aa-single.2
  (let* ((array (make-array '3 :element-type 'bit :initial-contents #*110))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  bit t #*110
  single-float t #(1.0f0 1.0f0 0.0f0))

(deftest coerce-aa-single.3a
  (let* ((array (make-array '3 :element-type '(signed-byte 8)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8) t #(10 20 30)
  single-float t #(10.0f0 20.0f0 30.0f0))

(deftest coerce-aa-single.3b
  (let* ((array (make-array '3 :element-type '(signed-byte 16)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16) t #(10 20 30)
  single-float t #(10.0f0 20.0f0 30.0f0))

(deftest coerce-aa-single.3c
  (let* ((array (make-array '3 :element-type '(signed-byte 32)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32) t #(10 20 30)
  single-float t #(10.0f0 20.0f0 30.0f0))

#+64-bit
(deftest coerce-aa-single.3d
  (let* ((array (make-array '3 :element-type '(signed-byte 64)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64) t #(10 20 30)
  single-float t #(10.0f0 20.0f0 30.0f0))

(deftest coerce-aa-single.4a
  (let* ((array (make-array '3 :element-type '(unsigned-byte 8)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8) t #(10 20 30)
  single-float t #(10.0f0 20.0f0 30.0f0))

(deftest coerce-aa-single.4b
  (let* ((array (make-array '3 :element-type '(unsigned-byte 16)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16) t #(10 20 30)
  single-float t #(10.0f0 20.0f0 30.0f0))

(deftest coerce-aa-single.4c
  (let* ((array (make-array '3 :element-type '(unsigned-byte 32)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #(10 20 30)
  single-float t #(10.0f0 20.0f0 30.0f0))

#+64-bit
(deftest coerce-aa-single.4d
  (let* ((array (make-array '3 :element-type '(unsigned-byte 64)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64) t #(10 20 30)
  single-float t #(10.0f0 20.0f0 30.0f0))

(deftest coerce-aa-single.5
  (let* ((array (make-array '3 :element-type 'single-float
                            :initial-contents '(1.2f0 3.4f0 -4.5f0)))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  single-float t #(1.2f0 3.4f0 -4.5f0)
  single-float t #(1.2f0 3.4f0 -4.5f0))

(deftest coerce-aa-single.6
  (let* ((array (make-array '3 :element-type 'double-float
                            :initial-contents '(1.2d0 3.4d0 -4.5d0)))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  double-float t #(1.2d0 3.4d0 -4.5d0)
  single-float t #(1.2f0 3.4f0 -4.5f0))

(deftest coerce-aa-single.7
  (let* ((array (make-array '3 :element-type 'long-float
                            :initial-contents '(1.2L0 3.4L0 -4.5L0)))
         (result (coerce array '(array single-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  long-float t #(1.2L0 3.4L0 -4.5L0)
  single-float t #(1.2f0 3.4f0 -4.5f0))


;;  (coerce array '(array double-float))
(deftest coerce-aa-double.1
  (let* ((array (make-array '5 :element-type t
                            :initial-contents '(1 1/2 0.25 5.6d0 7.8L0)))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  t t #(1 1/2 0.25 5.6d0 7.8L0)
  double-float t #(1.0d0 0.5d0 0.25d0 5.6d0 7.8d0))

(deftest coerce-aa-double.2
  (let* ((array (make-array '3 :element-type 'bit :initial-contents #*110))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  bit t #*110
  double-float t #(1.0d0 1.0d0 0.0d0))

(deftest coerce-aa-double.3a
  (let* ((array (make-array '3 :element-type '(signed-byte 8)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8) t #(10 20 30)
  double-float t #(10.0d0 20.0d0 30.0d0))

(deftest coerce-aa-double.3b
  (let* ((array (make-array '3 :element-type '(signed-byte 16)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16) t #(10 20 30)
  double-float t #(10.0d0 20.0d0 30.0d0))

(deftest coerce-aa-double.3c
  (let* ((array (make-array '3 :element-type '(signed-byte 32)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32) t #(10 20 30)
  double-float t #(10.0d0 20.0d0 30.0d0))

#+64-bit
(deftest coerce-aa-double.3d
  (let* ((array (make-array '3 :element-type '(signed-byte 64)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64) t #(10 20 30)
  double-float t #(10.0d0 20.0d0 30.0d0))

(deftest coerce-aa-double.4a
  (let* ((array (make-array '3 :element-type '(unsigned-byte 8)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8) t #(10 20 30)
  double-float t #(10.0d0 20.0d0 30.0d0))

(deftest coerce-aa-double.4b
  (let* ((array (make-array '3 :element-type '(unsigned-byte 16)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16) t #(10 20 30)
  double-float t #(10.0d0 20.0d0 30.0d0))

(deftest coerce-aa-double.4c
  (let* ((array (make-array '3 :element-type '(unsigned-byte 32)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #(10 20 30)
  double-float t #(10.0d0 20.0d0 30.0d0))

#+64-bit
(deftest coerce-aa-double.4d
  (let* ((array (make-array '3 :element-type '(unsigned-byte 64)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64) t #(10 20 30)
  double-float t #(10.0d0 20.0d0 30.0d0))

(deftest coerce-aa-double.5
  (let* ((array (make-array '3 :element-type 'single-float
                            :initial-contents '(1.0f0 2.0f0 30.0f0)))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  single-float t #(1.0f0 2.0f0 30.0f0)
  double-float t #(1.0d0 2.0d0 30.0d0))

(deftest coerce-aa-double.6
  (let* ((array (make-array '3 :element-type 'double-float
                            :initial-contents '(1.0d0 2.0d0 30.0d0)))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  double-float t #(1.0d0 2.0d0 30.0d0)
  double-float t #(1.0d0 2.0d0 30.0d0))

(deftest coerce-aa-double.7
  (let* ((array (make-array '3 :element-type 'long-float
                            :initial-contents '(1.0L0 2.0L0 30.0L0)))
         (result (coerce array '(array double-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  long-float t #(1.0L0 2.0L0 30.0L0)
  double-float t #(1.0d0 2.0d0 30.0d0))


;;  (coerce array '(array long-float))
(deftest coerce-aa-long.1
  (let* ((array (make-array '5 :element-type t
                            :initial-contents '(1 1/2 0.25 5.0d0 7.8L0)))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type array)
      (array-general-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  t t #(1 1/2 0.25 5.0d0 7.8L0)
  long-float t #(1.0L0 0.5L0 0.25L0 5.0L0 7.8L0))

(deftest coerce-aa-long.2
  (let* ((array (make-array '3 :element-type 'bit :initial-contents #*110))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  bit t #*110
  long-float t #(1.0L0 1.0L0 0.0L0))

(deftest coerce-aa-long.3a
  (let* ((array (make-array '3 :element-type '(signed-byte 8)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 8) t #(10 20 30)
  long-float t #(10.0L0 20.0L0 30.0L0))

(deftest coerce-aa-long.3b
  (let* ((array (make-array '3 :element-type '(signed-byte 16)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 16) t #(10 20 30)
  long-float t #(10.0L0 20.0L0 30.0L0))

(deftest coerce-aa-long.3c
  (let* ((array (make-array '3 :element-type '(signed-byte 32)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 32) t #(10 20 30)
  long-float t #(10.0L0 20.0L0 30.0L0))

#+64-bit
(deftest coerce-aa-long.3d
  (let* ((array (make-array '3 :element-type '(signed-byte 64)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (signed-byte 64) t #(10 20 30)
  long-float t #(10.0L0 20.0L0 30.0L0))

(deftest coerce-aa-long.4a
  (let* ((array (make-array '3 :element-type '(unsigned-byte 8)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 8) t #(10 20 30)
  long-float t #(10.0L0 20.0L0 30.0L0))

(deftest coerce-aa-long.4b
  (let* ((array (make-array '3 :element-type '(unsigned-byte 16)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 16) t #(10 20 30)
  long-float t #(10.0L0 20.0L0 30.0L0))

(deftest coerce-aa-long.4c
  (let* ((array (make-array '3 :element-type '(unsigned-byte 32)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 32) t #(10 20 30)
  long-float t #(10.0L0 20.0L0 30.0L0))

#+64-bit
(deftest coerce-aa-long.4d
  (let* ((array (make-array '3 :element-type '(unsigned-byte 64)
                            :initial-contents '(10 20 30)))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  (unsigned-byte 64) t #(10 20 30)
  long-float t #(10.0L0 20.0L0 30.0L0))

(deftest coerce-aa-long.5
  (let* ((array (make-array '3 :element-type 'single-float
                            :initial-contents '(1.0f0 2.0f0 30.0f0)))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  single-float t #(1.0f0 2.0f0 30.0f0)
  long-float t #(1.0L0 2.0L0 30.0L0))

(deftest coerce-aa-long.6
  (let* ((array (make-array '3 :element-type 'double-float
                            :initial-contents '(1.0d0 2.0d0 30.0d0)))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  double-float t #(1.0d0 2.0d0 30.0d0)
  long-float t #(1.0L0 2.0L0 30.0L0))

(deftest coerce-aa-long.7
  (let* ((array (make-array '3 :element-type 'long-float
                            :initial-contents '(1.0L0 2.0L0 30.0L0)))
         (result (coerce array '(array long-float))))
    (values
      (array-element-type array)
      (array-specialized-p array)
      array
      (array-element-type result)
      (array-specialized-p result)
      result))
  long-float t #(1.0L0 2.0L0 30.0L0)
  long-float t #(1.0L0 2.0L0 30.0L0))

