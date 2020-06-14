;;
;;  compile-array
;;

;;
;;  no-dimension
;;
(deftest compile-array-nil.1
  (let ((x (exec-compile
             (make-array nil :initial-contents 10))))
    (values x (array-element-type x)))
  #0a10
  t)

(deftest compile-array-nil.2
  (let ((x (exec-compile
             (make-array nil :element-type t :initial-contents #\A))))
    (values x (array-element-type x)))
  #0a#\A
  t)

(deftest compile-array-nil.3
  (let ((x (exec-compile
             (make-array nil :element-type 'character :initial-contents #\Z))))
    (values x (array-element-type x)))
  #0a#\Z
  character)

(deftest compile-array-nil.4
  (let ((x (exec-compile
             (make-array nil :element-type 'bit :initial-contents 1))))
    (values x (array-element-type x)))
  #0a1
  bit)

(deftest compile-array-nil.5
  (let ((x (exec-compile
             (make-array nil :element-type 'single-float :initial-contents 12.3f0))))
    (values x (array-element-type x)))
  #0a12.3f0
  single-float)


;;
;;  element-type t
;;
(deftest compile-array-t.1
  (exec-compile
    (make-array 3 :element-type t :initial-element :hello))
  #1a(:hello :hello :hello))

(deftest compile-array-t.2
  (exec-compile
    (make-array 0 :element-type t :initial-element :hello))
  #1a())

(deftest compile-array-t.3
  (let ((x (exec-compile
             (make-array 3 :element-type t :initial-contents '(10 (20 30) #\a)))))
    (values
      x
      (adjustable-array-p x)
      (array-has-fill-pointer-p x)
      (array-displacement x)))
  #1a(10 (20 30) #\a)
  nil nil nil)

(deftest compile-array-t.4
  (let ((x (exec-compile
             (make-array 5 :element-type t
                         :adjustable t
                         :fill-pointer 3
                         :initial-contents '(a b c d e)))))
    (values
      x
      (adjustable-array-p x)
      (array-has-fill-pointer-p x)
      (array-displacement x)))
  #1a(a b c)
  t t nil)

(deftest compile-array-t.5
  (let ((x (exec-compile
             (make-array 3 :element-type t
                         :displaced-to #1a(a b c d e f g)
                         :displaced-index-offset 2))))
    (values
      x
      (array-element-type x)
      (adjustable-array-p x)
      (array-has-fill-pointer-p x)
      (array-displacement x)))
  #1a(c d e) t
  nil nil nil)


;;
;;  element-type bit
;;
(deftest compile-array-bit.1
  (exec-compile
    (make-array 3 :element-type 'bit :initial-element 1))
  #*111)

(deftest compile-array-bit.2
  (exec-compile
    (make-array 0 :element-type 'bit :initial-element 1))
  #1a())

(deftest compile-array-bit.3
  (let ((x (exec-compile
             (make-array 3 :element-type 'bit :initial-contents '(1 0 1)))))
    (values
      x
      (adjustable-array-p x)
      (array-has-fill-pointer-p x)
      (array-displacement x)))
  #*101
  nil nil nil)

(deftest compile-array-bit.4
  (let ((x (exec-compile
             (make-array 5 :element-type 'bit
                         :adjustable t
                         :fill-pointer 3
                         :initial-contents '(1 1 0 1 1)))))
    (values
      x
      (adjustable-array-p x)
      (array-has-fill-pointer-p x)
      (array-displacement x)))
  #*110
  t t nil)

(deftest compile-array-bit.5
  (let ((x (exec-compile
             (make-array 3 :element-type 'bit
                         :displaced-to #*11001
                         :displaced-index-offset 2))))
    (values
      x
      (array-element-type x)
      (adjustable-array-p x)
      (array-has-fill-pointer-p x)
      (array-displacement x)))
  #*001 bit
  nil nil nil)


;;
;;  element-type character
;;
(deftest compile-array-character.1
  (exec-compile
    (make-array 3 :element-type 'character :initial-element #\a))
  "aaa")

(deftest compile-array-character.2
  (exec-compile
    (make-array 0 :element-type 'character :initial-element #\a))
  "")

(deftest compile-array-character.3
  (let ((x (exec-compile
             (make-array 3 :element-type 'character :initial-contents "abc"))))
    (values
      x
      (adjustable-array-p x)
      (array-has-fill-pointer-p x)
      (array-displacement x)))
  "abc"
  nil nil nil)

(deftest compile-array-character.4
  (let ((x (exec-compile
             (make-array 5 :element-type 'character
                         :adjustable t
                         :fill-pointer 3
                         :initial-contents "abcde"))))
    (values
      x
      (adjustable-array-p x)
      (array-has-fill-pointer-p x)
      (array-displacement x)))
  "abc"
  t t nil)

(deftest compile-array-character.5
  (let ((x (exec-compile
             (make-array 3 :element-type 'character
                         :displaced-to "ABCDE"
                         :displaced-index-offset 2))))
    (values
      x
      (array-element-type x)
      (adjustable-array-p x)
      (array-has-fill-pointer-p x)
      (array-displacement x)))
  "CDE" character
  nil nil nil)


;;
;;  element-type signed-byte
;;
(deftest compile-array-signed-byte.1
  (exec-compile
    (make-array 3 :element-type '(signed-byte 8) :initial-element 22))
  #1a(22 22 22))

(deftest compile-array-signed-byte.2
  (exec-compile
    (make-array 0 :element-type '(signed-byte 16) :initial-element 33))
  #1a())

(deftest compile-array-signed-byte.3
  (let ((x (exec-compile
             (make-array 3 :element-type '(signed-byte 32)
                         :initial-contents '(10 20 30)))))
    (values
      x
      (adjustable-array-p x)
      (array-has-fill-pointer-p x)
      (array-displacement x)))
  #1a(10 20 30)
  nil nil nil)

(deftest compile-array-signed-byte.4
  (let ((x (exec-compile
             (make-array 5 :element-type '(signed-byte 8)
                         :adjustable t
                         :fill-pointer 3
                         :initial-contents '(11 22 33 44 55)))))
    (values
      x
      (adjustable-array-p x)
      (array-has-fill-pointer-p x)
      (array-displacement x)))
  #1a(11 22 33)
  t t nil)

(deftest compile-array-signed-byte.5
  (let ((x (exec-compile
             (make-array 3 :element-type '(signed-byte 16)
                         :displaced-to
                         (make-array 5 :element-type '(signed-byte 16)
                                     :initial-contents '(2 3 4 5 6))
                         :displaced-index-offset 2))))
    (values
      x
      (array-element-type x)
      (adjustable-array-p x)
      (array-has-fill-pointer-p x)
      (array-displacement x)))
  #1a(4 5 6) (signed-byte 16)
  nil nil nil)


;;
;;  two-dimensions
;;
(deftest compile-array-dimensions.1
  (exec-compile
    (make-array '(2 3) :element-type t :initial-contents '((1 2 3) (4 5 6))))
  #2a((1 2 3) (4 5 6)))

(deftest compile-array-dimensions.2
  (exec-compile
    (make-array '(2 3) :element-type 'double-float
                :initial-contents '((1.2d0 3.4d0 5.6d0) (7.8d0 9.1d0 2.3d0))))
  #2a((1.2d0 3.4d0 5.6d0) (7.8d0 9.1d0 2.3d0)))

