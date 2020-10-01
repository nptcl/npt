;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function MAP
;;

;;  nil
(deftest map-nil.1
  (let (a)
    (map nil (lambda (x) (push x a)) "abc")
    a)
  (#\c #\b #\a))

(deftest map-nil.2
  (let (a)
    (map nil (lambda (x) (push x a)) "abc"))
  nil)

(deftest map-nil.3
  (let (a)
    (map nil (lambda (x y) (push (list x y) a)) "abc" nil)
    a)
  nil)

(deftest map-nil.4
  (let (a)
    (map nil (lambda (x y) (push (list x y) a)) "abc" '(d e))
    a)
  ((#\b e) (#\a d)))

(deftest map-nil.5
  (let (a)
    (map nil (lambda (x y) (push (list x y) a)) "abc" '(d e f g))
    a)
  ((#\c f) (#\b e) (#\a d)))

(deftest map-nil.6
  (map nil 'equal "abc" '(d e f g))
  nil)


;;  list
(defun map-list-test1 (x)
  (code-char
    (1+ (char-code x))))

(defun map-list-test2 (x y)
  (list (map-list-test1 x)
        (map-list-test1 y)))

(deftest map-list.1
  (map 'list #'map-list-test1 "abc")
  (#\b #\c #\d))

(deftest map-list.2
  (map 'list #'map-list-test2 "abc" nil)
  nil)

(deftest map-list.3
  (map 'list #'map-list-test2 "abc" '(#\d #\e))
  ((#\b #\e) (#\c #\f)))

(deftest map-list.4
  (map 'list #'map-list-test2 "abc" '(#\d #\e #\f #\g))
  ((#\b #\e) (#\c #\f) (#\d #\g)))

(deftest map-list.5
  (map 'list 'map-list-test1 "abc")
  (#\b #\c #\d))


;;  cons
(deftest map-cons.1
  (map 'cons #'map-list-test1 "abc")
  (#\b #\c #\d))

(deftest-error map-cons.2
  (map 'cons #'map-list-test2 "abc" nil))

(deftest map-cons.3
  (map 'cons #'map-list-test2 "abc" '(#\d #\e))
  ((#\b #\e) (#\c #\f)))

(deftest map-cons.4
  (map 'cons #'map-list-test2 "abc" '(#\d #\e #\f #\g))
  ((#\b #\e) (#\c #\f) (#\d #\g)))

(deftest map-cons.5
  (map 'cons 'map-list-test1 "abc")
  (#\b #\c #\d))


;;  vector bit
(defun map-bit-not (x)
  (if (eql x 0) 1 0))

(deftest map-vector-bit.1
  (map '(vector bit) #'map-bit-not #*1101)
  #*0010)

(deftest map-vector-bit.2
  (map '(vector bit) #'logior #*1001 #*0101)
  #*1101)

(deftest map-vector-bit.3
  (map '(vector bit) #'logior #*100 #*0101)
  #*110)

(deftest map-vector-bit.4
  (map '(vector bit) #'logior #*10011 '(0 1 0 1))
  #*1101)

(deftest map-vector-bit.5
  (map '(vector bit) #'logior #*1001 '(0 1 0 1 1 1 0 0 0))
  #*1101)

(deftest map-vector-bit.6
  (map '(vector bit) #'logior nil)
  #*)

(deftest-error map-vector-bit.7
  (map '(vector bit) #'logior '(0 1 1 2 2 1 1)))

(deftest map-vector-bit.8
  (map '(vector bit 3) #'values '(0 1 1))
  #*011)

(deftest-error map-vector-bit.9
  (map '(vector bit 2) #'values '(0 1 1)))


;;  vector character
(defun map-list-char+ (&rest args)
  (code-char
    (apply #'+
           (mapcar
             (lambda (x)
               (if (characterp x)
                 (char-code x)
                 x))
             args))))

(deftest map-vector-character.1
  (map '(vector character) #'map-list-test1 "abc")
  "bcd")

(deftest map-vector-character.2
  (map '(vector character) #'map-list-char+ "abc" '(1 2 3))
  "bdf")

(deftest map-vector-character.3
  (map '(vector character) #'map-list-char+ "abcdef" '(1 2 3))
  "bdf")

(deftest map-vector-character.4
  (map '(vector character) #'map-list-char+ "abc" '(1 2 3 4 5 6))
  "bdf")

(deftest map-vector-character.5
  (map '(vector character) #'values nil)
  "")

(deftest-error map-vector-character.6
  (map '(vector character) #'values '(#\A 10 #\C)))

(deftest map-vector-character.7
  (map '(vector character 3) #'values "ABC")
  "ABC")

(deftest-error map-vector-character.8
  (map '(vector character 4) #'values "ABC"))


;;  signed-byte
(deftest map-vector-signed.1
  (map '(vector (signed-byte 8)) #'1+ '(10 20 30))
  #(11 21 31))

(deftest map-vector-signed.2
  (map '(vector (signed-byte 8)) #'values '(10 -128 127))
  #(10 -128 127))

(deftest-error map-vector-signed.3
  (map '(vector (signed-byte 8)) #'values '(10 0 128)))

(deftest-error map-vector-signed.4
  (map '(vector (signed-byte 8)) #'values '(10 -129 0)))

(deftest-error map-vector-signed.5
  (map '(vector (signed-byte 8)) #'values '(10 20 #\A)))

(deftest map-vector-signed.6
  (map '(vector (signed-byte 8)) #'+ '(10 20 30) '(3 4 5 6))
  #(13 24 35))

(deftest map-vector-signed.7
  (map '(vector (signed-byte 8)) #'+ '(10 20 30 40) '(3 4 5))
  #(13 24 35))

(deftest map-vector-signed.8
  (map '(vector (signed-byte 8)) #'+ nil)
  #())

(deftest map-vector-signed.9
  (map '(vector (signed-byte 16)) #'+ #(10 500 -600) '(1 2 -3) '(1 1 1 1 1 1))
  #(12 503 -602))

(deftest map-vector-signed.10
  (map '(vector (signed-byte 16) 3) #'values '(1 2 3))
  #(1 2 3))

(deftest-error map-vector-signed.11
  (map '(vector (signed-byte 16) 1) #'values '(1 2 3)))


;;  unsigned-byte
(deftest map-vector-unsigned.1
  (map '(vector (unsigned-byte 8)) #'1+ '(10 20 30))
  #(11 21 31))

(deftest map-vector-unsigned.2
  (map '(vector (unsigned-byte 8)) #'values '(10 0 255))
  #(10 0 255))

(deftest-error map-vector-unsigned.3
  (map '(vector (unsigned-byte 8)) #'values '(10 0 256)))

(deftest-error map-vector-unsigned.4
  (map '(vector (unsigned-byte 8)) #'values '(10 -1 0)))

(deftest-error map-vector-unsigned.5
  (map '(vector (unsigned-byte 8)) #'values '(10 20 #\A)))

(deftest map-vector-unsigned.6
  (map '(vector (unsigned-byte 8)) #'+ '(10 20 30) '(3 4 5 6))
  #(13 24 35))

(deftest map-vector-unsigned.7
  (map '(vector (unsigned-byte 8)) #'+ '(10 20 30 40) '(3 4 5))
  #(13 24 35))

(deftest map-vector-unsigned.8
  (map '(vector (unsigned-byte 8)) #'+ nil)
  #())

(deftest map-vector-unsigned.9
  (map '(vector (unsigned-byte 16)) #'+ #(10 500 600) '(1 2 -3) '(1 1 1 1 1 1))
  #(12 503 598))

(deftest map-vector-unsigned.10
  (map '(vector (unsigned-byte 16) 3) #'values '(1 2 3))
  #(1 2 3))

(deftest-error map-vector-unsigned.11
  (map '(vector (unsigned-byte 16) 10) #'values '(1 2 3)))


;;  vector float
(deftest map-vector-float.1
  (map '(vector double-float) #'1+ '(10d0 20d0 30d0))
  #(11d0 21d0 31d0))

(deftest-error map-vector-float.2
  (map '(vector single-float) #'values '(10 20 30)))

(deftest map-vector-float.3
  (map '(vector single-float) #'+ '(10f0 20f0 30f0) '(1 2 3 4 5 6))
  #(11f0 22f0 33f0))

(deftest map-vector-float.4
  (map '(vector long-float) #'+ '(10L0 20L0 30L0 40L0 50Lo) '(1 2 3))
  #(11L0 22L0 33L0))

(deftest map-vector-float.5
  (map '(vector single-float) #'+ nil)
  #())

(deftest-error map-vector-float.6
  (map '(vector single-float) #'values '(1f0 #\A)))

(deftest map-vector-float.7
  (map '(vector single-float 3) #'values '(1f0 2f0 3f4))
  #(1f0 2f0 3f4))

(deftest-error map-vector-float.8
  (map '(vector single-float 2) #'values '(1f0 2f0 3f4)))


;;  vector general
(deftest map-vector-general.1
  (map '(vector pathname) #'1+ '(10 20 30))
  #(11 21 31))

(deftest map-vector-general.2
  (map 'vector #'+ '(10 20 30) '(1 2 3 4 5))
  #(11 22 33))

(deftest map-vector-general.3
  (map 'vector #'+ '(10 20 30 40 50) '(1 2 3))
  #(11 22 33))

(deftest map-vector-general.4
  (map 'vector #'values nil)
  #())

(deftest map-vector-general.5
  (map 'vector #'values '(10 #\A "Hello" :test))
  #(10 #\A "Hello" :test))

(deftest map-vector-general.6
  (map '(vector * 4) #'values '(10 #\A "Hello" :test))
  #(10 #\A "Hello" :test))

(deftest-error map-vector-general.7
  (map '(vector * 10) #'values '(10 #\A "Hello" :test)))


;;  simple-vector
(deftest map-simple-vector.1
  (map 'simple-vector #'1+ '(10 20 30))
  #(11 21 31))

(deftest map-simple-vector.2
  (map 'simple-vector #'+ '(10 20 30) #(1 2 3 4 5))
  #(11 22 33))

(deftest map-simple-vector.3
  (map 'simple-vector #'+ '(10 20 30 40 50) #(1 2 3))
  #(11 22 33))

(deftest map-simple-vector.4
  (map 'simple-vector #'values nil)
  #())

(deftest map-simple-vector.5
  (map 'simple-vector #'values '(10 #\A :hello))
  #(10 #\A :hello))

(deftest map-simple-vector.6
  (map '(simple-vector 3) #'1+ '(10 20 30))
  #(11 21 31))

(deftest-error map-simple-vector.7
  (map '(simple-vector 4) #'1+ '(10 20 30)))


;;  string
(deftest map-string.1
  (map 'string #'map-list-test1 "abc")
  "bcd")

(deftest map-string.2
  (map 'base-string #'map-list-char+ "abc" '(1 2 3 4 5))
  "bdf")

(deftest map-string.3
  (map 'base-string #'map-list-char+ "abcdef" '(1 2 3))
  "bdf")

(deftest map-string.4
  (map 'base-string #'map-list-char+ "abcdef" '(1 2 3) '(1 1 1 1 1))
  "ceg")

(deftest map-string.5
  (map 'base-string #'values nil)
  "")

(deftest map-string.6
  (map '(string 3) #'map-list-test1 "abc")
  "bcd")

(deftest-error map-string.7
  (map '(string 4) #'map-list-test1 "abc"))


;;  array
(deftest map-array.1
  (map 'array #'1+ #(10 20 30))
  #(11 21 31))

(deftest map-array.2
  (map '(array character) #'map-list-test1 '(#\a #\b #\c))
  "bcd")

(deftest map-array.3
  (map '(array *) #'+ #(1 2 3) '(4 5 6 7 8 9))
  #1a(5 7 9))

(deftest map-array.4
  (map '(array *) #'+ #(1 2 3 4 5 6) '(4 5 6))
  #1a(5 7 9))

(deftest map-array.5
  (map '(array * (3)) #'values '(1 2 3))
  #1a(1 2 3))

(deftest-error map-array.6
  (map '(array * (3 4)) #'values '(1 2 3)))


;;  bit-vector
(deftest map-bit-vector.1
  (map 'bit-vector #'map-bit-not #*1101)
  #*0010)

(deftest map-bit-vector.2
  (map 'bit-vector #'logior #*1001 #*0101)
  #*1101)

(deftest map-bit-vector.3
  (map 'bit-vector #'logior #*100 #*0101)
  #*110)

(deftest map-bit-vector.4
  (map 'bit-vector #'logior #*10011 '(0 1 0 1))
  #*1101)

(deftest map-bit-vector.5
  (map 'bit-vector #'logior #*1001 '(0 1 0 1 1 1 0 0 0))
  #*1101)

(deftest map-bit-vector.6
  (map 'bit-vector #'logior nil)
  #*)

(deftest-error map-bit-vector.7
  (map 'bit-vector #'logior '(0 1 1 2 2 1 1)))

(deftest map-bit-vector.8
  (map '(bit-vector 3) #'values '(0 1 1))
  #*011)

(deftest-error map-bit-vector.9
  (map '(bit-vector 2) #'values '(0 1 1)))

(deftest map-bit-vector.10
  (map 'bit-vector #'map-bit-not '(1 0 0 1))
  #*0110)

(deftest map-bit-vector.11
  (map '(bit-vector 4) #'map-bit-not '(1 0 0 1))
  #*0110)

(deftest-error map-bit-vector.12
  (map '(bit-vector 5) #'map-bit-not '(1 0 0 1)))

;;  error
(deftest-error map-error.1
  (eval '(map 10 #'values nil))
  type-error)

(deftest-error map-error.2
  (eval '(map nil 20 nil))
  type-error)

(deftest-error map-error.3
  (eval '(map nil #'values 30)))

(deftest-error! map-error.4
  (eval '(map nil)))

(deftest-error! map-error.5
  (eval '(map nil #'values)))


;;  ANSI Common Lisp
(deftest map-test.1
  (map 'string
       #'(lambda (x y)
           (char "01234567890ABCDEF" (mod (+ x y) 16)))
       '(1 2 3 4)
       '(10 9 8 7))
  "AAAA")

(defparameter *map-list* '("lower" "UPPER" "" "123"))

(deftest map-test.2
  (map nil #'nstring-upcase *map-list*)
  nil)

(deftest map-test.3
  *map-list*
  ("LOWER" "UPPER" "" "123"))

(deftest map-test.4
  (map 'list #'- '(1 2 3 4))
  (-1 -2 -3 -4))

(deftest map-test.5
  (map 'string
       #'(lambda (x) (if (oddp x) #\1 #\0))
       '(1 2 3 4))
  "1010")

(deftest-error map-test.6
  (map '(vector * 4) #'cons "abc" "de"))


;;
;;  Function MAP-INTO
;;
(deftest map-into.1
  (let ((a '(10 20 30)))
    (map-into a #'+ '(1 2) '(3 4 5 6 7)))
  (4 6 30))

(deftest map-into.2
  (let ((a '(10 20 30)))
    (map-into a #'+ #(1 2 3 4) '(3 4 5 6 7))
    a)
  (4 6 8))

(deftest map-into.3
  (let ((a #(10 20 30)))
    (map-into a #'+ '(1 2) '(3 4 5 6 7)))
  #(4 6 30))

(deftest map-into.4
  (let ((a #(10 20 30)))
    (map-into a #'+ #(1 2 3 4) '(3 4 5 6 7))
    a)
  #(4 6 8))

(deftest map-into.5
  (let ((a (make-array
             10 :element-type 'character
             :fill-pointer nil :initial-contents "Helloaaaaa")))
    (map-into a #'values "abcd"))
  "abcdoaaaaa")

(deftest map-into.6
  (let ((a (make-array
             10 :element-type 'character
             :fill-pointer t :initial-contents "Helloaaaaa")))
    (map-into a #'values "abcd"))
  "abcd")

(deftest map-into.7
  (let (list)
    (values
      (map-into nil (lambda (x) (push x list)) '(1 2 3))
      list))
  nil nil)

(deftest map-into.8
  (let ((x '(10 20 30)))
    (map-into x #'values))
  (nil nil nil))

(deftest map-into.9
  (let ((x #(10 20 30)))
    (map-into x #'+)
    x)
  #(0 0 0))

(deftest-error map-into-error.1
  (eval '(map-into 10 #'values '(1 2 3)))
  type-error)

(deftest-error map-into-error.2
  (eval '(map-into nil 20 '(1 2 3)))
  type-error)

(deftest-error! map-into-error.3
  (eval '(map-into nil)))

(deftest-error! map-into-error.4
  (eval '(map-into nil #'values 10)))

;; ANSI Common Lisp
(defparameter *map-into-list1* (list 1 2 3 4))
(defparameter *map-into-list2* (list 10 10 10 10))

(deftest map-into-test.1
  (map-into *map-into-list1* #'+ *map-into-list1* *map-into-list2*)
  (11 12 13 14))

(deftest map-into-test.2
  *map-into-list1*
  (11 12 13 14))

(deftest map-into-test.3
  *map-into-list2*
  (10 10 10 10))

(defparameter *map-into-list3* '(one two three))

(deftest map-into-test.4
  (map-into *map-into-list1* #'cons *map-into-list3* *map-into-list1*)
  ((one . 11) (two . 12) (three . 13) 14))

(deftest map-into-test.5
  (every
    #'symbolp
    (map-into *map-into-list1* #'gensym))
  t)

(deftest map-into-test.6
  (every #'symbolp *map-into-list1*)
  t)

