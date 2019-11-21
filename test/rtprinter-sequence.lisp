;;
;;  ANSI COMMON LISP: 22. Printer
;;

;;
;;  cons
;;
(deftest write-cons.1
  (with-default-print
    (let ((v (cons 10 nil)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(10)" "(10)")

(deftest write-cons.2
  (with-default-print
    (let ((v (cons 10 20)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(10 . 20)" "(10 . 20)")

(deftest write-cons.3
  (with-default-print
    (let ((v (list 10 20 #\A))
          (*print-radix* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(10. 20. #\\A)" "(10. 20. A)")

(deftest write-cons.4
  (with-default-print
    (let ((v (list* 10 20 #\A #\b))
          (*print-radix* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(10. 20. #\\A . #\\b)" "(10. 20. A . b)")

(deftest write-cons.5
  (with-default-print
    (let ((v '((((((((((10 20 30))))))) 40 50)))))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "((((((((((10 20 30))))))) 40 50)))"
  "((((((((((10 20 30))))))) 40 50)))")

(deftest write-cons.6
  (with-default-print
    (let ((v '(a b c d e f g h))
          (*print-length* 5))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(A B C D E ...)"
  "(A B C D E ...)")

(deftest write-cons.7
  (with-default-print
    (let ((v '(a b ((((((((c)))))))) d e f g h))
          (*print-level* 5))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(A B ((((#)))) D E F G H)"
  "(A B ((((#)))) D E F G H)")

(deftest write-cons.8
  (with-default-print
    (let ((v '(a b ((((((((c)))))))) d e f g h))
          (*print-length* 3)
          (*print-level* 3))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(A B ((#)) ...)"
  "(A B ((#)) ...)")

(deftest write-cons-circle.1
  (with-default-print
    (let ((v (cons 10 nil))
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(10)" "(10)")

(deftest write-cons-circle.2
  (with-default-print
    (let ((v (cons 10 20))
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(10 . 20)" "(10 . 20)")

(deftest write-cons-circle.3
  (with-default-print
    (let ((v (list 10 20 #\A))
          (*print-radix* t)
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(10. 20. #\\A)" "(10. 20. A)")

(deftest write-cons-circle.4
  (with-default-print
    (let ((v (list* 10 20 #\A #\b))
          (*print-radix* t)
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(10. 20. #\\A . #\\b)" "(10. 20. A . b)")

(deftest write-cons-circle.5
  (with-default-print
    (let ((v '((((((((((10 20 30))))))) 40 50))))
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "((((((((((10 20 30))))))) 40 50)))"
  "((((((((((10 20 30))))))) 40 50)))")

(deftest write-cons-circle.6
  (with-default-print
    (let ((v '(a b c d e f g h))
          (*print-length* 5)
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(A B C D E ...)"
  "(A B C D E ...)")

(deftest write-cons-circle.7
  (with-default-print
    (let ((v '(a b ((((((((c)))))))) d e f g h))
          (*print-level* 5)
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(A B ((((#)))) D E F G H)"
  "(A B ((((#)))) D E F G H)")

(deftest write-cons-circle.8
  (with-default-print
    (let ((v '(a b ((((((((c)))))))) d e f g h))
          (*print-length* 3)
          (*print-level* 3)
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(A B ((#)) ...)"
  "(A B ((#)) ...)")

(deftest write-cons-circle.9
  (with-default-print
    (let ((v '#1=(a b c . #1#))
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#1=(A B C . #1#)"
  "#1=(A B C . #1#)")

(deftest write-cons-circle.10
  (with-default-print
    (let ((v '#1=(a b #2=(c) #1# . #2#))
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#1=(A B #2=(C) #1# . #2#)"
  "#1=(A B #2=(C) #1# . #2#)")


;;
;;  vector
;;
(deftest write-vector.1
  (with-default-print
    (let ((v #()))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#()" "#()")

(deftest write-vector.2
  (with-default-print
    (let ((v #(10)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(10)" "#(10)")

(deftest write-vector.3
  (with-default-print
    (let ((v #(10 nil #\A))
          (*print-radix* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(10. NIL #\\A)" "#(10. NIL A)")

(deftest write-vector.4
  (with-default-print
    (let ((v #(#(#(#(#(#(#(#(#(#(10 20 30))))))) 40 50)))))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(#(#(#(#(#(#(#(#(#(10 20 30))))))) 40 50)))"
  "#(#(#(#(#(#(#(#(#(#(10 20 30))))))) 40 50)))")

(deftest write-vector.5
  (with-default-print
    (let ((v #(a b c d e f g h))
          (*print-length* 5))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(A B C D E ...)"
  "#(A B C D E ...)")

(deftest write-vector.6
  (with-default-print
    (let ((v #(a b #(#(#(#(#(#(#(#(c)))))))) d e f g h))
          (*print-level* 5))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(A B #(#(#(#(#)))) D E F G H)"
  "#(A B #(#(#(#(#)))) D E F G H)")

(deftest write-vector.7
  (with-default-print
    (let ((v #(a b #(#(#(#(#(#(#(#(c)))))))) d e f g h))
          (*print-length* 3)
          (*print-level* 3))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(A B #(#(#)) ...)"
  "#(A B #(#(#)) ...)")

(deftest write-vector.8
  (with-default-print
    (let ((v '(a b #((#((#((#((c)))))))) d e f g h))
          (*print-length* 3)
          (*print-level* 3))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(A B #((#)) ...)"
  "(A B #((#)) ...)")

(deftest write-vector-circle.1
  (with-default-print
    (let ((v #())
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#()" "#()")

(deftest write-vector-circle.2
  (with-default-print
    (let ((v #(10))
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(10)" "#(10)")

(deftest write-vector-circle.3
  (with-default-print
    (let ((v #(10 nil #\A))
          (*print-radix* t)
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(10. NIL #\\A)" "#(10. NIL A)")

(deftest write-vector-circle.4
  (with-default-print
    (let ((v #(#(#(#(#(#(#(#(#(#(10 20 30))))))) 40 50))))
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(#(#(#(#(#(#(#(#(#(10 20 30))))))) 40 50)))"
  "#(#(#(#(#(#(#(#(#(#(10 20 30))))))) 40 50)))")

(deftest write-vector-circle.5
  (with-default-print
    (let ((v #(a b c d e f g h))
          (*print-length* 5)
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(A B C D E ...)"
  "#(A B C D E ...)")

(deftest write-vector-circle.6
  (with-default-print
    (let ((v #(a b #(#(#(#(#(#(#(#(c)))))))) d e f g h))
          (*print-level* 5)
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(A B #(#(#(#(#)))) D E F G H)"
  "#(A B #(#(#(#(#)))) D E F G H)")

(deftest write-vector-circle.7
  (with-default-print
    (let ((v #(a b #(#(#(#(#(#(#(#(c)))))))) d e f g h))
          (*print-length* 3)
          (*print-level* 3)
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(A B #(#(#)) ...)"
  "#(A B #(#(#)) ...)")

(deftest write-vector-circle.8
  (with-default-print
    (let ((v '(a b #((#((#((#((c)))))))) d e f g h))
          (*print-length* 3)
          (*print-level* 3)
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "(A B #((#)) ...)"
  "(A B #((#)) ...)")

(deftest write-vector-circle.9
  (with-default-print
    (let ((v '#1=#(a b c #1#))
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#1=#(A B C #1#)"
  "#1=#(A B C #1#)")

(deftest write-vector-circle.10
  (with-default-print
    (let ((v '#1=#(a b #2=(c) #1# #2# d e f))
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#1=#(A B #2=(C) #1# #2# D E F)"
  "#1=#(A B #2=(C) #1# #2# D E F)")

(deftest write-vector-circle.11
  (with-default-print
    (let ((v '#(a b (c d e #(f g #1=(h i) j) k) l m #1# o))
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(A B (C D E #(F G #1=(H I) J) K) L M #1# O)"
  "#(A B (C D E #(F G #1=(H I) J) K) L M #1# O)")


;;
;;  array
;;

;;  general-array
(deftest write-array.1
  (with-default-print
    (let ((v (make-array nil)))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t "#0ANIL" "#0ANIL")

(deftest write-array.2
  (with-default-print
    (let ((v (make-array 0)))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t "#()" "#()")

(deftest write-array.3
  (with-default-print
    (let ((v (make-array 5 :initial-contents '(a b c d e))))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#(A B C D E)"
  "#(A B C D E)")

(deftest write-array.4
  (with-default-print
    (let ((v (make-array '(2 3) :initial-contents '((a b c) (d e f)))))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#2A((A B C) (D E F))"
  "#2A((A B C) (D E F))")

(deftest write-array.5
  (with-default-print
    (let ((v (make-array '(2 3) :initial-contents '((a 10 #\A) (b 20 #\b))))
          (*print-radix* t))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#2A((A 10. #\\A) (B 20. #\\b))"
  "#2A((A 10. A) (B 20. b))")

(deftest write-array.6
  (with-default-print
    (let ((v (make-array 9 :initial-contents '(a b c d e f g h i)))
          (*print-length* 5))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#(A B C D E ...)"
  "#(A B C D E ...)")

(deftest write-array.7
  (with-default-print
    (let ((v (make-array '(2 5) :initial-contents '((a b c d e) (f g h i j))))
          (*print-length* 3))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#2A((A B C ...) (F G H ...))"
  "#2A((A B C ...) (F G H ...))")

(deftest write-array.8
  (with-default-print
    (let ((v (make-array '(4 5) :initial-contents
                         '((a b c d e) (f g h i j) (k l m n o) (p q r s t))))
          (*print-length* 3))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#2A((A B C ...) (F G H ...) (K L M ...) ...)"
  "#2A((A B C ...) (F G H ...) (K L M ...) ...)")

(deftest write-array.9
  (with-default-print
    (let ((v #1a(#1a(#1a(#1a(#1a(#1a(a b c)))))))
          (*print-level* 3))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#(#(#(#)))"
  "#(#(#(#)))")

(deftest write-array.10
  (with-default-print
    (let ((v #1a(#((#1a(#((a b c)))))))
          (*print-level* 3))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#(#((#)))"
  "#(#((#)))")

(deftest write-array.11
  (with-default-print
    (let ((v #1a(a #1a(#1a(#1a(#1a(#1a(a b c))) e f g h i)) b c d e f))
          (*print-length* 2)
          (*print-level* 3))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#(A #(#(# E ...)) ...)"
  "#(A #(#(# E ...)) ...)")

(deftest write-array-circle.1
  (with-default-print
    (let ((v (make-array nil))
          (*print-circle* t))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t "#0ANIL" "#0ANIL")

(deftest write-array-circle.2
  (with-default-print
    (let ((v (make-array 0))
          (*print-circle* t))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t "#()" "#()")

(deftest write-array-circle.3
  (with-default-print
    (let ((v (make-array 5 :initial-contents '(a b c d e)))
          (*print-circle* t))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#(A B C D E)"
  "#(A B C D E)")

(deftest write-array-circle.4
  (with-default-print
    (let ((v (make-array '(2 3) :initial-contents '((a b c) (d e f))))
          (*print-circle* t))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#2A((A B C) (D E F))"
  "#2A((A B C) (D E F))")

(deftest write-array-circle.5
  (with-default-print
    (let ((v (make-array '(2 3) :initial-contents '((a 10 #\A) (b 20 #\b))))
          (*print-radix* t)
          (*print-circle* t))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#2A((A 10. #\\A) (B 20. #\\b))"
  "#2A((A 10. A) (B 20. b))")

(deftest write-array-circle.6
  (with-default-print
    (let ((v (make-array 9 :initial-contents '(a b c d e f g h i)))
          (*print-length* 5)
          (*print-circle* t))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#(A B C D E ...)"
  "#(A B C D E ...)")

(deftest write-array-circle.7
  (with-default-print
    (let ((v (make-array '(2 5) :initial-contents '((a b c d e) (f g h i j))))
          (*print-length* 3)
          (*print-circle* t))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#2A((A B C ...) (F G H ...))"
  "#2A((A B C ...) (F G H ...))")

(deftest write-array-circle.8
  (with-default-print
    (let ((v (make-array '(4 5) :initial-contents
                         '((a b c d e) (f g h i j) (k l m n o) (p q r s t))))
          (*print-length* 3)
          (*print-circle* t))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#2A((A B C ...) (F G H ...) (K L M ...) ...)"
  "#2A((A B C ...) (F G H ...) (K L M ...) ...)")

(deftest write-array-circle.9
  (with-default-print
    (let ((v #1a(#1a(#1a(#1a(#1a(#1a(a b c)))))))
          (*print-level* 3)
          (*print-circle* t))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#(#(#(#)))"
  "#(#(#(#)))")

(deftest write-array-circle.10
  (with-default-print
    (let ((v #1a(#((#1a(#((a b c)))))))
          (*print-level* 3)
          (*print-circle* t))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#(#((#)))"
  "#(#((#)))")

(deftest write-array-circle.11
  (with-default-print
    (let ((v #1a(a #1a(#1a(#1a(#1a(#1a(a b c))) e f g h i)) b c d e f))
          (*print-length* 2)
          (*print-level* 3)
          (*print-circle* t))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#(A #(#(# E ...)) ...)"
  "#(A #(#(# E ...)) ...)")

(deftest write-array-circle.12
  (with-default-print
    (let ((v #1=#1a(a b c #1# d))
          (*print-circle* t))
      (values
        (lisp-system::array-general-p v)
        (prin1-to-string v)
        (princ-to-string v))))
  t
  "#1=#(A B C #1# D)"
  "#1=#(A B C #1# D)")

(deftest write-array-circle.13
  (with-default-print
    (let ((v '#1a(a b (c d e #(f g #1=(h i) j) k) l m #1# o))
          (*print-circle* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(A B (C D E #(F G #1=(H I) J) K) L M #1# O)"
  "#(A B (C D E #(F G #1=(H I) J) K) L M #1# O)")


;;  specialized-array bit
(deftest write-array-bit.1
  (with-default-print
    (let ((v (make-array 5 :element-type 'bit :initial-contents '(1 0 0 1 1))))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#*10011"
  "#*10011")

(deftest write-array-bit.2
  (with-default-print
    (let ((v (make-array '(2 5) :element-type 'bit
                         :initial-contents '((0 0 1 1 1) (1 0 0 1 1)))))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#2A((0 0 1 1 1) (1 0 0 1 1))"
  "#2A((0 0 1 1 1) (1 0 0 1 1))")


;;  specialized-array character
(deftest write-array-character.1
  (with-default-print
    (let ((v (make-array 5 :element-type 'character :initial-contents "Hello")))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "\"Hello\""
  "Hello")

(deftest write-array-character.2
  (with-default-print
    (let ((v (make-array '(2 5) :element-type 'character
                         :initial-contents '("Hello" "ABcdE"))))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#2A((#\\H #\\e #\\l #\\l #\\o) (#\\A #\\B #\\c #\\d #\\E))"
  "#2A((H e l l o) (A B c d E))")


;;  specialized-array
(deftest write-array-specialized.1
  (with-default-print
    (let ((v (make-array 3 :element-type '(signed-byte 8)
                         :initial-contents '(10 -2 3))))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(10 -2 3)" "#(10 -2 3)")


(deftest write-array-specialized.2
  (with-default-print
    (let ((v (make-array 3 :element-type '(unsigned-byte 8)
                         :initial-contents '(10 2 3))))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(10 2 3)" "#(10 2 3)")

(deftest write-array-specialized.3
  (with-default-print
    (let ((v (make-array 3 :element-type 'single-float
                         :initial-contents '(1.2F0 2.3F0 -3.4F0))))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(1.2 2.3 -3.4)"
  "#(1.2 2.3 -3.4)")

(deftest write-array-specialized.4
  (with-default-print
    (let ((v (make-array 3 :element-type 'double-float
                         :initial-contents '(1.2D0 2.3D0 -3.4D0))))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#(1.2D0 2.3D0 -3.4D0)"
  "#(1.2D0 2.3D0 -3.4D0)")

(deftest write-array-specialized.5
  (with-default-print
    (let ((v (make-array '(2 3) :element-type 'long-float
                         :initial-contents
                         '((1.2L0 2.3L0 -3.4L0)
                           (4.5L0 2.1L0 3.2L0)))))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#2A((1.2L0 2.3L0 -3.4L0) (4.5L0 2.1L0 3.2L0))"
  "#2A((1.2L0 2.3L0 -3.4L0) (4.5L0 2.1L0 3.2L0))")

