;;
;;  ANSI COMMON LISP: 22. Printer
;;

;;  nil
(deftest write-nil.1
  (with-default-print
    (princ-to-string nil))
  "NIL")

(deftest write-nil.2
  (with-default-print
    (prin1-to-string nil))
  "NIL")

(deftest write-nil.3
  (with-default-print
    (write-to-string nil))

  "NIL")

(deftest write-nil.4
  (with-default-print
    (let ((*print-circle* t))
      (princ-to-string nil)))
  "NIL")

(deftest write-nil.5
  (with-default-print
    (let ((*print-circle* t))
      (write-to-string nil)))
  "NIL")

;; t
(deftest write-t.1
  (with-default-print
    (princ-to-string t))
  "T")

(deftest write-t.2
  (with-default-print
    (prin1-to-string t))
  "T")

(deftest write-t.3
  (with-default-print
    (let ((*print-circle* t))
      (write-to-string t)))
  "T")


;;  character
(deftest write-character.1
  (with-default-print
    (princ-to-string #\A))
  "A")

(deftest write-character.2
  (with-default-print
    (prin1-to-string #\A))
  "#\\A")

(deftest write-character.3
  (with-default-print
    (princ-to-string #\Space))
  " ")

(deftest write-character.4
  (with-default-print
    (prin1-to-string #\Space))
  "#\\Space")

(deftest write-character.5
  (with-default-print
    (let ((*print-circle* t))
      (write-to-string #\A)))
  "#\\A")


;;  string
(deftest write-string.1
  (with-default-print
    (princ-to-string "Hello"))
  "Hello")

(deftest write-string.2
  (with-default-print
    (prin1-to-string "Hello"))
  "\"Hello\"")

(deftest write-string.3
  (with-default-print
    (let ((*print-circle* t))
      (write-to-string "Hello")))
  "\"Hello\"")


;;  hash-table
(defun write-hash-table-make (&key (test 'eql))
  (let ((x (make-hash-table :test test)))
    (setf (gethash :aaa x) 10)
    (setf (gethash :bbb x) 20)
    (setf (gethash :ccc x) 30)
    x))

(deftest write-hash-table.1
  (with-default-print
    (subseq
      (princ-to-string
        (write-hash-table-make :test 'eq))
      0 33))
  "#<HASH-TABLE :TEST EQ :COUNT 3 #x")

(deftest write-hash-table.2
  (with-default-print
    (subseq
      (princ-to-string
        (write-hash-table-make :test 'equalp))
      0 37))
  "#<HASH-TABLE :TEST EQUALP :COUNT 3 #x")

(deftest write-hash-table.3
  (with-default-print
    (let ((*print-circle* t))
      (subseq
        (write-to-string
          (write-hash-table-make :test 'equalp))
        0 37)))
  "#<HASH-TABLE :TEST EQUALP :COUNT 3 #x")


;;  readtable
(deftest write-readtable.1
  (with-default-print
    (subseq
      (princ-to-string *readtable*)
      0 14))
  "#<READTABLE #x")

(deftest write-readtable.2
  (with-default-print
    (subseq
      (prin1-to-string *readtable*)
      0 14))
  "#<READTABLE #x")

(deftest write-readtable.3
  (with-default-print
    (let ((*print-circle* t))
      (subseq
        (write-to-string *readtable*)
        0 14)))
  "#<READTABLE #x")


;;  symbol
(deftest write-symbol.1
  (with-default-print
    (princ-to-string :hello))
  "HELLO")

(deftest write-symbol.2
  (with-default-print
    (prin1-to-string :hello))
  ":HELLO")

(deftest write-symbol-escape.1
  (with-default-print
    (prin1-to-string (make-symbol "HELLO")))
  "#:HELLO")

(deftest write-symbol-escape.2
  (with-default-print
    (let ((*print-gensym* nil))
      (prin1-to-string (make-symbol "HELLO"))))
  "HELLO")

(deftest write-symbol-escape.3
  (with-default-print
    (prin1-to-string (intern "ABC")))
  "ABC")

(deftest write-symbol-escape.4
  (with-default-print
    (prin1-to-string :hello))
  ":HELLO")

(defpackage write-symbol-package)
(deftest write-symbol-escape.5
  (with-default-print
    (prin1-to-string 'write-symbol-package::hello))
  "WRITE-SYMBOL-PACKAGE::HELLO")

(deftest write-symbol-escape.6
  (with-default-print
    (export 'write-symbol-package::abcd 'write-symbol-package)
    (prin1-to-string 'write-symbol-package::abcd))
  "WRITE-SYMBOL-PACKAGE:ABCD")

(deftest write-symbol-escape.7
  (with-default-print
    (prin1-to-string (intern "ABCDEFG")))
  "ABCDEFG")

(defun write-symbol-print (case1 case2 name)
  (let ((*print-case* case2)
        (*readtable* (copy-readtable))
        (symbol (intern name)))
    (setf (readtable-case *readtable*) case1)
    (values
      (prin1-to-string symbol)
      (princ-to-string symbol))))

(deftest write-symbol-up-up.1
  (write-symbol-print :upcase :upcase "ABCD")
  "ABCD" "ABCD")

(deftest write-symbol-up-up.2
  (write-symbol-print :upcase :upcase "abcd")
  "|abcd|" "abcd")

(deftest write-symbol-up-up.3
  (write-symbol-print :upcase :upcase "AbCD")
  "|AbCD|" "AbCD")

(deftest write-symbol-up-up.4
  (write-symbol-print :upcase :upcase "AB CD")
  #-ccl "|AB CD|" #+ccl "AB\\ CD"
  "AB CD")

(deftest write-symbol-up-up.5
  (write-symbol-print :upcase :upcase "ab cd")
  "|ab cd|" "ab cd")

(deftest write-symbol-up-down.1
  (write-symbol-print :upcase :downcase "ABCD")
  "abcd" "abcd")

(deftest write-symbol-up-down.2
  (write-symbol-print :upcase :downcase "abcd")
  "|abcd|" "abcd")

(deftest write-symbol-up-down.3
  (write-symbol-print :upcase :downcase "AbCD")
  "|AbCD|" "abcd")

(deftest write-symbol-up-down.4
  (write-symbol-print :upcase :downcase "AB CD")
  #-ccl "|AB CD|" #+ccl "AB\\ CD"
  "ab cd")

(deftest write-symbol-up-down.5
  (write-symbol-print :upcase :downcase "ab cd")
  "|ab cd|" "ab cd")

(deftest write-symbol-up-capitalize.1
  (write-symbol-print :upcase :capitalize "ABCD")
  "Abcd" "Abcd")

(deftest write-symbol-up-capitalize.2
  (write-symbol-print :upcase :capitalize "abcd")
  "|abcd|" "abcd")

(deftest write-symbol-up-capitalize.3
  (write-symbol-print :upcase :capitalize "AbCD")
  "|AbCD|" "Abcd")

(deftest write-symbol-up-capitalize.4
  (write-symbol-print :upcase :capitalize "AB CD")
  #-ccl "|AB CD|" #+ccl "AB\\ CD"
  "Ab Cd")

(deftest write-symbol-up-capitalize.5
  (write-symbol-print :upcase :capitalize "ab cd")
  "|ab cd|" "ab cd")

(deftest write-symbol-down-up.1
  (write-symbol-print :downcase :upcase "ABCD")
  "|ABCD|" "ABCD")

(deftest write-symbol-down-up.2
  (write-symbol-print :downcase :upcase "abcd")
  "ABCD" "ABCD")

(deftest write-symbol-down-up.3
  (write-symbol-print :downcase :upcase "AbCD")
  "|AbCD|" "ABCD")

(deftest write-symbol-down-up.4
  (write-symbol-print :downcase :upcase "AB CD")
  "|AB CD|" "AB CD")

(deftest write-symbol-down-up.5
  (write-symbol-print :downcase :upcase "ab cd")
  #-ccl "|ab cd|" #+ccl "ab\\ cd"
  "AB CD")

(deftest write-symbol-down-down.1
  (write-symbol-print :downcase :downcase "ABCD")
  "|ABCD|" "ABCD")

(deftest write-symbol-down-down.2
  (write-symbol-print :downcase :downcase "abcd")
  "abcd" "abcd")

(deftest write-symbol-down-down.3
  (write-symbol-print :downcase :downcase "AbCD")
  "|AbCD|" "AbCD")

(deftest write-symbol-down-down.4
  (write-symbol-print :downcase :downcase "AB CD")
  "|AB CD|" "AB CD")

(deftest write-symbol-down-down.5
  (write-symbol-print :downcase :downcase "ab cd")
  #-ccl "|ab cd|" #+ccl "ab\\ cd"
  "ab cd")

(deftest write-symbol-down-capitalize.1
  (write-symbol-print :downcase :capitalize "ABCD")
  "|ABCD|" "ABCD")

(deftest write-symbol-down-capitalize.2
  (write-symbol-print :downcase :capitalize "abcd")
  "Abcd" "Abcd")

(deftest write-symbol-down-capitalize.3
  (write-symbol-print :downcase :capitalize "AbCD")
  "|AbCD|" "AbCD")

(deftest write-symbol-down-capitalize.4
  (write-symbol-print :downcase :capitalize "AB CD")
  "|AB CD|" "AB CD")

(deftest write-symbol-down-capitalize.5
  (write-symbol-print :downcase :capitalize "ab cd")
  #-ccl "|ab cd|" #+ccl "ab\\ cd"
  "Ab Cd")

(deftest write-symbol-preserve-up.1
  (write-symbol-print :preserve :upcase "ABCD")
  "ABCD" "ABCD")

(deftest write-symbol-preserve-up.2
  (write-symbol-print :preserve :upcase "abcd")
  "abcd" "abcd")

(deftest write-symbol-preserve-up.3
  (write-symbol-print :preserve :upcase "AbCD")
  "AbCD" "AbCD")

(deftest write-symbol-preserve-up.4
  (write-symbol-print :preserve :upcase "AB CD")
  "|AB CD|" "AB CD")

(deftest write-symbol-preserve-up.5
  (write-symbol-print :preserve :upcase "ab cd")
  "|ab cd|" "ab cd")

(deftest write-symbol-preserve-down.1
  (write-symbol-print :preserve :downcase "ABCD")
  "ABCD" "ABCD")

(deftest write-symbol-preserve-down.2
  (write-symbol-print :preserve :downcase "abcd")
  "abcd" "abcd")

(deftest write-symbol-preserve-down.3
  (write-symbol-print :preserve :downcase "AbCD")
  "AbCD" "AbCD")

(deftest write-symbol-preserve-down.4
  (write-symbol-print :preserve :downcase "AB CD")
  "|AB CD|" "AB CD")

(deftest write-symbol-preserve-down.5
  (write-symbol-print :preserve :downcase "ab cd")
  "|ab cd|" "ab cd")

(deftest write-symbol-preserve-capitalize.1
  (write-symbol-print :preserve :capitalize "ABCD")
  "ABCD" "ABCD")

(deftest write-symbol-preserve-capitalize.2
  (write-symbol-print :preserve :capitalize "abcd")
  "abcd" "abcd")

(deftest write-symbol-preserve-capitalize.3
  (write-symbol-print :preserve :capitalize "AbCD")
  "AbCD" "AbCD")

(deftest write-symbol-preserve-capitalize.4
  (write-symbol-print :preserve :capitalize "AB CD")
  "|AB CD|" "AB CD")

(deftest write-symbol-preserve-capitalize.5
  (write-symbol-print :preserve :capitalize "ab cd")
  "|ab cd|" "ab cd")

(deftest write-symbol-invert-up.1
  (write-symbol-print :invert :upcase "ABCD")
  "abcd" "abcd")

(deftest write-symbol-invert-up.2
  (write-symbol-print :invert :upcase "abcd")
  "ABCD" "ABCD")

(deftest write-symbol-invert-up.3
  (write-symbol-print :invert :upcase "AbCD")
  "AbCD" "AbCD")

(deftest write-symbol-invert-up.4
  (write-symbol-print :invert :upcase "AB CD")
  "|AB CD|" "ab cd")

(deftest write-symbol-invert-up.5
  (write-symbol-print :invert :upcase "ab cd")
  "|ab cd|" "AB CD")

(deftest write-symbol-invert-down.1
  (write-symbol-print :invert :downcase "ABCD")
  "abcd" "abcd")

(deftest write-symbol-invert-down.2
  (write-symbol-print :invert :downcase "abcd")
  "ABCD" "ABCD")

(deftest write-symbol-invert-down.3
  (write-symbol-print :invert :downcase "AbCD")
  "AbCD" "AbCD")

(deftest write-symbol-invert-down.4
  (write-symbol-print :invert :downcase "AB CD")
  "|AB CD|" "ab cd")

(deftest write-symbol-invert-down.5
  (write-symbol-print :invert :downcase "ab cd")
  "|ab cd|" "AB CD")

(deftest write-symbol-invert-capitalize.1
  (write-symbol-print :invert :capitalize "ABCD")
  "abcd" "abcd")

(deftest write-symbol-invert-capitalize.2
  (write-symbol-print :invert :capitalize "abcd")
  "ABCD" "ABCD")

(deftest write-symbol-invert-capitalize.3
  (write-symbol-print :invert :capitalize "AbCD")
  "AbCD" "AbCD")

(deftest write-symbol-invert-capitalize.4
  (write-symbol-print :invert :capitalize "AB CD")
  "|AB CD|" "ab cd")

(deftest write-symbol-invert-capitalize.5
  (write-symbol-print :invert :capitalize "ab cd")
  "|ab cd|" "AB CD")


;;  fixnum
(defun write-fixnum-print (base radix value)
  (with-default-print
    (let ((*print-base* base)
          (*print-radix* radix))
      (unless (lisp-system::fixnump value)
        (error "Invalid fixnum value ~S." value))
      (values
        (prin1-to-string value)
        (princ-to-string value)))))

(deftest write-fixnum.1
  (write-fixnum-print 10 nil 123)
  "123" "123")

(deftest write-fixnum.2
  (write-fixnum-print 10 t 123)
  "123." "123.")

(deftest write-fixnum.3
  (write-fixnum-print 2 nil #b1111011)
  "1111011" "1111011")

(deftest write-fixnum.4
  (write-fixnum-print 2 t #b1111011)
  "#b1111011" "#b1111011")

(deftest write-fixnum.5
  (write-fixnum-print 8 nil #o173)
  "173" "173")

(deftest write-fixnum.6
  (write-fixnum-print 8 t #o173)
  "#o173" "#o173")

(deftest write-fixnum.7
  (write-fixnum-print 16 nil #x7B)
  "7B" "7B")

(deftest write-fixnum.8
  (write-fixnum-print 16 t #x7B)
  "#x7B" "#x7B")

(deftest write-fixnum.9
  (write-fixnum-print 20 nil #20r3H343I)
  "3H343I" "3H343I")

(deftest write-fixnum.10
  (write-fixnum-print 20 t #20r3H343I)
  "#20r3H343I" "#20r3H343I")

(deftest write-fixnum.11
  (write-fixnum-print 10 nil 0)
  "0" "0")

(deftest write-fixnum.12
  (write-fixnum-print 10 t 0)
  "0." "0.")

(deftest write-fixnum.13
  (write-fixnum-print 10 nil -123)
  "-123" "-123")

(deftest write-fixnum.14
  (write-fixnum-print 10 t -123)
  "-123." "-123.")

(deftest write-fixnum.15
  (write-fixnum-print 20 nil #20r-3H343I)
  "-3H343I" "-3H343I")

(deftest write-fixnum.16
  (write-fixnum-print 20 t #20r-3H343I)
  "#20r-3H343I" "#20r-3H343I")


;;  bignum
(defun write-bignum-print (base radix value)
  (with-default-print
    (let ((*print-base* base)
          (*print-radix* radix))
      (unless (lisp-system::bignump value)
        (error "Invalid bignum value ~S." value))
      (values
        (prin1-to-string value)
        (princ-to-string value)))))

(deftest write-bignum.1
  (write-bignum-print 10 nil 123456789012345678909876543211112223334445)
  "123456789012345678909876543211112223334445"
  "123456789012345678909876543211112223334445")

(deftest write-bignum.2
  (write-bignum-print 10 t 123456789012345678909876543211112223334445)
  "123456789012345678909876543211112223334445."
  "123456789012345678909876543211112223334445.")

(deftest write-bignum.3
  (write-bignum-print 16 nil #xE9D300053895AFE1A1E24BBA)
  "E9D300053895AFE1A1E24BBA" "E9D300053895AFE1A1E24BBA")

(deftest write-bignum.4
  (write-bignum-print 16 t #xE9D300053895AFE1A1E24BBA)
  "#xE9D300053895AFE1A1E24BBA" "#xE9D300053895AFE1A1E24BBA")

(deftest write-bignum.5
  (write-bignum-print 20 nil #20r-1BAI5H47D03G817DE300HGF)
  "-1BAI5H47D03G817DE300HGF" "-1BAI5H47D03G817DE300HGF")

(deftest write-bignum.6
  (write-bignum-print 20 t #20r-1BAI5H47D03G817DE300HGF)
  "#20r-1BAI5H47D03G817DE300HGF" "#20r-1BAI5H47D03G817DE300HGF")


;;  ratio
(defun write-ratio-print (base radix value)
  (with-default-print
    (let ((*print-base* base)
          (*print-radix* radix))
      (unless (lisp-system::ratiop value)
        (error "Invalid ratio value ~S." value))
      (values
        (prin1-to-string value)
        (princ-to-string value)))))

(deftest write-ratio.1
  (write-ratio-print 10 nil 10/123)
  "10/123" "10/123")

(deftest write-ratio.2
  (write-ratio-print 10 t 10/123)
  "#10r10/123" "#10r10/123")

(deftest write-ratio.3
  (write-ratio-print 10 nil -10/123)
  "-10/123" "-10/123")

(deftest write-ratio.4
  (write-ratio-print 10 t -10/123)
  "#10r-10/123" "#10r-10/123")

(deftest write-ratio.5
  (write-ratio-print 2 nil #b1010/1111011)
  "1010/1111011" "1010/1111011")

(deftest write-ratio.6
  (write-ratio-print 2 t #b1010/1111011)
  "#b1010/1111011" "#b1010/1111011")


;;  single-float
(defun write-float-print (float value)
  (with-default-print
    (let ((*read-default-float-format* float))
      (unless (typep value 'float)
        (error "Invalid float value ~S." value))
      (values
        (prin1-to-string value)
        (princ-to-string value)))))

(deftest write-single-float.1
  (write-float-print 'single-float 10.25f0)
  "10.25" "10.25")

(deftest write-single-float.2
  (write-float-print 'double-float 10.25f0)
  "10.25F0" "10.25F0")

(deftest write-single-float.3
  (write-float-print 'single-float 1f20)
  "1.0E20" "1.0E20")

(deftest write-single-float.4
  (write-float-print 'double-float 1f20)
  "1.0F20" "1.0F20")

(deftest write-single-float.5
  (write-float-print 'double-float 3f-20)
  "3.0F-20" "3.0F-20")


;;  double-float
(deftest write-double-float.1
  (write-float-print 'double-float 10.25d0)
  "10.25" "10.25")

(deftest write-double-float.2
  (write-float-print 'long-float 10.25d0)
  "10.25D0" "10.25D0")

(deftest write-double-float.3
  (write-float-print 'double-float 1d20)
  "1.0E20" "1.0E20")

(deftest write-double-float.4
  (write-float-print 'long-float 1d20)
  "1.0D20" "1.0D20")

(deftest write-double-float.5
  (write-float-print 'long-float 3d-20)
  "3.0D-20" "3.0D-20")


;;  long-float
(deftest write-long-float.1
  (write-float-print 'long-float 10.25L0)
  "10.25" "10.25")

(deftest write-long-float.2
  (write-float-print 'single-float 10.25L0)
  "10.25L0" "10.25L0")

(deftest write-long-float.3
  (write-float-print 'long-float 1L20)
  "1.0E20" "1.0E20")

(deftest write-long-float.4
  (write-float-print 'single-float 1L20)
  "1.0L20" "1.0L20")

(deftest write-long-float.5
  (write-float-print 'single-float 3L-20)
  "3.0L-20" "3.0L-20")


;;  complex
(deftest write-complex.1
  (with-default-print
    (let ((v #c(-10 20)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#C(-10 20)" "#C(-10 20)")

(deftest write-complex.2
  (with-default-print
    (let ((v #c(10 -20))
          (*print-radix* t))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#C(10. -20.)" "#C(10. -20.)")

(deftest write-complex.3
  (with-default-print
    (let ((v #c(1.2d0 -3.4d0)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#C(1.2D0 -3.4D0)" "#C(1.2D0 -3.4D0)")


;;  function
(deftest write-function.1
  (with-default-print
    (let ((v #'car))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#<COMPILED-FUNCTION CAR>"
  "#<COMPILED-FUNCTION CAR>")

(deftest write-function.2
  (with-default-print
    (let ((v #'(setf car)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#<COMPILED-FUNCTION (SETF CAR)>"
  "#<COMPILED-FUNCTION (SETF CAR)>")

(defun write-function-test (a b)
  (+ a b 100))

(deftest write-function.3
  (with-default-print
    (let ((v #'write-function-test))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#<FUNCTION WRITE-FUNCTION-TEST>"
  "#<FUNCTION WRITE-FUNCTION-TEST>")

(defun (setf write-function-test) (a b)
  (+ a b 100))

(deftest write-function.4
  (with-default-print
    (let ((v #'(setf write-function-test)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#<FUNCTION (SETF WRITE-FUNCTION-TEST)>"
  "#<FUNCTION (SETF WRITE-FUNCTION-TEST)>")

(deftest write-function.5
  (with-default-print
    (let ((v (lambda () :hello)))
      (values
        (subseq (prin1-to-string v) 0 20)
        (subseq (princ-to-string v) 0 20))))
  "#<FUNCTION LAMBDA #x"
  "#<FUNCTION LAMBDA #x")


;;  package
(deftest write-package.1
  (with-default-print
    (let ((v (find-package 'common-lisp-user)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#<PACKAGE COMMON-LISP-USER>"
  "#<PACKAGE COMMON-LISP-USER>")


;;  random-state
(deftest write-random-state.1
  (with-default-print
    (let ((v (make-random-state)))
      (values
        (subseq (prin1-to-string v) 0 17)
        (subseq (princ-to-string v) 0 17))))
  "#<RANDOM-STATE #x"
  "#<RANDOM-STATE #x")


;;  pathname
(deftest write-pathname.1
  (with-default-print
    (let ((v (make-pathname :host 'lisp-system::unix
                            :defaults #p"/usr/local/bin/npt")))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#P\"/usr/local/bin/npt\""
  "/usr/local/bin/npt")

(deftest write-pathname.2
  (with-default-print
    (let ((v (make-pathname :host 'lisp-system::windows
                            :defaults #p"/usr/local/bin/npt")))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#P\"\\\\usr\\\\local\\\\bin\\\\npt\""
  "\\usr\\local\\bin\\npt")


;;  stream
(deftest write-stream.1
  (with-default-print
    (let ((v (make-broadcast-stream)))
      (values
        (subseq (prin1-to-string v) 0 21)
        (subseq (princ-to-string v) 0 21))))
  "#<BROADCAST-STREAM #x"
  "#<BROADCAST-STREAM #x")


;;
;;  quote
;;
(deftest write-quote.1
  (with-default-print
    (let ((v (quote `hello)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "`HELLO" "`HELLO")

(deftest write-quote.2
  (with-default-print
    (let ((v (quote `(,hello))))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "`(,HELLO)" "`(,HELLO)")

(deftest write-quote.3
  (with-default-print
    (let ((v (quote `(,@hello))))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "`(,@HELLO)" "`(,@HELLO)")

(deftest write-quote.4
  (with-default-print
    (let ((v (quote `(,.hello))))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "`(,.HELLO)" "`(,.HELLO)")


;;  restart
(defmacro write-restart-value (name &optional report)
  `(restart-bind
     ((,name (lambda (c) (declare (ignore c)))
             ,@(when report
                 `(:report-function ,report))))
     (find-restart ',name)))

(deftest write-restart.1
  (with-default-print
    (let ((v (write-restart-value hello)))
      (values
        (subseq (prin1-to-string v) 0 18)
        (subseq (princ-to-string v) 0 18))))
  "#<RESTART HELLO #x"
  "#<RESTART HELLO #x")

(deftest write-restart.2
  (with-default-print
    (let ((v (write-restart-value
               hello (lambda (s) (princ "ABCD" s)))))
      (values
        (subseq (prin1-to-string v) 0 18)
        (princ-to-string v))))
  "#<RESTART HELLO #x"
  "ABCD")


;;  environment
(deftest write-environment.1
  (with-default-print
    (let ((v (macrolet ((call (&environment env) env)) (call))))
      (values
        (subseq (prin1-to-string v) 0 16)
        (subseq (princ-to-string v) 0 16))))
  "#<ENVIRONMENT #x"
  "#<ENVIRONMENT #x")


;;  bit-vector
(deftest write-bit-vector.1
  (with-default-print
    (let ((v #*1001011101))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#*1001011101"
  "#*1001011101")


;;  print-dispatch
(deftest write-print-dispatch.1
  (with-default-print
    (let ((v (copy-pprint-dispatch)))
      (values
        (subseq (prin1-to-string v) 0 19)
        (subseq (princ-to-string v) 0 19))))
  "#<PRINT-DISPATCH #x"
  "#<PRINT-DISPATCH #x")


;;  bytespec
(deftest write-bytespec.1
  (with-default-print
    (let ((v (byte 5 12)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#<BYTESPEC SIZE:5 POSITION:12>"
  "#<BYTESPEC SIZE:5 POSITION:12>")

