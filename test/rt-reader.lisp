;;
;;  ANSI COMMON LISP: 23. Reader
;;
(deftest read-suppress.1
  (let ((*read-suppress* t))
    (read-from-string "(cons 1 2)"))
  nil 10)

(deftest read-suppress.2
  (let ((*read-suppress* 'hello))
    (read-from-string "(cons 1 2)"))
  nil 10)

(deftest read-suppress.3
  (let ((*read-suppress*))
    (read-from-string "(cons 1 2)"))
  (cons 1 2) 10)

(deftest read-suppress.4
  (let ((*read-suppress* t))
    (read-from-string "(10 20 . 30 40)"))
  nil 15)

(deftest read-suppress.5
  (let ((*read-suppress* t))
    (read-from-string "12.34e1000000000"))
  nil 16)

(deftest read-suppress.6
  (let ((*read-suppress* t))
    (read-from-string "#(a b c 'hello)"))
  nil 15)

(deftest read-suppress.7
  (let ((*features* (cons :hello *features*)))
    (read-from-string "#+hello 999"))
  999 11)

(deftest-error read-suppress.8
  (let ((*features* (cons :hello *features*)))
    (read-from-string "#-hello 999"))
  end-of-file)

(deftest-error read-suppress.9
  (let ((*features* (cons :hello *features*)))
    (read-from-string "#-hello (10 20 . 30 40)"))
  end-of-file)

(deftest read-suppress.10
  (let ((*features* (cons :hello *features*)))
    (read-from-string "#-hello (10 20 . 30 40) aaa"))
  aaa 27)

(defun read-hello (x)
  (let ((*features* (cons :hello *features*)))
    (values
      (read-from-string x))))

(deftest-error read-suppress-error.1
  (read-hello "#-hello # 'aaa"))

(deftest read-suppress-equal.1
  (read-hello "(#+hello #1= 100 200)")
  (100 200))

(deftest read-suppress-equal.2
  (read-hello "(#-hello #1= 100 200)")
  (200))

(deftest read-suppress-sharp.1
  (read-hello "(#1= 100 #+hello #1#)")
  (100 100))

(deftest read-suppress-sharp.2
  (read-hello "(#1= 100 #-hello #1#)")
  (100))

(deftest read-suppress-single-quote.1
  (read-hello "(100 #-hello #'car)")
  (100))

(deftest read-suppress-open-parensis.1
  (read-hello "(100 #+hello (10 20 30))")
  (100 (10 20 30)))

(deftest read-suppress-open-parensis.2
  (read-hello "(100 #-hello (10 20 30))")
  (100))

(deftest read-suppress-open-parensis.3
  (read-hello "(100 #-hello (10 20 . 30 40 50))")
  (100))

(deftest-error read-suppress-close-parensis.1
  (read-hello "(100 #+hello ))"))

(deftest-error read-suppress-close-parensis.2
  (read-hello "(100 #-hello ))"))

(deftest read-suppress-asterisk.1
  (read-hello "(100 #+Hello #*1011)")
  (100 #*1011))

(deftest read-suppress-asterisk.2
  (read-hello "(100 #-Hello #*1011)")
  (100))

(deftest read-suppress-colon.1
  (length (read-hello "(100 #+Hello #:gensym)"))
  2)

(deftest read-suppress-colon.2
  (read-hello "(100 #-Hello #:gensym)")
  (100))

(deftest-error read-suppress-less.1
  (read-hello "(100 #+Hello #<)"))

(deftest-error read-suppress-less.2
  (read-hello "(100 #-Hello #<)"))

(deftest read-suppress-backslash.1
  (read-hello "(100 #+Hello #\\Space)")
  (100 #\Space))

(deftest read-suppress-backslash.2
  (read-hello "(100 #-Hello #\\Space)")
  (100))

(deftest read-suppress-or.1
  (read-hello "(100 #+Hello #| Hello |# 200)")
  (100 200))

(deftest read-suppress-or.2
  (read-hello "(100 #-Hello #| Hello |# 200)")
  (100))

(deftest read-suppress-plus.1
  (read-hello "(100 #+Hello #+aaa 200 300)")
  (100 300))

(deftest read-suppress-plus.2
  (read-hello "(100 #-Hello #+aaa 200 300)")
  (100))

(deftest read-suppress-minus.1
  (read-hello "(100 #+Hello #-aaa 200 300)")
  (100 200 300))

(deftest read-suppress-minus.2
  (read-hello "(100 #-Hello #-aaa 200 300)")
  (100 300))

(deftest read-suppress-dot.1
  (read-hello "(100 #+Hello #.(+ 10 20 30))")
  (100 60))

(deftest read-suppress-dot.2
  (read-hello "(100 #-Hello #.(+ 10 20 30))")
  (100))

(defvar *read-suppress-special* nil)
(deftest read-suppress-dot.3
  (let ((*read-suppress-special* nil))
    (values
      (read-hello "(100 #+Hello #.(setq *read-suppress-special* 200))")
      *read-suppress-special*))
  (100 200)
  200)

(deftest read-suppress-dot.4
  (let ((*read-suppress-special* nil))
    (values
      (read-hello "(100 #-Hello #.(setq *read-suppress-special* 200))")
      *read-suppress-special*))
  (100)
  nil)

(deftest read-suppress-radix.1
  (read-hello "(100 #+Hello #16rFF)")
  (100 255))

(deftest read-suppress-radix.2
  (read-hello "(100 #-Hello #10rFF)")
  (100))

(deftest-error read-suppress-radix.3
  (read-hello "(100 #+Hello #99rFF)"))

(deftest read-suppress-radix.4
  (read-hello "(100 #-Hello #99rFF)")
  (100))

(deftest-error read-suppress-radix.5
  (read-hello "(100 #+Hello #10rFF)"))

(deftest read-suppress-radix.6
  (read-hello "(100 #-Hello #10rFF)")
  (100))

(deftest-error read-suppress-radix.7
  (read-hello "(100 #+Hello #rFF)"))

(deftest read-suppress-radix.8
  (read-hello "(100 #-Hello #rFF)")
  (100))

(deftest read-suppress-binary.1
  (read-hello "(100 #+Hello #b1010)")
  (100 10))

(deftest read-suppress-binary.2
  (read-hello "(100 #-Hello #b1010)")
  (100))

(deftest-error read-suppress-binary.3
  (read-hello "(100 #+Hello #bABC)"))

(deftest read-suppress-binary.4
  (read-hello "(100 #-Hello #bABC)")
  (100))

(deftest read-suppress-octal.1
  (read-hello "(100 #+Hello #o765)")
  (100 501))

(deftest read-suppress-octal.2
  (read-hello "(100 #-Hello #o765)")
  (100))

(deftest-error read-suppress-octal.3
  (read-hello "(100 #+Hello #oABC)"))

(deftest read-suppress-octal.4
  (read-hello "(100 #-Hello #oABC)")
  (100))

(deftest read-suppress-hexadecimal.1
  (read-hello "(100 #+Hello #xFF)")
  (100 255))

(deftest read-suppress-hexadecimal.2
  (read-hello "(100 #-Hello #xFF)")
  (100))

(deftest-error read-suppress-hexadecimal.3
  (read-hello "(100 #+Hello #xHello)"))

(deftest read-suppress-hexadecimal.4
  (read-hello "(100 #-Hello #xHello)")
  (100))

(deftest read-suppress-complex.1
  (read-hello "(100 #+Hello #c(2 3))")
  (100 #c(2 3)))

(deftest read-suppress-complex.2
  (read-hello "(100 #-Hello #c(2 3))")
  (100))

(deftest-error read-suppress-complex.3
  (read-hello "(100 #+Hello #c(a b))"))

(deftest read-suppress-complex.4
  (read-hello "(100 #-Hello #c(a b))")
  (100))

(deftest-error read-suppress-complex.5
  (read-hello "(100 #+Hello #c(2 3 4 5 6))"))

(deftest read-suppress-complex.6
  (read-hello "(100 #-Hello #c(2 3 4 5 6))")
  (100))

(deftest read-suppress-array.1
  (read-hello "(100 #+Hello #1a(10 20 30))")
  (100 #1a(10 20 30)))

(deftest read-suppress-array.2
  (read-hello "(100 #-Hello #1a(10 20 30))")
  (100))

(deftest-error read-suppress-array.3
  (read-hello "(100 #+Hello #a(10 20 30))"))

(deftest read-suppress-array.4
  (read-hello "(100 #-Hello #a(10 20 30))")
  (100))

(deftest-error read-suppress-array.5
  (read-hello "(100 #+Hello #3a(a b c))"))

(deftest read-suppress-array.6
  (read-hello "(100 #-Hello #3a(a b c))")
  (100))

(deftest read-suppress-pathname.1
  (length (read-hello "(100 #+Hello #p\"Hello\")"))
  2)

(deftest read-suppress-pathname.2
  (read-hello "(100 #-Hello #p\"Hello\")")
  (100))

(deftest read-suppress-pathname.3
  (read-hello "(100 #-Hello #pHello)")
  (100))


;;
;;  colon
;;
(defpackage read-intern-colon (:use cl)
  (:export aaa bbb ccc ddd))

(deftest read-intern-colon.1
  (eq (intern "BBB" "KEYWORD")
      (read-from-string ":bbb"))
  t)

(deftest read-intern-colon.2
  (eq (intern "BBB" "READ-INTERN-COLON")
      (read-from-string "read-intern-colon::bbb"))
  t)

(deftest read-intern-colon.3
  (eq (intern "BBB" "READ-INTERN-COLON")
      (read-from-string "read-intern-colon:bbb"))
  t)

(deftest read-intern-colon.4
  (eq (intern "EEE" "READ-INTERN-COLON")
      (read-from-string "read-intern-colon::eee"))
  t)

(deftest-error read-intern-colon.5
  (read-from-string "read-intern-colon:eee"))

(deftest read-intern-colon.6
  (let ((*package* (find-package 'read-intern-colon)))
    (eq (intern "EEE" "READ-INTERN-COLON")
        (read-from-string "read-intern-colon::eee")))
  t)


;;
;;  error
;;
(deftest reader-quote-error.1
  (progn
    (read-from-string "`(block `(lambda 10))")
    (values)))


;;
;;  do-tests
;;
(do-tests :test t)

