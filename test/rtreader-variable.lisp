;;
;;  ANSI COMMON LISP: 23. Reader
;;

;;
;;  Variable *READ-BASE*
;;
(deftest read-base.1
  *read-base*
  10)

(deftest read-base.2
  (let ((*read-base* 8))
    (values
      (read-from-string "123")))
  83)

(deftest read-base.3
  (let ((*read-base* 8))
    (values
      (read-from-string "#x123")))
  291)

(deftest read-base.4
  (let ((*read-base* 8))
    (values
      (read-from-string "123.")))
  123)

(deftest read-base.5
  (let ((*read-base* 8))
    (values
      (read-from-string "999.")))
  999)

(deftest read-base.6
  (let (list)
    (dotimes (i 6)
      (let ((*read-base* (+ 10. i)))
        (let ((object (read-from-string "(\\DAD DAD |BEE| BEE 123. 123)")))
          (push (list *read-base* object) list))))
    (nreverse list))
  ((10 (dad dad bee bee 123 123))
   (11 (dad dad bee bee 123 146))
   (12 (dad dad bee bee 123 171))
   (13 (dad dad bee bee 123 198))
   (14 (dad 2701 bee bee 123 227))
   (15 (dad 3088 bee 2699 123 258))))

(deftest-error read-base-error.1
  (eval '(setq *read-base* 1))
  type-error)

(deftest-error read-base-error.2
  (eval '(setq *read-base* 37))
  type-error)


;;
;;  Variable *READ-DEFAULT-FLOAT-FORMAT*
;;
(deftest read-default-float-format.1
  *read-default-float-format*
  single-float)

(deftest read-default-float-format.2
  (let ((*read-default-float-format* 'short-float))
    (values
      (read-from-string "12.3")))
  12.3f0)

(deftest read-default-float-format.3
  (let ((*read-default-float-format* 'single-float))
    (values
      (read-from-string "12.3")))
  12.3f0)

(deftest read-default-float-format.4
  (let ((*read-default-float-format* 'double-float))
    (values
      (read-from-string "12.3")))
  12.3d0)

(deftest read-default-float-format.5
  (let ((*read-default-float-format* 'long-float))
    (values
      (read-from-string "12.3")))
  12.3L0)

(deftest read-default-float-format.6
  (let ((*read-default-float-format* 'double-float))
    (values
      (read-from-string "(1.0 1.0e0 1.0s0 1.0f0 1.0d0 1.0L0)")))
  (1.0d0 1.0d0 1.0 1.0 1.0d0 1.0L0))

(deftest-error read-default-float-format.7
  (eval '(setq *read-default-float-format* :hello))
  type-error)


;;
;;  Variable *READ-EVAL*
;;
(deftest read-eval.1
  *read-eval*
  t)

(deftest read-eval.2
  (let ((*read-eval* t))
    (values
      (read-from-string "#.(+ 1 2 3)")))
  6)

(deftest-error read-eval.3
  (let ((*read-eval* nil))
    (values
      (read-from-string "#.(+ 1 2 3)")))
  reader-error)


;;
;;  Variable *READ-SUPPRESS*
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

(deftest read-suppress-test.1
  *read-suppress*
  nil)

(deftest read-suppress-test.2
  (let ((*read-suppress* t))
    (mapcar #'read-from-string
            '("#(foo bar baz)" "#P(:type :lisp)" "#c1.2"
              "#.(PRINT 'FOO)" "#3AHELLO" "#S(INTEGER)"
              "#*ABC" "#\\GARBAGE" "#RALPHA" "#3R444")))
  (nil nil nil nil nil nil nil nil nil nil))


;;
;;  Variable *READTABLE*
;;
(deftest readtable.1
  (readtablep
    *readtable*)
  t)

(deftest-error readtable.2
  (eval '(setq *readtable* 100))
  type-error)

