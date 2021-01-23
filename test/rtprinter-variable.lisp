;;
;;  ANSI COMMON LISP: 22. Printer
;;

;;
;;  Variable *PRINT-ARRAY*
;;

;;  on array - circle
;;  on array - no-circle
;;  off array - circle
;;  off array - no-circle
(deftest print-array.1
  (with-default-print
    (let ((*print-circle* t)
          (*print-array* nil))
      (equal (with-output-to-string (stream)
               (write #1a(1 2 3) :stream stream))
             "#(1 2 3)")))
  nil)

(deftest print-array.2
  (with-default-print
    (let ((*print-circle* t)
          (*print-array* t))
      (with-output-to-string (stream)
        (write #1a(1 2 3) :stream stream))))
  "#(1 2 3)")

(deftest print-array.3
  (with-default-print
    (let ((*print-circle* nil)
          (*print-array* nil))
      (equal (write-to-string #1a(1 2 3))
             "#(1 2 3)")))
  nil)

(deftest print-array.4
  (with-default-print
    (let ((*print-circle* nil)
          (*print-array* t))
      (write-to-string #1a(1 2 3))))
  "#(1 2 3)")

;;  on vector - circle
;;  on vector - no-circle
;;  off vector - circle
;;  off vector - no-circle
(deftest print-array.5
  (with-default-print
    (let ((*print-circle* t))
      (equal (with-output-to-string (stream)
               (write #(1 2 3) :array nil :stream stream))
             "#(1 2 3)")))
  nil)

(deftest print-array.6
  (with-default-print
    (let ((*print-circle* t))
      (with-output-to-string (stream)
        (write #(1 2 3) :array t :stream stream))))
  "#(1 2 3)")

(deftest print-array.7
  (with-default-print
    (let ((*print-circle* nil))
      (equal (write-to-string #(1 2 3) :array nil)
             "#(1 2 3)")))
  nil)

(deftest print-array.8
  (with-default-print
    (let ((*print-circle* nil))
      (write-to-string #(1 2 3) :array t)))
  "#(1 2 3)")

;;  on bit-vector
;;  off bit-vector
(deftest print-array.9
  (with-default-print
    (equal (write-to-string #*11001 :array nil)
           "#*11001"))
  nil)

(deftest print-array.10
  (with-default-print
    (write-to-string #*11001 :array t))
  "#*11001")

;;  on string
;;  off string
(deftest print-array.11
  (with-default-print
    (write-to-string "Hello" :array nil :escape nil))
  "Hello")

(deftest print-array.12
  (with-default-print
    (write-to-string "Hello" :array t :escape nil))
  "Hello")


;;
;;  Variable *PRINT-BASE*
;;
(deftest print-base.1
  (with-default-print
    (let ((*print-base* 10)
          (*print-radix* t))
      (write-to-string 10)))
  "10.")

(deftest print-base.2
  (with-default-print
    (let ((*print-radix* t))
      (with-output-to-string (x)
        (write 10 :base 2 :stream x))))
  "#b1010")

(deftest print-base.3
  (with-default-print
    (let ((*print-radix* t))
      (write-to-string 10 :base 8)))
  "#o12")

(deftest print-base.4
  (with-default-print
    (let ((*print-base* 16)
          (*print-radix* t))
      (write-to-string 123)))
  "#x7B")

(deftest print-base.5
  (with-default-print
    (let ((*print-base* 5)
          (*print-radix* t))
      (write-to-string 123)))
  "#5r443")

(deftest print-base.6
  (with-default-print
    (let ((*print-base* 36)
          (*print-radix* t))
      (write-to-string 1000000000)))
  "#36rGJDGXS")

(deftest print-base.7
  (with-default-print
    (let ((*print-base* 10)
          (*print-radix* t))
      (write-to-string 4/5)))
  "#10r4/5")

(deftest-error print-base-error.1
  (eval '(let ((*print-base* 1)
               (*print-radix* t))
           (write-to-string 123))))

(deftest-error print-base-error.2
  (eval '(let ((*print-base* 37)
               (*print-radix* t))
           (write-to-string 123))))

(deftest-error print-base-error.3
  (write-to-string 123 :base 1 :radix t))

(deftest-error print-base-error.4
  (write-to-string 123 :base 37 :radix t))


;;
;;  Variable *PRINT-RADIX*
;;
(deftest print-radix.1
  (with-default-print
    (let ((*print-base* 10)
          (*print-radix* nil))
      (write-to-string 10)))
  "10")

(deftest print-radix.2
  (with-default-print
    (let ((*print-base* 10)
          (*print-radix* t))
      (write-to-string 10)))
  "10.")

(deftest print-radix.3
  (with-default-print
    (with-output-to-string (x)
      (write 10 :base 2 :radix nil :stream x)))
  "1010")

(deftest print-radix.4
  (with-default-print
    (with-output-to-string (x)
      (write 10 :base 2 :radix t :stream x)))
  "#b1010")

(deftest print-radix.5
  (with-default-print
    (write-to-string 10 :base 8 :radix nil))
  "12")

(deftest print-radix.6
  (with-default-print
    (write-to-string 10 :base 8 :radix 10))
  "#o12")

;;  ANSI Common Lisp
(deftest print-base-test.1
  (with-default-print
    (let ((*print-base* 24.) (*print-radix* t))
      (write-to-string 23.)))
  "#24rN")

(deftest print-base-test.2
  (with-default-print
    (let ((*print-base* 10)
          (*print-radix* nil)
          list)
      (dotimes (i 35)
        (let ((*print-base* (+ i 2)))
          (push (write-to-string 40) list)
          (if (zerop (mod i 10))
            (push nil list))))
      (nreverse list)))
  ("101000" nil
   "1111" "220" "130" "104" "55" "50" "44" "40" "37" "34" nil
   "31" "2C" "2A" "28" "26" "24" "22" "20" "1J" "1I" nil
   "1H" "1G" "1F" "1E" "1D" "1C" "1B" "1A" "19" "18" nil
   "17" "16" "15" "14"))

(deftest print-base-test.3
  (with-default-print
    (let (list)
      (dolist (pb '(2 3 8 10 16))
        (let ((*print-radix* t)
              (*print-base* pb))
          (push (format nil "~S  ~S" 10 1/10) list)))
      (nreverse list)))
  ("#b1010  #b1/1010"
   "#3r101  #3r1/101"
   "#o12  #o1/12"
   "10.  #10r1/10"
   "#xA  #x1/A"))


;;
;;  Variable *PRINT-CASE*
;;
(deftest print-case.1
  (with-default-print
    (let ((*print-case* :upcase))
      (write-to-string :hello)))
  ":HELLO")

(deftest print-case.2
  (with-default-print
    (write-to-string :hello :case :downcase))
  ":hello")

(deftest print-case.3
  (with-default-print
    (with-output-to-string (x)
      (write :hello-abcd :case :capitalize :stream x)))
  ":Hello-Abcd")

(deftest-error! print-case-error.1
  (eval '(let ((*print-case* t))
           (write-to-string :hello))))

(deftest-error print-case-error.2
  (write :hello :case :hello))

(deftest-error print-case-error.3
  (write-to-string :hello :case :hello))


;;
;;  Variable *PRINT-CIRCLE*
;;
(deftest print-circle.1
  (with-default-print
    (let ((*print-circle* t))
      (with-output-to-string (x)
        (write ' #1=(a b c . #1#) :stream x))))
  "#1=(A B C . #1#)")

(deftest print-circle.2
  (with-default-print
    (with-output-to-string (x)
      (write ' #1=(a b c . #1#) :circle t :stream x)))
  "#1=(A B C . #1#)")

(deftest print-circle.3
  (with-default-print
    (write-to-string ' #1=(a b c . #1#) :circle 100))
  "#1=(A B C . #1#)")

(deftest print-circle-test.1
  (let ((a (list 1 2 3)))
    (setf (cdddr a) a)
    (let ((*print-circle* t))
      (values
        (write-to-string a)
        :done)))
  "#1=(1 2 3 . #1#)"
  :done)


;;
;;  Variable *PRINT-ESCAPE*
;;
(deftest print-escape.1
  (with-default-print
    (let ((*print-escape* t))
      (with-output-to-string (x)
        (write #\Z :stream x))))
  "#\\Z")

(deftest print-escape.2
  (with-default-print
    (with-output-to-string (x)
      (write #\Z :escape nil :stream x)))
  "Z")

(deftest print-escape.3
  (with-default-print
    (write-to-string :hello :escape t))
  ":HELLO")

(deftest print-escape.4
  (with-default-print
    (write-to-string :hello :escape nil))
  "HELLO")

(deftest print-escape-test.1
  (let ((*print-escape* t))
    (write-to-string #\a))
  "#\\a")

(deftest print-escape-test.2
  (let ((*print-escape* nil))
    (write-to-string #\a))
  "a")


;;
;;  Variable *PRINT-GENSYM*
;;
(deftest print-gensym.1
  (with-default-print
    (let ((*print-gensym* t))
      (with-output-to-string (x)
        (write (make-symbol "HELLO") :stream x))))
  "#:HELLO")

(deftest print-gensym.2
  (with-default-print
    (with-output-to-string (x)
      (write (make-symbol "HELLO") :gensym t :escape nil :stream x)))
  "HELLO")

(deftest print-gensym.3
  (with-default-print
    (write-to-string (make-symbol "HELLO") :gensym nil :escape t))
  "HELLO")

(deftest print-gensym.4
  (with-default-print
    (write-to-string (make-symbol "HELLO") :gensym t :escape t))
  "#:HELLO")


;;
;;  Variable *PRINT-LEVEL*
;;
(deftest print-level.1
  (with-default-print
    (let ((*print-level* nil))
      (write-to-string '(a b c d))))
  "(A B C D)")

(deftest print-level.2
  (with-default-print
    (let ((*print-length* nil)
          (a '(1 (2 (3 (4 (5 (6)))))))
          list)
      (dotimes (i 8)
        (let ((*print-level* i))
          (push (format nil "~D -- ~S" i a) list)))
      (nreverse list)))
  ("0 -- #"
   "1 -- (1 #)"
   "2 -- (1 (2 #))"
   "3 -- (1 (2 (3 #)))"
   "4 -- (1 (2 (3 (4 #))))"
   "5 -- (1 (2 (3 (4 (5 #)))))"
   "6 -- (1 (2 (3 (4 (5 (6))))))"
   "7 -- (1 (2 (3 (4 (5 (6))))))"))

(deftest print-level.3
  (with-default-print
    (let ((*print-length* nil)
          (a #(1 (2 (3 (4 (5 (6)))))))
          list)
      (dotimes (i 8)
        (let ((*print-level* i))
          (push (format nil "~D -- ~S" i a) list)))
      (nreverse list)))
  ("0 -- #"
   "1 -- #(1 #)"
   "2 -- #(1 (2 #))"
   "3 -- #(1 (2 (3 #)))"
   "4 -- #(1 (2 (3 (4 #))))"
   "5 -- #(1 (2 (3 (4 (5 #)))))"
   "6 -- #(1 (2 (3 (4 (5 (6))))))"
   "7 -- #(1 (2 (3 (4 (5 (6))))))"))

(deftest-error! print-level-error.1
  (eval '(let ((*print-level* t))
           (write 10))))

(deftest-error print-level-error.2
  (eval '(write 10 :level :hello)))

(deftest-error print-level-error.3
  (eval '(write-to-string 10 :level :hello)))


;;
;;  Variable *PRINT-LENGTH*
;;
(deftest print-length.1
  (with-default-print
    (let ((*print-length* nil))
      (write-to-string '(a b c d))))
  "(A B C D)")

(deftest print-length.2
  (with-default-print
    (let ((*print-level* nil)
          (a '(1 2 3 4 5 6))
          list)
      (dotimes (i 7)
        (let ((*print-length* i))
          (push (format nil "~D -- ~S" i a) list)))
      (nreverse list)))
  ("0 -- (...)"
   "1 -- (1 ...)"
   "2 -- (1 2 ...)"
   "3 -- (1 2 3 ...)"
   "4 -- (1 2 3 4 ...)"
   "5 -- (1 2 3 4 5 ...)"
   "6 -- (1 2 3 4 5 6)"))

(deftest print-length.3
  (with-default-print
    (let ((*print-level* nil)
          (a #(1 2 3 4 5 6))
          list)
      (dotimes (i 7)
        (let ((*print-length* i))
          (push (format nil "~D -- ~S" i a) list)))
      (nreverse list)))
  ("0 -- #(...)"
   "1 -- #(1 ...)"
   "2 -- #(1 2 ...)"
   "3 -- #(1 2 3 ...)"
   "4 -- #(1 2 3 4 ...)"
   "5 -- #(1 2 3 4 5 ...)"
   "6 -- #(1 2 3 4 5 6)"))

(deftest-error! print-length-error.1
  (eval '(let ((*print-length* t))
           (write 10))))

(deftest-error print-length-error.2
  (eval '(write 10 :length :hello)))

(deftest-error print-length-error.3
  (eval '(write-to-string 10 :length :hello)))


;;
;;  Variable *PRINT-LINES*
;;
(deftest print-lines.1
  (with-default-print
    (let ((*print-lines* nil))
      (write-to-string 10))
    (values)))

(deftest-error! print-lines-error.1
  (eval '(let ((*print-lines* t))
           (write 10))))

(deftest-error print-lines-error.2
  (eval '(write 10 :lines :hello)))

(deftest-error print-lines-error.3
  (eval '(write-to-string 10 :lines :hello)))


;;
;;  Variable *PRINT-MISER-WIDTH*
;;
(deftest print-miser-width.1
  (with-default-print
    (let ((*print-miser-width* nil))
      (write-to-string 10))
    (values)))

(deftest-error! print-miser-width-error.1
  (eval '(let ((*print-miser-width* t))
           (write 10))))

(deftest-error print-miser-width-error.2
  (eval '(write 10 :miser-width :hello)))

(deftest-error print-miser-width-error.3
  (eval '(write-to-string 10 :miser-width :hello)))


;;
;;  Variable *PRINT-PRETTY*
;;
(deftest print-pretty.1
  (with-default-print
    (let ((*print-pretty* t))
      (with-output-to-string (x)
        (write '(quote 10) :stream x))))
  "'10")

(deftest print-pretty.2
  (with-default-print
    (with-output-to-string (x)
      (write '(quote 10) :pretty nil :stream x)))
  "(QUOTE 10)")

(deftest print-pretty.3
  (with-default-print
    (write-to-string '(quote hello) :pretty t))
  "'HELLO")


;;
;;  Variable *PRINT-RIGHT-MARGIN*
;;
(deftest print-right-margin.1
  (with-default-print
    (let ((*print-right-margin* nil))
      (write-to-string 10))
    (values)))

(deftest-error! print-right-margin-error.1
  (eval '(let ((*print-right-margin* t))
           (write 10))))

(deftest-error print-right-margin-error.2
  (eval '(write 10 :right-margin :hello)))

(deftest-error print-right-margin-error.3
  (eval '(write-to-string 10 :right-margin :hello)))



;;
;;  Variable *PRINT-READABLY*
;;

;;  *print-escape*
(deftest print-readably-escape.1
  (with-default-print
    (flet ((z (x y) (let ((*print-readably* x)
                          (*print-escape* y))
                      (with-output-to-string (stream)
                        (write :hello :stream stream)))))
      (values
        (z nil nil)
        (z t nil)
        (z nil t)
        (z t t))))
  "HELLO" ":HELLO" ":HELLO" ":HELLO")

(deftest print-readably-escape.2
  (with-default-print
    (flet ((z (x y) (with-output-to-string (stream)
                      (write :hello :readably x :escape y :stream stream))))
      (values
        (z nil nil)
        (z t nil)
        (z nil t)
        (z t t))))
  "HELLO" ":HELLO" ":HELLO" ":HELLO")

(deftest print-readably-escape.3
  (with-default-print
    (flet ((z (x y) (write-to-string :hello :readably x :escape y)))
      (values
        (z nil nil)
        (z t nil)
        (z nil t)
        (z t t))))
  "HELLO" ":HELLO" ":HELLO" ":HELLO")


;;  *print-array*
(deftest print-readably-array.1
  (with-default-print
    (flet ((z (x y) (let ((*print-readably* x)
                          (*print-array* y))
                      (subseq
                        (with-output-to-string (stream)
                          (write #(1 2 3) :stream stream))
                        0 2))))
      (values
        (z nil nil)
        (z t nil)
        (z nil t)
        (z t t))))
  "#<" "#(" "#(" "#(")

(deftest print-readably-array.2
  (with-default-print
    (flet ((z (x y) (subseq
                      (with-output-to-string (stream)
                        (write #1a(1 2 3) :readably x :array y :stream stream))
                      0 2)))
      (values
        (z nil nil)
        (z t nil)
        (z nil t)
        (z t t))))
  "#<" "#(" "#(" "#(")

(deftest print-readably-array.3
  (with-default-print
    (flet ((z (x y) (subseq
                      (write-to-string #*10110 :readably x :array y)
                      0 2)))
      (values
        (z nil nil)
        (z t nil)
        (z nil t)
        (z t t))))
  "#<" "#*" "#*" "#*")


;;  *print-gensym*
(deftest print-readably-gensym.1
  (with-default-print
    (flet ((z (x y) (let ((*print-readably* x)
                          (*print-gensym* y))
                      (with-output-to-string (stream)
                        (write (make-symbol "HELLO") :stream stream)))))
      (values
        (z nil nil)
        (z t nil)
        (z nil t)
        (z t t))))
  "HELLO" "#:HELLO" "#:HELLO" "#:HELLO")

(deftest print-readably-gensym.2
  (with-default-print
    (flet ((z (x y) (with-output-to-string (stream)
                      (write (make-symbol "HELLO")
                             :readably x :gensym y :stream stream))))
      (values
        (z nil nil)
        (z t nil)
        (z nil t)
        (z t t))))
  "HELLO" "#:HELLO" "#:HELLO" "#:HELLO")

(deftest print-readably-gensym.3
  (with-default-print
    (flet ((z (x y) (write-to-string (make-symbol "HELLO") :readably x :gensym y)))
      (values
        (z nil nil)
        (z t nil)
        (z nil t)
        (z t t))))
  "HELLO" "#:HELLO" "#:HELLO" "#:HELLO")


;;  *print-level*
(deftest print-readably-level.1
  (with-default-print
    (flet ((z (x) (let ((*print-readably* x)
                        (*print-level* 2))
                    (with-output-to-string (stream)
                      (write '(a (b (c (d (e))))) :stream stream)))))
      (values (z t) (z nil))))
  "(A (B (C (D (E)))))" "(A (B #))")

(deftest print-readably-level.2
  (with-default-print
    (flet ((z (x) (with-output-to-string (stream)
                    (write '(a (b (c (d (e)))))
                           :readably x :level 2 :stream stream))))
      (values (z t) (z nil))))
  "(A (B (C (D (E)))))" "(A (B #))")

(deftest print-readably-level.3
  (with-default-print
    (flet ((z (x) (write-to-string '(a (b (c (d (e))))) :readably x :level 2)))
      (values (z t) (z nil))))
  "(A (B (C (D (E)))))" "(A (B #))")


;;  *print-length*
(deftest print-readably-length.1
  (with-default-print
    (flet ((z (x) (let ((*print-readably* x)
                        (*print-length* 2))
                    (with-output-to-string (stream)
                      (write '(a b c d e) :stream stream)))))
      (values (z t) (z nil))))
  "(A B C D E)" "(A B ...)")

(deftest print-readably-length.2
  (with-default-print
    (flet ((z (x) (with-output-to-string (stream)
                    (write '(a b c d e) :readably x :length 2 :stream stream))))
      (values (z t) (z nil))))
  "(A B C D E)" "(A B ...)")

(deftest print-readably-length.3
  (with-default-print
    (flet ((z (x) (write-to-string '(a b c d e) :readably x :length 2)))
      (values (z t) (z nil))))
  "(A B C D E)" "(A B ...)")


;;  *print-lines*
(deftest print-readably-lines.1
  (with-default-print
    (flet ((z (x) (let ((*print-pretty* t)
                        (*print-right-margin* 11)
                        (*print-miser-width* nil)
                        (*print-lines* 3)
                        (*print-readably* x))
                    (with-output-to-string (*standard-output*)
                      (pprint-logical-block (nil nil)
                        (write "AAA") (pprint-newline :linear)
                        (write "BBB") (pprint-newline :linear)
                        (write "CCC") (pprint-newline :linear)
                        (write "DDD") (pprint-newline :linear))))))
      (values (z t) (z nil))))
  #.(mkstr "\"AAA\"" #\newline
           "\"BBB\"" #\newline
           "\"CCC\"" #\newline
           "\"DDD\"" #\newline)
  #.(mkstr "\"AAA\"" #\newline
           "\"BBB\"" #\newline
           "\"CCC\" .."))

