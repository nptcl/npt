;;
;;  ANSI COMMON LISP: 22. Printer
;;

;;
;;  LogicalBlock
;;
(deftest format-logical-block.1
  (format nil "~<~:>" nil)
  "")

(deftest format-logical-block.2
  (format nil "~<+++~A+++~:>" 10)
  "10")

(deftest format-logical-block.3
  (format nil "~<+++~A+++~:>" '(10))
  "+++10+++")

(deftest format-logical-block.4
  (format nil "~<AAA~;BBB~:>" nil)
  "AAABBB")

(deftest format-logical-block.5
  (format nil "~<~;BBB~:>" nil)
  "BBB")

(deftest format-logical-block.6
  (format nil "~:<BBB~:>" nil)
  "(BBB)")

(deftest format-logical-block.7
  (format nil "~:<AAA~;BBB~:>" nil)
  "AAABBB)")

(deftest format-logical-block.8
  (format nil "~:<~;BBB~:>" nil)
  "BBB)")

(deftest format-logical-block.9
  (format nil "~<AAA~;~:>" nil)
  "AAA")

(deftest-error format-logical-block.10
  (format nil "~<A~SAA~;BBB~:>" '(20)))

(deftest format-logical-block.11
  (format nil "~<AAA~;BBB~;CCC~:>" nil)
  "AAABBBCCC")

(deftest-error format-logical-block.12
  (format nil "~<AAA~;BBB~;C~SCC~:>" '(10)))

(deftest format-logical-block.13
  (format nil "~:<AAA~;BBB~;CCC~:>" nil)
  "AAABBBCCC")

(deftest format-logical-block.14
  (format nil "~:<-->~;++~W++~;<--~:>" '(10 20 30))
  "-->++10++<--")

(deftest format-logical-block.15
  (format nil "~:<-->~;++~A~A~A++~;<--~:>~A" '(#\< #\A #\>) "Hello")
  "-->++<A>++<--Hello")

(deftest format-logical-block.16
  (format nil "~<+++~W~^~W+++~:>~A" '(10 20 30) "ZZZ")
  "+++1020+++ZZZ")

(deftest format-logical-block.17
  (format nil "~<+++~W~^~W+++~:>~A" '(10 20 30) "ZZZ")
  "+++1020+++ZZZ")

(deftest format-logical-block.18
  (format nil "~<+++~W~^~W+++~:>" '(10 20 30))
  "+++1020+++")

(deftest format-logical-block.19
  (format nil "~<+++~W~^~W+++~:>~A" '(10) "ZZZ")
  "+++10ZZZ")

(deftest-error format-logical-block.20
  (format nil "~<~A-~@[AAA~]~:>" '(10 . 20)))

(deftest format-logical-block.21
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~<100~:_200~:_300~:_400~:_500~:_600~:>" nil))
  #.(mkstr "100200300" #\newline "400500600"))

(deftest format-logical-block.22
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "--~<100~:_200~:_300~:_400~:_500~:_600~:>" nil))
  #.(mkstr "--100200" #\newline
           "  300400" #\newline
           "  500600"))

(deftest format-logical-block.23
  (let ((*print-pretty* nil)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "--~<100~:_200~:_300~:_400~:_500~:_600~:>" nil))
  "--100200300400500600")

(deftest format-logical-block.24
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~<100 200 300 400 500 600~:@>" nil))
  #.(mkstr "100 200" #\newline
           "300 400" #\newline
           "500 600"))

(deftest format-logical-block.25
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~<100  200  300  400  500  600~:@>" nil))
  #.(mkstr "100  200" #\newline
           "300  400" #\newline
           "500  600"))

(deftest-error format-logical-block.26
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~@<~A~:>~A" 10 20)))

(deftest format-logical-block.27
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~@<+++~A, ~A+++~:>" 10 20 30 40))
  "+++10, 20+++")

(deftest-error format-logical-block.28
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~@<+++~A, ~A+++~:>~A" 10 20 30 40)))

(deftest format-logical-block.29
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~A~@<+++~A, ~A+++~:>" 10 20 30 40))
  "10+++20, 30+++")

(deftest format-logical-block-goto.1
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~A~@<+++~*~A, ~A+++~:>" 10 20 30 40))
  "10+++30, 40+++")

(deftest format-logical-block-goto.2
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~A~@<+++~:*~A, ~A+++~:>" 10 20 30 40))
  "10+++10, 20+++")

(deftest-error format-logical-block-goto.3
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~A~<+++~:*~A, ~A+++~:>" 10 '(20 30 40))))

(deftest-error format-logical-block-goto.4
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~A~<+++~A, ~10*~A+++~:>" 10 '(20 30 40 . 50))))

(deftest format-logical-block-goto.5
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~A~@<+++~:*~A, ~A+++~:>" 10 20 30 40))
  "10+++10, 20+++")

(deftest format-logical-block-goto.6
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~@<+++~A, ~:*~A+++~:>" 20 30 40))
  "+++20, 20+++")

(deftest format-logical-block-loop.1
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (format nil "~<+++~@{<~A>~}+++~:>" '(10 20 30 40)))
  "+++<10><20><30><40>+++")

(defun fmt-logical-block-loop2 (stream list)
  (format stream "~<~@{<~W>~}~:>" list))

(deftest format-logical-block-loop.2
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 77)
        (*print-miser-width* nil))
    (fmt-logical-block-loop2
      nil
      '#1=(aaa bbb #1#)))
  "#1=<AAA><BBB><#1#>")

(deftest format-logical-block-loop.3
  (let ((*print-pprint-dispatch* lisp-system::*empty-print-dispatch*)
        (*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 77)
        (*print-miser-width* nil))
    (fmt-logical-block-loop2
      nil
      '#1=(aaa bbb #1#)))
  "#1=<AAA><BBB><#1#>")

(deftest format-logical-block-loop.4
  (format nil "~<++~{<~A>~}++~A~:>~A" '((10 20 30) 40) 50)
  "++<10><20><30>++4050")

(deftest format-logical-block-loop.5
  (format nil "~<++~{<~A>~}++~A~:>~A" '((10 20 30 . 35) 40) 50)
  "++<10><20><30><. 3550")

(deftest format-logical-block-loop.6
  (format nil "~<++~{<~A~0^>~}++~A~:>~A" '((10 20 30 . 35) 40) 50)
  "++<10++4050")


;;
;;  Example
;;
(defun format-defun-examples (stream list)
  (format stream "~:<~W ~@_~:I~W ~:_~W~1I ~_~W~:>" list))

(deftest format-defun-examples.1
  (let ((*print-pretty* t)
        (*print-right-margin* 26)
        (*print-miser-width* nil))
    (format-defun-examples nil '(defun prod (x y) (* x y))))
  "(DEFUN PROD (X Y) (* X Y))")

(deftest format-defun-examples.2
  (let ((*print-pretty* t)
        (*print-right-margin* 25)
        (*print-miser-width* nil))
    (format-defun-examples nil '(defun prod (x y) (* x y))))
  #.(mkstr "(DEFUN PROD (X Y)" #\newline
           "  (* X Y))"))

(deftest format-defun-examples.3
  (let ((*print-pretty* t)
        (*print-right-margin* 15)
        (*print-miser-width* 13))
    (format-defun-examples nil '(defun prod (x y) (* x y))))
  #.(mkstr "(DEFUN PROD" #\newline
           "       (X Y)" #\newline
           "  (* X Y))"))

(deftest format-defun-examples.4
  (let ((*print-pretty* t)
        (*print-right-margin* 15)
        (*print-miser-width* 14))
    (format-defun-examples nil '(defun prod (x y) (* x y))))
  #.(mkstr "(DEFUN" #\newline
           " PROD" #\newline
           " (X Y)" #\newline
           " (* X Y))"))

(deftest format-defun-examples.5
  (let ((*print-pretty* t)
        (*print-right-margin* 20)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (pprint-logical-block (stream nil :per-line-prefix ";;; ")
        (format-defun-examples stream '(defun prod (x y) (* x y))))))
  #.(mkstr ";;; (DEFUN PROD" #\newline
           ";;;        (X Y)" #\newline
           ";;;   (* X Y))"))

(defun format-let-examples (stream list)
  (format stream "~:<~W~^ ~:<~@{~:<~@{~W~^ ~_~}~:>~^ ~:_~}~:>~1I~@{~^ ~_~W~}~:>" list))

(deftest format-let-examples.1
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 77)
        (*print-miser-width* nil))
    (format-let-examples
      nil
      '#1=(let (x (*print-length* (f (g 3)))
                  (z . 2) (k (car y)))
            (setq x (sqrt z)) #1#)))
  "#1=(LET (X (*PRINT-LENGTH* (F #)) (Z . 2) (K (CAR Y))) (SETQ X (SQRT Z)) #1#)")

(deftest format-let-examples.2
  (let ((*print-pprint-dispatch* lisp-system::*empty-print-dispatch*)
        (*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 77)
        (*print-miser-width* nil))
    (format-let-examples
      nil
      '#1=(let (x (*print-length* (f (g 3)))
                  (z . 2) (k (car y)))
            (setq x (sqrt z)) #1#)))
  "#1=(LET (X (*PRINT-LENGTH* (F #)) (Z . 2) (K (CAR Y))) (SETQ X (SQRT Z)) #1#)")

(deftest format-let-examples.3
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 76)
        (*print-miser-width* nil))
    (format-let-examples
      nil
      '#1=(let (x (*print-length* (f (g 3)))
                  (z . 2) (k (car y)))
            (setq x (sqrt z)) #1#)))
  #.(mkstr "#1=(LET (X (*PRINT-LENGTH* (F #)) (Z . 2) (K (CAR Y)))" #\newline
           "     (SETQ X (SQRT Z))" #\newline
           "     #1#)"))

(deftest format-let-examples.4
  (let ((*print-pprint-dispatch* lisp-system::*empty-print-dispatch*)
        (*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 76)
        (*print-miser-width* nil))
    (format-let-examples
      nil
      '#1=(let (x (*print-length* (f (g 3)))
                  (z . 2) (k (car y)))
            (setq x (sqrt z)) #1#)))
  #.(mkstr "#1=(LET (X (*PRINT-LENGTH* (F #)) (Z . 2) (K (CAR Y)))" #\newline
           "     (SETQ X (SQRT Z))" #\newline
           "     #1#)"))

(deftest format-let-examples.5
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 35)
        (*print-miser-width* nil))
    (format-let-examples
      nil
      '#1=(let (x (*print-length* (f (g 3)))
                  (z . 2) (k (car y)))
            (setq x (sqrt z)) #1#)))
  #.(mkstr "#1=(LET (X (*PRINT-LENGTH* (F #))" #\newline
           "         (Z . 2) (K (CAR Y)))" #\newline
           "     (SETQ X (SQRT Z))" #\newline
           "     #1#)"))

(deftest format-let-examples.6
  (let ((*print-pprint-dispatch* lisp-system::*empty-print-dispatch*)
        (*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 35)
        (*print-miser-width* nil))
    (format-let-examples
      nil
      '#1=(let (x (*print-length* (f (g 3)))
                  (z . 2) (k (car y)))
            (setq x (sqrt z)) #1#)))
  #.(mkstr "#1=(LET (X (*PRINT-LENGTH* (F #))" #\newline
           "         (Z . 2) (K (CAR Y)))" #\newline
           "     (SETQ X (SQRT Z))" #\newline
           "     #1#)"))

(deftest format-let-examples.7
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-length* 3)
        (*print-right-margin* 22)
        (*print-miser-width* nil))
    (format-let-examples
      nil
      '#1=(let (x (*print-length* (f (g 3)))
                  (z . 2) (k (car y)))
            (setq x (sqrt z)) #1#)))
  #.(mkstr
      "(LET (X" #\newline
      "      (*PRINT-LENGTH*" #\newline
      "       (F #))" #\newline
      "      (Z . 2) ...)" #\newline
      "  (SETQ X (SQRT Z))" #\newline
      "  ...)"))

(deftest format-let-examples.8
  (let ((*print-pprint-dispatch* lisp-system::*empty-print-dispatch*)
        (*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-length* 3)
        (*print-right-margin* 22)
        (*print-miser-width* nil))
    (format-let-examples
      nil
      '#1=(let (x (*print-length* (f (g 3)))
                  (z . 2) (k (car y)))
            (setq x (sqrt z)) #1#)))
  #.(mkstr
      "(LET (X" #\newline
      "      (*PRINT-LENGTH*" #\newline
      "       (F #))" #\newline
      "      (Z . 2) ...)" #\newline
      "  (SETQ X (SQRT Z))" #\newline
      "  ...)"))


;;
;;  Iteration
;;
(deftest format-logical-block-iteration-list.1
  (format nil "~<~{<~A>~}~A~:>~A" '((10 20 30) 40) 50)
  "<10><20><30>4050")

(deftest format-logical-block-iteration-list.2
  (format nil "~@<~{<~A>~}~A~:>" '(10 20 30) 40)
  "<10><20><30>40")

(deftest format-logical-block-iteration-list.3
  (format nil "~<~{<~A,~A>~}~A~:>~A" '((10 20 30 40) 50) 60)
  "<10,20><30,40>5060")

(deftest format-logical-block-iteration-list.4
  (format nil "~@<~{<~A,~A>~}~A~:>" '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-logical-block-iteration-list.5
  (format nil "~<~{<~A,~0^~A>~}~A~:>" '((10 20 30 40) 50))
  "<10,50")

(deftest format-logical-block-iteration-list.6
  (format nil "~@<~{<~A,~0^~A>~}~A~:>" '(10 20 30 40) 50)
  "<10,50")

(deftest format-logical-block-iteration-list.7
  (format nil "~<~{<~A,~^~A>~}~A~:>" '((10 20 30) 50))
  "<10,20><30,50")

(deftest format-logical-block-iteration-list.8
  (format nil "~@<~{<~A,~^~A>~}~A~:>" '(10 20 30) 50)
  "<10,20><30,50")

(deftest format-logical-block-iteration-list.9
  (with-empty-dispatch
    (format nil "~<~{<~A>~}~A~:>~A" '((10 20 30) 40) 50))
  "<10><20><30>4050")

(deftest format-logical-block-iteration-list.10
  (with-empty-dispatch
    (format nil "~@<~{<~A>~}~A~:>" '(10 20 30) 40))
  "<10><20><30>40")

(deftest format-logical-block-iteration-list.11
  (with-empty-dispatch
    (format nil "~<~{<~A,~A>~}~A~:>~A" '((10 20 30 40) 50) 60))
  "<10,20><30,40>5060")

(deftest format-logical-block-iteration-list.12
  (with-empty-dispatch
    (format nil "~@<~{<~A,~A>~}~A~:>" '(10 20 30 40) 50))
  "<10,20><30,40>50")

(deftest format-logical-block-iteration-list.13
  (with-empty-dispatch
    (format nil "~<~{<~A,~0^~A>~}~A~:>" '((10 20 30 40) 50)))
  "<10,50")

(deftest format-logical-block-iteration-list.14
  (with-empty-dispatch
    (format nil "~@<~{<~A,~0^~A>~}~A~:>" '(10 20 30 40) 50))
  "<10,50")

(deftest format-logical-block-iteration-list.15
  (with-empty-dispatch
    (format nil "~<~{<~A,~^~A>~}~A~:>" '((10 20 30) 50)))
  "<10,20><30,50")

(deftest format-logical-block-iteration-list.16
  (with-empty-dispatch
    (format nil "~@<~{<~A,~^~A>~}~A~:>" '(10 20 30) 50))
  "<10,20><30,50")

(deftest format-logical-block-iteration-rest.1
  (format nil "~<~@{<~A,~A>~}~:>~A" '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-logical-block-iteration-rest.2
  (format nil "~@<~@{<~A,~A>~}~:>" 10 20 30 40)
  "<10,20><30,40>")

(deftest format-logical-block-iteration-rest.3
  (format nil "~<~2@{<~A,~A>~}~A~:>~A" '(10 20 30 40 50 60) 70)
  "<10,20><30,40>5070")

(deftest format-logical-block-iteration-rest.4
  (format nil "~@<~2@{<~A,~A>~}~A~:>" 10 20 30 40 50 60)
  "<10,20><30,40>50")

(deftest format-logical-block-iteration-rest.5
  (format nil "~<~@{<~A,~0^~A>~}~:>~A" '(10 20 30 40) 50)
  "<10,50")

(deftest format-logical-block-iteration-rest.6
  (format nil "~@<~@{<~A,~0^~A>~}~:>" 10 20 30 40 50)
  "<10,")

(deftest format-logical-block-iteration-rest.7
  (with-empty-dispatch
    (format nil "~<~@{<~A,~A>~}~:>~A" '(10 20 30 40) 50))
  "<10,20><30,40>50")

(deftest format-logical-block-iteration-rest.8
  (with-empty-dispatch
    (format nil "~@<~@{<~A,~A>~}~:>" 10 20 30 40))
  "<10,20><30,40>")

(deftest format-logical-block-iteration-rest.9
  (with-empty-dispatch
    (format nil "~<~2@{<~A,~A>~}~A~:>~A" '(10 20 30 40 50 60) 70))
  "<10,20><30,40>5070")

(deftest format-logical-block-iteration-rest.10
  (with-empty-dispatch
    (format nil "~@<~2@{<~A,~A>~}~A~:>" 10 20 30 40 50 60))
  "<10,20><30,40>50")

(deftest format-logical-block-iteration-rest.11
  (with-empty-dispatch
    (format nil "~<~@{<~A,~0^~A>~}~:>~A" '(10 20 30 40) 50))
  "<10,50")

(deftest format-logical-block-iteration-rest.12
  (with-empty-dispatch
    (format nil "~@<~@{<~A,~0^~A>~}~:>" 10 20 30 40 50))
  "<10,")

(deftest format-logical-block-iteration-listargs.1
  (format nil "~<~:{<~A,~A>~}~A~:>~A" '(((10 20 30) (40 50) (60 70)) 80) 90)
  "<10,20><40,50><60,70>8090")

(deftest format-logical-block-iteration-listargs.2
  (format nil "~@<~:{<~A,~A>~}~A~:>" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-logical-block-iteration-listargs.3
  (format nil "~<~2:{<~A,~A>~}~A~:>~A" '(((10 20 30) (40 50) (60 70)) 80) 90)
  "<10,20><40,50>8090")

(deftest format-logical-block-iteration-listargs.4
  (format nil "~@<~2:{<~A,~A>~}~A~:>" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50>80")

(deftest format-logical-block-iteration-listargs.5
  (format nil "~<~:{<~A,~0^~A>~}~A~:>~A" '(((10 20 30) (40 50) (60 70)) 80) 90)
  "<10,<40,<60,8090")

(deftest format-logical-block-iteration-listargs.6
  (format nil "~@<~:{<~A,~0^~A>~}~A~:>" '((10 20 30) (40 50) (60 70)) 80)
  "<10,<40,<60,80")

(deftest format-logical-block-iteration-listargs.7
  (with-empty-dispatch
    (format nil "~<~:{<~A,~A>~}~A~:>~A" '(((10 20 30) (40 50) (60 70)) 80) 90))
  "<10,20><40,50><60,70>8090")

(deftest format-logical-block-iteration-listargs.8
  (with-empty-dispatch
    (format nil "~@<~:{<~A,~A>~}~A~:>" '((10 20 30) (40 50) (60 70)) 80))
  "<10,20><40,50><60,70>80")

(deftest format-logical-block-iteration-listargs.9
  (with-empty-dispatch
    (format nil "~<~2:{<~A,~A>~}~A~:>~A" '(((10 20 30) (40 50) (60 70)) 80) 90))
  "<10,20><40,50>8090")

(deftest format-logical-block-iteration-listargs.10
  (with-empty-dispatch
    (format nil "~@<~2:{<~A,~A>~}~A~:>" '((10 20 30) (40 50) (60 70)) 80))
  "<10,20><40,50>80")

(deftest format-logical-block-iteration-listargs.11
  (with-empty-dispatch
    (format nil "~<~:{<~A,~0^~A>~}~A~:>~A" '(((10 20 30) (40 50) (60 70)) 80) 90))
  "<10,<40,<60,8090")

(deftest format-logical-block-iteration-listargs.12
  (with-empty-dispatch
    (format nil "~@<~:{<~A,~0^~A>~}~A~:>" '((10 20 30) (40 50) (60 70)) 80))
  "<10,<40,<60,80")

(deftest format-logical-block-iteration-restargs.1
  (format nil "~<~:@{<~A,~A>~}~:>~A" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-logical-block-iteration-restargs.2
  (format nil "~@<~:@{<~A,~A>~}~:>" '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60,70>")

(deftest format-logical-block-iteration-restargs.3
  (format nil "~<~2:@{<~A,~A>~}~A~:>~A" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50>(60 70)80")

(deftest format-logical-block-iteration-restargs.4
  (format nil "~@<~2:@{<~A,~A>~}~A~:>" '(10 20 30) '(40 50) '(60 70) 80)
  "<10,20><40,50>(60 70)")

(deftest format-logical-block-iteration-restargs.5
  (format nil "~<~:@{<~A,~0^~A>~}~:>~A" '((10 20 30) (40 50) (60 70)) 80)
  "<10,<40,<60,80")

(deftest format-logical-block-iteration-restargs.6
  (format nil "~@<~:@{<~A,~0^~A>~}~:>" '(10 20 30) '(40 50) '(60 70))
  "<10,<40,<60,")

(deftest format-logical-block-iteration-restargs.7
  (with-empty-dispatch
    (format nil "~<~:@{<~A,~A>~}~:>~A" '((10 20 30) (40 50) (60 70)) 80))
  "<10,20><40,50><60,70>80")

(deftest format-logical-block-iteration-restargs.8
  (with-empty-dispatch
    (format nil "~@<~:@{<~A,~A>~}~:>" '(10 20 30) '(40 50) '(60 70)))
  "<10,20><40,50><60,70>")

(deftest format-logical-block-iteration-restargs.9
  (with-empty-dispatch
    (format nil "~<~2:@{<~A,~A>~}~A~:>~A" '((10 20 30) (40 50) (60 70)) 80))
  "<10,20><40,50>(60 70)80")

(deftest format-logical-block-iteration-restargs.10
  (with-empty-dispatch
    (format nil "~@<~2:@{<~A,~A>~}~A~:>" '(10 20 30) '(40 50) '(60 70) 80))
  "<10,20><40,50>(60 70)")

(deftest format-logical-block-iteration-restargs.11
  (with-empty-dispatch
    (format nil "~<~:@{<~A,~0^~A>~}~:>~A" '((10 20 30) (40 50) (60 70)) 80))
  "<10,<40,<60,80")

(deftest format-logical-block-iteration-restargs.12
  (with-empty-dispatch
    (format nil "~@<~:@{<~A,~0^~A>~}~:>" '(10 20 30) '(40 50) '(60 70)))
  "<10,<40,<60,")

(deftest format-logical-block-iteration2-list2.1
  (format nil "~<~{~}~A~:>~A" '("<~S,~S>" (10 20 30 40) 50) 60)
  "<10,20><30,40>5060")

(deftest format-logical-block-iteration2-list2.2
  (format nil "~<~{~}~A~:>~A" '(#.(formatter "<~S,~S>") (10 20 30 40) 50) 60)
  "<10,20><30,40>5060")

(deftest format-logical-block-iteration2-list2.3
  (format nil "~@<~{~}~A~:>" "<~S,~S>" '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-logical-block-iteration2-list2.4
  (format nil "~@<~{~}~A~:>" (formatter "<~S,~S>") '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-logical-block-iteration2-list2.5
  (format nil "~<~{~}~A~:>~A" '("<~A,~0^~A>" (10 20 30 40) 50) 60)
  "<10,5060")

(deftest format-logical-block-iteration2-list2.6
  (format nil "~<~{~}~A~:>~A" '(#.(formatter "<~A,~0^~A>") (10 20 30 40) 50) 60)
  "<10,<20,<30,<40,5060")

(deftest format-logical-block-iteration2-list2.7
  (with-empty-dispatch
    (format nil "~<~{~}~A~:>~A" '("<~S,~S>" (10 20 30 40) 50) 60))
  "<10,20><30,40>5060")

(deftest format-logical-block-iteration2-list2.8
  (with-empty-dispatch
    (format nil "~<~{~}~A~:>~A" '(#.(formatter "<~S,~S>") (10 20 30 40) 50) 60))
  "<10,20><30,40>5060")

(deftest format-logical-block-iteration2-list2.9
  (with-empty-dispatch
    (format nil "~@<~{~}~A~:>" "<~S,~S>" '(10 20 30 40) 50))
  "<10,20><30,40>50")

(deftest format-logical-block-iteration2-list2.10
  (with-empty-dispatch
    (format nil "~@<~{~}~A~:>" (formatter "<~S,~S>") '(10 20 30 40) 50))
  "<10,20><30,40>50")

(deftest format-logical-block-iteration2-list2.11
  (with-empty-dispatch
    (format nil "~<~{~}~A~:>~A" '("<~A,~0^~A>" (10 20 30 40) 50) 60))
  "<10,5060")

(deftest format-logical-block-iteration2-list2.12
  (with-empty-dispatch
    (format nil "~<~{~}~A~:>~A" '(#.(formatter "<~A,~0^~A>") (10 20 30 40) 50) 60))
  "<10,<20,<30,<40,5060")

(deftest format-logical-block-iteration2-rest.1
  (format nil "~<~@{~}~:>~A" '("<~A,~A>" 10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-logical-block-iteration2-rest.2
  (format nil "~<~@{~}~:>~A" '(#.(formatter "<~A,~A>") 10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-logical-block-iteration2-rest.3
  (format nil "~@<~@{~}~:>" "<~A,~A>" 10 20 30 40)
  "<10,20><30,40>")

(deftest format-logical-block-iteration2-rest.4
  (format nil "~@<~@{~}~:>" (formatter "<~A,~A>") 10 20 30 40)
  "<10,20><30,40>")

(deftest format-logical-block-iteration2-rest.5
  (format nil "~<~@{~}~:>~A" '("<~A~0^,~A>" 10 20 30 40) 50)
  "<1050")

(deftest format-logical-block-iteration2-rest.6
  (format nil "~<~@{~}~:>~A" '(#.(formatter "<~A~0^,~A>") 10 20 30 40) 50)
  "<10<20<30<4050")

(deftest format-logical-block-iteration2-rest.7
  (format nil "~@<~@{~}~:>" "<~A~0^,~A>" 10 20 30 40)
  "<10")

(deftest format-logical-block-iteration2-rest.8
  (format nil "~@<~@{~}~:>" (formatter "<~A~0^,~A>") 10 20 30 40)
  "<10<20<30<40")

(deftest format-logical-block-iteration2-rest.9
  (with-empty-dispatch
    (format nil "~<~@{~}~:>~A" '("<~A,~A>" 10 20 30 40) 50))
  "<10,20><30,40>50")

(deftest format-logical-block-iteration2-rest.10
  (with-empty-dispatch
    (format nil "~<~@{~}~:>~A" '(#.(formatter "<~A,~A>") 10 20 30 40) 50))
  "<10,20><30,40>50")

(deftest format-logical-block-iteration2-rest.11
  (with-empty-dispatch
    (format nil "~@<~@{~}~:>" "<~A,~A>" 10 20 30 40))
  "<10,20><30,40>")

(deftest format-logical-block-iteration2-rest.12
  (with-empty-dispatch
    (format nil "~@<~@{~}~:>" (formatter "<~A,~A>") 10 20 30 40))
  "<10,20><30,40>")

(deftest format-logical-block-iteration2-rest.13
  (with-empty-dispatch
    (format nil "~<~@{~}~:>~A" '("<~A~0^,~A>" 10 20 30 40) 50))
  "<1050")

(deftest format-logical-block-iteration2-rest.14
  (with-empty-dispatch
    (format nil "~<~@{~}~:>~A" '(#.(formatter "<~A~0^,~A>") 10 20 30 40) 50))
  "<10<20<30<4050")

(deftest format-logical-block-iteration2-rest.15
  (with-empty-dispatch
    (format nil "~@<~@{~}~:>" "<~A~0^,~A>" 10 20 30 40))
  "<10")

(deftest format-logical-block-iteration2-rest.16
  (with-empty-dispatch
    (format nil "~@<~@{~}~:>" (formatter "<~A~0^,~A>") 10 20 30 40))
  "<10<20<30<40")

(deftest format-logical-block-iteration2-listargs.1
  (format nil "~<~:{~}~A~:>~A" '("<~A,~A>" ((10 20 30) (40 50) (60 70)) 80) 90)
  "<10,20><40,50><60,70>8090")

(deftest format-logical-block-iteration2-listargs.2
  (format nil "~<~:{~}~A~:>~A"
          '(#.(formatter "<~A,~A>") ((10 20 30) (40 50) (60 70)) 80) 90)
  "<10,20><40,50><60,70>8090")

(deftest format-logical-block-iteration2-listargs.3
  (format nil "~@<~:{~}~A~:>" "<~A,~A>" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-logical-block-iteration2-listargs.4
  (format nil "~@<~:{~}~A~:>" (formatter "<~A,~A>") '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-logical-block-iteration2-listargs.5
  (format nil "~<~:{~}~A~:>~A" '("<~A~0^,~A>" ((10 20 30) (40 50) (60 70)) 80) 90)
  "<10<40<608090")

(deftest format-logical-block-iteration2-listargs.6
  (format nil "~<~:{~}~A~:>~A"
          '(#.(formatter "<~A~0^,~A>") ((10 20 30) (40 50) (60 70)) 80) 90)
  "<10<40<608090")

(deftest format-logical-block-iteration2-listargs.7
  (format nil "~@<~:{~}~A~:>" "<~A~0^,~A>" '((10 20 30) (40 50) (60 70)) 80)
  "<10<40<6080")

(deftest format-logical-block-iteration2-listargs.8
  (format nil "~@<~:{~}~A~:>"
          (formatter "<~A~0^,~A>") '((10 20 30) (40 50) (60 70)) 80)
  "<10<40<6080")

(deftest format-logical-block-iteration2-listargs.9
  (with-empty-dispatch
    (format nil "~<~:{~}~A~:>~A" '("<~A,~A>" ((10 20 30) (40 50) (60 70)) 80) 90))
  "<10,20><40,50><60,70>8090")

(deftest format-logical-block-iteration2-listargs.10
  (with-empty-dispatch
    (format nil "~<~:{~}~A~:>~A"
            '(#.(formatter "<~A,~A>") ((10 20 30) (40 50) (60 70)) 80) 90))
  "<10,20><40,50><60,70>8090")

(deftest format-logical-block-iteration2-listargs.11
  (with-empty-dispatch
    (format nil "~@<~:{~}~A~:>" "<~A,~A>" '((10 20 30) (40 50) (60 70)) 80))
  "<10,20><40,50><60,70>80")

(deftest format-logical-block-iteration2-listargs.12
  (with-empty-dispatch
    (format nil "~@<~:{~}~A~:>" (formatter "<~A,~A>") '((10 20 30) (40 50) (60 70)) 80))
  "<10,20><40,50><60,70>80")

(deftest format-logical-block-iteration2-listargs.13
  (with-empty-dispatch
    (format nil "~<~:{~}~A~:>~A" '("<~A~0^,~A>" ((10 20 30) (40 50) (60 70)) 80) 90))
  "<10<40<608090")

(deftest format-logical-block-iteration2-listargs.14
  (with-empty-dispatch
    (format nil "~<~:{~}~A~:>~A"
            '(#.(formatter "<~A~0^,~A>") ((10 20 30) (40 50) (60 70)) 80) 90))
  "<10<40<608090")

(deftest format-logical-block-iteration2-listargs.15
  (with-empty-dispatch
    (format nil "~@<~:{~}~A~:>" "<~A~0^,~A>" '((10 20 30) (40 50) (60 70)) 80))
  "<10<40<6080")

(deftest format-logical-block-iteration2-listargs.16
  (with-empty-dispatch
    (format nil "~@<~:{~}~A~:>"
            (formatter "<~A~0^,~A>") '((10 20 30) (40 50) (60 70)) 80))
  "<10<40<6080")

(deftest format-logical-block-iteration2-restargs.1
  (format nil "~<~:@{~}~:>~A" '("<~A,~A>" (10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-logical-block-iteration2-restargs.2
  (format nil "~<~:@{~}~:>~A" '(#.(formatter "<~A,~A>") (10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-logical-block-iteration2-restargs.3
  (format nil "~@<~:@{~}~:>" "<~A,~A>" '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60,70>")

(deftest format-logical-block-iteration2-restargs.4
  (format nil "~@<~:@{~}~:>" (formatter "<~A,~A>") '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60,70>")

(deftest format-logical-block-iteration2-restargs.5
  (format nil "~<~:@{~}~:>~A" '("<~A~0^,~A>" (10 20 30) (40 50) (60 70)) 80)
  "<10<40<6080")

(deftest format-logical-block-iteration2-restargs.6
  (format nil "~<~:@{~}~:>~A"
          '(#.(formatter "<~A~0^,~A>") (10 20 30) (40 50) (60 70)) 80)
  "<10<40<6080")

(deftest format-logical-block-iteration2-restargs.7
  (format nil "~@<~:@{~}~:>" "<~A~0^,~A>" '(10 20 30) '(40 50) '(60 70))
  "<10<40<60")

(deftest format-logical-block-iteration2-restargs.8
  (format nil "~@<~:@{~}~:>" (formatter "<~A~0^,~A>") '(10 20 30) '(40 50) '(60 70))
  "<10<40<60")

(deftest format-logical-block-iteration2-restargs.9
  (with-empty-dispatch
    (format nil "~<~:@{~}~:>~A" '("<~A,~A>" (10 20 30) (40 50) (60 70)) 80))
  "<10,20><40,50><60,70>80")

(deftest format-logical-block-iteration2-restargs.10
  (with-empty-dispatch
    (format nil "~<~:@{~}~:>~A" '(#.(formatter "<~A,~A>") (10 20 30) (40 50) (60 70)) 80))
  "<10,20><40,50><60,70>80")

(deftest format-logical-block-iteration2-restargs.11
  (with-empty-dispatch
    (format nil "~@<~:@{~}~:>" "<~A,~A>" '(10 20 30) '(40 50) '(60 70)))
  "<10,20><40,50><60,70>")

(deftest format-logical-block-iteration2-restargs.12
  (with-empty-dispatch
    (format nil "~@<~:@{~}~:>" (formatter "<~A,~A>") '(10 20 30) '(40 50) '(60 70)))
  "<10,20><40,50><60,70>")

(deftest format-logical-block-iteration2-restargs.13
  (with-empty-dispatch
    (format nil "~<~:@{~}~:>~A" '("<~A~0^,~A>" (10 20 30) (40 50) (60 70)) 80))
  "<10<40<6080")

(deftest format-logical-block-iteration2-restargs.14
  (with-empty-dispatch
    (format nil "~<~:@{~}~:>~A"
            '(#.(formatter "<~A~0^,~A>") (10 20 30) (40 50) (60 70)) 80))
  "<10<40<6080")

(deftest format-logical-block-iteration2-restargs.15
  (with-empty-dispatch
    (format nil "~@<~:@{~}~:>" "<~A~0^,~A>" '(10 20 30) '(40 50) '(60 70)))
  "<10<40<60")

(deftest format-logical-block-iteration2-restargs.16
  (with-empty-dispatch
    (format nil "~@<~:@{~}~:>" (formatter "<~A~0^,~A>") '(10 20 30) '(40 50) '(60 70)))
  "<10<40<60")


;;
;;  Justification in Logical Block
;;
(deftest format-logical-block-justification.1
  (format nil "~<AAA~20@:<~A~>BBB~:>" '("Hello"))
  "AAA       Hello        BBB")

(deftest format-logical-block-justification.2
  (format nil "~<AAA~20@:<~A~;~0^ZZZ~>BBB~:>" '("Hello"))
  "AAA       Hello        BBB")

(deftest format-logical-block-justification.3
  (format nil "~@<AAA~20@:<~A~>BBB~:>" "Hello")
  "AAA       Hello        BBB")

(deftest format-logical-block-justification.4
  (format nil "~@<AAA~20@:<~A~;~0^ZZZ~>BBB~:>" "Hello")
  "AAA       Hello        BBB")


;;
;;  Logical Block :fill
;;
(deftest format-logical-block-fill.1
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-right-margin* 20)
        (*print-miser-width* nil))
    (format nil "~<AAA BBB ~A ~A ~A DDD EEE~:@>" '(10 20 30)))
  #.(mkstr "AAA BBB 10 20 30" #\newline "DDD EEE"))

(deftest format-logical-block-fill.2
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-right-margin* 10)
        (*print-miser-width* 100))
    (format nil "~<AAA BBB ~A ~A ~A DDD EEE~:@>" '(10 20 30)))
  #.(mkstr
      "AAA" #\newline
      "BBB" #\newline
      "10" #\newline
      "20" #\newline
      "30" #\newline
      "DDD" #\newline
      "EEE"))

(deftest format-logical-block-fill.3
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-right-margin* 15)
        (*print-miser-width* nil))
    (format nil "~@<AAA BBB ~A ~A ~A DDD EEE~:@>" 10 20 30))
  #.(mkstr "AAA BBB 10 20" #\newline "30 DDD EEE"))

(deftest format-logical-block-fill.4
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-right-margin* 10)
        (*print-miser-width* 100))
    (format nil "~@:<AAA BBB ~A ~A ~A DDD EEE~:@>" 10 20 30))
  #.(mkstr
      "(AAA" #\newline
      " BBB" #\newline
      " 10" #\newline
      " 20" #\newline
      " 30" #\newline
      " DDD" #\newline
      " EEE)"))

