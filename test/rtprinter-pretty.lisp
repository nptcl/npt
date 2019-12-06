;;
;;  ANSI COMMON LISP: 22. Printer
;;

;;
;;  pprint-logical-block
;;
(deftest pprint-logical-block.1
  (pprint-logical-block (nil nil))
  nil)

(deftest pprint-logical-block.2
  (pprint-logical-block (t nil))
  nil)

(deftest pprint-logical-block.3
  (with-output-to-string (stream)
    (pprint-logical-block (stream nil)))
  "")

(deftest pprint-logical-block.4
  (with-output-to-string (*standard-output*)
    (pprint-logical-block (nil 'aaa)
      :hello))
  "AAA")

(deftest pprint-logical-block.5
  (let (result)
    (values
      (with-output-to-string (*terminal-io*)
        (setq result (pprint-logical-block (t 'aaa)
                       :hello)))
      result))
  "AAA" nil)

(deftest pprint-logical-block.6
  (with-output-to-string (stream)
    (pprint-logical-block (stream 'aaa)
      :hello))
  "AAA")

(deftest pprint-logical-block.7
  (with-output-to-string (stream)
    (pprint-logical-block (stream nil)
      (prin1 :hello stream)))
  ":HELLO")

(deftest pprint-logical-block.8
  (let ((*print-circle* t))
    (with-output-to-string (stream)
      (pprint-logical-block (stream nil)
        (prin1 :hello stream))))
  ":HELLO")

(deftest-error pprint-logical-block.9
  (let (check)
    (with-output-to-string (stream)
      (pprint-logical-block (stream nil)
        (setq check stream)))
    (prin1 :hello check)))

(deftest pprint-logical-block.10
  (with-output-to-string (stream)
    (pprint-logical-block (stream nil)
      (princ "AAA" stream)
      (pprint-logical-block (stream nil)
        (princ "BBB" stream))
      (princ "CCC" stream)))
  "AAABBBCCC")

(deftest pprint-logical-block-prefix.1
  (pprint-logical-block-output
    (100 nil :prefix "HELLO")
    (princ "ABC"))
  "HELLOABC")

(deftest pprint-logical-block-prefix.2
  (pprint-logical-block-output
    (100 nil :prefix "(" :suffix ")")
    (princ "ABC"))
  "(ABC)")

(deftest pprint-logical-block-prefix.4
  (pprint-logical-block-output
    (100 nil :per-line-prefix ";;; ")
    (princ "ABC"))
  ";;; ABC")

(deftest-error pprint-logical-block-prefix.5
  (eval '(pprint-logical-block-output
           (100 nil :prefix "ERROR" :per-line-prefix ";;; ")
           (princ "ABC"))))

(deftest pprint-logical-block-prefix.6
  (pprint-logical-block-output
    (100 nil :per-line-prefix "AAA")
    (pprint-logical-block (nil nil :per-line-prefix "BBB")
      (pprint-logical-block (nil nil :per-line-prefix "CCC")
        (princ "DDD"))))
  "AAABBBCCCDDD")


;;
;;  pprint-pop
;;
(deftest pprint-pop.1
  (let ((result 'default))
    (values
      (with-output-to-string (stream)
        (pprint-logical-block (stream nil)
          (setq result (pprint-pop))))
      result))
  "" nil)

(deftest pprint-pop.2
  (let ((result 'default))
    (values
      (with-output-to-string (stream)
        (pprint-logical-block (stream '(a))
          (setq result (pprint-pop))))
      result))
  "" a)

(deftest pprint-pop.3
  (let ((x 'default)
        (y 'default))
    (values
      (with-output-to-string (stream)
        (pprint-logical-block (stream '(a b c))
          (setq x (pprint-pop))
          (setq y (pprint-pop))))
      x y))
  "" a b)

(deftest pprint-pop.4
  (let ((x 'default)
        (y 'default))
    (values
      (with-output-to-string (stream)
        (pprint-logical-block (stream '(a))
          (setq x (pprint-pop))
          (setq y (pprint-pop))))
      x y))
  "" a nil)

(deftest pprint-pop.5
  (let ((x 'default))
    (values
      (with-output-to-string (stream)
        (pprint-logical-block (stream 'aaa)
          (setq x (pprint-pop))))
      x))
  "AAA" default)

(deftest pprint-pop.6
  (let ((x 'default)
        (y 'default)
        (z 'default))
    (values
      (with-output-to-string (stream)
        (pprint-logical-block (stream '(aaa . bbb))
          (setq x (pprint-pop))
          (setq y (pprint-pop))
          (setq z (pprint-pop))))
      x y z))
  ". BBB" aaa default default)

(deftest-error pprint-pop.7
  (let (call)
    (with-output-to-string (stream)
      (pprint-logical-block (stream '(aaa . bbb))
        (setq call (lambda () (pprint-pop))))
      (funcall call))))

(deftest pprint-pop.8
  (let ((x 'default)
        (y 'default)
        (z 'default)
        (*print-length* 2))
    (values
      (with-output-to-string (stream)
        (pprint-logical-block (stream '(aa bb cc dd ee))
          (setq x (pprint-pop))
          (setq y (pprint-pop))
          (setq z (pprint-pop))))
      x y z))
  "..." aa bb default)

(deftest pprint-pop-output.1
  (with-output-to-string (*terminal-io*)
    (pprint-logical-block (t '(aaa . bbb))
      (pprint-pop)
      (pprint-pop)))
  ". BBB")

(deftest pprint-pop-output.2
  (with-output-to-string (*standard-output*)
    (pprint-logical-block (nil '(aaa . bbb))
      (pprint-pop)
      (pprint-pop)))
  ". BBB")

(deftest pprint-exit-if-list-exhausted.1
  (with-output-to-string (stream)
    (pprint-logical-block (stream nil)
      (pprint-exit-if-list-exhausted)
      (princ "HELLO" stream)))
  "")

(deftest pprint-exit-if-list-exhausted.2
  (with-output-to-string (stream)
    (pprint-logical-block (stream 'aaa)
      (pprint-exit-if-list-exhausted)
      (princ "HELLO" stream)))
  "AAA")

(deftest pprint-exit-if-list-exhausted.3
  (with-output-to-string (stream)
    (pprint-logical-block (stream '(a b c))
      (pprint-exit-if-list-exhausted)
      (princ "HELLO" stream)))
  "HELLO")

(deftest-error pprint-exit-if-list-exhausted.4
  (let (call)
    (with-output-to-string (stream)
      (pprint-logical-block (stream '(a b c))
        (setq call (lambda () (pprint-exit-if-list-exhausted))))
      (funcall call))))


;;
;;  pprint-newline
;;
(deftest pprint-newline.1
  (with-output-to-string (stream)
    (pprint-newline :linear stream)
    (pprint-newline :fill stream)
    (pprint-newline :miser stream)
    (pprint-newline :mandatory stream))
  "")

(deftest pprint-newline.2
  (pprint-newline :linear)
  nil)


;;
;;  :linear
;;
(deftest pprint-newline-linear.1
  (pprint-logical-block-output
    (100 nil)
    (princ "AAA")
    (pprint-newline :linear)
    (princ "BBB"))
  "AAABBB")

(deftest pprint-newline-linear.2
  (pprint-logical-block-output
    (100 100)
    (princ "AAA") (pprint-newline :linear)
    (princ "BBB") (pprint-newline :linear)
    (princ "CCC") (pprint-newline :linear)
    (princ "DDD") (pprint-newline :linear))
  "AAABBBCCCDDD")

(deftest pprint-newline-linear.3
  (pprint-logical-block-output
    (12 nil)
    (princ "AAA") (pprint-newline :linear)
    (princ "BBB") (pprint-newline :linear)
    (princ "CCC") (pprint-newline :linear)
    (princ "DDD") (pprint-newline :linear))
  "AAABBBCCCDDD")

(deftest pprint-newline-linear.4
  (pprint-logical-block-output
    (11 nil)
    (princ "AAA") (pprint-newline :linear)
    (princ "BBB") (pprint-newline :linear)
    (princ "CCC") (pprint-newline :linear)
    (princ "DDD") (pprint-newline :linear))
  #.(mkstr "AAA" #\newline
           "BBB" #\newline
           "CCC" #\newline
           "DDD" #\newline))


;;
;;  miser
;;
(deftest pprint-newline-miser.1
  (pprint-logical-block-output
    (100 nil)
    (princ "AAA") (pprint-newline :miser)
    (princ "BBB") (pprint-newline :miser)
    (princ "CCC") (pprint-newline :miser))
  "AAABBBCCC")

(deftest pprint-newline-miser.2
  (pprint-logical-block-output
    (5 nil)
    (princ "AAA") (pprint-newline :miser)
    (princ "BBB") (pprint-newline :miser)
    (princ "CCC") (pprint-newline :miser))
  "AAABBBCCC")

(deftest pprint-newline-miser.3
  (pprint-logical-block-output
    (5 5)
    (princ "AAA") (pprint-newline :miser)
    (princ "BBB") (pprint-newline :miser)
    (princ "CCC") (pprint-newline :miser))
  #.(mkstr "AAA" #\newline
           "BBB" #\newline
           "CCC" #\newline))

(deftest pprint-newline-miser.4
  (pprint-logical-block-output
    (10 100)
    (princ "AAA") (pprint-newline :miser)
    (princ "BBB") (pprint-newline :miser)
    (princ "CCC") (pprint-newline :miser))
  "AAABBBCCC")

(deftest pprint-newline-miser.5
  (pprint-logical-block-output
    (5 4)
    (princ "AAA") (pprint-newline :miser)
    (princ "BBB") (pprint-newline :miser)
    (princ "CCC") (pprint-newline :miser))
  "AAABBBCCC")

(deftest pprint-newline-miser.6
  (pprint-logical-block-output
    (5 100)
    (princ "AAA") (pprint-newline :linear)
    (princ "BBB") (pprint-newline :linear)
    (princ "CCC") (pprint-newline :miser)
    (princ "DDD") (pprint-newline :miser))
  #.(mkstr "AAA" #\newline
           "BBB" #\newline
           "CCC" #\newline
           "DDD" #\newline))

(deftest pprint-newline-miser.7
  (pprint-logical-block-output
    (10 nil)
    (princ "AAA") (pprint-newline :linear)
    (princ "BBB") (pprint-newline :linear)
    (princ "CCC") (pprint-newline :miser)
    (princ "DDD") (pprint-newline :miser))
  #.(mkstr "AAA" #\newline
           "BBB" #\newline
           "CCCDDD"))


;;
;;  fill
;;
(deftest pprint-newline-fill.1
  (pprint-logical-block-output
    (100 nil)
    (princ "AAA") (pprint-newline :fill)
    (princ "BBB") (pprint-newline :fill)
    (princ "CCC") (pprint-newline :fill))
  "AAABBBCCC")

(deftest pprint-newline-fill.2
  (pprint-logical-block-output
    (10 nil)
    (princ "AAA") (pprint-newline :fill)
    (princ "BBB") (pprint-newline :fill)
    (princ "CCC") (pprint-newline :fill)
    (princ "DDD") (pprint-newline :fill)
    (princ "EEE") (pprint-newline :fill))
  #.(mkstr "AAABBBCCC" #\newline "DDDEEE"))

(deftest pprint-newline-fill.3
  (pprint-logical-block-output
    (5 100)
    (princ "AAA") (pprint-newline :fill)
    (princ "BBB") (pprint-newline :fill)
    (princ "CCC") (pprint-newline :fill))
  #.(mkstr "AAA" #\newline
           "BBB" #\newline
           "CCC" #\newline))

(deftest pprint-newline-fill.4
  (pprint-logical-block-output
    (10 nil)
    (princ "AAA") (pprint-newline :linear)
    (princ "BBB") (pprint-newline :fill)
    (princ "CCC") (pprint-newline :fill)
    (princ "DDD") (pprint-newline :fill)
    (princ "EEE") (pprint-newline :fill))
  #.(mkstr "AAA" #\newline "BBBCCCDDD" #\newline "EEE"))


;;
;;  mandatory
;;
(deftest pprint-newline-mandatory.1
  (pprint-logical-block-output
    (100 nil)
    (princ "AAA") (pprint-newline :mandatory)
    (princ "BBB") (pprint-newline :mandatory)
    (princ "CCC") (pprint-newline :mandatory))
  #.(mkstr "AAA" #\newline "BBB" #\newline "CCC" #\newline))

(deftest pprint-newline-mandatory.2
  (pprint-logical-block-output
    (100 nil)
    (princ "AAA") (pprint-newline :linear)
    (princ "BBB") (pprint-newline :linear)
    (princ "CCC") (pprint-newline :mandatory))
  #.(mkstr "AAA" #\newline "BBB" #\newline "CCC" #\newline))

(deftest pprint-newline-mandatory.3
  (pprint-logical-block-output
    (100 100)
    (princ "AAA") (pprint-newline :miser)
    (princ "BBB") (pprint-newline :miser)
    (princ "CCC") (pprint-newline :mandatory))
  #.(mkstr "AAA" #\newline "BBB" #\newline "CCC" #\newline))

(deftest pprint-newline-mandatory.4
  (pprint-logical-block-output
    (100 2)
    (princ "AAA") (pprint-newline :miser)
    (princ "BBB") (pprint-newline :miser)
    (princ "CCC") (pprint-newline :mandatory))
  #.(mkstr "AAABBBCCC" #\newline))

(deftest pprint-newline-mandatory.5
  (pprint-logical-block-output
    (100 nil)
    (princ "AAA") (pprint-newline :linear)
    (princ "BBB") (pprint-newline :linear)
    (pprint-logical-block (nil nil)
      (princ "CCC") (pprint-newline :linear)
      (princ "DDD") (pprint-newline :linear)
      (pprint-logical-block (nil nil)
        (princ "EEE") (pprint-newline :linear)
        (princ "FFF") (pprint-newline :mandatory))))
  #.(mkstr "AAA" #\newline "BBB" #\newline
           "CCC" #\newline "DDD" #\newline
           "EEE" #\newline "FFF" #\newline))


;;
;;  terpri
;;
(deftest pprint-newline-terpri.1
  (pprint-logical-block-output
    (100 nil)
    (princ "AAA") (terpri)
    (princ "BBB") (terpri)
    (princ "CCC") (terpri))
  #.(mkstr "AAA" #\newline "BBB" #\newline "CCC" #\newline))

(deftest pprint-newline-terpri.2
  (pprint-logical-block-output
    (100 nil)
    (princ "AAA") (pprint-newline :linear)
    (princ "BBB") (pprint-newline :linear)
    (princ "CCC") (terpri))
  #.(mkstr "AAA" #\newline "BBB" #\newline "CCC" #\newline))

(deftest pprint-newline-terpri.3
  (pprint-logical-block-output
    (100 100)
    (princ "AAA") (pprint-newline :miser)
    (princ "BBB") (pprint-newline :miser)
    (princ "CCC") (terpri))
  #.(mkstr "AAA" #\newline "BBB" #\newline "CCC" #\newline))

(deftest pprint-newline-terpri.4
  (pprint-logical-block-output
    (100 2)
    (princ "AAA") (pprint-newline :miser)
    (princ "BBB") (pprint-newline :miser)
    (princ "CCC") (terpri))
  #.(mkstr "AAABBBCCC" #\newline))

(deftest pprint-newline-terpri.5
  (pprint-logical-block-output
    (100 nil)
    (princ "AAA") (pprint-newline :linear)
    (princ "BBB") (pprint-newline :linear)
    (pprint-logical-block (nil nil)
      (princ "CCC") (pprint-newline :linear)
      (princ "DDD") (pprint-newline :linear)
      (pprint-logical-block (nil nil)
        (princ "EEE") (pprint-newline :linear)
        (princ "FFF") (terpri))))
  #.(mkstr "AAA" #\newline "BBB" #\newline
           "CCC" #\newline "DDD" #\newline
           "EEE" #\newline "FFF" #\newline))


;;
;;  indent
;;
(deftest pprint-indent.1
  (with-output-to-string (*standard-output*)
    (princ "AAA")
    (let ((*print-pretty* t)
          (*print-right-margin* 100)
          (*print-miser-width* nil))
      (pprint-logical-block (nil nil)
        (princ "BBB")
        (pprint-newline :mandatory)
        (princ "CCC"))))
  #.(mkstr "AAABBB" #\newline
           "   CCC"))

(deftest pprint-indent.2
  (with-output-to-string (*standard-output*)
    (princ "AAA")
    (let ((*print-pretty* t)
          (*print-right-margin* 100)
          (*print-miser-width* nil))
      (pprint-logical-block (nil nil :prefix "(")
        (princ "BBB")
        (pprint-newline :mandatory)
        (princ "CCC"))))
  #.(mkstr "AAA(BBB" #\newline
           "    CCC"))

(deftest pprint-indent.3
  (with-output-to-string (*standard-output*)
    (princ "AAA")
    (let ((*print-pretty* t)
          (*print-right-margin* 100)
          (*print-miser-width* nil))
      (pprint-logical-block (nil nil :per-line-prefix ";;;")
        (princ "BBB")
        (pprint-newline :mandatory)
        (princ "CCC"))))
  #.(mkstr "AAA;;;BBB" #\newline
           "   ;;;CCC"))

(deftest pprint-indent.4
  (with-output-to-string (*standard-output*)
    (princ "AAA")
    (let ((*print-pretty* t)
          (*print-right-margin* 100)
          (*print-miser-width* nil))
      (pprint-logical-block (nil nil :per-line-prefix ";;;")
        (princ "BBB")
        (pprint-newline :mandatory)
        (princ "CCC"))))
  #.(mkstr "AAA;;;BBB" #\newline
           "   ;;;CCC"))

(deftest pprint-indent.5
  (with-output-to-string (*standard-output*)
    (princ "AAA")
    (let ((*print-pretty* t)
          (*print-right-margin* 100)
          (*print-miser-width* nil))
      (pprint-logical-block (nil nil :per-line-prefix ";;;")
        (princ "BBB")
        (pprint-indent :block 0)
        (pprint-newline :mandatory)
        (princ "CCC"))))
  #.(mkstr "AAA;;;BBB" #\newline
           "   ;;;CCC"))

(deftest pprint-indent.6
  (with-output-to-string (*standard-output*)
    (princ "AAA")
    (let ((*print-pretty* t)
          (*print-right-margin* 100)
          (*print-miser-width* nil))
      (pprint-logical-block (nil nil :per-line-prefix ";;;")
        (princ "BBB")
        (pprint-indent :block 5)
        (pprint-newline :mandatory)
        (princ "CCC"))))
  #.(mkstr "AAA;;;BBB" #\newline
           "   ;;;     CCC"))

(deftest pprint-indent.7
  (with-output-to-string (*standard-output*)
    (princ "AAA")
    (let ((*print-pretty* t)
          (*print-right-margin* 100)
          (*print-miser-width* nil))
      (pprint-logical-block (nil nil :per-line-prefix ";;;")
        (princ "BBB")
        (pprint-indent :current 0)
        (pprint-newline :mandatory)
        (princ "CCC"))))
  #.(mkstr "AAA;;;BBB" #\newline
           "   ;;;   CCC"))

(deftest pprint-indent.8
  (with-output-to-string (*standard-output*)
    (princ "AAA")
    (let ((*print-pretty* t)
          (*print-right-margin* 100)
          (*print-miser-width* nil))
      (pprint-logical-block (nil nil :per-line-prefix ";;;")
        (princ "BBB")
        (pprint-indent :current 2)
        (pprint-newline :mandatory)
        (princ "CCC"))))
  #.(mkstr "AAA;;;BBB" #\newline
           "   ;;;     CCC"))

(deftest pprint-indent.9
  (with-output-to-string (*standard-output*)
    (princ "AAA")
    (let ((*print-pretty* t)
          (*print-right-margin* 100)
          (*print-miser-width* nil))
      (pprint-logical-block (nil nil :per-line-prefix ";;;")
        (princ "BBB")
        (pprint-indent :current -2)
        (pprint-newline :mandatory)
        (princ "CCC"))))
  #.(mkstr "AAA;;;BBB" #\newline
           "   ;;; CCC"))

(deftest pprint-indent.10
  (with-output-to-string (*standard-output*)
    (princ "AAA")
    (let ((*print-pretty* t)
          (*print-right-margin* 100)
          (*print-miser-width* nil))
      (pprint-logical-block (nil nil :per-line-prefix ";;;")
        (princ "BBB")
        (pprint-indent :current -2000)
        (pprint-newline :mandatory)
        (princ "CCC"))))
  #.(mkstr "AAA;;;BBB" #\newline
           "   ;;;CCC"))

(deftest pprint-indent.11
  (with-output-to-string (*standard-output*)
    (pprint-indent :block 10))
  "")


;;
;;  tab
;;
(deftest pprint-tab.10
  (pprint-logical-block-output
    (100 nil)
    (princ "AAA")
    (pprint-tab :line 10 4)
    (princ "BBB"))
  "AAA       BBB")


;;
;;  circle
;;
(deftest pprint-circle.1
  (let ((*print-circle* t)
        (*print-pretty* t)
        (*print-right-margin* 100)
        (*print-miser-width* nil))
    (with-output-to-string (*standard-output*)
      (pprint-logical-block (nil '(10 20 30))
        (princ (pprint-pop))
        (princ (pprint-pop))
        (princ (pprint-pop)))))
  "102030")


;;
;;  examples
;;
(defun examples-pprint-defun (*standard-output* list)
  (pprint-logical-block (nil list :prefix "(" :suffix ")")
    (write (first list))
    (write-char #\Space)
    (pprint-newline :miser)
    (pprint-indent :current 0)
    (write (second list))
    (write-char #\Space)
    (pprint-newline :fill)
    (write (third list))
    (pprint-indent :block 1)
    (write-char #\Space)
    (pprint-newline :linear)
    (write (fourth list))))

(deftest pprint-defun-examples.1
  (let ((*print-pretty* t)
        (*print-right-margin* 26)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (examples-pprint-defun stream '(defun prod (x y) (* x y)))))
  "(DEFUN PROD (X Y) (* X Y))")

(deftest pprint-defun-examples.2
  (let ((*print-pretty* t)
        (*print-right-margin* 25)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (examples-pprint-defun stream '(defun prod (x y) (* x y)))))
  #.(mkstr "(DEFUN PROD (X Y)" #\newline
           "  (* X Y))"))

(deftest pprint-defun-examples.3
  (let ((*print-pretty* t)
        (*print-right-margin* 15)
        (*print-miser-width* 13))
    (with-output-to-string (stream)
      (examples-pprint-defun stream '(defun prod (x y) (* x y)))))
  #.(mkstr "(DEFUN PROD" #\newline
           "       (X Y)" #\newline
           "  (* X Y))"))

(deftest pprint-defun-examples.4
  (let ((*print-pretty* t)
        (*print-right-margin* 15)
        (*print-miser-width* 14))
    (with-output-to-string (stream)
      (examples-pprint-defun stream '(defun prod (x y) (* x y)))))
  #.(mkstr "(DEFUN" #\newline
           " PROD" #\newline
           " (X Y)" #\newline
           " (* X Y))"))

(deftest pprint-defun-examples.5
  (let ((*print-pretty* t)
        (*print-right-margin* 20)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (pprint-logical-block (stream nil :per-line-prefix ";;; ")
        (examples-pprint-defun stream '(defun prod (x y) (* x y))))))
  #.(mkstr ";;; (DEFUN PROD" #\newline
           ";;;        (X Y)" #\newline
           ";;;   (* X Y))"))

(defun examples-pprint-let (stream list)
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    ;; let
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    ;; args
    (pprint-logical-block (stream (pprint-pop) :prefix "(" :suffix ")")
      (pprint-exit-if-list-exhausted)
      (loop (pprint-logical-block (stream (pprint-pop) :prefix "(" :suffix ")")
              (pprint-exit-if-list-exhausted)
              (loop (write (pprint-pop) :stream stream)
                    (pprint-exit-if-list-exhausted)
                    (write-char #\Space stream)
                    (pprint-newline :linear stream)))
            (pprint-exit-if-list-exhausted)
            (write-char #\Space stream)
            (pprint-newline :fill stream)))
    ;; body
    (pprint-indent :block 1 stream)
    (loop (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-newline :linear stream)
          (write (pprint-pop) :stream stream))))

(deftest pprint-let-examples.1
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 77)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (examples-pprint-let
        stream
        '#1=(let (x (*print-length* (f (g 3)))
                    (z . 2) (k (car y)))
              (setq x (sqrt z)) #1#))))
  "#1=(LET (X (*PRINT-LENGTH* (F #)) (Z . 2) (K (CAR Y))) (SETQ X (SQRT Z)) #1#)")

(deftest pprint-let-examples.2
  (let ((*print-pprint-dispatch* lisp-system::*empty-print-dispatch*)
        (*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 77)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (examples-pprint-let
        stream
        '#1=(let (x (*print-length* (f (g 3)))
                    (z . 2) (k (car y)))
              (setq x (sqrt z)) #1#))))
  "#1=(LET (X (*PRINT-LENGTH* (F #)) (Z . 2) (K (CAR Y))) (SETQ X (SQRT Z)) #1#)")

(deftest pprint-let-examples.3
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 76)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (examples-pprint-let
        stream
        '#1=(let (x (*print-length* (f (g 3)))
                    (z . 2) (k (car y)))
              (setq x (sqrt z)) #1#))))
  #.(mkstr "#1=(LET (X (*PRINT-LENGTH* (F #)) (Z . 2) (K (CAR Y)))" #\newline
           "     (SETQ X (SQRT Z))" #\newline
           "     #1#)"))

(deftest pprint-let-examples.4
  (let ((*print-pprint-dispatch* lisp-system::*empty-print-dispatch*)
        (*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 76)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (examples-pprint-let
        stream
        '#1=(let (x (*print-length* (f (g 3)))
                    (z . 2) (k (car y)))
              (setq x (sqrt z)) #1#))))
  #.(mkstr "#1=(LET (X (*PRINT-LENGTH* (F #)) (Z . 2) (K (CAR Y)))" #\newline
           "     (SETQ X (SQRT Z))" #\newline
           "     #1#)"))

(deftest pprint-let-examples.5
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 35)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (examples-pprint-let
        stream
        '#1=(let (x (*print-length* (f (g 3)))
                    (z . 2) (k (car y)))
              (setq x (sqrt z)) #1#))))
  #.(mkstr "#1=(LET (X (*PRINT-LENGTH* (F #))" #\newline
           "         (Z . 2) (K (CAR Y)))" #\newline
           "     (SETQ X (SQRT Z))" #\newline
           "     #1#)"))

(deftest pprint-let-examples.6
  (let ((*print-pprint-dispatch* lisp-system::*empty-print-dispatch*)
        (*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-right-margin* 35)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (examples-pprint-let
        stream
        '#1=(let (x (*print-length* (f (g 3)))
                    (z . 2) (k (car y)))
              (setq x (sqrt z)) #1#))))
  #.(mkstr "#1=(LET (X (*PRINT-LENGTH* (F #))" #\newline
           "         (Z . 2) (K (CAR Y)))" #\newline
           "     (SETQ X (SQRT Z))" #\newline
           "     #1#)"))

(deftest pprint-let-examples.7
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-length* 3)
        (*print-right-margin* 22)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (examples-pprint-let
        stream
        '#1=(let (x (*print-length* (f (g 3)))
                    (z . 2) (k (car y)))
              (setq x (sqrt z)) #1#))))
  #.(mkstr
      "(LET (X" #\newline
      "      (*PRINT-LENGTH*" #\newline
      "       (F #))" #\newline
      "      (Z . 2) ...)" #\newline
      "  (SETQ X (SQRT Z))" #\newline
      "  ...)"))

(deftest pprint-let-examples.8
  (let ((*print-pprint-dispatch* lisp-system::*empty-print-dispatch*)
        (*print-pretty* t)
        (*print-circle* t)
        (*print-level* 4)
        (*print-length* 3)
        (*print-right-margin* 22)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (examples-pprint-let
        stream
        '#1=(let (x (*print-length* (f (g 3)))
                    (z . 2) (k (car y)))
              (setq x (sqrt z)) #1#))))
  #.(mkstr
      "(LET (X" #\newline
      "      (*PRINT-LENGTH*" #\newline
      "       (F #))" #\newline
      "      (Z . 2) ...)" #\newline
      "  (SETQ X (SQRT Z))" #\newline
      "  ...)"))

(defun examples-pprint-vector (*standard-output* v)
  (pprint-logical-block (nil nil :prefix "#(" :suffix ")")
    (let ((end (length v)) (i 0))
      (when (plusp end)
        (loop (pprint-pop)
              (write (aref v i))
              (if (= (incf i) end) (return nil))
              (write-char #\Space)
              (pprint-newline :fill))))))

(deftest pprint-vector-examples.1
  (let ((*print-pretty* t)
        (*print-right-margin* 15)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (examples-pprint-vector
        stream
        '#(12 34 567 8 9012 34 567 89 0 1 23))))
  #.(mkstr "#(12 34 567 8" #\newline
           "  9012 34 567" #\newline
           "  89 0 1 23)"))

(defun examples-pprint-tabular (s list &optional (colon-p t) at-sign-p (tabsize nil))
  (declare (ignore at-sign-p))
  (when (null tabsize) (setq tabsize 16))
  (pprint-logical-block (s list :prefix (if colon-p "(" "")
                           :suffix (if colon-p ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write (pprint-pop) :stream s)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space s)
          (pprint-tab :section-relative 0 tabsize s)
          (pprint-newline :fill s))))

(deftest pprint-tabular-examples.1
  (let ((*print-pretty* t)
        (*print-right-margin* 25)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (princ "Roads " stream)
      (examples-pprint-tabular stream '(elm main maple center) nil nil 8)))
  #.(mkstr "Roads ELM     MAIN" #\newline
           "      MAPLE   CENTER"))


;;
;;  lines
;;
(deftest pprint-lines.1
  (let ((*print-pretty* t)
        (*print-right-margin* 11)
        (*print-miser-width* nil)
        (*print-lines* 10))
    (with-output-to-string (*standard-output*)
      (pprint-logical-block (nil nil)
        (princ "AAA") (pprint-newline :linear)
        (princ "BBB") (pprint-newline :linear)
        (princ "CCC") (pprint-newline :linear)
        (princ "DDD") (pprint-newline :linear))))
  #.(mkstr "AAA" #\newline
           "BBB" #\newline
           "CCC" #\newline
           "DDD" #\newline))

(deftest pprint-lines.2
  (let ((*print-pretty* t)
        (*print-right-margin* 11)
        (*print-miser-width* nil)
        (*print-lines* 3))
    (with-output-to-string (*standard-output*)
      (pprint-logical-block (nil nil)
        (princ "AAA") (pprint-newline :linear)
        (princ "BBB") (pprint-newline :linear)
        (princ "CCC") (pprint-newline :linear)
        (princ "DDD") (pprint-newline :linear))))
  #.(mkstr "AAA" #\newline
           "BBB" #\newline
           "CCC .."))

