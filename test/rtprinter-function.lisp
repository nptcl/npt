;;
;;  ANSI COMMON LISP: 22. Printer
;;

(deftest pprint-fill.1
  (let ((*print-pretty* t)
        (*print-right-margin* 15)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (pprint-fill
        stream
        '(12 34 567 8 9012 34 567 89 0 1 23))))
  #.(mkstr "(12 34 567 8" #\newline
           " 9012 34 567" #\newline
           " 89 0 1 23)"))

(deftest pprint-fill.2
  (let ((*print-pretty* t)
        (*print-right-margin* 30)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (pprint-fill
        stream
        '(12 34 567 8 9012 34 567 89 0 1 23))))
  #.(mkstr "(12 34 567 8 9012 34 567 89 0" #\newline " 1 23)"))

(deftest pprint-fill.3
  (let ((*print-pretty* t)
        (*print-right-margin* 30)
        (*print-miser-width* 30))
    (with-output-to-string (stream)
      (pprint-fill
        stream
        '(12 34 567 8 9012 34 567 89 0 1 23))))
  #.(mkstr "(12" #\newline
           " 34" #\newline
           " 567" #\newline
           " 8" #\newline
           " 9012" #\newline
           " 34" #\newline
           " 567" #\newline
           " 89" #\newline
           " 0" #\newline
           " 1" #\newline
           " 23)"))

(deftest pprint-fill.4
  (let ((*print-pretty* t)
        (*print-right-margin* 15)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (pprint-fill
        stream
        '(12 34 567 8 9012 34 567 89 0 1 23)
        nil)))
  #.(mkstr "12 34 567 8" #\newline
           "9012 34 567 89" #\newline
           "0 1 23"))

(deftest pprint-linear.1
  (let ((*print-pretty* t)
        (*print-right-margin* 20)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (pprint-linear
        stream
        '(12 34 567 8 9012))))
  "(12 34 567 8 9012)")

(deftest pprint-linear.2
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (pprint-linear
        stream
        '(12 34 567 8 9012))))
  #.(mkstr "(12" #\newline
           " 34" #\newline
           " 567" #\newline
           " 8" #\newline
           " 9012)"))

(deftest pprint-linear.3
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (pprint-linear
        stream
        '(12 34 567 8 9012)
        nil)))
  #.(mkstr "12" #\newline
           "34" #\newline
           "567" #\newline
           "8" #\newline
           "9012"))

(deftest pprint-tabular.1
  (let ((*print-pretty* t)
        (*print-right-margin* 40)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (pprint-tabular
        stream
        '(12 34 567 8 9012 34 567 89 0 1 23)
        t t 8)))
  #.(mkstr "(12      34      567     8" #\newline
           " 9012    34      567     89" #\newline
           " 0       1       23)"))

(deftest pprint-tabular.2
  (let ((*print-pretty* t)
        (*print-right-margin* 25)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (princ "Roads " stream)
      (pprint-tabular stream '(elm main maple center) nil nil 8)))
  #.(mkstr "Roads ELM     MAIN" #\newline
           "      MAPLE   CENTER"))

(deftest pprint-tabular.3
  (let ((*print-pretty* t)
        (*print-right-margin* 25)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (princ "Roads " stream)
      (pprint-tabular stream '(elm main maple center) t nil 8)))
  #.(mkstr "Roads (ELM     MAIN" #\newline
           "       MAPLE   CENTER)"))

(deftest pprint-tabular.4
  (let ((*print-pretty* t)
        (*print-right-margin* 100)
        (*print-miser-width* nil))
    (with-output-to-string (stream)
      (princ "Roads " stream)
      (pprint-tabular stream '(elm main))))
  "Roads (ELM             MAIN)")


;;
;;  dispatch vector
;;
(deftest dispatch-cons.1
  (let ((*print-pretty* t)
        (*print-right-margin* 100)
        (*print-miser-width* nil))
    (with-output-to-string (*standard-output*)
      (princ
        '(12 34 567 8 9012 34 567 89 0 1 23))))
  "(12 34 567 8 9012 34 567 89 0 1 23)")

(deftest dispatch-cons.2
  (let ((*print-pretty* t)
        (*print-right-margin* 15)
        (*print-miser-width* nil))
    (with-output-to-string (*standard-output*)
      (princ
        '(12 34 567 8 9012 34 567 89 0 1 23))))
  #.(mkstr "(12 34 567 8" #\newline
           " 9012 34 567" #\newline
           " 89 0 1 23)"))

(deftest dispatch-cons.3
  (let ((*print-pretty* t)
        (*print-right-margin* 15)
        (*print-miser-width* 100))
    (with-output-to-string (*standard-output*)
      (princ
        '(12 34 567 8 9012))))
  #.(mkstr "(12" #\newline
           " 34" #\newline
           " 567" #\newline
           " 8" #\newline
           " 9012)"))

(deftest dispatch-vector.1
  (let ((*print-pretty* t)
        (*print-right-margin* 100)
        (*print-miser-width* nil))
    (with-output-to-string (*standard-output*)
      (princ
        #(12 34 567 8 9012 34 567 89 0 1 23))))
  "#(12 34 567 8 9012 34 567 89 0 1 23)")

(deftest dispatch-vector.2
  (let ((*print-pretty* t)
        (*print-right-margin* 15)
        (*print-miser-width* nil))
    (with-output-to-string (*standard-output*)
      (princ
        #(12 34 567 8 9012 34 567 89 0 1 23))))
  #.(mkstr "#(12 34 567 8" #\newline
           "  9012 34 567" #\newline
           "  89 0 1 23)"))

(deftest dispatch-vector.3
  (let ((*print-pretty* t)
        (*print-right-margin* 15)
        (*print-miser-width* nil))
    (with-output-to-string (*standard-output*)
      (princ "Hello")))
  "Hello")

(deftest dispatch-vector.4
  (let ((*print-pretty* t)
        (*print-right-margin* 15)
        (*print-miser-width* nil))
    (with-output-to-string (*standard-output*)
      (princ #*10111)))
  "#*10111")

(deftest dispatch-call.1
  (let ((*print-pretty* t)
        (*print-right-margin* 100)
        (*print-miser-width* nil))
    (with-output-to-string (*standard-output*)
      (princ
        '(list 12 34 567 8))))
  "(LIST 12 34 567 8)")

(deftest dispatch-call.2
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* nil))
    (with-output-to-string (*standard-output*)
      (princ
        '(list 12 34 567 8))))
  #.(mkstr "(LIST 12" #\newline
           "      34" #\newline
           "      567" #\newline
           "      8)"))

(deftest dispatch-call.3
  (let ((*print-pretty* t)
        (*print-right-margin* 10)
        (*print-miser-width* 100))
    (with-output-to-string (*standard-output*)
      (princ
        '(list 12 34 567 8))))
  #.(mkstr "(LIST" #\newline
           " 12" #\newline
           " 34" #\newline
           " 567" #\newline
           " 8)"))

(deftest dispatch-defun.1
  (let ((*print-pretty* t)
        (*print-right-margin* 30)
        (*print-miser-width* nil))
    (with-output-to-string (*standard-output*)
      (prin1
        '(defun hello (a b &rest c &aux (d 10))
           (declare (ignorable a b c))
           "Hello function"
           (setq a (+ b c d))
           (* a 999)))))
  #.(mkstr "(DEFUN HELLO (A B &REST C" #\newline
           "              &AUX (D 10))" #\newline
           "  (DECLARE (IGNORABLE A B C))" #\newline
           "  \"Hello function\"" #\newline
           "  (SETQ A (+ B C D))" #\newline
           "  (* A 999))"))

(deftest dispatch-defun.2
  (let ((*print-pretty* t)
        (*print-right-margin* 20)
        (*print-miser-width* 100))
    (with-output-to-string (*standard-output*)
      (prin1
        '(defun hello (a b &rest c &aux (d 10))
           (declare (ignorable a b c))
           "Hello function"
           (setq a (+ b c d))
           (* a 999)))))
  #.(mkstr "(DEFUN" #\newline
           " HELLO" #\newline
           " (A" #\newline
           "  B" #\newline
           "  &REST" #\newline
           "  C" #\newline
           "  &AUX" #\newline
           "  (D 10))" #\newline
           " (DECLARE" #\newline
           "  (IGNORABLE A B C))" #\newline
           " \"Hello function\"" #\newline
           " (SETQ A (+ B C D))" #\newline
           " (* A 999))"))

(deftest dispatch-let.1
  (let ((*print-pretty* t)
        (*print-right-margin* 30)
        (*print-miser-width* nil))
    (with-output-to-string (*standard-output*)
      (prin1
        '(let (a (b) (c (+ 10 20 30 40)) (d 30))
           "Hello function"
           (setq a (+ b c d))
           (* a 999)))))
  #.(mkstr "(LET (A (B)" #\newline
           "      (C (+ 10 20 30 40))" #\newline
           "      (D 30))" #\newline
           "  \"Hello function\"" #\newline
           "  (SETQ A (+ B C D))" #\newline
           "  (* A 999))"))

(deftest dispatch-let.2
  (let ((*print-pretty* t)
        (*print-right-margin* 20)
        (*print-miser-width* 100))
    (with-output-to-string (*standard-output*)
      (prin1
        '(let (a (b) (c (+ 10 20 30 40)) (d 30))
           "Hello function"
           (setq a (+ b c d))
           (* a 999)))))
  #.(mkstr "(LET" #\newline
           " (A" #\newline
           "  (B)" #\newline
           "  (C" #\newline
           "   (+ 10 20 30 40))" #\newline
           "  (D 30))" #\newline
           " \"Hello function\"" #\newline
           " (SETQ A (+ B C D))" #\newline
           " (* A 999))"))

