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

