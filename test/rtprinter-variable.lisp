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

;;  variable
;;  write :base
;;  write-to-string :base


;; Variable *PRINT-RADIX*
;; Variable *PRINT-CASE*
;; Variable *PRINT-CIRCLE*
;; Variable *PRINT-ESCAPE*
;; Variable *PRINT-GENSYM*
;; Variable *PRINT-LEVEL*
;; Variable *PRINT-LENGTH*
;; Variable *PRINT-LINES*
;; Variable *PRINT-MISER-WIDTH*
;; Variable *PRINT-PRETTY*
;; Variable *PRINT-READABLY*
;; Variable *PRINT-RIGHT-MARGIN*

