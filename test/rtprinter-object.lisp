;;
;;  ANSI COMMON LISP: 22. Printer
;;

;;  clos
(deftest write-clos.1
  (with-default-print
    (let ((v (find-class 'class)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#<STANDARD-CLASS CLASS>"
  "#<STANDARD-CLASS CLASS>")

(deftest write-clos.2
  (with-default-print
    (let ((v (find-class 'standard-class)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#<STANDARD-CLASS STANDARD-CLASS>"
  "#<STANDARD-CLASS STANDARD-CLASS>")

(deftest write-clos.3
  (with-default-print
    (let ((v (find-class 'integer)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#<BUILT-IN-CLASS INTEGER>"
  "#<BUILT-IN-CLASS INTEGER>")

(defclass write-clos1 () ())
(deftest write-clos.4
  (with-default-print
    (let ((v (find-class 'write-clos1)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#<STANDARD-CLASS WRITE-CLOS1>"
  "#<STANDARD-CLASS WRITE-CLOS1>")

(deftest write-clos.5
  (with-default-print
    (let ((v (make-instance 'write-clos1)))
      (values
        (subseq (prin1-to-string v) 0 16)
        (subseq (princ-to-string v) 0 16))))
  "#<WRITE-CLOS1 #x"
  "#<WRITE-CLOS1 #x")

(defclass write-clos2 () ())
(defmethod print-object ((pos write-clos2) stream)
  (declare (ignore pos))
  (if *print-escape*
    (princ "AAA" stream)
    (princ "BBB" stream)))
(deftest write-clos.6
  (with-default-print
    (let ((v (make-instance 'write-clos2)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "AAA" "BBB")


;;  structure
(defstruct write-structure1)
(deftest write-structure.1
  (with-default-print
    (let ((v (find-class 'write-structure1)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#<STRUCTURE-CLASS WRITE-STRUCTURE1>"
  "#<STRUCTURE-CLASS WRITE-STRUCTURE1>")

(deftest write-structure.2
  (with-default-print
    (let ((v (make-write-structure1)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#S(WRITE-STRUCTURE1)"
  "#S(WRITE-STRUCTURE1)")

(defstruct write-structure2 aaa bbb)
(deftest write-structure.3
  (with-default-print
    (let ((v (make-write-structure2 :aaa 10 :bbb 20)))
      (values
        (prin1-to-string v)
        (princ-to-string v))))
  "#S(WRITE-STRUCTURE2 :AAA 10 :BBB 20)"
  "#S(WRITE-STRUCTURE2 :AAA 10 :BBB 20)")

