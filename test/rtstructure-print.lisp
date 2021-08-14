;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  print-object
;;

;;  clos
(defstruct clos-print-object-1)
(deftest clos-print-object.1
  (princ-to-string (make-clos-print-object-1))
  "#S(CLOS-PRINT-OBJECT-1)")

(defun clos-print-object-2-call (value stream)
  (declare (ignore value))
  (princ "Hello" stream))

(defstruct (clos-print-object-2 (:print-object clos-print-object-2-call)))
(deftest clos-print-object.2
  (princ-to-string (make-clos-print-object-2))
  "Hello")

(defstruct (clos-print-object-3 (:include clos-print-object-2)))
(deftest clos-print-object.3
  (princ-to-string (make-clos-print-object-3))
  "Hello")

(defstruct (clos-print-object-4 (:include clos-print-object-2) :print-object))
(deftest clos-print-object.4
  (princ-to-string (make-clos-print-object-4))
  "#S(CLOS-PRINT-OBJECT-4)")


;;  list
(deftest-error list-clos-print-object.1
  (defstruct (list-clos-print-object-1 (:type list) :named :print-object)))

(deftest-error list-clos-print-object.2
  (defstruct (list-clos-print-object-2
               (:type list) :named
               (:print-object list-clos-print-object-2-call))))


;;  vector
(deftest-error vector-print-object.1
  (defstruct (vector-print-object-1 (:type vector) :named :print-object)))

(deftest-error vector-print-object.2
  (defstruct (vector-print-object-2
               (:type vector) :named
               (:print-object vector-print-object-2-call))))


;;
;;  print-function
;;

;;  clos
(defstruct clos-print-function-1)
(deftest clos-print-function.1
  (princ-to-string (make-clos-print-function-1))
  "#S(CLOS-PRINT-FUNCTION-1)")

(defun clos-print-function-2-call (value stream level)
  (declare (ignore value level))
  (princ "Hello" stream))

(defstruct (clos-print-function-2 (:print-function clos-print-function-2-call)))
(deftest clos-print-function.2
  (princ-to-string (make-clos-print-function-2))
  "Hello")

(defstruct (clos-print-function-3 (:include clos-print-function-2)))
(deftest clos-print-function.3
  (princ-to-string (make-clos-print-function-3))
  "Hello")

(defstruct (clos-print-function-4 (:include clos-print-function-2) :print-function))
(deftest clos-print-function.4
  (princ-to-string (make-clos-print-function-4))
  "#S(CLOS-PRINT-FUNCTION-4)")


;;  list
(deftest-error list-print-function.1
  (defstruct (list-print-function-1 (:type list) :named :print-function)))

(deftest-error list-print-function.2
  (defstruct (list-print-function-2
               (:type list) :named
               (:print-function list-print-function-2-call))))


;;  vector
(deftest-error vector-print-function.1
  (defstruct (vector-print-function-1 (:type vector) :named :print-function)))

(deftest-error vector-print-function.2
  (defstruct (vector-print-function-2
               (:type vector) :named
               (:print-function vector-print-function-2-call))))


;;
;;  change
;;
(deftest change-print-object.1
  (progn
    (defstruct change-print-object-1)
    (defstruct change-print-object-1)
    (princ-to-string (make-change-print-object-1)))
  "#S(CHANGE-PRINT-OBJECT-1)")

(defun change-print-object-call-1 (value stream)
  (declare (ignore value))
  (princ "Hello" stream))

(deftest change-print-object.2
  (progn
    (defstruct change-print-object-2)
    (defstruct (change-print-object-2
                 (:print-object change-print-object-call-1)))
    (princ-to-string (make-change-print-object-2)))
  "Hello")

(deftest change-print-object.3
  (progn
    (defstruct (change-print-object-3
                 (:print-object change-print-object-call-1)))
    (defstruct change-print-object-3)
    (princ-to-string (make-change-print-object-3)))
  "#S(CHANGE-PRINT-OBJECT-3)")

(defun change-print-object-call-2 (value stream)
  (declare (ignore value))
  (princ "ABC" stream))

(deftest change-print-object.4
  (progn
    (defstruct (change-print-object-4
                 (:print-object change-print-object-call-1)))
    (defstruct (change-print-object-4
                 (:print-object change-print-object-call-2)))
    (princ-to-string (make-change-print-object-4)))
  "ABC")

(defun change-print-object-call-3 (value stream level)
  (declare (ignore value level))
  (princ "AAA" stream))

(deftest change-print-object.5
  (progn
    (defstruct change-print-object-5)
    (defstruct (change-print-object-5
                 (:print-function change-print-object-call-3)))
    (princ-to-string (make-change-print-object-5)))
  "AAA")

(deftest change-print-object.6
  (progn
    (defstruct (change-print-object-6
                 (:print-object change-print-object-call-3)))
    (defstruct change-print-object-6)
    (princ-to-string (make-change-print-object-6)))
  "#S(CHANGE-PRINT-OBJECT-6)")

(defun change-print-object-call-4 (value stream)
  (declare (ignore value))
  (princ "BBB" stream))

(deftest change-print-object.7
  (progn
    (defstruct (change-print-object-7
                 (:print-object change-print-object-call-1)))
    (defstruct (change-print-object-7
                 (:print-object change-print-object-call-4)))
    (princ-to-string (make-change-print-object-7)))
  "BBB")

