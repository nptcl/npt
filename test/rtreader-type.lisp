;;
;;  ANSI COMMON LISP: 23. Reader
;;

;;
;;  System Class READTABLE
;;
(deftest readtable-type.1
  (lisp-system:closp
    (find-class 'readtable))
  t)

(deftest readtable-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'readtable)))
  (readtable t))

(deftest readtable-type.3
  (typep *readtable* 'readtable)
  t)

(deftest readtable-type.4
  (typep (copy-readtable) 'readtable)
  t)

(deftest readtable-type.5
  (typep 10 'readtable)
  nil)

(deftest readtable-type.6
  (typep nil 'readtable)
  nil)


;;
;;  Function READTABLEP
;;
(deftest readtablep.1
  (readtablep *readtable*)
  t)

(deftest readtablep.2
  (readtablep (copy-readtable nil))
  t)

(deftest readtablep.3
  (readtablep 100)
  nil)

(deftest readtablep.4
  (readtablep '*readtable*)
  nil)

(deftest-error! readtablep-error.1
  (eval '(readtablep)))

(deftest-error! readtablep-error.2
  (eval '(readtablep nil nil)))

