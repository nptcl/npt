;;
;;  ANSI COMMON LISP: 4. Types and Classes
;;

;;
;;  Type NIL
;;
(deftest nil-type.1
  (typep nil nil)
  nil)

(deftest nil-type.2
  (typep t nil)
  nil)

(deftest nil-type.3
  (typep 10 nil)
  nil)


;;
;;  System Class T
;;
(deftest t-type.1
  (lisp-system:closp
    (find-class 'readtable))
  t)

(deftest t-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 't)))
  (t))

(deftest t-type.3
  (typep nil t)
  t)

(deftest t-type.4
  (typep 10 t)
  t)

(deftest t-type.5
  (typep t t)
  t)

