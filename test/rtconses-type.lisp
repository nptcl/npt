;;
;;  ANSI COMMON LISP: 14. Conses
;;

;;
;;  System Class LIST
;;
(deftest list-type.1
  (lisp-system:closp
    (find-class 'list))
  t)

(deftest list-type.2
  (flet ((findc (x) (lisp-system:closp
                      (find (find-class x)
                            (lisp-clos:class-precedence-list
                              (find-class 'list))))))
    (every #'findc '(list sequence t)))
  t)

(deftest list-type.3
  (typep nil 'list)
  t)

(deftest list-type.4
  (typep '(10 20 30) 'list)
  t)

(deftest list-type.5
  (typep '(10 . 30) 'list)
  t)

(deftest list-type.6
  (typep #(10 20 30) 'list)
  nil)

(deftest list-type.7
  (typep t 'list)
  nil)


;;
;;  System Class NULL
;;
(deftest null-type.1
  (lisp-system:closp
    (find-class 'null))
  t)

(deftest null-type.2
  (flet ((findc (x) (lisp-system:closp
                      (find (find-class x)
                            (lisp-clos:class-precedence-list
                              (find-class 'null))))))
    (every #'findc '(null symbol list sequence t)))
  t)

(deftest null-type.3
  (typep nil 'null)
  t)

(deftest null-type.4
  (typep #() 'null)
  nil)

(deftest null-type.5
  (typep '(10 20) 'null)
  nil)

(deftest null-type.6
  (typep "Hello" 'null)
  nil)

(deftest null-type.7
  (typep (list) 'null)
  t)


;;
;;  System Class CONS
;;
(deftest cons-type.1
  (lisp-system:closp
    (find-class 'cons))
  t)

(deftest cons-type.2
  (flet ((findc (x) (lisp-system:closp
                      (find (find-class x)
                            (lisp-clos:class-precedence-list
                              (find-class 'cons))))))
    (every #'findc '(cons list sequence t)))
  t)

(deftest cons-type.3
  (typep nil 'cons)
  nil)

(deftest cons-type.4
  (typep '(10 20) 'cons)
  t)

(deftest cons-type.5
  (typep '(10 . 30) 'cons)
  t)

(deftest cons-type.6
  (typep #(10) 'cons)
  nil)

(deftest cons-type.7
  (typep #\A 'cons)
  nil)


;;
;;  Type ATOM
;;
(deftest atom-type.1
  (find-class 'atom nil)
  nil)

(deftest atom-type.2
  (typep nil 'atom)
  t)

(deftest atom-type.3
  (typep '(10) 'atom)
  nil)

(deftest atom-type.4
  (typep #(10) 'atom)
  t)

(deftest atom-type.5
  (typep :hello 'atom)
  t)

