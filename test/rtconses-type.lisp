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
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'list)))
  (list sequence t))

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
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'null)))
  (null symbol list sequence t))

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
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'cons)))
  (cons list sequence t))

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


;;
;;  Function CONSP
;;
(deftest consp.1
  (consp nil)
  nil)

(deftest consp.2
  (consp (cons 1 2))
  t)

(deftest consp.3
  (consp #(10 20 30))
  nil)

(deftest consp.4
  (consp '())
  nil)

(deftest consp.5
  (consp 'nil)
  nil)

(deftest consp.6
  (consp '(10 20 30))
  t)

(deftest consp.7
  (consp 10)
  nil)

(deftest-error! consp.8
  (eval '(consp)))

(deftest-error! consp.9
  (eval '(consp 10 20)))

(deftest-error consp.10
  (funcall #'consp))

(deftest consp.11
  (funcall #'consp '(10))
  t)

(deftest-error consp.12
  (funcall #'consp 10 20))


;;
;;  Function ATOM
;;
(deftest atom.1
  (atom 'sss)
  t)

(deftest atom.2
  (atom (cons 1 2))
  nil)

(deftest atom.3
  (atom nil)
  t)

(deftest atom.4
  (atom '())
  t)

(deftest atom.5
  (atom 3)
  t)

(deftest atom.6
  (atom '(10 20 30))
  nil)

(deftest-error! atom.7
  (eval '(atom)))

(deftest-error! atom.8
  (eval '(atom 10 20)))


;;
;;  Function LISTP
;;
(deftest listp.1
  (listp nil)
  t)

(deftest listp.2
  (listp '(10 . 20))
  t)

(deftest listp.3
  (listp 10)
  nil)

(deftest listp.4
  (listp (cons 1 2))
  t)

(deftest listp.5
  (listp (make-array 6))
  nil)

(deftest listp.6
  (listp t)
  nil)

(deftest-error! listp-error.1
  (eval '(listp)))

(deftest-error! listp-error.2
  (eval '(listp 10 20)))


;;
;;  Function ENDP
;;
(deftest endp.1
  (endp nil)
  t)

(deftest endp.2
  (endp '(1 2))
  nil)

(deftest endp.3
  (endp (cddr '(1 2)))
  t)

(deftest-error! endp-error.1
  (eval '(endp 10))
  type-error)

(deftest-error! endp-error.2
  (eval '(endp)))

(deftest-error! endp-error.3
  (eval '(endp nil nil)))


;;
;;  Function NULL
;;
(deftest null.1
  (null nil)
  t)

(deftest null.2
  (null '())
  t)

(deftest null.3
  (null '(10 20 30))
  nil)

(deftest null.4
  (null "Hello")
  nil)

(deftest null.5
  (null t)
  nil)

(deftest null.6
  (null 1)
  nil)

(deftest-error! null-error.1
  (eval '(null)))

(deftest-error! null-error.2
  (eval '(null nil nil)))

