;;
;;  ANSI COMMON LISP: 14. Conses
;;

;;
;;  Function CONS
;;
(deftest cons.1
  (cons 1 2)
  (1 . 2))

(deftest cons.2
  (cons 1 nil)
  (1))

(deftest cons.3
  (cons nil 2)
  (nil . 2))

(deftest cons.4
  (cons nil nil)
  (nil))

(deftest cons.5
  (cons 1 (cons 2 (cons 3 (cons 4 nil))))
  (1 2 3 4))

(deftest cons.6
  (cons 'a 'b)
  (a . b))

(deftest cons.7
  (cons 'a (cons 'b (cons 'c '())))
  (a b c))

(deftest cons.8
  (cons 'a '(b c d))
  (a b c d))

(deftest cons.9
  (eq (cons nil nil) (cons nil nil))
  nil)

(deftest-error! cons.10
  (eval '(cons)))

(deftest-error! cons.11
  (eval '(cons 10)))

(deftest-error! cons.12
  (eval '(cons 10 20 30)))

(deftest-error cons.13
  (funcall #'cons 10))

(deftest cons.14
  (funcall #'cons 10 20)
  (10 . 20))

(deftest-error cons.15
  (funcall #'cons 10 20 30))


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
;;  Function RPLACA
;;
(deftest rplaca.1
  (let ((a (cons 10 20)))
    (rplaca a 99)
    a)
  (99 . 20))

(deftest rplaca.2
  (let ((a (cons 10 20)))
    (rplaca a 99))
  (99 . 20))

(deftest rplaca.3
  (let ((a (cons 10 20)))
    (eq a (rplaca a 99)))
  t)

(deftest rplaca.4
  (let ((some-list (list* 'one 'two 'three 'four)))
    (values
      (rplaca some-list 'uno)
      some-list))
  (uno two three . four)
  (uno two three . four))

(deftest-error! rplaca.5
  (eval '(rplaca)))

(deftest-error! rplaca.6
  (eval '(rplaca (cons 10 20))))

(deftest-error! rplaca.7
  (eval '(rplaca (cons 10 20) 30 40)))

(deftest-error rplaca.8
  (eval '(rplaca nil 10)))

(deftest-error rplaca.9
  (eval '(rplaca 'hello 10)))


;;
;;  Function RPLACD
;;
(deftest rplacd.1
  (let ((a (cons 10 20)))
    (rplacd a 99)
    a)
  (10 . 99))

(deftest rplacd.2
  (let ((a (cons 10 20)))
    (rplacd a 99))
  (10 . 99))

(deftest rplacd.3
  (let ((a (cons 10 20)))
    (eq a (rplacd a 99)))
  t)

(deftest rplacd.4
  (let ((some-list '(uno two three . four)))
    (values
      (rplacd (last some-list) (list 'IV))
      some-list))
  (three iv)
  (uno two three iv))

(deftest-error! rplacd.5
  (eval '(rplacd)))

(deftest-error! rplacd.6
  (eval '(rplacd (cons 10 20))))

(deftest-error! rplacd.7
  (eval '(rplacd (cons 10 20) 30 40)))

(deftest-error rplacd.8
  (eval '(rplacd nil 10)))

(deftest-error rplacd.9
  (eval '(rplacd 'hello 10)))


;;
;;  Function CAR
;;
(deftest car.1
  (car nil)
  nil)

(deftest car.2
  (car '(10 . 20))
  10)

(deftest-error car.3
  (eval '(car 'hello))
  type-error)

(deftest-error! car.4
  (eval '(car)))

(deftest-error! car.5
  (eval '(car nil nil)))


;;
;;  Funciton CDR
;;
(deftest cdr.1
  (cdr nil)
  nil)

(deftest cdr.2
  (cdr '(10 . 20))
  20)

(deftest-error cdr.3
  (eval '(cdr 'hello))
  type-error)

(deftest-error! cdr.4
  (eval '(cdr)))

(deftest-error! cdr.5
  (eval '(cdr nil nil)))


;;
;;  Function (SETF CAR)
;;
(deftest setf-car.1
  (let ((a '(10 20 30)))
    (setf (car a) 999))
  999)

(deftest setf-car.2
  (let ((a '(10 20 30)))
    (setf (car a) 999)
    a)
  (999 20 30))

(deftest-error setf-car.3
  (setf (car 'hello) 999)
  type-error)

(deftest-error setf-car.4
  (setf (car nil) 999)
  type-error)

(deftest-error! setf-car.5
  (setf (car) 999))

(deftest-error! setf-car.6
  (let ((x '(10 20 30)))
    (setf (car x 10 20) 999)))


;;
;;  Function (SETF CDR)
;;
(deftest setf-cdr.1
  (let ((a '(10 20 30)))
    (setf (cdr a) 999))
  999)

(deftest setf-cdr.2
  (let ((a '(10 20 30)))
    (setf (cdr a) 999)
    a)
  (10 . 999))

(deftest-error setf-cdr.3
  (setf (cdr 'hello) 999)
  type-error)

(deftest-error setf-cdr.4
  (setf (cdr nil) 999)
  type-error)

(deftest-error! setf-cdr.5
  (setf (cdr) 999))

(deftest-error! setf-cdr.6
  (let ((x '(10 20 30)))
    (setf (cdr x 10 20) 999)))

(deftest carcdr.1
  (car nil)
  nil)

(deftest carcdr.2
  (cdr '(1 . 2))
  2)

(deftest carcdr.3
  (cdr '(1 2))
  (2))

(deftest carcdr.4
  (cadr '(1 2))
  2)

(deftest carcdr.5
  (car '(a b c))
  a)

(deftest carcdr.6
  (cdr '(a b c))
  (b c))


;;
;;  Function CxR
;;
(defun seqlist3 (x &aux (i 0))
  (labels ((sz (z)
               (unless (zerop z)
                 (cons (incf i) (sz (1- z)))))
           (sy (y z)
               (unless (zerop y)
                 (cons (sz z) (sy (1- y) z))))
           (sx (x y z)
               (unless (zerop x)
                 (cons (sy y z) (sx (1- x) y z))))
           (sw (w x y z)
               (unless (zerop w)
                 (cons (sx x y z) (sw (1- w) x y z)))))
    (sw x x x x)))

(defparameter *cxr* (seqlist3 5))

(deftest cxr.1
  (eq (caar *cxr*) (car (car *cxr*)))
  t)

(deftest cxr.2
  (eq (cadr *cxr*) (car (cdr *cxr*)))
  t)

(deftest cxr.3
  (eq (cdar *cxr*) (cdr (car *cxr*)))
  t)

(deftest cxr.4
  (eq (cddr *cxr*) (cdr (cdr *cxr*)))
  t)

(deftest cxr.5
  (eq (caaar *cxr*) (car (car (car *cxr*))))
  t)

(deftest cxr.6
  (eq (caadr *cxr*) (car (car (cdr *cxr*))))
  t)

(deftest cxr.7
  (eq (cadar *cxr*) (car (cdr (car *cxr*))))
  t)

(deftest cxr.8
  (eq (caddr *cxr*) (car (cdr (cdr *cxr*))))
  t)

(deftest cxr.9
  (eq (cdaar *cxr*) (cdr (car (car *cxr*))))
  t)

(deftest cxr.10
  (eq (cdadr *cxr*) (cdr (car (cdr *cxr*))))
  t)

(deftest cxr.11
  (eq (cddar *cxr*) (cdr (cdr (car *cxr*))))
  t)

(deftest cxr.12
  (eq (cdddr *cxr*) (cdr (cdr (cdr *cxr*))))
  t)

(deftest cxr.13
  (eq (caaaar *cxr*) (car (car (car (car *cxr*)))))
  t)

(deftest cxr.14
  (eq (caaadr *cxr*) (car (car (car (cdr *cxr*)))))
  t)

(deftest cxr.15
  (eq (caadar *cxr*) (car (car (cdr (car *cxr*)))))
  t)

(deftest cxr.16
  (eq (caaddr *cxr*) (car (car (cdr (cdr *cxr*)))))
  t)

(deftest cxr.17
  (eq (cadaar *cxr*) (car (cdr (car (car *cxr*)))))
  t)

(deftest cxr.18
  (eq (cadadr *cxr*) (car (cdr (car (cdr *cxr*)))))
  t)

(deftest cxr.19
  (eq (caddar *cxr*) (car (cdr (cdr (car *cxr*)))))
  t)

(deftest cxr.20
  (eq (cadddr *cxr*) (car (cdr (cdr (cdr *cxr*)))))
  t)

(deftest cxr.21
  (eq (cdaaar *cxr*) (cdr (car (car (car *cxr*)))))
  t)

(deftest cxr.22
  (eq (cdaadr *cxr*) (cdr (car (car (cdr *cxr*)))))
  t)

(deftest cxr.23
  (eq (cdadar *cxr*) (cdr (car (cdr (car *cxr*)))))
  t)

(deftest cxr.24
  (eq (cdaddr *cxr*) (cdr (car (cdr (cdr *cxr*)))))
  t)

(deftest cxr.25
  (eq (cddaar *cxr*) (cdr (cdr (car (car *cxr*)))))
  t)

(deftest cxr.26
  (eq (cddadr *cxr*) (cdr (cdr (car (cdr *cxr*)))))
  t)

(deftest cxr.27
  (eq (cdddar *cxr*) (cdr (cdr (cdr (car *cxr*)))))
  t)

(deftest cxr.28
  (eq (cddddr *cxr*) (cdr (cdr (cdr (cdr *cxr*)))))
  t)

(defmacro setf-cxr-eq (x y z)
  (let ((g (gensym)))
    `(let ((*cxr* (seqlist3 ,z)))
       (setf ,x ',g)
       (eq ,y ',g))))

(deftest setf-cxr.1
  (setf-cxr-eq (caar *cxr*) (car (car *cxr*)) 3)
  t)

(deftest setf-cxr.2
  (setf-cxr-eq (cadr *cxr*) (car (cdr *cxr*)) 3)
  t)

(deftest setf-cxr.3
  (setf-cxr-eq (cdar *cxr*) (cdr (car *cxr*)) 3)
  t)

(deftest setf-cxr.4
  (setf-cxr-eq (cddr *cxr*) (cdr (cdr *cxr*)) 3)
  t)

(deftest setf-cxr.5
  (setf-cxr-eq (caaar *cxr*) (car (car (car *cxr*))) 4)
  t)

(deftest setf-cxr.6
  (setf-cxr-eq (caadr *cxr*) (car (car (cdr *cxr*))) 4)
  t)

(deftest setf-cxr.7
  (setf-cxr-eq (cadar *cxr*) (car (cdr (car *cxr*))) 4)
  t)

(deftest setf-cxr.8
  (setf-cxr-eq (caddr *cxr*) (car (cdr (cdr *cxr*))) 4)
  t)

(deftest setf-cxr.9
  (setf-cxr-eq (cdaar *cxr*) (cdr (car (car *cxr*))) 4)
  t)

(deftest setf-cxr.10
  (setf-cxr-eq (cdadr *cxr*) (cdr (car (cdr *cxr*))) 4)
  t)

(deftest setf-cxr.11
  (setf-cxr-eq (cddar *cxr*) (cdr (cdr (car *cxr*))) 4)
  t)

(deftest setf-cxr.12
  (setf-cxr-eq (cdddr *cxr*) (cdr (cdr (cdr *cxr*))) 4)
  t)

(deftest setf-cxr.13
  (setf-cxr-eq (caaaar *cxr*) (car (car (car (car *cxr*)))) 5)
  t)

(deftest setf-cxr.14
  (setf-cxr-eq (caaadr *cxr*) (car (car (car (cdr *cxr*)))) 5)
  t)

(deftest setf-cxr.15
  (setf-cxr-eq (caadar *cxr*) (car (car (cdr (car *cxr*)))) 5)
  t)

(deftest setf-cxr.16
  (setf-cxr-eq (caaddr *cxr*) (car (car (cdr (cdr *cxr*)))) 5)
  t)

(deftest setf-cxr.17
  (setf-cxr-eq (cadaar *cxr*) (car (cdr (car (car *cxr*)))) 5)
  t)

(deftest setf-cxr.18
  (setf-cxr-eq (cadadr *cxr*) (car (cdr (car (cdr *cxr*)))) 5)
  t)

(deftest setf-cxr.19
  (setf-cxr-eq (caddar *cxr*) (car (cdr (cdr (car *cxr*)))) 5)
  t)

(deftest setf-cxr.20
  (setf-cxr-eq (cadddr *cxr*) (car (cdr (cdr (cdr *cxr*)))) 5)
  t)

(deftest setf-cxr.21
  (setf-cxr-eq (cdaaar *cxr*) (cdr (car (car (car *cxr*)))) 5)
  t)

(deftest setf-cxr.22
  (setf-cxr-eq (cdaadr *cxr*) (cdr (car (car (cdr *cxr*)))) 5)
  t)

(deftest setf-cxr.23
  (setf-cxr-eq (cdadar *cxr*) (cdr (car (cdr (car *cxr*)))) 5)
  t)

(deftest setf-cxr.24
  (setf-cxr-eq (cdaddr *cxr*) (cdr (car (cdr (cdr *cxr*)))) 5)
  t)

(deftest setf-cxr.25
  (setf-cxr-eq (cddaar *cxr*) (cdr (cdr (car (car *cxr*)))) 5)
  t)

(deftest setf-cxr.26
  (setf-cxr-eq (cddadr *cxr*) (cdr (cdr (car (cdr *cxr*)))) 5)
  t)

(deftest setf-cxr.27
  (setf-cxr-eq (cdddar *cxr*) (cdr (cdr (cdr (car *cxr*)))) 5)
  t)

(deftest setf-cxr.28
  (setf-cxr-eq (cddddr *cxr*) (cdr (cdr (cdr (cdr *cxr*)))) 5)
  t)

(deftest cxr-test.1
  (values
    (caar nil)
    (cddr nil)
    (cdadar nil)
    (cddddr nil))
  nil nil nil nil)


;;
;;  Function FIRST SECOND ... TENTH
;;
(deftest first.1
  (first '(10 20 30))
  10)

(deftest first.2
  (first nil)
  nil)

(defparameter *first* '(1 2 3 (4 5 6) ((V)) vi 7 8 9 10))

(deftest first.3
  (first *first*)
  1)

(deftest first.4
  (tenth *first*)
  10)

(deftest first.5
  (fifth *first*)
  ((V)))

(deftest first.6
  (second (fourth *first*))
  5)

(deftest first.7
  (sixth '(1 2 3))
  nil)

(deftest first.8
  (values
    (setf (fourth *first*) "four")
    *first*)
  "four"
  (1 2 3 "four" ((V)) VI 7 8 9 10))

(deftest first.9
  (second '(10 20 30))
  20)

(deftest first.10
  (fifth '(10 20 30 40 50 60 70))
  50)

(deftest first.11
  (let ((x '(a b c d e f g)))
    (eq (fifth x) (nth 4 x)))
  t)

(deftest-error first.12
  (eval '(first 10))
  type-error)

(deftest-error first.13
  (eval '(eighth 10))
  type-error)

(deftest-error! first.14
  (eval '(first)))

(deftest-error! first.15
  (eval '(first nil nil)))

(deftest-error! first.16
  (eval '(tenth)))

(deftest-error! first.17
  (eval '(tenth nil nil)))

(defparameter *tenth* '(a b c d e f g h i j k l m n o p q r))

(deftest tenth.1
  (eq (first *tenth*) (car *tenth*))
  t)

(deftest tenth.2
  (eq (second *tenth*) (car (cdr *tenth*)))
  t)

(deftest tenth.3
  (eq (third *tenth*) (car (cddr *tenth*)))
  t)

(deftest tenth.4
  (eq (fourth *tenth*) (car (cdddr *tenth*)))
  t)

(deftest tenth.5
  (eq (fifth *tenth*) (car (cddddr *tenth*)))
  t)

(deftest tenth.6
  (eq (sixth *tenth*) (car (cdr (cddddr *tenth*))))
  t)

(deftest tenth.7
  (eq (seventh *tenth*) (car (cddr (cddddr *tenth*))))
  t)

(deftest tenth.8
  (eq (eighth *tenth*) (car (cdddr (cddddr *tenth*))))
  t)

(deftest tenth.9
  (eq (ninth *tenth*) (car (cddddr (cddddr *tenth*))))
  t)

(deftest tenth.10
  (eq (tenth *tenth*) (car (cdr (cddddr (cddddr *tenth*)))))
  t)

(defmacro setf-tenth-eq (x y)
  (let ((g (gensym)))
    `(let ((*tenth* (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n)))
       (setf ,x ',g)
       (eq ,y ',g))))

(deftest setf-tenth.1
  (setf-tenth-eq (first *tenth*) (car *tenth*))
  t)

(deftest setf-tenth.2
  (setf-tenth-eq (second *tenth*) (car (cdr *tenth*)))
  t)

(deftest setf-tenth.3
  (setf-tenth-eq (third *tenth*) (car (cddr *tenth*)))
  t)

(deftest setf-tenth.4
  (setf-tenth-eq (fourth *tenth*) (car (cdddr *tenth*)))
  t)

(deftest setf-tenth.5
  (setf-tenth-eq (fifth *tenth*) (car (cddddr *tenth*)))
  t)

(deftest setf-tenth.6
  (setf-tenth-eq (sixth *tenth*) (car (cdr (cddddr *tenth*))))
  t)

(deftest setf-tenth.7
  (setf-tenth-eq (seventh *tenth*) (car (cddr (cddddr *tenth*))))
  t)

(deftest setf-tenth.8
  (setf-tenth-eq (eighth *tenth*) (car (cdddr (cddddr *tenth*))))
  t)

(deftest setf-tenth.9
  (setf-tenth-eq (ninth *tenth*) (car (cddddr (cddddr *tenth*))))
  t)

(deftest setf-tenth.10
  (setf-tenth-eq (tenth *tenth*) (car (cdr (cddddr (cddddr *tenth*)))))
  t)

