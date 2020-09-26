;;
;;  ANSI COMMON LISP: 14. Conses
;;

;;
;;  Function MAPC
;;
(deftest mapc.1
  (mapc #'1+ '(10 20 30))
  (10 20 30))

(deftest mapc.2
  (mapc #'1+ nil)
  nil)

(deftest mapc.3
  (let (a)
    (mapc (lambda (x) (push x a)) '(10 20 30))
    (nreverse a))
  (10 20 30))

(deftest mapc.4
  (let (a)
    (mapc (lambda (&rest x) (push x a)) '(10 20 30) '(40 50 60))
    (nreverse a))
  ((10 40) (20 50) (30 60)))

(deftest mapc.5
  (let (a)
    (mapc (lambda (&rest x) (push x a)) '(10 20 30) '(40 50))
    (nreverse a))
  ((10 40) (20 50)))

(deftest mapc.6
  (let (a)
    (mapc (lambda (&rest x) (push x a)) '(10) '(40 50 60) '(1 2 3 4))
    a)
  ((10 40 1)))

(deftest mapc.7
  (let (a)
    (mapc (lambda (&rest x) (push x a)) '(10 20 30) nil '(40 50 60))
    a)
  nil)

(deftest mapc.8
  (let (a)
    (mapc (lambda (&rest x) (push x a)) '(10 20 30) '(40 50 60)))
  (10 20 30))

(deftest mapc.9
  (let (a)
    (mapc (lambda (&rest x) (push x a)) '(10 20 30 . 40) '(40 50))
    (nreverse a))
  ((10 40) (20 50)))

(deftest mapc.10
  (let (dummy)
    (values
      (mapc #'(lambda (&rest x) (setq dummy (append dummy x)))
            '(1 2 3 4)
            '(a b c d e)
            '(x y z))
      dummy))
  (1 2 3 4)
  (1 a x 2 b y 3 c z))

(deftest-error! mapc-error.1
  (eval '(mapc 10 nil)))

(deftest-error! mapc-error.2
  (eval '(mapc #'1+ 20)))

(deftest-error! mapc-error.3
  (eval '(mapc #'1+ nil nil nil 20 nil)))

(deftest-error! mapc-error.4
  (eval '(mapc #'1+)))

(deftest-error! mapc-error.5
  (eval '(mapc #'1+ '(10 20 . 30))))


;;
;;  Function MAPCAR
;;
(deftest mapcar.1
  (mapcar #'1+ '(10 20 30))
  (11 21 31))

(deftest mapcar.2
  (mapcar #'1+ nil)
  nil)

(deftest mapcar.3
  (mapcar #'list'(10 20 30) '(40 50 60))
  ((10 40) (20 50) (30 60)))

(deftest mapcar.4
  (mapcar #'list '(10 20 30) '(40 50))
  ((10 40) (20 50)))

(deftest mapcar.5
  (mapcar #'list '(10) '(40 50 60) '(1 2 3 4))
  ((10 40 1)))

(deftest mapcar.6
  (mapcar #'list '(10 20 30) nil '(40 50 60))
  nil)

(deftest mapcar.7
  (mapcar #'list '(10 20 30 . 40) '(40 50))
  ((10 40) (20 50)))

(deftest mapcar.8
  (mapcar #'car '((1 a) (2 b) (3 c)))
  (1 2 3))

(deftest mapcar.9
  (mapcar #'abs '(3 -4 2 -5 -6))
  (3 4 2 5 6))

(deftest mapcar.10
  (mapcar #'cons '(a b c) '(1 2 3))
  ((a . 1) (b . 2) (c . 3)))

(deftest-error! mapcar-error.1
  (eval '(mapcar 10 nil)))

(deftest-error! mapcar-error.2
  (eval '(mapcar #'1+ 20)))

(deftest-error! mapcar-error.3
  (eval '(mapcar #'1+ nil nil nil 20 nil)))

(deftest-error! mapcar-error.4
  (eval '(mapcar #'1+)))

(deftest-error! mapcar-error.5
  (eval '(mapcar #'1+ '(10 20 . 30))))


;;
;;  Function MAPCAN
;;
(deftest mapcan.1
  (mapcan #'list '(10 20 30))
  (10 20 30))

(deftest mapcan.2
  (mapcan #'list nil)
  nil)

(deftest mapcan.3
  (mapcan #'list'(10 20 30) '(40 50 60))
  (10 40 20 50 30 60))

(deftest mapcan.4
  (mapcan #'list '(10 20 30) '(40 50))
  (10 40 20 50))

(deftest mapcan.5
  (mapcan #'list '(10) '(40 50 60) '(1 2 3 4))
  (10 40 1))

(deftest mapcan.6
  (mapcan #'list '(10 20 30) nil '(40 50 60))
  nil)

(deftest mapcan.7
  (mapcan #'list '(10 20 30 . 40) '(40 50))
  (10 40 20 50))

(deftest mapcan.8
  (mapcan #'(lambda (x y) (if (null x) nil (list x y)))
          '(nil nil nil d e)
          '(1 2 3 4 5 6))
  (d 4 e 5))

(deftest mapcan.9
  (mapcan #'(lambda (x) (and (numberp x) (list x)))
          '(a 1 b c 3 4 d 5))
  (1 3 4 5))

(deftest-error! mapcan-error.1
  (eval '(mapcan 10 nil)))

(deftest-error! mapcan-error.2
  (eval '(mapcan #'1+ 20)))

(deftest-error! mapcan-error.3
  (eval '(mapcan #'1+ nil nil nil 20 nil)))

(deftest-error! mapcan-error.4
  (eval '(mapcan #'1+)))

(deftest-error! mapcan-error.5
  (eval '(mapcan #'1+ '(10 20 . 30))))

(deftest mapcan-error.6
  (mapcan #'values '(nil (d e f) (g h i)))
  (d e f g h i))


;;
;;  Function MAPL
;;
(deftest mapl.1
  (mapl #'list '(10 20 30))
  (10 20 30))

(deftest mapl.2
  (mapl #'list nil)
  nil)

(deftest mapl.3
  (let (a)
    (mapl (lambda (x) (push x a)) '(10 20 30))
    (nreverse a))
  ((10 20 30) (20 30) (30)))

(deftest mapl.4
  (let (a)
    (mapl (lambda (&rest x) (push x a)) '(10 20 30) '(40 50 60))
    (nreverse a))
  (((10 20 30) (40 50 60)) ((20 30) (50 60)) ((30) (60))))

(deftest mapl.5
  (let (a)
    (mapl (lambda (&rest x) (push x a)) '(10 20 30) '(40 50))
    (nreverse a))
  (((10 20 30) (40 50)) ((20 30) (50))))

(deftest mapl.6
  (let (a)
    (mapl (lambda (&rest x) (push x a)) '(10) '(40 50 60))
    (nreverse a))
  (((10) (40 50 60))))

(deftest mapl.7
  (let (a)
    (mapl (lambda (&rest x) (push x a)) '(10 20 30) nil '(40 50 60))
    (nreverse a))
  nil)

(deftest mapl.8
  (let (dummy)
    (values
      (mapl #'(lambda (x) (push x dummy)) '(1 2 3 4))
      dummy))
  (1 2 3 4)
  ((4) (3 4) (2 3 4) (1 2 3 4)))

(deftest-error! mapl-error.1
  (eval '(mapl 10 nil)))

(deftest-error! mapl-error.2
  (eval '(mapl #'1+ 20)))

(deftest-error! mapl-error.3
  (eval '(mapl #'1+ nil nil nil 20 nil)))

(deftest-error! mapl-error.4
  (eval '(mapl #'1+)))

(deftest-error! mapl-error.5
  (eval '(mapl #'1+ '(10 20 . 30))))


;;
;;  Function MAPLIST
;;
(deftest maplist.1
  (maplist #'values '(10 20 30))
  ((10 20 30) (20 30) (30)))

(deftest maplist.2
  (maplist #'list nil)
  nil)

(deftest maplist.3
  (maplist #'list '(10 20 30) '(40 50 60))
  (((10 20 30) (40 50 60)) ((20 30) (50 60)) ((30) (60))))

(deftest maplist.4
  (maplist #'list '(10 20 30) '(40 50))
  (((10 20 30) (40 50)) ((20 30) (50))))

(deftest maplist.5
  (maplist #'list '(10) '(40 50 60))
  (((10) (40 50 60))))

(deftest maplist.6
  (maplist #'list '(10 20 30) '(40 50 60) nil)
  nil)

(deftest maplist.7
  (maplist #'append '(1 2 3 4) '(1 2) '(1 2 3))
  ((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3)))

(deftest maplist.8
  (maplist #'(lambda (x) (cons 'foo x)) '(a b c d))
  ((foo a b c d) (foo b c d) (foo c d) (foo d)))

(deftest maplist.9
  (maplist #'(lambda (x) (if (member (car x) (cdr x)) 0 1)) '(a b a c d b c))
  (0 0 1 0 1 1 1))

(deftest-error! maplist-error.1
  (eval '(maplist 10 nil)))

(deftest-error! maplist-error.2
  (eval '(maplist #'1+ 20)))

(deftest-error! maplist-error.3
  (eval '(maplist #'1+ nil nil nil 20 nil)))

(deftest-error! maplist-error.4
  (eval '(maplist #'1+)))

(deftest-error! maplist-error.5
  (eval '(maplist #'1+ '(10 20 . 30))))


;;
;;  Function MAPCON
;;
(deftest mapcon.1
  (mapcon #'list '(10 20 30))
  ((10 20 30) (20 30) (30)))

(deftest mapcon.2
  (mapcon #'list nil)
  nil)

(deftest mapcon.3
  (mapcon #'list '(10 20 30) '(40 50 60))
  ((10 20 30) (40 50 60) (20 30) (50 60) (30) (60)))

(deftest mapcon.4
  (mapcon #'list '(10 20 30) '(40 50))
  ((10 20 30) (40 50) (20 30) (50)))

(deftest mapcon.5
  (mapcon #'list '(10) '(40 50 60))
  ((10) (40 50 60)))

(deftest mapcon.6
  (mapcon #'list '(10 20 30) '(40 50 60) nil)
  nil)

(deftest mapcon.7
  (mapcon #'list '(1 2 3 4))
  ((1 2 3 4) (2 3 4) (3 4) (4)))

(deftest-error! mapcon-error.1
  (eval '(mapcon 10 nil)))

(deftest-error! mapcon-error.2
  (eval '(mapcon #'1+ 20)))

(deftest-error! mapcon-error.3
  (eval '(mapcon #'1+ nil nil nil 20 nil)))

(deftest-error! mapcon-error.4
  (eval '(mapcon #'1+)))

(deftest-error! mapcon-error.5
  (eval '(mapcon #'1+ '(10 20 . 30))))

