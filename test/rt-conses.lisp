;;
;;  ANSI COMMON LISP: 14. Conses
;;
(deftest cons.1
  (cons 10 20)
  (10 . 20))

(deftest cons.2
  (cons nil nil)
  (nil))

(deftest consp.1
  (consp '(10 20 30))
  t)

(deftest consp.2
  (consp 10)
  nil)

(deftest consp.3
  (consp nil)
  nil)

(deftest atom.1
  (atom 10)
  t)

(deftest atom.2
  (atom '(10 20 30))
  nil)

(deftest atom.3
  (atom nil)
  t)

(deftest rplaca.1
  (let ((a (cons 10 20)))
    (rplaca a 99)
    a)
  (99 . 20))

(deftest rplaca.2
  (let ((a (cons 10 20)))
    (rplaca a 99))
  (99 . 20))

(deftest rplacd.1
  (let ((a (cons 10 20)))
    (rplacd a 99)
    a)
  (10 . 99))

(deftest rplacd.2
  (let ((a (cons 10 20)))
    (rplacd a 99))
  (10 . 99))

(deftest car.1
  (car '(10 . 20))
  10)

(deftest cdr.1
  (cdr '(10 . 20))
  20)

(deftest setf-car.1
  (let ((a '(10 20 30)))
    (setf (car a) 999)
    a)
  (999 20 30))

(deftest setf-cdr.1
  (let ((a '(10 20 30)))
    (setf (cdr a) 999)
    a)
  (10 . 999))

(deftest first.1
  (first '(10 20 30))
  10)

(deftest second.1
  (second '(10 20 30))
  20)

(deftest fifth.1
  (fifth '(10 20 30 40 50 60 70))
  50)

(deftest sublis.1
  (sublis nil nil)
  nil)

(deftest sublis.2
  (sublis '((a . b) (c . d)) '(a b c d e f))
  (b b d d e f))

(deftest nsublis.1
  (nsublis nil nil)
  nil)

(deftest nsublis.2
  (nsublis '((a . b) (c . d)) '(a b c d e f))
  (b b d d e f))

(deftest subst.1
  (subst 10 20 nil)
  nil)

(deftest subst.2
  (subst 'z 'a '(a b c (d e f (a))))
  (z b c (d e f (z))))

(deftest nsubst.1
  (nsubst 10 20 nil)
  nil)

(deftest nsubst.2
  (nsubst 'z 'a '(a b c (d e f (a))))
  (z b c (d e f (z))))

(deftest subst-if.1
  (subst-if 10 (constantly nil) nil)
  nil)

(deftest subst-if.2
  (subst-if 10 (constantly nil) '(a b c (d e (f)) g))
  (a b c (d e (f)) g))

(deftest subst-if.3
  (subst-if 10 (lambda (x) (eq x 'd)) '(a b c (d e (f)) g))
  (a b c (10 e (f)) g))

(deftest nsubst-if.1
  (nsubst-if 10 (constantly nil) nil)
  nil)

(deftest nsubst-if.2
  (nsubst-if 10 (constantly nil) '(a b c (d e (f)) g))
  (a b c (d e (f)) g))

(deftest nsubst-if.3
  (nsubst-if 10 (lambda (x) (eq x 'd)) '(a b c (d e (f)) g))
  (a b c (10 e (f)) g))

(deftest subst-if-not.1
  (subst-if-not 10 (constantly t) nil)
  nil)

(deftest subst-if-not.2
  (subst-if-not 10 (constantly t) '(a b c (d e (f)) g))
  (a b c (d e (f)) g))

(deftest subst-if-not.3
  (subst-if-not 10 (lambda (x) (not (eq x 'd))) '(a b c (d e (f)) g))
  (a b c (10 e (f)) g))

(deftest nsubst-if-not.1
  (nsubst-if-not 10 (constantly t) nil)
  nil)

(deftest nsubst-if-not.2
  (nsubst-if-not 10 (constantly t) '(a b c (d e (f)) g))
  (a b c (d e (f)) g))

(deftest nsubst-if-not.3
  (nsubst-if-not 10 (lambda (x) (not (eq x 'd))) '(a b c (d e (f)) g))
  (a b c (10 e (f)) g))

(deftest tree-equal.1
  (tree-equal nil nil)
  t)

(deftest tree-equal.2
  (tree-equal 10 10)
  t)

(deftest tree-equal.3
  (tree-equal 10 20)
  nil)

(deftest tree-equal.4
  (tree-equal '(10) 10)
  nil)

(deftest tree-equal.5
  (tree-equal 10 '(10))
  nil)

(deftest tree-equal.6
  (tree-equal '(10) '(10))
  t)

(deftest tree-equal.7
  (tree-equal '(20) '(10))
  nil)

(deftest tree-equal.8
  (tree-equal '(a b (c d) e) '(a b (c d) e))
  t)

(deftest tree-equal.9
  (tree-equal '(a b (c d . f) e) '(a b (c d) e))
  nil)

(deftest push.1
  (let (x)
    (push 10 x))
  (10))

(deftest list.1
  (list)
  nil)

(deftest list.2
  (list 10 20 30)
  (10 20 30))

(deftest list*.1
  (list* 10)
  10)

(deftest list*.2
  (list* 10 20 30)
  (10 20 . 30))

(deftest list-length.1
  (list-length nil)
  0)

(deftest list-length.2
  (list-length '(10 20 30))
  3)

(deftest list-length.3
  (list-length (quote #1= (10 20 . #1#)))
  nil)

(deftest listp.1
  (listp nil)
  t)

(deftest listp.2
  (listp '(10 . 20))
  t)

(deftest listp.3
  (listp 10)
  nil)

(deftest make-list.1
  (make-list 3)
  (nil nil nil))

(deftest make-list.2
  (make-list 4 :initial-element 10)
  (10 10 10 10))

(deftest push.2
  (let (x)
    (push 10 x)
    x)
  (10))

(deftest push.3
  (let (x)
    (push 10 x)
    (push 20 x))
  (20 10))

(deftest push.4
  (let (x)
    (push 10 x)
    (push 20 x)
    x)
  (20 10))

(deftest push.5
  (let (x)
    (push 10 x)
    (push 20 x)
    (push 30 x))
  (30 20 10))

(deftest pop.1
  (let (x)
    (pop x))
  nil)

(deftest pop.2
  (let (x)
    (pop x)
    x)
  nil)

(deftest pop.3
  (let ((x '(10 20 30)))
    (values (pop x) x))
  10 (20 30))

(deftest nth.1
  (nth 0 '(10 20 30 40))
  10)

(deftest nth.2
  (nth 2 '(10 20 30 40))
  30)

(deftest nth.3
  (nth 3 '(10 20 30))
  nil)

(deftest setf-nth.1
  (let ((a '(10 20 30)))
    (setf (nth 1 a) 999)
    a)
  (10 999 30))

(deftest endp.1
  (endp nil)
  t)

(deftest endp.2
  (endp '(10 20 30))
  nil)

(deftest null.1
  (null nil)
  t)

(deftest null.2
  (null '(10 20 30))
  nil)

(deftest null.3
  (null "Hello")
  nil)

(deftest nconc.1
  (nconc)
  nil)

(deftest nconc.2
  (nconc nil)
  nil)

(deftest nconc.3
  (nconc 10)
  10)

(deftest nconc.4
  (nconc (list 10 20 30) 40)
  (10 20 30 . 40))

(deftest nconc.5
  (nconc (list 10 20) (list 30 40 50) (list 60))
  (10 20 30 40 50 60))

(deftest append.1
  (append)
  nil)

(deftest append.2
  (append nil)
  nil)

(deftest append.3
  (append 10)
  10)

(deftest append.4
  (append (list 10 20 30) 40)
  (10 20 30 . 40))

(deftest append.5
  (append (list 10 20) (list 30 40 50) (list 60))
  (10 20 30 40 50 60))

(deftest append.6
  (let ((a (list 10 20)))
    (append a (list 30 40) 50)
    a)
  (10 20))

(deftest revappend.1
  (revappend nil nil)
  nil)

(deftest revappend.2
  (revappend nil 10)
  10)

(deftest revappend.3
  (revappend (list 10 20 30) 999)
  (30 20 10 . 999))

(deftest revappend.4
  (revappend (list 'c 'b 'a) (list 'd 'e 'f))
  (a b c d e f))

(deftest revappend.5
  (let ((a (list 'a 'b 'c)))
    (revappend a 10)
    a)
  (a b c))

(deftest nreconc.1
  (nreconc nil nil)
  nil)

(deftest nreconc.2
  (nreconc nil 10)
  10)

(deftest nreconc.3
  (nreconc (list 10 20 30) 999)
  (30 20 10 . 999))

(deftest nreconc.4
  (nreconc (list 'c 'b 'a) (list 'd 'e 'f))
  (a b c d e f))

(deftest butlast.1
  (butlast nil)
  nil)

(deftest butlast.2
  (butlast '(10))
  nil)

(deftest butlast.3
  (butlast '(10 20 30))
  (10 20))

(deftest butlast.4
  (let ((a (list 10 20 30)))
    (butlast a)
    a)
  (10 20 30))

(deftest butlast.5
  (values
    (butlast '(a b c) 0)
    (butlast '(a b c) 1)
    (butlast '(a b c) 2)
    (butlast '(a b c) 3)
    (butlast '(a b c) 4)
    (butlast '(a b c) 100000000000000000000000000000000000000))
  (a b c) (a b) (a) nil nil nil)

(deftest nbutlast.1
  (nbutlast nil)
  nil)

(deftest nbutlast.2
  (nbutlast '(10))
  nil)

(deftest nbutlast.3
  (nbutlast '(10 20 30))
  (10 20))

(deftest nbutlast.4
  (let ((a (list 10 20 30)))
    (nbutlast a)
    a)
  (10 20))

(deftest nbutlast.5
  (values
    (nbutlast (list 'a 'b 'c) 0)
    (nbutlast (list 'a 'b 'c) 1)
    (nbutlast (list 'a 'b 'c) 2)
    (nbutlast (list 'a 'b 'c) 3)
    (nbutlast (list 'a 'b 'c) 4)
    (nbutlast (list 'a 'b 'c) 100000000000000000000000000000000000000))
  (a b c) (a b) (a) nil nil nil)

(deftest last.1
  (last nil)
  nil)

(deftest last.2
  (last '(10))
  (10))

(deftest last.3
  (last '(a b c d))
  (d))

(deftest last.4
  (values
    (last '(a b c) 0)
    (last '(a b c) 1)
    (last '(a b c) 2)
    (last '(a b c) 3)
    (last '(a b c) 4)
    (last '(a b c) 100000000000000000000000000000000000000))
  nil (c) (b c) (a b c) (a b c) (a b c))

(deftest ldiff.1
  (ldiff nil nil)
  nil)

(deftest ldiff.2
  (ldiff '(a b c) nil)
  (a b c))

(deftest ldiff.3
  (ldiff nil '(a b c))
  nil)

(deftest ldiff.4
  (let ((a '(a b c)))
    (ldiff a a))
  nil)

(deftest ldiff.5
  (ldiff '(a b c) '(c))
  (a b c))

(deftest ldiff.6
  (let* ((c '(c))
         (abc (list* 'a 'b c)))
    (ldiff abc c))
  (a b))

(deftest ldiff.7
  (let* ((bc '(b c))
         (abc (list* 'a bc)))
    (ldiff abc bc))
  (a))

(deftest ldiff.8
  (let ((a '(a b c)))
    (ldiff a a))
  nil)

(deftest tailp.1
  (tailp nil nil)
  t)

(deftest tailp.2
  (tailp '(a b c) nil)
  nil)

(deftest tailp.3
  (tailp nil '(a b c))
  t)

(deftest tailp.4
  (tailp '(c) '(a b c))
  nil)

(deftest tailp.5
  (let* ((c '(c))
         (abc (list* 'a 'b c)))
    (tailp c abc))
  t)

(deftest tailp.6
  (let ((a '(a b c)))
    (tailp a a))
  t)

(deftest nthcdr.1
  (nthcdr 0 '(10 20 30 40))
  (10 20 30 40))

(deftest nthcdr.2
  (nthcdr 2 '(10 20 30 40))
  (30 40))

(deftest nthcdr.3
  (nthcdr 3 '(10 20 30))
  nil)

(deftest member.1
  (member 20 '(10 20 30 40))
  (20 30 40))

(deftest member.2
  (member 999 '(10 20 30 40))
  nil)

(deftest member.3
  (member 31 '(10 20 30 40) :key #'1+)
  (30 40))

(deftest member.4
  (member '(b) '((a) (b) (c) (d)) :test #'equal)
  ((b) (c) (d)))

(deftest member.5
  (member '(a) '((a) (b) (c) (d)) :test-not #'equal)
  ((b) (c) (d)))

(deftest member-if.1
  (member-if (lambda (x) (eq 'b x)) '(a b c d))
  (b c d))

(deftest member-if.2
  (member-if (lambda (x) (eq 'z x)) '(a b c d))
  nil)

(deftest member-if.3
  (member-if (lambda (x) (eql 21 x)) '(10 20 30 40) :key #'1+)
  (20 30 40))

(deftest member-if-not.1
  (member-if-not (lambda (x) (eq 'a x)) '(a b c d))
  (b c d))

(deftest member-if-not.2
  (member-if-not (lambda (x) x) '(a b c d))
  nil)

(deftest member-if-not.3
  (member-if-not (lambda (x) (eql 11 x)) '(10 20 30 40) :key #'1+)
  (20 30 40))

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

(deftest acons.1
  (acons 10 20 nil)
  ((10 . 20)))

(deftest acons.2
  (acons 10 20 '(a b c))
  ((10 . 20) a b c))

(deftest assoc.1
  (assoc 10 nil)
  nil)

(deftest assoc.2
  (assoc 'y '((x . 10) (y . 20) (z . 3)))
  (y . 20))

(deftest assoc.3
  (assoc 'a '((x . 10) (y . 20) (z . 3)))
  nil)

(deftest assoc.4
  (assoc 21 '((10 . a) (20 . b) (30 . c)) :key #'1+ :test #'eql)
  (20 . b))

(deftest assoc.5
  (assoc '(10) '((10 . a) ((10) . b) (30 . c)) :test #'equal)
  ((10) . b))

(deftest assoc-if.1
  (assoc-if (lambda (x) (eql x 3)) '((1 . a) (2 . b) (3 . c)))
  (3 . c))

(deftest assoc-if.2
  (assoc-if (lambda (x) (eql x 3)) nil)
  nil)

(deftest assoc-if.3
  (assoc-if (lambda (x) (eql x 100)) '((1 . a) (2 . b) (3 . c)))
  nil)

(deftest assoc-if-not.1
  (assoc-if-not (lambda (x) (eql x 1)) '((1 . a) (2 . b) (3 . c)))
  (2 . b))

(deftest assoc-if-not.2
  (assoc-if-not (lambda (x) (eql x 1)) nil)
  nil)

(deftest assoc-if-not.3
  (assoc-if-not (lambda (x) (numberp x)) '((1 . a) (2 . b) (3 . c)))
  nil)

(deftest copy-alist.1
  (copy-alist nil)
  nil)

(deftest copy-alist.2
  (copy-alist '((a . b) (c . d)))
  ((a . b) (c . d)))

(deftest copy-alist.3
  (let ((a '((a . b) (c . d))))
    (eq (copy-alist a) a))
  nil)

(deftest copy-alist.4
  (let* ((a '(a . b))
         (b (list a '(c . d))))
    (eq (car (copy-alist b)) a))
  nil)

(deftest copy-alist.5
  (let* ((a '(a . b))
         (b (list a '(c . d))))
    (equal (car (copy-alist b)) a))
  t)

(deftest pairlis.1
  (pairlis '(a b) '(10 20))
  ((b . 20) (a . 10)))

(deftest pairlis.2
  (pairlis nil nil)
  nil)

(deftest pairlis.3
  (pairlis '(a b) '(10 20) '((c . 30)))
  ((b . 20) (a . 10) (c . 30)))

(deftest rassoc.1
  (rassoc 10 nil)
  nil)

(deftest rassoc.2
  (rassoc '20 '((x . 10) (y . 20) (z . 3)))
  (y . 20))

(deftest rassoc.3
  (rassoc 'a '((x . 10) (y . 20) (z . 3)))
  nil)

(deftest rassoc.4
  (rassoc 21 '((a . 10) (b . 20) (c . 30)) :key #'1+ :test #'eql)
  (b . 20))

(deftest rassoc.5
  (rassoc '(10) '((a . 10) (b . (10)) (c . 30)) :test #'equal)
  (b . (10)))

(deftest rassoc-if.1
  (rassoc-if (lambda (x) (eql x 'c)) '((1 . a) (2 . b) (3 . c)))
  (3 . c))

(deftest rassoc-if.2
  (rassoc-if (lambda (x) (eql x 3)) nil)
  nil)

(deftest rassoc-if.3
  (rassoc-if (lambda (x) (eql x 100)) '((1 . a) (2 . b) (3 . c)))
  nil)

(deftest rassoc-if-not.1
  (rassoc-if-not (lambda (x) (eql x 'a)) '((1 . a) (2 . b) (3 . c)))
  (2 . b))

(deftest rassoc-if-not.2
  (rassoc-if-not (lambda (x) (eql x 1)) nil)
  nil)

(deftest rassoc-if-not.3
  (rassoc-if-not (lambda (x) (symbolp x)) '((1 . a) (2 . b) (3 . c)))
  nil)

(deftest get-properties.1
  (get-properties '(a b c d e f) '(c e))
  c d (c d e f))

(deftest get-properties.2
  (get-properties '(a b c d e f) '(e c))
  c d (c d e f))

(deftest get-properties.3
  (get-properties '(a b c d e f) '(z q))
  nil nil nil)

(deftest getf.1
  (getf nil nil)
  nil)

(deftest getf.2
  (getf nil 10)
  nil)

(deftest getf.3
  (getf '(a b c d) 'c)
  d)

(deftest getf.4
  (getf '(a b c d) 'z)
  nil)

(deftest getf.5
  (getf '(a b c d) 'z :hello)
  :hello)

(deftest getf.6
  (getf '(a b c d) 'a :hello)
  b)

(deftest getf.7
  (getf '(a b a d) 'a)
  b)

(deftest setf-getf.1
  (let (x)
    (setf (getf x 'a) 10)
    x)
  (a 10))

(deftest setf-getf.2
  (let (x)
    (setf (getf x 'a) 10)
    (setf (getf x 'a) 20)
    x)
  (a 20))

(deftest setf-getf.3
  (let (x)
    (setf (getf x 'a) 10)
    (setf (getf x 'b) 20)
    x)
  (b 20 a 10))

(deftest setf-getf.4
  (let (x y)
    (setf (getf x 'a (setq y 999)) 10)
    x)
  (a 10))

(deftest setf-getf.5
  (let (x y)
    (setf (getf x 'a (setq y 999)) 10)
    y)
  999)

(deftest remf.1
  (let (x)
    (remf x 'a))
  nil)

(deftest remf.2
  (let ((x '(a b c d e f)))
    (remf x 'a))
  t)

(deftest remf.3
  (let ((x '(a b c d e f)))
    (remf x 'a)
    x)
  (c d e f))

(deftest remf.4
  (let ((x '(a b c d e f)))
    (remf x 'c))
  t)

(deftest remf.5
  (let ((x '(a b c d e f)))
    (remf x 'c)
    x)
  (a b e f))

(deftest remf.6
  (let ((x '(a b c d e f)))
    (remf x 'z))
  nil)

(deftest remf.7
  (let ((x '(a b c d e f)))
    (remf x 'z)
    x)
  (a b c d e f))

(deftest intersection.1
  (intersection nil nil)
  nil)

(deftest intersection.2
  (intersection '(a b c) nil)
  nil)

(deftest intersection.3
  (intersection nil '(a b c))
  nil)

(deftest intersection.4
  (intersection '(a b c) '(d e f g))
  nil)

(deftest intersection.5
  (intersection '(a b c) '(d b c g))
  (c b))

(deftest intersection.6
  (intersection '(a b c) '(d b g))
  (b))

(deftest nintersection.1
  (nintersection nil nil)
  nil)

(deftest nintersection.2
  (nintersection '(a b c) nil)
  nil)

(deftest nintersection.3
  (nintersection nil '(a b c))
  nil)

(deftest nintersection.4
  (nintersection '(a b c) '(d e f g))
  nil)

(deftest nintersection.5
  (nintersection '(a b c) '(d b c g))
  (b c))

(deftest nintersection.6
  (nintersection '(a b c) '(d b g))
  (b))

(deftest nintersection.7
  (nintersection '(a b c d) '(a c g))
  (a c))

(deftest nintersection.8
  (nintersection '(a b c d) '(b d g))
  (b d))

(deftest adjoin.1
  (adjoin 'a nil)
  (a))

(deftest adjoin.2
  (adjoin 'b '(a b c d))
  (a b c d))

(deftest adjoin.3
  (adjoin 'z '(a b c d))
  (z a b c d))

(deftest pushnew.1
  (let (x)
    (pushnew 'a x))
  (a))

(deftest pushnew.2
  (let (x)
    (pushnew 'a x)
    x)
  (a))

(deftest pushnew.3
  (let (x)
    (pushnew 'a x)
    (pushnew 'b x))
  (b a))

(deftest pushnew.4
  (let (x)
    (pushnew 'a x)
    (pushnew 'b x)
    x)
  (b a))

(deftest pushnew.5
  (let ((x '(a b c d)))
    (pushnew 'c x))
  (a b c d))

(deftest pushnew.6
  (let ((x '(a b c d)))
    (pushnew 'c x)
    x)
  (a b c d))

(deftest pushnew.7
  (let ((x '(a b c d)))
    (pushnew 'z x))
  (z a b c d))

(deftest pushnew.8
  (let ((x '(a b c d)))
    (pushnew 'z x)
    x)
  (z a b c d))

(deftest set-difference.1
  (set-difference nil nil)
  nil)

(deftest set-difference.2
  (set-difference '(a b c) nil)
  (c b a))

(deftest set-difference.3
  (set-difference nil '(a b c))
  nil)

(deftest set-difference.4
  (set-difference '(a b c) '(d e f g))
  (c b a))

(deftest set-difference.5
  (set-difference '(a b c) '(d e a c f g))
  (b))

(deftest set-difference.6
  (let ((x '(a b c)))
    (set-difference x '(d e a b c f g))
    x)
  (a b c))

(deftest nset-difference.1
  (nset-difference nil nil)
  nil)

(deftest nset-difference.2
  (nset-difference '(a b c) nil)
  (a b c))

(deftest nset-difference.3
  (nset-difference nil '(a b c))
  nil)

(deftest nset-difference.4
  (nset-difference '(a b c) '(d e f g))
  (a b c))

(deftest nset-difference.5
  (nset-difference '(a b c) '(d e a c f g))
  (b))

(deftest set-exclusive-or.1
  (set-exclusive-or nil nil)
  nil)

(deftest set-exclusive-or.2
  (set-exclusive-or '(a b c) nil)
  (c b a))

(deftest set-exclusive-or.3
  (set-exclusive-or nil '(a b c))
  (c b a))

(deftest set-exclusive-or.4
  (set-exclusive-or '(a b) '(c d e))
  (e d c b a))

(deftest set-exclusive-or.5
  (set-exclusive-or '(a c) '(c d e))
  (e d a))

(deftest set-exclusive-or.6
  (set-exclusive-or '(a a a b) '(c d e))
  (e d c b a a a))

(deftest set-exclusive-or.7
  (set-exclusive-or '(a b) '(c d d e))
  (e d d c b a))

(deftest set-exclusive-or.8
  (set-exclusive-or '(a b c) '(a b c))
  nil)

(deftest nset-exclusive-or.1
  (nset-exclusive-or nil nil)
  nil)

(deftest nset-exclusive-or.2
  (nset-exclusive-or '(a b c) nil)
  (a b c))

(deftest nset-exclusive-or.3
  (nset-exclusive-or nil '(a b c))
  (c b a))

(deftest nset-exclusive-or.4
  (nset-exclusive-or '(a b) '(c d e))
  (e d c a b))

(deftest nset-exclusive-or.5
  (nset-exclusive-or '(a c) '(c d e))
  (e d a))

(deftest nset-exclusive-or.6
  (nset-exclusive-or '(a a a b) '(c d e))
  (e d c a a a b))

(deftest nset-exclusive-or.7
  (nset-exclusive-or '(a b) '(c d d e))
  (e d d c a b))

(deftest nset-exclusive-or.8
  (nset-exclusive-or '(a b c) '(a b c))
  nil)

(deftest subsetp.1
  (subsetp nil nil)
  t)

(deftest subsetp.2
  (subsetp '(a b c) nil)
  nil)

(deftest subsetp.3
  (subsetp nil '(a b c))
  t)

(deftest subsetp.4
  (subsetp '(a b c) '(z a b c d e))
  t)

(deftest subsetp.5
  (subsetp '(a b c) '(z a b d e))
  nil)

(deftest union.1
  (union nil nil)
  nil)

(deftest union.2
  (union '(a b c) nil)
  (c b a))

(deftest union.3
  (union nil '(a b c))
  (c b a))

(deftest union.4
  (union '(a b c) '(d e))
  (e d c b a))

(deftest union.5
  (union '(a b b b b c) '(d e))
  (e d c b a))

(deftest union.6
  (union '(a b b b b c) '(b b b d d d e))
  (e d c b a))

(deftest nunion.1
  (nunion nil nil)
  nil)

(deftest nunion.2
  (nunion '(a b c) nil)
  (a b c))

(deftest nunion.3
  (nunion nil '(a b c))
  (c b a))

(deftest nunion.4
  (nunion '(a b c) '(d e))
  (e d a b c))

(deftest nunion.5
  (nunion '(a b b b b c) '(d e))
  (e d a b c))

(deftest nunion.6
  (nunion '(a b b b b c) '(b b b d d d e))
  (e d a b c))


;;
;;  do-tests
;;
(do-tests :test t)

