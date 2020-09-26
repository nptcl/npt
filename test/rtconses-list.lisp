;;
;;  ANSI COMMON LISP: 14. Conses
;;

;;
;;  Function COPY-TREE
;;
(deftest copy-tree.1
  (copy-tree nil)
  nil)

(deftest copy-tree.2
  (copy-tree 10)
  10)

(deftest copy-tree.3
  (copy-tree '(a b c))
  (a b c))

(deftest copy-tree.4
  (let ((x '(a b c d)))
    (eq (copy-tree x) x))
  nil)

(deftest copy-tree.5
  (copy-tree '(a b . c))
  (a b . c))

(deftest copy-tree.6
  (copy-tree '(a (b (c) (d) ((e))) . c))
  (a (b (c) (d) ((e))) . c))

(deftest copy-tree.7
  (let ((x '(x y z)))
    (eq x (car (copy-tree (list x 'b 'c 'd)))))
  nil)

(deftest-error! copy-tree-error.1
  (eval '(copy-tree)))

(deftest-error! copy-tree-error.2
  (eval '(copy-tree nil nil)))


;;
;;  Function SUBLIS
;;
(deftest sublis.1
  (sublis nil nil)
  nil)

(deftest sublis.2
  (sublis nil 10)
  10)

(deftest sublis.3
  (sublis '((a . b) (c . d)) '(a b c d e f))
  (b b d d e f))

(deftest sublis.4
  (let ((x '(a b c d e f)))
    (eq (sublis '((a . b) (c . d)) '(a b c d e f)) x))
  nil)

(deftest sublis.5
  (sublis '((4 . a) (5 . b)) '(2 3 4 5 6 7 8 9)
          :key (lambda (x)
                 (if (realp x)
                   (1+ x)
                   x)))
  (2 a b 5 6 7 8 9))

(deftest sublis.6
  (sublis '((2 . a) (7 . b)) '(2 3 4 5 6 7 8 9)
          :test (lambda (x y)
                  (and (realp x)
                       (realp y)
                       (zerop (mod (max x y) (min x y))))))
  (a 3 a 5 a b a 9))

(deftest sublis.7
  (sublis '((a . 10)) 'a)
  10)

(deftest sublis.8
  (sublis '((a . 10)) '(a b c)
          :test (lambda (x y)
                  (or (listp x) (listp y))))
  10)

(deftest-error sublis-error.1
  (eval '(sublis :hello nil)))

(deftest-error! sublis-error.2
  (eval '(sublis nil)))

(deftest-error! sublis-error.3
  (eval '(sublis nil nil nil)))

(deftest-error! sublis-error.4
  (eval '(sublis nil nil :test)))

(deftest-error! sublis-error.5
  (eval '(sublis nil nil :hello 10)))

(deftest-error! sublis-error.6
  (eval '(sublis nil nil :key 10)))

(deftest-error! sublis-error.7
  (eval '(sublis nil nil :test (constantly t) :test-not (constantly t))))


;;
;;  Function NSUBLIS
;;
(deftest nsublis.1
  (nsublis nil nil)
  nil)

(deftest nsublis.2
  (nsublis nil 10)
  10)

(deftest nsublis.3
  (nsublis '((a . b) (c . d)) '(a b c d e f))
  (b b d d e f))

(deftest nsublis.4
  (let ((x '(a b c d e f)))
    (eq (nsublis '((a . b) (c . d)) '(a b c d e f)) x))
  nil)

(deftest nsublis.5
  (nsublis '((4 . a) (5 . b)) '(2 3 4 5 6 7 8 9)
           :key (lambda (x)
                  (if (realp x)
                    (1+ x)
                    x)))
  (2 a b 5 6 7 8 9))

(deftest nsublis.6
  (nsublis '((2 . a) (7 . b)) '(2 3 4 5 6 7 8 9)
           :test (lambda (x y)
                   (and (realp x)
                        (realp y)
                        (zerop (mod (max x y) (min x y))))))
  (a 3 a 5 a b a 9))

(deftest nsublis.7
  (nsublis '((a . 10)) 'a)
  10)
(deftest nsublis.8
  (nsublis '((a . 10)) '(a b c)
           :test (lambda (x y)
                   (or (listp x) (listp y))))
  10)
(deftest-error nsublis-error.1
  (eval '(nsublis :hello nil)))

(deftest-error! nsublis-error.2
  (eval '(nsublis nil)))

(deftest-error! nsublis-error.3
  (eval '(nsublis nil nil nil)))

(deftest-error! nsublis-error.4
  (eval '(nsublis nil nil :test)))

(deftest-error! nsublis-error.5
  (eval '(nsublis nil nil :hello 10)))

(deftest-error! nsublis-error.6
  (eval '(nsublis nil nil :key 10)))

(deftest-error! nsublis-error.7
  (eval '(nsublis nil nil :test (constantly t) :test-not (constantly t))))


;;  ANSI Common Lisp
(deftest sublis-test.1
  (sublis '((x . 100) (z . zprime))
          '(plus x (minus g z x p) 4 . x))
  (plus 100 (minus g zprime 100 p) 4 . 100))

(deftest sublis-test.2
  (sublis '(((+ x y) . (- x y)) ((- x y) . (+ x y)))
          '(* (/ (+ x y) (+ x p)) (- x y))
          :test #'equal)
  (* (/ (- x y) (+ x p)) (+ x y)))

(defparameter *sublis-tree1* '(1 (1 2) ((1 2 3)) (((1 2 3 4)))))

(deftest sublis-test.3
  (sublis '((3 . "three")) *sublis-tree1*)
  (1 (1 2) ((1 2 "three")) (((1 2 "three" 4)))))

(deftest sublis-test.4
  (values
    (sublis '((t . "string"))
            (sublis '((1 . "") (4 . 44)) *sublis-tree1*)
            :key #'stringp)
    *sublis-tree1*)
  ("string" ("string" 2) (("string" 2 3)) ((("string" 2 3 44))))
  (1 (1 2) ((1 2 3)) (((1 2 3 4)))))

(defparameter *sublis-tree2* '("one" ("one" "two") (("one" "Two" "three"))))

(deftest sublis-test.5
  (values
    (sublis '(("two" . 2)) *sublis-tree2*)
    *sublis-tree2*)
  ("one" ("one" "two") (("one" "Two" "three")))
  ("one" ("one" "two") (("one" "Two" "three"))))

(deftest sublis-test.6
  (sublis '(("two" . 2)) *sublis-tree2* :test 'equal)
  ("one" ("one" 2) (("one" "Two" "three"))))

(deftest sublis-test.7
  (nsublis '((t . 'temp))
           *sublis-tree1*
           :key #'(lambda (x) (or (atom x) (< (list-length x) 3))))
  ((quote temp) (quote temp) quote temp))

(defun test-sublis-it (fn)
  (let* ((shared-piece (list 'a 'b))
         (data (list shared-piece shared-piece)))
    (funcall fn '((a . b) (b . a)) data)))

(deftest sublis-test.8
  (test-sublis-it #'sublis)
  ((b a) (b a)))

(deftest sublis-test.9
  (test-sublis-it #'nsublis)
  ((a b) (a b)))


;;
;;  Function TREE-EQUAL
;;
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

(deftest tree-equal.11
  (let (list)
    (values
      (tree-equal '(a b c) '(a b (c))
                  :test (lambda (x y)
                          (push x list)
                          (push y list)
                          (equal x y)))
      (nreverse list)))
  nil
  (a a b b))

(deftest tree-equal.12
  (tree-equal '(2 a (5)) '(3 "A" (10))
              :test (lambda (x y) (eq (realp x) (realp y))))
  t)

(deftest tree-equal.13
  (tree-equal '(2 a (5)) '(3 "A" ((10)))
              :test (lambda (x y) (eq (realp x) (realp y))))
  nil)

(deftest tree-equal.14
  (tree-equal '(2 a (5 . 4) . 7) '("a" 3 (#\A) . t)
              :test-not (lambda (x y) (eq (realp x) (realp y))))
  t)

(deftest tree-equal.15
  (tree-equal '(2 a (5 . 4) . 7) '("a" 3 (#\A . 4) . t)
              :test-not (lambda (x y) (eq (realp x) (realp y))))
  nil)

(deftest-error! tree-equal-error.1
  (eval '(tree-equal 10)))

(deftest-error tree-equal-error.2
  (eval '(tree-equal 10 20 30)))

(deftest-error tree-equal-error.3
  (eval '(tree-equal 10 20 :test)))

(deftest-error tree-equal-error.4
  (eval '(tree-equal 10 20 :test 10)))

(deftest-error tree-equal-error.5
  (eval '(tree-equal 10 20 :hello 10)))

(deftest-error tree-equal-error.6
  (eval '(tree-equal 10 20 :test (constantly t) :test-not (constantly t))))


;;
;;  Function COPY-LIST
;;
(deftest copy-list.1
  (copy-list nil)
  nil)

(deftest copy-list.2
  (copy-list '(10 20))
  (10 20))

(deftest copy-list.3
  (copy-list '(10 20 . 30))
  (10 20 . 30))

(deftest copy-list.4
  (let ((x '(10 20)))
    (eq x (copy-list x)))
  nil)

(deftest copy-list.5
  (let ((x '(10 20)))
    (eq x (car (copy-list (list x 30 40)))))
  t)

(defparameter *copy-list1* (list 1 (list 2 3)))
(defparameter *copy-list2* *copy-list1*)
(defparameter *copy-list3* (copy-list *copy-list1*))

(deftest copy-list.6
  (eq *copy-list2* *copy-list1*)
  t)

(deftest copy-list.7
  (eq *copy-list3* *copy-list1*)
  nil)

(deftest copy-list.8
  (equal *copy-list3* *copy-list1*)
  t)

(deftest copy-list.9
  (rplaca *copy-list1* "one")
  ("one" (2 3)))

(deftest copy-list.10
  *copy-list2*
  ("one" (2 3)))

(deftest copy-list.11
  *copy-list3*
  (1 (2 3)))

(deftest copy-list.12
  (setf (caadr *copy-list1*) "two")
  "two")

(deftest copy-list.13
  *copy-list1*
  ("one" ("two" 3)))

(deftest copy-list.14
  *copy-list2*
  ("one" ("two" 3)))

(deftest copy-list.15
  *copy-list3*
  (1 ("two" 3)))

(deftest-error copy-list-error.1
  (eval '(copy-list 10)))

(deftest-error! copy-list-error.2
  (eval '(copy-list)))

(deftest-error! copy-list-error.3
  (eval '(copy-list nil nil)))


;;
;;  Function LIST
;;
(deftest list.1
  (list)
  nil)

(deftest list.2
  (list 10 20 30)
  (10 20 30))


;;
;;  Function LIST*
;;
(deftest list*.1
  (list* 10)
  10)

(deftest list*.2
  (list* 10 20 30)
  (10 20 . 30))

(deftest-error! list*.3
  (eval '(list*)))

;; ANSI Common Lisp
(deftest list-test.1
  (list 1)
  (1))

(deftest list-test.2
  (list* 1)
  1)

(defparameter *list-test1* 1)

(deftest list-test.3
  (list *list-test1* 2)
  (1 2))

(deftest list-test.4
  (list* *list-test1* 2)
  (1 . 2))

(deftest list-test.5
  (list)
  nil)

(deftest list-test.6
  (setq *list-test1* '(1 2))
  (1 2))

(deftest list-test.7
  (eq *list-test1* (list* *list-test1*))
  t)

(deftest list-test.8
  (list 3 4 'a (car '(b . c)) (+ 6 -2))
  (3 4 a b 4))

(deftest list-test.9
  (list* 'a 'b 'c 'd)
  (a b c . d))

(deftest list-test.10
  (cons 'a (cons 'b (cons 'c 'd)))
  (a b c . d))

(deftest list-test.11
  (list* 'a 'b 'c '(d e f))
  (a b c d e f))


;;
;;  Function LIST-LENGTH
;;
(deftest list-length.1
  (list-length nil)
  0)

(deftest list-length.2
  (list-length '(10 20 30))
  3)

(deftest list-length.3
  (list-length (quote #1= (10 20 . #1#)))
  nil)

(deftest-error list-length-error.1
  (eval '(list-length 10)))

(deftest-error! list-length-error.2
  (eval '(list-length)))

(deftest-error! list-length-error.3
  (eval '(list-length nil nil)))

(deftest-error! list-length-error.4
  (eval '(list-length '(a b c . d))))

;; ANSI Common Lisp
(deftest list-length-test.1
  (list-length '(a b c d))
  4)

(deftest list-length-test.2
  (list-length '(a (b c) d))
  3)

(deftest list-length-test.3
  (list-length '())
  0)

(deftest list-length-test.4
  (list-length nil)
  0)

(defun list-length-circular-list (&rest elements)
  (let ((cycle (copy-list elements)))
    (nconc cycle cycle)))

(deftest list-length-test.5
  (list-length (list-length-circular-list 'a 'b))
  nil)

(deftest list-length-test.6
  (list-length (list-length-circular-list 'a))
  nil)

(deftest list-length-test.7
  (list-length (list-length-circular-list))
  0)


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
;;  Function MAKE-LIST
;;
(deftest make-list.1
  (make-list 5)
  (nil nil nil nil nil))

(deftest make-list.2
  (make-list 3 :initial-element 'rah)
  (rah rah rah))

(deftest make-list.3
  (make-list 2 :initial-element '(1 2 3))
  ((1 2 3) (1 2 3)))

(deftest make-list.4
  (make-list 0)
  nil)

(deftest make-list.5
  (make-list 0 :initial-element 'new-element)
  nil)

(deftest-error! make-list-error.1
  (eval '(make-list -1))
  type-error)

(deftest-error! make-list-error.2
  (eval '(make-list)))

(deftest-error! make-list-error.3
  (eval '(make-list :hello)))

(deftest-error make-list-error.4
  (eval '(make-list 10 20)))

(deftest-error make-list-error.5
  (eval '(make-list 0 :initial-element)))

(deftest-error make-list-error.6
  (eval '(make-list 0 :hello t)))


;;
;;  Macro PUSH
;;
(deftest push.1
  (let (x)
    (push 10 x))
  (10))

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

(deftest push.6
  (let ((x (list 10)))
    (push 20 (cdr x))
    (push 30 (cdr x))
    (push 40 (cdr x))
    x)
  (10 40 30 20))

(defparameter *push-list* '(nil))

(deftest push.7
  (push 1 (car *push-list*))
  (1))

(deftest push.8
  *push-list*
  ((1)))

(deftest push.9
  (push 1 (car *push-list*))
  (1 1))

(deftest push.10
  *push-list*
  ((1 1)))

(deftest push.11
  (setq *push-list* '(a (b c) d))
  (a (b c) d))

(deftest push.12
  (push 5 (cadr *push-list*))
  (5 b c))

(deftest push.13
  *push-list*
  (a (5 b c) d))

(deftest-error push-error.1
  (eval '(push)))

(deftest-error push-error.2
  (eval '(push 10)))

(deftest-error push-error.3
  (eval '(let (x) (push 10 x 20))))

(deftest-error push-error.4
  (eval '(push 10 :hello)))


;;
;;  Macro POP
;;
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

(deftest pop.4
  (let ((x '(10 20 30)))
    (values (pop (cdr x)) x))
  20 (10 30))

(defparameter *pop-list* '(a b c))

(deftest pop.5
  (pop *pop-list*)
  a)

(deftest pop.6
  *pop-list*
  (b c))

(deftest pop.7
  (setq *pop-list* '((1 2 3 4)))
  ((1 2 3 4)))

(deftest pop.8
  (pop (car *pop-list*))
  1)

(deftest pop.9
  *pop-list*
  ((2 3 4)))

(deftest-error pop-error.1
  (eval '(pop)))

(deftest-error pop-error.2
  (eval '(let (x) (pop x x))))

(deftest-error pop-error.3
  (eval '(pop :hello)))

(deftest-error pop-error.4
  (eval '(let ((x 10)) (pop x))))


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

(deftest nconc.6
  (let ((x '(t t)))
    (eq (nconc x) x))
  t)

(deftest nconc.7
  (let ((x '(t t)))
    (eq (nconc nil nil x nil nil) x))
  t)

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


;;
;;  Function LAST
;;
(deftest last.1
  (last nil)
  nil)

(deftest last.2
  (last '(10))
  (10))

(deftest last.3
  (last '(1 2 3))
  (3))

(deftest last.4
  (last '(1 2 . 3))
  (2 . 3))

(deftest last.5
  (last '(a b c d))
  (d))

(deftest last.6
  (let ((x '(a b c d)))
    (rplacd (last x) (list 'e 'f))
    x)
  (a b c d e f))

(deftest last.7
  (last '(a b c d e f))
  (f))

(deftest last.8
  (last '(a b c))
  (c))

(deftest last.9
  (values
    (last '(a b c) 0)
    (last '(a b c) 1)
    (last '(a b c) 2)
    (last '(a b c) 3)
    (last '(a b c) 4)
    (last '(a b c) 100000000000000000000000000000000000000))
  nil
  (c)
  (b c)
  (a b c)
  (a b c)
  (a b c))

(deftest last.10
  (values
    (last '(a . b) 0)
    (last '(a . b) 1)
    (last '(a . b) 2)
    (last '(a . b) 100000000000000000000000000000000000000))
  b
  (a . b)
  (a . b)
  (a . b))

(deftest-error last.11
  (eval '(last :hello)))

(deftest-error last.12
  (eval '(last '(a b c) :hello)))

(deftest-error! last.13
  (eval '(last)))

(deftest-error! last.14
  (eval '(last nil 10 20)))


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

(deftest mapcan-error.7
  (mapcan #'values '(nil (d e f) (g h i)))
  (d e f g h i))

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

