;;
;;  ANSI COMMON LISP: 14. Conses
;;

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


;;
;;  Accessor NTH
;;
(deftest nth.1
  (nth 0 nil)
  nil)

(deftest nth.2
  (nth 1 nil)
  nil)

(deftest nth.3
  (nth 0 '(10 20 30 40))
  10)

(deftest nth.4
  (nth 2 '(10 20 30 40))
  30)

(deftest nth.5
  (nth 3 '(10 20 30))
  nil)

(deftest nth.6
  (nth 1 '(a b . c))
  b)

(deftest-error nth-error.1
  (nth 2 '(a b . c)))

(deftest-error nth-error.2
  (eval '(nth 1 #(a b c))))

(deftest-error! nth-error.3
  (eval '(nth -1 nil)))

(deftest-error! nth-error.4
  (eval '(nth 1)))

(deftest-error! nth-error.5
  (eval '(nth 1 nil nil)))


;;
;;  Accessor (SETF NTH)
;;
(deftest setf-nth.1
  (let ((a '(10 20 30)))
    (setf (nth 1 a) 999)
    a)
  (10 999 30))

(deftest setf-nth.2
  (let ((a '(10 20 30)))
    (setf (nth 0 a) 999)
    a)
  (999 20 30))

(deftest setf-nth.3
  (let ((a '(10 20 30)))
    (setf (nth 2 a) 999)
    a)
  (10 20 999))

(deftest-error setf-nth-error.1
  (let ((a '(10 20 30)))
    (setf (nth 3 a) 999)))

(deftest-error setf-nth-error.2
  (let ((a '(10 20 . 30)))
    (setf (nth 2 a) 999)))

(deftest-error setf-nth-error.3
  (let ((a #(10 20 30)))
    (setf (nth 2 a) 999)))

(deftest-error setf-nth-error.4
  (let ((a '(10 20 30)))
    (setf (nth -1 a) 999)))

(deftest-error setf-nth-error.5
  (setf (nth 2) 999))

(deftest-error setf-nth-error.6
  (let ((a '(10 20 30)))
    (setf (nth 2 a a) 999)))

;; ANSI Common Lisp
(deftest nth-test.1
  (nth 0 '(foo bar baz))
  foo)

(deftest nth-test.2
  (nth 1 '(foo bar baz))
  bar)

(deftest nth-test.3
  (nth 3 '(foo bar baz))
  nil)

(defparameter *nth-list* (list 0 1 2 3))

(deftest nth-test.4
  (setf (nth 2 *nth-list*) "two")
  "two")

(deftest nth-test.5
  *nth-list*
  (0 1 "two" 3))


;;
;;  Function NCONC
;;
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

(deftest nconc.8
  (let ((x '(a b c))
        (y '(d e f)))
    (values
      (nconc x y)
      x))
  (a b c d e f)
  (a b c d e f))

(deftest nconc.9
  (let ((x (list 'a 'b 'c 'd 'e))
        (y (list 'f 'g 'h 'i 'j))
        (z (list 'k 'l 'm)))
    (setq x (nconc x y z))
    (values x y z))
  (a b c d e f g h i j k l m)
  (f g h i j k l m)
  (k l m))

(deftest nconc.10
  (let ((x (list 'a 'b 'c 'd 'e))
        (y (list 'f 'g 'h 'i 'j))
        (z (list 'k 'l 'm)))
    (setq x (nconc nil x y nil z))
    (values x y z))
  (a b c d e f g h i j k l m)
  (f g h i j k l m)
  (k l m))

(deftest nconc.11
  (let ((x '(a b c)))
    (list-length (nconc x x)))
  nil)

(deftest-error nconc-error.1
  (nconc 10 20 30))


;;
;;  Function APPEND
;;
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

(deftest append.7
  (let ((x '(t t)))
    (eq (append nil nil x nil nil) x))
  t)

(deftest append.8
  (let ((x '(a b c))
        (y '(d e f)))
    (values
      (append x y)
      x))
  (a b c d e f)
  (a b c))

(deftest append.9
  (let ((x (list 'a 'b 'c 'd 'e))
        (y (list 'f 'g 'h 'i 'j))
        (z (list 'k 'l 'm)))
    (setq x (append x y z))
    (values x y z))
  (a b c d e f g h i j k l m)
  (f g h i j)
  (k l m))

(deftest append.11
  (let ((x '(a b c)))
    (list-length (append x x)))
  6)

(deftest append.12
  (append '(a b c) '(d e f) '() '(g))
  (a b c d e f g))

(deftest append.13
  (append '(a b c) 'd)
  (a b c . d))

(defparameter *append-list* '(a b c))

(deftest append.14
  (append *append-list* '(d))
  (a b c d))

(deftest append.15
  *append-list*
  (a b c))

(deftest append.16
  (append)
  nil)

(deftest append.17
  (append 'a)
  a)

(deftest-error append-error.1
  (append 10 20 30))


;;
;;  Function REVAPPEND
;;
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

(deftest revappend.6
  (let ((x (list 1 2 3))
        (y (list 'a 'b 'c)))
    (values
      (revappend x y)
      (equal x '(1 2 3))
      (equal y '(a b c))))
  (3 2 1 a b c) t t)

(deftest revappend.7
  (revappend '(1 2 3) '())
  (3 2 1))

(deftest revappend.8
  (revappend '(1 2 3) '(a . b))
  (3 2 1 a . b))

(deftest revappend.9
  (revappend '() '(a b c))
  (a b c))

(deftest revappend.10
  (revappend '(1 2 3) 'a)
  (3 2 1 . a))

(deftest revappend.11
  (revappend '() 'a)
  a)

(deftest-error! revappend-error.1
  (eval '(revappend 10 20)))

(deftest-error! revappend-error.2
  (eval '(revappend nil)))

(deftest-error! revappend-error.3
  (eval '(revappend nil nil nil)))

(deftest-error revappend-error.4
  (revappend '(x y z . w) 10))


;;
;;  Function NRECONC
;;
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

(deftest nreconc.5
  (let ((x '(1 2 3))
        (y '(a b c)))
    (values
      (nreconc x y)
      (equal x '(1 2 3))
      (equal y '(a b c))))
  (3 2 1 a b c) nil t)

(deftest nreconc.7
  (nreconc '(1 2 3) '())
  (3 2 1))

(deftest nreconc.8
  (nreconc '(1 2 3) '(a . b))
  (3 2 1 a . b))

(deftest nreconc.9
  (nreconc '() '(a b c))
  (a b c))

(deftest nreconc.10
  (nreconc '(1 2 3) 'a)
  (3 2 1 . a))

(deftest nreconc.11
  (nreconc '() 'a)
  a)

(deftest-error! nreconc-error.1
  (eval '(nreconc 10 20)))

(deftest-error! nreconc-error.2
  (eval '(nreconc nil)))

(deftest-error! nreconc-error.3
  (eval '(nreconc nil nil nil)))

(deftest-error nreconc-error.4
  (nreconc '(x y z . w) 10))


;;
;;  Function BUTLAST
;;
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

(deftest-error! butlast-error.1
  (eval '(butlast 10)))

(deftest-error! butlast-error.2
  (eval '(butlast nil -1))
  type-error)

(deftest-error! butlast-error.3
  (eval '(butlast)))

(deftest-error! butlast-error.4
  (eval '(butlast nil 1 nil)))


;;
;;  Function NBUTLAST
;;
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

(deftest-error! nbutlast-error.1
  (eval '(nbutlast 10)))

(deftest-error! nbutlast-error.2
  (eval '(nbutlast nil -1))
  type-error)

(deftest-error! nbutlast-error.3
  (eval '(nbutlast)))

(deftest-error! nbutlast-error.4
  (eval '(nbutlast nil 1 nil)))

;; ANSI Common Lisp
(defparameter *butlast-list1* '(1 2 3 4 5 6 7 8 9))

(deftest butlast-test.1
  (butlast *butlast-list1*)
  (1 2 3 4 5 6 7 8))

(deftest butlast-test.2
  (butlast *butlast-list1* 5)
  (1 2 3 4))

(deftest butlast-test.3
  (butlast *butlast-list1* (+ 5 5))
  nil)

(deftest butlast-test.4
  *butlast-list1*
  (1 2 3 4 5 6 7 8 9))

(deftest butlast-test.5
  (nbutlast *butlast-list1* 3)
  (1 2 3 4 5 6))

(deftest butlast-test.6
  *butlast-list1*
  (1 2 3 4 5 6))

(deftest butlast-test.7
  (nbutlast *butlast-list1* 99)
  nil)

(deftest butlast-test.8
  *butlast-list1*
  (1 2 3 4 5 6))

(deftest butlast-test.9
  (butlast '(a b c d))
  (a b c))

(deftest butlast-test.10
  (butlast '((a b) (c d)))
  ((a b)))

(deftest butlast-test.11
  (butlast '(a))
  nil)

(deftest butlast-test.12
  (butlast nil)
  nil)

(defparameter *butlast-list2* (list 'a 'b 'c 'd))

(deftest butlast-test.13
  (nbutlast *butlast-list2*)
  (a b c))

(deftest butlast-test.14
  *butlast-list2*
  (a b c))

(deftest butlast-test.15
  (nbutlast (list 'a))
  nil)

(deftest butlast-test.16
  (nbutlast '())
  nil)


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

(deftest-error last-error.1
  (eval '(last :hello)))

(deftest-error last-error.2
  (eval '(last nil -1))
  type-error)

(deftest-error last-error.3
  (eval '(last '(a b c) :hello)))

(deftest-error! last-error.4
  (eval '(last)))

(deftest-error! last-error.5
  (eval '(last nil 10 20)))


;;
;;  Function LDIFF
;;
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

(deftest ldiff.9
  (mapcar
    (lambda (x)
      (mapcar
        (lambda (y) (ldiff x y))
        (list x (cddr x) (copy-list (cddr x)) '(f g h) '() 'd 'x)))
    '((a b c) (a b c . d)))
  ((nil (a b) (a b c) (a b c) (a b c) (a b c) (a b c))
   (nil (a b) (a b c . d) (a b c . d) (a b c . d) (a b c) (a b c . d))))

(deftest-error ldiff-error.1
  (eval '(ldiff 10 20)))

(deftest-error! ldiff-error.2
  (eval '(ldiff nil)))

(deftest-error! ldiff-error.3
  (eval '(ldiff nil nil nil)))


;;
;;  Function TAILP
;;
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

(deftest tailp.7
  (mapcar
    (lambda (x)
      (mapcar
        (lambda (y) (tailp y x))
        (list x (cddr x) (copy-list (cddr x)) '(f g h) '() 'd 'x)))
    '((a b c) (a b c . d)))
  ((t t nil nil t nil nil)
   (t t nil nil nil t nil)))

(deftest-error tailp-error.1
  (eval '(tailp 10 20)))

(deftest-error! tailp-error.2
  (eval '(tailp 10)))

(deftest-error! tailp-error.3
  (eval '(tailp 10 nil nil)))


;;
;;  Function NTHCDR
;;
(deftest nthcdr.1
  (nthcdr 0 '(10 20 30 40))
  (10 20 30 40))

(deftest nthcdr.2
  (nthcdr 2 '(10 20 30 40))
  (30 40))

(deftest nthcdr.3
  (nthcdr 3 '(10 20 30))
  nil)

(deftest nthcdr.4
  (nthcdr 0 '())
  nil)

(deftest nthcdr.5
  (nthcdr 3 '())
  nil)

(deftest nthcdr.6
  (nthcdr 0 '(a b c))
  (a b c))

(deftest nthcdr.7
  (nthcdr 2 '(a b c))
  (c))

(deftest nthcdr.8
  (nthcdr 4 '(a b c))
  ())

(deftest nthcdr.9
  (nthcdr 1 '(0 . 1))
  1)

(deftest-error nthcdr-error.1
  (eval '(locally
           (declare (optimize (safety 3)))
           (nthcdr 3 '(0 . 1)))))

(deftest-error nthcdr-error.2
  (eval '(nthcdr -1 nil))
  type-error)

(deftest-error nthcdr-error.3
  (eval '(nthcdr 0 10)))

(deftest-error! nthcdr-error.4
  (eval '(nthcdr 0)))

(deftest-error! nthcdr-error.5
  (eval '(nthcdr 0 nil nil)))

