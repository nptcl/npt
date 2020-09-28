;;
;;  ANSI COMMON LISP: 14. Conses
;;

;;
;;  Function INTERSECTION
;;
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

(deftest intersection.7
  (intersection '(1 99 6 9) '(1 2 3 4 5 6 7)
                :key (lambda (x) (if (eql x 3) 99 x)))
  (6 99 1))

(deftest intersection.8
  (intersection '((1) (2) (3)) '((2) (5) (7) (3)) :test #'equal)
  ((3) (2)))

(deftest intersection.9
  (intersection '((1) (2) (3)) '((2) (5) (7) (3))
                :test-not (lambda (x y) (not (equal x y))))
  ((3) (2)))

(deftest-error intersection-error.1
  (eval '(intersection 10 nil)))

(deftest-error intersection-error.2
  (eval '(intersection nil 20)))

(deftest-error! intersection-error.3
  (eval '(intersection nil)))

(deftest-error! intersection-error.4
  (eval '(intersection nil nil nil)))

(deftest-error! intersection-error.5
  (eval '(intersection nil nil :key 10)))

(deftest-error! intersection-error.6
  (eval '(intersection nil nil :hello 10)))

(deftest-error! intersection-error.7
  (eval '(intersection nil nil :test (constantly t) :test-not (constantly t))))


;;
;;  Function NINTERSECTION
;;
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

(deftest nintersection.9
  (nintersection '(1 99 6 9) '(1 2 3 4 5 6 7)
                 :key (lambda (x) (if (eql x 3) 99 x)))
  (1 99 6))

(deftest nintersection.10
  (nintersection '((1) (2) (3)) '((2) (5) (7) (3)) :test #'equal)
  ((2) (3)))

(deftest nintersection.11
  (nintersection '((1) (2) (3)) '((2) (5) (7) (3))
                 :test-not (lambda (x y) (not (equal x y))))
  ((2) (3)))

(deftest-error nintersection-error.1
  (eval '(nintersection 10 nil)))

(deftest-error nintersection-error.2
  (eval '(nintersection nil 20)))

(deftest-error! nintersection-error.3
  (eval '(nintersection nil)))

(deftest-error! nintersection-error.4
  (eval '(nintersection nil nil nil)))

(deftest-error! nintersection-error.5
  (eval '(nintersection nil nil :key 10)))

(deftest-error! nintersection-error.6
  (eval '(nintersection nil nil :hello 10)))

(deftest-error! nintersection-error.7
  (eval '(nintersection nil nil :test (constantly t) :test-not (constantly t))))

;; ANSI Common Lisp
(defparameter *intersection-list1* (list 1 1 2 3 4 'a 'b 'c "A" "B" "C" "d"))
(defparameter *intersection-list2* (list 1 4 5 'b 'c 'd "a" "B" "c" "D"))

(deftest intersection-test.1
  (intersection *intersection-list1* *intersection-list2*)
  (c b 4 1 1))

(deftest intersection-test.2
  (intersection *intersection-list1* *intersection-list2* :test 'equal)
  ("B" c b 4 1 1))

(deftest intersection-test.3
  (intersection *intersection-list1* *intersection-list2* :test #'equalp)
  ("d" "C" "B" "A" c b 4 1 1))

(deftest intersection-test.4
  (nintersection *intersection-list1* *intersection-list2*)
  (1 1 4 b c))

(deftest intersection-test.5
  *intersection-list1*
  (1 1 4 b c))

(deftest intersection-test.6
  *intersection-list2*
  (1 4 5 b c d "a" "B" "c" "D"))

(deftest intersection-test.7
  (progn
    (setq *intersection-list1* (copy-list '((1 . 2) (2 . 3) (3 . 4) (4 . 5))))
    (setq *intersection-list2* (copy-list '((1 . 3) (2 . 4) (3 . 6) (4 . 8))))
    (nintersection *intersection-list1* *intersection-list2* :key #'cdr))
  ((2 . 3) (3 . 4)))

(deftest intersection-test.8
  *intersection-list1*
  ((1 . 2) (2 . 3) (3 . 4)))

(deftest intersection-test.9
  *intersection-list2*
  ((1 . 3) (2 . 4) (3 . 6) (4 . 8)))


;;
;;  Function ADJOIN
;;
(deftest adjoin.1
  (adjoin 'a nil)
  (a))

(deftest adjoin.2
  (adjoin 'b '(a b c d))
  (a b c d))

(deftest adjoin.3
  (adjoin 'z '(a b c d))
  (z a b c d))

(deftest adjoin.4
  (adjoin '3 '(3 5 7 9) :key #'1+)
  (3 5 7 9))

(deftest adjoin.5
  (adjoin '4 '(3 5 7 9) :key #'1+)
  (4 3 5 7 9))

(deftest adjoin.6
  (adjoin '5 '(3 5 7 9) :key #'1+)
  (3 5 7 9))

(deftest adjoin.7
  (adjoin '5 '(a b c d) :test (lambda (x y) (and (realp x) (realp y))))
  (5 a b c d))

(deftest adjoin.8
  (adjoin '5 '(a b 4 d) :test (lambda (x y) (and (realp x) (realp y))))
  (a b 4 d))

(deftest adjoin.9
  (adjoin '5 '(a b c d) :test-not (lambda (x y) (not (and (realp x) (realp y)))))
  (5 a b c d))

(deftest adjoin.10
  (adjoin '5 '(a b c 1) :test-not (lambda (x y) (not (and (realp x) (realp y)))))
  (a b c 1))

(deftest-error adjoin-error.1
  (eval '(adjoin 10 20))
  type-error)

(deftest-error! adjoin-error.2
  (eval '(adjoin 10)))

(deftest-error! adjoin-error.3
  (eval '(adjoin 10 nil 30)))

(deftest-error! adjoin-error.4
  (eval '(adjoin 10 nil :key)))

(deftest-error! adjoin-error.5
  (eval '(adjoin 10 nil :key 10)))

(deftest-error! adjoin-error.6
  (eval '(adjoin 10 nil :test (constantly t) :test-not (constantly t))))

;; ANSI Common Lisp
(defparameter *adjoin-list* '())

(deftest adjoin-test.1
  (adjoin 'a *adjoin-list*)
  (a))

(deftest adjoin-test.2
  *adjoin-list*
  nil)

(deftest adjoin-test.3
  (setq *adjoin-list* (adjoin '(test-item 1) *adjoin-list*))
  ((test-item 1)))

(deftest adjoin-test.4
  (adjoin '(test-item 1) *adjoin-list*)
  ((test-item 1) (test-item 1)))

(deftest adjoin-test.5
  (adjoin '(test-item 1) *adjoin-list* :test 'equal)
  ((test-item 1)))

(deftest adjoin-test.6
  (adjoin '(new-test-item 1) *adjoin-list* :key #'cadr)
  ((test-item 1)))

(deftest adjoin-test.7
  (adjoin '(new-test-item 1) *adjoin-list*)
  ((new-test-item 1) (test-item 1)))


;;
;;  Macro PUSHNEW
;;
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

(deftest pushnew.9
  (let ((x '((a) b c d e)))
    (pushnew 'x (car x))
    (pushnew 'y (cdr x))
    x)
  ((x a) y b c d e))

(deftest pushnew.10
  (let ((x '((a) b c d e)))
    (pushnew 'a (car x))
    (pushnew 'd (cdr x))
    x)
  ((a) b c d e))

(deftest pushnew.11
  (let ((x '(2 4 6 8)))
    (pushnew 2 x :key #'1+ :test #'eql)
    x)
  (2 4 6 8))

(deftest pushnew.12
  (let ((x '(2 4 6 8)))
    (pushnew 3 x :key #'1+ :test #'eql)
    x)
  (3 2 4 6 8))

(deftest pushnew.13
  (let ((x '(2 (4) 6 8)))
    (pushnew '(4) x :test-not (lambda (x y) (not (equal x y))))
    x)
  (2 (4) 6 8))

(deftest pushnew.14
  (let ((x '(2 (4) 6 8)))
    (pushnew '(3) x :test-not (lambda (x y) (not (equal x y))))
    x)
  ((3) 2 (4) 6 8))

(deftest-error pushnew-error.1
  (eval '(pushnew 10 20)))

(deftest-error pushnew-error.2
  (eval '(let (x) (pushnew 10 x 30))))

(deftest-error pushnew-error.3
  (eval '(let (x) (pushnew 10))))

(deftest-error pushnew-error.4
  (eval '(let (x) (pushnew 10 x :key))))

(deftest-error pushnew-error.5
  (eval '(let (x) (pushnew 10 x :key 20))))

(deftest-error pushnew-error.6
  (eval '(let (x) (pushnew 10 x :test (constantly t) :test-not (constantly t)))))

;; ANSI Common Lisp
(defparameter *pushnew-list1* '(a (b c) d))

(deftest pushnew-test.1
  (pushnew 5 (cadr *pushnew-list1*))
  (5 b c))

(deftest pushnew-test.2
  *pushnew-list1*
  (a (5 b c) d))

(deftest pushnew-test.3
  (pushnew 'b (cadr *pushnew-list1*))
  (5 b c))

(deftest pushnew-test.4
  *pushnew-list1*
  (a (5 b c) d))

(defparameter *pushnew-list2* '((1) (1 2) (1 2 3)))

(deftest pushnew-test.5
  (pushnew '(2) *pushnew-list2*)
  ((2) (1) (1 2) (1 2 3)))

(deftest pushnew-test.6
  (pushnew '(1) *pushnew-list2*)
  ((1) (2) (1) (1 2) (1 2 3)))

(deftest pushnew-test.7
  (pushnew '(1) *pushnew-list2* :test 'equal)
  ((1) (2) (1) (1 2) (1 2 3)))

(deftest pushnew-test.8
  (pushnew '(1) *pushnew-list2* :key #'car)
  ((1) (2) (1) (1 2) (1 2 3)))


;;
;;  Function SET-DIFFERENCE
;;
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

(deftest set-difference.7
  (let ((x '(a b c)))
    (set-difference x '(d e a b c f g)))
  nil)

(deftest set-difference.8
  (set-difference '(2 4 6 8 10) '(4 5) :key #'1+ :test #'eql)
  (10 8 6 2))

(deftest set-difference.9
  (set-difference '(2 4 6 8 10) '(4 5) :key #'1+
                  :test-not (lambda (x y) (not (eql x y))))
  (10 8 6 2))

(deftest-error set-difference-error.1
  (eval '(set-difference 10 nil))
  type-error)

(deftest-error set-difference-error.2
  (eval '(set-difference nil 20))
  type-error)

(deftest-error! set-difference-error.3
  (eval '(set-difference nil)))

(deftest-error! set-difference-error.4
  (eval '(set-difference nil nil nil)))

(deftest-error! set-difference-error.5
  (eval '(set-difference nil nil :key)))

(deftest-error! set-difference-error.6
  (eval '(set-difference nil nil :key 10)))

(deftest-error! set-difference-error.7
  (eval '(set-difference nil nil :hello 10)))

(deftest-error! set-difference-error.8
  (eval '(set-difference nil nil :test (constantly t) :test-not (constantly t))))


;;
;;  Function NSET-DIFFERENCE
;;
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

(deftest nset-difference.6
  (let ((x '(a b c)))
    (nset-difference x '(d e a b c f g))
    x)
  (a b c))

(deftest nset-difference.7
  (let ((x '(a b c)))
    (nset-difference x '(d e a b c f g)))
  nil)

(deftest nset-difference.8
  (nset-difference '(2 4 6 8 10) '(4 5) :key #'1+ :test #'eql)
  (2 6 8 10))

(deftest nset-difference.9
  (nset-difference '(2 4 6 8 10) '(4 5) :key #'1+
                   :test-not (lambda (x y) (not (eql x y))))
  (2 6 8 10))

(deftest nset-difference.10
  (nset-difference '(a b c d e f) '(c b d))
  (a e f))

(deftest-error nset-difference-error.1
  (eval '(nset-difference 10 nil))
  type-error)

(deftest-error nset-difference-error.2
  (eval '(nset-difference nil 20))
  type-error)

(deftest-error! nset-difference-error.3
  (eval '(nset-difference nil)))

(deftest-error! nset-difference-error.4
  (eval '(nset-difference nil nil nil)))

(deftest-error! nset-difference-error.5
  (eval '(nset-difference nil nil :key)))

(deftest-error! nset-difference-error.6
  (eval '(nset-difference nil nil :key 10)))

(deftest-error! nset-difference-error.7
  (eval '(nset-difference nil nil :hello 10)))

(deftest-error! nset-difference-error.8
  (eval '(nset-difference nil nil :test (constantly t) :test-not (constantly t))))

;; ANSI Common Lisp
(defparameter *set-difference1* (list "A" "b" "C" "d"))
(defparameter *set-difference2* (list "a" "B" "C" "d"))

(deftest set-difference-test.1
  (set-difference *set-difference1* *set-difference2*)
  ("d" "C" "b" "A"))

(deftest set-difference-test.2
  (set-difference *set-difference1* *set-difference2* :test 'equal)
  ("b" "A"))

(deftest set-difference-test.3
  (set-difference *set-difference1* *set-difference2* :test #'equalp)
  nil)

(deftest set-difference-test.4
  (nset-difference *set-difference1* *set-difference2* :test #'string=)
  ("A" "b"))

(deftest set-difference-test.5
  (progn
    (setq *set-difference1* '(("a" . "b") ("c" . "d") ("e" . "f")))
    (setq *set-difference2* '(("c" . "a") ("e" . "b") ("d" . "a")))
    (nset-difference *set-difference1* *set-difference2* :test #'string= :key #'cdr))
  (("c" . "d") ("e" . "f")))

(deftest set-difference-test.6
  *set-difference1*
  (("a" . "b") ("c" . "d") ("e" . "f")))

(deftest set-difference-test.7
  *set-difference2*
  (("c" . "a") ("e" . "b") ("d" . "a")))

(deftest set-difference-test.8
  (set-difference
    '("strawberry" "chocolate" "banana" "lemon" "pistachio" "rhubarb")
    '(#\c #\w)
    :test #'(lambda (s c) (find c s)))
  ("rhubarb" "lemon" "banana"))


;;
;;  Function SET-EXCLUSIVE-OR
;;
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

(deftest set-exclusive-or.9
  (set-exclusive-or '(a b c d e f g h) '(d e f i))
  (i h g c b a))

(deftest set-exclusive-or.10
  (set-exclusive-or '(d e f i) '(a b c d e f g h))
  (h g c b a i))

(deftest set-exclusive-or.11
  (set-exclusive-or '(1 2 3 4) '(3 4 5 6) :key #'1+ :test #'eql)
  (6 5 2 1))

(deftest set-exclusive-or.12
  (set-exclusive-or '(3 4 5 6) '(1 2 3 4) :key #'1+ :test #'eql)
  (2 1 6 5))

(deftest set-exclusive-or.13
  (set-exclusive-or '((3) 4 5 6) '(1 2 3 4) :test #'equal)
  (3 2 1 6 5 (3)))

(deftest set-exclusive-or.14
  (set-exclusive-or '((3) 4 5 6) '(1 2 3 4)
                    :test-not (lambda (x y) (not (equal x y))))
  (3 2 1 6 5 (3)))

(deftest-error set-exclusive-or-error.1
  (eval '(set-exclusive-or 10 nil))
  type-error)

(deftest-error set-exclusive-or-error.2
  (eval '(set-exclusive-or nil 20))
  type-error)

(deftest-error! set-exclusive-or-error.3
  (eval '(set-exclusive-or nil)))

(deftest-error! set-exclusive-or-error.4
  (eval '(set-exclusive-or nil nil nil)))

(deftest-error set-exclusive-or-error.5
  (eval '(set-exclusive-or nil nil :key)))

(deftest-error set-exclusive-or-error.6
  (eval '(set-exclusive-or nil nil :key 10)))

(deftest-error set-exclusive-or-error.7
  (eval '(set-exclusive-or nil nil :hello 10)))

(deftest-error set-exclusive-or-error.8
  (eval '(set-exclusive-or nil nil :test (constantly t) :test-not (constantly t))))


;;
;;  Function NSET-EXCLUSIVE-OR
;;
(deftest nset-exclusive-or.1
  (nset-exclusive-or nil nil)
  nil)

(deftest nset-exclusive-or.2
  (nset-exclusive-or '(a b c) nil)
  (a b c))

(deftest nset-exclusive-or.3
  (nset-exclusive-or nil '(a b c))
  (a b c))

(deftest nset-exclusive-or.4
  (nset-exclusive-or '(a b) '(c d e))
  (a b c d e))

(deftest nset-exclusive-or.5
  (nset-exclusive-or '(a c) '(c d e))
  (a d e))

(deftest nset-exclusive-or.6
  (nset-exclusive-or '(a a a b) '(c d e))
  (a a a b c d e))

(deftest nset-exclusive-or.7
  (nset-exclusive-or '(a b) '(c d d e))
  (a b c d d e))

(deftest nset-exclusive-or.8
  (nset-exclusive-or '(a b c) '(a b c))
  nil)

(deftest nset-exclusive-or.9
  (nset-exclusive-or '(a b c d e f g h) '(d e f i))
  (a b c g h i))

(deftest nset-exclusive-or.10
  (nset-exclusive-or '(d e f i) '(a b c d e f g h))
  (i a b c g h))

(deftest nset-exclusive-or.11
  (nset-exclusive-or '(1 2 3 4) '(3 4 5 6) :key #'1+ :test #'eql)
  (1 2 5 6))

(deftest nset-exclusive-or.12
  (nset-exclusive-or '(3 4 5 6) '(1 2 3 4) :key #'1+ :test #'eql)
  (5 6 1 2))

(deftest nset-exclusive-or.13
  (nset-exclusive-or '((3) 4 5 6) '(1 2 3 4) :test #'equal)
  ((3) 5 6 1 2 3))

(deftest nset-exclusive-or.14
  (nset-exclusive-or '((3) 4 5 6) '(1 2 3 4)
                     :test-not (lambda (x y) (not (equal x y))))
  ((3) 5 6 1 2 3))

(deftest-error nset-exclusive-or-error.1
  (eval '(nset-exclusive-or 10 nil))
  type-error)

(deftest-error nset-exclusive-or-error.2
  (eval '(nset-exclusive-or nil 20))
  type-error)

(deftest-error! nset-exclusive-or-error.3
  (eval '(nset-exclusive-or nil)))

(deftest-error! nset-exclusive-or-error.4
  (eval '(nset-exclusive-or nil nil nil)))

(deftest-error nset-exclusive-or-error.5
  (eval '(nset-exclusive-or nil nil :key)))

(deftest-error nset-exclusive-or-error.6
  (eval '(nset-exclusive-or nil nil :key 10)))

(deftest-error nset-exclusive-or-error.7
  (eval '(nset-exclusive-or nil nil :hello 10)))

(deftest-error nset-exclusive-or-error.8
  (eval '(nset-exclusive-or nil nil :test (constantly t) :test-not (constantly t))))

;; ANSI Common Lisp
(defparameter *set-exclusive-or1* (list 1 "a" "b"))
(defparameter *set-exclusive-or2* (list 1 "A" "b"))

(deftest set-exclusive-or-test.1
  (set-exclusive-or *set-exclusive-or1* *set-exclusive-or2*)
  ("b" "A" "b" "a"))

(deftest set-exclusive-or-test.2
  (set-exclusive-or *set-exclusive-or1* *set-exclusive-or2* :test #'equal)
  ("A" "a"))

(deftest set-exclusive-or-test.3
  (set-exclusive-or *set-exclusive-or1* *set-exclusive-or2* :test 'equalp)
  nil)

(deftest set-exclusive-or-test.4
  (nset-exclusive-or *set-exclusive-or1* *set-exclusive-or2*)
  ("a" "b" "A" "b"))

(deftest set-exclusive-or-test.5
  (progn
    (setq *set-exclusive-or1* '(("a" . "b") ("c" . "d") ("e" . "f")))
    (setq *set-exclusive-or2* '(("c" . "a") ("e" . "b") ("d" . "a")))
    (nset-exclusive-or *set-exclusive-or1* *set-exclusive-or2*
                       :test #'string= :key #'cdr))
  (("c" . "d") ("e" . "f") ("c" . "a") ("d" . "a")))

(deftest set-exclusive-or-test.6
  *set-exclusive-or1*
  (("a" . "b") ("c" . "d") ("e" . "f") ("c" . "a") ("d" . "a")))

(deftest set-exclusive-or-test.7
  *set-exclusive-or2*
  (("c" . "a") ("d" . "a")))


;;
;;  Function SUBSETP
;;
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

(deftest subsetp.6
  (subsetp '(z a b c d e) '(a b c))
  nil)

(deftest subsetp.7
  (subsetp '(1 3) '(2 3 4 5 6) :key #'1+ :test #'eql)
  nil)

(deftest subsetp.8
  (subsetp '(2 6) '(2 3 4 5 6) :key #'1+ :test #'eql)
  t)

(deftest subsetp.9
  (subsetp '(2 4) '(1 3 5 7) :test (lambda (x y) (and (evenp x) (evenp y))))
  nil)

(deftest subsetp.10
  (subsetp '(2 4) '(1 3 6 7) :test (lambda (x y) (and (evenp x) (evenp y))))
  t)

(deftest-error subsetp-error.1
  (eval '(subsetp 10 nil))
  type-error)

(deftest-error subsetp-error.2
  (eval '(subsetp nil 20))
  type-error)

(deftest-error! subsetp-error.3
  (eval '(subsetp nil)))

(deftest-error! subsetp-error.4
  (eval '(subsetp nil nil nil)))

(deftest-error! subsetp-error.5
  (eval '(subsetp nil nil :key)))

(deftest-error! subsetp-error.6
  (eval '(subsetp nil nil :key 10)))

(deftest-error! subsetp-error.7
  (eval '(subsetp nil nil :hello 10)))

(deftest-error! subsetp-error.8
  (eval '(subsetp nil nil :test (constantly t) :test-not (constantly t))))

;; ANSI Common Lisp
(defparameter *subsetp* '(1 "a" (1 2)))

(deftest subsetp-test.1
  (subsetp '(1) *subsetp*)
  t)

(deftest subsetp-test.2
  (subsetp '((1 2)) *subsetp*)
  nil)

(deftest subsetp-test.3
  (subsetp '((1 2)) *subsetp* :test 'equal)
  t)

(deftest subsetp-test.4
  (subsetp '(1 "A") *subsetp* :test #'equalp)
  t)

(deftest subsetp-test.5
  (subsetp '((1) (2)) '((1) (2)))
  nil)

(deftest subsetp-test.6
  (subsetp '((1) (2)) '((1) (2)) :key #'car)
  t)


;;
;;  Function UNION
;;
(deftest union.1
  (union nil nil)
  nil)

(deftest union.2
  (union '(a b c) nil)
  (a b c))

(deftest union.3
  (union nil '(a b c))
  (a b c))

(deftest union.4
  (union '(a b c) '(d e))
  (a b c d e))

(deftest union.5
  (union '(a b b b b c) '(d e))
  (a b b b b c d e))

(deftest union.6
  (union '(a b b b b c) '(b b b d d d e))
  (a b b b b c d d d e))

(deftest union.7
  (union '(1 2 3) '(2 3 4 5) :key #'1+ :test #'eql)
  (1 2 3 4 5))

(deftest union.8
  (union '(1 (2) 3) '(2 3 4 5) :test #'equal)
  (1 (2) 3 2 4 5))

(deftest union.9
  (union '(1 (2) 3) '(2 3 4 5) :test-not (lambda (x y) (not (equal x y))))
  (1 (2) 3 2 4 5))

(deftest union.10
  (union '(a a a) '(a a a a a a b))
  (a a a a a a b))

(deftest union.11
  (union '(a a a a a a b) '(a a a))
  (a a a a a a b))

(deftest-error union-error.1
  (eval '(union 10 nil))
  type-error)

(deftest-error union-error.2
  (eval '(union nil 20))
  type-error)

(deftest-error! union-error.3
  (eval '(union nil)))

(deftest-error union-error.4
  (eval '(union nil nil nil)))

(deftest-error union-error.5
  (eval '(union nil nil :key)))

(deftest-error union-error.6
  (eval '(union nil nil :key 10)))

(deftest-error union-error.7
  (eval '(union nil nil :hello 10)))

(deftest-error union-error.8
  (eval '(union nil nil :test (constantly t) :test-not (constantly t))))


;;
;;  Function NUNION
;;
(deftest nunion.1
  (nunion nil nil)
  nil)

(deftest nunion.2
  (nunion '(a b c) nil)
  (a b c))

(deftest nunion.3
  (nunion nil '(a b c))
  (a b c))

(deftest nunion.4
  (nunion '(a b c) '(d e))
  (a b c d e))

(deftest nunion.5
  (nunion '(a b b b b c) '(d e))
  (a b b b b c d e))

(deftest nunion.6
  (nunion '(a b b b b c) '(b b b d d d e))
  (a b b b b c d d d e))

(deftest nunion.7
  (nunion '(1 2 3) '(2 3 4 5) :key #'1+ :test #'eql)
  (1 2 3 4 5))

(deftest nunion.8
  (nunion '(1 (2) 3) '(2 3 4 5) :test #'equal)
  (1 (2) 3 2 4 5))

(deftest nunion.9
  (nunion '(1 (2) 3) '(2 3 4 5) :test-not (lambda (x y) (not (equal x y))))
  (1 (2) 3 2 4 5))

(deftest-error nunion-error.1
  (eval '(nunion 10 nil))
  type-error)

(deftest-error nunion-error.2
  (eval '(nunion nil 20))
  type-error)

(deftest-error! nunion-error.3
  (eval '(nunion nil)))

(deftest-error nunion-error.4
  (eval '(nunion nil nil nil)))

(deftest-error nunion-error.5
  (eval '(nunion nil nil :key)))

(deftest-error nunion-error.6
  (eval '(nunion nil nil :key 10)))

(deftest-error nunion-error.7
  (eval '(nunion nil nil :hello 10)))

(deftest-error nunion-error.8
  (eval '(nunion nil nil :test (constantly t) :test-not (constantly t))))

