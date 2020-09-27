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

(deftest sublis.9
  (sublis '((a . 10) nil (nil . 20)) '(a nil c))
  (10 20 c . 20))

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

(deftest nsublis.9
  (nsublis '((a . 10) nil (nil . 20)) '(a nil c))
  (10 20 c . 20))

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

