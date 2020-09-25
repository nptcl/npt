;;
;;  ANSI COMMON LISP: 14. Conses
;;

;;
;;  Function SUBST
;;
(deftest subst.1
  (subst 10 20 nil)
  nil)

(deftest subst.2
  (subst 10 20 30)
  30)

(deftest subst.3
  (subst 10 20 20)
  10)

(deftest subst.4
  (subst 'z 'a '(a b c (d e f (a))))
  (z b c (d e f (z))))

(deftest subst.5
  (subst 99 3 '(1 2 3 (4 1 2 (3)))
         :key (lambda (x)
                (if (realp x)
                  (1+ x)
                  x)))
  (1 99 3 (4 1 99 (3))))

(deftest subst.6
  (subst 99 3 '(1 2 3 (4 1 2 (3)))
         :key (lambda (x)
                (if (realp x)
                  (1+ x)
                  x)))
  (1 99 3 (4 1 99 (3))))

(deftest subst.7
  (subst 99 3 '(2 3 (4 5 6) 7 8 9)
         :test (lambda (x y)
                 (and (realp x)
                      (realp y)
                      (zerop (mod (max x y) (min x y))))))
  (2 99 (4 5 99) 7 8 99))

(deftest subst.8
  (subst 99 3 '(2 3 4 5 6 7 8 9)
         :test (lambda (x y) (or (listp x) (listp y))))
  99)

(deftest-error! subst-error.1
  (eval '(subst 10 20)))

(deftest-error! subst-error.2
  (eval '(subst 10 20 30 40)))

(deftest-error! subst-error.3
  (eval '(subst 10 20 30 :test)))

(deftest-error! subst-error.4
  (eval '(subst 10 20 30 :test 10)))

(deftest-error! subst-error.5
  (eval '(subst 10 20 30 :hello 10)))

(deftest-error! subst-error.6
  (eval '(subst 10 20 30 :test (constantly t) :test-not (constantly t))))


;;
;;  Function NSUBST
;;
(deftest nsubst.1
  (nsubst 10 20 nil)
  nil)

(deftest nsubst.2
  (nsubst 10 20 30)
  30)

(deftest nsubst.3
  (nsubst 10 20 20)
  10)

(deftest nsubst.4
  (nsubst 'z 'a '(a b c (d e f (a))))
  (z b c (d e f (z))))

(deftest nsubst.5
  (nsubst 99 3 '(1 2 3 (4 1 2 (3)))
          :key (lambda (x)
                 (if (realp x)
                   (1+ x)
                   x)))
  (1 99 3 (4 1 99 (3))))

(deftest nsubst.6
  (nsubst 99 3 '(1 2 3 (4 1 2 (3)))
          :key (lambda (x)
                 (if (realp x)
                   (1+ x)
                   x)))
  (1 99 3 (4 1 99 (3))))

(deftest nsubst.7
  (nsubst 99 3 '(2 3 (4 5 6) 7 8 9)
          :test (lambda (x y)
                  (and (realp x)
                       (realp y)
                       (zerop (mod (max x y) (min x y))))))
  (2 99 (4 5 99) 7 8 99))

(deftest nsubst.8
  (nsubst 99 3 '(2 3 4 5 6 7 8 9)
          :test (lambda (x y) (or (listp x) (listp y))))
  99)

(deftest-error! nsubst-error.1
  (eval '(nsubst 10 20)))

(deftest-error! nsubst-error.2
  (eval '(nsubst 10 20 30 40)))

(deftest-error! nsubst-error.3
  (eval '(nsubst 10 20 30 :test)))

(deftest-error! nsubst-error.4
  (eval '(nsubst 10 20 30 :test 10)))

(deftest-error! nsubst-error.5
  (eval '(nsubst 10 20 30 :hello 10)))

(deftest-error! nsubst-error.6
  (eval '(nsubst 10 20 30 :test (constantly t) :test-not (constantly t))))


;;
;;  Function SUBST-IF
;;
(deftest subst-if.1
  (subst-if 10 (constantly nil) nil)
  nil)

(deftest subst-if.2
  (subst-if 10 (constantly nil) 20)
  20)

(deftest subst-if.3
  (subst-if 10 (lambda (x) (equal x 30)) 30)
  10)

(deftest subst-if.4
  (subst-if 10 (constantly nil) '(a b c (d e (f)) g))
  (a b c (d e (f)) g))

(deftest subst-if.5
  (subst-if 10 (lambda (x) (eq x 'd)) '(a b c (d e (f)) g))
  (a b c (10 e (f)) g))

(deftest subst-if.6
  (subst-if 99 (lambda (x) (equal x 2)) '(1 2 3 (4 1 2 (3))))
  (1 99 3 (4 1 99 (3))))

(deftest subst-if.7
  (subst-if 99 (lambda (x) (equal x 3))
            '(1 2 3 (4 1 2 (3)))
            :key (lambda (x)
                   (if (realp x)
                     (1+ x)
                     x)))
  (1 99 3 (4 1 99 (3))))

(deftest subst-if.8
  (subst-if 99 #'listp '(1 2 3 4))
  99)

(deftest-error subst-if-error.1
  (eval '(subst-if 10 20 30))
  type-error)

(deftest-error! subst-if-error.2
  (eval '(subst-if 10 (constantly nil))))

(deftest-error! subst-if-error.3
  (eval '(subst-if 10 (constantly nil) 30 40)))

(deftest-error! subst-if-error.4
  (eval '(subst-if 10 (constantly nil) 30 :key)))

(deftest-error! subst-if-error.5
  (eval '(subst-if 10 (constantly nil) 30 :key 40)))

(deftest-error! subst-if-error.6
  (eval '(subst-if 10 (constantly nil) 30 :hello 40)))


;;
;;  Function NSUBST-IF
;;
(deftest nsubst-if.1
  (nsubst-if 10 (constantly nil) nil)
  nil)

(deftest nsubst-if.2
  (nsubst-if 10 (constantly nil) 20)
  20)

(deftest nsubst-if.3
  (nsubst-if 10 (lambda (x) (equal x 30)) 30)
  10)

(deftest nsubst-if.4
  (nsubst-if 10 (constantly nil) '(a b c (d e (f)) g))
  (a b c (d e (f)) g))

(deftest nsubst-if.5
  (nsubst-if 10 (lambda (x) (eq x 'd)) '(a b c (d e (f)) g))
  (a b c (10 e (f)) g))

(deftest nsubst-if.6
  (nsubst-if 99 (lambda (x) (equal x 2)) '(1 2 3 (4 1 2 (3))))
  (1 99 3 (4 1 99 (3))))

(deftest nsubst-if.7
  (nsubst-if 99 (lambda (x) (equal x 3))
             '(1 2 3 (4 1 2 (3)))
             :key (lambda (x)
                    (if (realp x)
                      (1+ x)
                      x)))
  (1 99 3 (4 1 99 (3))))

(deftest nsubst-if.8
  (nsubst-if 99 #'listp '(1 2 3 4))
  99)

(deftest-error nsubst-if-error.1
  (eval '(nsubst-if 10 20 30))
  type-error)

(deftest-error! nsubst-if-error.2
  (eval '(nsubst-if 10 (constantly nil))))

(deftest-error! nsubst-if-error.3
  (eval '(nsubst-if 10 (constantly nil) 30 40)))

(deftest-error! nsubst-if-error.4
  (eval '(nsubst-if 10 (constantly nil) 30 :key)))

(deftest-error! nsubst-if-error.5
  (eval '(nsubst-if 10 (constantly nil) 30 :key 40)))

(deftest-error! nsubst-if-error.6
  (eval '(nsubst-if 10 (constantly nil) 30 :hello 40)))


;;
;;  Function SUBST-IF-NOT
;;
(deftest subst-if-not.1
  (subst-if-not 10 (constantly t) nil)
  nil)

(deftest subst-if-not.2
  (subst-if-not 10 (constantly t) 20)
  20)

(deftest subst-if-not.3
  (subst-if-not 10 (lambda (x) (not (equal x 30))) 30)
  10)

(deftest subst-if-not.4
  (subst-if-not 10 (constantly t) '(a b c (d e (f)) g))
  (a b c (d e (f)) g))

(deftest subst-if-not.5
  (subst-if-not 10 (lambda (x) (not (eq x 'd))) '(a b c (d e (f)) g))
  (a b c (10 e (f)) g))

(deftest subst-if-not.6
  (subst-if-not 99 (lambda (x) (not (equal x 2))) '(1 2 3 (4 1 2 (3))))
  (1 99 3 (4 1 99 (3))))

(deftest subst-if-not.7
  (subst-if-not 99 (lambda (x) (not (equal x 3)))
                '(1 2 3 (4 1 2 (3)))
                :key (lambda (x)
                       (if (realp x)
                         (1+ x)
                         x)))
  (1 99 3 (4 1 99 (3))))

(deftest subst-if-not.8
  (subst-if-not 99 (lambda (x) (not (listp x))) '(1 2 3 4))
  99)

(deftest-error subst-if-not-error.1
  (eval '(subst-if-not 10 20 30))
  type-error)

(deftest-error! subst-if-not-error.2
  (eval '(subst-if-not 10 (constantly nil))))

(deftest-error! subst-if-not-error.3
  (eval '(subst-if-not 10 (constantly nil) 30 40)))

(deftest-error! subst-if-not-error.4
  (eval '(subst-if-not 10 (constantly nil) 30 :key)))

(deftest-error! subst-if-not-error.5
  (eval '(subst-if-not 10 (constantly nil) 30 :key 40)))

(deftest-error! subst-if-not-error.6
  (eval '(subst-if-not 10 (constantly nil) 30 :hello 40)))


;;
;;  Function NSUBST-IF-NOT
;;
(deftest nsubst-if-not.1
  (nsubst-if-not 10 (constantly t) nil)
  nil)

(deftest nsubst-if-not.2
  (nsubst-if-not 10 (constantly t) 20)
  20)

(deftest nsubst-if-not.3
  (nsubst-if-not 10 (lambda (x) (not (equal x 30))) 30)
  10)

(deftest nsubst-if-not.4
  (nsubst-if-not 10 (constantly t) '(a b c (d e (f)) g))
  (a b c (d e (f)) g))

(deftest nsubst-if-not.5
  (nsubst-if-not 10 (lambda (x) (not (eq x 'd))) '(a b c (d e (f)) g))
  (a b c (10 e (f)) g))

(deftest nsubst-if-not.6
  (nsubst-if-not 99 (lambda (x) (not (equal x 2))) '(1 2 3 (4 1 2 (3))))
  (1 99 3 (4 1 99 (3))))

(deftest nsubst-if-not.7
  (nsubst-if-not 99 (lambda (x) (not (equal x 3)))
                 '(1 2 3 (4 1 2 (3)))
                 :key (lambda (x)
                        (if (realp x)
                          (1+ x)
                          x)))
  (1 99 3 (4 1 99 (3))))

(deftest nsubst-if-not.8
  (nsubst-if-not 99 (lambda (x) (not (listp x))) '(1 2 3 4))
  99)

(deftest-error nsubst-if-not-error.1
  (eval '(nsubst-if-not 10 20 30))
  type-error)

(deftest-error! nsubst-if-not-error.2
  (eval '(nsubst-if-not 10 (constantly nil))))

(deftest-error! nsubst-if-not-error.3
  (eval '(nsubst-if-not 10 (constantly nil) 30 40)))

(deftest-error! nsubst-if-not-error.4
  (eval '(nsubst-if-not 10 (constantly nil) 30 :key)))

(deftest-error! nsubst-if-not-error.5
  (eval '(nsubst-if-not 10 (constantly nil) 30 :key 40)))

(deftest-error! nsubst-if-not-error.6
  (eval '(nsubst-if-not 10 (constantly nil) 30 :hello 40)))


;; ANSI Common Lisp
(defparameter *subst-tree1* '(1 (1 2) (1 2 3) (1 2 3 4)))

(deftest subst-test.1
  (subst "two" 2 *subst-tree1*)
  (1 (1 "two") (1 "two" 3) (1 "two" 3 4)))

(deftest subst-test.2
  (subst "five" 5 *subst-tree1*)
  (1 (1 2) (1 2 3) (1 2 3 4)))

(deftest subst-test.3
  (eq *subst-tree1* (subst "five" 5 *subst-tree1*))
  nil)

(deftest subst-test.4
  (subst 'tempest 'hurricane '(shakespeare wrote (the hurricane)))
  (shakespeare wrote (the tempest)))

(deftest subst-test.5
  (subst 'foo 'nil '(shakespeare wrote (twelfth night)))
  (shakespeare wrote (twelfth night . foo) . foo))

(deftest subst-test.6
  (subst '(a . cons) '(old . pair)
         '((old . spice) ((old . shoes) old . pair) (old . pair))
         :test #'equal)
  ((old . spice) ((old . shoes) a . cons) (a . cons)))

(deftest subst-test.7
  (subst-if 5 #'listp *subst-tree1*)
  5)

(deftest subst-test.8
  (subst-if-not '(x) #'consp *subst-tree1*)
  ((x) ((x) (x) x) ((x) (x) (x) x) ((x) (x) (x) (x) x) x))
;; error: (1 x)

(deftest subst-test.9
  *subst-tree1*
  (1 (1 2) (1 2 3) (1 2 3 4)))

(deftest subst-test.10
  (nsubst 'x 3 *subst-tree1* :key #'(lambda (y) (and (listp y) (third y))))
  (1 (1 2) x x))

(deftest subst-test.11
  *subst-tree1*
  (1 (1 2) x x))

