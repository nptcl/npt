;;
;;  ANSI COMMON LISP: 14. Conses
;;

;;
;;  Function MEMBER
;;
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

(deftest member.6
  (member 2 '(1 2 3))
  (2 3))

(deftest member.7
  (member 2 '((1 . 2) (3 . 4)) :test-not #'= :key #'cdr)
  ((3 . 4)))

(deftest member.8
  (member 'e '(a b c d))
  nil)

(deftest member.9
  (member 'a '(g (a y) c a d e a f))
  (a d e a f))

(deftest-error member-error.1
  (eval '(member 10 '(a b d . e))))

(deftest-error member-error.2
  (eval '(member 10 20)))

(deftest-error! member-error.3
  (eval '(member 10)))

(deftest-error! member-error.4
  (eval '(member 10 nil 30)))

(deftest-error! member-error.5
  (eval '(member 10 nil :key)))

(deftest-error! member-error.6
  (eval '(member 10 nil :key 10)))

(deftest-error! member-error.7
  (eval '(member 10 nil :hello 10)))

(deftest-error! member-error.8
  (eval '(member 10 nil :test (constantly t) :test-not (constantly t))))


;;
;;  Function MEMBER-IF
;;
(deftest member-if.1
  (member-if (lambda (x) (eq 'b x)) '(a b c d))
  (b c d))

(deftest member-if.2
  (member-if (lambda (x) (eq 'z x)) '(a b c d))
  nil)

(deftest member-if.3
  (member-if (lambda (x) (eql 21 x)) '(10 20 30 40) :key #'1+)
  (20 30 40))

(deftest member-if.4
  (member-if #'listp '(a b nil c d))
  (nil c d))

(deftest member-if.5
  (member-if #'numberp '(a #\space 5/3 foo))
  (5/3 foo))

(deftest-error member-if-error.1
  (eval '(member-if (constantly nil) '(a b d . e))))

(deftest-error member-if-error.2
  (eval '(member-if (constantly nil) 20)))

(deftest-error! member-if-error.3
  (eval '(member-if (constantly t))))

(deftest-error! member-if-error.4
  (eval '(member-if (constantly nil) nil 30)))

(deftest-error! member-if-error.5
  (eval '(member-if (constantly nil) nil :key)))

(deftest-error! member-if-error.6
  (eval '(member-if (constantly nil) nil :key 10)))

(deftest-error! member-if-error.7
  (eval '(member-if (constantly nil) nil :hello 10)))


;;
;;  Function MEMBER-IF-NOT
;;
(deftest member-if-not.1
  (member-if-not (lambda (x) (eq 'a x)) '(a b c d))
  (b c d))

(deftest member-if-not.2
  (member-if-not (lambda (x) x) '(a b c d))
  nil)

(deftest member-if-not.3
  (member-if-not (lambda (x) (eql 11 x)) '(10 20 30 40) :key #'1+)
  (20 30 40))

(deftest member-if-not.4
  (member-if-not #'zerop '(3 6 9 11 . 12) :key #'(lambda (x) (mod x 3)))
  (11 . 12))

(deftest-error member-if-not-error.1
  (eval '(member-if-not (constantly t) '(a b d . e))))

(deftest-error member-if-not-error.2
  (eval '(member-if-not (constantly t) 20)))

(deftest-error! member-if-not-error.3
  (eval '(member-if-not (constantly nil))))

(deftest-error! member-if-not-error.4
  (eval '(member-if-not (constantly t) nil 30)))

(deftest-error! member-if-not-error.5
  (eval '(member-if-not (constantly t) nil :key)))

(deftest-error! member-if-not-error.6
  (eval '(member-if-not (constantly t) nil :key 10)))

(deftest-error! member-if-not-error.7
  (eval '(member-if-not (constantly t) nil :hello 10)))

