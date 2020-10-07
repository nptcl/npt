;;
;;  ANSI COMMON LISP: 13. Characters
;;

;;
;;  Function CHAR=
;;
(deftest char=.1
  (char= #\a)
  t)

(deftest char=.2
  (char= #\a #\a)
  t)

(deftest char=.3
  (char= #\a #\A)
  nil)

(deftest char=.4
  (char= #\z #\A)
  nil)

(deftest char=.5
  (char= #\z #\z #\z)
  t)

(deftest char=.6
  (char= #\a #\z #\z)
  nil)

(deftest char=.7
  (char= #\z #\a #\z)
  nil)

(deftest char=.8
  (char= #\z #\z #\a)
  nil)

(deftest-error char=-error.1
  (eval '(char=))
  program-error)

(deftest-error char=-error.2
  (eval '(char= 10)))


;;
;;  Function CHAR/=
;;
(deftest char/=.1
  (char/= #\a)
  t)

(deftest char/=.2
  (char/= #\a #\a)
  nil)

(deftest char/=.3
  (char/= #\a #\A)
  t)

(deftest char/=.4
  (char/= #\z #\A)
  t)

(deftest char/=.5
  (char/= #\a #\b #\c)
  t)

(deftest char/=.6
  (char/= #\b #\b #\c)
  nil)

(deftest char/=.7
  (char/= #\a #\b #\b)
  nil)

(deftest char/=.8
  (char/= #\c #\b #\c)
  nil)

(deftest-error char/=-error.1
  (eval '(char/=))
  program-error)

(deftest-error char/=-error.2
  (eval '(char/= 10)))


;;
;;  Function CHAR<
;;
(deftest char<.1
  (char< #\z)
  t)

(deftest char<.2
  (char< #\a #\c)
  t)

(deftest char<.3
  (char< #\c #\a)
  nil)

(deftest char<.4
  (char< #\a #\a)
  nil)

(deftest char<.5
  (char< #\a #\b #\c)
  t)

(deftest char<.6
  (char< #\a #\a #\c)
  nil)

(deftest char<.7
  (char< #\a #\c #\a)
  nil)

(deftest char<.8
  (char< #\a #\b #\b)
  nil)

(deftest char<.9
  (char< #\c #\b #\a)
  nil)

(deftest-error char<-error.1
  (eval '(char<))
  program-error)

(deftest-error char<-error.2
  (eval '(char< 10)))


;;
;;  Function CHAR>
;;
(deftest char>.1
  (char> #\z)
  t)

(deftest char>.2
  (char> #\b #\a)
  t)

(deftest char>.3
  (char> #\a #\c)
  nil)

(deftest char>.4
  (char> #\a #\a)
  nil)

(deftest char>.5
  (char> #\c #\b #\a)
  t)

(deftest char>.6
  (char> #\c #\c #\a)
  nil)

(deftest char>.7
  (char> #\c #\a #\c)
  nil)

(deftest char>.8
  (char> #\b #\a #\a)
  nil)

(deftest char>.9
  (char> #\a #\b #\c)
  nil)

(deftest-error char>-error.1
  (eval '(char>))
  program-error)

(deftest-error char>-error.2
  (eval '(char> 10)))


;;
;;  Function CHAR<=
;;
(deftest char<=.1
  (char<= #\a)
  t)

(deftest char<=.2
  (char<= #\a #\b)
  t)

(deftest char<=.3
  (char<= #\c #\c)
  t)

(deftest char<=.4
  (char<= #\b #\a)
  nil)

(deftest char<=.5
  (char<= #\c #\e #\g)
  t)

(deftest char<=.6
  (char<= #\c #\e #\e)
  t)

(deftest char<=.7
  (char<= #\e #\c #\g)
  nil)

(deftest char<=.8
  (char<= #\c #\g #\e)
  nil)

(deftest char<=.9
  (char<= #\e #\g #\c)
  nil)

(deftest char<=.10
  (char<= #\g #\e #\c)
  nil)

(deftest-error char<=-error.1
  (eval '(char<=))
  program-error)

(deftest-error char<=-error.2
  (eval '(char<= 10)))


;;
;;  Function CHAR>=
;;
(deftest char>=.1
  (char>= #\a)
  t)

(deftest char>=.2
  (char>= #\b #\a)
  t)

(deftest char>=.3
  (char>= #\a #\a)
  t)

(deftest char>=.4
  (char>= #\a #\b)
  nil)

(deftest char>=.5
  (char>= #\c #\b #\a)
  t)

(deftest char>=.6
  (char>= #\c #\c #\a)
  t)

(deftest char>=.7
  (char>= #\b #\c #\a)
  nil)

(deftest char>=.8
  (char>= #\c #\a #\b)
  nil)

(deftest char>=.9
  (char>= #\b #\a #\c)
  nil)

(deftest char>=.10
  (char>= #\a #\b #\c)
  nil)

(deftest-error char>=-error.1
  (eval '(char>=))
  program-error)

(deftest-error char>=-error.2
  (eval '(char>= 10)))


;;
;;  Function CHAR-EQUAL
;;
(deftest char-equal.1
  (char-equal #\a)
  t)

(deftest char-equal.2
  (char-equal #\a #\a)
  t)

(deftest char-equal.3
  (char-equal #\A #\a)
  t)

(deftest char-equal.4
  (char-equal #\a #\b)
  nil)

(deftest char-equal.5
  (char-equal #\a #\B)
  nil)

(deftest char-equal.6
  (char-equal #\A #\a #\A)
  t)

(deftest char-equal.7
  (char-equal #\a #\b #\b)
  nil)

(deftest char-equal.8
  (char-equal #\a #\b #\a)
  nil)

(deftest char-equal.9
  (char-equal #\a #\a #\b)
  nil)

(deftest char-equal.10
  (char-equal #\a #\b #\c)
  nil)

(deftest-error char-equal-error.1
  (eval '(char-equal))
  program-error)

(deftest-error char-equal-error.2
  (eval '(char-equal 10)))


;;
;;  Function CHAR-NOT-EQUAL
;;
(deftest char-not-equal.1
  (char-not-equal #\a)
  t)

(deftest char-not-equal.2
  (char-not-equal #\a #\b)
  t)

(deftest char-not-equal.3
  (char-not-equal #\a #\a)
  nil)

(deftest char-not-equal.4
  (char-not-equal #\a #\A)
  nil)

(deftest char-not-equal.5
  (char-not-equal #\a #\b #\c)
  t)

(deftest char-not-equal.6
  (char-not-equal #\a #\A #\c)
  nil)

(deftest char-not-equal.7
  (char-not-equal #\a #\b #\a)
  nil)

(deftest char-not-equal.8
  (char-not-equal #\a #\b #\b)
  nil)

(deftest char-not-equal.9
  (char-not-equal #\a #\a #\a)
  nil)

(deftest-error char-not-equal-error.1
  (eval '(char-not-equal))
  program-error)

(deftest-error char-not-equal-error.2
  (eval '(char-not-equal 10)))


;;
;;  Function CHAR-LESSP
;;
(deftest char-lessp.1
  (char-lessp #\z)
  t)

(deftest char-lessp.2
  (char-lessp #\a #\c)
  t)

(deftest char-lessp.3
  (char-lessp #\a #\C)
  t)

(deftest char-lessp.4
  (char-lessp #\a #\c)
  t)

(deftest char-lessp.5
  (char-lessp #\c #\a)
  nil)

(deftest char-lessp.6
  (char-lessp #\C #\a)
  nil)

(deftest char-lessp.7
  (char-lessp #\c #\A)
  nil)

(deftest char-lessp.8
  (char-lessp #\a #\a)
  nil)

(deftest char-lessp.9
  (char-lessp #\A #\a)
  nil)

(deftest char-lessp.10
  (char-lessp #\a #\A)
  nil)

(deftest char-lessp.11
  (char-lessp #\a #\B #\c)
  t)

(deftest char-lessp.12
  (char-lessp #\a #\A #\c)
  nil)

(deftest char-lessp.13
  (char-lessp #\a #\c #\a)
  nil)

(deftest char-lessp.14
  (char-lessp #\a #\b #\b)
  nil)

(deftest char-lessp.15
  (char-lessp #\c #\b #\a)
  nil)

(deftest-error char-lessp-error.1
  (eval '(char-lessp))
  program-error)

(deftest-error char-lessp-error.2
  (eval '(char-lessp 10)))


;;
;;  Function CHAR-GREATERP
;;
(deftest char-greaterp.1
  (char-greaterp #\z)
  t)

(deftest char-greaterp.2
  (char-greaterp #\b #\a)
  t)

(deftest char-greaterp.3
  (char-greaterp #\B #\a)
  t)

(deftest char-greaterp.4
  (char-greaterp #\b #\A)
  t)

(deftest char-greaterp.5
  (char-greaterp #\a #\c)
  nil)

(deftest char-greaterp.6
  (char-greaterp #\A #\c)
  nil)

(deftest char-greaterp.7
  (char-greaterp #\a #\C)
  nil)

(deftest char-greaterp.8
  (char-greaterp #\a #\a)
  nil)

(deftest char-greaterp.9
  (char-greaterp #\a #\A)
  nil)

(deftest char-greaterp.10
  (char-greaterp #\A #\a)
  nil)

(deftest char-greaterp.11
  (char-greaterp #\c #\B #\a)
  t)

(deftest char-greaterp.12
  (char-greaterp #\c #\c #\a)
  nil)

(deftest char-greaterp.13
  (char-greaterp #\c #\a #\c)
  nil)

(deftest char-greaterp.14
  (char-greaterp #\b #\a #\a)
  nil)

(deftest char-greaterp.15
  (char-greaterp #\a #\b #\c)
  nil)

(deftest-error char-greaterp-error.1
  (eval '(char-greaterp))
  program-error)

(deftest-error char-greaterp-error.2
  (eval '(char-greaterp 10)))


;;
;;  Function CHAR-NOT-GREATERP
;;
(deftest char-not-greaterp.1
  (char-not-greaterp #\a)
  t)

(deftest char-not-greaterp.2
  (char-not-greaterp #\a #\b)
  t)

(deftest char-not-greaterp.3
  (char-not-greaterp #\A #\b)
  t)

(deftest char-not-greaterp.4
  (char-not-greaterp #\a #\B)
  t)

(deftest char-not-greaterp.5
  (char-not-greaterp #\c #\c)
  t)

(deftest char-not-greaterp.6
  (char-not-greaterp #\C #\c)
  t)

(deftest char-not-greaterp.7
  (char-not-greaterp #\c #\C)
  t)

(deftest char-not-greaterp.8
  (char-not-greaterp #\b #\a)
  nil)

(deftest char-not-greaterp.9
  (char-not-greaterp #\B #\a)
  nil)

(deftest char-not-greaterp.10
  (char-not-greaterp #\b #\A)
  nil)

(deftest char-not-greaterp.11
  (char-not-greaterp #\c #\E #\g)
  t)

(deftest char-not-greaterp.12
  (char-not-greaterp #\C #\e #\e)
  t)

(deftest char-not-greaterp.13
  (char-not-greaterp #\e #\c #\g)
  nil)

(deftest char-not-greaterp.14
  (char-not-greaterp #\c #\g #\e)
  nil)

(deftest char-not-greaterp.15
  (char-not-greaterp #\e #\g #\c)
  nil)

(deftest char-not-greaterp.16
  (char-not-greaterp #\g #\e #\c)
  nil)

(deftest-error char-not-greaterp-error.1
  (eval '(char-not-greaterp))
  program-error)

(deftest-error char-not-greaterp-error.2
  (eval '(char-not-greaterp 10)))


;;
;;  Function CHAR-NOT-LESSP
;;
(deftest char-not-lessp.1
  (char-not-lessp #\a)
  t)

(deftest char-not-lessp.2
  (char-not-lessp #\b #\a)
  t)

(deftest char-not-lessp.3
  (char-not-lessp #\B #\a)
  t)

(deftest char-not-lessp.4
  (char-not-lessp #\b #\A)
  t)

(deftest char-not-lessp.5
  (char-not-lessp #\a #\a)
  t)

(deftest char-not-lessp.6
  (char-not-lessp #\A #\a)
  t)

(deftest char-not-lessp.7
  (char-not-lessp #\a #\A)
  t)

(deftest char-not-lessp.8
  (char-not-lessp #\a #\b)
  nil)

(deftest char-not-lessp.9
  (char-not-lessp #\A #\b)
  nil)

(deftest char-not-lessp.10
  (char-not-lessp #\a #\B)
  nil)

(deftest char-not-lessp.11
  (char-not-lessp #\c #\B #\a)
  t)

(deftest char-not-lessp.12
  (char-not-lessp #\c #\c #\A)
  t)

(deftest char-not-lessp.13
  (char-not-lessp #\b #\c #\a)
  nil)

(deftest char-not-lessp.14
  (char-not-lessp #\c #\a #\b)
  nil)

(deftest char-not-lessp.15
  (char-not-lessp #\b #\a #\c)
  nil)

(deftest char-not-lessp.16
  (char-not-lessp #\a #\b #\c)
  nil)

(deftest-error char-not-lessp-error.1
  (eval '(char-not-lessp))
  program-error)

(deftest-error char-not-lessp-error.2
  (eval '(char-not-lessp 10)))


;;  ANSI Common Lisp
(deftest char-compare.1
  (char= #\d #\d)
  t)

(deftest char-compare.2
  (char= #\A #\a)
  nil)

(deftest char-compare.3
  (char= #\d #\x)
  nil)

(deftest char-compare.4
  (char= #\d #\D)
  nil)

(deftest char-compare.5
  (char/= #\d #\d)
  nil)

(deftest char-compare.6
  (char/= #\d #\x)
  t)

(deftest char-compare.7
  (char/= #\d #\D)
  t)

(deftest char-compare.8
  (char= #\d #\d #\d #\d)
  t)

(deftest char-compare.9
  (char/= #\d #\d #\d #\d)
  nil)

(deftest char-compare.10
  (char= #\d #\d #\x #\d)
  nil)

(deftest char-compare.11
  (char/= #\d #\d #\x #\d)
  nil)

(deftest char-compare.12
  (char= #\d #\y #\x #\c)
  nil)

(deftest char-compare.13
  (char/= #\d #\y #\x #\c)
  t)

(deftest char-compare.14
  (char= #\d #\c #\d)
  nil)

(deftest char-compare.15
  (char/= #\d #\c #\d)
  nil)

(deftest char-compare.16
  (char< #\d #\x)
  t)

(deftest char-compare.17
  (char<= #\d #\x)
  t)

(deftest char-compare.18
  (char< #\d #\d)
  nil)

(deftest char-compare.19
  (char<= #\d #\d)
  t)

(deftest char-compare.20
  (char< #\a #\e #\y #\z)
  t)

(deftest char-compare.21
  (char<= #\a #\e #\y #\z)
  t)

(deftest char-compare.22
  (char< #\a #\e #\e #\y)
  nil)

(deftest char-compare.23
  (char<= #\a #\e #\e #\y)
  t)

(deftest char-compare.24
  (char> #\e #\d)
  t)

(deftest char-compare.25
  (char>= #\e #\d)
  t)

(deftest char-compare.26
  (char> #\d #\c #\b #\a)
  t)

(deftest char-compare.27
  (char>= #\d #\c #\b #\a)
  t)

(deftest char-compare.28
  (char> #\d #\d #\c #\a)
  nil)

(deftest char-compare.29
  (char>= #\d #\d #\c #\a)
  t)

(deftest char-compare.30
  (char> #\e #\d #\b #\c #\a)
  nil)

(deftest char-compare.31
  (char>= #\e #\d #\b #\c #\a)
  nil)

(deftest char-compare.32
  (char> #\z #\A)
  t)

(deftest char-compare.33
  (char> #\Z #\a)
  nil)

(deftest char-compare.34
  (char-equal #\A #\a)
  t)

(deftest char-compare.35
  (stable-sort (list #\b #\A #\B #\a #\c #\C) #'char-lessp)
  (#\A #\a #\b #\B #\c #\C))

(deftest char-compare.36
  (stable-sort (list #\b #\A #\B #\a #\c #\C) #'char<)
  (#\A #\B #\C #\a #\b #\c))

