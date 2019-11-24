;;
;;  ANSI COMMON LISP: 13. Characters
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

(deftest character.1
  (character #\a)
  #\a)

(deftest character.2
  (character #\A)
  #\A)

(deftest character.3
  (character "a")
  #\a)

(deftest character.4
  (character "A")
  #\A)

(deftest character.5
  (character 'a)
  #\A)

(deftest characterp.1
  (characterp #\a)
  t)

(deftest characterp.2
  (characterp "a")
  nil)

(deftest alpha-char-p.1
  (alpha-char-p #\a)
  t)

(deftest alpha-char-p.2
  (alpha-char-p #\Z)
  t)

(deftest alpha-char-p.3
  (alpha-char-p #\5)
  nil)

(deftest alpha-char-p.4
  (alpha-char-p #\Newline)
  nil)

(deftest alphanumericp.1
  (alphanumericp #\a)
  t)

(deftest alphanumericp.2
  (alphanumericp #\Z)
  t)

(deftest alphanumericp.3
  (alphanumericp #\9)
  t)

(deftest alphanumericp.4
  (alphanumericp #\Newline)
  nil)

(deftest alphanumericp.5
  (alphanumericp #\#)
  nil)

(deftest digit-char.1
  (digit-char 0)
  #\0)

(deftest digit-char.2
  (digit-char 10 11)
  #\A)

(deftest digit-char.3
  (digit-char 10 10)
  nil)

(deftest digit-char.4
  (digit-char 7)
  #\7)

(deftest digit-char.5
  (digit-char 12)
  nil)

(deftest digit-char.6
  (digit-char 12 16)
  #\C)

(deftest digit-char.7
  (digit-char 6 2)
  nil)

(deftest digit-char.8
  (digit-char 1 2)
  #\1)

(deftest digit-char-p.1
  (digit-char-p #\5)
  5)

(deftest digit-char-p.2
  (digit-char-p #\5 2)
  nil)

(deftest digit-char-p.3
  (digit-char-p #\A)
  nil)

(deftest digit-char-p.4
  (digit-char-p #\a)
  nil)

(deftest digit-char-p.5
  (digit-char-p #\A 11)
  10)

(deftest digit-char-p.6
  (digit-char-p #\a 11)
  10)

(deftest digit-char-p.7
  (digit-char-p #\9)
  9)

(deftest digit-char-p.8
  (digit-char-p #\f 16)
  15)

(deftest digit-char-p.9
  (digit-char-p #\G 16)
  nil)

(deftest graphic-char-p.1
  (graphic-char-p #\G)
  t)

(deftest graphic-char-p.2
  (graphic-char-p #\#)
  t)

(deftest graphic-char-p.3
  (graphic-char-p #\Space)
  t)

(deftest graphic-char-p.4
  (graphic-char-p #\Newline)
  nil)

(deftest standard-char-p.1
  (standard-char-p #\Space)
  t)

(deftest standard-char-p.2
  (standard-char-p #\~)
  t)

(deftest standard-char-p.3
  (standard-char-p #\Bell)
  nil)

(deftest char-upcase.1
  (char-upcase #\a)
  #\A)

(deftest char-upcase.2
  (char-upcase #\A)
  #\A)

(deftest char-upcase.3
  (char-upcase #\g)
  #\G)

(deftest char-upcase.4
  (char-upcase #\9)
  #\9)

(deftest char-upcase.5
  (char-upcase #\@)
  #\@)

(deftest char-downcase.1
  (char-downcase #\a)
  #\a)

(deftest char-downcase.2
  (char-downcase #\A)
  #\a)

(deftest char-downcase.3
  (char-downcase #\G)
  #\g)

(deftest char-downcase.4
  (char-downcase #\1)
  #\1)

(deftest char-downcase.5
  (char-downcase #\#)
  #\#)

(deftest upper-case-p.1
  (upper-case-p #\A)
  t)

(deftest upper-case-p.2
  (upper-case-p #\a)
  nil)

(deftest upper-case-p.3
  (upper-case-p #\G)
  t)

(deftest upper-case-p.4
  (upper-case-p #\5)
  nil)

(deftest upper-case-p.5
  (upper-case-p #\Bell)
  nil)

(deftest lower-case-p.1
  (lower-case-p #\z)
  t)

(deftest lower-case-p.2
  (lower-case-p #\Z)
  nil)

(deftest lower-case-p.3
  (lower-case-p #\w)
  t)

(deftest lower-case-p.4
  (lower-case-p #\4)
  nil)

(deftest lower-case-p.5
  (lower-case-p #\@)
  nil)

(deftest both-case-p.1
  (both-case-p #\a)
  t)

(deftest both-case-p.2
  (both-case-p #\A)
  t)

(deftest both-case-p.3
  (both-case-p #\g)
  t)

(deftest both-case-p.4
  (both-case-p #\5)
  nil)

(deftest both-case-p.5
  (both-case-p #\%)
  nil)

(deftest char-code.1
  (char-code #\$)
  36)

(deftest char-code.2
  (char-code #\a)
  97)

(deftest char-code.3
  (char-code #\A)
  65)

(deftest char-int.1
  (char-int #\$)
  36)

(deftest char-int.2
  (char-int #\a)
  97)

(deftest char-int.3
  (char-int #\A)
  65)

(deftest code-char.1
  (code-char 65.)
  #\A)

(deftest code-char.2
  (code-char 97)
  #\a)

(deftest code-char.3
  (code-char (char-code #\Space))
  #\Space)

(deftest char-code-limit.1
  char-code-limit
  #x110000)

(deftest char-name.1
  (char-name #\ )
  "Space")

(deftest char-name.2
  (char-name #\Space)
  "Space")

(deftest char-name.3
  (char-name #\Page)
  "Page")

(deftest char-name.4
  (char-name #\a)
  nil)  ;; Unicode: LATIN-SMALL-LETTER-A

(deftest char-name.5
  (char-name #\Z)
  nil)  ;; Unicode: LATIN-CAPITAL-LETTER-Z

(deftest char-name-error.6
  (char-name #\T)
  nil)  ;; Unicode: LATIN-CAPITAL-LETTER-T

(deftest name-char.1
  (name-char 'space)
  #\Space)

(deftest name-char.2
  (name-char "space")
  #\Space)

(deftest name-char.3
  (name-char "Space")
  #\Space)


;;
;;  do-tests
;;
(do-tests :test t)

