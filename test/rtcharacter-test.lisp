;;
;;  ANSI COMMON LISP: 13. Characters
;;

;;
;;  Function CHARACTER
;;
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

(deftest character.6
  (character '\a)
  #\a)

(deftest-error character-error.1
  (eval '(character 65.))
  type-error)

(deftest-error character-error.2
  (eval '(character 'apple))
  type-error)

(deftest-error! character-error.3
  (eval '(character)))

(deftest-error! character-error.4
  (eval '(character #\a #\a)))


;;
;;  Function ALPHA-CHAR-P
;;
(deftest alpha-char-p.1
  (alpha-char-p #\a)
  t)

(deftest alpha-char-p.2
  (alpha-char-p #\z)
  t)

(deftest alpha-char-p.3
  (alpha-char-p #\A)
  t)

(deftest alpha-char-p.4
  (alpha-char-p #\Z)
  t)

(deftest alpha-char-p.5
  (alpha-char-p #\5)
  nil)

(deftest alpha-char-p.6
  (alpha-char-p #\Newline)
  nil)

(deftest-error! alpha-char-p-error.1
  (eval '(alpha-char-p)))

(deftest-error! alpha-char-p-error.2
  (eval '(alpha-char-p #\a #\a)))

(deftest-error alpha-char-p-error.3
  (eval '(alpha-char-p 10))
  type-error)


;;
;;  Function ALPHANUMERICP
;;
(deftest alphanumericp.1
  (alphanumericp #\a)
  t)

(deftest alphanumericp.2
  (alphanumericp #\Z)
  t)

(deftest alphanumericp.3
  (alphanumericp #\a)
  t)

(deftest alphanumericp.4
  (alphanumericp #\z)
  t)

(deftest alphanumericp.5
  (alphanumericp #\0)
  t)

(deftest alphanumericp.6
  (alphanumericp #\9)
  t)

(deftest alphanumericp.7
  (alphanumericp #\Newline)
  nil)

(deftest alphanumericp.8
  (alphanumericp #\#)
  nil)

(deftest-error! alphanumericp-error.1
  (eval '(alphanumericp)))

(deftest-error! alphanumericp-error.2
  (eval '(alphanumericp #\a #\a)))

(deftest-error alphanumericp-error.3
  (eval '(alphanumericp 10))
  type-error)


;;
;;  Function DIGIT-CHAR
;;
(deftest digit-char.1
  (digit-char 0)
  #\0)

(deftest digit-char.2
  (digit-char 7)
  #\7)

(deftest digit-char.3
  (digit-char 9)
  #\9)

(deftest digit-char.4
  (digit-char 10)
  nil)

(deftest digit-char.5
  (digit-char 12)
  nil)

(deftest digit-char.6
  (digit-char 0 10)
  #\0)

(deftest digit-char.7
  (digit-char 9 10)
  #\9)

(deftest digit-char.8
  (digit-char 10 10)
  nil)

(deftest digit-char.9
  (digit-char 9 11)
  #\9)

(deftest digit-char.10
  (digit-char 10 11)
  #\A)

(deftest digit-char.11
  (digit-char 12 16)
  #\C)

(deftest digit-char.12
  (digit-char 6 2)
  nil)

(deftest digit-char.13
  (digit-char 1 2)
  #\1)

(deftest digit-char.14
  (digit-char 35 36)
  #\Z)

(deftest-error digit-char-error.1
  (eval '(digit-char nil 10))
  type-error)

(deftest-error digit-char-error.2
  (eval '(digit-char 10 nil))
  type-error)

(deftest-error digit-char-error.3
  (eval '(digit-char 10 1)))

(deftest-error digit-char-error.4
  (eval '(digit-char 10 37)))

(deftest-error! digit-char-error.5
  (eval '(digit-char)))

(deftest-error! digit-char-error.6
  (eval '(digit-char 10 20 30)))


;;
;;  Function DIGIT-CHAR-P
;;
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

(deftest digit-char-p.10
  (digit-char-p #\z 36)
  35)

(deftest digit-char-p.11
  (digit-char-p #\# 36)
  nil)

(deftest digit-char-p.12
  (mapcar #'(lambda (radix)
              (map 'list #'(lambda (x) (digit-char-p x radix))
                   "059AaFGZ"))
          '(2 8 10 16 36))
  ((0 nil nil nil nil nil nil nil)
   (0 5 nil nil nil nil nil nil)
   (0 5 9 nil nil nil nil nil)
   (0 5 9 10 10 15 nil nil)
   (0 5 9 10 10 15 16 35)))

(deftest-error digit-char-p-error.1
  (eval '(digit-char-p 10))
  type-error)

(deftest-error digit-char-p-error.2
  (eval '(digit-char-p #\a #\b))
  type-error)

(deftest-error! digit-char-p-error.3
  (eval '(digit-char-p)))

(deftest-error! digit-char-p-error.4
  (eval '(digit-char-p #\a 16 17)))

(deftest-error digit-char-p-error.5
  (eval '(digit-char-p #\a 1)))

(deftest-error digit-char-p-error.6
  (eval '(digit-char-p #\a 37)))


;;
;;  Function GRAPHIC-CHAR-P
;;
(deftest graphic-char-p.1
  (graphic-char-p #\a)
  t)

(deftest graphic-char-p.2
  (graphic-char-p #\G)
  t)

(deftest graphic-char-p.3
  (graphic-char-p #\3)
  t)

(deftest graphic-char-p.4
  (graphic-char-p #\#)
  t)

(deftest graphic-char-p.5
  (graphic-char-p #\Space)
  t)

(deftest graphic-char-p.6
  (graphic-char-p #\Newline)
  nil)

(deftest-error graphic-char-p-error.1
  (eval '(graphic-char-p 10))
  type-error)

(deftest-error! graphic-char-p-error.2
  (eval '(graphic-char-p)))

(deftest-error! graphic-char-p-error.3
  (eval '(graphic-char-p #\a #\a)))


;;
;;  Function CHAR-UPCASE
;;
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
  (char-upcase #\z)
  #\Z)

(deftest char-upcase.5
  (char-upcase #\9)
  #\9)

(deftest char-upcase.6
  (char-upcase #\@)
  #\@)

(deftest-error char-upcase-error.1
  (eval '(char-upcase 10))
  type-error)

(deftest-error! char-upcase-error.2
  (eval '(char-upcase)))

(deftest-error! char-upcase-error.3
  (eval '(char-upcase #\a #\a)))


;;
;;  Function CHAR-DOWNCASE
;;
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
  (char-downcase #\Z)
  #\z)

(deftest char-downcase.5
  (char-downcase #\1)
  #\1)

(deftest char-downcase.6
  (char-downcase #\#)
  #\#)

(deftest-error char-downcase-error.1
  (eval '(char-downcase 10))
  type-error)

(deftest-error! char-downcase-error.2
  (eval '(char-downcase)))

(deftest-error! char-downcase-error.3
  (eval '(char-downcase #\a #\a)))


;;  ANSI Common Lisp
(deftest char-upcase-test.1
  (char-upcase #\a)
  #\A)

(deftest char-upcase-test.2
  (char-upcase #\A)
  #\A)

(deftest char-upcase-test.3
  (char-downcase #\a)
  #\a)

(deftest char-upcase-test.4
  (char-downcase #\A)
  #\a)

(deftest char-upcase-test.5
  (char-upcase #\9)
  #\9)

(deftest char-upcase-test.6
  (char-downcase #\9)
  #\9)

(deftest char-upcase-test.7
  (char-upcase #\@)
  #\@)

(deftest char-upcase-test.8
  (char-downcase #\@)
  #\@)

(deftest char-upcase-test.9
  (dotimes (code 300)
    (let ((char (code-char code)))
      (when char
        (unless (cond ((upper-case-p char)
                       (char= (char-upcase (char-downcase char)) char))
                      ((lower-case-p char)
                       (char= (char-downcase (char-upcase char)) char))
                      (t (and (char= (char-upcase (char-downcase char)) char)
                              (char= (char-downcase (char-upcase char)) char))))
          (return char)))))
  nil)


;;
;;  Function UPPER-CASE-P
;;
(deftest upper-case-p.1
  (upper-case-p #\A)
  t)

(deftest upper-case-p.2
  (upper-case-p #\a)
  nil)

(deftest upper-case-p.3
  (upper-case-p #\Z)
  t)

(deftest upper-case-p.4
  (upper-case-p #\z)
  nil)

(deftest upper-case-p.5
  (upper-case-p #\G)
  t)

(deftest upper-case-p.6
  (upper-case-p #\5)
  nil)

(deftest upper-case-p.7
  (upper-case-p #\Bell)
  nil)

(deftest-error upper-case-p-error.1
  (eval '(upper-case-p 10))
  type-error)

(deftest-error! upper-case-p-error.2
  (eval '(upper-case-p)))

(deftest-error! upper-case-p-error.3
  (eval '(upper-case-p #\a #\a)))


;;
;;  Function LOWER-CASE-P
;;
(deftest lower-case-p.1
  (lower-case-p #\a)
  t)

(deftest lower-case-p.2
  (lower-case-p #\A)
  nil)

(deftest lower-case-p.3
  (lower-case-p #\z)
  t)

(deftest lower-case-p.4
  (lower-case-p #\Z)
  nil)

(deftest lower-case-p.5
  (lower-case-p #\w)
  t)

(deftest lower-case-p.6
  (lower-case-p #\4)
  nil)

(deftest lower-case-p.7
  (lower-case-p #\@)
  nil)

(deftest-error lower-case-p-error.1
  (eval '(lower-case-p 10))
  type-error)

(deftest-error! lower-case-p-error.2
  (eval '(lower-case-p)))

(deftest-error! lower-case-p-error.3
  (eval '(lower-case-p #\a #\a)))


;;
;;  Function BOTH-CASE-P
;;
(deftest both-case-p.1
  (both-case-p #\a)
  t)

(deftest both-case-p.2
  (both-case-p #\A)
  t)

(deftest both-case-p.3
  (both-case-p #\z)
  t)

(deftest both-case-p.4
  (both-case-p #\Z)
  t)

(deftest both-case-p.5
  (both-case-p #\g)
  t)

(deftest both-case-p.6
  (both-case-p #\5)
  nil)

(deftest both-case-p.7
  (both-case-p #\%)
  nil)

(deftest-error both-case-p-error.1
  (eval '(both-case-p 10))
  type-error)

(deftest-error! both-case-p-error.2
  (eval '(both-case-p)))

(deftest-error! both-case-p-error.3
  (eval '(both-case-p #\a #\a)))


;;  ANSI Common Lisp
(deftest upper-case-p-test.1
  (upper-case-p #\A)
  t)

(deftest upper-case-p-test.2
  (upper-case-p #\a)
  nil)

(deftest upper-case-p-test.3
  (both-case-p #\a)
  t)

(deftest upper-case-p-test.4
  (both-case-p #\5)
  nil)

(deftest upper-case-p-test.5
  (lower-case-p #\5)
  nil)

(deftest upper-case-p-test.6
  (upper-case-p #\5)
  nil)

(deftest upper-case-p-test.7
  (lower-case-p #\Bell)
  nil)


;;
;;  Function CHAR-CODE
;;
(deftest char-code.1
  (char-code #\$)
  36)

(deftest char-code.2
  (char-code #\a)
  97)

(deftest char-code.3
  (char-code #\A)
  65)

(deftest char-code.4
  (char-code #\u3033)
  #x3033)

(deftest-error char-code-error.1
  (eval '(char-code 10))
  type-error)

(deftest-error! char-code-error.2
  (eval '(char-code)))

(deftest-error! char-code-error.3
  (eval '(char-code #\a #\a)))


;;
;;  Function CHAR-INT
;;
(deftest char-int.1
  (char-int #\$)
  36)

(deftest char-int.2
  (char-int #\a)
  97)

(deftest char-int.3
  (char-int #\A)
  65)

(deftest char-int.4
  (char-int #\u3033)
  #x3033)

(deftest-error char-int-error.1
  (eval '(char-int 10))
  type-error)

(deftest-error! char-int-error.2
  (eval '(char-int)))

(deftest-error! char-int-error.3
  (eval '(char-int #\a #\a)))


;;
;;  Function CODE-CHAR
;;
(deftest code-char.1
  (code-char 65.)
  #\A)

(deftest code-char.2
  (code-char 97)
  #\a)

(deftest code-char.3
  (code-char (char-code #\Space))
  #\Space)

(deftest code-char.4
  (let ((x (1- char-code-limit)))
    (eql (char-code (code-char x)) x))
  t)

(deftest code-char.5
  (code-char #xD800)  ;; Surrogate pair code
  nil)

(deftest-error code-char-error.1
  (eval '(code-char -1)))

(deftest-error code-char-error.2
  (eval '(code-char char-code-limit)))

(deftest-error code-char-error.3
  (eval '(code-char "Hello")))

(deftest-error! code-char-error.4
  (eval '(code-char)))

(deftest-error! code-char-error.5
  (eval '(code-char 10 20)))


;;
;;  Constant Variable CHAR-CODE-LIMIT
;;
(deftest char-code-limit.1
  char-code-limit
  #x110000)

(deftest char-code-limit.2
  (<= 96 char-code-limit)
  t)

(deftest-error char-code-limit.3
  (eval '(setq char-code-limit 10)))


;;
;;  Function CHAR-NAME
;;
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

(deftest char-name.6
  (char-name #\T)
  nil)  ;; Unicode: LATIN-CAPITAL-LETTER-T

(deftest-error char-name-error.1
  (eval '(char-name 10))
  type-error)

(deftest-error! char-name-error.2
  (eval '(char-name)))

(deftest-error! char-name-error.3
  (eval '(char-name #\a #\a)))


;;
;;  Function NAME-CHAR
;;
(deftest name-char.1
  (name-char 'space)
  #\Space)

(deftest name-char.2
  (name-char "space")
  #\Space)

(deftest name-char.3
  (name-char "Space")
  #\Space)

(deftest name-char.4
  (name-char 'a)
  nil)

(deftest name-char.5
  (name-char "A")
  nil)

(deftest-error name-char-error.1
  (eval '(name-char 10)))

(deftest-error! name-char-error.2
  (eval '(name-char)))

(deftest-error! name-char-error.3
  (eval '(name-char "a" "a")))

