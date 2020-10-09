;;
;;  ANSI COMMON LISP: 13. Characters
;;

;;
;;  System Class CHARACTER
;;
(deftest character-type.1
  (lisp-system:closp
    (find-class 'character))
  t)

(deftest character-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'character)))
  (character t))

(deftest character-type.3
  (typep #\A 'character)
  t)

(deftest character-type.4
  (typep #\u3030 'character)
  t)

(deftest character-type.5
  (typep 10 'character)
  nil)

(deftest character-type.6
  (typep nil 'character)
  nil)


;;
;;  Type BASE-CHAR
;;
(deftest base-char-type.1
  (subtypep 'base-char 'character)
  t t)

(deftest base-char-type.2
  (subtypep 'standard-char 'base-char)
  t t)

(deftest base-char-type.3
  (subtypep 'base-char 'standard-char)
  nil t)

(deftest base-char-type.4
  (typep #\A 'base-char)
  t)

(deftest base-char-type.5
  (typep #\u3030 'base-char)
  t)

(deftest base-char-type.6
  (typep nil 'base-char)
  nil)


;;
;;  Type STANDARD-CHAR
;;
(deftest standard-char.1
  (subtypep 'standard-char 'character)
  t t)

(deftest standard-char.2
  (typep #\A 'standard-char)
  t)

(deftest standard-char.3
  (typep #\u3030 'standard-char)
  nil)

(deftest standard-char.4
  (typep nil 'standard-char)
  nil)

(deftest standard-char.5
  (every
    (lambda (x)
      (typep x 'standard-char))
    (concatenate
      'string
      "!\"#$%&'(),;<=>?[\\]^_`{|}~.+-*/@"
      "0123456789"
      "abcdefghijklmnopqrstuvwxyz"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  t)


;;
;;  Type EXTENDED-CHAR
;;
(deftest extended-char.1
  (subtypep 'extended-char 'character)
  t t)

(deftest extended-char.2
  (typep #\A 'extended-char)
  nil)

(deftest extended-char.3
  (typep nil 'extended-char)
  nil)


;;
;;  Function CHARACTERP
;;
(deftest characterp.1
  (characterp #\a)
  t)

(deftest characterp.2
  (characterp 'a)
  nil)

(deftest characterp.3
  (characterp "a")
  nil)

(deftest characterp.4
  (characterp 65.)
  nil)

(deftest characterp.5
  (characterp #\Newline)
  t)

(deftest characterp.6
  (characterp #\Rubout)
  t)

(deftest-error! characterp-error.1
  (eval '(characterp)))

(deftest-error! characterp-error.2
  (eval '(characterp 10 20)))


;;
;;  Function STANDARD-CHAR-P
;;
(deftest standard-char-p.1
  (standard-char-p #\Space)
  t)

(deftest standard-char-p.2
  (standard-char-p #\~)
  t)

(deftest standard-char-p.3
  (standard-char-p #\Bell)
  nil)

(deftest standard-char-p.4
  (every
    #'standard-char-p
    (concatenate
      'string
      "!\"#$%&'(),;<=>?[\\]^_`{|}~.+-*/@"
      "0123456789"
      "abcdefghijklmnopqrstuvwxyz"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  t)

(deftest-error standard-char-p-error.1
  (eval '(standard-char-p 10))
  type-error)

(deftest-error! standard-char-p-error.2
  (eval '(standard-char-p)))

(deftest-error! standard-char-p-error.3
  (eval '(standard-char-p #\a #\a)))

