;;
;;  ANSI COMMON LISP: 16. Strings
;;
(defun make-strarray (x)
  (make-array (length x) :element-type 'character :initial-contents x))


;;
;;  Accessor CHAR
;;
(deftest char.1
  (char "Hello" 0)
  #\H)

(deftest char.2
  (char "Hello" 4)
  #\o)

(deftest-error char.3
  (char "Hello" 5))

(deftest char.4
  (char (make-strarray "Hello") 0)
  #\H)

(deftest char.5
  (char (make-strarray "Hello") 4)
  #\o)

(deftest-error char.6
  (char (make-strarray "Hello") 5))

(deftest char.7
  (char
    (make-array 5 :element-type 'character :initial-contents "Hello" :adjustable t)
    4)
  #\o)

(deftest char.8
  (char
    (make-array 5 :element-type 'character
                :initial-contents "ABCDE"
                :fill-pointer 3)
    2)
  #\C)

(deftest char.9
  (char
    (make-array 5 :element-type 'character
                :initial-contents "ABCDE"
                :fill-pointer 3)
    3)
  #\D)

(deftest-error char.10
  (char
    (make-array 5 :element-type 'character
                :initial-contents "ABCDE"
                :fill-pointer 3)
    5))

(deftest-error char-error.1
  (eval '(char 10 20))
  type-error)

(deftest-error! char-error.2
  (eval '(char "Hello")))

(deftest-error! char-error.3
  (eval '(char "Hello" 20 30)))


;;
;;  Accessor (SETF CHAR)
;;
(deftest setf-char.1
  (setf (char "Hello" 0) #\A)
  #\A)

(deftest setf-char.2
  (let ((x "Hello"))
    (setf (char x 0) #\A)
    x)
  "Aello")

(deftest setf-char.3
  (let ((x "Hello"))
    (setf (char x 4) #\B)
    x)
  "HellB")

(deftest-error setf-char.4
  (setf (char "Hello" 5) #\C))

(deftest setf-char.5
  (setf (char (make-strarray "Hello") 0) #\A)
  #\A)

(deftest setf-char.6
  (let ((x (make-strarray "Hello")))
    (setf (char x 0) #\A)
    x)
  "Aello")

(deftest setf-char.7
  (let ((x (make-strarray "Hello")))
    (setf (char x 4) #\Z)
    x)
  "HellZ")

(deftest-error setf-char.8
  (setf (char (make-strarray "Hello") 5) #\H))

(deftest setf-char.9
  (let ((x (make-array 5 :element-type 'character
                       :initial-contents "Hello" :adjustable t)))
    (setf (char x 4) #\Z)
    x)
  "HellZ")

(deftest-error setf-char.10
  (let ((x (make-array 5 :element-type 'character
                       :initial-contents "ABCDE"
                       :fill-pointer 3)))
    (setf (char x 5) #\Z)))

(deftest setf-char.11
  (let ((x (make-array 5 :element-type 'character
                       :initial-contents "ABCDE"
                       :fill-pointer 3)))
    (setf (char x 2) #\Z)
    x)
  "ABZ")

(deftest setf-char.12
  (let ((x (make-array 5 :element-type 'character
                       :initial-contents "ABCDE"
                       :fill-pointer 3)))
    (setf (char x 3) #\Z)
    x)
  "ABC")

(deftest setf-char.13
  (let ((x (make-array 5 :element-type 'character
                       :initial-contents "ABCDE"
                       :fill-pointer 3)))
    (setf (char x 3) #\Z)
    (char x 3))
  #\Z)

(deftest-error setf-char-error.1
  (eval '(setf (char 10 20) #\A))
  type-error)

(deftest-error! setf-char-error.2
  (eval '(setf (char "Hello") #\Z)))

(deftest-error! setf-char-error.3
  (eval '(setf (char "Hello" 20 30) #\Z)))

(deftest-error setf-char-error.4
  (eval '(setf (char "Hello" 3) 10)))


;;
;;
;;  Accessor SCHAR
;;
(deftest schar.1
  (schar "Hello" 0)
  #\H)

(deftest schar.2
  (schar "Hello" 4)
  #\o)

(deftest-error schar.3
  (schar "Hello" 5))

(deftest schar.4
  (schar (make-strarray "Hello") 0)
  #\H)

(deftest schar.5
  (schar (make-strarray "Hello") 4)
  #\o)

(deftest-error schar.6
  (schar (make-strarray "Hello") 5))

(deftest-error schar.7
  (schar
    (make-array 5 :element-type 'character :initial-contents "Hello" :adjustable t)
    4)
  type-error)

(deftest-error schar.8
  (schar
    (make-array 5 :element-type 'character
                :initial-contents "ABCDE"
                :fill-pointer 3)
    2)
  type-error)

(deftest-error schar-error.1
  (eval '(schar 10 20))
  type-error)

(deftest-error! schar-error.2
  (eval '(schar "Hello")))

(deftest-error! schar-error.3
  (eval '(schar "Hello" 20 30)))


;;
;;  Accessor (SETF CHAR)
;;
(deftest setf-schar.1
  (setf (schar "Hello" 0) #\A)
  #\A)

(deftest setf-schar.2
  (let ((x "Hello"))
    (setf (schar x 0) #\A)
    x)
  "Aello")

(deftest setf-schar.3
  (let ((x "Hello"))
    (setf (schar x 4) #\B)
    x)
  "HellB")

(deftest-error setf-schar.4
  (setf (schar "Hello" 5) #\C))

(deftest setf-schar.5
  (setf (schar (make-strarray "Hello") 0) #\A)
  #\A)

(deftest setf-schar.6
  (let ((x (make-strarray "Hello")))
    (setf (schar x 0) #\A)
    x)
  "Aello")

(deftest setf-schar.7
  (let ((x (make-strarray "Hello")))
    (setf (schar x 4) #\Z)
    x)
  "HellZ")

(deftest-error setf-schar.8
  (setf (schar (make-strarray "Hello") 5) #\H))

(deftest-error setf-schar.9
  (let ((x (make-array 5 :element-type 'character
                       :initial-contents "Hello" :adjustable t)))
    (setf (schar x 4) #\Z)
    x)
  type-error)

(deftest-error setf-schar.10
  (let ((x (make-array 5 :element-type 'character
                       :initial-contents "ABCDE"
                       :fill-pointer 3)))
    (setf (schar x 2) #\Z)
    x)
  type-error)

(deftest-error setf-schar-error.1
  (eval '(setf (schar 10 20) #\A))
  type-error)

(deftest-error! setf-schar-error.2
  (eval '(setf (schar "Hello") #\Z)))

(deftest-error! setf-schar-error.3
  (eval '(setf (schar "Hello" 20 30) #\Z)))

(deftest-error setf-schar-error.4
  (eval '(setf (schar "Hello" 3) 10)))


;;  ANSI Common Lisp
(deftest char-test.1
  (progn
    (setq *char-string* (make-string 6 :initial-element #\A))
    (schar *char-string* 4))
  #\A)

(deftest char-test.2
  (setf (schar *char-string* 4) #\B)
  #\B)

(deftest char-test.3
  *char-string*
  "AAAABA")

(deftest char-test.4
  (setq *char-filled*
        (make-array 6 :element-type 'character
                    :fill-pointer 5 :initial-contents *char-string*))
  "AAAAB")

(deftest char-test.5
  (char *char-filled* 4)
  #\B)

(deftest char-test.6
  (char *char-filled* 5)
  #\A)

(deftest char-test.7
  (setf (char *char-filled* 3) #\C)
  #\C)

(deftest char-test.8
  (setf (char *char-filled* 5) #\D)
  #\D)

(deftest char-test.9
  (setf (fill-pointer *char-filled*) 6)
  6)

(deftest char-test.10
  *char-filled*
  "AAACBD")


;;
;;  Function STRING
;;
(deftest string.1
  (string "Hello")
  "Hello")

(deftest string.2
  (string 'hello)
  "HELLO")

(deftest string.3
  (string #\a)
  "a")

(deftest string.4
  (string #\X)
  "X")

(deftest string.5
  (string nil)
  "NIL")

(deftest string.6
  (string t)
  "T")

(deftest string.7
  (string "already a string")
  "already a string")

(deftest string.8
  (string 'elm)
  "ELM")

(deftest string.9
  (string #\c)
  "c")

(deftest-error string-error.1
  (eval '(string 10))
  type-error)

(deftest-error! string-error.2
  (eval '(string)))

(deftest-error! string-error.3
  (eval '(string 'hello 10)))


;;
;;  Function STRING-UPCASE
;;
(deftest string-upcase.1
  (string-upcase "hello10a")
  "HELLO10A")

(deftest string-upcase.2
  (let ((a "hello10a"))
    (eq (string-upcase a) a))
  nil)

(deftest string-upcase.3
  (string-upcase "hello10a" :start 0)
  "HELLO10A")

(deftest string-upcase.4
  (string-upcase "hello10a" :start 2)
  "heLLO10A")

(deftest string-upcase.5
  (string-upcase "hello10a" :start 8)
  "hello10a")

(deftest string-upcase.6
  (string-upcase "hello10a" :end 2)
  "HEllo10a")

(deftest string-upcase.7
  (string-upcase "hello10a" :end 8)
  "HELLO10A")

(deftest string-upcase.8
  (string-upcase "hello10a" :end nil)
  "HELLO10A")

(deftest string-upcase.9
  (string-upcase "hello10a" :start 2 :end 3)
  "heLlo10a")

(deftest string-upcase.10
  (string-upcase "hello10a" :start 4 :end 4)
  "hello10a")

(deftest string-upcase.11
  (string-upcase 'hello)
  "HELLO")

(deftest string-upcase.12
  (string-upcase #\a)
  "A")

(deftest-error string-upcase-error.1
  (eval '(string-upcase 10))
  type-error)

(deftest-error! string-upcase-error.2
  (eval '(string-upcase)))

(deftest-error string-upcase-error.3
  (eval '(string-upcase "Hello" nil)))

(deftest-error string-upcase-error.4
  (eval '(string-upcase "Hello" :start)))

(deftest-error string-upcase-error.5
  (eval '(string-upcase "Hello" :start #\A)))

(deftest-error string-upcase-error.6
  (eval '(string-upcase "Hello" :hello #\A)))

(deftest-error string-upcase-error.7
  (eval '(string-upcase "Hello" :start 6)))

(deftest-error string-upcase-error.8
  (eval '(string-upcase "Hello" :end 6)))

(deftest-error string-upcase-error.9
  (eval '(string-upcase "Hello" :start 3 :end 2)))


;;
;;  Function NSTRING-UPCASE
;;
(deftest nstring-upcase.1
  (nstring-upcase "hello10a")
  "HELLO10A")

(deftest nstring-upcase.2
  (let ((a "hello10a"))
    (eq (nstring-upcase a) a))
  t)

(deftest nstring-upcase.3
  (nstring-upcase "hello10a" :start 0)
  "HELLO10A")

(deftest nstring-upcase.4
  (nstring-upcase "hello10a" :start 2)
  "heLLO10A")

(deftest nstring-upcase.5
  (nstring-upcase "hello10a" :start 8)
  "hello10a")

(deftest nstring-upcase.6
  (nstring-upcase "hello10a" :end 2)
  "HEllo10a")

(deftest nstring-upcase.7
  (nstring-upcase "hello10a" :end 8)
  "HELLO10A")

(deftest nstring-upcase.8
  (nstring-upcase "hello10a" :end nil)
  "HELLO10A")

(deftest nstring-upcase.9
  (nstring-upcase "hello10a" :start 2 :end 3)
  "heLlo10a")

(deftest nstring-upcase.10
  (nstring-upcase "hello10a" :start 4 :end 4)
  "hello10a")

(deftest-error nstring-upcase-error.1
  (eval '(nstring-upcase 10))
  type-error)

(deftest-error! nstring-upcase-error.2
  (eval '(nstring-upcase)))

(deftest-error nstring-upcase-error.3
  (eval '(nstring-upcase "Hello" nil)))

(deftest-error nstring-upcase-error.4
  (eval '(nstring-upcase "Hello" :start)))

(deftest-error nstring-upcase-error.5
  (eval '(nstring-upcase "Hello" :start #\A)))

(deftest-error nstring-upcase-error.6
  (eval '(nstring-upcase "Hello" :hello #\A)))

(deftest-error nstring-upcase-error.7
  (eval '(nstring-upcase "Hello" :start 6)))

(deftest-error nstring-upcase-error.8
  (eval '(nstring-upcase "Hello" :end 6)))

(deftest-error nstring-upcase-error.9
  (eval '(nstring-upcase "Hello" :start 3 :end 2)))


;;
;;  Function STRING-UPCASE
;;
(deftest string-downcase.1
  (string-downcase "HELLO10A")
  "hello10a")

(deftest string-downcase.2
  (let ((a "HELLO10A"))
    (eq (string-downcase a) a))
  nil)

(deftest string-downcase.3
  (string-downcase "HELLO10A" :start 0)
  "hello10a")

(deftest string-downcase.4
  (string-downcase "HELLO10A" :start 2)
  "HEllo10a")

(deftest string-downcase.5
  (string-downcase "HELLO10A" :start 8)
  "HELLO10A")

(deftest string-downcase.6
  (string-downcase "HELLO10A" :end 2)
  "heLLO10A")

(deftest string-downcase.7
  (string-downcase "HELLO10A" :end 8)
  "hello10a")

(deftest string-downcase.8
  (string-downcase "HELLO10A" :end nil)
  "hello10a")

(deftest string-downcase.9
  (string-downcase "HELLO10A" :start 2 :end 3)
  "HElLO10A")

(deftest string-downcase.10
  (string-downcase "HELLO10A" :start 4 :end 4)
  "HELLO10A")

(deftest string-downcase.11
  (string-downcase 'hello)
  "hello")

(deftest string-downcase.12
  (string-downcase #\A)
  "a")

(deftest-error string-downcase-error.1
  (eval '(string-downcase 10))
  type-error)

(deftest-error! string-downcase-error.2
  (eval '(string-downcase)))

(deftest-error string-downcase-error.3
  (eval '(string-downcase "Hello" nil)))

(deftest-error string-downcase-error.4
  (eval '(string-downcase "Hello" :start)))

(deftest-error string-downcase-error.5
  (eval '(string-downcase "Hello" :start #\A)))

(deftest-error string-downcase-error.6
  (eval '(string-downcase "Hello" :hello #\A)))

(deftest-error string-downcase-error.7
  (eval '(string-downcase "Hello" :start 6)))

(deftest-error string-downcase-error.8
  (eval '(string-downcase "Hello" :end 6)))

(deftest-error string-downcase-error.9
  (eval '(string-downcase "Hello" :start 3 :end 2)))


;;
;;  Function NSTRING-UPCASE
;;
(deftest nstring-downcase.1
  (nstring-downcase "HELLO10A")
  "hello10a")

(deftest nstring-downcase.2
  (let ((a "HELLO10A"))
    (eq (nstring-downcase a) a))
  t)

(deftest nstring-downcase.3
  (nstring-downcase "HELLO10A" :start 0)
  "hello10a")

(deftest nstring-downcase.4
  (nstring-downcase "HELLO10A" :start 2)
  "HEllo10a")

(deftest nstring-downcase.5
  (nstring-downcase "HELLO10A" :start 8)
  "HELLO10A")

(deftest nstring-downcase.6
  (nstring-downcase "HELLO10A" :end 2)
  "heLLO10A")

(deftest nstring-downcase.7
  (nstring-downcase "HELLO10A" :end 8)
  "hello10a")

(deftest nstring-downcase.8
  (nstring-downcase "HELLO10A" :end nil)
  "hello10a")

(deftest nstring-downcase.9
  (nstring-downcase "HELLO10A" :start 2 :end 3)
  "HElLO10A")

(deftest nstring-downcase.10
  (nstring-downcase "HELLO10A" :start 4 :end 4)
  "HELLO10A")

(deftest-error nstring-downcase-error.1
  (eval '(nstring-downcase 10))
  type-error)

(deftest-error! nstring-downcase-error.2
  (eval '(nstring-downcase)))

(deftest-error nstring-downcase-error.3
  (eval '(nstring-downcase "Hello" nil)))

(deftest-error nstring-downcase-error.4
  (eval '(nstring-downcase "Hello" :start)))

(deftest-error nstring-downcase-error.5
  (eval '(nstring-downcase "Hello" :start #\A)))

(deftest-error nstring-downcase-error.6
  (eval '(nstring-downcase "Hello" :hello #\A)))

(deftest-error nstring-downcase-error.7
  (eval '(nstring-downcase "Hello" :start 6)))

(deftest-error nstring-downcase-error.8
  (eval '(nstring-downcase "Hello" :end 6)))

(deftest-error nstring-downcase-error.9
  (eval '(nstring-downcase "Hello" :start 3 :end 2)))


;;
;;  Function STRING-CAPITALIZE
;;
(deftest string-capitalize.1
  (string-capitalize "")
  "")

(deftest string-capitalize.2
  (string-capitalize "a")
  "A")

(deftest string-capitalize.3
  (string-capitalize "abc DEF ghi0jel 111z")
  "Abc Def Ghi0jel 111z")

(deftest string-capitalize.4
  (let ((x "abc DEF ghi0jel 111z"))
    (eq x (string-capitalize "abc DEF ghi0jel 111z")))
  nil)

(deftest string-capitalize.5
  (string-capitalize "abc DEF ghi0jel 111z" :start 0)
  "Abc Def Ghi0jel 111z")

(deftest string-capitalize.6
  (string-capitalize "abc DEF ghi0jel 111z" :start 1)
  "aBc Def Ghi0jel 111z")

(deftest string-capitalize.7
  (string-capitalize "abc DEF ghi0jel 111z" :start 3)
  "abc Def Ghi0jel 111z")

(deftest string-capitalize.8
  (string-capitalize "abc DEF ghi0jel 111z" :start 19)
  "abc DEF ghi0jel 111Z")

(deftest string-capitalize.9
  (string-capitalize "abc DEF ghi0jel 111z" :start 20)
  "abc DEF ghi0jel 111z")

(deftest string-capitalize.10
  (string-capitalize "abc DEF ghi0jel 111z" :end 0)
  "abc DEF ghi0jel 111z")

(deftest string-capitalize.11
  (string-capitalize "abc DEF ghi0jel 111z" :end 1)
  "Abc DEF ghi0jel 111z")

(deftest string-capitalize.12
  (string-capitalize "abc DEF ghi0jel 111z" :end 6)
  "Abc DeF ghi0jel 111z")

(deftest string-capitalize.13
  (string-capitalize "abc DEF ghi0jel 111z" :end 20)
  "Abc Def Ghi0jel 111z")

(deftest string-capitalize.14
  (string-capitalize "abc DEF ghi0jel 111z" :end nil)
  "Abc Def Ghi0jel 111z")

(deftest string-capitalize.15
  (string-capitalize "abc DEF ghi0jel 111z" :start 5 :end 10)
  "abc DEf Ghi0jel 111z")

(deftest string-capitalize.16
  (string-capitalize 'hello)
  "Hello")

(deftest string-capitalize.17
  (string-capitalize #\a)
  "A")

(deftest-error string-capitalize-error.1
  (eval '(string-capitalize 10))
  type-error)

(deftest-error! string-capitalize-error.2
  (eval '(string-capitalize)))

(deftest-error string-capitalize-error.3
  (eval '(string-capitalize "Hello" nil)))

(deftest-error string-capitalize-error.4
  (eval '(string-capitalize "Hello" :start)))

(deftest-error string-capitalize-error.5
  (eval '(string-capitalize "Hello" :start #\A)))

(deftest-error string-capitalize-error.6
  (eval '(string-capitalize "Hello" :hello #\A)))

(deftest-error string-capitalize-error.7
  (eval '(string-capitalize "Hello" :start 6)))

(deftest-error string-capitalize-error.8
  (eval '(string-capitalize "Hello" :end 6)))

(deftest-error string-capitalize-error.9
  (eval '(string-capitalize "Hello" :start 3 :end 2)))

(deftest string-capitalize-test.1
  (string-capitalize "elm 13c arthur;fig don't")
  "Elm 13c Arthur;Fig Don'T")

(deftest string-capitalize-test.2
  (string-capitalize " hello ")
  " Hello ")

(deftest string-capitalize-test.3
  (string-capitalize "occlUDeD cASEmenTs FOreSTAll iNADVertent DEFenestraTION")
  "Occluded Casements Forestall Inadvertent Defenestration")

(deftest string-capitalize-test.4
  (string-capitalize 'kludgy-hash-search)
  "Kludgy-Hash-Search")

(deftest string-capitalize-test.5
  (string-capitalize "DON'T!")
  "Don'T!")    ;not "Don't!"

(deftest string-capitalize-test.6
  (string-capitalize "pipe 13a, foo16c")
  "Pipe 13a, Foo16c")


;;
;;  Function NSTRING-CAPITALIZE
;;
(deftest nstring-capitalize.1
  (nstring-capitalize "")
  "")

(deftest nstring-capitalize.2
  (nstring-capitalize "a")
  "A")

(deftest nstring-capitalize.3
  (nstring-capitalize "abc DEF ghi0jel 111z")
  "Abc Def Ghi0jel 111z")

(deftest nstring-capitalize.4
  (let ((x "abc DEF ghi0jel 111z"))
    (eq x (nstring-capitalize "abc DEF ghi0jel 111z")))
  nil)

(deftest nstring-capitalize.5
  (nstring-capitalize "abc DEF ghi0jel 111z" :start 0)
  "Abc Def Ghi0jel 111z")

(deftest nstring-capitalize.6
  (nstring-capitalize "abc DEF ghi0jel 111z" :start 1)
  "aBc Def Ghi0jel 111z")

(deftest nstring-capitalize.7
  (nstring-capitalize "abc DEF ghi0jel 111z" :start 3)
  "abc Def Ghi0jel 111z")

(deftest nstring-capitalize.8
  (nstring-capitalize "abc DEF ghi0jel 111z" :start 19)
  "abc DEF ghi0jel 111Z")

(deftest nstring-capitalize.9
  (nstring-capitalize "abc DEF ghi0jel 111z" :start 20)
  "abc DEF ghi0jel 111z")

(deftest nstring-capitalize.10
  (nstring-capitalize "abc DEF ghi0jel 111z" :end 0)
  "abc DEF ghi0jel 111z")

(deftest nstring-capitalize.11
  (nstring-capitalize "abc DEF ghi0jel 111z" :end 1)
  "Abc DEF ghi0jel 111z")

(deftest nstring-capitalize.12
  (nstring-capitalize "abc DEF ghi0jel 111z" :end 6)
  "Abc DeF ghi0jel 111z")

(deftest nstring-capitalize.13
  (nstring-capitalize "abc DEF ghi0jel 111z" :end 20)
  "Abc Def Ghi0jel 111z")

(deftest nstring-capitalize.14
  (nstring-capitalize "abc DEF ghi0jel 111z" :end nil)
  "Abc Def Ghi0jel 111z")

(deftest nstring-capitalize.15
  (nstring-capitalize "abc DEF ghi0jel 111z" :start 5 :end 10)
  "abc DEf Ghi0jel 111z")

(deftest-error nstring-capitalize-error.1
  (eval '(nstring-capitalize 10))
  type-error)

(deftest-error! nstring-capitalize-error.2
  (eval '(nstring-capitalize)))

(deftest-error nstring-capitalize-error.3
  (eval '(nstring-capitalize "Hello" nil)))

(deftest-error nstring-capitalize-error.4
  (eval '(nstring-capitalize "Hello" :start)))

(deftest-error nstring-capitalize-error.5
  (eval '(nstring-capitalize "Hello" :start #\A)))

(deftest-error nstring-capitalize-error.6
  (eval '(nstring-capitalize "Hello" :hello #\A)))

(deftest-error nstring-capitalize-error.7
  (eval '(nstring-capitalize "Hello" :start 6)))

(deftest-error nstring-capitalize-error.8
  (eval '(nstring-capitalize "Hello" :end 6)))

(deftest-error nstring-capitalize-error.9
  (eval '(nstring-capitalize "Hello" :start 3 :end 2)))

(deftest nstring-capitalize-test.1
  (nstring-capitalize "elm 13c arthur;fig don't")
  "Elm 13c Arthur;Fig Don'T")

(deftest nstring-capitalize-test.2
  (nstring-capitalize " hello ")
  " Hello ")

(deftest nstring-capitalize-test.3
  (nstring-capitalize "occlUDeD cASEmenTs FOreSTAll iNADVertent DEFenestraTION")
  "Occluded Casements Forestall Inadvertent Defenestration")

(deftest nstring-capitalize-test.4
  (nstring-capitalize "kludgy-hash-search")
  "Kludgy-Hash-Search")

(deftest nstring-capitalize-test.5
  (nstring-capitalize "DON'T!")
  "Don'T!")    ;not "Don't!"

(deftest nstring-capitalize-test.6
  (nstring-capitalize "pipe 13a, foo16c")
  "Pipe 13a, Foo16c")


;;
;;  Function STRING-TRIM
;;
(deftest string-trim.1
  (string-trim '(#\a #\b) "abcdefgAAAaaabbb")
  "cdefgAAA")

(deftest string-trim.2
  (string-trim #(#\b #\a) "abcdefgAAAaaabbb")
  "cdefgAAA")

(deftest string-trim.3
  (string-trim "AB" 'abcdef)
  "CDEF")

(deftest string-trim.4
  (string-trim "AB" #\A)
  "")

(deftest string-trim.5
  (string-trim "ab" (make-array 5 :element-type 'character
                                :initial-contents "AaaaB"
                                :fill-pointer 3))
  "A")

(deftest-error string-trim-error.1
  (eval '(string-trim 10 "Hello"))
  type-error)

(deftest-error string-trim-error.2
  (eval '(string-trim "AB" 20))
  type-error)

(deftest-error! string-trim-error.3
  (eval '(string-trim "AB")))

(deftest-error! string-trim-error.4
  (eval '(string-trim "AB" "CDE" nil)))


;;
;;  Function STRING-LEFT-TRIM
;;
(deftest string-left-trim.1
  (string-left-trim "ba" "abcdefgAAAaaabbb")
  "cdefgAAAaaabbb")

(deftest string-left-trim.2
  (string-left-trim "AB" 'abcdef)
  "CDEF")

(deftest string-left-trim.3
  (string-left-trim "AB" #\A)
  "")

(deftest string-left-trim.5
  (string-left-trim "ab" (make-array 5 :element-type 'character
                                     :initial-contents "AaaaB"
                                     :fill-pointer 3))
  "Aaa")

(deftest-error string-left-trim-error.1
  (eval '(string-left-trim 10 "Hello"))
  type-error)

(deftest-error string-left-trim-error.2
  (eval '(string-left-trim "AB" 20))
  type-error)

(deftest-error! string-left-trim-error.3
  (eval '(string-left-trim "AB")))

(deftest-error! string-left-trim-error.4
  (eval '(string-left-trim "AB" "CDE" nil)))


;;
;;  Function STRING-RIGHT-TRIM
;;
(deftest string-right-trim.1
  (string-right-trim "ab" "abcdefgAAAaaabbb")
  "abcdefgAAA")

(deftest string-right-trim.2
  (string-right-trim "AB" 'abcdef)
  "ABCDEF")
(deftest string-right-trim.3
  (string-right-trim "AB" #\A)
  "")

(deftest string-right-trim.5
  (string-right-trim "ab" (make-array 5 :element-type 'character
                                      :initial-contents "AaaaB"
                                      :fill-pointer 3))
  "A")

(deftest-error string-right-trim-error.1
  (eval '(string-right-trim 10 "Hello"))
  type-error)

(deftest-error string-right-trim-error.2
  (eval '(string-right-trim "AB" 20))
  type-error)

(deftest-error! string-right-trim-error.3
  (eval '(string-right-trim "AB")))

(deftest-error! string-right-trim-error.4
  (eval '(string-right-trim "AB" "CDE" nil)))


;;  ANSI Common Lisp
(deftest string-trim-test.1
  (string-trim "abc" "abcaakaaakabcaaa")
  "kaaak")

(deftest string-trim-test.2
  (string-trim '(#\Space #\Tab #\Newline)
               (concatenate
                 'string
                 " garbanzo beans"
                 '(#\newline #\tab #\tab #\space)))

  "garbanzo beans")

(deftest string-trim-test.3
  (string-trim " (*)" " ( *three (silly) words* ) ")
  "three (silly) words")

(deftest string-trim-test.4
  (string-left-trim "abc" "labcabcabc")
  "labcabcabc")

(deftest string-trim-test.5
  (string-left-trim " (*)" " ( *three (silly) words* ) ")
  "three (silly) words* ) ")

(deftest string-trim-test.6
  (string-right-trim " (*)" " ( *three (silly) words* ) ")
  " ( *three (silly) words")


;;
;;  Function MAKE-STRING
;;
(deftest make-string.1
  (stringp
    (make-string 5))
  t)

(deftest make-string.2
  (length
    (make-string 10))
  10)

(deftest make-string.3
  (make-string 10 :initial-element #\a)
  "aaaaaaaaaa")

(deftest make-string.4
  (make-string 5 :initial-element #\Z :element-type 'standard-char)
  "ZZZZZ")

(deftest-error make-string.5
  (make-string 5 :initial-element #\u3033 :element-type 'standard-char))

(deftest-error make-string-error.1
  (eval '(make-string :hello))
  type-error)

(deftest-error! make-string-error.2
  (eval '(make-string)))

(deftest-error make-string-error.3
  (eval '(make-string 10 nil)))

(deftest-error make-string-error.4
  (eval '(make-string 10 :initial-element)))

(deftest-error make-string-error.5
  (eval '(make-string 10 :initial-element 10)))

(deftest-error make-string-error.6
  (eval '(make-string 10 :hello 10)))

