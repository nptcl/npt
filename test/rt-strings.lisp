;;
;;  ANSI COMMON LISP: 16. Strings
;;
(deftest simple-string-p.1
  (simple-string-p "Hello")
  t)

(deftest simple-string-p.2
  (simple-string-p
    (make-array
      10
      :element-type 'character
      :fill-pointer nil
      :adjustable nil
      :displaced-to nil))
  t)

(deftest simple-string-p.3
  (simple-string-p
    (make-array
      10
      :element-type 'character
      :fill-pointer nil
      :adjustable t
      :displaced-to nil))
  nil)

(deftest simple-string-p.4
  (simple-string-p
    (make-array
      10
      :element-type 't
      :fill-pointer nil
      :adjustable nil
      :displaced-to nil))
  nil)

(deftest simple-string-p.5
  (simple-string-p t)
  nil)

(deftest char.1
  (char "Hello" 0)
  #\H)

(deftest char.2
  (char "Hello" 4)
  #\o)

(defun strarray (x)
  (make-array (length x) :element-type 'character :initial-contents x))

(deftest char.3
  (char (strarray "Hello") 0)
  #\H)

(deftest char.4
  (char (strarray "Hello") 4)
  #\o)

(deftest char.5
  (char
    (make-array 5 :element-type 'character :initial-contents "Hello" :adjustable t)
    4)
  #\o)

(deftest schar.1
  (schar "Hello" 0)
  #\H)

(deftest schar.2
  (schar "Hello" 4)
  #\o)

(deftest schar.3
  (schar (strarray "Hello") 0)
  #\H)

(deftest schar.4
  (schar (strarray "Hello") 4)
  #\o)

(deftest-error schar.5
  (schar
    (make-array 5 :element-type 'character :initial-contents "Hello" :adjustable t)
    4)
  type-error)

(deftest setf-char.1
  (let ((a "Hello"))
    (setf (char a 1) #\a))
  #\a)

(deftest setf-char.2
  (let ((a "Hello"))
    (setf (char a 1) #\a)
    a)
  "Hallo")

(deftest setf-char.3
  (let ((a (strarray "Hello")))
    (setf (char a 1) #\a)
    a)
  "Hallo")

(deftest setf-schar.1
  (let ((a "Hello"))
    (setf (schar a 1) #\a))
  #\a)

(deftest setf-schar.2
  (let ((a "Hello"))
    (setf (schar a 1) #\a)
    a)
  "Hallo")

(deftest setf-schar.3
  (let ((a (strarray "Hello")))
    (setf (schar a 1) #\a)
    a)
  "Hallo")

(deftest-error setf-schar.4
  (let ((a (make-array 5 :element-type 'character
                       :initial-contents "Hello" :adjustable t)))
    (setf (schar a 1) #\a))
  type-error)

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

(deftest string-upcase.1
  (string-upcase "Hello10")
  "HELLO10")

(deftest string-upcase.2
  (let ((a "Hello10"))
    (eq (string-upcase a) a))
  nil)

(deftest string-upcase.3
  (string-upcase "Hello10" :start 2)
  "HeLLO10")

(deftest string-upcase.4
  (string-upcase "Hello10" :end 2)
  "HEllo10")

(deftest string-upcase.5
  (string-upcase "Hello10" :start 2 :end 3)
  "HeLlo10")

(deftest string-downcase.1
  (string-downcase "HelLO10")
  "hello10")

(deftest string-capitalize.1
  (string-capitalize "abc DEF ghi0jel 111z")
  "Abc Def Ghi0jel 111z")

(deftest string-capitalize.2
  (string-capitalize "elm 13c arthur;fig don't")
  "Elm 13c Arthur;Fig Don'T")

(deftest string-capitalize.3
  (string-capitalize " hello ")
  " Hello ")

(deftest string-capitalize.4
  (string-capitalize "occlUDeD cASEmenTs FOreSTAll iNADVertent DEFenestraTION")
  "Occluded Casements Forestall Inadvertent Defenestration")

(deftest string-capitalize.5
  (string-capitalize 'kludgy-hash-search)
  "Kludgy-Hash-Search")

(deftest string-capitalize.6
  (string-capitalize "DON'T!")
  "Don'T!")    ;not "Don't!"

(deftest string-capitalize.7
  (string-capitalize "pipe 13a, foo16c")
  "Pipe 13a, Foo16c")

(deftest nstring-upcase.1
  (nstring-upcase "Hello10")
  "HELLO10")

(deftest nstring-upcase.2
  (let ((a "Hello10"))
    (eq (nstring-upcase a) a))
  t)

(deftest nstring-upcase.3
  (nstring-upcase "Hello10" :start 2)
  "HeLLO10")

(deftest nstring-upcase.4
  (nstring-upcase "Hello10" :end 2)
  "HEllo10")

(deftest nstring-upcase.5
  (nstring-upcase "Hello10" :start 2 :end 3)
  "HeLlo10")

(deftest nstring-downcase.1
  (nstring-downcase "HelLO10")
  "hello10")

(deftest nstring-capitalize.1
  (nstring-capitalize "abc DEF ghi0jel 111z")
  "Abc Def Ghi0jel 111z")

(deftest nstring-capitalize.2
  (nstring-capitalize "elm 13c arthur;fig don't")
  "Elm 13c Arthur;Fig Don'T")

(deftest nstring-capitalize.3
  (nstring-capitalize " hello ")
  " Hello ")

(deftest nstring-capitalize.4
  (nstring-capitalize "occlUDeD cASEmenTs FOreSTAll iNADVertent DEFenestraTION")
  "Occluded Casements Forestall Inadvertent Defenestration")

(deftest nstring-capitalize.5
  (nstring-capitalize "kludgy-hash-search")
  "Kludgy-Hash-Search")

(deftest nstring-capitalize.6
  (nstring-capitalize "DON'T!")
  "Don'T!")    ;not "Don't!"

(deftest nstring-capitalize.7
  (nstring-capitalize "pipe 13a, foo16c")
  "Pipe 13a, Foo16c")

(deftest string-trim.1
  (string-trim '(#\a #\b) "abcdefgAAAaaabbb")
  "cdefgAAA")

(deftest string-trim.2
  (string-trim #(#\a #\b) "abcdefgAAAaaabbb")
  "cdefgAAA")

(deftest string-trim.3
  (string-trim "AB" 'abcdef)
  "CDEF")

(deftest string-left-trim.1
  (string-left-trim "ab" "abcdefgAAAaaabbb")
  "cdefgAAAaaabbb")

(deftest string-left-trim.2
  (string-left-trim "AB" 'abcdef)
  "CDEF")

(deftest string-right-trim.1
  (string-right-trim "ab" "abcdefgAAAaaabbb")
  "abcdefgAAA")

(deftest string-right-trim.2
  (string-right-trim "AB" 'abcdef)
  "ABCDEF")

(deftest string=.1
  (string= "Hello" "Hello")
  t)

(deftest string=.2
  (string= "Hello" "HEllo")
  nil)

(deftest string=.3
  (string= 'a #\A)
  t)

(deftest string=.4
  (string= "aaaHellobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  t)

(deftest string=.5
  (string= "aaa" "aaaa")
  nil)

(deftest string/=.1
  (string/= "Hello" "Hello")
  nil)

(deftest string/=.2
  (string/= "Hello" "HEllo")
  1)

(deftest string/=.3
  (string/= 'a #\A)
  nil)

(deftest string/=.4
  (string/= 'a #\a)
  0)

(deftest string/=.5
  (string/= "aaaHellobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  nil)

(deftest string/=.6
  (string/= "aaaHeLlobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  5)

(deftest string/=.7
  (string/= "aaa" "aaaa")
  3)

(deftest string/=.8
  (string/= "aaaa" "aaa")
  3)

(deftest string<.1
  (string< "abcdef" "abceef")
  3)

(deftest string<.2
  (string< "abcdef" "abcdef")
  nil)

(deftest string<.3
  (string< "abcdef" "abccef")
  nil)

(deftest string<.4
  (string< "aaa" "aaaa")
  3)

(deftest string<.5
  (string< "aaa" "aaa")
  nil)

(deftest string<.6
  (string< "aaa" "aa")
  nil)

(deftest string>.1
  (string> "abcdef" "abceef")
  nil)

(deftest string>.2
  (string> "abcdef" "abcdef")
  nil)

(deftest string>.3
  (string> "abcdef" "abccef")
  3)

(deftest string>.4
  (string> "aaa" "aaaa")
  nil)

(deftest string>.5
  (string> "aaa" "aaa")
  nil)

(deftest string>.6
  (string> "aaa" "aa")
  2)

(deftest string<=.1
  (string<= "abcdef" "abceef")
  3)

(deftest string<=.2
  (string<= "abcdef" "abcdef")
  6)

(deftest string<=.3
  (string<= "abcdef" "abccef")
  nil)

(deftest string<=.4
  (string<= "aaa" "aaaa")
  3)

(deftest string<=.5
  (string<= "aaa" "aaa")
  3)

(deftest string<=.6
  (string<= "aaa" "aa")
  nil)

(deftest string>=.1
  (string>= "abcdef" "abceef")
  nil)

(deftest string>=.2
  (string>= "abcdef" "abcdef")
  6)

(deftest string>=.3
  (string>= "abcdef" "abccef")
  3)

(deftest string>=.4
  (string>= "aaa" "aaaa")
  nil)

(deftest string>=.5
  (string>= "aaa" "aaa")
  3)

(deftest string>=.6
  (string>= "aaa" "aa")
  2)

(deftest string<.a
  (string< "aaaa" "aaab")
  3)

(deftest string>=.a
  (string>= "aaaaa" "aaaa")
  4)

(deftest string-equal.1
  (string-equal "Hello" "Hello")
  t)

(deftest string-equal.2
  (string-equal "Hello" "HEllo")
  t)

(deftest string-equal.3
  (string-equal "Hello" "HElloaaa")
  nil)

(deftest string-equal.4
  (string-equal 'a #\A)
  t)

(deftest string-equal.5
  (string-equal 'a #\a)
  t)

(deftest string-equal.6
  (string-equal 'a #\b)
  nil)

(deftest string-equal.7
  (string-equal "aaaHeLLobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  t)

(deftest string-equal.8
  (string-equal "aaa" "aaaa")
  nil)

(deftest string-not-equal.1
  (string-not-equal "Hello" "Hello")
  nil)

(deftest string-not-equal.2
  (string-not-equal "Hello" "HEllo")
  nil)

(deftest string-not-equal.3
  (string-not-equal "Hallo" "HEllo")
  1)

(deftest string-not-equal.4
  (string-not-equal 'a #\A)
  nil)

(deftest string-not-equal.5
  (string-not-equal 'a #\a)
  nil)

(deftest string-not-equal.6
  (string-not-equal 'b #\a)
  0)

(deftest string-not-equal.7
  (string-not-equal "aaaHeLLobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  nil)

(deftest string-not-equal.8
  (string-not-equal "aaaHeXlobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  5)

(deftest string-not-equal.9
  (string-not-equal "aaa" "aaaa")
  3)

(deftest string-not-equal.10
  (string-not-equal "aaaa" "aaa")
  3)

(deftest string-lessp.1
  (string-lessp "aBcdef" "abceef")
  3)

(deftest string-lessp.2
  (string-lessp "aBcdef" "abcdef")
  nil)

(deftest string-lessp.3
  (string-lessp "aBcdef" "abccef")
  nil)

(deftest string-lessp.4
  (string-lessp "aaa" "aaaa")
  3)

(deftest string-lessp.5
  (string-lessp "aaa" "aaa")
  nil)

(deftest string-lessp.6
  (string-lessp "aaa" "aa")
  nil)

(deftest string-greaterp.1
  (string-greaterp "aBcdef" "abceef")
  nil)

(deftest string-greaterp.2
  (string-greaterp "aBcdef" "abcdef")
  nil)

(deftest string-greaterp.3
  (string-greaterp "aBcdef" "abccef")
  3)

(deftest string-greaterp.4
  (string-greaterp "aAa" "aaaa")
  nil)

(deftest string-greaterp.5
  (string-greaterp "aAa" "aaa")
  nil)

(deftest string-greaterp.6
  (string-greaterp "aAa" "aa")
  2)

(deftest string-not-greaterp.1
  (string-not-greaterp "aBcdef" "abceef")
  3)

(deftest string-not-greaterp.2
  (string-not-greaterp "aBcdef" "abcdef")
  6)

(deftest string-not-greaterp.3
  (string-not-greaterp "aBcdef" "abccef")
  nil)

(deftest string-not-greaterp.4
  (string-not-greaterp "aaA" "aaaa")
  3)

(deftest string-not-greaterp.5
  (string-not-greaterp "aaA" "aaa")
  3)

(deftest string-not-greaterp.6
  (string-not-greaterp "aaA" "aa")
  nil)

(deftest string-not-lessp.1
  (string-not-lessp "Abcdef" "abceef")
  nil)

(deftest string-not-lessp.2
  (string-not-lessp "Abcdef" "abcdef")
  6)

(deftest string-not-lessp.3
  (string-not-lessp "Abcdef" "abccef")
  3)

(deftest string-not-lessp.4
  (string-not-lessp "Aaa" "aaaa")
  nil)

(deftest string-not-lessp.5
  (string-not-lessp "Aaa" "aaa")
  3)

(deftest string-not-lessp.6
  (string-not-lessp "Aaa" "aa")
  2)

(deftest string-lessp.a
  (string-lessp "aaaa" "aaab")
  3)

(deftest string-not-lessp.a
  (string-not-lessp "aaaaa" "aaaa")
  4)

(deftest stringp.1
  (stringp "Hello")
  t)

(deftest stringp.2
  (stringp
    (make-array
      10
      :element-type 'character
      :fill-pointer nil
      :adjustable nil
      :displaced-to nil))
  t)

(deftest stringp.3
  (stringp
    (make-array
      10
      :element-type 'character
      :fill-pointer nil
      :adjustable t
      :displaced-to nil))
  t)

(deftest stringp.4
  (stringp
    (make-array
      10
      :element-type 't
      :fill-pointer nil
      :adjustable nil
      :displaced-to nil))
  nil)

(deftest stringp.5
  (stringp t)
  nil)

(deftest make-string.1
  (stringp
    (make-string 10))
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


;;
;;  do-tests
;;
(do-tests :test t)

