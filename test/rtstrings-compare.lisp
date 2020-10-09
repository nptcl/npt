;;
;;  ANSI COMMON LISP: 16. Strings
;;

;;
;;  Function STRING=
;;
(deftest string=.1
  (string= "" "")
  t)

(deftest string=.2
  (string= "Hello" "")
  nil)

(deftest string=.3
  (string= "" "Hello")
  nil)

(deftest string=.4
  (string= "Hello" "Hello")
  t)

(deftest string=.5
  (string= "Hello" "HEllo")
  nil)

(deftest string=.6
  (string= 'a #\A)
  t)

(deftest string=.7
  (string= "aaa" "aaaa")
  nil)

(deftest string=.8
  (string= "aaaHellobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  t)

(deftest string=-start1.1
  (string= "aHello" "Hello" :start1 0)
  nil)

(deftest string=-start1.2
  (string= "aHello" "Hello" :start1 1)
  t)

(deftest string=-start1.3
  (string= "Hello" "" :start1 5)
  t)

(deftest string=-end1.1
  (string= "Hello" "" :end1 0)
  t)

(deftest string=-end1.2
  (string= "Hello" "" :end1 1)
  nil)

(deftest string=-end1.3
  (string= "Hello" "Hell" :end1 4)
  t)

(deftest string=-end1.4
  (string= "Hello" "Hello" :end1 5)
  t)

(deftest string=-end1.5
  (string= "Hello" "Hello" :end1 nil)
  t)

(deftest string=-start2.1
  (string= "Hello" "aHello" :start2 0)
  nil)

(deftest string=-start2.2
  (string= "Hello" "aHello" :start2 1)
  t)

(deftest string=-start2.3
  (string= "" "Hello" :start2 5)
  t)

(deftest string=-end2.1
  (string= "" "Hello" :end2 0)
  t)

(deftest string=-end2.2
  (string= "" "Hello" :end2 1)
  nil)

(deftest string=-end2.3
  (string= "Hell" "Hello" :end2 4)
  t)

(deftest string=-end2.4
  (string= "Hello" "Hello" :end2 5)
  t)

(deftest string=-end2.5
  (string= "Hello" "Hello" :end2 nil)
  t)

(deftest-error string=-error.1
  (eval '(string= 10 "Hello"))
  type-error)

(deftest-error string=-error.2
  (eval '(string= "Hello" 20))
  type-error)

(deftest-error! string=-error.3
  (eval '(string= "Hello")))

(deftest-error! string=-error.4
  (eval '(string= "Hello")))

(deftest-error string=-error.5
  (eval '(string= "Hello" "abc" :start1)))

(deftest-error string=-error.6
  (eval '(string= "Hello" "abc" :start1 "Hello")))

(deftest-error string=-error.7
  (eval '(string= "Hello" "abc" :start1 6)))

(deftest-error string=-error.8
  (eval '(string= "Hello" "abc" :end1 6)))

(deftest-error string=-error.9
  (eval '(string= "Hello" "abc" :start2 4)))

(deftest-error string=-error.10
  (eval '(string= "Hello" "abc" :end2 4)))

(deftest-error string=-error.11
  (eval '(string= "Hello" "abc" :start1 3 :end1 2)))

(deftest-error string=-error.12
  (eval '(string= "Hello" "abc" :start2 2 :end2 1)))


;;
;;  Function STRING/=
;;
(deftest string/=.1
  (string/= "" "")
  nil)

(deftest string/=.2
  (string/= "Hello" "")
  0)

(deftest string/=.3
  (string/= "" "Hello")
  0)

(deftest string/=.4
  (string/= "Hello" "Hello")
  nil)

(deftest string/=.5
  (string/= "Hello" "HEllo")
  1)

(deftest string/=.6
  (string/= 'a #\A)
  nil)

(deftest string/=.7
  (string/= 'a #\a)
  0)

(deftest string/=.8
  (string/= "aaa" "aaaa")
  3)

(deftest string/=.9
  (string/= "aaaHellobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  nil)

(deftest string/=.10
  (string/= "aaaHeLlobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  5)

(deftest string/=-start1.1
  (string/= "aHello" "Hello" :start1 0)
  0)

(deftest string/=-start1.2
  (string/= "aHello" "Hello" :start1 1)
  nil)

(deftest string/=-start1.3
  (string/= "aHello" "HelLo" :start1 1)
  4)

(deftest string/=-start1.4
  (string/= "Hello" "" :start1 5)
  nil)

(deftest string/=-end1.1
  (string/= "Hello" "" :end1 0)
  nil)

(deftest string/=-end1.2
  (string/= "Hello" "Hello" :end1 1)
  1)

(deftest string/=-end1.3
  (string/= "Hello" "H" :end1 1)
  nil)

(deftest string/=-end1.4
  (string/= "Hello" "Hello" :end1 5)
  nil)

(deftest string/=-end1.5
  (string/= "Hello" "Hello" :end1 nil)
  nil)

(deftest string/=-start2.1
  (string/= "Hello" "Hello" :start2 0)
  nil)

(deftest string/=-start2.2
  (string/= "Hello" "Hello" :start2 1)
  0)

(deftest string/=-start2.3
  (string/= "Hello" "aHello" :start2 1)
  nil)

(deftest string/=-start2.4
  (string/= "Hello" "aHelLo" :start2 1)
  3)

(deftest string/=-start2.5
  (string/= "" "HelLo" :start2 5)
  nil)

(deftest string/=-end2.1
  (string/= "" "HelLo" :end2 0)
  nil)

(deftest string/=-end2.2
  (string/= "Hello" "Hello" :end2 1)
  1)

(deftest string/=-end2.3
  (string/= "Hell" "Hello" :end2 4)
  nil)

(deftest string/=-end2.4
  (string/= "Hell" "HeLlo" :end2 4)
  2)

(deftest string/=-end2.5
  (string/= "Hello" "Hello" :end2 5)
  nil)

(deftest string/=-end2.6
  (string/= "Hello" "Hello" :end2 nil)
  nil)

(deftest string/=-start-end.1
  (string/= "Hello" "ABCDHello" :start1 2 :end1 4 :start2 6 :end2 8)
  nil)

(deftest string/=-start-end.2
  (string/= "Hello" "ABCDHeLlo" :start1 2 :end1 4 :start2 6 :end2 8)
  2)

(deftest string/=-start-end.3
  (string/= "Hello" "ABCDHelLo" :start1 2 :end1 4 :start2 6 :end2 8)
  3)

(deftest string/=-start-end.4
  (string/= "ABCDHello" "Hello" :start2 2 :end2 4 :start1 6 :end1 8)
  nil)

(deftest string/=-start-end.5
  (string/= "ABCDHeLlo" "Hello" :start2 2 :end2 4 :start1 6 :end1 8)
  6)

(deftest string/=-start-end.6
  (string/= "ABCDHelLo" "Hello" :start2 2 :end2 4 :start1 6 :end1 8)
  7)

(deftest-error string/=-error.1
  (eval '(string/= 10 "Hello"))
  type-error)

(deftest-error string/=-error.2
  (eval '(string/= "Hello" 20))
  type-error)

(deftest-error! string/=-error.3
  (eval '(string/= "Hello")))

(deftest-error! string/=-error.4
  (eval '(string/= "Hello")))

(deftest-error string/=-error.5
  (eval '(string/= "Hello" "abc" :start1)))

(deftest-error string/=-error.6
  (eval '(string/= "Hello" "abc" :start1 "Hello")))

(deftest-error string/=-error.7
  (eval '(string/= "Hello" "abc" :start1 6)))

(deftest-error string/=-error.8
  (eval '(string/= "Hello" "abc" :end1 6)))

(deftest-error string/=-error.9
  (eval '(string/= "Hello" "abc" :start2 4)))

(deftest-error string/=-error.10
  (eval '(string/= "Hello" "abc" :end2 4)))

(deftest-error string/=-error.11
  (eval '(string/= "Hello" "abc" :start1 3 :end1 2)))

(deftest-error string/=-error.12
  (eval '(string/= "Hello" "abc" :start2 2 :end2 1)))


;;
;;  Function STRING<
;;
(deftest string<.1
  (string< "" "")
  nil)

(deftest string<.2
  (string< "Hello" "")
  nil)

(deftest string<.3
  (string< "" "Hello")
  0)

(deftest string<.4
  (string< "abcdef" "abceef")
  3)

(deftest string<.5
  (string< "abcdef" "abcdef")
  nil)

(deftest string<.6
  (string< "abcdef" "abccef")
  nil)

(deftest string<.7
  (string< "aaa" "aaaa")
  3)

(deftest string<.8
  (string< "aaa" "aaa")
  nil)

(deftest string<.9
  (string< "aaa" "aa")
  nil)

(deftest string<.10
  (string< 'a #\A)
  nil)

(deftest string<.11
  (string< 'a #\B)
  0)

(deftest string<.12
  (string< "aaaHellobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  nil)

(deftest string<.13
  (string< "aaaHeLlobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  5)

(deftest string<.14
  (string< "aaaa" "aaab")
  3)

(deftest string<-start1.1
  (string< "aHello" "Hello" :start1 0)
  nil)

(deftest string<-start1.2
  (string< "AHello" "Hello" :start1 0)
  0)

(deftest string<-start1.3
  (string< "aHello" "Hello" :start1 1)
  nil)

(deftest string<-start1.4
  (string< "aHelLo" "Hello" :start1 1)
  4)

(deftest string<-start1.5
  (string< "Hello" "" :start1 5)
  nil)

(deftest string<-start1.6
  (string< "Hello" "Z" :start1 5)
  5)

(deftest string<-end1.1
  (string< "Hello" "" :end1 0)
  nil)

(deftest string<-end1.2
  (string< "Hello" "Z" :end1 0)
  0)

(deftest string<-end1.3
  (string< "Hello" "Hello" :end1 1)
  1)

(deftest string<-end1.4
  (string< "Hello" "H" :end1 1)
  nil)

(deftest string<-end1.5
  (string< "Hello" "Hello" :end1 5)
  nil)

(deftest string<-end1.6
  (string< "Hello" "Hellz" :end1 nil)
  4)

(deftest string<-start2.1
  (string< "Hello" "Hello" :start2 0)
  nil)

(deftest string<-start2.2
  (string< "Hello" "Hello" :start2 1)
  0)

(deftest string<-start2.3
  (string< "Hello" "aHello" :start2 1)
  nil)

(deftest string<-start2.4
  (string< "Hello" "aHelzo" :start2 1)
  3)

(deftest string<-start2.5
  (string< "" "HelLo" :start2 5)
  nil)

(deftest string<-end2.1
  (string< "" "HelLo" :end2 0)
  nil)

(deftest string<-end2.2
  (string< "Hello" "Hello" :end2 1)
  nil)

(deftest string<-end2.3
  (string< "Hello" "Zello" :end2 1)
  0)

(deftest string<-end2.4
  (string< "Hell" "Hello" :end2 4)
  nil)

(deftest string<-end2.5
  (string< "Hell" "Hezlo" :end2 4)
  2)

(deftest string<-end2.6
  (string< "Hello" "Hello" :end2 5)
  nil)

(deftest string<-end2.7
  (string< "Hello" "Hellz" :end2 nil)
  4)

(deftest string<-start-end.1
  (string< "Hello" "ABCDHello" :start1 2 :end1 4 :start2 6 :end2 8)
  nil)

(deftest string<-start-end.2
  (string< "Hello" "ABCDHezlo" :start1 2 :end1 4 :start2 6 :end2 8)
  2)

(deftest string<-start-end.3
  (string< "Hello" "ABCDHelzo" :start1 2 :end1 4 :start2 6 :end2 8)
  3)

(deftest string<-start-end.4
  (string< "ABCDHello" "Hello" :start2 2 :end2 4 :start1 6 :end1 8)
  nil)

(deftest string<-start-end.5
  (string< "ABCDHeLlo" "Hello" :start2 2 :end2 4 :start1 6 :end1 8)
  6)

(deftest string<-start-end.6
  (string< "ABCDHelLo" "Hello" :start2 2 :end2 4 :start1 6 :end1 8)
  7)

(deftest-error string<-error.1
  (eval '(string< 10 "Hello"))
  type-error)

(deftest-error string<-error.2
  (eval '(string< "Hello" 20))
  type-error)

(deftest-error! string<-error.3
  (eval '(string< "Hello")))

(deftest-error! string<-error.4
  (eval '(string< "Hello")))

(deftest-error string<-error.5
  (eval '(string< "Hello" "abc" :start1)))

(deftest-error string<-error.6
  (eval '(string< "Hello" "abc" :start1 "Hello")))

(deftest-error string<-error.7
  (eval '(string< "Hello" "abc" :start1 6)))

(deftest-error string<-error.8
  (eval '(string< "Hello" "abc" :end1 6)))

(deftest-error string<-error.9
  (eval '(string< "Hello" "abc" :start2 4)))

(deftest-error string<-error.10
  (eval '(string< "Hello" "abc" :end2 4)))

(deftest-error string<-error.11
  (eval '(string< "Hello" "abc" :start1 3 :end1 2)))

(deftest-error string<-error.12
  (eval '(string< "Hello" "abc" :start2 2 :end2 1)))


;;
;;  Function STRING>
;;
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


;;
;;  Function STRING<=
;;
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


;;
;;  Function STRING>=
;;
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

(deftest string>=.7
  (string>= "aaaaa" "aaaa")
  4)


;;
;;  Function STRING-EQUAL
;;
(deftest string-equal.1
  (string-equal "" "")
  t)

(deftest string-equal.2
  (string-equal "Hello" "")
  nil)

(deftest string-equal.3
  (string-equal "" "Hello")
  nil)

(deftest string-equal.4
  (string-equal "Hello" "Hello")
  t)

(deftest string-equal.5
  (string-equal "Hello" "HEllo")
  t)

(deftest string-equal.6
  (string-equal "HeZlo" "HEllo")
  nil)

(deftest string-equal.7
  (string-equal "Hello" "HElloaaa")
  nil)

(deftest string-equal.8
  (string-equal 'a #\A)
  t)

(deftest string-equal.9
  (string-equal 'a #\a)
  t)

(deftest string-equal.10
  (string-equal 'a #\b)
  nil)

(deftest string-equal.11
  (string-equal "aaaHeLLobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  t)

(deftest string-equal.12
  (string-equal "aaaHeZllobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  nil)

(deftest string-equal.13
  (string-equal "aaa" "aaaa")
  nil)

(deftest string-equal.14
  (string-equal "aaaa" "aaa")
  nil)

(deftest string-equal-start1.1
  (string-equal "aHello" "hello" :start1 0)
  nil)

(deftest string-equal-start1.2
  (string-equal "aHello" "hello" :start1 1)
  t)

(deftest string-equal-start1.3
  (string-equal "Hello" "" :start1 5)
  t)

(deftest string-equal-end1.1
  (string-equal "Hello" "" :end1 0)
  t)

(deftest string-equal-end1.2
  (string-equal "Hello" "" :end1 1)
  nil)

(deftest string-equal-end1.3
  (string-equal "Hello" "hell" :end1 4)
  t)

(deftest string-equal-end1.4
  (string-equal "Hello" "hello" :end1 5)
  t)

(deftest string-equal-end1.5
  (string-equal "Hello" "hello" :end1 nil)
  t)

(deftest string-equal-start2.1
  (string-equal "Hello" "ahello" :start2 0)
  nil)

(deftest string-equal-start2.2
  (string-equal "Hello" "ahello" :start2 1)
  t)

(deftest string-equal-start2.3
  (string-equal "" "hello" :start2 5)
  t)

(deftest string-equal-end2.1
  (string-equal "" "hello" :end2 0)
  t)

(deftest string-equal-end2.2
  (string-equal "" "hello" :end2 1)
  nil)

(deftest string-equal-end2.3
  (string-equal "Hell" "hello" :end2 4)
  t)

(deftest string-equal-end2.4
  (string-equal "Hello" "hello" :end2 5)
  t)

(deftest string-equal-end2.5
  (string-equal "Hello" "hello" :end2 nil)
  t)

(deftest-error string-equal-error.1
  (eval '(string-equal 10 "Hello"))
  type-error)

(deftest-error string-equal-error.2
  (eval '(string-equal "Hello" 20))
  type-error)

(deftest-error! string-equal-error.3
  (eval '(string-equal "Hello")))

(deftest-error! string-equal-error.4
  (eval '(string-equal "Hello")))

(deftest-error string-equal-error.5
  (eval '(string-equal "Hello" "abc" :start1)))

(deftest-error string-equal-error.6
  (eval '(string-equal "Hello" "abc" :start1 "Hello")))

(deftest-error string-equal-error.7
  (eval '(string-equal "Hello" "abc" :start1 6)))

(deftest-error string-equal-error.8
  (eval '(string-equal "Hello" "abc" :end1 6)))

(deftest-error string-equal-error.9
  (eval '(string-equal "Hello" "abc" :start2 4)))

(deftest-error string-equal-error.10
  (eval '(string-equal "Hello" "abc" :end2 4)))

(deftest-error string-equal-error.11
  (eval '(string-equal "Hello" "abc" :start1 3 :end1 2)))

(deftest-error string-equal-error.12
  (eval '(string-equal "Hello" "abc" :start2 2 :end2 1)))


;;
;;  Function STRING-NOT-EQUAL
;;
(deftest string-not-equal.1
  (string-not-equal "" "")
  nil)

(deftest string-not-equal.2
  (string-not-equal "Hello" "")
  0)

(deftest string-not-equal.3
  (string-not-equal "" "Hello")
  0)

(deftest string-not-equal.4
  (string-not-equal "Hello" "Hello")
  nil)

(deftest string-not-equal.5
  (string-not-equal "Hello" "HEllo")
  nil)

(deftest string-not-equal.6
  (string-not-equal "Hallo" "HEllo")
  1)

(deftest string-not-equal.7
  (string-not-equal 'a #\A)
  nil)

(deftest string-not-equal.8
  (string-not-equal 'a #\a)
  nil)

(deftest string-not-equal.9
  (string-not-equal 'b #\a)
  0)

(deftest string-not-equal.10
  (string-not-equal "aaaHeLLobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  nil)

(deftest string-not-equal.11
  (string-not-equal "aaaHeXlobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  5)

(deftest string-not-equal.12
  (string-not-equal "aAa" "aaaa")
  3)

(deftest string-not-equal.13
  (string-not-equal "aAaa" "aaa")
  3)

(deftest string-not-equal-start1.1
  (string-not-equal "aHello" "hello" :start1 0)
  0)

(deftest string-not-equal-start1.2
  (string-not-equal "aHello" "hello" :start1 1)
  nil)

(deftest string-not-equal-start1.3
  (string-not-equal "aHello" "helzo" :start1 1)
  4)

(deftest string-not-equal-start1.4
  (string-not-equal "Hello" "" :start1 5)
  nil)

(deftest string-not-equal-end1.1
  (string-not-equal "Hello" "" :end1 0)
  nil)

(deftest string-not-equal-end1.2
  (string-not-equal "Hello" "hello" :end1 1)
  1)

(deftest string-not-equal-end1.3
  (string-not-equal "Hello" "H" :end1 1)
  nil)

(deftest string-not-equal-end1.4
  (string-not-equal "Hello" "hello" :end1 5)
  nil)

(deftest string-not-equal-end1.5
  (string-not-equal "Hello" "hello" :end1 nil)
  nil)

(deftest string-not-equal-start2.1
  (string-not-equal "Hello" "hello" :start2 0)
  nil)

(deftest string-not-equal-start2.2
  (string-not-equal "Hello" "hello" :start2 1)
  0)

(deftest string-not-equal-start2.3
  (string-not-equal "Hello" "ahello" :start2 1)
  nil)

(deftest string-not-equal-start2.4
  (string-not-equal "Hello" "ahelzo" :start2 1)
  3)

(deftest string-not-equal-start2.5
  (string-not-equal "" "hello" :start2 5)
  nil)

(deftest string-not-equal-end2.1
  (string-not-equal "" "hello" :end2 0)
  nil)

(deftest string-not-equal-end2.2
  (string-not-equal "Hello" "hello" :end2 1)
  1)

(deftest string-not-equal-end2.3
  (string-not-equal "Hell" "hello" :end2 4)
  nil)

(deftest string-not-equal-end2.4
  (string-not-equal "Hell" "Hezlo" :end2 4)
  2)

(deftest string-not-equal-end2.5
  (string-not-equal "Hello" "hello" :end2 5)
  nil)

(deftest string-not-equal-end2.6
  (string-not-equal "Hello" "hello" :end2 nil)
  nil)

(deftest string-not-equal-start-end.1
  (string-not-equal "Hello" "ABCDhello" :start1 2 :end1 4 :start2 6 :end2 8)
  nil)

(deftest string-not-equal-start-end.2
  (string-not-equal "Hello" "ABCDHezlo" :start1 2 :end1 4 :start2 6 :end2 8)
  2)

(deftest string-not-equal-start-end.3
  (string-not-equal "Hello" "ABCDhelzo" :start1 2 :end1 4 :start2 6 :end2 8)
  3)

(deftest string-not-equal-start-end.4
  (string-not-equal "ABCDHello" "hello" :start2 2 :end2 4 :start1 6 :end1 8)
  nil)

(deftest string-not-equal-start-end.5
  (string-not-equal "ABCDHezlo" "hello" :start2 2 :end2 4 :start1 6 :end1 8)
  6)

(deftest string-not-equal-start-end.6
  (string-not-equal "ABCDHelzo" "hello" :start2 2 :end2 4 :start1 6 :end1 8)
  7)

(deftest-error string-not-equal-error.1
  (eval '(string-not-equal 10 "Hello"))
  type-error)

(deftest-error string-not-equal-error.2
  (eval '(string-not-equal "Hello" 20))
  type-error)

(deftest-error! string-not-equal-error.3
  (eval '(string-not-equal "Hello")))

(deftest-error! string-not-equal-error.4
  (eval '(string-not-equal "Hello")))

(deftest-error string-not-equal-error.5
  (eval '(string-not-equal "Hello" "abc" :start1)))

(deftest-error string-not-equal-error.6
  (eval '(string-not-equal "Hello" "abc" :start1 "Hello")))

(deftest-error string-not-equal-error.7
  (eval '(string-not-equal "Hello" "abc" :start1 6)))

(deftest-error string-not-equal-error.8
  (eval '(string-not-equal "Hello" "abc" :end1 6)))

(deftest-error string-not-equal-error.9
  (eval '(string-not-equal "Hello" "abc" :start2 4)))

(deftest-error string-not-equal-error.10
  (eval '(string-not-equal "Hello" "abc" :end2 4)))

(deftest-error string-not-equal-error.11
  (eval '(string-not-equal "Hello" "abc" :start1 3 :end1 2)))

(deftest-error string-not-equal-error.12
  (eval '(string-not-equal "Hello" "abc" :start2 2 :end2 1)))


;;
;;  Function STRING-LESSP
;;
(deftest string-lessp.1
  (string-lessp "" "")
  nil)

(deftest string-lessp.2
  (string-lessp "Hello" "")
  nil)

(deftest string-lessp.3
  (string-lessp "" "Hello")
  0)

(deftest string-lessp.4
  (string-lessp "aBcdef" "abceef")
  3)

(deftest string-lessp.5
  (string-lessp "aBcdef" "abcdef")
  nil)

(deftest string-lessp.6
  (string-lessp "aBcdef" "abccef")
  nil)

(deftest string-lessp.7
  (string-lessp "aaa" "aaaa")
  3)

(deftest string-lessp.8
  (string-lessp "aaa" "aaa")
  nil)

(deftest string-lessp.9
  (string-lessp "aaa" "aa")
  nil)

(deftest string-lessp.10
  (string-lessp 'a #\A)
  nil)

(deftest string-lessp.11
  (string-lessp 'a #\a)
  nil)

(deftest string-lessp.12
  (string-lessp 'a #\B)
  0)

(deftest string-lessp.13
  (string-lessp "aaaHellobbb" "ccHEllodd" :start1 3 :end1 8 :start2 2 :end2 7)
  nil)

(deftest string-lessp.14
  (string-lessp "aaaHealobbb" "ccHellodd" :start1 3 :end1 8 :start2 2 :end2 7)
  5)

(deftest string-lessp.15
  (string-lessp "aaaa" "aaab")
  3)

(deftest string-lessp.16
  (string-lessp "aaab" "aaaa")
  nil)

(deftest string-lessp-start1.1
  (string-lessp "zHello" "Hello" :start1 0)
  nil)

(deftest string-lessp-start1.2
  (string-lessp "AHello" "Hello" :start1 0)
  0)

(deftest string-lessp-start1.3
  (string-lessp "aHello" "Hello" :start1 1)
  nil)

(deftest string-lessp-start1.4
  (string-lessp "aHelAo" "Hello" :start1 1)
  4)

(deftest string-lessp-start1.5
  (string-lessp "Hello" "" :start1 5)
  nil)

(deftest string-lessp-start1.6
  (string-lessp "Hello" "Z" :start1 5)
  5)

(deftest string-lessp-end1.1
  (string-lessp "Hello" "" :end1 0)
  nil)

(deftest string-lessp-end1.2
  (string-lessp "Hello" "Z" :end1 0)
  0)

(deftest string-lessp-end1.3
  (string-lessp "Hello" "Hello" :end1 1)
  1)

(deftest string-lessp-end1.4
  (string-lessp "Hello" "H" :end1 1)
  nil)

(deftest string-lessp-end1.5
  (string-lessp "Hello" "Hello" :end1 5)
  nil)

(deftest string-lessp-end1.6
  (string-lessp "Hello" "Hellz" :end1 nil)
  4)

(deftest string-lessp-start2.1
  (string-lessp "Hello" "Hello" :start2 0)
  nil)

(deftest string-lessp-start2.2
  (string-lessp "Hello" "Hello" :start2 1)
  nil)

(deftest string-lessp-start2.3
  (string-lessp "Hello" "aHello" :start2 1)
  nil)

(deftest string-lessp-start2.4
  (string-lessp "Hello" "aHelzo" :start2 1)
  3)

(deftest string-lessp-start2.5
  (string-lessp "" "HelLo" :start2 5)
  nil)

(deftest string-lessp-end2.1
  (string-lessp "" "HelLo" :end2 0)
  nil)

(deftest string-lessp-end2.2
  (string-lessp "Hello" "Hello" :end2 1)
  nil)

(deftest string-lessp-end2.3
  (string-lessp "Hello" "Zello" :end2 1)
  0)

(deftest string-lessp-end2.4
  (string-lessp "Hell" "Hello" :end2 4)
  nil)

(deftest string-lessp-end2.5
  (string-lessp "Hell" "Hezlo" :end2 4)
  2)

(deftest string-lessp-end2.6
  (string-lessp "Hello" "Hello" :end2 5)
  nil)

(deftest string-lessp-end2.7
  (string-lessp "Hello" "Hellz" :end2 nil)
  4)

(deftest string-lessp-start-end.1
  (string-lessp "Hello" "ABCDHello" :start1 2 :end1 4 :start2 6 :end2 8)
  nil)

(deftest string-lessp-start-end.2
  (string-lessp "Hello" "ABCDHezlo" :start1 2 :end1 4 :start2 6 :end2 8)
  2)

(deftest string-lessp-start-end.3
  (string-lessp "Hello" "ABCDHelzo" :start1 2 :end1 4 :start2 6 :end2 8)
  3)

(deftest string-lessp-start-end.4
  (string-lessp "ABCDHello" "Hello" :start2 2 :end2 4 :start1 6 :end1 8)
  nil)

(deftest string-lessp-start-end.5
  (string-lessp "ABCDHeAlo" "Hello" :start2 2 :end2 4 :start1 6 :end1 8)
  6)

(deftest string-lessp-start-end.6
  (string-lessp "ABCDHelAo" "Hello" :start2 2 :end2 4 :start1 6 :end1 8)
  7)

(deftest-error string-lessp-error.1
  (eval '(string-lessp 10 "Hello"))
  type-error)

(deftest-error string-lessp-error.2
  (eval '(string-lessp "Hello" 20))
  type-error)

(deftest-error! string-lessp-error.3
  (eval '(string-lessp "Hello")))

(deftest-error! string-lessp-error.4
  (eval '(string-lessp "Hello")))

(deftest-error string-lessp-error.5
  (eval '(string-lessp "Hello" "abc" :start1)))

(deftest-error string-lessp-error.6
  (eval '(string-lessp "Hello" "abc" :start1 "Hello")))

(deftest-error string-lessp-error.7
  (eval '(string-lessp "Hello" "abc" :start1 6)))

(deftest-error string-lessp-error.8
  (eval '(string-lessp "Hello" "abc" :end1 6)))

(deftest-error string-lessp-error.9
  (eval '(string-lessp "Hello" "abc" :start2 4)))

(deftest-error string-lessp-error.10
  (eval '(string-lessp "Hello" "abc" :end2 4)))

(deftest-error string-lessp-error.11
  (eval '(string-lessp "Hello" "abc" :start1 3 :end1 2)))

(deftest-error string-lessp-error.12
  (eval '(string-lessp "Hello" "abc" :start2 2 :end2 1)))


;;
;;  Function STRING-GREATERP
;;
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


;;
;;  Function STRING-NOT-GREATERP
;;
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


;;
;;  Function STRING-NOT-LESSP
;;
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

(deftest string-not-lessp.7
  (string-not-lessp "aaaaa" "aaaa")
  4)


;;  ANSI Common Lisp
(deftest string-compare.1
  (string= "foo" "foo")
  t)

(deftest string-compare.2
  (string= "foo" "Foo")
  nil)

(deftest string-compare.3
  (string= "foo" "bar")
  nil)

(deftest string-compare.4
  (string= "together" "frog" :start1 1 :end1 3 :start2 2)
  t)

(deftest string-compare.5
  (string-equal "foo" "Foo")
  t)

(deftest string-compare.6
  (string= "abcd" "01234abcd9012" :start2 5 :end2 9)
  t)

(deftest string-compare.7
  (string< "aaaa" "aaab")
  3)

(deftest string-compare.8
  (string>= "aaaaa" "aaaa")
  4)

(deftest string-compare.9
  (string-not-greaterp "Abcde" "abcdE")
  5)

(deftest string-compare.10
  (string-lessp "012AAAA789" "01aaab6" :start1 3 :end1 7 :start2 2 :end2 6)
  6)

(deftest string-compare.11
  (string-not-equal "AAAA" "aaaA")
  nil)

