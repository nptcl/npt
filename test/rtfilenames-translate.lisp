;;
;;  ANSI COMMON LISP: 19. Filenames
;;
(deftest translate-pathname.1
  (translate-pathname #p"hello.txt" #p"hello.txt" #p"hello.txt")
  #p"hello.txt")

(deftest translate-pathname.2
  (translate-pathname #p"hello.txt" #p"*.txt" #p"*.txt")
  #p"hello.txt")

(deftest translate-pathname.3
  (translate-pathname #p"hello.txt" #p"h*o.txt" #p"*.txt")
  #p"ell.txt")

(deftest translate-pathname.4
  (translate-pathname #p"hello.txt" #p"h*o.txt" #p"a?c.txt")
  #p"aellc.txt")

(deftest translate-pathname.5
  (translate-pathname #p"hello.txt" #p"h*o.txt" #p"a*c*d.txt")
  #p"aellcd.txt")

(deftest-error translate-pathname.6
  (translate-pathname #p"hello.txt" #p"h*o.txt" #p"acd.txt"))

(deftest translate-pathname.7
  (translate-pathname #p"abcdefg.txt" #p"a*cd*g.txt" #p"**.txt")
  #p"bef.txt")

(deftest translate-pathname.8
  (translate-pathname #p"abcdefg.txt" #p"a*cd*g.txt" #p"*Z*.txt")
  #p"bZef.txt")

(deftest translate-pathname.9
  (translate-pathname
    #p"/usr/local/hello.txt"
    #p"/usr/local/hello.txt"
    #p"/usr/local/hello.txt")
  #p"/usr/local/hello.txt")

(deftest translate-pathname.10
  (translate-pathname
    #p"/usr/local/hello.txt"
    #p"/usr/*/hello.txt"
    #p"/usr/*/hello.txt")
  #p"/usr/local/hello.txt")

(deftest translate-pathname.11
  (translate-pathname
    #p"/usr/local/hello.txt"
    #p"/usr/l*/hello.txt"
    #p"/usr/*/hello.txt")
  #p"/usr/ocal/hello.txt")

(deftest translate-pathname.12
  (translate-pathname
    #p"/usr/local/hello.txt"
    #p"/usr/*/hello.txt"
    #p"/usr/l*/hello.txt")
  #p"/usr/llocal/hello.txt")

(deftest translate-pathname.13
  (translate-pathname
    #p"/usr/local/hello.txt"
    #p"/usr/l*/hello.txt"
    #p"/usr/z*/hello.txt")
  #p"/usr/zocal/hello.txt")

(deftest translate-pathname.14
  (translate-pathname
    #p"/usr/local/hello.txt"
    #p"/**/hello.txt"
    #p"/usr/*/hello.txt")
  #p"/usr/usr/local/hello.txt")

(deftest-error translate-pathname-error.1
  (eval '(translate-pathname 10 #p"a*" #p"b*"))
  type-error)

(deftest-error translate-pathname-error.2
  (eval '(translate-pathname #p"abc" 20 #p"b*"))
  type-error)

(deftest-error translate-pathname-error.3
  (eval '(translate-pathname #p"abc" #p"a*" 30))
  type-error)

(deftest-error! translate-pathname-error.4
  (eval '(translate-pathname #p"abc" #p"a*")))

(deftest-error! translate-pathname-error.5
  (eval '(translate-pathname #p"abc" #p"a*" #p"b*" nil)))

(deftest-error translate-pathname-error.6
  (eval '(translate-pathname #p"abc" #p"ac" #p"b*")))

;;  ANSI Common Lisp
(deftest translate-pathname-test.1
  (pathname-name (translate-pathname "foobar" "foo*" "*baz"))
  "barbaz")

(deftest translate-pathname-test.2
  (pathname-name (translate-pathname "foobar" "foo*" "*"))
  "bar")

(deftest translate-pathname-test.3
  (pathname-name (translate-pathname "foobar" "*"    "foo*"))
  "foofoobar")

(deftest translate-pathname-test.4
  (pathname-name (translate-pathname "bar"    "*"    "foo*"))
  "foobar")

(deftest translate-pathname-test.5
  (pathname-name (translate-pathname "foobar" "foo*" "baz*"))
  "bazbar")

