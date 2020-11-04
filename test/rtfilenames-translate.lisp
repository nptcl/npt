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

