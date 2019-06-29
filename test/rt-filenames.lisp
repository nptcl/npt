;
;;  ANSI COMMON LISP: 19. Filenames
;;
(deftest pathname.1
  (pathname "aaa.txt")
  #p"aaa.txt")

(deftest pathname.2
  (pathname
    (pathname "aaa.txt"))
  #p"aaa.txt")

(deftest make-pathname.1
  (make-pathname)
  #p"")

(deftest make-pathname.2
  (make-pathname :host "hello")
  #p"hello:;")

(deftest make-pathname.3
  (make-pathname :directory '(:relative "aaa"))
  #p"aaa/")

(deftest make-pathname.4
  (make-pathname :directory '(:absolute "aaa" "bbb"))
  #p"/aaa/bbb/")

(deftest make-pathname.5
  (make-pathname :name "Hello")
  #p"Hello")

(deftest make-pathname.6
  (make-pathname :name "Hello" :type "txt")
  #p"Hello.txt")

(deftest make-pathname.7
  (make-pathname :host "logical" :directory '(:relative "aaa" "bbb")
                 :name "Hello" :type "txt")
  #p"logical:;aaa;bbb;Hello.txt")

(deftest make-pathname.8
  (make-pathname :host "logical" :directory '(:absolute "aaa" "bbb")
                 :name "Hello" :type "txt")
  #p"logical:aaa;bbb;Hello.txt")

(deftest pathnamep.1
  (pathnamep #p"hello.txt")
  t)

(deftest pathnamep.2
  (pathnamep #p"logical:aaa;bbb;Hello.txt")
  t)

(deftest pathnamep.3
  (pathnamep t)
  nil)

(deftest pathname-host.1
  (pathname-host #p"logical:;aaa.txt")
  "logical")

(deftest pathname-host.2
  (pathname-host #p"logical:;aaa.txt" :case :local)
  "logical")

(deftest pathname-host.3
  (pathname-host #p"logICAL:;aaa.txt" :case :common)
  "logICAL")

(deftest pathname-directory.1
  (pathname-directory
    #p"/usr/local/bin/aaa.txt")
  (:absolute "usr" "local" "bin"))

(deftest pathname-name.1
  (pathname-name
    #p"/usr/local/bin/aaa.txt")
  "aaa")

(deftest pathname-type.1
  (pathname-type
    #p"/usr/local/bin/aaa.txt")
  "txt")

(deftest pathname-version.1
  (pathname-version
    #p"logical:;aaa;bbb;name.txt.100")
  100)

(deftest pathname-version.2
  (pathname-version
    #p"logical:;aaa;bbb;name.txt.newest")
  :newest)

(deftest load-logical-pathname-translations.1
  (load-logical-pathname-translations "TEST-PATH1")
  nil)

(deftest logical-pathname-translations.1
  (logical-pathname-translations "TEST-PATH2")
  nil)

(deftest logical-pathname-translations.2
  (progn
    (setf (logical-pathname-translations "TEST-PATH3")
          '(("path;*.*.*" "/usr/local/path/")))
    (logical-pathname-translations "TEST-PATH3"))
  ((#p"test-path3:path;*.*.*" #p"/usr/local/path/")))

(deftest logical-pathname-translations.3
  (progn
    (setf (logical-pathname-translations "TEST-PATH4")
          '(("path;*.*.*" "/usr/local/path/")
            ("aaa;bbb;*.lisp" "/usr/local/lisp/*.l")))
    (logical-pathname-translations "TEST-PATH4"))
  ((#p"test-path4:path;*.*.*" #p"/usr/local/path/")
    (#p"test-path4:aaa;bbb;*.lisp" #p"/usr/local/lisp/*.l"
    )))

(deftest logical-pathname.1
  (logical-pathname #p"logical:*.*.*")
  #p"logical:*.*.*")

(deftest logical-pathname.2
  (logical-pathname "logical:*.*.*")
  #p"logical:*.*.*")

(deftest namestring.1
  (namestring #p"/usr/local/bin/aaa.txt")
  #-windows "/usr/local/bin/aaa.txt"
  #+windows "\\usr\\local\\bin\\aaa.txt")

(deftest namestring.2
  (namestring #p"logical:aaa;bbb;")
  "logical:aaa;bbb;")

(deftest namestring.3
  (namestring "logical:aaa;bbb;")
  "logical:aaa;bbb;")

(deftest file-namestring.1
  (file-namestring #p"/usr/local/aaa/hello.txt")
  "hello.txt")

(deftest file-namestring.2
  (file-namestring #p"logical:;aaa;bbb;hello.txt")
  "hello.txt")

(deftest file-namestring.3
  (file-namestring #p"logical:;aaa;bbb;hello.txt.100")
  "hello.txt.100")

(deftest directory-namestring.1
  (directory-namestring #p"/usr/local/aaa/hello.txt")
  #-windows "/usr/local/aaa/"
  #+windows "\\usr\\local\\aaa\\")

(deftest directory-namestring.2
  (directory-namestring #p"usr/local/aaa/hello.txt")
  #-windows "usr/local/aaa/"
  #+windows "usr\\local\\aaa\\")

(deftest directory-namestring.3
  (directory-namestring #p"logical:;aaa;bbb;hello.txt")
  ";aaa;bbb;")

(deftest host-namestring.1
  (host-namestring #p"/usr/local/aaa/hello.txt")
  "")

(deftest host-namestring.2
  (host-namestring #p"logical:;aaa;bbb;hello.txt")
  "logical")

(deftest enough-namestring.1
  (enough-namestring #p"/usr/local/bin/" #p"/usr/local/")
  #-windows "bin/"
  #+windows "bin\\")

(deftest enough-namestring.2
  (enough-namestring "/usr/local/bin/" "/usr/local/")
  #-windows "bin/"
  #+windows "bin\\")

(deftest enough-namestring.3
  (enough-namestring "usr/local/bin/" "usr/local/")
  #-windows "bin/"
  #+windows "bin\\")

(deftest enough-namestring.4
  (enough-namestring #p"/usr/local/" #p"/usr/local/bin/")
  #-windows "/usr/local/"
  #+windows "\\usr\\local\\")

(deftest enough-namestring.5
  (enough-namestring "/usr/local/" "/usr/local/bin/")
  #-windows "/usr/local/"
  #+windows "\\usr\\local\\")

(deftest enough-namestring.6
  (enough-namestring #p"logical:usr;local;bin;" #p"/usr/local/")
  "logical:usr;local;bin;")

(deftest enough-namestring.7
  (enough-namestring #p"/usr/local/bin/" #p"logical:usr;local;")
  #-windows "/usr/local/bin/"
  #+windows "\\usr\\local\\bin\\")

(deftest enough-namestring.8
  (enough-namestring #p"usr/local/bin/" #p"/usr/local/")
  #-windows "usr/local/bin/"
  #+windows "usr\\local\\bin\\")

(deftest enough-namestring.9
  (enough-namestring #p"/usr/local/bin/" #p"usr/local/")
  #-windows "/usr/local/bin/"
  #+windows "\\usr\\local\\bin\\")

(deftest enough-namestring.10
  (enough-namestring #p"/usr/local/bin/" #p"/usr/aaa/")
  #-windows "/usr/local/bin/"
  #+windows "\\usr\\local\\bin\\")

(deftest enough-namestring.11
  (enough-namestring #p"/" #p"/")
  "")

(deftest enough-namestring.12
  (enough-namestring #p"/" #p"")
  #-windows "/"
  #+windows "\\")

(deftest enough-namestring.13
  (enough-namestring #p"" #p"/")
  "")

(deftest parse-namestring.1
  (parse-namestring "/usr/local/bin/")
  #p"/usr/local/bin/" 15)

(deftest parse-namestring.2
  (parse-namestring "logical:;usr;local;bin;")
  #p"logical:;usr;local;bin;" 23)

(deftest parse-namestring.3
  (parse-namestring "/usr/local/bin/" nil *default-pathname-defaults*)
  #p"/usr/local/bin/" 15)

(deftest parse-namestring.4
  (parse-namestring "logical:;usr;local;bin;" nil *default-pathname-defaults*)
  #p"logical:;usr;local;bin;" 23)

(deftest parse-namestring.5
  (parse-namestring "logical:;usr;local;bin;" "logical" *default-pathname-defaults*)
  #p"logical:;usr;local;bin;" 23)

(deftest-error parse-namestring.6
  (parse-namestring "logical:;usr;local;bin;" "error" *default-pathname-defaults*))

(deftest parse-namestring.7
  (parse-namestring "aa/usr/local/bin/hello.txt"
                    nil *default-pathname-defaults*
                    :start 2 :end 17)
  #p"/usr/local/bin/" 17)

(deftest wild-pathname-p.1
  (wild-pathname-p (make-pathname :name :wild))
  t)

(deftest wild-pathname-p.2
  (wild-pathname-p #p"/usr/*/local/")
  t)

(deftest wild-pathname-p.3
  (wild-pathname-p #p"/usr/**/local/")
  t)

(deftest wild-pathname-p.4
  (wild-pathname-p #p"/usr/aaa*bbb/local/")
  t)

(deftest wild-pathname-p.5
  (wild-pathname-p #p"/usr/local/bin/*.txt")
  t)

(deftest wild-pathname-p.6
  (wild-pathname-p #p"/usr/local/bin/hello.*")
  t)

(deftest wild-pathname-p.7
  (wild-pathname-p #p"/usr/local/bin/he*llo.txt")
  t)

(deftest wild-pathname-p.8
  (wild-pathname-p #p"/usr/local/bin/hello.txt")
  nil)

(deftest pathname-match-p.1
  (pathname-match-p #p"hello.txt" #p"hello.txt")
  t)

(deftest pathname-match-p.2
  (pathname-match-p #p"hello.txt" #p"*.txt")
  t)

(deftest pathname-match-p.3
  (pathname-match-p #p"hello.txt" #p"he*o.txt")
  t)

(deftest pathname-match-p.4
  (pathname-match-p #p"he*o.txt" #p"he*o.txt")
  t)

(deftest pathname-match-p.5
  (pathname-match-p #p"h*o.txt" #p"he*o.txt")
  nil)

(deftest pathname-match-p.6
  (pathname-match-p #p"/usr/local/bin/" #p"/usr/*/bin/")
  t)

(deftest pathname-match-p.7
  (pathname-match-p #p"/usr/local/bin/" #p"/usr/lo*/bin/")
  t)

(deftest pathname-match-p.8
  (pathname-match-p #p"/usr/local/bin/" #p"/usr/**/bin/")
  t)

(deftest pathname-match-p.9
  (pathname-match-p #p"/usr/local/aaa/bbb/ccc/bin/" #p"/usr/**/bin/")
  t)

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

(deftest translate-logical-pathname.1
  (translate-logical-pathname #p"hello.txt")
  #p"hello.txt")

(deftest translate-logical-pathname.2
  (progn
    (setf (logical-pathname-translations "logical")
          '(("*.*.*" "/var/")
            ("path;to;*.*" "/usr/local/")))
    (translate-logical-pathname #p"logical:hello.txt"))
  #p"/var/hello.txt")

(deftest translate-logical-pathname.3
  (progn
    (setf (logical-pathname-translations "logical")
          '(("*.*.*" "/var/")
            ("path;to;*.*" "/usr/local/")))
    (translate-logical-pathname #p"logical:path;to;hello.txt"))
  #p"/usr/local/hello.txt")

(deftest merge-pathnames.1
  (merge-pathnames #p"bin/aaa.bin" #p"/usr/local/")
  #p"/usr/local/bin/aaa.bin")


;;
;;  error
;;
(deftest pathname-host-string.1
  (pathname-host "abcd:;aaa.txt")
  "abcd")

(deftest pathname-directory-string.1
  (pathname-directory "/usr/local/bin/aaa.txt")
  (:absolute "usr" "local" "bin"))

(deftest pathname-name-string.1
  (pathname-name "/usr/local/bin/aaa.txt")
  "aaa")

(deftest pathname-type-string.1
  (pathname-type "/usr/local/bin/aaa.txt")
  "txt")

(deftest pathname-version-string.1
  (pathname-version "abcd:;aaa;bbb;name.txt.100")
  100)


;;
;;  do-tests
;;
(do-tests :test t)

