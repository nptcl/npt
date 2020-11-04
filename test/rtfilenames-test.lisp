;;
;;  ANSI COMMON LISP: 19. Filenames
;;

;;
;;  Function PARSE-NAMESTRING
;;
(deftest parse-namestring.1
  (parse-namestring "/usr/local/bin/")
  #p"/usr/local/bin/" 15)

(deftest parse-namestring.2
  (multiple-value-bind (check index)
    (parse-namestring "test:;usr;local;bin;")
    (and (equalp (pathname-host check) "test")
         (eql index 20)))
  t)

(deftest parse-namestring.3
  (parse-namestring "/usr/local/bin/" nil)
  #p"/usr/local/bin/" 15)

(deftest parse-namestring.4
  (parse-namestring "/usr/local/bin/" nil *default-pathname-defaults*)
  #p"/usr/local/bin/" 15)

(deftest parse-namestring.5
  (multiple-value-bind (check index)
    (parse-namestring "test:;usr;local;bin;" nil *default-pathname-defaults*)
    (and (equal check (parse-namestring "test:;usr;local;bin;"))
         (eql index 20)))
  t)

(deftest parse-namestring.6
  (multiple-value-bind (check index)
    (parse-namestring
      "test:;usr;local;bin;" "test" *default-pathname-defaults*)
    (and (equal check (parse-namestring "test:;usr;local;bin;"))
         (eql index 20)))
  t)

(deftest parse-namestring.7
  (parse-namestring #p"Hello.txt")
  #p"Hello.txt" 0)

(deftest parse-namestring.8
  (parse-namestring #p"Hello.txt" nil *default-pathname-defaults*)
  #p"Hello.txt" 0)

(deftest parse-namestring.9
  (parse-namestring #p"Hello.txt" nil *default-pathname-defaults* :start 2 :end 3)
  #p"Hello.txt" 2)

(deftest-error parse-namestring.10
  (parse-namestring #p"Hello.txt" "test"))

(deftest parse-namestring.11
  (let ((x (open #p"_parse_namestring.txt" :direction :output
                 :if-exists :supersede :if-does-not-exist :create)))
    (multiple-value-prog1 (parse-namestring x)
      (close x :abort t)))
  #p"_parse_namestring.txt" 0)

(deftest-error parse-namestring.12
  (let ((x (open #p"_parse_namestring.txt" :direction :output
                 :if-exists :supersede :if-does-not-exist :create)))
    (unwind-protect
      (parse-namestring x "test")
      (close x :abort t))))

(deftest-error parse-namestring-logical.1
  (parse-namestring
    "test:;usr;local;bin;" "name" *default-pathname-defaults*))

(deftest parse-namestring-logical.2
  (parse-namestring
    "test:;usr;local;bin;" "test" *default-pathname-defaults*)
  #p"test:;usr;local;bin;" 20)

(deftest parse-namestring-logical.3
  (parse-namestring
    ";usr;local;bin;" "test" *default-pathname-defaults*)
  #p"test:;usr;local;bin;" 15)

(deftest parse-namestring-logical.4
  (parse-namestring ";usr;local;bin;" nil #p"test:")
  #p"test:;usr;local;bin;" 15)

(deftest-error parse-namestring-start.1
  (parse-namestring "Hello.txt" nil *default-pathname-defaults* :start -1))

(deftest parse-namestring-start.2
  (parse-namestring "Hello.txt" nil *default-pathname-defaults* :start 0)
  #p"Hello.txt" 9)

(deftest parse-namestring-start.3
  (parse-namestring "Hello.txt" nil *default-pathname-defaults* :start 1)
  #p"ello.txt" 9)

(deftest parse-namestring-start.4
  (parse-namestring "Hello.txt" nil *default-pathname-defaults* :start 8)
  #p"t" 9)

(deftest parse-namestring-start.5
  (parse-namestring "Hello.txt" nil *default-pathname-defaults* :start 9)
  #p"" 9)

(deftest-error parse-namestring-start.6
  (parse-namestring "Hello.txt" nil *default-pathname-defaults*
                    :start 10))

(deftest-error parse-namestring-end.1
  (parse-namestring "Hello.txt" nil *default-pathname-defaults* :end -1))

(deftest parse-namestring-end.2
  (parse-namestring "Hello.txt" nil *default-pathname-defaults* :end 0)
  #p"" 0)

(deftest parse-namestring-end.3
  (parse-namestring "Hello.txt" nil *default-pathname-defaults* :end 1)
  #p"H" 1)

(deftest parse-namestring-end.4
  (parse-namestring "Hello.txt" nil *default-pathname-defaults* :end 8)
  #p"Hello.tx" 8)

(deftest parse-namestring-end.5
  (parse-namestring "Hello.txt" nil *default-pathname-defaults* :end 9)
  #p"Hello.txt" 9)

(deftest parse-namestring-end.6
  (parse-namestring "Hello.txt" nil *default-pathname-defaults* :end nil)
  #p"Hello.txt" 9)

(deftest-error parse-namestring-end.7
  (parse-namestring "Hello.txt" nil *default-pathname-defaults* :end 10))

(deftest parse-namestring-start-end.1
  (parse-namestring "aa/usr/local/bin/hello.txt"
                    nil *default-pathname-defaults*
                    :start 2 :end 17)
  #p"/usr/local/bin/" 17)

(deftest parse-namestring-start-end.2
  (parse-namestring "aa/usr/local/bin/hello.txt"
                    nil *default-pathname-defaults*
                    :start 4 :end 4)
  #p"" 4)

(deftest-error parse-namestring-start-end.3
  (parse-namestring "aa/usr/local/bin/hello.txt"
                    nil *default-pathname-defaults*
                    :start 4 :end 3))

(deftest parse-namestring-junk-allowed.1
  (parse-namestring "Hello.txt"
                    nil *default-pathname-defaults*
                    :junk-allowed t)
  #p"Hello.txt" 9)

(deftest parse-namestring-junk-allowed.2
  (parse-namestring "hello&&file.txt" "test"
                    *default-pathname-defaults* :junk-allowed t)
  nil 5)

(deftest-error parse-namestring-junk-allowed.3
  (parse-namestring "hello&&file.txt" "test" *default-pathname-defaults*)
  parse-error)

(deftest-error parse-namestring-error.1
  (parse-namestring "Hello.txt" "no-such-logical-name")
  type-error)

(deftest-error parse-namestring-error.2
  (eval '(parse-namestring 10))
  type-error)

(deftest-error parse-namestring-error.3
  (eval '(parse-namestring "Hello.txt" 20))
  type-error)

(deftest-error parse-namestring-error.4
  (eval '(parse-namestring "Hello.txt" "test" 30))
  type-error)

(deftest-error parse-namestring-error.5
  (eval '(parse-namestring "Hello.txt" "test" :start)))

(deftest-error parse-namestring-error.6
  (eval '(parse-namestring "Hello.txt" "test" :start :hello)))

(deftest-error parse-namestring-error.7
  (eval '(parse-namestring "Hello.txt" "test" :hello)))

(deftest-error parse-namestring-error.8
  (eval '(parse-namestring "Hello.txt" "test" :hello 40)))

(deftest-error! parse-namestring-error.9
  (eval '(parse-namestring)))

(deftest-error parse-namestring-error.10
  (with-open-stream (file (lisp-system:make-memory-output-stream))
    (with-open-file (stream file :direction :output)
      (parse-namestring stream))))


;;
;;
;;
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


;;
;;
;;
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



;;
;;
;;
(deftest merge-pathnames.1
  (merge-pathnames #p"bin/aaa.bin" #p"/usr/local/")
  #p"/usr/local/bin/aaa.bin")



;;
;;
;;
(deftest namestring.1
  (namestring #p"/usr/local/bin/aaa.txt")
  #-windows "/usr/local/bin/aaa.txt"
  #+windows "\\usr\\local\\bin\\aaa.txt")

(deftest namestring.2
  (namestring
    (parse-namestring "logical-name:aaa;bbb;"))
  "LOGICAL-NAME:aaa;bbb;")

(deftest namestring.3
  (namestring "logical-name:aaa;bbb;")
  "LOGICAL-NAME:aaa;bbb;")


;;
;;
;;
(deftest file-namestring.1
  (file-namestring #p"/usr/local/aaa/hello.txt")
  "hello.txt")

(deftest file-namestring.2
  (file-namestring
    (parse-namestring "logical-name:;aaa;bbb;hello.txt"))
  "hello.txt")

(deftest file-namestring.3
  (file-namestring
    (parse-namestring "logical-name:;aaa;bbb;hello.txt.100"))
  "hello.txt.100")


;;
;;
;;
(deftest directory-namestring.1
  (directory-namestring #p"/usr/local/aaa/hello.txt")
  #-windows "/usr/local/aaa/"
  #+windows "\\usr\\local\\aaa\\")

(deftest directory-namestring.2
  (directory-namestring #p"usr/local/aaa/hello.txt")
  #-windows "usr/local/aaa/"
  #+windows "usr\\local\\aaa\\")

(deftest directory-namestring.3
  (directory-namestring
    (parse-namestring "logical-name:;aaa;bbb;hello.txt"))
  ";aaa;bbb;")


;;
;;
;;
(deftest host-namestring.1
  (host-namestring #p"/usr/local/aaa/hello.txt")
  "")

(deftest host-namestring.2
  (host-namestring
    (parse-namestring "logical-name:;aaa;bbb;hello.txt"))
  "LOGICAL-NAME")


;;
;;
;;
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
  (enough-namestring
    (parse-namestring "logical-name:usr;local;bin;")
    #p"/usr/local/")
  "LOGICAL-NAME:usr;local;bin;")

(deftest enough-namestring.7
  (enough-namestring
    #p"/usr/local/bin/"
    (parse-namestring "logical-name:usr;local;"))
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


;;
;;  error
;;
(deftest pathname-host-string.1
  (pathname-host "logical-name:;aaa.txt")
  "LOGICAL-NAME")

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
  (pathname-version "logical-name:;aaa;bbb;name.txt.100")
  100)

(deftest issues-sharp-8.1
  (make-pathname :type nil)
  #p"")

(deftest issues-sharp-8.2
  (make-pathname :type (car nil))
  #p"")


;;
;;  home directory
;;
#+unix
(deftest home-directory.1
  (car
    (pathname-directory
      (parse-namestring "~/hello.txt")))
  :absolute)

#+unix
(deftest home-directory.2
  (equalp
    (elt (pathname-directory
           (parse-namestring "~/aaa/bbb/hello.txt"))
         1)
    "~")
  nil)

#+unix
(deftest home-directory.3
  (let ((lisp-system::*environment* nil))
    (car (pathname-directory
           (parse-namestring "~/hello.txt"))))
  :absolute)

#+unix
(deftest home-directory.4
  (let ((lisp-system::*environment* nil))
    (car (pathname-directory
           (parse-namestring "~root/hello.txt"))))
  :absolute)

