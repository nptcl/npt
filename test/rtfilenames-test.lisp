;;
;;  ANSI COMMON LISP: 19. Filenames
;;

;;
;;  Function PATHNAME
;;
(deftest pathname.1
  (pathname "aaa.txt")
  #p"aaa.txt")

(deftest pathname.2
  (pathname
    (pathname "aaa.txt"))
  #p"aaa.txt")

(deftest pathname.3
  (let ((x (open #p"_pathname-test.txt" :direction :output)))
    (prog1 (pathname x)
      (close x :abort t)))
  #p"_pathname-test.txt")

(deftest pathname.4
  (let ((x (open #p"_pathname-test.txt" :direction :output)))
    (close x :abort t)
    (pathname x))
  #p"_pathname-test.txt")

(deftest pathname.5
  (let ((x #p"hello.txt"))
    (eq x (pathname x)))
  t)

(deftest pathname.6
  (let ((x (logical-pathname "test:aaa;bbb;Hello.txt")))
    (eq x (pathname x)))
  t)

(deftest-error pathname-error.1
  (with-open-stream (x (make-string-output-stream))
    (pathname x))
  type-error)

(deftest-error pathname-error.2
  (eval '(pathname 10))
  type-error)

(deftest-error! pathname-error.3
  (eval '(pathname)))

(deftest-error! pathname-error.4
  (eval '(pathname "hello" nil)))


;;
;;  Function MAKE-PATHNAME
;;
(deftest make-pathname.1
  (make-pathname)
  #p"")

(deftest make-pathname.2
  (let ((x (make-pathname :host "hello")))
    (pathname-host x))
  "hello")

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


(setf (logical-pathname-translations "logical-name")
      '(("*.*.*" "/var/") ("path;to;*.*" "/usr/local/")))
(deftest make-pathname.7
  (equal
    (make-pathname
      :host "logical-name" :directory '(:relative "aaa" "bbb")
      :name "Hello" :type "txt")
    (parse-namestring
      "logical-name:;aaa;bbb;Hello.txt"))
  t)

(deftest make-pathname.8
  (equal
    (make-pathname :host "logical-name" :directory '(:absolute "aaa" "bbb")
                   :name "Hello" :type "txt")
    (parse-namestring
      "logical-name:aaa;bbb;Hello.txt"))
  t)

(deftest make-pathname.9
  (equal
    (make-pathname :directory '(:relative)
                   :name "other-name:aaa;bbb;Hello" :type "txt")
    #p"other-name:aaa;bbb;Hello.txt")
  t)



;;
;;
;;
(deftest pathname-host.1
  (pathname-host
    (parse-namestring "logical-name:;aaa.txt"))
  "logical-name")

(deftest pathname-host.2
  (pathname-host
    (parse-namestring "logical-name:;aaa.txt") :case :local)
  "logical-name")

(setf (logical-pathname-translations "logICAL-name")
      '(("*.*.*" "/usr/") ("path;to;*.*" "/opt/local/")))
(deftest pathname-host.3
  (pathname-host
    (parse-namestring "logICAL-name:;aaa.txt") :case :common)
  "logICAL-name")

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
    (parse-namestring "logical-name:;aaa;bbb;name.txt.100"))
  100)

(deftest pathname-version.2
  (pathname-version
    (parse-namestring "logical-name:;aaa;bbb;name.txt.newest"))
  :newest)

(deftest logical-pathname-translations.1
  (logical-pathname-translations "TEST-PATH2")
  nil)

(setf (logical-pathname-translations "TEST-PATH3")
      '(("path;*.*.*" "/usr/local/path/")))
(deftest logical-pathname-translations.2
  (equal
    (logical-pathname-translations "TEST-PATH3")
    (list (list (parse-namestring "test-path3:path;*.*.*")
                (parse-namestring "/usr/local/path/"))))
  t)

(setf (logical-pathname-translations "TEST-PATH4")
      '(("path;*.*.*" "/usr/local/path/")
        ("aaa;bbb;*.lisp" "/usr/local/lisp/*.l")))
(deftest logical-pathname-translations.3
  (equal
    (logical-pathname-translations "TEST-PATH4")
    (list (list (parse-namestring "test-path4:path;*.*.*")
                (parse-namestring "/usr/local/path/"))
          (list (parse-namestring "test-path4:aaa;bbb;*.lisp")
                (parse-namestring "/usr/local/lisp/*.l"))))
  t)

(deftest logical-pathname.1
  (equal
    (logical-pathname
      (parse-namestring "logical-name:*.*.*"))
    (parse-namestring "logical-name:*.*.*"))
  t)

(deftest logical-pathname.2
  (equal
    (logical-pathname "logical-name:*.*.*")
    (parse-namestring "logical-name:*.*.*"))
  t)

(deftest namestring.1
  (namestring #p"/usr/local/bin/aaa.txt")
  #-windows "/usr/local/bin/aaa.txt"
  #+windows "\\usr\\local\\bin\\aaa.txt")

(deftest namestring.2
  (namestring
    (parse-namestring "logical-name:aaa;bbb;"))
  "logical-name:aaa;bbb;")

(deftest namestring.3
  (namestring "logical-name:aaa;bbb;")
  "logical-name:aaa;bbb;")

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

(deftest host-namestring.1
  (host-namestring #p"/usr/local/aaa/hello.txt")
  "")

(deftest host-namestring.2
  (host-namestring
    (parse-namestring "logical-name:;aaa;bbb;hello.txt"))
  "logical-name")

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
  "logical-name:usr;local;bin;")

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

(deftest parse-namestring.1
  (parse-namestring "/usr/local/bin/")
  #p"/usr/local/bin/" 15)

(deftest parse-namestring.2
  (multiple-value-bind (check index)
    (parse-namestring "logical-name:;usr;local;bin;")
    (and (equal (pathname-host check) "logical-name")
         (eql index 28)))
  t)

(deftest parse-namestring.3
  (parse-namestring "/usr/local/bin/" nil *default-pathname-defaults*)
  #p"/usr/local/bin/" 15)

(deftest parse-namestring.4
  (multiple-value-bind (check index)
    (parse-namestring "logical-name:;usr;local;bin;" nil *default-pathname-defaults*)
    (and (equal check (parse-namestring "logical-name:;usr;local;bin;"))
         (eql index 28)))
  t)

(deftest parse-namestring.5
  (multiple-value-bind (check index)
    (parse-namestring
      "logical-name:;usr;local;bin;" "logical-name" *default-pathname-defaults*)
    (and (equal check (parse-namestring "logical-name:;usr;local;bin;"))
         (eql index 28)))
  t)

(deftest-error parse-namestring.6
  (parse-namestring
    "logical-name:;usr;local;bin;" "error" *default-pathname-defaults*))

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
    (setf (logical-pathname-translations "logical-name")
          '(("*.*.*" "/var/")
            ("path;to;*.*" "/usr/local/")))
    (equal
      (translate-logical-pathname
        (parse-namestring "logical-name:hello.txt"))
      (parse-namestring "/var/hello.txt")))
  t)

(deftest translate-logical-pathname.3
  (progn
    (setf (logical-pathname-translations "logical-name")
          '(("*.*.*" "/var/")
            ("path;to;*.*" "/usr/local/")))
    (equal
      (translate-logical-pathname
        (parse-namestring "logical-name:path;to;hello.txt"))
      (parse-namestring "/usr/local/hello.txt")))
  t)

(deftest merge-pathnames.1
  (merge-pathnames #p"bin/aaa.bin" #p"/usr/local/")
  #p"/usr/local/bin/aaa.bin")

(deftest load-logical-pathname-translations.1
  (load-logical-pathname-translations "no-such-logical-pathname")
  nil)

(deftest load-logical-pathname-translations.2
  (let ((lisp-system::*load-logical-pathname-translations*
          (make-pathname :type "lisp" :defaults #p"test/")))
    (load-logical-pathname-translations "rtfilenames-logical"))
  t)

(deftest load-logical-pathname-translations.3
  (let ((lisp-system::*load-logical-pathname-translations*
          (make-pathname :type "lisp" :defaults #p"test/")))
    (load-logical-pathname-translations "rtfilenames-logical")
    (values
      (equal
        (translate-logical-pathname
          (parse-namestring "rtfilenames-logical:path;to;hello.txt"))
        (parse-namestring "/ccc/ddd/eee/hello.txt"))
      (equal
        (translate-logical-pathname
          (parse-namestring "rtfilenames-logical:zzz.txt"))
        (parse-namestring "/aaa/bbb/zzz.txt"))))
  t t)


;;
;;  error
;;
(deftest pathname-host-string.1
  (pathname-host "logical-name:;aaa.txt")
  "logical-name")

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

