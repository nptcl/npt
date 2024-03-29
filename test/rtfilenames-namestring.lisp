;;
;;  ANSI COMMON LISP: 19. Filenames
;;

;;
;;  Function NAMESTRING
;;
(deftest namestring.1
  (namestring #p"/usr/local/bin/aaa.txt")
  #-windows "/usr/local/bin/aaa.txt"
  #+windows "\\usr\\local\\bin\\aaa.txt")

(deftest namestring.2
  (namestring
    (logical-pathname "test:aaa;bbb;hello.txt.10"))
  "TEST:aaa;bbb;hello.txt.10")

(deftest namestring.3
  (namestring "test:aaa;bbb;")
  "TEST:aaa;bbb;")

(deftest namestring.4
  (namestring
    (parse-namestring
      "/usr/local/bin/aaa.txt"
      'lisp-system::unix))
  "/usr/local/bin/aaa.txt")

(deftest namestring.5
  (namestring
    (parse-namestring
      "d:\\usr\\local\\bin\\aaa.txt"
      'lisp-system::windows))
  "D:\\usr\\local\\bin\\aaa.txt")

#-ansi-c
(deftest namestring.6
  (let ((x (open "Hello.txt" :direction :output :if-exists :supersede)))
    (prog1 (namestring x)
      (close x :abort t)))
  "Hello.txt")

(deftest namestring.7
  (namestring (make-pathname :directory nil :name "hello" :type "txt"))
  "hello.txt")

(deftest namestring.8
  (namestring (make-pathname :directory '(:relative) :name "hello" :type "txt"))
  "hello.txt")

(deftest namestring-directory.1
  (namestring
    (make-pathname
      :host 'lisp-system::unix :directory nil
      :name "hello" :type "txt"))
  "hello.txt")

(deftest namestring-directory.2
  (namestring
    (make-pathname
      :host 'lisp-system::unix :directory '(:relative)
      :name "hello" :type "txt"))
  "hello.txt")

(deftest namestring-directory.3
  (namestring
    (make-pathname
      :host 'lisp-system::windows :directory nil
      :name "hello" :type "txt"))
  "hello.txt")

(deftest namestring-directory.4
  (namestring
    (make-pathname
      :host 'lisp-system::windows :directory '(:relative)
      :name "hello" :type "txt"))
  "hello.txt")

(deftest namestring-directory.5
  (namestring
    (make-pathname
      :host 'lisp-system::windows :device "C" :directory nil
      :name "hello" :type "txt"))
  "C:hello.txt")

(deftest namestring-directory.6
  (namestring
    (make-pathname
      :host 'lisp-system::windows  :device "C" :directory '(:relative)
      :name "hello" :type "txt"))
  "C:hello.txt")

(deftest namestring-directory.7
  (namestring
    (make-pathname
      :host "test" :directory nil
      :name "hello" :type "txt"))
  "TEST:;hello.txt")

(deftest namestring-directory.8
  (namestring
    (make-pathname
      :host "test" :directory '(:relative)
      :name "hello" :type "txt"))
  "TEST:;hello.txt")

(deftest-error namestring-error.1
  (eval '(namestring 10))
  type-error)

(deftest-error! namestring-error.2
  (eval '(namestring)))

(deftest-error! namestring-error.3
  (eval '(namestring "Hello.txt" nil)))


;;
;;  Function FILE-NAMESTRING
;;
(deftest file-namestring.1
  (file-namestring #p"/usr/local/aaa/hello.txt")
  "hello.txt")

(deftest file-namestring.2
  (file-namestring
    (parse-namestring "test:;aaa;bbb;hello.txt"))
  "hello.txt")

(deftest file-namestring.3
  (file-namestring
    (parse-namestring "test:;aaa;bbb;hello.txt.100"))
  "hello.txt.100")

(deftest file-namestring.4
  (file-namestring
    (parse-namestring "path/to/notepad.exe" 'lisp-system::unix))
  "notepad.exe")

(deftest file-namestring.5
  (file-namestring
    (parse-namestring "path\\to\\notepad.exe" 'lisp-system::windows))
  "notepad.exe")

(deftest file-namestring.6
  (file-namestring #p"hello")
  "hello")

(deftest file-namestring.7
  (file-namestring (make-pathname :type "txt"))
  ".txt")

(deftest file-namestring.8
  (file-namestring (make-pathname :host "test" :version 999))
  ".999")

(deftest file-namestring-wild.1
  (file-namestring (make-pathname :name :wild :type :wild))
  "*.*")

(deftest file-namestring-wild.2
  (file-namestring (make-pathname
                     :host "test" :name :wild :type :wild :version :wild))
  "*.*.*")

(deftest file-namestring-wild.3
  (file-namestring #p"*.txt")
  "*.txt")

(deftest file-namestring-wild.4
  (file-namestring #p"hello.*")
  "hello.*")

(deftest file-namestring-wild.5
  (file-namestring (make-pathname :name "a*c" :type "def*"))
  "a*c.def*")

(deftest-error file-namestring-error.1
  (eval '(file-namestring 10))
  type-error)

(deftest-error! file-namestring-error.2
  (eval '(file-namestring)))

(deftest-error! file-namestring-error.3
  (eval '(file-namestring "Hello.txt" nil)))


;;
;;  Function DIRECTORY-NAMESTRING
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
    (parse-namestring "test:;aaa;bbb;hello.txt"))
  ";aaa;bbb;")

(deftest directory-namestring.4
  (directory-namestring
    (parse-namestring "path/to/hello.txt" 'lisp-system::unix))
  "path/to/")

(deftest directory-namestring.5
  (directory-namestring
    (parse-namestring "path/to/hello.txt" 'lisp-system::windows))
  "path\\to\\")

(deftest directory-namestring-wild.1
  (directory-namestring
    (make-pathname
      :host 'lisp-system::unix
      :directory '(:absolute "aaa" :wild :wild "ccc")))
  "/aaa/*/*/ccc/")

(deftest directory-namestring-wild.2
  (directory-namestring
    (make-pathname
      :host 'lisp-system::unix
      :directory '(:absolute "aaa" :wild-inferiors "ccc")))
  "/aaa/**/ccc/")

(deftest directory-namestring-wild.3
  (directory-namestring
    (make-pathname
      :host 'lisp-system::unix
      :directory '(:absolute "aaa" :up "ccc")))
  "/aaa/../ccc/")

(deftest directory-namestring-wild.4
  (directory-namestring
    (make-pathname
      :host 'lisp-system::unix
      :directory '(:absolute "aaa" "AB*CD" "ccc")))
  "/aaa/AB*CD/ccc/")

(deftest directory-namestring-directory.1
  (directory-namestring
    (make-pathname
      :host 'lisp-system::unix :directory nil
      :name "hello" :type "txt"))
  "")

(deftest directory-namestring-directory.2
  (directory-namestring
    (make-pathname
      :host 'lisp-system::unix :directory '(:relative)
      :name "hello" :type "txt"))
  "")

(deftest directory-namestring-directory.3
  (directory-namestring
    (make-pathname
      :host 'lisp-system::windows :directory nil
      :name "hello" :type "txt"))
  "")

(deftest directory-namestring-directory.4
  (directory-namestring
    (make-pathname
      :host 'lisp-system::windows :directory '(:relative)
      :name "hello" :type "txt"))
  "")

(deftest directory-namestring-directory.5
  (directory-namestring
    (make-pathname
      :host "test" :directory nil
      :name "hello" :type "txt"))
  ";")

(deftest directory-namestring-directory.6
  (directory-namestring
    (make-pathname
      :host "test" :directory '(:relative)
      :name "hello" :type "txt"))
  ";")

(deftest-error directory-namestring-error.1
  (eval '(directory-namestring 10))
  type-error)

(deftest-error! directory-namestring-error.2
  (eval '(directory-namestring)))

(deftest-error! directory-namestring-error.3
  (eval '(directory-namestring "Hello.txt" nil)))


;;
;;  Function HOST-NAMESTRING
;;
(deftest host-namestring.1
  (host-namestring #p"/usr/local/aaa/hello.txt")
  "")

(deftest host-namestring.2
  (host-namestring
    (parse-namestring "test:;aaa;bbb;hello.txt"))
  "TEST")

(deftest host-namestring.3
  (host-namestring
    (parse-namestring "hello.txt" 'lisp-system::unix))
  "")

(deftest host-namestring.4
  (host-namestring
    (parse-namestring "hello.txt" 'lisp-system::windows))
  "")

(deftest-error host-namestring-error.1
  (eval '(host-namestring 10))
  type-error)

(deftest-error! host-namestring-error.2
  (eval '(host-namestring)))

(deftest-error! host-namestring-error.3
  (eval '(host-namestring "Hello.txt" nil)))


;;
;;  Function ENOUGH-NAMESTRING
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
    (parse-namestring "test:usr;local;bin;")
    #p"/usr/local/")
  "TEST:usr;local;bin;")

(deftest enough-namestring.7
  (enough-namestring
    #p"/usr/local/bin/"
    (parse-namestring "test:usr;local;"))
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

(deftest enough-namestring.14
  (enough-namestring #p"" #p"")
  "")

(deftest-error enough-namestring-error.1
  (eval '(enough-namestring 10))
  type-error)

(deftest-error enough-namestring-error.2
  (eval '(enough-namestring "Hello.txt" 20))
  type-error)

(deftest-error! enough-namestring-error.3
  (eval '(enough-namestring)))

(deftest-error! enough-namestring-error.4
  (eval '(enough-namestring "Hello.txt" *default-pathname-defaults* nil)))


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

#-ansi-c
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

(deftest parse-namestring-logical.5
  (pathname-directory
    (parse-namestring "test:;hello.txt"))
  nil)

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

