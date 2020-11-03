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
  "HELLO")

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
  (equal
    (make-pathname
      :host "test" :directory '(:relative "aaa" "bbb")
      :name "Hello" :type "txt")
    (logical-pathname
      "test:;aaa;bbb;Hello.txt"))
  t)

(deftest make-pathname.8
  (equal
    (make-pathname :host "test" :directory '(:absolute "aaa" "bbb")
                   :name "Hello" :type "txt")
    (logical-pathname
      "test:aaa;bbb;Hello.txt"))
  t)

(deftest make-pathname.9
  (equal
    (make-pathname :directory '(:relative)
                   :name "other-name:aaa;bbb;Hello" :type "txt")
    #p"other-name:aaa;bbb;Hello.txt")
  t)

(deftest make-pathname.10
  (make-pathname
    :name "Hello" :type "txt"
    :defaults
    (make-pathname :name "aaa" :type "bbb"
                   :directory '(:absolute "usr")))
  #p"/usr/Hello.txt")

(deftest make-pathname.11
  (make-pathname)
  #p"")

(deftest make-pathname.12
  (make-pathname :directory "path" :name "hello" :type "txt")
  #p"/path/hello.txt")

(deftest make-pathname.13
  (make-pathname :directory :wild :name "hello" :type "txt")
  #p"/**/hello.txt")

(defun make-pathname-test (&rest args)
  (let ((x (apply #'make-pathname args))
        (host (pathname-host *default-pathname-defaults*)))
    (funcall #'make-pathname :defaults x :host host)))

(deftest make-pathname-case-unix.1
  (make-pathname-test :host 'lisp-system::unix :name "ABC" :case :local)
  #p"ABC")

(deftest make-pathname-case-unix.2
  (make-pathname-test :host 'lisp-system::unix :name "ABC" :case :common)
  #p"abc")

(deftest make-pathname-case-unix.3
  (make-pathname-test :host 'lisp-system::unix :name "abc" :case :local)
  #p"abc")

(deftest make-pathname-case-unix.4
  (make-pathname-test :host 'lisp-system::unix :name "abc" :case :common)
  #p"ABC")

(deftest make-pathname-case-unix.5
  (make-pathname-test :host 'lisp-system::unix :name "Abc" :case :local)
  #p"Abc")

(deftest make-pathname-case-unix.6
  (make-pathname-test :host 'lisp-system::unix :name "Abc" :case :common)
  #p"Abc")

(deftest make-pathname-case-windows.1
  (make-pathname-test :host 'lisp-system::windows :name "ABC" :case :local)
  #p"ABC")

(deftest make-pathname-case-windows.2
  (make-pathname-test :host 'lisp-system::windows :name "ABC" :case :common)
  #p"ABC")

(deftest make-pathname-case-windows.3
  (make-pathname-test :host 'lisp-system::windows :name "abc" :case :local)
  #p"abc")

(deftest make-pathname-case-windows.4
  (make-pathname-test :host 'lisp-system::windows :name "abc" :case :common)
  #p"abc")

(deftest make-pathname-case-windows.5
  (make-pathname-test :host 'lisp-system::windows :name "Abc" :case :local)
  #p"Abc")

(deftest make-pathname-case-windows.6
  (make-pathname-test :host 'lisp-system::windows :name "Abc" :case :common)
  #p"Abc")

(deftest make-pathname-case-logical.1
  (make-pathname :host "test" :name "ABC" :case :local)
  #p"test:;ABC")

(deftest make-pathname-case-logical.2
  (make-pathname :host "test" :name "ABC" :case :common)
  #p"test:;abc")

(deftest make-pathname-case-logical.3
  (make-pathname :host "test" :name "abc" :case :local)
  #p"test:;abc")

(deftest make-pathname-case-logical.4
  (make-pathname :host "test" :name "abc" :case :common)
  #p"test:;ABC")

(deftest make-pathname-case-logical.5
  (make-pathname :host "test" :name "Abc" :case :local)
  #p"test:;Abc")

(deftest make-pathname-case-logical.6
  (make-pathname :host "test" :name "Abc" :case :common)
  #p"test:;Abc")

(deftest make-pathname-case-logical.7
  (make-pathname-test
    :host 'lisp-system::unix :case :common
    :name "ABC" :type "DEF" :directory '(:relative "GGG" "hhh" "Ijk"))
  #p"ggg/HHH/Ijk/abc.def")

(deftest make-pathname-case-logical.8
  (make-pathname-test
    :host 'lisp-system::windows :case :common
    :name "abc" :type "def" :directory '(:absolute "GGG" "hhh" "Ijk"))
  #p"/GGG/hhh/Ijk/abc.def")

(deftest make-pathname-case-logical.9
  (make-pathname
    :host "test" :case :common
    :name "abc" :type "Def" :directory '(:relative "GGG" "hhh" "Ijk"))
  #p"test:;ggg;HHH;Ijk;ABC.Def")

(deftest-error make-pathname-error.1
  (make-pathname :host :hello))

(deftest-error make-pathname-error.2
  (make-pathname :name :hello))

(deftest-error make-pathname-error.3
  (make-pathname :type :hello))

(deftest-error make-pathname-error.4
  (make-pathname :directory :hello))

(deftest-error make-pathname-error.5
  (make-pathname :version :hello))

(deftest-error make-pathname-error.6
  (make-pathname :case :hello))

(deftest-error make-pathname-error.7
  (make-pathname :defaults :hello))

(deftest-error make-pathname-error.8
  (make-pathname :hello :hello))

(deftest-error make-pathname-error.9
  (eval '(make-pathname :name)))

;;  ANSI Common Lisp
(deftest make-pathname-test.1
  (make-pathname :directory '(:absolute "public" "games")
                 :name "chess" :type "db")
  #p"/public/games/chess.db")

(deftest make-pathname-test.2
  (equal
    (let (list)
      (dolist (case '(:common :local))
        (dolist (host '(lisp-system::unix lisp-system::windows))
          (push (make-pathname-test
                  :host host :case case
                  :directory '(:absolute "PUBLIC" "GAMES")
                  :name "CHESS" :type "DB")
                list))
        (push (make-pathname
                :host "test" :case case
                :directory '(:absolute "PUBLIC" "GAMES")
                :name "CHESS" :type "DB")
              list))
      (nreverse list))
    (mapcar
      #'parse-namestring
      '("/public/games/chess.db"
        "/PUBLIC/GAMES/CHESS.DB"
        "test:public;games;chess.db"
        "/PUBLIC/GAMES/CHESS.DB"
        "/PUBLIC/GAMES/CHESS.DB"
        "test:PUBLIC;GAMES;CHESS.DB"
        )))
  t)


;;
;;  Function PATHNAME-HOST
;;
#+unix
(deftest pathname-host-unix.1
  (pathname-host
    (make-pathname :host 'lisp-system::unix :name "Hello"))
  lisp-system::unix)

#+unix
(deftest pathname-host-unix.2
  (pathname-host
    (make-pathname :host 'lisp-system::unix :name "Hello" :case :local))
  lisp-system::unix)

#+unix
(deftest pathname-host-unix.3
  (pathname-host
    (make-pathname :host 'lisp-system::unix :name "Hello" :case :common))
  lisp-system::unix)

#+windows
(deftest pathname-host-windows.1
  (pathname-host
    (make-pathname :host 'lisp-system::windows :name "Hello"))
  lisp-system::windows)

#+windows
(deftest pathname-host-windows.2
  (pathname-host
    (make-pathname :host 'lisp-system::windows :name "Hello" :case :local))
  lisp-system::windows)

#+windows
(deftest pathname-host-windows.3
  (pathname-host
    (make-pathname :host 'lisp-system::windows :name "Hello" :case :common))
  lisp-system::windows)

(deftest pathname-host-logical.1
  (pathname-host
    (logical-pathname "test:;aaa.txt"))
  "TEST")

(deftest pathname-host-logical.2
  (pathname-host
    (logical-pathname "TEST:;aaa.txt") :case :local)
  "TEST")

(deftest pathname-host-logical.3
  (pathname-host
    (logical-pathname "test:;aaa.txt") :case :common)
  "TEST")

(deftest pathname-host-logical.4
  (pathname-host
    (logical-pathname "TEST:;aaa.txt") :case :common)
  "TEST")

(deftest pathname-host-logical.5
  (pathname-host
    (logical-pathname "Test:;aaa.txt") :case :common)
  "TEST")

(deftest pathname-host-logical.6
  (symbolp
    (pathname-host #p"Hello.txt"))
  t)

(deftest pathname-host-logical.7
  (pathname-host "Test:;aaa.txt")
  "TEST")

(deftest-error pathname-host-error.1
  (eval '(pathname-host 10))
  type-error)

(deftest-error pathname-host-error.2
  (eval '(pathname-host #p"Hello.txt" :case)))

(deftest-error pathname-host-error.3
  (eval '(pathname-host #p"Hello.txt" :case :hello)))

(deftest-error pathname-host-error.4
  (eval '(pathname-host #p"Hello.txt" :hello)))

(deftest-error pathname-host-error.5
  (eval '(pathname-host #p"Hello.txt" :hello 10)))

(deftest-error! pathname-host-error.6
  (eval '(pathname-host)))


;;
;;  Function PATHNAME-DEVICE
;;
(deftest pathname-device-unix.1
  (pathname-device
    (parse-namestring-unix "/usr/local/bin/hello.txt"))
  nil)

(deftest pathname-device-windows.1
  (pathname-device
    (parse-namestring-unix "\\temp\\hello.txt"))
  nil)

(deftest pathname-device-windows.2
  (pathname-device
    (parse-namestring-windows "\\temp\\hello.txt"))
  nil)

(deftest pathname-device-windows.3
  (pathname-device
    (parse-namestring-windows "C:\\temp\\hello.txt"))
  "C")

(deftest pathname-device-windows.4
  (pathname-device
    (parse-namestring-windows "c:\\temp\\hello.txt"))
  "C")

(deftest pathname-device-windows.5
  (pathname-device
    (parse-namestring-windows "\\\\.\\COM1"))
  lisp-system::device)

(deftest pathname-device-logical.1
  (pathname-device (logical-pathname "test:;aaa.ccc"))
  :unspecific)

(deftest-error pathname-device-error.1
  (eval '(pathname-device 10))
  type-error)

(deftest-error pathname-device-error.2
  (eval '(pathname-device #p"Hello.txt" :case)))

(deftest-error pathname-device-error.3
  (eval '(pathname-device #p"Hello.txt" :case :hello)))

(deftest-error pathname-device-error.4
  (eval '(pathname-device #p"Hello.txt" :hello)))

(deftest-error pathname-device-error.5
  (eval '(pathname-device #p"Hello.txt" :hello 10)))

(deftest-error! pathname-device-error.6
  (eval '(pathname-device)))


;;
;;  Function PATHNAME-DIRECTORY
;;
(deftest pathname-directory.1
  (pathname-directory #p"/usr/local/bin/hello.txt")
  (:absolute "usr" "local" "bin"))

(deftest pathname-directory.2
  (pathname-directory #p"Hello.txt")
  nil)

(deftest pathname-directory-unix.1
  (pathname-directory
    (parse-namestring-unix "/usr/local/bin/aaa.txt"))
  (:absolute "usr" "local" "bin"))

(deftest pathname-directory-unix.2
  (pathname-directory
    (parse-namestring-unix "temp/bin/aaa.txt"))
  (:relative "temp" "bin"))

(deftest pathname-directory-unix.3
  (pathname-directory
    (parse-namestring-unix "/abc/DEF/Ghi/aaa.txt") :case :local)
  (:absolute "abc" "DEF" "Ghi"))

(deftest pathname-directory-unix.4
  (pathname-directory
    (parse-namestring-unix "/abc/DEF/Ghi/aaa.txt") :case :common)
  (:absolute "ABC" "def" "Ghi"))

(deftest pathname-directory-unix.5
  (pathname-directory
    (parse-namestring-unix "/ABC*DEF/*/**/aaa.txt") :case :common)
  (:absolute "abc*def" :wild :wild-inferiors))

(deftest pathname-directory-windows.1
  (pathname-directory
    (parse-namestring-windows "\\usr\\local\\bin\\aaa.txt"))
  (:absolute "usr" "local" "bin"))

(deftest pathname-directory-windows.2
  (pathname-directory
    (parse-namestring-windows "temp\\bin\\aaa.txt"))
  (:relative "temp" "bin"))

(deftest pathname-directory-windows.3
  (pathname-directory
    (parse-namestring-windows "\\abc\\DEF\\Ghi\\aaa.txt") :case :local)
  (:absolute "abc" "DEF" "Ghi"))

(deftest pathname-directory-windows.4
  (pathname-directory
    (parse-namestring-windows "\\abc\\DEF\\Ghi\\aaa.txt") :case :common)
  (:absolute "abc" "DEF" "Ghi"))

(deftest pathname-directory-windows.5
  (pathname-directory
    (parse-namestring-windows "\\ABC*DEF\\*\\**\\aaa.txt") :case :common)
  (:absolute "ABC*DEF" :wild :wild-inferiors))

(deftest pathname-directory-logical.1
  (pathname-directory
    (logical-pathname "test:usr;local;bin;aaa.txt"))
  (:absolute "usr" "local" "bin"))

(deftest pathname-directory-logical.2
  (pathname-directory
    (logical-pathname "test:;temp;bin;aaa.txt"))
  (:relative "temp" "bin"))

(deftest pathname-directory-logical.3
  (pathname-directory
    (logical-pathname "test:abc;DEF;Ghi;aaa.txt") :case :local)
  (:absolute "abc" "DEF" "Ghi"))

(deftest pathname-directory-logical.4
  (pathname-directory
    (logical-pathname "test:abc;DEF;Ghi;aaa.txt") :case :common)
  (:absolute "abc" "DEF" "Ghi"))

(deftest pathname-directory-logical.5
  (pathname-directory
    (logical-pathname "test:ABC*DEF;*;**;aaa.txt") :case :common)
  (:absolute "ABC*DEF" :wild :wild-inferiors))

(deftest-error pathname-directory-error.1
  (eval '(pathname-directory 10))
  type-error)

(deftest-error pathname-directory-error.2
  (eval '(pathname-directory #p"Hello.txt" :case)))

(deftest-error pathname-directory-error.3
  (eval '(pathname-directory #p"Hello.txt" :case :hello)))

(deftest-error pathname-directory-error.4
  (eval '(pathname-directory #p"Hello.txt" :hello)))

(deftest-error pathname-directory-error.5
  (eval '(pathname-directory #p"Hello.txt" :hello 10)))

(deftest-error! pathname-directory-error.6
  (eval '(pathname-directory)))


;;
;;  Function PATHNAME-NAME
;;
(deftest pathname-name.1
  (pathname-name
    #p"/usr/local/bin/aaa.txt")
  "aaa")

(deftest pathname-name-unix.1
  (pathname-name
    (parse-namestring-unix "/usr/local/bin/aaa.txt"))
  "aaa")

(deftest pathname-name-unix.2
  (pathname-name
    (parse-namestring-unix "temp/bin/"))
  nil)

(deftest pathname-name-unix.3
  (pathname-name
    (parse-namestring-unix "/path/to/aaa.txt") :case :local)
  "aaa")

(deftest pathname-name-unix.4
  (pathname-name
    (parse-namestring-unix "/path/to/abc.txt") :case :common)
  "ABC")

(deftest pathname-name-unix.5
  (pathname-name
    (parse-namestring-unix "/path/to/DEF.txt") :case :common)
  "def")

(deftest pathname-name-unix.6
  (pathname-name
    (parse-namestring-unix "/path/to/hellO.txt") :case :common)
  "hellO")

(deftest pathname-name-unix.7
  (pathname-name
    (parse-namestring-unix "/path/to/ABC*DEF.txt") :case :common)
  "abc*def")

(deftest pathname-name-windows.1
  (pathname-name
    (parse-namestring-windows "\\usr\\local\\bin\\aaa.txt"))
  "aaa")

(deftest pathname-name-windows.2
  (pathname-name
    (parse-namestring-windows "temp\\bin\\"))
  nil)

(deftest pathname-name-windows.3
  (pathname-name
    (parse-namestring-windows "\\path\\to\\aaa.txt") :case :local)
  "aaa")

(deftest pathname-name-windows.4
  (pathname-name
    (parse-namestring-windows "\\path\\to\\abc.txt") :case :common)
  "abc")

(deftest pathname-name-windows.5
  (pathname-name
    (parse-namestring-windows "\\path\\to\\DEF.txt") :case :common)
  "DEF")

(deftest pathname-name-windows.6
  (pathname-name
    (parse-namestring-windows "\\path\\to\\hellO.txt") :case :common)
  "hellO")

(deftest pathname-name-windows.7
  (pathname-name
    (parse-namestring-windows "\\path\\to\\ABC*DEF.txt") :case :common)
  "ABC*DEF")

(deftest pathname-name-logical.1
  (pathname-name
    (logical-pathname "test:;usr;local;bin;aaa.txt"))
  "aaa")

(deftest pathname-name-logical.2
  (pathname-name
    (logical-pathname "test:temp;bin;"))
  nil)

(deftest pathname-name-logical.3
  (pathname-name
    (logical-pathname "test:;path;to;aaa.txt") :case :local)
  "aaa")

(deftest pathname-name-logical.4
  (pathname-name
    (logical-pathname "test:;path;to;abc.txt") :case :common)
  "abc")

(deftest pathname-name-logical.5
  (pathname-name
    (logical-pathname "test:;path;to;DEF.txt") :case :common)
  "DEF")

(deftest pathname-name-logical.6
  (pathname-name
    (logical-pathname "test:;path;to;hellO.txt") :case :common)
  "hellO")

(deftest pathname-name-logical.7
  (pathname-name
    (logical-pathname "test:;path;to;ABC*DEF.txt") :case :common)
  "ABC*DEF")

(deftest-error pathname-name-error.1
  (eval '(pathname-name 10))
  type-error)

(deftest-error pathname-name-error.2
  (eval '(pathname-name #p"Hello.txt" :case)))

(deftest-error pathname-name-error.3
  (eval '(pathname-name #p"Hello.txt" :case :hello)))

(deftest-error pathname-name-error.4
  (eval '(pathname-name #p"Hello.txt" :hello)))

(deftest-error pathname-name-error.5
  (eval '(pathname-name #p"Hello.txt" :hello 10)))

(deftest-error! pathname-name-error.6
  (eval '(pathname-name)))


;;
;;  Function PATHNAME-TYPE
;;
(deftest pathname-type.1
  (pathname-type
    #p"/usr/local/bin/aaa.txt")
  "txt")

(deftest pathname-type-unix.1
  (pathname-type
    (parse-namestring-unix "/usr/local/bin/aaa.txt"))
  "txt")

(deftest pathname-type-unix.2
  (pathname-type
    (parse-namestring-unix "/usr/local/bin/aaa"))
  nil)

(deftest pathname-type-unix.3
  (pathname-type
    (parse-namestring-unix "/usr/local/bin/aaa."))
  "")

(deftest pathname-type-unix.4
  (pathname-type
    (parse-namestring-unix "temp/bin/"))
  nil)

(deftest pathname-type-unix.5
  (pathname-type
    (parse-namestring-unix "/path/to/aaa.txt") :case :local)
  "txt")

(deftest pathname-type-unix.6
  (pathname-type
    (parse-namestring-unix "/path/to/aaa.abc") :case :common)
  "ABC")

(deftest pathname-type-unix.7
  (pathname-type
    (parse-namestring-unix "/path/to/bbb.DEF") :case :common)
  "def")

(deftest pathname-type-unix.8
  (pathname-type
    (parse-namestring-unix "/path/to/hello.Hello") :case :common)
  "Hello")

(deftest pathname-type-unix.9
  (pathname-type
    (parse-namestring-unix "/path/to/name.ABC*DEF") :case :common)
  "abc*def")

(deftest pathname-type-windows.1
  (pathname-type
    (parse-namestring-windows "\\usr\\local\\bin\\aaa.txt"))
  "txt")

(deftest pathname-type-windows.2
  (pathname-type
    (parse-namestring-windows "\\usr\\local\\bin\\aaa"))
  nil)

(deftest pathname-type-windows.3
  (pathname-type
    (parse-namestring-windows "\\usr\\local\\bin\\aaa."))
  "")

(deftest pathname-type-windows.4
  (pathname-type
    (parse-namestring-windows "temp\\bin\\"))
  nil)

(deftest pathname-type-windows.5
  (pathname-type
    (parse-namestring-windows "\\path\\to\\aaa.txt") :case :local)
  "txt")

(deftest pathname-type-windows.6
  (pathname-type
    (parse-namestring-windows "\\path\\to\\aaa.abc") :case :common)
  "abc")

(deftest pathname-type-windows.7
  (pathname-type
    (parse-namestring-windows "\\path\\to\\bbb.DEF") :case :common)
  "DEF")

(deftest pathname-type-windows.8
  (pathname-type
    (parse-namestring-windows "\\path\\to\\hello.Hello") :case :common)
  "Hello")

(deftest pathname-type-windows.9
  (pathname-type
    (parse-namestring-windows "\\path\\to\\hello.ABC*DEF") :case :common)
  "ABC*DEF")

(deftest pathname-type-logical.1
  (pathname-type
    (logical-pathname "test:;usr;local;bin;aaa.txt"))
  "txt")

(deftest pathname-type-logical.2
  (pathname-type
    (logical-pathname "test:;usr;local;bin;aaa"))
  nil)

(deftest pathname-type-logical.3
  (pathname-type
    (logical-pathname "test:;usr;local;bin;aaa."))
  "")

(deftest pathname-type-logical.4
  (pathname-type
    (logical-pathname "test:temp;bin;"))
  nil)

(deftest pathname-type-logical.5
  (pathname-type
    (logical-pathname "test:;path;to;aaa.txt") :case :local)
  "txt")

(deftest pathname-type-logical.6
  (pathname-type
    (logical-pathname "test:;path;to;aaa.abc") :case :common)
  "abc")

(deftest pathname-type-logical.7
  (pathname-type
    (logical-pathname "test:;path;to;bbb.DEF") :case :common)
  "DEF")

(deftest pathname-type-logical.8
  (pathname-type
    (logical-pathname "test:;path;to;hello.Hello") :case :common)
  "Hello")

(deftest pathname-type-logical.9
  (pathname-type
    (logical-pathname "test:;path;to;hello.ABC*DEF") :case :common)
  "ABC*DEF")

(deftest-error pathname-type-error.1
  (eval '(pathname-type 10))
  type-error)

(deftest-error pathname-type-error.2
  (eval '(pathname-type #p"Hello.txt" :case)))

(deftest-error pathname-type-error.3
  (eval '(pathname-type #p"Hello.txt" :case :hello)))

(deftest-error pathname-type-error.4
  (eval '(pathname-type #p"Hello.txt" :hello)))

(deftest-error pathname-type-error.5
  (eval '(pathname-type #p"Hello.txt" :hello 10)))

(deftest-error! pathname-type-error.6
  (eval '(pathname-type)))


;;
;;  Function PATHNAME-VERSION
;;
(deftest pathname-version-unix.1
  (let ((x (parse-namestring-unix "/path/to/name.txt.100")))
    (values
      (pathname-name x)
      (pathname-type x)
      (pathname-version x)))
  "name.txt" "100" :unspecific)

(deftest pathname-version-unix.2
  (let ((x (parse-namestring-unix "/path/to/name.txt.newest")))
    (values
      (pathname-name x)
      (pathname-type x)
      (pathname-version x)))
  "name.txt" "newest" :unspecific)

(deftest pathname-version-windows.1
  (let ((x (parse-namestring-windows "\\path\\to\\name.txt.100")))
    (values
      (pathname-name x)
      (pathname-type x)
      (pathname-version x)))
  "name.txt" "100" :unspecific)

(deftest pathname-version-windows.2
  (let ((x (parse-namestring-windows "\\path\\to\\name.txt.newest")))
    (values
      (pathname-name x)
      (pathname-type x)
      (pathname-version x)))
  "name.txt" "newest" :unspecific)

(deftest pathname-version-logical.1
  (pathname-version
    (logical-pathname "test:;aaa;bbb;name.txt.100"))
  100)

(deftest pathname-version-logical.2
  (pathname-version
    (logical-pathname "test:;aaa;bbb;name.txt.newest"))
  :newest)

(deftest-error pathname-version-error.1
  (eval '(pathname-version 10))
  type-error)

(deftest-error! pathname-version-error.2
  (eval '(pathname-version)))

(deftest-error! pathname-version-error.3
  (eval '(pathname-version #p"Hello" nil)))


;;  ANSI Common Lisp
(deftest pathname-test.1
  (let ((q (make-pathname :host "TEST"
                          :directory "CHAPMAN"
                          :name "LOGIN" :type "COM")))
    (values
      (pathname-host q)
      (pathname-name q)
      (pathname-type q)))
  "TEST" "LOGIN" "COM")

#+unix
(progn
  (deftest pathname-test.2
    (pathname-directory "foo.l")
    nil)

  (deftest pathname-test.3
    (pathname-device "foo.l")
    nil)  ;; :UNSPECIFIC

  (deftest pathname-test.4
    (pathname-name "foo.l")
    "foo")

  (deftest pathname-test.5
    (pathname-name "foo.l" :case :local)
    "foo")

  (deftest pathname-test.6
    (pathname-name "foo.l" :case :common)
    "FOO")

  (deftest pathname-test.7
    (pathname-type "foo.l")
    "l")

  (deftest pathname-test.8
    (pathname-type "foo.l" :case :local)
    "l")

  (deftest pathname-test.9
    (pathname-type "foo.l" :case :common)
    "L")

  (deftest pathname-test.10
    (pathname-type "foo")
    nil)  ;; :UNSPECIFIC

  (deftest pathname-test.11
    (pathname-type "foo" :case :common)
    nil)  ;; :UNSPECIFIC

  (deftest pathname-test.12
    (pathname-type "foo.")
    "")

  (deftest pathname-test.13
    (pathname-type "foo." :case :common)
    "")

  (deftest pathname-test.14
    (pathname-directory (parse-namestring "/foo/bar/baz.lisp") :case :local)
    (:absolute "foo" "bar"))

  (deftest pathname-test.15
    (pathname-directory (parse-namestring "/foo/bar/baz.lisp") :case :common)
    (:absolute "FOO" "BAR"))

  (deftest pathname-test.16
    (pathname-directory (parse-namestring "../baz.lisp"))
    (:relative :up))

  (deftest pathname-test.17
    (pathname-directory (parse-namestring "/foo/BAR/../Mum/baz"))
    (:absolute "foo" "BAR" :UP "Mum"))

  (deftest pathname-test.18
    (pathname-directory (parse-namestring "/foo/BAR/../Mum/baz") :case :common)
    (:absolute "FOO" "bar" :UP "Mum"))

  (deftest pathname-test.19
    (pathname-directory (parse-namestring "/foo/*/bar/baz.l"))
    (:absolute "foo" :WILD "bar"))

  (deftest pathname-test.20
    (pathname-directory (parse-namestring "/foo/*/bar/baz.l") :case :common)
    (:absolute "FOO" :WILD "BAR")))

