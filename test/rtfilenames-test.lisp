;;
;;  ANSI COMMON LISP: 19. Filenames
;;

;;
;;  Variable *DEFAULT-PATHNAME-DEFAULTS*
;;
(deftest default-pathname-defaults.1
  (lisp-system:specialp '*default-pathname-defaults*)
  t)

(deftest default-pathname-defaults.2
  (pathnamep *default-pathname-defaults*)
  t)

(deftest default-pathname-defaults.3
  (pathname-host
    *default-pathname-defaults*)
  #+windows lisp-system::windows
  #-windows lisp-system::unix)


;;
;;  Function WILD-PATHNAME-P
;;
(deftest wild-pathname-p.1
  (wild-pathname-p #p"")
  nil)

(deftest wild-pathname-p.2
  (wild-pathname-p "")
  nil)

(deftest wild-pathname-p.3
  (wild-pathname-p (make-pathname :name :wild))
  t)

(deftest wild-pathname-p.4
  (wild-pathname-p #p"/usr/*/local/")
  t)

(deftest wild-pathname-p.5
  (wild-pathname-p #p"/usr/**/local/")
  t)

(deftest wild-pathname-p.6
  (wild-pathname-p #p"/usr/aaa*bbb/local/")
  t)

(deftest wild-pathname-p.7
  (wild-pathname-p #p"/usr/local/bin/*.txt")
  t)

(deftest wild-pathname-p.8
  (wild-pathname-p #p"/usr/local/bin/hello.*")
  t)

(deftest wild-pathname-p.9
  (wild-pathname-p #p"/usr/local/bin/he*llo.txt")
  t)

(deftest wild-pathname-p.10
  (wild-pathname-p #p"/usr/local/bin/hello.txt")
  nil)

(deftest wild-pathname-p.11
  (wild-pathname-p #p"test:hello.txt.*")
  t)

(deftest wild-pathname-p-field.1
  (wild-pathname-p #p"/*/hello*.*" :host)
  nil)

(deftest wild-pathname-p-field.2
  (wild-pathname-p #p"/*/hello*.*" :device)
  nil)

(deftest wild-pathname-p-field.3
  (wild-pathname-p
    (parse-namestring "\\usr\\local\\" 'lisp-system::windows)
    :device)
  nil)

(deftest wild-pathname-p-field.4
  (wild-pathname-p #p"/*/hello*.*" :directory)
  t)

(deftest wild-pathname-p-field.5
  (wild-pathname-p #p"/usr/local/hello*.*" :directory)
  nil)

(deftest wild-pathname-p-field.6
  (wild-pathname-p #p"/*/hello*.*" :name)
  t)

(deftest wild-pathname-p-field.7
  (wild-pathname-p #p"/*/hello.*" :name)
  nil)

(deftest wild-pathname-p-field.8
  (wild-pathname-p #p"/*/hello*.*" :type)
  t)

(deftest wild-pathname-p-field.9
  (wild-pathname-p #p"/*/hello*.txt" :type)
  nil)

(deftest wild-pathname-p-field.10
  (wild-pathname-p #p"test:;*;hello*.txt.*" :version)
  t)

(deftest wild-pathname-p-field.11
  (wild-pathname-p #p"test:;*;hello*.txt" :version)
  nil)

(deftest wild-pathname-p-field.12
  (wild-pathname-p #p"test:;*;hello*.txt.999" :version)
  nil)

(deftest wild-pathname-p-field.13
  (wild-pathname-p #p"test:;*;hello*.txt.999" nil)
  t)

(deftest-error wild-pathname-p-error.1
  (eval '(wild-pathname-p 10)))

(deftest-error wild-pathname-p-error.2
  (eval '(wild-pathname-p #p"Hello.txt" 20)))

(deftest-error! wild-pathname-p-error.3
  (eval '(wild-pathname-p)))

(deftest-error! wild-pathname-p-error.4
  (eval '(wild-pathname-p #p"Hello.txt" nil nil)))

;;  ANSI Common Lisp
(deftest wild-pathname-p-test.1
  (wild-pathname-p (make-pathname :name :wild))
  t)

(deftest wild-pathname-p-test.2
  (wild-pathname-p (make-pathname :name :wild) :name)
  t)

(deftest wild-pathname-p-test.3
  (wild-pathname-p (make-pathname :name :wild) :type)
  nil)

(deftest wild-pathname-p-test.4
  (wild-pathname-p (parse-namestring "s:\\foo\\**\\" 'lisp-system::windows))
  t)

(deftest wild-pathname-p-test.5
  (wild-pathname-p (make-pathname :name "F*O"))
  t)


;;
;;  Function PATHNAME-MATCH-P
;;
(deftest pathname-match-p-directory.1
  (pathname-match-p #p"/usr/local/bin/" #p"/usr/*/bin/")
  t)

(deftest pathname-match-p-directory.2
  (pathname-match-p #p"/usr/local/bin/" #p"/usr/lo*/bin/")
  t)

(deftest pathname-match-p-directory.3
  (pathname-match-p #p"/usr/local/bin/" #p"/usr/**/bin/")
  t)

(deftest pathname-match-p-directory.4
  (pathname-match-p #p"/usr/local/aaa/bbb/ccc/bin/" #p"/usr/**/bin/")
  t)

(deftest pathname-match-p-directory.5
  (pathname-match-p #p"path/to/hello.txt" #p"*.*")
  t)

(deftest pathname-match-p-directory.6
  (pathname-match-p #p"path/to/hello.txt" #p"./*.*")
  nil)

(deftest pathname-match-p-name.1
  (pathname-match-p #p"hello.txt" #p"hello.txt")
  t)

(deftest pathname-match-p-name.2
  (pathname-match-p #p"hello.txt" #p"abc.txt")
  nil)

(deftest pathname-match-p-name.3
  (pathname-match-p #p"hello.txt" #p"*.txt")
  t)

(deftest pathname-match-p-name.4
  (pathname-match-p #p"hello.txt" #p"he*o.txt")
  t)

(deftest pathname-match-p-name.5
  (pathname-match-p #p"he*o.txt" #p"he*o.txt")
  t)

(deftest pathname-match-p-name.6
  (pathname-match-p #p"h*o.txt" #p"he*o.txt")
  nil)

(deftest pathname-match-p-type.1
  (pathname-match-p #p"hello.txt" #p"hello.txt")
  t)

(deftest pathname-match-p-type.2
  (pathname-match-p #p"hello.txt" #p"hello.abc")
  nil)

(deftest pathname-match-p-type.3
  (pathname-match-p #p"hello.txt" #p"hello.*")
  t)

(deftest pathname-match-p-type.4
  (pathname-match-p #p"hello.txt" #p"hello.*t")
  t)

(deftest pathname-match-p-type.5
  (pathname-match-p #p"hello.t*t" #p"he*o.t*t")
  t)

(deftest pathname-match-p-type.6
  (pathname-match-p #p"hello.t*t" #p"hello.t*")
  nil)

(deftest pathname-match-p-version.1
  (pathname-match-p #p"test:hello.txt" #p"test:hello.txt")
  t)

(deftest pathname-match-p-version.2
  (pathname-match-p #p"test:hello.txt" #p"test:hello.txt.*")
  t)

(deftest pathname-match-p-version.3
  (pathname-match-p #p"test:hello.txt.newest" #p"test:hello.txt.*")
  t)

(deftest pathname-match-p-version.4
  (pathname-match-p #p"test:hello.txt.10" #p"test:hello.txt.*")
  t)

(deftest pathname-match-p-version.5
  (pathname-match-p #p"test:hello.txt.*" #p"test:hello.txt.*")
  t)

(deftest pathname-match-p-version.6
  (pathname-match-p #p"test:hello.txt.10" #p"test:hello.txt")
  nil)

(deftest-error pathname-match-p-error.1
  (eval '(pathname-match-p 10))
  type-error)

(deftest-error pathname-match-p-error.2
  (eval '(pathname-match-p #p"Hello.txt" 20))
  type-error)

(deftest-error! pathname-match-p-error.3
  (eval '(pathname-match-p)))

(deftest-error! pathname-match-p-error.4
  (eval '(pathname-match-p #p"hello.txt" #p"hello.txt" nil)))


;;
;;  Function MERGE-PATHNAMES
;;
(deftest merge-pathnames.1
  (let ((*default-pathname-defaults* #p"/usr/local/"))
    (merge-pathnames #p"bin/aaa.bin"))
  #p"/usr/local/bin/aaa.bin")

(deftest merge-pathnames.2
  (merge-pathnames #p"bin/aaa.bin" #p"/usr/local/")
  #p"/usr/local/bin/aaa.bin")

(deftest merge-pathnames.3
  (merge-pathnames
    (parse-namestring "test:;bin;aaa.bin")
    (parse-namestring "test:usr;local;") 10)
  #p"test:usr;local;bin;aaa.bin.10")

(deftest merge-pathnames.4
  (merge-pathnames
    (parse-namestring "test:;bin;aaa.bin.newest")
    (parse-namestring "test:usr;local;") 10)
  #p"test:usr;local;bin;aaa.bin.newest")

(deftest merge-pathnames-device.1
  (let ((*default-pathname-defaults*
          (make-pathname :host 'lisp-system::windows :device "E")))
    (namestring
      (merge-pathnames
        (parse-namestring "hello.txt" 'lisp-system::windows))))
  "E:hello.txt")

(deftest merge-pathnames-device.2
  (let ((*default-pathname-defaults*
          (make-pathname :host 'lisp-system::windows :device "E"
                         :directory '(:absolute "windows"))))
    (namestring
      (merge-pathnames
        (parse-namestring "hello.txt" 'lisp-system::windows))))
  "E:\\windows\\hello.txt")

(deftest merge-pathnames-device.3
  (let ((*default-pathname-defaults*
          (make-pathname :host 'lisp-system::windows :device "E"
                         :directory '(:absolute "windows"))))
    (namestring
      (merge-pathnames
        (parse-namestring "D:hello.txt" 'lisp-system::windows))))
  "D:\\windows\\hello.txt")

(deftest merge-pathnames-directory.1
  (merge-pathnames #p"hello.txt" #p"/usr/local/")
  #p"/usr/local/hello.txt")

(deftest merge-pathnames-directory.2
  (merge-pathnames #p"/tmp/hello.txt" #p"/usr/local/")
  #p"/tmp/hello.txt")

(deftest merge-pathnames-name.1
  (merge-pathnames #p"/usr/local/" #p"hello.txt")
  #p"/usr/local/hello.txt")

(deftest merge-pathnames-name.2
  (merge-pathnames #p"/usr/local/abc.zip" #p"hello.txt")
  #p"/usr/local/abc.zip")

(deftest merge-pathnames-type.1
  (merge-pathnames #p"/usr/local/abc" #p"hello.txt")
  #p"/usr/local/abc.txt")

(deftest merge-pathnames-type.2
  (merge-pathnames #p"/usr/local/abc.zip" #p"hello.txt")
  #p"/usr/local/abc.zip")

(deftest merge-pathnames-version.1
  (merge-pathnames #p"test:hello.txt" #p"test:file.txt")
  #p"test:hello.txt")

(deftest merge-pathnames-version.2
  (merge-pathnames #p"test:hello.txt" #p"test:file.txt")
  #p"test:hello.txt.newest")

(deftest merge-pathnames-version.3
  (merge-pathnames #p"test:hello.txt" #p"test:file.txt.newest")
  #p"test:hello.txt")

(deftest merge-pathnames-version.4
  (merge-pathnames #p"test:hello.txt" #p"test:file.txt.newest")
  #p"test:hello.txt.newest")

(deftest merge-pathnames-version.5
  (merge-pathnames #p"test:hello.txt" #p"test:file.txt.123")
  #p"test:hello.txt")  ;; not 123

(deftest merge-pathnames-version.6
  (merge-pathnames #p"test:hello.txt.newest" #p"test:file.txt.123")
  #p"test:hello.txt.newest")

(deftest merge-pathnames-version.7
  (merge-pathnames #p"test:hello.txt.999" #p"test:file.txt.123")
  #p"test:hello.txt.999")

(deftest merge-pathnames-version.8
  (merge-pathnames #p"test:.txt" #p"test:file.txt")
  #p"test:file.txt")

(deftest merge-pathnames-version.9
  (merge-pathnames #p"test:.txt" #p"test:file.txt")
  #p"test:file.txt.newest")

(deftest merge-pathnames-version.10
  (merge-pathnames #p"test:.txt" #p"test:file.txt.newest")
  #p"test:file.txt")

(deftest merge-pathnames-version.11
  (merge-pathnames #p"test:.txt" #p"test:file.txt.newest")
  #p"test:file.txt.newest")

(deftest merge-pathnames-version.12
  (merge-pathnames #p"test:.txt" #p"test:file.txt.123")
  #p"test:file.txt.123")

(deftest merge-pathnames-version.13
  (merge-pathnames #p"test:.txt.newest" #p"test:file.txt.123")
  #p"test:file.txt.newest")

(deftest merge-pathnames-version.14
  (merge-pathnames #p"test:.txt.999" #p"test:file.txt.123")
  #p"test:file.txt.999")

(deftest-error merge-pathnames-error.1
  (eval '(merge-pathnames 10))
  type-error)

(deftest-error merge-pathnames-error.2
  (eval '(merge-pathnames #p"hello.txt" 20))
  type-error)

(deftest-error merge-pathnames-error.3
  (eval '(merge-pathnames #p"hello.txt" #p"/usr/local/" :hello))
  type-error)

(deftest-error! merge-pathnames-error.4
  (eval '(merge-pathnames)))

(deftest-error! merge-pathnames-error.5
  (eval '(merge-pathnames #p"hello.txt" #p"/usr/local/" 10 nil)))


;;
;;  error
;;
(deftest pathname-host-string.1
  (pathname-host "test:;aaa.txt")
  "TEST")

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
  (pathname-version "test:;aaa;bbb;name.txt.100")
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

