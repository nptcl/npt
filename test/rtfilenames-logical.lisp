;;
;;  ANSI COMMON LISP: 19. Filenames
;;

;;
;;  Function LOAD-LOGICAL-PATHNAME-TRANSLATIONS
;;
(deftest load-logical-pathname-translations.1
  (load-logical-pathname-translations "no-such-logical-pathname")
  nil)

(deftest load-logical-pathname-translations.2
  (let ((lisp-system::*load-logical-pathname-translations*
          (make-pathname :type "lisp" :defaults #p"test/")))
    (load-logical-pathname-translations "rtfilenames-name"))
  t)

(deftest load-logical-pathname-translations.3
  (let ((lisp-system::*load-logical-pathname-translations*
          (make-pathname :type "lisp" :defaults #p"no-such-directory/")))
    (load-logical-pathname-translations "rtfilenames-name"))
  nil)

(deftest load-logical-pathname-translations.4
  (let ((lisp-system::*load-logical-pathname-translations*
          (make-pathname :type "lisp" :defaults #p"test/")))
    (load-logical-pathname-translations "rtfilenames-name")
    (equal
      (translate-logical-pathname
        (parse-namestring "rtfilenames-name:path;to;hello.txt"))
      (parse-namestring "/ccc/ddd/eee/hello.txt")))
  t)

(deftest load-logical-pathname-translations.5
  (let ((lisp-system::*load-logical-pathname-translations*
          (make-pathname :type "lisp" :defaults #p"test/")))
    (load-logical-pathname-translations "rtfilenames-name")
    (equal
      (translate-logical-pathname
        (parse-namestring "rtfilenames-name:zzz.txt"))
      (parse-namestring "/aaa/bbb/zzz.txt")))
  t)

(deftest-error load-logical-pathname-translations-error.1
  (eval '(load-logical-pathname-translations :hello))
  type-error)

(deftest-error! load-logical-pathname-translations-error.2
  (eval '(load-logical-pathname-translations)))

(deftest-error! load-logical-pathname-translations-error.3
  (eval '(load-logical-pathname-translations "hello" nil)))


;;
;;  Accessor LOGICAL-PATHNAME-TRANSLATIONS
;;
(deftest logical-pathname-translations.1
  (logical-pathname-translations "TEST-PATH2")
  nil)

(deftest logical-pathname-translations.2
  (progn
    (setf (logical-pathname-translations "TEST-PATH3")
          '(("path;*.*.*" "/usr/local/path/")))
    (equal
      (logical-pathname-translations "TEST-PATH3")
      (list (list (parse-namestring "test-path3:path;*.*.*")
                  (parse-namestring "/usr/local/path/")))))
  t)

(deftest logical-pathname-translations.3
  (progn
    (setf (logical-pathname-translations "test-path4")
          '(("path;*.*.*" "/usr/local/path/")
            ("aaa;bbb;*.lisp" "/usr/local/lisp/*.l")))
    (equal
      (logical-pathname-translations "TEST-PATH4")
      (list (list (parse-namestring "test-path4:path;*.*.*")
                  (parse-namestring "/usr/local/path/"))
            (list (parse-namestring "test-path4:aaa;bbb;*.lisp")
                  (parse-namestring "/usr/local/lisp/*.l")))))
  t)

(deftest-error logical-pathname-translations-error.1
  (eval '(logical-pathname-translations 10))
  type-error)

(deftest-error! logical-pathname-translations-error.2
  (eval '(logical-pathname-translations)))

(deftest-error! logical-pathname-translations-error.3
  (eval '(logical-pathname-translations "hello" nil)))

(deftest-error logical-pathname-translations-error.4
  (eval '(setf (logical-pathname-translations 10) nil))
  type-error)

(deftest-error! logical-pathname-translations-error.5
  (eval '(setf (logical-pathname-translations) nil)))

(deftest-error! logical-pathname-translations-error.6
  (eval '(setf (logical-pathname-translations "hello" nil) nil)))

(deftest-error logical-pathname-translations-error.7
  (eval '(setf (logical-pathname-translations "hello") 30)))


;;  ANSI Common Lisp
(deftest logical-pathname-translations-test.1
  (progn
    (setf (logical-pathname-translations "prog")
          '(("RELEASED;*.*.*"        "/sys/bin/my-prog/")
            ("RELEASED;*;*.*.*"      "/sys/bin/my-prog/*/")
            ("EXPERIMENTAL;*.*.*"    "/usr/Joe/development/prog/")
            ("EXPERIMENTAL;*;*.*.*"  "/usr/Joe/development/prog/*/")))
    (values)))

(deftest logical-pathname-translations-test.2
  (progn
    (setf (logical-pathname-translations "prog")
          '(("CODE;*.*.*"             "/lib/prog/")))
    (translate-logical-pathname "prog:code;documentation.lisp"))
  #p"/lib/prog/documentation.lisp")

(deftest logical-pathname-translations-test.3
  (progn
    (setf (logical-pathname-translations "prog")
          '(("CODE;DOCUMENTATION.*.*" "/lib/prog/docum.*")
            ("CODE;*.*.*"             "/lib/prog/")))
    (translate-logical-pathname "prog:code;documentation.lisp"))
  #p"/lib/prog/docum.lisp")


;;
;;  Function LOGICAL-PATHNAME
;;
(deftest logical-pathname.1
  (equal
    (logical-pathname
      (parse-namestring "test:*.*.*"))
    (parse-namestring "test:*.*.*"))
  t)

(deftest logical-pathname.2
  (equal
    (logical-pathname "test:*.*.*")
    (parse-namestring "test:*.*.*"))
  t)

(deftest-error logical-pathname.3
  (logical-pathname "hello.txt"))

(deftest logical-pathname.4
  (logical-pathname "test:hello.txt")
  #p"test:hello.txt")

(setf (logical-pathname-translations "TEST-OPEN1")
      '(("*.*.*" "")))
#-ansi-c
(deftest logical-pathname.5
  (let ((x (open (logical-pathname "test-open1:;hello.txt")
                 :direction :output :if-exists :supersede)))
    (unwind-protect
      (logical-pathname x)
      (close x :abort t)))
  #p"test-open1:;hello.txt")

#-ansi-c
(deftest logical-pathname.6
  (let ((x (open (logical-pathname "test-open1:;hello.txt")
                 :direction :output :if-exists :supersede)))
    (close x :abort t)
    (logical-pathname x))
  #p"test-open1:;hello.txt")

(deftest-error logical-pathname-error.1
  (eval '(logical-pathname 10))
  type-error)

(deftest-error logical-pathname-error.2
  (with-open-stream (x (make-broadcast-stream))
    (logical-pathname x))
  type-error)

(deftest-error logical-pathname-error.3
  (logical-pathname #p"Hello.txt")
  type-error)

(deftest-error! logical-pathname-error.4
  (eval '(logical-pathname)))

(deftest-error! logical-pathname-error.5
  (eval '(logical-pathname #p"test-open1:;hello.txt" nil)))


;;
;;  Function TRANSLATE-LOGICAL-PATHNAME
;;
(deftest translate-logical-pathname.1
  (translate-logical-pathname #p"hello.txt")
  #p"hello.txt")

(deftest translate-logical-pathname.2
  (progn
    (setf (logical-pathname-translations "test2")
          '(("path;to;*.*" "/usr/local/")
            ("*.*.*" "/var/")))
    (equal
      (translate-logical-pathname
        (parse-namestring "test2:hello.txt"))
      (parse-namestring "/var/hello.txt")))
  t)

(deftest translate-logical-pathname.3
  (progn
    (setf (logical-pathname-translations "test2")
          '(("path;to;*.*" "/usr/local/")
            ("*.*.*" "/var/")))
    (equal
      (translate-logical-pathname
        (parse-namestring "test2:path;to;hello.txt"))
      (parse-namestring "/usr/local/hello.txt")))
  t)

#-ansi-c
(deftest translate-logical-pathname.4
  (let ((x (open (translate-logical-pathname "test-open1:;hello.txt")
                 :direction :output :if-exists :supersede)))
    (unwind-protect
      (translate-logical-pathname x)
      (close x :abort t)))
  #p"hello.txt")

#-ansi-c
(deftest translate-logical-pathname.5
  (let ((x (open (translate-logical-pathname "test-open1:;hello.txt")
                 :direction :output :if-exists :supersede)))
    (close x :abort t)
    (translate-logical-pathname x))
  #p"hello.txt")

(deftest-error translate-logical-pathname-error.1
  (eval '(translate-logical-pathname 10))
  type-error)

(deftest-error translate-logical-pathname-error.2
  (with-open-stream (x (make-broadcast-stream))
    (translate-logical-pathname x))
  type-error)

(deftest-error! translate-logical-pathname-error.3
  (eval '(translate-logical-pathname)))

(deftest-error! translate-logical-pathname-error.4
  (eval '(translate-logical-pathname #p"test-open1:;hello.txt" nil)))

