;;
;;  ANSI COMMON LISP: 19. Filenames
;;

;;
;;
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
          (make-pathname :type "lisp" :defaults #p"test/")))
    (load-logical-pathname-translations "rtfilenames-name")
    (equal
      (translate-logical-pathname
        (parse-namestring "rtfilenames-name:path;to;hello.txt"))
      (parse-namestring "/ccc/ddd/eee/hello.txt")))
  t)

(deftest load-logical-pathname-translations.4
  (let ((lisp-system::*load-logical-pathname-translations*
          (make-pathname :type "lisp" :defaults #p"test/")))
    (load-logical-pathname-translations "rtfilenames-name")
    (equal
      (translate-logical-pathname
        (parse-namestring "rtfilenames-name:zzz.txt"))
      (parse-namestring "/aaa/bbb/zzz.txt")))
  t)


;;
;;
;;
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


;;
;;
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


;;
;;
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

