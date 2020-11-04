;;
;;  ANSI COMMON LISP: 19. Filenames
;;
(deftest pathname-equal.1
  (equal
    (make-pathname :directory "path")
    (make-pathname :directory "path"))
  t)

(deftest pathname-equal.2
  (equal
    (make-pathname :host 'lisp-system::unix :directory "path")
    (make-pathname :host 'lisp-system::windows :directory "path"))
  nil)

(deftest pathname-equal.3
  (equal
    (make-pathname :host 'lisp-system::unix :directory "path")
    (make-pathname :host "test" :directory "path"))
  nil)

(deftest pathname-equal.4
  (equal
    (make-pathname :host 'lisp-system::windows :directory "path")
    (make-pathname :host "test" :directory "path"))
  nil)

(deftest pathname-equal-unix.1
  (equal
    (parse-namestring-unix "")
    (parse-namestring-unix ""))
  t)

(deftest pathname-equal-unix.2
  (values
    (equal (parse-namestring-unix "/aaa/") (parse-namestring-unix "/aaa/"))
    (equal (parse-namestring-unix "/aaa/") (parse-namestring-unix "/AAA/"))
    (equal (parse-namestring-unix "/aaa/") (parse-namestring-unix "/Aaa/")))
  t nil nil)

(deftest pathname-equal-unix.3
  (values
    (equal (parse-namestring-unix "aaa/") (parse-namestring-unix "aaa/"))
    (equal (parse-namestring-unix "aaa/") (parse-namestring-unix "AAA/"))
    (equal (parse-namestring-unix "aaa/") (parse-namestring-unix "Aaa/")))
  t nil nil)

(deftest pathname-equal-unix.4
  (values
    (equal (parse-namestring-unix "/aaa/") (parse-namestring-unix "aaa/"))
    (equal (parse-namestring-unix "/aaa/") (parse-namestring-unix "AAA/"))
    (equal (parse-namestring-unix "/aaa/") (parse-namestring-unix "Aaa/")))
  nil nil nil)

(deftest pathname-equal-unix.5
  (values
    (equal (parse-namestring-unix "aaa") (parse-namestring-unix "aaa"))
    (equal (parse-namestring-unix "aaa") (parse-namestring-unix "AAA"))
    (equal (parse-namestring-unix "aaa") (parse-namestring-unix "Aaa")))
  t nil nil)

(deftest pathname-equal-unix.6
  (values
    (equal (parse-namestring-unix "z.aaa") (parse-namestring-unix "z.aaa"))
    (equal (parse-namestring-unix "z.aaa") (parse-namestring-unix "z.AAA"))
    (equal (parse-namestring-unix "z.aaa") (parse-namestring-unix "z.Aaa")))
  t nil nil)

(deftest pathname-equal-unix.7
  (values
    (equal (parse-namestring-unix "hello.txt")
           (parse-namestring-unix "h*o.txt"))
    (equal (parse-namestring-unix "h*o.txt")
           (parse-namestring-unix "h*o.txt"))
    (equal (parse-namestring-unix "/a/b/c/d/")
           (parse-namestring-unix "/a/*/c/d/"))
    (equal (parse-namestring-unix "/a/b/c/d/")
           (parse-namestring-unix "/a/**/"))
    (equal (parse-namestring-unix "/a/**/")
           (parse-namestring-unix "/a/**/")))
  nil t nil nil t)

(deftest pathname-equal-windows.1
  (equal
    (parse-namestring-windows "")
    (parse-namestring-windows ""))
  t)

(deftest pathname-equal-windows.2
  (values
    (equal (parse-namestring-windows "/aaa/") (parse-namestring-windows "/aaa/"))
    (equal (parse-namestring-windows "/aaa/") (parse-namestring-windows "/AAA/"))
    (equal (parse-namestring-windows "/aaa/") (parse-namestring-windows "/Aaa/")))
  t t t)

(deftest pathname-equal-windows.3
  (values
    (equal (parse-namestring-windows "aaa/") (parse-namestring-windows "aaa/"))
    (equal (parse-namestring-windows "aaa/") (parse-namestring-windows "AAA/"))
    (equal (parse-namestring-windows "aaa/") (parse-namestring-windows "Aaa/")))
  t t t)

(deftest pathname-equal-windows.4
  (values
    (equal (parse-namestring-windows "/aaa/") (parse-namestring-windows "aaa/"))
    (equal (parse-namestring-windows "/aaa/") (parse-namestring-windows "AAA/"))
    (equal (parse-namestring-windows "/aaa/") (parse-namestring-windows "Aaa/")))
  nil nil nil)

(deftest pathname-equal-windows.5
  (values
    (equal (parse-namestring-windows "aaa") (parse-namestring-windows "aaa"))
    (equal (parse-namestring-windows "aaa") (parse-namestring-windows "AAA"))
    (equal (parse-namestring-windows "aaa") (parse-namestring-windows "Aaa")))
  t t t)

(deftest pathname-equal-windows.6
  (values
    (equal (parse-namestring-windows "z.aaa") (parse-namestring-windows "z.aaa"))
    (equal (parse-namestring-windows "z.aaa") (parse-namestring-windows "z.AAA"))
    (equal (parse-namestring-windows "z.aaa") (parse-namestring-windows "z.Aaa")))
  t t t)

(deftest pathname-equal-windows.7
  (values
    (equal (make-pathname :host 'lisp-system::windows :device "C")
           (make-pathname :host 'lisp-system::windows :device "D"))
    (equal (make-pathname :host 'lisp-system::windows :device "C")
           (make-pathname :host 'lisp-system::windows :device "C"))
    (equal (make-pathname :host 'lisp-system::windows :device "C")
           (make-pathname :host 'lisp-system::windows :device "c")))
  nil t t)

(deftest pathname-equal-windows.8
  (values
    (equal (parse-namestring-windows "C:\\windows\\notepad.exe")
           (parse-namestring-windows "c:\\Windows\\Notepad.EXE"))
    (equal (parse-namestring-windows "C:\\windows\\notepad.exe")
           (parse-namestring-windows "c:Windows\\Notepad.EXE")))
  t nil)

(deftest pathname-equal-windows.9
  (values
    (equal (parse-namestring-windows "hello.txt")
           (parse-namestring-windows "h*o.txt"))
    (equal (parse-namestring-windows "h*o.txt")
           (parse-namestring-windows "h*o.txt"))
    (equal (parse-namestring-windows "\\a\\b\\c\\d\\")
           (parse-namestring-windows "\\a\\*\\c\\d\\"))
    (equal (parse-namestring-windows "\\a\\b\\c\\d\\")
           (parse-namestring-windows "\\a\\**\\"))
    (equal (parse-namestring-windows "\\a\\**\\")
           (parse-namestring-windows "\\a\\**\\")))
  nil t nil nil t)

(deftest pathname-equal-logical.1
  (equal
    (logical-pathname "test:")
    (logical-pathname "test:"))
  t)

(deftest pathname-equal-logical.2
  (values
    (equal (logical-pathname "test:aaa;") (logical-pathname "test:aaa;"))
    (equal (logical-pathname "test:aaa;") (logical-pathname "test:AAA;"))
    (equal (logical-pathname "test:aaa;") (logical-pathname "test:Aaa;")))
  t t t)

(deftest pathname-equal-logical.3
  (values
    (equal (logical-pathname "test:;aaa;") (logical-pathname "test:;aaa;"))
    (equal (logical-pathname "test:;aaa;") (logical-pathname "test:;AAA;"))
    (equal (logical-pathname "test:;aaa;") (logical-pathname "test:;Aaa;")))
  t t t)

(deftest pathname-equal-logical.4
  (values
    (equal (logical-pathname "test:aaa;") (logical-pathname "test:;aaa;"))
    (equal (logical-pathname "test:aaa;") (logical-pathname "test:;AAA;"))
    (equal (logical-pathname "test:aaa;") (logical-pathname "test:;Aaa;")))
  nil nil nil)

(deftest pathname-equal-logical.5
  (values
    (equal (logical-pathname "test:;aaa") (logical-pathname "test:;aaa"))
    (equal (logical-pathname "test:;aaa") (logical-pathname "test:;AAA"))
    (equal (logical-pathname "test:;aaa") (logical-pathname "test:;Aaa")))
  t t t)

(deftest pathname-equal-logical.6
  (values
    (equal (logical-pathname "test:;z.aaa") (logical-pathname "test:;z.aaa"))
    (equal (logical-pathname "test:;z.aaa") (logical-pathname "test:;z.AAA"))
    (equal (logical-pathname "test:;z.aaa") (logical-pathname "test:;z.Aaa")))
  t t t)

(deftest pathname-equal-logical.7
  (values
    (equal (logical-pathname "Test:;z.aaa") (logical-pathname "test:;z.aaa"))
    (equal (logical-pathname "TEST:;z.aaa") (logical-pathname "test:;z.AAA"))
    (equal (logical-pathname "test:;z.aaa") (logical-pathname "test:;z.Aaa")))
  t t t)

(deftest pathname-equal-logical.8
  (values
    (equal (make-pathname :host "test" :name "hello" :version :newest)
           (make-pathname :host "test" :name "hello" :version :newest))
    (equal (make-pathname :host "test" :name "hello" :version 1)
           (make-pathname :host "test" :name "hello" :version :newest))
    (equal (make-pathname :host "test" :name "hello" :version :newest)
           (make-pathname :host "test" :name "hello" :version 2))
    (equal (make-pathname :host "test" :name "hello" :version 3)
           (make-pathname :host "test" :name "hello" :version 3)))
  t nil nil t)

(deftest pathname-equal-logical.9
  (values
    (equal (logical-pathname "test:;hello.txt")
           (logical-pathname "test:;h*o.txt"))
    (equal (logical-pathname "test:;h*o.txt")
           (logical-pathname "test:;h*o.txt"))
    (equal (logical-pathname "test:;a;b;c;d;")
           (logical-pathname "test:;a;*;c;d;"))
    (equal (logical-pathname "test:;a;b;c;d;")
           (logical-pathname "test:;a;**;"))
    (equal (logical-pathname "test:;a;**;")
           (logical-pathname "test:;a;**;")))
  nil t nil nil t)

