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


#|
;; The results of the following five forms are all implementation-dependent.
;; The second item in particular is shown with multiple results just to
;; emphasize one of many particular variations which commonly occurs.
(deftest translate-pathname-test.1
  (pathname-name (translate-pathname "foobar" "foo*" "*baz"))
  "barbaz")

(pathname-name (translate-pathname "foobar" "foo*" "*"))
=>  "foobar"
OR=>  "bar"
(pathname-name (translate-pathname "foobar" "*"    "foo*")) =>  "foofoobar"
(pathname-name (translate-pathname "bar"    "*"    "foo*")) =>  "foobar"
(pathname-name (translate-pathname "foobar" "foo*" "baz*")) =>  "bazbar"

(defun translate-logical-pathname-1 (pathname rules)
  (let ((rule (assoc pathname rules :test #'pathname-match-p)))
    (unless rule (error "No translation rule for ~A" pathname))
    (translate-pathname pathname (first rule) (second rule))))
(translate-logical-pathname-1 "FOO:CODE;BASIC.LISP"
                              '(("FOO:DOCUMENTATION;" "MY-UNIX:/doc/foo/")
                                ("FOO:CODE;"          "MY-UNIX:/lib/foo/")
                                ("FOO:PATCHES;*;"     "MY-UNIX:/lib/foo/patch/*/")))
=>  #P"MY-UNIX:/lib/foo/basic.l"

;; This example assumes one particular set of wildcard conventions
;; Not all file systems will run this example exactly as written
(defun rename-files (from to)
  (dolist (file (directory from))
    (rename-file file (translate-pathname file from to))))
(rename-files "/usr/me/*.lisp" "/dev/her/*.l")
;Renames /usr/me/init.lisp to /dev/her/init.l
(rename-files "/usr/me/pcl*/*" "/sys/pcl/*/")
;Renames /usr/me/pcl-5-may/low.lisp to /sys/pcl/pcl-5-may/low.lisp
;In some file systems the result might be /sys/pcl/5-may/low.lisp
(rename-files "/usr/me/pcl*/*" "/sys/library/*/")
;Renames /usr/me/pcl-5-may/low.lisp to /sys/library/pcl-5-may/low.lisp
;In some file systems the result might be /sys/library/5-may/low.lisp
(rename-files "/usr/me/foo.bar" "/usr/me2/")
;Renames /usr/me/foo.bar to /usr/me2/foo.bar
(rename-files "/usr/joe/*-recipes.text" "/usr/jim/cookbook/joe's-*-rec.text")
;Renames /usr/joe/lamb-recipes.text to /usr/jim/cookbook/joe's-lamb-rec.text
;Renames /usr/joe/pork-recipes.text to /usr/jim/cookbook/joe's-pork-rec.text
;Renames /usr/joe/veg-recipes.text to /usr/jim/cookbook/joe's-veg-rec.text
|#

