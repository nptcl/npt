;;
;;  ANSI COMMON LISP: 20. Files
;;
(defun make-testfile ()
  (flet ((file (x) (with-open-file (output x :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)))
         (path (x) (ensure-directories-exist x)))
    (path #p"test/rt-files/aaa/bbb/ccc/")
    (path #p"test/rt-files/aaa/ddd/")
    (path #p"test/rt-files/aaa/eee/")
    (path #p"test/rt-files/aaa/bbb/")
    (path #p"test/rt-files/aaa/")
    (path #p"test/rt-files/fff/")
    (path #p"test/rt-files/ggg.hhh/")
    (path #p"test/rt-files/iii./")
    (path #p"test/rt-files/.jjj1/")
    (path #p"test/rt-files/.jjj2.jjj3/")
    (path #p"test/rt-files/.jjj4.jjj5.jjj6/")
    (path #p"test/rt-files/")
    (file #p"test/rt-files/fff/kkk")
    (file #p"test/rt-files/fff/kkk1.txt")
    (file #p"test/rt-files/fff/kkk2.txt")
    (file #p"test/rt-files/lll.txt")
    (file #p"test/rt-files/mmm.txt")
    (file #p"test/rt-files/nnn1.doc")
    (file #p"test/rt-files/nnn2.doc")
    (file #p"test/rt-files/nnn3.doc")
    (file #p"test/rt-files/nnn4.nnn5.doc")
    (file #p"test/rt-files/.nnn6")
    (file #p"test/rt-files/.nnn7.doc")
    (file #p"test/rt-files/.nnn8.nnn9.doc")
    (file #p"test/rt-files/ooo1.")
    (file #p"test/rt-files/ooo2.ooo3.")
    (file #p"test/rt-files/.ooo4.")
    (file #p"test/rt-files/.ooo5.ooo6.")
    (file #p"test/rt-files/.ooo7.ooo8.ooo9")
    (file #p"test/rt-files/.oooa.ooob.oooc.")
    (file #p"test/rt-files/ppp")
    (file #p"test/rt-files/qqq")))

(defun delete-testfile ()
  (flet ((file (x) (lisp-system::remove-file x))
         (path (x) (lisp-system::remove-directory x)))
    (file #p"test/rt-files/fff/kkk")
    (file #p"test/rt-files/fff/kkk1.txt")
    (file #p"test/rt-files/fff/kkk2.txt")
    (file #p"test/rt-files/lll.txt")
    (file #p"test/rt-files/mmm.txt")
    (file #p"test/rt-files/nnn1.doc")
    (file #p"test/rt-files/nnn2.doc")
    (file #p"test/rt-files/nnn3.doc")
    (file #p"test/rt-files/nnn4.nnn5.doc")
    (file #p"test/rt-files/.nnn6")
    (file #p"test/rt-files/.nnn7.doc")
    (file #p"test/rt-files/.nnn8.nnn9.doc")
    (file #p"test/rt-files/ooo1.")
    (file #p"test/rt-files/ooo2.ooo3.")
    (file #p"test/rt-files/.ooo4.")
    (file #p"test/rt-files/.ooo5.ooo6.")
    (file #p"test/rt-files/.ooo7.ooo8.ooo9")
    (file #p"test/rt-files/.oooa.ooob.oooc.")
    (file #p"test/rt-files/ppp")
    (file #p"test/rt-files/qqq")
    (path #p"test/rt-files/aaa/bbb/ccc/")
    (path #p"test/rt-files/aaa/ddd/")
    (path #p"test/rt-files/aaa/eee/")
    (path #p"test/rt-files/aaa/bbb/")
    (path #p"test/rt-files/aaa/")
    (path #p"test/rt-files/fff/")
    (path #p"test/rt-files/ggg.hhh/")
    (path #p"test/rt-files/iii./")
    (path #p"test/rt-files/.jjj1/")
    (path #p"test/rt-files/.jjj2.jjj3/")
    (path #p"test/rt-files/.jjj4.jjj5.jjj6/")
    (path #p"test/rt-files/")
    t))
(make-testfile)


;;
;;  Function DIRECTORY
;;
(deftest directory.1
  (null
    (directory #p"./test/*.lisp"))
  nil)

(deftest directory.2
  (pathname-type
    (find "rt-files"
          (directory #p"./test/*.lisp")
          :key #'pathname-name
          :test #'equal))
  "lisp")

(defun directory-list (x)
  (sort
    (mapcar
      (lambda (x)
        (substitute #\/ #\\ (namestring x)))
      (directory x))
    #'string<))

(deftest directory.3
  (directory-list #p"test/rt-files/*.*")
  ("test/rt-files/.jjj1/"
   "test/rt-files/.jjj2.jjj3/"
   "test/rt-files/.jjj4.jjj5.jjj6/"
   "test/rt-files/.nnn6"
   "test/rt-files/.nnn7.doc"
   "test/rt-files/.nnn8.nnn9.doc"
   #-windows "test/rt-files/.ooo4."
   #+windows "test/rt-files/.ooo4"
   #-windows "test/rt-files/.ooo5.ooo6."
   #+windows "test/rt-files/.ooo5.ooo6"
   "test/rt-files/.ooo7.ooo8.ooo9"
   #-windows "test/rt-files/.oooa.ooob.oooc."
   #+windows "test/rt-files/.oooa.ooob.oooc"
   "test/rt-files/aaa/"
   "test/rt-files/fff/"
   "test/rt-files/ggg.hhh/"
   #-windows "test/rt-files/iii./"
   #+windows "test/rt-files/iii/"
   "test/rt-files/lll.txt"
   "test/rt-files/mmm.txt"
   "test/rt-files/nnn1.doc"
   "test/rt-files/nnn2.doc"
   "test/rt-files/nnn3.doc"
   "test/rt-files/nnn4.nnn5.doc"
   #-windows "test/rt-files/ooo1."
   #+windows "test/rt-files/ooo1"
   #-windows "test/rt-files/ooo2.ooo3."
   #+windows "test/rt-files/ooo2.ooo3"
   "test/rt-files/ppp"
   "test/rt-files/qqq"))

(deftest directory.4
  (directory-list #p"test/rt-files/*.")
  #-windows
  ("test/rt-files/.ooo4."
   "test/rt-files/.ooo5.ooo6."
   "test/rt-files/.oooa.ooob.oooc."
   "test/rt-files/iii./"
   "test/rt-files/ooo1."
   "test/rt-files/ooo2.ooo3.")
  #+windows nil)

(deftest directory.5
  (directory-list #p"test/rt-files/*")
  ("test/rt-files/.jjj1/"
   "test/rt-files/.jjj2.jjj3/"
   "test/rt-files/.jjj4.jjj5.jjj6/"
   "test/rt-files/.nnn6"
   "test/rt-files/.nnn7.doc"
   "test/rt-files/.nnn8.nnn9.doc"
   #-windows "test/rt-files/.ooo4."
   #+windows "test/rt-files/.ooo4"
   #-windows "test/rt-files/.ooo5.ooo6."
   #+windows "test/rt-files/.ooo5.ooo6"
   "test/rt-files/.ooo7.ooo8.ooo9"
   #-windows "test/rt-files/.oooa.ooob.oooc."
   #+windows "test/rt-files/.oooa.ooob.oooc"
   "test/rt-files/aaa/"
   "test/rt-files/fff/"
   "test/rt-files/ggg.hhh/"
   #-windows "test/rt-files/iii./"
   #+windows "test/rt-files/iii/"
   "test/rt-files/lll.txt"
   "test/rt-files/mmm.txt"
   "test/rt-files/nnn1.doc"
   "test/rt-files/nnn2.doc"
   "test/rt-files/nnn3.doc"
   "test/rt-files/nnn4.nnn5.doc"
   #-windows "test/rt-files/ooo1."
   #+windows "test/rt-files/ooo1"
   #-windows "test/rt-files/ooo2.ooo3."
   #+windows "test/rt-files/ooo2.ooo3"
   "test/rt-files/ppp"
   "test/rt-files/qqq"))

(deftest directory.6
  (directory-list #p"test/rt-files/*.doc")
  ("test/rt-files/.nnn7.doc"
   "test/rt-files/.nnn8.nnn9.doc"
   "test/rt-files/nnn1.doc"
   "test/rt-files/nnn2.doc"
   "test/rt-files/nnn3.doc"
   "test/rt-files/nnn4.nnn5.doc"))

(deftest directory.7
  (directory-list #p"test/rt-files/**/*.txt")
  ("test/rt-files/fff/kkk1.txt"
   "test/rt-files/fff/kkk2.txt"
   "test/rt-files/lll.txt"
   "test/rt-files/mmm.txt"))

(deftest directory.8
  (directory-list #p"test/rt-files/**/*.*")
  ("test/rt-files/.jjj1/"
   "test/rt-files/.jjj2.jjj3/"
   "test/rt-files/.jjj4.jjj5.jjj6/"
   "test/rt-files/.nnn6"
   "test/rt-files/.nnn7.doc"
   "test/rt-files/.nnn8.nnn9.doc"
   #-windows "test/rt-files/.ooo4."
   #+windows "test/rt-files/.ooo4"
   #-windows "test/rt-files/.ooo5.ooo6."
   #+windows "test/rt-files/.ooo5.ooo6"
   "test/rt-files/.ooo7.ooo8.ooo9"
   #-windows "test/rt-files/.oooa.ooob.oooc."
   #+windows "test/rt-files/.oooa.ooob.oooc"
   "test/rt-files/aaa/"
   "test/rt-files/aaa/bbb/"
   "test/rt-files/aaa/bbb/ccc/"
   "test/rt-files/aaa/ddd/"
   "test/rt-files/aaa/eee/"
   "test/rt-files/fff/"
   "test/rt-files/fff/kkk"
   "test/rt-files/fff/kkk1.txt"
   "test/rt-files/fff/kkk2.txt"
   "test/rt-files/ggg.hhh/"
   #-windows "test/rt-files/iii./"
   #+windows "test/rt-files/iii/"
   "test/rt-files/lll.txt"
   "test/rt-files/mmm.txt"
   "test/rt-files/nnn1.doc"
   "test/rt-files/nnn2.doc"
   "test/rt-files/nnn3.doc"
   "test/rt-files/nnn4.nnn5.doc"
   #-windows "test/rt-files/ooo1."
   #+windows "test/rt-files/ooo1"
   #-windows "test/rt-files/ooo2.ooo3."
   #+windows "test/rt-files/ooo2.ooo3"
   "test/rt-files/ppp"
   "test/rt-files/qqq"))

(deftest directory.9
  (directory #p"/no/such/path/name/*.*")
  nil)

(deftest directory-close.1
  (delete-testfile)
  t)

(deftest-error directory-error.1
  (eval '(directory 10))
  type-error)

(deftest-error! directory-error.2
  (eval '(directory #p"." nil)))

(deftest-error! directory-error.3
  (eval '(directory)))


;;
;;  Function PROBE-FILE
;;
(deftest probe-file.1
  (car (pathname-directory
         (probe-file #p"test/rt-files.lisp")))
  :absolute)

(deftest probe-file.2
  (probe-file #p"test/no-such-file")
  nil)

(deftest probe-file.3
  (car (pathname-directory
         (probe-file #p"./")))
  :absolute)

(deftest probe-file.4
  (let ((x (open #p"_debug_probe.txt" :direction :output
                 :if-exists :supersede :if-does-not-exist :create)))
    (unwind-protect
      (car (pathname-directory (probe-file x)))
      (close x :abort t)))
  :absolute)

(deftest probe-file.5
  (let ((x (open #p"_debug_probe.txt" :direction :output
                 :if-exists :supersede :if-does-not-exist :create)))
    (close x :abort t)
    (probe-file x))
  nil)

(deftest probe-file.6
  (let ((x (open #p"_debug_probe.txt" :direction :output
                 :if-exists :supersede :if-does-not-exist :create)))
    (close x)
    (prog1 (car (pathname-directory (probe-file x)))
      (delete-file x)))
  :absolute)

(deftest-error probe-file-error.1
  (probe-file #p"./*.*")
  file-error)

(deftest-error probe-file-error.2
  (eval '(probe-file 10))
  type-error)

(deftest-error! probe-file-error.3
  (eval '(probe-file)))

(deftest-error! probe-file-error.4
  (eval '(probe-file #p"./" nil)))


;;
;;
;;
(deftest truename.1
  (car (pathname-directory
         (truename #p"test/rt-files.lisp")))
  :absolute)

#-windows
(deftest-error truename.2
  (truename #p"no-such-directory/no-such-file")
  file-error)

#+windows
(deftest truename.2
  (car (pathname-directory
         (truename #p"no-such-directory/no-such-file")))
  :absolute)

(deftest truename.3
  (car (pathname-directory
         (truename #p"./")))
  :absolute)

(deftest truename.4
  (let ((x (open #p"_debug_probe.txt" :direction :output
                 :if-exists :supersede :if-does-not-exist :create)))
    (unwind-protect
      (car (pathname-directory (truename x)))
      (close x :abort t)))
  :absolute)

#-windows
(deftest-error truename.5
  (let ((x (open #p"_debug_probe.txt" :direction :output
                 :if-exists :supersede :if-does-not-exist :create)))
    (close x :abort t)
    (truename x))
  file-error)

#+windows
(deftest truename.5
  (let ((x (open #p"_debug_probe.txt" :direction :output
                 :if-exists :supersede :if-does-not-exist :create)))
    (close x :abort t)
    (car (pathname-directory
           (truename x))))
  :absolute)

(deftest truename.6
  (let ((x (open #p"_debug_probe.txt" :direction :output
                 :if-exists :supersede :if-does-not-exist :create)))
    (close x)
    (prog1 (car (pathname-directory (truename x)))
      (delete-file x)))
  :absolute)

#+unix
(deftest-error truename.7
  (truename #p"no-such/file-name-truename-error.txt"))

(deftest-error truename-error.1
  (truename #p"./*.*")
  file-error)

(deftest-error truename-error.2
  (eval '(truename 10))
  type-error)

(deftest-error! truename-error.3
  (eval '(truename)))

(deftest-error! truename-error.4
  (eval '(truename #p"./" nil)))



;;
;;
;;
(deftest file-author.1
  (stringp
    (file-author #p"test/rt-files.lisp"))
  t)

(deftest file-write-date.1
  (integerp
    (file-write-date #p"test/rt-files.lisp"))
  t)

(deftest-error file-write-date.2
  (file-write-date #p"test/no-such-file"))

(defvar *rename-file1* #p"_debug1.txt")
(defvar *rename-file2* #p"_debug2.txt")

(defun probe-delete-file (file)
  (if (probe-file file)
    (delete-file file)))

(defun probe-file-boolean (file)
  (not (not (probe-file file))))

(defun write-string-file (file value)
  (with-open-file (output file :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (princ value output)))

(deftest-error rename-file.1
  (progn
    (probe-delete-file *rename-file1*)
    (probe-delete-file *rename-file2*)
    (rename-file *rename-file1* *rename-file2*)))

(deftest-error rename-file.2
  (progn
    (write-string-file *rename-file1* "aaa")
    (write-string-file *rename-file2* "bbb")
    (rename-file *rename-file1* *rename-file2*)))

(deftest rename-file.3
  (progn
    (probe-delete-file *rename-file1*)
    (probe-delete-file *rename-file2*)
    (write-string-file *rename-file1* "aaa")
    (multiple-value-bind (a b c)
      (rename-file *rename-file1* *rename-file2*)
      (values
        (probe-file-boolean a)
        (probe-file-boolean b)
        (probe-file-boolean c)
        (car (pathname-directory b))
        (car (pathname-directory c)))))
  t nil t :absolute :absolute)

(deftest-error delete-file.1
  (progn
    (probe-delete-file *rename-file1*)
    (probe-delete-file *rename-file2*)
    (delete-file *rename-file1*)))

(deftest delete-file.2
  (progn
    (probe-delete-file *rename-file1*)
    (write-string-file *rename-file1* "aaa")
    (values
      (delete-file *rename-file1*)
      (probe-file-boolean *rename-file1*)))
  t nil)

