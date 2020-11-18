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
;;  Function ENSURE-DIRECTORIES-EXIST
;;
(deftest ensure-directories-exist.1
  (let ((path #p"test/ensure-directories-exists-1/"))
    (lisp-system:remove-directory path nil)
    (multiple-value-bind (x y) (ensure-directories-exist path)
      (values (pathnamep x) y)))
  t t)

(deftest ensure-directories-exist.2
  (let ((path #p"test/ensure-directories-exists-2/"))
    (lisp-system:remove-directory path nil)
    (ensure-directories-exist path)
    (multiple-value-bind (x y) (ensure-directories-exist path)
      (values (pathnamep x) y)))
  t nil)

(deftest ensure-directories-exist.3
  (let ((path #p"test/ensure-directories-exists-3/hello.txt"))
    (multiple-value-bind (x y) (ensure-directories-exist path)
      (values (pathnamep x) y)))
  t t)

(deftest ensure-directories-exist.4
  (probe-file #p"test/ensure-directories-exists-3/hello.txt")
  nil)

(deftest ensure-directories-exist.5
  (let ((path #p"test/ensure-directories-exists-5/"))
    (lisp-system:remove-directory path nil)
    (equalp
      (with-output-to-string (*standard-output*)
        (ensure-directories-exist path :verbose t))
      ""))
  nil)

(deftest ensure-directories-exist.6
  (let ((path #p"test/ensure-directories-exists-6/"))
    (lisp-system:remove-directory path nil)
    (ensure-directories-exist path)
    (equalp
      (with-output-to-string (*standard-output*)
        (ensure-directories-exist path :verbose t))
      ""))
  t)

(deftest ensure-directories-exist.7
  (let ((path #p"test/ensure-directories-exists-7/"))
    (lisp-system:remove-directory path nil)
    (equalp
      (with-output-to-string (*standard-output*)
        (ensure-directories-exist path))
      ""))
  t)

(deftest ensure-directories-exist.8
  (progn
    (lisp-system:remove-directory
      #p"test/ensure-directories-exists-8/" nil)
    (multiple-value-bind (x y)
      (ensure-directories-exist #p"test/ensure-directories-exists-8/*.*")
      (values (pathnamep x) y)))
  t t)

(deftest-error ensure-directories-exist.9
  (ensure-directories-exist #p"test/*/ensure-directories-exist-9/")
  file-error)

(deftest ensure-directories-exist-delete.1
  (flet ((path (x) (lisp-system:remove-directory x nil)))
    (path #p"test/ensure-directories-exists-1/")
    (path #p"test/ensure-directories-exists-2/")
    (path #p"test/ensure-directories-exists-3/")
    (path #p"test/ensure-directories-exists-4/")
    (path #p"test/ensure-directories-exists-5/")
    (path #p"test/ensure-directories-exists-6/")
    (path #p"test/ensure-directories-exists-7/")
    (path #p"test/ensure-directories-exists-8/")
    (path #p"test/ensure-directories-exists-9/")
    (values)))

(deftest-error ensure-directories-exist-error.1
  (eval '(ensure-directories-exist 10))
  type-error)

(deftest-error ensure-directories-exist-error.2
  (eval '(ensure-directories-exist
           #p"test/ensure-directories-exists-error" :hello)))

(deftest-error ensure-directories-exist-error.3
  (eval '(ensure-directories-exist
           #p"test/ensure-directories-exists-error" :hello 10)))

(deftest-error ensure-directories-exist-error.4
  (eval '(ensure-directories-exist
           #p"test/ensure-directories-exists-error" :verbose)))

(deftest-error! ensure-directories-exist-error.5
  (eval '(ensure-directories-exist)))


;;
;;  Function TRUENAME
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
;;  Function FILE-AUTHOR
;;
(deftest file-author.1
  (stringp
    (file-author #p"test/rt-files.lisp"))
  t)

(deftest file-author.2
  (let ((stream (open #p"test/file-author.txt" :direction :output
                      :if-exists :supersede :if-does-not-exist :create)))
    (unwind-protect
      (stringp (file-author stream))
      (close stream :abort t)))
  t)

#-windows
(deftest-error file-author.3
  (file-author #p"test/no-such-file")
  file-error)

#+windows
(deftest file-author.3
  (file-author #p"test/no-such-file")
  nil)

(deftest-error file-author.4
  (file-author #p"test/*.*")
  file-error)

(deftest-error file-author-error.1
  (eval '(file-author 10))
  type-error)

(deftest-error! file-author-error.2
  (eval '(file-author)))

(deftest-error! file-author-error.3
  (eval '(file-author #p"test/rt-files.lisp" nil)))


;;
;;  Function FILE-WRITE-DATE
;;
(deftest file-write-date.1
  (integerp
    (file-write-date #p"test/rt-files.lisp"))
  t)

(deftest file-write-date.2
  (let ((stream (open #p"test/file-write-date.txt" :direction :output
                      :if-exists :supersede :if-does-not-exist :create)))
    (unwind-protect
      (integerp (file-write-date stream))
      (close stream :abort t)))
  t)

(deftest-error file-write-date.3
  (file-write-date #p"test/no-such-file")
  file-error)

(deftest-error file-write-date.4
  (file-write-date #p"test/*.*")
  file-error)

(deftest-error file-write-date-error.1
  (eval '(file-write-date 10))
  type-error)

(deftest-error! file-write-date-error.2
  (eval '(file-write-date)))

(deftest-error! file-write-date-error.3
  (eval '(file-write-date #p"test/rt-files.lisp" nil)))


;;
;;  Function RENAME-FILE
;;
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
    (rename-file *rename-file1* *rename-file2*))
  file-error)

(deftest-error rename-file.2
  (progn
    (write-string-file *rename-file1* "aaa")
    (write-string-file *rename-file2* "bbb")
    (rename-file *rename-file1* *rename-file2*))
  file-error)

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

(deftest-error rename-file.4
  (rename-file #p"_debug3.*" *rename-file2*)
  file-error)

(deftest-error rename-file.5
  (rename-file *rename-file1* #p"_debug3.*")
  file-error)

(deftest-error rename-file-error.1
  (eval '(rename-file 10 *rename-file2*))
  type-error)

(deftest-error rename-file-error.2
  (eval '(rename-file *rename-file1* 20))
  type-error)

(deftest-error! rename-file-error.3
  (eval '(rename-file *rename-file1*)))

(deftest-error! rename-file-error.4
  (eval '(rename-file *rename-file1* *rename-file2* nil)))


;;
;;  Function DELETE-FILE
;;
(deftest-error delete-file.1
  (progn
    (probe-delete-file *rename-file1*)
    (probe-delete-file *rename-file2*)
    (delete-file *rename-file1*))
  file-error)

(deftest delete-file.2
  (progn
    (probe-delete-file *rename-file1*)
    (write-string-file *rename-file1* "aaa")
    (values
      (delete-file *rename-file1*)
      (probe-file-boolean *rename-file1*)))
  t nil)

(deftest-error delete-file.3
  (delete-file #p"_debug.*")
  file-error)

(deftest-error delete-file-error.1
  (eval '(delete-file 10))
  type-error)

(deftest-error! delete-file-error.2
  (eval '(delete-file)))

(deftest-error! delete-file-error.3
  (eval '(delete-file *rename-file1* nil)))

