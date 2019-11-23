;;
;;  ANSI COMMON LISP: 20. Files
;;
#-ansi-c
(deftest directory.1
  (null
    (directory #p"./test/*.lisp"))
  nil)

#-ansi-c
(deftest directory.2
  (pathname-type
    (find "rt-files"
          (directory #p"./test/*.lisp")
          :key #'pathname-name
          :test #'equal))
  "lisp")

(deftest probe-file.1
  (probe-file #p"test/rt-files.lisp")
  t)

(deftest probe-file.2
  (probe-file #p"test/no-such-file")
  nil)

#-ansi-c
(deftest probe-file.3
  (probe-file #p"./")
  t)

#+unix
(deftest truename.1
  (car (pathname-directory
         (truename #p"test/rt-files.lisp")))
  :absolute)

#+windows
(deftest truename.1
  (car (pathname-directory
         (truename #p"test/rt-files.lisp")))
  :absolute)

#+ansi-c
(deftest-error truename.2
  (truename #p"no-such/file-name-truename-error.txt"))

#+unix
(deftest-error truename.2
  (truename #p"no-such/file-name-truename-error.txt"))

#-ansi-c
(deftest file-author.1
  (stringp
    (file-author #p"test/rt-files.lisp"))
  t)

#-ansi-c
(deftest file-write-date.1
  (integerp
    (file-write-date #p"test/rt-files.lisp"))
  t)

#-ansi-c
(deftest-error file-write-date.2
  (file-write-date #p"test/no-such-file"))

(defvar *rename-file1* #p"_debug1.txt")
(defvar *rename-file2* #p"_debug2.txt")

(defun probe-delete-file (file)
  (if (probe-file file)
    (delete-file file)))

(defun write-string-file (file value)
  (with-open-file (output file :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (princ value output)))

#-ansi-c
(deftest-error rename-file.1
  (progn
    (probe-delete-file *rename-file1*)
    (probe-delete-file *rename-file2*)
    (rename-file *rename-file1* *rename-file2*)))

#-ansi-c
(deftest-error rename-file.2
  (progn
    (write-string-file *rename-file1* "aaa")
    (write-string-file *rename-file2* "bbb")
    (rename-file *rename-file1* *rename-file2*)))

#-ansi-c
(deftest rename-file.3
  (progn
    (probe-delete-file *rename-file1*)
    (probe-delete-file *rename-file2*)
    (write-string-file *rename-file1* "aaa")
    (multiple-value-bind (a b c)
      (rename-file *rename-file1* *rename-file2*)
      (values
        (probe-file a)
        (probe-file b)
        (probe-file c)
        (car (pathname-directory b))
        (car (pathname-directory c)))))
  t nil t :absolute :absolute)

#-ansi-c
(deftest-error delete-file.1
  (progn
    (probe-delete-file *rename-file1*)
    (probe-delete-file *rename-file2*)
    (delete-file *rename-file1*)))

#-ansi-c
(deftest delete-file.2
  (progn
    (probe-delete-file *rename-file1*)
    (write-string-file *rename-file1* "aaa")
    (values
      (delete-file *rename-file1*)
      (probe-file *rename-file1*)))
  t nil)

(deftest file-error-pathname.1
  (handler-case
    (error (make-condition 'file-error :pathname #p"hello.txt"))
    (file-error (c) (file-error-pathname c)))
  #p"hello.txt")


;;
;;  do-tests
;;
(do-tests :test t)

