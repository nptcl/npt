;;
;;  ANSI COMMON LISP: 20. Files
;;
'(deftest directory.1
   (directory #p"./test/")
   nil)

#-ansi-c
(deftest probe-file.1
  (probe-file #p"test/rt-files.lisp")
  t)

#-ansi-c
(deftest probe-file.2
  (probe-file #p"test/no-such-file")
  nil)

#-ansi-c
(deftest probe-file.3
  (probe-file #p"./")
  t)

#+unix
(deftest-error truename-error-posix.1
  (truename #p"no-such/file-name-truename-error.txt"))

#+unix
(deftest truename-error-posix.2
  (char
    (namestring
      (truename #p"test/rt-files.lisp"))
    0)
  #\/)  ;; absolute pathname

#+windows
(deftest truename-error-windows.1
  (subseq
    (namestring
      (truename #p"test/rt-files.lisp"))
    1 3)
  ":\\")  ;; absolute pathname


;;
;;  do-tests
;;
(do-tests :test t)

