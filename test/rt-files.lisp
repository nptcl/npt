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

(deftest truename.1
  (truename #p"aaa/hello.txt")
  #p"aaa/hello.txt")


;;
;;  do-tests
;;
(do-tests :test t)

