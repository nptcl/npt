;;
;;  ANSI COMMON LISP: 20. Files
;;
(deftest probe-file.1
  (probe-file #p"test/rt-files.lisp")
  t)

(deftest probe-file.2
  (probe-file #p"test/no-such-file")
  nil)

(deftest-error truename.1
  (truename #p"no-such/file-name-truename-error.txt"))

(deftest file-error-pathname.1
  (handler-case
    (error (make-condition 'file-error :pathname #p"hello.txt"))
    (file-error (c) (file-error-pathname c)))
  #p"hello.txt")

