;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  open-io
;;
(deftest open-io.1
  (progn
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :io))
      (streamp x)))
  t)

(deftest open-io.2
  (with-open-stream (io (make-memory-io-stream))
    (with-open-stream (x (open io :direction :io))
      (streamp x)))
  t)

(deftest open-io.3
  (progn
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :io))
      (write-char #\X x)
      (write-char #\Y x)
      (write-char #\Z x))
    (read-line-1))
  "XYZ")

(deftest open-io.4
  (with-open-stream (io (make-memory-io-stream))
    (with-open-stream (x (open io :direction :io))
      (write-char #\X x)
      (write-char #\Y x)
      (write-char #\Z x))
    (read-line-1 io))
  "XYZ")

;;  if-exists
(deftest open-io-if-exists-error.1
  (progn
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :io :if-exists :error))
      (streamp x))
    (probe-file *file*))
  t)

(deftest-error open-io-if-exists-error.2
  (with-temp-file
    (with-open-stream (x (open *file* :direction :io :if-exists :error))
      (streamp x)))
  file-error)

(deftest-error open-io-if-exists-error.3
  (with-open-stream (io (make-memory-io-stream))
    (with-open-stream (x (open io :direction :io :if-exists :error))
      (streamp x)))
  file-error)


;;  :new-version is equivalent to :supersede.
(deftest open-io-if-exists-new-version.1
  (with-temp-file
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :io :if-exists :new-version))
      (streamp x))
    (with-open-file (x *file*)
      (read-char x nil nil)))
  nil)

(deftest open-io-if-exists-new-version.2
  (with-temp-file
    (make-temp-file *file* "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open *file* :direction :io :if-exists :new-version))
      (streamp x))
    (with-open-file (x *file*)
      (read-char x nil nil)))
  nil)

(deftest open-io-if-exists-new-version.3
  (with-open-stream (io (make-memory-io-stream))
    (make-temp-file io "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open io :direction :io :if-exists :new-version))
      (streamp x))
    (with-open-file (x io)
      (read-char x nil nil)))
  nil)

(deftest open-io-if-exists-new-version.4
  (with-temp-file
    (make-temp-file *file* "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open *file* :direction :io :if-exists :new-version))
      (format x "Hello"))
    (read-line-1))
  "Hello")

(deftest open-io-if-exists-new-version.5
  (with-open-stream (io (make-memory-io-stream))
    (make-temp-file io "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open io :direction :io :if-exists :new-version))
      (format x "Hello"))
    (read-line-1 io))
  "Hello")


;;  :rename
(deftest open-io-if-exists-rename.1
  (with-temp-file
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :io :if-exists :rename))
      (streamp x))
    (with-open-file (x *file*)
      (read-char x nil nil)))
  nil)

(deftest open-io-if-exists-rename.2
  (let ((name #p"open-rename.txt")
        (name0 #p"open-rename.txt.0")
        (name1 #p"open-rename.txt.1"))
    (delete-probe-file name name0 name1)
    (make-temp-file name "AAAAA")
    (unwind-protect
      (with-open-stream (x (open name :direction :io :if-exists :rename))
        (namestring x))
      (delete-probe-file name name0 name1)))
  "open-rename.txt")

(deftest open-io-if-exists-rename.3
  (let ((name #p"open-rename.txt")
        (name0 #p"open-rename.txt.0")
        (name1 #p"open-rename.txt.1"))
    (delete-probe-file name name0 name1)
    (make-temp-file name "AAAAA")
    (unwind-protect
      (progn
        (with-open-stream (x (open name :direction :io :if-exists :rename))
          (format x "Hello"))
        (values
          (read-line-1 name)
          (read-line-1 name0)))
      (delete-probe-file name name0 name1)))
  "Hello" "AAAAA")

(deftest open-io-if-exists-rename.4
  (let ((name #p"open-rename.txt")
        (name0 #p"open-rename.txt.0")
        (name1 #p"open-rename.txt.1"))
    (delete-probe-file name name0 name1)
    (make-temp-file name "AAA")
    (make-temp-file name0 "BBB")
    (unwind-protect
      (progn
        (with-open-stream (x (open name :direction :io :if-exists :rename))
          (format x "Hello"))
        (values
          (read-line-1 name)
          (read-line-1 name0)
          (read-line-1 name1)))
      (delete-probe-file name name0 name1)))
  "Hello" "BBB" "AAA")

(deftest open-io-if-exists-rename.5
  (with-open-stream (io (make-memory-io-stream))
    (with-open-stream (x (open io :direction :io :if-exists :rename))
      (format x "Hello"))
    (read-line-1 io))
  "Hello")


;;  The operating system does not distinguish the notions of deletion and expunging.
;;  Then :rename-and-delete is equivalent to :supersede.
(deftest open-io-if-exists-rename-and-delete.1
  (with-temp-file
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :io
                               :if-exists :rename-and-delete))
      (streamp x))
    (with-open-file (x *file*)
      (read-char x nil nil)))
  nil)

(deftest open-io-if-exists-rename-and-delete.2
  (with-temp-file
    (make-temp-file *file* "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open *file* :direction :io
                               :if-exists :rename-and-delete))
      (streamp x))
    (with-open-file (x *file*)
      (read-char x nil nil)))
  nil)

(deftest open-io-if-exists-rename-and-delete.3
  (with-open-stream (io (make-memory-io-stream))
    (make-temp-file io "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open io :direction :io
                               :if-exists :rename-and-delete))
      (streamp x))
    (with-open-file (x io)
      (read-char x nil nil)))
  nil)

(deftest open-io-if-exists-rename-and-delete.4
  (with-temp-file
    (make-temp-file *file* "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open *file* :direction :io
                               :if-exists :rename-and-delete))
      (format x "Hello"))
    (read-line-1))
  "Hello")

(deftest open-io-if-exists-rename-and-delete.5
  (with-open-stream (io (make-memory-io-stream))
    (make-temp-file io "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open io :direction :io
                               :if-exists :rename-and-delete))
      (format x "Hello"))
    (read-line-1 io))
  "Hello")


;;  :overwrite
(deftest-error open-io-if-exists-overwrite.1
  (with-temp-file
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :io :if-exists :overwrite))
      (streamp x)))
  file-error)

(deftest open-io-if-exists-overwrite.2
  (with-temp-file
    (make-temp-file *file* "ABCDEFG")
    (with-open-stream (x (open *file* :direction :io :if-exists :overwrite))
      (streamp x)))
  t)

(deftest open-io-if-exists-overwrite.3
  (with-open-stream (io (make-memory-io-stream))
    (make-temp-file io "ABCDEFG")
    (with-open-stream (x (open io :direction :io :if-exists :overwrite))
      (streamp x)))
  t)

(deftest open-io-if-exists-overwrite.4
  (with-temp-file
    (make-temp-file *file* "ABCDEFG")
    (with-open-stream (x (open *file* :direction :io :if-exists :overwrite))
      (format x "Hello"))
    (read-line-1))
  "HelloFG")

(deftest open-io-if-exists-overwrite.5
  (with-open-stream (io (make-memory-io-stream))
    (make-temp-file io "ABCDEFG")
    (with-open-stream (x (open io :direction :io :if-exists :overwrite))
      (format x "Hello"))
    (read-line-1 io))
  "HelloFG")


;;  :append
(deftest-error open-io-if-exists-append.1
  (with-temp-file
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :io :if-exists :append))
      (streamp x)))
  file-error)

(deftest open-io-if-exists-append.2
  (with-temp-file
    (make-temp-file *file* "ABCDEFG")
    (with-open-stream (x (open *file* :direction :io :if-exists :append))
      (streamp x)))
  t)

(deftest open-io-if-exists-append.3
  (with-open-stream (io (make-memory-io-stream))
    (make-temp-file io "ABCDEFG")
    (with-open-stream (x (open io :direction :io :if-exists :append))
      (streamp x)))
  t)

(deftest open-io-if-exists-append.4
  (with-temp-file
    (make-temp-file *file* "ABCDEFG")
    (with-open-stream (x (open *file* :direction :io :if-exists :append))
      (format x "Hello"))
    (read-line-1))
  "ABCDEFGHello")

(deftest open-io-if-exists-append.5
  (with-open-stream (io (make-memory-io-stream))
    (make-temp-file io "ABCDEFG")
    (with-open-stream (x (open io :direction :io :if-exists :append))
      (format x "Hello"))
    (read-line-1 io))
  "ABCDEFGHello")

(deftest open-io-if-exists-append.6
  (with-temp-file
    (make-temp-file *file* "ABCDEFG")
    (with-open-stream (x (open *file* :direction :io :if-exists :append))
      (format x "Hello")
      (file-position x :start)
      (values
        (read-line x))))
  "ABCDEFGHello")

(deftest open-io-if-exists-append.7
  (with-open-stream (io (make-memory-io-stream))
    (make-temp-file io "ABCDEFG")
    (with-open-stream (x (open io :direction :io :if-exists :append))
      (format x "Hello")
      (file-position x :start)
      (values
        (read-line x))))
  "ABCDEFGHello")


;;  :supersede
(deftest open-io-if-exists-supersede.1
  (with-temp-file
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :io :if-exists :supersede))
      (streamp x))
    (with-open-file (x *file*)
      (read-char x nil nil)))
  nil)

(deftest open-io-if-exists-supersede.2
  (with-temp-file
    (make-temp-file *file* "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open *file* :direction :io :if-exists :supersede))
      (streamp x))
    (with-open-file (x *file*)
      (read-char x nil nil)))
  nil)

(deftest open-io-if-exists-supersede.3
  (with-open-stream (io (make-memory-io-stream))
    (make-temp-file io "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open io :direction :io :if-exists :supersede))
      (streamp x))
    (with-open-file (x io)
      (read-char x nil nil)))
  nil)

(deftest open-io-if-exists-supersede.4
  (with-temp-file
    (make-temp-file *file* "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open *file* :direction :io :if-exists :supersede))
      (format x "Hello"))
    (read-line-1))
  "Hello")

(deftest open-io-if-exists-supersede.5
  (with-open-stream (io (make-memory-io-stream))
    (make-temp-file io "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open io :direction :io :if-exists :supersede))
      (format x "Hello"))
    (read-line-1 io))
  "Hello")

(deftest open-io-if-exists-supersede.6
  (with-temp-file
    (make-temp-file *file* "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open *file* :direction :io :if-exists :supersede))
      (format x "Hello")
      (file-position x :start)
      (values
        (read-line x))))
  "Hello")

(deftest open-io-if-exists-supersede.7
  (with-open-stream (io (make-memory-io-stream))
    (make-temp-file io "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    (with-open-stream (x (open io :direction :io :if-exists :supersede))
      (format x "Hello")
      (file-position x :start)
      (values
        (read-line x))))
  "Hello")


;;  nil
(deftest open-io-if-exists-nil.1
  (with-temp-file
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :io :if-exists nil))
      (streamp x)))
  t)

(deftest open-io-if-exists-nil.2
  (with-temp-file
    (open *file* :direction :io :if-exists nil))
  nil)

(deftest open-io-if-exists-nil.3
  (with-open-stream (io (make-memory-io-stream))
    (open io :direction :io :if-exists nil))
  nil)

