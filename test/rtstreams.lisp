;;
;;  ANSI COMMON LISP: 21. Streams
;;
(defvar *file* #p"_rt-stream.txt")
(defvar *file1* #p"_rt-stream1.txt")
(defvar *file2* #p"_rt-stream2.txt")

(defmacro with-extend-to-string ((var array) &body body)
  `(let ((,array (make-array
                   10 :fill-pointer 0 :adjustable t :element-type 'character)))
     (declare (ignorable ,array))
     (with-output-to-string (,var ,array)
       ,@body)))

#-ansi-c
(defun test-delete-file (file)
  (delete-file file))

#+ansi-c
(defun test-delete-file (file)
  (declare (ignore file)))

(defun delete-temp-file ()
  (when (probe-file *file*)
    (test-delete-file *file*)))

(defmacro with-delete-temp-file (&body body)
  `(progn
     (delete-temp-file)
     ,@body))

(defun make-temp-file (file string)
  (with-open-file (output file :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format output string)))

(defmacro with-make-file ((file string) &body body)
  `(unwind-protect
     (progn
       (make-temp-file ,file ,string)
       ,@body)
     (test-delete-file ,file)))

(defmacro with-temp-file (&body body)
  `(with-make-file (*file* "ABC") ,@body))

(defmacro with-temp-file1 (&body body)
  `(with-make-file (*file1* "ABC") ,@body))

(defmacro with-temp-file2 (&body body)
  `(with-make-file (*file2* "ABC") ,@body))

(defmacro with-temp-file1-file2 (&body body)
  `(with-temp-file1
     (with-temp-file2
       ,@body)))

(defmacro with-binary-output ((var file) &body body)
  `(with-open-file (,var ,file :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type 'unsigned-byte)
     ,@body))

(defmacro with-overwrite-file ((var file &rest args) &body body)
  `(with-open-file (,var ,file ,@args :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
     ,@body))

