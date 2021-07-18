(defpackage make-filelist-lisp (:use cl))
(in-package make-filelist-lisp)

(defparameter +filelist+       #p"filelist")
(defparameter +filetest+       #p"filelist.test")
(defparameter +source-file+    #p"source.mk")
(defparameter +release-file+   #p"release.mk")
(defparameter +extension+      #p"EXTENSION")
(defvar *list*)
(defvar *test*)
(defvar *ext*)

;; input
(defun read-list (input)
  (do (x list) (nil)
    (setq x (read-line input nil nil))
    (unless x
      (return (nreverse list)))
    (push (string-trim '(#\space #\tab) x) list)))

(defun read-list-remove (input)
  (remove-if
    (lambda (x)
      (let ((len (length x)))
        (or (= len 0)
            (and (<= 1 len)
                 (char= (char x 0) #\#))
            (and (null *ext*)
                 (<= 4 len)
                 (string-equal (subseq x 0 4) "ext/")))))
    (read-list input)))

(defun parse-namestring-p (str &optional base)
  (let ((path (parse-namestring str)))
    (if (probe-file path)
      path
      (when base
        (let ((path (merge-pathnames path base)))
          (and (probe-file path) path))))))

(defun file-source-p (str)
  (or (parse-namestring-p str)
      (parse-namestring-p str #p"../../src/")))

(defun file-test-p (str)
  (or (parse-namestring-p str #p"test/")
      (parse-namestring-p str #p"../../test/")))

(defun testfile (str)
  (let ((test (format nil "test_~A" str)))
    (if (file-test-p test)
      test
      str)))

(defun file-name-type (p)
  (format nil "~A.~A" (pathname-name p) (pathname-type p)))

(defun read-source-list (input)
  (mapcar
    (lambda (x)
      (let ((p (file-source-p x)))
        (unless p
          (error "File ~A is not found." x))
        (file-name-type p)))
    (read-list-remove input)))

(defmacro with-overwrite-file ((var file) &body body)
  `(with-open-file (,var ,file :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
     ,@body))


;;
;;  open file
;;
(setq *ext* (probe-file +extension+))
(with-open-file (input +filelist+)
  (setq *list* (read-source-list input))
  (setq *test* (mapcar #'testfile *list*)))


;;
;;  write
;;
(with-overwrite-file (output +filetest+)
  (format output "~{ ~A~}" *test*))

(with-overwrite-file (output +source-file+)
  (format output "source =~{ ~A~}" *test*))

(with-overwrite-file (output +release-file+)
  (format output "source =~{ ~A~}" *list*))

