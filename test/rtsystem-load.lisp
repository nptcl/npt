;;
;;  ANSI COMMON LISP: 24. System Construction
;;

;;
;;  Function LOAD
;;
(deftest load.1
  (let (*load-value*)
    (load +load-file1+))
  t)

(deftest load.2
  (let (*load-value*)
    (load +load-file1+)
    *load-value*)
  :hello)

(deftest load.3
  (let (*load-value*)
    (load (make-pathname :type nil :defaults +load-file1+))
    *load-value*)
  :hello)

(deftest load-memory.1
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-file (output lisp :direction :output)
      (format output "(setq *load-value* :abc)"))
    (file-position lisp :start)
    (with-open-file (input lisp)
      (let (*load-value*)
        (load input)
        *load-value*)))
  :abc)

(deftest load-memory.2
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-file (output lisp :direction :output)
      (format output "(setq *load-value* :def)"))
    (file-position lisp :start)
    (let (*load-value*)
      (load lisp)
      *load-value*))
  :def)

(deftest load-memory.3
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-file (output lisp :direction :output)
      (format output "(setq *load-value* :def)"))
    (file-position lisp :start)
    (let (*load-value*)
      (load lisp :type 'lisp)
      *load-value*))
  :def)

(deftest load-input.1
  (let ((*default-pathname-defaults*
          (make-pathname :type "lisp" :defaults *default-pathname-defaults*)))
    (let* ((lisp (make-pathname
                   :name "rtsystem-load1"
                   :directory '(:relative "test"))))
      (let (*load-value*)
        (load lisp)
        *load-value*)))
  :hello)

(defmacro load-verbose-call ()
  *load-verbose*)

(defun load-verbose (call)
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-file (output lisp :direction :output)
      (format output "(setq *load-value* (load-verbose-call))"))
    (file-position lisp :start)
    (let (*load-value*)
      (with-output-to-string (*standard-output*)
        (funcall call lisp))
      *load-value*)))

(deftest load-verbose.1
  (let ((*load-verbose* nil))
    (load-verbose
      (lambda (lisp)
        (load lisp))))
  nil)

(deftest load-verbose.2
  (let ((*load-verbose* t))
    (load-verbose
      (lambda (lisp)
        (load lisp))))
  t)

(deftest load-verbose.3
  (let ((*load-verbose* nil))
    (load-verbose
      (lambda (lisp)
        (load lisp :verbose nil))))
  nil)

(deftest load-verbose.4
  (let ((*load-verbose* t))
    (load-verbose
      (lambda (lisp)
        (load lisp :verbose nil))))
  nil)

(deftest load-verbose.5
  (let ((*load-verbose* nil))
    (load-verbose
      (lambda (lisp)
        (load lisp :verbose t))))
  t)

(deftest load-verbose.6
  (let ((*load-verbose* t))
    (load-verbose
      (lambda (lisp)
        (load lisp :verbose t))))
  t)

(defmacro load-print-call ()
  *load-print*)

(defun load-print (call)
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-file (output lisp :direction :output)
      (format output "(setq *load-value* (load-print-call))"))
    (file-position lisp :start)
    (let (*load-value*)
      (with-output-to-string (*standard-output*)
        (funcall call lisp))
      *load-value*)))

(deftest load-print.1
  (let ((*load-print* nil))
    (load-print
      (lambda (lisp)
        (load lisp))))
  nil)

(deftest load-print.2
  (let ((*load-print* t))
    (load-print
      (lambda (lisp)
        (load lisp))))
  t)

(deftest load-print.3
  (let ((*load-print* nil))
    (load-print
      (lambda (lisp)
        (load lisp :print nil))))
  nil)

(deftest load-print.4
  (let ((*load-print* t))
    (load-print
      (lambda (lisp)
        (load lisp :print nil))))
  nil)

(deftest load-print.5
  (let ((*load-print* nil))
    (load-print
      (lambda (lisp)
        (load lisp :print t))))
  t)

(deftest load-print.6
  (let ((*load-print* t))
    (load-print
      (lambda (lisp)
        (load lisp :print t))))
  t)

(deftest load-external-format.1
  (let ((lisp +load-file2+))
    (lisp-system:remove-file lisp nil)
    (with-open-file (output lisp :direction :output :external-format 'utf16be)
      (format output "(setq *load-value* :abc)"))
    (let (*load-value*)
      (load lisp :external-format 'utf16be)
      (lisp-system:remove-file lisp)
      *load-value*))
  :abc)

(deftest load-external-format.2
  (let ((lisp-system::*external-format* 'utf32le)
        (lisp +load-file2+))
    (lisp-system:remove-file lisp nil)
    (with-open-file (output lisp :direction :output :external-format 'utf32le)
      (format output "(setq *load-value* :def)"))
    (let (*load-value*)
      (load lisp)
      (lisp-system:remove-file lisp)
      *load-value*))
  :def)

(deftest load-external-format.3
  (let ((lisp-system::*external-format* 'utf32be)
        (lisp +load-file2+))
    (lisp-system:remove-file lisp nil)
    (with-open-file (output lisp :direction :output :external-format 'utf32be)
      (format output "(setq *load-value* :abc)"))
    (let (*load-value*)
      (load lisp :external-format :default)
      (lisp-system:remove-file lisp)
      *load-value*))
  :abc)

(deftest load-external-format.4
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-file (output lisp :direction :output :external-format 'utf16be)
      (format output "(setq *load-value* :def)"))
    (file-position lisp :start)
    (let (*load-value*)
      (load lisp :external-format 'utf16be)
      *load-value*))
  :def)

(deftest load-external-format.5
  (let ((lisp-system::*external-format* 'utf32be))
    (with-open-stream (lisp (lisp-system:make-memory-io-stream))
      (with-open-file (output lisp :direction :output :external-format 'utf32be)
        (format output "(setq *load-value* :abc)"))
      (file-position lisp :start)
      (let (*load-value*)
        (load lisp)
        *load-value*)))
  :abc)

(deftest load-external-format.6
  (let ((lisp-system::*external-format* 'utf32be))
    (with-open-stream (lisp (lisp-system:make-memory-io-stream))
      (with-open-file (output lisp :direction :output :external-format 'utf32be)
        (format output "(setq *load-value* :abc)"))
      (file-position lisp :start)
      (let (*load-value*)
        (load lisp :external-format :default)
        *load-value*)))
  :abc)

(deftest-error load-external-if-does-not-exist.1
  (let ((lisp +load-file2+)
        (*load-value*))
    (lisp-system:remove-file lisp nil)
    (load lisp))
  file-error)

(deftest-error load-external-if-does-not-exist.2
  (let ((lisp +load-file2+)
        (*load-value*))
    (lisp-system:remove-file lisp nil)
    (load lisp :if-does-not-exist t))
  file-error)

(deftest load-external-if-does-not-exist.3
  (let ((lisp +load-file2+)
        (*load-value*))
    (lisp-system:remove-file lisp nil)
    (load lisp :if-does-not-exist nil))
  nil)

(deftest load-external-if-does-not-exist.4
  (let ((lisp +load-file1+)
        (*load-value*))
    (load lisp :if-does-not-exist nil))
  t)

(setf (logical-pathname-translations "load")
      '(("path;*.*.*" "/usr/local/path/")
        ("aaa;bbb;*.input" "test/*.lisp")))

(deftest load-logical-pathname.1
  (let (*load-value*)
    (load "load:aaa;bbb;rtsystem-load1.input"))
  t)

(deftest load-logical-pathname.2
  (let (*load-value*)
    (load "load:aaa;bbb;rtsystem-load1.input")
    *load-value*)
  :hello)

(deftest load-readtable.1
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-file (s lisp :direction :output)
      (format s "(setq *readtable* (copy-readtable *readtable*))"))
    (file-position lisp :start)
    (let ((check *readtable*))
      (load lisp)
      (eq check *readtable*)))
  t)

(defpackage load-package-test)
(deftest load-package.1
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-file (s lisp :direction :output)
      (format s "(in-package load-package-test)"))
    (file-position lisp :start)
    (let ((*package* *package*))
      (load lisp)
      (eq *package* (find-package 'common-lisp-user))))
  t)

(defmacro load-special-truename-call ()
  *load-truename*)

(deftest load-special-truename.1
  (let ((lisp +load-file2+))
    (lisp-system:remove-file lisp nil)
    (with-open-file (output lisp :direction :output)
      (format output "(setq *load-value* (load-special-truename-call))"))
    (load lisp)
    (let (*load-value*)
      (load lisp)
      (lisp-system:remove-file lisp)
      (values
        (car (pathname-directory *load-value*))
        (pathname-name *load-value*)
        (pathname-type *load-value*))))
  :absolute
  "rtsystem-load2"
  "lisp")

(deftest load-special-truename.2
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-file (output lisp :direction :output)
      (format output "(setq *load-value* (load-special-truename-call))"))
    (file-position lisp :start)
    (load lisp)
    (let (*load-value*)
      (load lisp :type 'lisp)
      *load-value*))
  nil)

(defmacro load-special-pathname-call ()
  *load-pathname*)

(deftest load-special-pathname.1
  (let ((lisp +load-file2+))
    (lisp-system:remove-file lisp nil)
    (with-open-file (output lisp :direction :output)
      (format output "(setq *load-value* (load-special-pathname-call))"))
    (let (*load-value*)
      (load lisp)
      (lisp-system:remove-file lisp)
      (values
        (car (pathname-directory *load-value*))
        (pathname-name *load-value*)
        (pathname-type *load-value*))))
  :relative
  "rtsystem-load2"
  "lisp")

(deftest load-special-pathname.2
  (let ((lisp (namestring +load-file2+)))
    (lisp-system:remove-file lisp nil)
    (with-open-file (output lisp :direction :output)
      (format output "(setq *load-value* (load-special-pathname-call))"))
    (let (*load-value*)
      (load lisp)
      (lisp-system:remove-file lisp)
      (values
        (pathnamep *load-value*)
        (car (pathname-directory *load-value*))
        (pathname-name *load-value*)
        (pathname-type *load-value*))))
  t
  :relative
  "rtsystem-load2"
  "lisp")

(deftest load-special-pathname.3
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-file (output lisp :direction :output)
      (format output "(setq *load-value* (load-special-pathname-call))"))
    (file-position lisp :start)
    (let (*load-value*)
      (load lisp :type 'lisp)
      *load-value*))
  nil)

(deftest-error load-error.1
  (load #p"*.lisp")
  file-error)

(deftest-error! load-error.2
  (eval '(load)))

(deftest-error! load-error.3
  (eval '(load 10))
  type-error)

(deftest load-delete.1
  (let ((lisp +load-file2+))
    (lisp-system:remove-file lisp nil)
    (values)))

