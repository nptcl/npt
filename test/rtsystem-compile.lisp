;;
;;  ANSI COMMON LISP: 24. System Construction
;;

;;
;;  Function COMPILE-FILE
;;
(deftest compile-file.1
  (let* ((*compile-value* nil)
         (lisp +compile-file1+)
         (fasl (compile-file-pathname lisp)))
    (compile-file lisp)
    (prog1 (pathnamep
             (probe-file fasl))
      (lisp-system:remove-file fasl)))
  t)

(deftest compile-file.2
  (let* ((*compile-value* nil)
         (lisp +compile-file1+)
         (fasl (compile-file-pathname lisp)))
    (prog1 (pathnamep
             (probe-file
               (compile-file lisp)))
      (lisp-system:remove-file fasl)))
  t)

(deftest compile-file.3
  (let* ((*compile-value* nil)
         (lisp +compile-file1+)
         (fasl (compile-file-pathname lisp)))
    (multiple-value-bind (x y z) (compile-file lisp)
      (setq x (equal x (truename fasl)))
      (lisp-system:remove-file fasl)
      (values x y z)))
  t nil nil)

(deftest compile-file.4
  (let* ((lisp +compile-file1+)
         (fasl (compile-file-pathname lisp)))
    (compile-file lisp)
    (let (*compile-value*)
      (load fasl)
      (lisp-system:remove-file fasl)
      *compile-value*))
  :hello)

(deftest compile-file-memory-stream.1
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-stream (fasl (lisp-system:make-memory-io-stream))
      (with-open-file (output lisp :direction :output)
        (format output "(setq *compile-value* :abc)"))
      (file-position lisp :start)
      (compile-file lisp :output-file fasl)
      (file-position fasl :start)
      (let (*compile-value*)
        (load fasl :type 'fasl)
        *compile-value*)))
  :abc)

(deftest compile-file-memory-stream.2
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-stream (fasl (lisp-system:make-memory-io-stream))
      (with-open-file (output lisp :direction :output)
        (format output "(setq *compile-value* :abc)"))
      (file-position lisp :start)
      (compile-file lisp :output-file fasl)))
  nil nil nil)

(deftest compile-file-input-file.1
  (let ((*default-pathname-defaults*
          (make-pathname :type "lisp" :defaults *default-pathname-defaults*)))
    (let* ((lisp (make-pathname
                   :name "rtsystem-compile1"
                   :directory '(:relative "test")))
           (fasl (compile-file-pathname lisp)))
      (compile-file lisp)
      (let (*compile-value*)
        (load fasl)
        (lisp-system:remove-file fasl)
        *compile-value*)))
  :hello)

(deftest compile-file-output-file.1
  (let* ((lisp +compile-file1+)
         (fasl (make-pathname
                 :type "fasl-others"
                 :defaults (compile-file-pathname lisp))))
    (compile-file lisp :output-file fasl)
    (let (*compile-value*)
      (load fasl :type 'fasl)
      (lisp-system:remove-file fasl)
      *compile-value*))
  :hello)

(defun compile-file-verbose (call)
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-stream (fasl (lisp-system:make-memory-io-stream))
      (let ((*compile-value* :default))
        (with-open-file (stream lisp :direction :output)
          (prin1
            '(eval-when (:compile-toplevel :load-toplevel :execute)
               (defmacro compile-file-verbose-value ()
                 *compile-verbose*))
            stream)
          (prin1
            '(setq *compile-value* (compile-file-verbose-value))
            stream))
        (file-position lisp :start)
        (with-output-to-string (*standard-output*)
          (funcall call lisp fasl))
        (file-position fasl :start)
        (load fasl :type 'fasl)
        *compile-value*))))

(deftest compile-file-verbose.1
  (let ((*compile-verbose* nil))
    (compile-file-verbose
      (lambda (lisp fasl)
        (compile-file lisp :output-file fasl))))
  nil)

(deftest compile-file-verbose.2
  (let ((*compile-verbose* t))
    (compile-file-verbose
      (lambda (lisp fasl)
        (compile-file lisp :output-file fasl))))
  t)

(deftest compile-file-verbose.3
  (let ((*compile-verbose* nil))
    (compile-file-verbose
      (lambda (lisp fasl)
        (compile-file lisp :output-file fasl :verbose nil))))
  nil)

(deftest compile-file-verbose.4
  (let ((*compile-verbose* t))
    (compile-file-verbose
      (lambda (lisp fasl)
        (compile-file lisp :output-file fasl :verbose nil))))
  nil)

(deftest compile-file-verbose.5
  (let ((*compile-verbose* nil))
    (compile-file-verbose
      (lambda (lisp fasl)
        (compile-file lisp :output-file fasl :verbose t))))
  t)

(deftest compile-file-verbose.6
  (let ((*compile-verbose* t))
    (compile-file-verbose
      (lambda (lisp fasl)
        (compile-file lisp :output-file fasl :verbose t))))
  t)

(defun compile-file-print (call)
  (let (output)
    (with-open-stream (lisp (lisp-system:make-memory-io-stream))
      (with-open-stream (fasl (lisp-system:make-memory-io-stream))
        (let ((*compile-value* :default))
          (with-open-file (stream lisp :direction :output)
            (prin1
              '(eval-when (:compile-toplevel :load-toplevel :execute)
                 (defmacro compile-file-print-value ()
                   *compile-print*))
              stream)
            (prin1
              '(setq *compile-value* (compile-file-print-value))
              stream))
          (file-position lisp :start)
          (let ((str (with-output-to-string (*standard-output*)
                       (funcall call lisp fasl))))
            (setq output (not (equal "" str))))
          (file-position fasl :start)
          (load fasl :type 'fasl)
          (values
            *compile-value*
            output))))))

(deftest compile-file-print.1
  (let ((*compile-print* nil))
    (compile-file-print
      (lambda (lisp fasl)
        (compile-file lisp :output-file fasl))))
  nil nil)

(deftest compile-file-print.2
  (let ((*compile-print* t))
    (compile-file-print
      (lambda (lisp fasl)
        (compile-file lisp :output-file fasl))))
  t t)

(deftest compile-file-print.3
  (let ((*compile-print* nil))
    (compile-file-print
      (lambda (lisp fasl)
        (compile-file lisp :output-file fasl :print nil))))
  nil nil)

(deftest compile-file-print.4
  (let ((*compile-print* t))
    (compile-file-print
      (lambda (lisp fasl)
        (compile-file lisp :output-file fasl :print nil))))
  nil nil)

(deftest compile-file-print.5
  (let ((*compile-print* nil))
    (compile-file-print
      (lambda (lisp fasl)
        (compile-file lisp :output-file fasl :print t))))
  t t)

(deftest compile-file-print.6
  (let ((*compile-print* t))
    (compile-file-print
      (lambda (lisp fasl)
        (compile-file lisp :output-file fasl :print t))))
  t t)

(deftest compile-file-external-format.1
  (let* ((lisp +compile-file2+)
         (fasl (compile-file-pathname lisp)))
    (lisp-system:remove-file lisp nil)
    (lisp-system:remove-file fasl nil)
    (with-open-file (output lisp :direction :output :external-format 'utf16be)
      (format output "(setq *compile-value* :abc)"))
    (compile-file lisp :external-format 'utf16be)
    (let (*compile-value*)
      (load fasl)
      (lisp-system:remove-file lisp)
      (lisp-system:remove-file fasl)
      *compile-value*))
  :abc)

(deftest compile-file-external-format.2
  (let* ((lisp-system::*external-format* 'utf16le)
         (lisp +compile-file2+)
         (fasl (compile-file-pathname lisp)))
    (lisp-system:remove-file lisp nil)
    (lisp-system:remove-file fasl nil)
    (with-open-file (output lisp :direction :output :external-format 'utf16le)
      (format output "(setq *compile-value* :abc)"))
    (compile-file lisp)
    (let (*compile-value*)
      (load fasl)
      (lisp-system:remove-file lisp)
      (lisp-system:remove-file fasl)
      *compile-value*))
  :abc)

(deftest compile-file-external-format.3
  (let* ((lisp-system::*external-format* 'utf32be)
         (lisp +compile-file2+)
         (fasl (compile-file-pathname lisp)))
    (lisp-system:remove-file lisp nil)
    (lisp-system:remove-file fasl nil)
    (with-open-file (output lisp :direction :output :external-format 'utf32be)
      (format output "(setq *compile-value* :abc)"))
    (compile-file lisp :external-format :default)
    (let (*compile-value*)
      (load fasl)
      (lisp-system:remove-file lisp)
      (lisp-system:remove-file fasl)
      *compile-value*))
  :abc)

(deftest compile-file-external-format.4
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-stream (fasl (lisp-system:make-memory-io-stream))
      (with-open-file (output lisp :direction :output :external-format 'utf16be)
        (format output "(setq *compile-value* :abc)"))
      (file-position lisp :start)
      (compile-file lisp :output-file fasl :external-format 'utf16be)
      (file-position fasl :start)
      (let (*compile-value*)
        (load fasl :type 'fasl)
        *compile-value*)))
  :abc)

(deftest compile-file-external-format.5
  (let ((lisp-system::*external-format* 'utf32le))
    (with-open-stream (lisp (lisp-system:make-memory-io-stream))
      (with-open-stream (fasl (lisp-system:make-memory-io-stream))
        (with-open-file (output lisp :direction :output :external-format 'utf32le)
          (format output "(setq *compile-value* :abc)"))
        (file-position lisp :start)
        (compile-file lisp :output-file fasl :external-format 'utf32le)
        (file-position fasl :start)
        (let (*compile-value*)
          (load fasl :type 'fasl)
          *compile-value*))))
  :abc)

(deftest compile-file-external-format.6
  (let ((lisp-system::*external-format* 'utf32be))
    (with-open-stream (lisp (lisp-system:make-memory-io-stream))
      (with-open-stream (fasl (lisp-system:make-memory-io-stream))
        (with-open-file (output lisp :direction :output :external-format 'utf32be)
          (format output "(setq *compile-value* :abc)"))
        (file-position lisp :start)
        (compile-file lisp :output-file fasl :external-format :default)
        (file-position fasl :start)
        (let (*compile-value*)
          (load fasl :type 'fasl)
          *compile-value*))))
  :abc)

(deftest compile-file-readtable.1
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-stream (fasl (lisp-system:make-memory-io-stream))
      (with-open-file (s lisp :direction :output)
        (format s "(setq *readtable (copy-readtable *readtable*))"))
      (file-position lisp :start)
      (let ((check *readtable*))
        (compile-file lisp :output-file fasl)
        (eq check *readtable*))))
  t)

(defpackage compile-file-package-test)
(deftest compile-file-package.1
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-stream (fasl (lisp-system:make-memory-io-stream))
      (with-open-file (s lisp :direction :output)
        (format s "(in-package compile-file-package-test)"))
      (file-position lisp :start)
      (let ((*package* *package*))
        (compile-file lisp :output-file fasl)
        (eq *package* (find-package 'common-lisp-user)))))
  t)

(defmacro compile-file-special-truename-call ()
  *compile-file-truename*)

(deftest compile-file-special-truename.1
  (let* ((lisp +compile-file2+)
         (fasl (compile-file-pathname lisp)))
    (lisp-system:remove-file lisp nil)
    (lisp-system:remove-file fasl nil)
    (with-open-file (output lisp :direction :output)
      (format output "(setq *compile-value* (compile-file-special-truename-call))"))
    (compile-file lisp)
    (let (*compile-value*)
      (load fasl)
      (lisp-system:remove-file lisp)
      (lisp-system:remove-file fasl)
      (values
        (car (pathname-directory *compile-value*))
        (pathname-name *compile-value*)
        (pathname-type *compile-value*))))
  :absolute
  "rtsystem-compile2"
  "lisp")

(deftest compile-file-special-truename.2
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-stream (fasl (lisp-system:make-memory-io-stream))
      (with-open-file (output lisp :direction :output)
        (format output "(setq *compile-value* (compile-file-special-truename-call))"))
      (file-position lisp :start)
      (compile-file lisp :output-file fasl)
      (file-position fasl :start)
      (let (*compile-value*)
        (load fasl :type 'fasl)
        *compile-value*)))
  nil)

(defmacro compile-file-special-pathname-call ()
  *compile-file-pathname*)

(deftest compile-file-special-pathname.1
  (let* ((lisp +compile-file2+)
         (fasl (compile-file-pathname lisp)))
    (lisp-system:remove-file lisp nil)
    (lisp-system:remove-file fasl nil)
    (with-open-file (output lisp :direction :output)
      (format output "(setq *compile-value* (compile-file-special-pathname-call))"))
    (compile-file lisp :output-file fasl)
    (let (*compile-value*)
      (load fasl)
      (lisp-system:remove-file lisp)
      (lisp-system:remove-file fasl)
      (values
        (car (pathname-directory *compile-value*))
        (pathname-name *compile-value*)
        (pathname-type *compile-value*))))
  :relative
  "rtsystem-compile2"
  "lisp")

(deftest compile-file-special-pathname.2
  (let* ((lisp (namestring +compile-file2+))
         (fasl (compile-file-pathname lisp)))
    (lisp-system:remove-file lisp nil)
    (lisp-system:remove-file fasl nil)
    (with-open-file (output lisp :direction :output)
      (format output "(setq *compile-value* (compile-file-special-pathname-call))"))
    (compile-file lisp :output-file fasl)
    (let (*compile-value*)
      (load fasl)
      (lisp-system:remove-file lisp)
      (lisp-system:remove-file fasl)
      (values
        (pathnamep *compile-value*)
        (car (pathname-directory *compile-value*))
        (pathname-name *compile-value*)
        (pathname-type *compile-value*))))
  t
  :relative
  "rtsystem-compile2"
  "lisp")

(deftest compile-file-special-pathname.3
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-stream (fasl (lisp-system:make-memory-io-stream))
      (with-open-file (output lisp :direction :output)
        (format output "(setq *compile-value* (compile-file-special-pathname-call))"))
      (file-position lisp :start)
      (compile-file lisp :output-file fasl)
      (file-position fasl :start)
      (let (*compile-value*)
        (load fasl :type 'fasl)
        *compile-value*)))
  nil)

(defmacro compile-file-warnings (switch)
  (when switch
    (warn "Hello"))
  nil)

(defmacro compile-file-style-warnings (switch)
  (when switch
    (warn 'style-warning))
  nil)

(deftest compile-file-warnings.1
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-stream (fasl (lisp-system:make-memory-io-stream))
      (with-open-file (s lisp :direction :output)
        (format s "(compile-file-warnings nil)"))
      (file-position lisp :start)
      (handler-bind ((warning #'muffle-warning))
        (compile-file lisp :output-file fasl))))
  nil nil nil)

(deftest compile-file-warnings.2
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-stream (fasl (lisp-system:make-memory-io-stream))
      (with-open-file (s lisp :direction :output)
        (format s "(compile-file-warnings t)"))
      (file-position lisp :start)
      (handler-bind ((warning #'muffle-warning))
        (compile-file lisp :output-file fasl))))
  nil t t)

(deftest compile-file-warnings.3
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-stream (fasl (lisp-system:make-memory-io-stream))
      (with-open-file (s lisp :direction :output)
        (format s "(+ \"Hello\")"))
      (file-position lisp :start)
      (handler-bind ((warning #'muffle-warning))
        (compile-file lisp :output-file fasl))))
  nil t t)

(deftest compile-file-warnings.4
  (with-open-stream (lisp (lisp-system:make-memory-io-stream))
    (with-open-stream (fasl (lisp-system:make-memory-io-stream))
      (with-open-file (s lisp :direction :output)
        (format s "(compile-file-style-warnings t)"))
      (file-position lisp :start)
      (handler-bind ((warning #'muffle-warning))
        (compile-file lisp :output-file fasl))))
  nil t nil)

(deftest-error compile-file-error.1
  (compile-file #p"*.lisp")
  file-error)

(deftest-error! compile-file-error.2
  (eval '(compile-file)))

(deftest-error! compile-file-error.3
  (eval '(compile-file 10))
  type-error)

(deftest compile-file-delete.1
  (let* ((lisp1 +compile-file1+)
         (lisp2 +compile-file2+)
         (fasl1 (compile-file-pathname lisp1))
         (fasl2 (compile-file-pathname lisp2)))
    (lisp-system:remove-file lisp2 nil)
    (lisp-system:remove-file fasl1 nil)
    (lisp-system:remove-file fasl2 nil)
    (values)))

