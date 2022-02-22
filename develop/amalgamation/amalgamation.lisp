(defpackage amalgamation
  (:use cl)
  (:export #:main #:main-header))
(in-package amalgamation)

(defvar *name*)
(defparameter *url*
  '("https://github.com/nptcl/npt"
    "https://github.com/nptcl/npt-amalgamation"))

(defparameter *base* '(#p"./" #p"../" #p"../../src/"))

(defconstant +header-standard+
  '("stddef.h" "complex.h" "ctype.h" "errno.h" "float.h" "inttypes.h" "limits.h"
    "locale.h" "math.h" "memory.h" "setjmp.h" "signal.h" "stdarg.h" "stddef.h"
    "stdint.h" "stdio.h" "stdlib.h" "string.h" "time.h"))

(defconstant +header-ignore+
  '(;;  C++
    "cmath" "complex"
    ;;  Unix
    "dirent.h" "fcntl.h" "pthread.h" "pwd.h" "semaphore.h" "sys/ioctl.h"
    "sys/select.h" "sys/stat.h" "sys/time.h" "sys/types.h" "sys/utsname.h"
    "sys/wait.h" "termios.h" "unistd.h"
    ;;  Windows
    "aclapi.h" "ntsecapi.h" "pathcch.h" "shlwapi.h" "synchapi.h" "windows.h"
    ;;  readline / editline
    "history.h" "readline.h" "editline.h"
    "edit/history.h" "edit/readline.h"
    "edit/readline/history.h" "edit/readline/readline.h"
    "editline/editline.h" "editline/history.h" "editline/readline.h"
    "readline/history.h" "readline/readline.h"
    ;;  lisp
    "config.h" "load.h"
    ;;  windows
    "windows_arch.h" "windows_terme.h" "windows_values.h" "windows_main.h"
    ))

(defconstant +header-include+
  '("cmpl_c99.h" "cmpl_cpp.h" "cmpl_windows.h"
    "console_ansi.h" "console_unix.h"
    "file_ansi.h" "file_arch.h" "file_unix.h" "file_windows.h"
    "files_ansi.h" "files_unix.h" "files_windows.h"
    "intern_const.h" "intern_count.h"
    "intern_symbol_32.h" "intern_symbol_64.h"
    "process_unix.h" "process_windows.h"
    "prompt_disable.h" "prompt_module.h" "prompt_terme.h" "terme_unix.h"
    "thread_disable.h" "thread_unix.h" "thread_single.h" "thread_windows.h"))

(defparameter +lisp-header+
  '("define.h"
    "typedef_basic.h"
    "typedef_integer.h"
    "typedef_stream.h"
    "typedef.h"
    "define_setjmp.h"
    "main_typedef.h"
    "main_argv.h"
    "main_init.h"
    "extern_control.h"
    "extern_error.h"
    "extern_execute.h"
    "extern_function.h"
    "extern_instance.h"
    "extern_object.h"
    "extern_print.h"
    "extern_sequence.h"
    "extern_stream.h"
    "extern_type.h"
    "extern_unicode.h"
    ))


;;
;;  tools
;;
(defun make-filename (x)
  (dolist (base *base*)
    (let ((x (merge-pathnames x base)))
      (if (probe-file x)
        (return x)))))

(defmacro while (expr &body body)
  `(do () (nil)
     (unless ,expr
       (return nil))
     ,@body))

(defmacro awhile (expr &body body)
  `(do (it) (nil)
     (setq it ,expr)
     (unless it
       (return nil))
     ,@body))

(defmacro aif (expr then &optional else)
  `(let ((it ,expr))
     (if it ,then ,else)))

(defmacro aif2 (expr then &optional else)
  (let ((g (gensym)))
    `(multiple-value-bind (it ,g) ,expr
       (if ,g ,then ,else))))

(defmacro with-overwrite-file ((var file) &body body)
  `(with-open-file (,var ,file :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
     ,@body))

(defun member-file (str list)
  (member str list :test 'equalp))

(defun split-whitespace (x)
  (let* ((x (string-trim '(#\Space #\Tab) x))
         (n (position-if
              (lambda (x) (member x '(#\Space #\Tab)))
              x)))
    (if (null n)
      (list x)
      (cons (subseq x 0 n) (split-whitespace (subseq x n))))))


;;
;;  implementation name
;;
(defun read-line-source (file)
  (with-open-file (input (make-filename file))
    (do ((x (read-line input nil nil)
            (read-line input nil nil))
         list)
      ((null x) (nreverse list))
      (push x list))))

(defun set-implementation-list (file)
  (remove-if-not
    (lambda (x)
      (= (length x) 3))
    (mapcar #'split-whitespace (read-line-source file))))

(defun set-implementation-name (&optional (file "version.h"))
  (dolist (list (set-implementation-list file))
    (destructuring-bind (x y z) list
      (when (and (equal x "#define")
                 (equal y "Lispname"))
        (setq *name* (string-trim '(#\") z))
        (format t "Name: ~A~%" *name*)))))


;;
;;  filelist
;;
(defun list-filelist (file)
  (with-open-file (input (make-filename file))
    (do ((x (read input nil nil)
            (read input nil nil))
         list)
      ((null x) list)
      (push (string-downcase x) list))))

(defun read-filelist ()
  (let ((file #p"release.mk"))
    (sort (remove-if
            (lambda (x)
              (or (equalp x "main.c")
                  (equalp x "source")
                  (equalp x "=")))
            (list-filelist file))
          #'string<)))


;;
;;  parse-include
;;
(defun parse-include-name (x)
  (let ((size (length x)))
    (when (<= size 2)
      (error "Invalid include name ~S" x))
    (let ((a (char x 0))
          (b (subseq x 1 (1- size)))
          (c (char x (1- size))))
      (unless (member a '(#\< #\"))
        (error "First parensis error ~S" a))
      (unless (member c '(#\> #\"))
        (error "First parensis error ~S" c))
      (values b a))))

(defun parse-include (x)
  (let ((list (split-whitespace x)))
    (if (and (= (length list) 2)
             (equal (car list) "#include"))
      (multiple-value-bind (name split) (parse-include-name (second list))
        (values t name split)))))


;;
;;  read-list
;;
(defvar *streams*)
(declaim (ftype function read-list))

(defun push-read-list (file)
  (let ((file (make-filename file)))
    (push (open file) *streams*)))

(defun line-read-list (str)
  (multiple-value-bind (check file split) (parse-include str)
    (declare (ignore split))
    (cond ((not check) str)
          ((member-file file +header-include+)
           (push-read-list file)
           (read-list))
          (t str))))

(defun read-list ()
  (when *streams*
    (let ((stream (car *streams*)))
      (aif (read-line stream nil nil)
        (line-read-list it)
        (progn
          (close stream)
          (pop *streams*)
          (read-list))))))


;;
;;  table
;;
(defvar *read*)
(defvar *body*)
(defvar *include*)
(defvar *standard*)
(defvar *count*)
(defvar *table*)

(defstruct fileinfo file body include standard count)

(defun make-fileinfo-table ()
  (make-hash-table :test 'equalp))

(defun push-source (str)
  (push str *body*)
  (incf *count*))

(defun push-include (str)
  (push str *include*)
  (push str *read*))

(defun read-source (str)
  (multiple-value-bind (check file split) (parse-include str)
    (cond ((null check)
           (push-source str))
          ((member-file file +header-standard+)
           (if (char= split #\<)
             (push file *standard*)
             (push-include file)))
          ((member-file file +header-ignore+)
           (push-source str))
          (t (push-include file)))))

(defun read-loop (file)
  (push-read-list file)
  (awhile (read-list)
    (read-source it)))

(defun setf-table (file)
  (setf (gethash file *table*)
        (make-fileinfo :file file
                       :body (nreverse *body*)
                       :include (nreverse *include*)
                       :standard *standard*
                       :count *count*))
  (makunbound '*body*)
  (makunbound '*include*)
  (makunbound '*standard*)
  (makunbound '*count*))

(defun exist-p-table (name)
  (multiple-value-bind (x y) (gethash name *table*)
    (declare (ignore x))
    y))

(defun make-table (file)
  (unless (exist-p-table file)
    ;(format t "Input: ~A~%" file)
    (let ((*streams* nil)
          (*body* nil)
          (*include* nil)
          (*standard* nil)
          (*count* 0))
      (read-loop file)
      (setf-table file))))

(defun read-table (list)
  (let ((*read* list))
    (while *read*
      (make-table
        (pop *read*)))))


;;
;;  grouping
;;
(defvar *upper-grouping* nil)
(defvar *output-header* t)
(defvar *output-source* t)
(defvar *output-header-call* nil)
(defvar *size-grouping*)
(defvar *include-grouping*)
(defvar *standard-grouping*)
(defvar *check-grouping*)

(defun include-list-grouping (file)
  (aif2 (gethash file *table*)
    (fileinfo-include it)
    (error "fileinfo error: ~S." file)))

(defun standard-list-grouping (file)
  (aif2 (gethash file *table*)
    (fileinfo-standard it)
    (error "fileinfo error: ~S." file)))

(defun count-list-grouping (file)
  (aif2 (gethash file *table*)
    (fileinfo-count it)
    (error "fileinfo error: ~S." file)))

(defun body-list-grouping (file)
  (aif2 (gethash file *table*)
    (fileinfo-body it)
    (error "fileinfo error: ~S." file)))


;;
;;  output
;;
(defun header-name-output (s name)
  (format s "/*~%")
  (format s " *  ~A -- ANSI Common Lisp Programming Language.~%" *name*)
  (dolist (x *url*)
    (format s " *    ~A~%" x))
  (format s " *~%")
  (format s " *  File: ~A~%" name)
  (format s " */~%"))

(defun header-pragma-output (s)
  (format s "#ifdef __clang__~%")
  (format s "#pragma clang diagnostic push~%")
  (format s "#pragma clang diagnostic ignored \"-Wunused-function\"~%")
  (format s "#endif~%")
  (format s "#ifdef __GNUC__~%")
  (format s "#pragma GCC diagnostic push~%")
  (format s "#pragma GCC diagnostic ignored \"-Wunused-function\"~%")
  (format s "#endif~%"))

(defun footer-pragma-output (s)
  (format s "#ifdef __GNUC__~%")
  (format s "#pragma GCC diagnostic pop~%")
  (format s "#endif~%")
  (format s "#ifdef __clang__~%")
  (format s "#pragma clang diagnostic pop~%")
  (format s "#endif~%"))

(defun header-cplusplus-output (s)
  (format s "#define LISP_AMALGAMATION~%")
  (format s "#ifdef __cplusplus~%")
  (format s "#ifndef __STDC_LIMIT_MACROS~%")
  (format s "#define __STDC_LIMIT_MACROS~%")
  (format s "#endif~%")
  (format s "#ifndef __STDC_CONSTANT_MACROS~%")
  (format s "#define __STDC_CONSTANT_MACROS~%")
  (format s "#endif~%")
  (format s "#endif~%"))

(defun header-standard-output (s list)
  (dolist (x list)
    (format s "#include <~A>~%" x)))

(defun header-first (s file)
  (format s "~2&")
  (let ((asterisk "************************************************************"))
    (format s "/~A~%" asterisk)
    (format s " *  ~A~%" file)
    (format s " ~A/~%" asterisk)))

(defun header-include-output (s list)
  (dolist (file list)
    (header-first s file)
    (dolist (x (body-list-grouping file))
      (format s "~A~%" x)))
  (fresh-line s))

(defun source-output (s name list)
  (header-name-output s name)
  (header-pragma-output s)
  (terpri s)
  (header-cplusplus-output s)
  (terpri s)
  (header-standard-output s *standard-grouping*)
  (terpri s)
  (when *output-header-call*
    (funcall *output-header-call* s))
  (when *output-header*
    (header-include-output s *include-grouping*))
  (when *output-source*
    (header-include-output s list))
  (footer-pragma-output s)
  (fresh-line s))

(defun output-grouping-name (name list)
  (format t "Output: ~A~%" name)
  (with-overwrite-file (s name)
    (source-output s name list)))

(defun output-grouping-index (index list)
  (output-grouping-name
    (if *upper-grouping*
      (format nil "lisp_file_~2,,,'0@A.c" index)
      "lisp.c")
    list))


;;
;;  fileinfo
;;
(defun header-grouping (file)
  (unless (member-file file *check-grouping*)
    (push file *check-grouping*)
    (dolist (x (include-list-grouping file))
      (header-grouping x))
    (dolist (x (standard-list-grouping file))
      (pushnew x *standard-grouping* :test 'equalp))
    (push file *include-grouping*)
    (incf *size-grouping* (count-list-grouping file))))

(defun source-grouping (file)
  (dolist (x (standard-list-grouping file))
    (pushnew x *standard-grouping* :test 'equalp))
  (incf *size-grouping* (count-list-grouping file)))

(defun include-grouping (file)
  (dolist (x (include-list-grouping file))
    (header-grouping x)))

(defun size-grouping-p (force)
  (cond (force t)
        ((null *upper-grouping*) nil)
        (t (< *upper-grouping* *size-grouping*))))

(defun size-grouping (index list &optional force)
  (let ((*size-grouping* 0)
        (*include-grouping* nil)
        (*standard-grouping* nil)
        (*check-grouping* nil))
    (dolist (x list)
      (include-grouping x))
    (dolist (x list)
      (source-grouping x))
    (when (size-grouping-p force)
      (setq *include-grouping* (nreverse *include-grouping*))
      (setq *standard-grouping* (sort *standard-grouping* #'string<))
      (output-grouping-index index list)
      t)))

(defun grouping (filelist)
  (let ((index 1) list)
    (dolist (x filelist)
      (push x list)
      (setq list (sort list #'string<))
      (when (size-grouping index list)
        (setq list nil)
        (incf index)))
    (when list
      (size-grouping index list t))))

(defun output-lisp-c ()
  (let ((list (read-filelist))
        (*table* (make-fileinfo-table)))
    (read-table list)
    (grouping list)))


;;
;;  source + header
;;
(defun grouping-file-header (list)
  (let ((file "lisp_file.h")
        (*size-grouping* 0)
        (*include-grouping* nil)
        (*standard-grouping* nil)
        (*check-grouping* nil)
        (*output-header* t)
        (*output-source* nil))
    (dolist (x list)
      (include-grouping x))
    (setq *include-grouping* (nreverse *include-grouping*))
    (setq *standard-grouping* (sort *standard-grouping* #'string<))
    (output-grouping-name file list)))

(defun output-lisp-file-header ()
  (let ((list (read-filelist))
        (*table* (make-fileinfo-table)))
    (read-table list)
    (grouping-file-header list)))

(defun output-header-call-source-only ()
  (setq *output-header-call*
        (lambda (s)
          (format s "#include \"lisp_file.h\"~%")
          (terpri s))))

(defun size-grouping-source-only (index list &optional force)
  (let ((*size-grouping* 0)
        (*include-grouping* nil)
        (*standard-grouping* nil)
        (*check-grouping* nil)
        (*output-header* nil)
        (*output-source* t)
        (*output-header-call*))
    (output-header-call-source-only)
    (dolist (x list)
      (source-grouping x))
    (when (size-grouping-p force)
      (setq *standard-grouping* (sort *standard-grouping* #'string<))
      (output-grouping-index index list)
      t)))

(defun grouping-source-only (filelist)
  (let ((index 1) list)
    (dolist (x filelist)
      (push x list)
      (setq list (sort list #'string<))
      (when (size-grouping-source-only index list)
        (setq list nil)
        (incf index)))
    (when list
      (size-grouping-source-only index list t))))

(defun output-lisp-file-c ()
  (let ((list (read-filelist))
        (*table* (make-fileinfo-table)))
    (read-table list)
    (grouping-source-only list)))


;;
;;  lisp.h
;;
(defun output-lisp-header-grouping (list)
  (remove-duplicates
    (sort (mapcan #'standard-list-grouping list)
          #'string<)
    :test #'equalp))

(defun output-lisp-header-body (s name list grouping)
  (header-name-output s name)
  (format s "~&#ifndef __LISP_HEADER__~%")
  (format s "~&#define __LISP_HEADER__~%")
  (terpri s)
  (header-cplusplus-output s)
  (terpri s)
  (header-standard-output s grouping)
  (terpri s)
  (header-include-output s list)
  (format s "~&#endif~%")
  (fresh-line s))

(defun output-lisp-header (name list)
  (let ((grouping (output-lisp-header-grouping list)))
    (with-overwrite-file (s name)
      (format t "Output: ~A~%" name)
      (output-lisp-header-body s name list grouping))))

(defun output-lisp-h ()
  (let ((name "lisp.h")
        (list +lisp-header+)
        (*table* (make-fileinfo-table)))
    (read-table list)
    (output-lisp-header name list)))


;;
;;  shell.c
;;
(defun output-shell-body (s name list grouping)
  (header-name-output s name)
  (format s "~&#include \"lisp.h\"~%")
  (header-standard-output s grouping)
  (terpri s)
  (header-include-output s list)
  (fresh-line s))

(defun output-shell-file (name list)
  (let ((grouping (output-lisp-header-grouping list)))
    (with-overwrite-file (s name)
      (format t "Output: ~A~%" name)
      (output-shell-body s name list grouping))))

(defun output-shell-c ()
  (let ((name "shell.c")
        (list (list "main.c"))
        (*table* (make-fileinfo-table)))
    (read-table list)
    (output-shell-file name list)))


;;
;;  main
;;
(defun main (*upper-grouping*)
  (set-implementation-name)
  (output-lisp-c)
  (output-lisp-h)
  (output-shell-c))

(defun main-header (*upper-grouping*)
  (set-implementation-name)
  (output-lisp-file-header)
  (output-lisp-file-c)
  (output-lisp-h)
  (output-shell-c))


;;
;;  single-file
;(main nil)

;;
;;  upper 50kLine
;(main 50000)

;;
;;  upper 30kLine + lisp_file.h
;(main-header 30000)

