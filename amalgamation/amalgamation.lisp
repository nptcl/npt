(defpackage amalgamation (:use cl))
(in-package amalgamation)

;;
;;  variables
;;
(defvar +name+)
(defparameter +url+ "https://github.com/nptcl/npt")
(defparameter +output-source+ #p"lisp.c")
(defparameter +output-header+ #p"lisp.h")
(defparameter +base+ '(#p"./" #p"../" #p"../src/"))
(defvar *include* nil)
(defvar *header-print* nil)
(defvar *header-include* nil)


;;
;;  file list
;;
(defparameter *header-files*
  '("version.h"
    "define.h"
    "typedef_basic.h"
    "typedef_integer.h"
    "typedef_enum.h"
    ("typedef_thread.h" :header t)
    "typedef.h"
    "alloc.h"
    "c99.h"
    "local.h"
    "execute.h"
    ("file_type.h" :header t)
    "info.h"
    "build.h"
    ("thread.h" :header t)
    "memory.h"
    "character.h"
    "unicode.h"
    "object.h"
    "pointer_table.h"
    "pointer_type.h"
    "pointer.h"
    ("arch.h" :header t)
    "array_typedef.h"
    "array_adjust.h"
    "array_coerce.h"
    "array_common.h"
    "array_copy.h"
    "array_object.h"
    "array_vector.h"
    "array.h"
    "bigdata.h"
    "bignum.h"
    "bigcons.h"
    "bit.h"
    "boole.h"
    "bytespec.h"
    "charqueue.h"
    ("constant.h" :include "intern_const.h")
    "clos_define.h"
    "clos_cache.h"
    "clos_class.h"
    "clos_combination.h"
    "clos_common.h"
    "clos_generic.h"
    "clos_method.h"
    "clos_type.h"
    "clos.h"
    "cmpl_arch.h"
    "cmpl_math.h"
    "cmpl_multi.h"
    "cmpl_plus.h"
    "cmpl.h"
    "code.h"
    "condition.h"
    "cons.h"
    "control.h"
    "copy.h"
    "document.h"
    "encode.h"
    "environment.h"
    "equal.h"
    "eval_code.h"
    "eval_declare.h"
    "eval_main.h"
    "eval_optparse.h"
    "eval_parse.h"
    "eval_scope.h"
    "eval.h"
    "eval_stack.h"
    "eval_table.h"
    "fasl.h"
    "pathname.h"
    "file_memory.h"
    "file.h"
    "files.h"
    "fmtfloat.h"
    "format.h"
    "function.h"
    "gc.h"
    "hashtable.h"
    "heap.h"
    "integer.h"
    "lambda.h"
    "math_exp.h"
    "math_power.h"
    "math_type.h"
    "md5encode.h"
    "number_gcd.h"
    "number_random.h"
    "number.h"
    "package.h"
    "readtable.h"
    "print.h"
    "prompt.h"
    "quote.h"
    "radix.h"
    "random.h"
    "random_float.h"
    "random_state.h"
    "ratio.h"
    "rational.h"
    "real_ceiling.h"
    "real_common.h"
    "real_decode.h"
    "real_division.h"
    "real_float.h"
    "real_floor.h"
    "real_round.h"
    "real_truncate.h"
    "real.h"
    "rt.h"
    "sequence.h"
    "setf.h"
    "stream.h"
    "stream_broadcast.h"
    "stream_concat.h"
    "stream_echo.h"
    "stream_error.h"
    "stream_file.h"
    "stream_prompt.h"
    "stream_string.h"
    "stream_synonym.h"
    "stream_twoway.h"
    "strtype.h"
    "structure.h"
    "sxhash.h"
    "symbol.h"
    "syscall.h"
    "token.h"
    "type.h"
    "type_coerce.h"
    "type_constant.h"
    "type_copy.h"
    "type_deftype.h"
    "type_name.h"
    "type_number.h"
    "type_object.h"
    "type_optimize.h"
    "type_parse.h"
    "type_range.h"
    "type_subtypep.h"
    "type_symbol.h"
    "type_table.h"
    "type_typep.h"
    "type_upgraded.h"
    "type_value.h"
    "common_header.h"
    "common.h"
    "mop_common.h"
    "mop.h"
    "user.h"))

(defparameter *source-files*
  '(("arch.c" :header t)
    "array_adjust.c"
    "array_coerce.c"
    "array_common.c"
    "array_copy.c"
    "array_object.c"
    "array_vector.c"
    "array.c"
    "bigcons.c"
    "bigdata.c"
    "bignum.c"
    "bit.c"
    "boole.c"
    "build.c"
    "bytespec.c"
    "c99.c"
    "character.c"
    "charqueue.c"
    "clos_cache.c"
    "clos_class.c"
    "clos_combination.c"
    "clos_common.c"
    "clos_generic.c"
    "clos_method.c"
    "clos_type.c"
    "clos.c"
    ("cmpl_arch.c" :include ("cmpl_windows.h" "cmpl_cpp.h" "cmpl_c99.h"))
    "cmpl_math.c"
    "cmpl_multi.c"
    "cmpl_plus.c"
    "cmpl.c"
    "code.c"
    "common_arrays.c"
    "common_characters.c"
    "common_conditions.c"
    "common_conses.c"
    "common_data.c"
    "common_environment.c"
    "common_eval.c"
    "common_filenames.c"
    "common_files.c"
    "common_hashtables.c"
    "common_header.c"
    "common_iteration.c"
    "common_numbers.c"
    "common_objects.c"
    "common_packages.c"
    "common_printer.c"
    "common_reader.c"
    "common_sequences.c"
    "common_streams.c"
    "common_strings.c"
    "common_structures.c"
    "common_symbols.c"
    "common_system.c"
    "common_types.c"
    "common.c"
    "condition.c"
    "cons.c"
    "constant.c"
    ("control.c" :header t)
    "copy.c"
    "core.c"
    "degrade.c"
    "develop.c"
    "document.c"
    "encode.c"
    ("environment.c" :header t)
    "equal.c"
    "eval_code.c"
    "eval_declare.c"
    "eval_main.c"
    "eval_optparse.c"
    "eval_parse.c"
    "eval_scope.c"
    "eval_stack.c"
    "eval_table.c"
    "eval.c"
    "execute.c"
    "fasl.c"
    ("file_memory.c" :include
     ("file_ansi.h" "file_posix.h" "file_windows.h"))
    "file.c"
    ("files.c" :include
     ("files_posix.h" "files_windows.h" "files_ansi.h"))
    "fmtfloat.c"
    "format.c"
    "function.c"
    "gc.c"
    "hashtable.c"
    "heap.c"
    "info.c"
    "integer.c"
    ("intern.c" :include ("intern_symbol_64.h" "intern_symbol_32.h"))
    "lambda.c"
    "local.c"
    "math_exp.c"
    "math_power.c"
    "math_type.c"
    "md5encode.c"
    "memory.c"
    "mop_class.c"
    "mop_generic.c"
    "mop_protocols.c"
    "mop_reader.c"
    "mop.c"
    "number_gcd.c"
    "number_random.c"
    "number.c"
    "object.c"
    ("package.c" :include "intern_count.h")
    "pathname.c"
    "pointer.c"
    "print.c"
    ("prompt.c" :header t)
    "quote.c"
    "radix.c"
    "random_float.c"
    ("random_state.c" :header t)
    "random.c"
    "ratio.c"
    "rational.c"
    "readlite.c"
    "readtable.c"
    "real_ceiling.c"
    "real_common.c"
    "real_decode.c"
    "real_division.c"
    "real_float.c"
    "real_floor.c"
    "real_round.c"
    "real_truncate.c"
    "real.c"
    "rt_load.c"
    "rt.c"
    "sequence.c"
    "setf.c"
    "stream_broadcast.c"
    "stream_concat.c"
    "stream_echo.c"
    "stream_error.c"
    "stream_file.c"
    "stream_prompt.c"
    "stream_string.c"
    "stream_synonym.c"
    "stream_twoway.c"
    "stream.c"
    "strtype.c"
    "structure.c"
    "sxhash.c"
    "symbol.c"
    "syscall.c"
    ("thread.c" :include
     ("thread_single.h" "thread_disable.h" "thread_posix.h" "thread_windows.h"))
    "token.c"
    "type_coerce.c"
    "type_constant.c"
    "type_copy.c"
    "type_deftype.c"
    "type_name.c"
    "type_number.c"
    "type_object.c"
    "type_optimize.c"
    "type_parse.c"
    "type_range.c"
    "type_subtypep.c"
    "type_symbol.c"
    "type_table.c"
    "type_typep.c"
    "type_upgraded.c"
    "type_value.c"
    "type.c"
    "unicode.c"
    "user.c"
    "extern.c"
    ;;"main.c"
    ))


;;
;;  tools
;;
(defun read-line-stream (&optional (s *standard-input*))
  (do ((x (read-line s nil nil)
          (read-line s nil nil))
       list)
    ((null x) (nreverse list))
    (push x list)))

(defun read-line-list (&optional (s *standard-input*))
  (if (streamp s)
    (read-line-stream s)
    (with-open-file (x s)
      (read-line-stream x))))

(defmacro dbind (&rest args)
  `(destructuring-bind ,@args))

(defmacro mvbind (&rest args)
  `(multiple-value-bind ,@args))

(defmacro mapfn ((x) expr &body body)
  `(mapcar
     (lambda (,x) ,@body)
     ,expr))

(defun eecho (&rest args &aux check)
  (dolist (x args)
    (if check
      (princ " " *error-output*)
      (setq check t))
    (princ x *error-output*))
  (terpri *error-output*))

(defun eechos (&rest args &aux check)
  (dolist (x args)
    (if check
      (princ " " *error-output*)
      (setq check t))
    (prin1 x *error-output*))
  (terpri *error-output*))

(defparameter +trim+ '(#\Space #\Tab))
(defun triml (x)
  (string-left-trim +trim+ x))

(defun trim (x)
  (string-trim +trim+ x))

(defmacro aif (expr then &optional else)
  `(let ((it ,expr))
     (if it ,then ,else)))

(defmacro awhen (expr &body body)
  `(aif ,expr (progn ,@body)))

(defun filter (call lst &key (key #'values) &aux acc)
  (dolist (x lst (nreverse acc))
    (let ((val (funcall call (funcall key x))))
      (if val (push val acc)))))

(defmacro filtfn ((x) expr &body body)
  `(filter
     (lambda (,x) ,@body)
     ,expr))

(defmacro dobind (bind expr &body body)
  (let ((g (gensym)))
    `(dolist (,g ,expr)
       (dbind ,bind ,g
         ,@body))))

(defun mklist (x)
  (if (listp x) x (list x)))


;;
;;  make-source
;;
(defun echo (&rest args)
  (dolist (x args)
    (format t "~&~A~%" x)))

(defun pragma-push ()
  (echo
    "#ifdef __clang__"
    "#pragma clang diagnostic push"
    "#pragma clang diagnostic ignored \"-Wunused-function\""
    "#endif"
    "#ifdef __GNUC__"
    "#pragma GCC diagnostic push"
    "#pragma GCC diagnostic ignored \"-Wunused-function\""
    "#endif"))

(defun pragma-pop ()
  (echo
    "#ifdef __clang__"
    "#pragma clang diagnostic pop"
    "#endif"
    "#ifdef __GNUC__"
    "#pragma GCC diagnostic pop"
    "#endif"))

(defun header-comment ()
  (format t "/*~%")
  (format t " *  ~A -- Lisp Programming Language.~%" +name+)
  (when +url+
    (format t " *  ~A~%" +url+))
  (format t " */~%")
  (pragma-push)
  (terpri)
  (format t "#define _g static~%")
  (format t "#define _s static~%")
  (format t "#define __extern static~%")
  (terpri)
  (dolist (x (sort *include* #'string<))
    (format t "#include ~A~%" x))
  (terpri))

(defun source-file (x)
  (dolist (base +base+)
    (let ((x (merge-pathnames x base)))
      (if (probe-file x)
        (return x)))))

(defun read-line-source (x)
  (read-line-list
    (source-file x)))

(defun position-string (list str)
  (position-if
    (lambda (x)
      (position x list :test 'eql))
    str))

(defun position-string-not (list str)
  (position-if-not
    (lambda (x)
      (position x list :test 'eql))
    str))

(defun split-whitespace (x)
  (aif (position-string-not +trim+ x)
    (let ((x (subseq x it)))
      (aif (position-string +trim+ x)
        (cons (subseq x 0 it) (split-whitespace (subseq x it)))
        (list x)))))

(defun include-p (x)
  (let ((list (split-whitespace x)))
    (when (= (length list) 2)
      (dbind (x y) list
        (when (equal x "#include")
          (values t y))))))

(defun include-system-p (x)
  (mvbind (x y) (include-p x)
    (when (and x (char= (char y 0) #\<))
      (values t y))))

(defun header-check (x)
  (or (not (include-p x))
      (and *header-print* (include-system-p x))))

(defun header-first (file ignore)
  (if ignore
    (fresh-line)
    (format t "~2&"))
  (let ((name (pathname-name file))
        (type (pathname-type file))
        (asterisk "************************************************************"))
    (format t "/~A~%" asterisk)
    (format t " *  ~A.~A~%" name type)
    (format t " ~A/~%" asterisk)))

(defun header-inline (x)
  (declare (ftype function header-load))
  (mvbind (x y) (include-p x)
    (let ((y (string-trim "\"" y)))
      (awhen (and x (find y (mklist *header-include*) :test 'equal))
        (let ((*header-print* t))
          (header-load y))
        t))))

(defun header-load (file &optional ignore)
  (header-first file ignore)
  (dolist (x (read-line-source file))
    (or (header-inline x)
        (when (header-check x)
          (write-line x))))
  (fresh-line))

(defun header-include-file (x)
  (dolist (x (read-line-source x))
    (mvbind (check name) (include-system-p x)
      (when check
        (pushnew name *include* :test 'equal)))))

(defun file-include-list (list)
  (filtfn (x) list
    (if (consp x)
      (dbind (x . y) x
        (unless (getf y :header)
          x))
      x)))

(defun header-all-list (list)
  (mapfn (x) list
    (if (consp x)
      x
      (list x))))

(defun header-include (list)
  (dolist (x (file-include-list list))
    (format *error-output* "Include-file: ~A.~%" x)
    (header-include-file x)))

(defun header-output (list)
  (dobind (x . y) (header-all-list list)
    (format *error-output* "Output-file: ~A.~%" x)
    (let ((*header-print* (getf y :header))
          (*header-include* (getf y :include)))
      (header-load x))))

(defun make-source ()
  (header-include *header-files*)
  (header-include *source-files*)
  (header-comment)
  (header-output *header-files*)
  (header-output *source-files*)
  (pragma-pop)
  (terpri))


;;
;;  main
;;
(defun set-implementation-list (&optional (file "version.h"))
  (filtfn (x) (read-line-source file)
    (let ((x (split-whitespace x)))
      (when (= (length x) 3)
        x))))

(defun set-implementation-name ()
  (dobind (x y z) (set-implementation-list)
    (when (and (equal x "#define")
               (equal y "Lispname"))
      (setq +name+ (string-trim "\"" z))
      (format *error-output* "Name: ~A~%" +name+))))

(defmacro with-output-file ((file) &body body)
  `(with-open-file (*standard-output*
                     ,file :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
     ,@body))

(defun output-lisp-c (&optional (file +output-source+))
  (with-output-file
    (file)
    (make-source))
  (format *error-output* "~&---~%")
  (format *error-output* "~&Source File -> ~A~%" file))

(defun output-lisp-h (&optional (file +output-header+))
  (with-output-file
    (file)
    (dolist (x (read-line-source "extern.h"))
      (write-line x)))
  (format *error-output* "~&Header File -> ~A~%" file))

(defun main ()
  (set-implementation-name)
  (output-lisp-c)
  (output-lisp-h))
(main)

