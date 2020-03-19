(defpackage amalgamation (:use cl))
(in-package amalgamation)

;;
;;  variables
;;
(defvar +name+)
(defparameter +url+
  '("https://github.com/nptcl/npt"
    "https://github.com/nptcl/npt-amalgamation"))
(defparameter +base+ '(#p"./" #p"../" #p"../../src/"))
(defvar *include-list*)
(defvar *header-print*)
(defvar *header-include*)


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

(defun trimqq (x)
  (string-trim "\"" x))

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
    "#ifdef __GNUC__"
    "#pragma GCC diagnostic pop"
    "#endif"
    "#ifdef __clang__"
    "#pragma clang diagnostic pop"
    "#endif"))

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
        (and (equal x "#include")
             (not (equalp (trimqq y) "config.h"))
             (values t y))))))

(defun extern-p (x)
  (let ((list (split-whitespace x)))
    (when (<= 1 (length list))
      (equal (car list) "__extern"))))

(defun include-system-p (x)
  (mvbind (x y) (include-p x)
    (when (and x (char= (char y 0) #\<))
      (values t y))))

(defun header-check (x)
  (and (not (extern-p x))
       (or (not (include-p x))
           (and *header-print* (include-system-p x)))))

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

(declaim (ftype function header-read))
(defun header-inline (x)
  (mvbind (x y) (include-p x)
    (let ((y (trimqq y)))
      (when (and x (find y (mklist *header-include*) :test 'equal))
        (let ((*header-print* t))
          (header-read y))
        t))))

(defun header-read (file &optional ignore)
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
        (pushnew name *include-list* :test 'equal)))))

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

(defun header-load (list)
  (dolist (x (file-include-list list))
    (format *error-output* "Include-file: ~A~%" x)
    (header-include-file x)))

(defun header-output (list)
  (dobind (x . y) (header-all-list list)
    (format *error-output* "Output-file: ~A~%" x)
    (let ((*header-print* (getf y :header))
          (*header-include* (getf y :include)))
      (header-read x))))


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
      (setq +name+ (trimqq z))
      (format *error-output* "Name: ~A~%" +name+))))

(defmacro with-output-file ((file) &body body)
  `(with-open-file (*standard-output*
                     ,file :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
     (let (*include-list* *header-print* *header-include*)
       ,@body)))


;;
;;  lisp.c
;;
(defparameter *lispc-header*
  '("version.h"
    "define.h"
    "typedef_basic.h"
    "typedef_integer.h"
    "typedef_stream.h"
    "typedef_value.h"
    ("typedef_thread.h" :header t)
    "typedef.h"
    "type_constant.h"
    "alloc.h"
    "c99.h"
    "local.h"
    "localtime.h"
    "execute.h"
    ("file_type.h" :header t)
    "info.h"
    "build_define.h"
    "build.h"
    ("thread.h" :header t)
    "memory.h"
    "character.h"
    "character_check.h"
    "character_common.h"
    "character_name.h"
    "unicode.h"
    "object.h"
    "pointer_table.h"
    "pointer_type.h"
    "pointer.h"
    ("arch.h" :header t)
    "array_typedef.h"
    "array.h"
    "array_access.h"
    "array_adjust.h"
    "array_coerce.h"
    "array_common.h"
    "array_copy.h"
    "array_inplace.h"
    "array_make.h"
    "array_sequence.h"
    "array_value.h"
    "array_vector.h"
    "bigdata.h"
    "bignum.h"
    "bigcons.h"
    "bit.h"
    "boole.h"
    "bytespec.h"
    "charqueue.h"
    ("constant_table.h" :include "intern_const.h")
    "constant.h"
    "clos_define.h"
    "clos_cache.h"
    "clos_class.h"
    "clos_combination.h"
    "clos_common.h"
    "clos_generic.h"
    "clos_make.h"
    "clos_method.h"
    "clos_redefine.h"
    "clos_type.h"
    "clos.h"
    "cmpl_arch.h"
    "cmpl_math.h"
    "cmpl_multi.h"
    "cmpl_plus.h"
    "cmpl.h"
    "code.h"
    "compile.h"
    "condition.h"
    "condition_common.h"
    "condition_debugger.h"
    "condition_define.h"
    "cons.h"
    "cons_common.h"
    "cons_list.h"
    "cons_plist.h"
    "console.h"
    "control.h"
    "control_callbind.h"
    "copy.h"
    "core.h"
    "data.h"
    "document.h"
    "eastasian.h"
    "eastasian_unicode.h"
    "encode.h"
    "encode_unicode.h"
    "env_code.h"
    "env_describe.h"
    "env_function.h"
    "env_time.h"
    "env_version.h"
    "environment.h"
    "equal.h"
    "eval_typedef.h"
    "eval_code.h"
    "eval_common.h"
    "eval_copy.h"
    "eval_declare.h"
    "eval_main.h"
    "eval_parse.h"
    "eval_scope.h"
    "eval.h"
    "eval_stack.h"
    "eval_table.h"
    "optimize.h"
    "optimize_common.h"
    "optimize_parse.h"
    "fasl.h"
    "pathname.h"
    "file_memory.h"
    "file.h"
    "files.h"
    "format_typedef.h"
    "format_float.h"
    "format_function.h"
    "format_parse.h"
    "format_print.h"
    "format.h"
    "function.h"
    "gc.h"
    "hashtable.h"
    "hashtable_common.h"
    "heap.h"
    "input.h"
    "integer.h"
    "lambda.h"
    "math_exp.h"
    "math_power.h"
    "math_type.h"
    "md5encode.h"
    "number_common.h"
    "number_gcd.h"
    "number_isqrt.h"
    "number_random.h"
    "number.h"
    "package.h"
    "package_common.h"
    "readtable.h"
    "readtable_common.h"
    "print.h"
    "print_common.h"
    "print_dispatch.h"
    "print_function.h"
    "print_object.h"
    "print_pretty.h"
    "print_write.h"
    "process.h"
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
    "require.h"
    "restart.h"
    "restart_value.h"
    "rt.h"
    "sequence.h"
    "sequence_common.h"
    "sequence_iterator.h"
    "sequence_range.h"
    "sequence_write.h"
    "sort.h"
    "setf.h"
    "stream.h"
    "stream_broadcast.h"
    "stream_common.h"
    "stream_concat.h"
    "stream_echo.h"
    "stream_error.h"
    "stream_file.h"
    "stream_pretty.h"
    "stream_prompt.h"
    "stream_string.h"
    "stream_synonym.h"
    "stream_twoway.h"
    "strtype.h"
    "strtype_common.h"
    "strvect.h"
    "structure.h"
    "structure_common.h"
    "sxhash.h"
    "symbol.h"
    "symbol_common.h"
    "syscall.h"
    "syscall_code.h"
    "token.h"
    "type.h"
    "type_coerce.h"
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
    "loop.h"
    "loop_bind.h"
    "loop_common.h"
    "loop_main.h"
    "loop_parse.h"
    "loop_symbol.h"
    "loop_variables.h"
    "user.h"
    "main_string.h"
    "main_argv.h"
    "main_init.h"
    "extern.h"
    "extern_init.h"
    "extern_control.h"
    "extern_object.h"
    "extern_string.h"
    "extern_stream.h"
    "extern_unicode.h"
    ))

(defparameter *lispc-source*
  '("variable.c"
    ("arch.c" :header t)
    "array_access.c"
    "array_adjust.c"
    "array_coerce.c"
    "array_common.c"
    "array_copy.c"
    "array_inplace.c"
    "array_make.c"
    "array_sequence.c"
    "array_value.c"
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
    "character_check.c"
    "character_common.c"
    "character_name.c"
    "charqueue.c"
    "clos_cache.c"
    "clos_class.c"
    "clos_combination.c"
    "clos_common.c"
    "clos_generic.c"
    "clos_make.c"
    "clos_method.c"
    "clos_redefine.c"
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
    "compile.c"
    "condition.c"
    "condition_common.c"
    "condition_debugger.c"
    "condition_define.c"
    "cons.c"
    "cons_common.c"
    "cons_list.c"
    "cons_plist.c"
    "constant.c"
    ("console.c" :include ("console_posix.h" "console_ansi.h"))
    ("control.c" :header t)
    "control_callbind.c"
    "copy.c"
    "core.c"
    "data.c"
    "degrade.c"
    "develop.c"
    "document.c"
    "eastasian_table.c"
    "eastasian.c"
    "eastasian_unicode.c"
    "encode.c"
    "encode_unicode.c"
    "env_code.c"
    "env_describe.c"
    "env_function.c"
    ("env_time.c" :header t)
    ("env_version.c" :header t)
    "environment.c"
    "equal.c"
    "eval_code.c"
    "eval_common.c"
    "eval_copy.c"
    "eval_declare.c"
    "eval_main.c"
    "eval_parse.c"
    "eval_scope.c"
    "eval_stack.c"
    "eval_table.c"
    "eval.c"
    "optimize.c"
    "optimize_common.c"
    "optimize_parse.c"
    "execute.c"
    "fasl.c"
    ("file_memory.c" :include
     ("file_ansi.h" "file_posix.h" "file_windows.h"))
    "file.c"
    ("files.c" :include
     ("files_posix.h" "files_windows.h" "files_ansi.h"))
    "format_float.c"
    "format_function.c"
    "format_parse.c"
    "format_print.c"
    "format.c"
    "function.c"
    "gc.c"
    "hashtable.c"
    "hashtable_common.c"
    "heap.c"
    "info.c"
    "input.c"
    "integer.c"
    ("intern.c" :include ("intern_symbol_64.h" "intern_symbol_32.h"))
    "lambda.c"
    "local.c"
    ("localtime.c" :header t)
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
    "number_common.c"
    "number_gcd.c"
    "number_isqrt.c"
    "number_random.c"
    "number.c"
    "object.c"
    ("package.c" :include "intern_count.h")
    "package_common.c"
    "pathname.c"
    "pointer.c"
    "print.c"
    "print_common.c"
    "print_dispatch.c"
    "print_function.c"
    "print_object.c"
    "print_pretty.c"
    "print_write.c"
    ("process.c" :header t)
    ("prompt.c" :header t)
    "quote.c"
    "radix.c"
    "random_float.c"
    ("random_state.c" :header t)
    "random.c"
    "ratio.c"
    "rational.c"
    "readtable.c"
    "readtable_common.c"
    "real_ceiling.c"
    "real_common.c"
    "real_decode.c"
    "real_division.c"
    "real_float.c"
    "real_floor.c"
    "real_round.c"
    "real_truncate.c"
    "real.c"
    "require.c"
    "restart.c"
    "restart_value.c"
    "rt_load.c"
    "rt.c"
    "sequence.c"
    "sequence_common.c"
    "sequence_iterator.c"
    "sequence_range.c"
    "sequence_write.c"
    "sort.c"
    "setf.c"
    "stream_broadcast.c"
    "stream_concat.c"
    "stream_common.c"
    "stream_echo.c"
    "stream_error.c"
    "stream_file.c"
    "stream_pretty.c"
    "stream_prompt.c"
    "stream_string.c"
    "stream_synonym.c"
    "stream_twoway.c"
    "stream.c"
    "strtype.c"
    "strtype_common.c"
    "strvect.c"
    "structure.c"
    "structure_common.c"
    "sxhash.c"
    "symbol.c"
    "symbol_common.c"
    "syscall.c"
    "syscall_code.c"
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
    "loop.c"
    "loop_bind.c"
    "loop_common.c"
    "loop_main.c"
    "loop_parse.c"
    "loop_symbol.c"
    "loop_variables.c"
    "user.c"
    "extern.c"
    ("extern_argv.c" :header t)
    "extern_init.c"
    "extern_control.c"
    "extern_object.c"
    "extern_string.c"
    "extern_stream.c"
    "extern_unicode.c"
    ))

(defun header-common ()
  (format t "/*~%")
  (format t " *  ~A -- Lisp Programming Language.~%" +name+)
  (dolist (x +url+)
    (format t " *    ~A~%" x))
  (format t " */~%"))

(defun header-cplusplus ()
  (echo
    "#define LISP_AMALGAMATION"
    "#ifdef __cplusplus"
    "#ifndef __STDC_LIMIT_MACROS"
    "#define __STDC_LIMIT_MACROS"
    "#endif"
    "#ifndef __STDC_CONSTANT_MACROS"
    "#define __STDC_CONSTANT_MACROS"
    "#endif"
    "#endif"))

(defun header-comment ()
  (header-common)
  (pragma-push)
  (terpri)
  (format t "#define _g static~%")
  (format t "#define _s static~%")
  (format t "#define __extern static~%")
  (terpri)
  (header-cplusplus)
  (terpri)
  (dolist (x (sort *include-list* #'string<))
    (format t "#include ~A~%" x))
  (terpri))

(defun output-lisp-c (&optional (file "lisp.c"))
  (with-output-file
    (file)
    (header-load *lispc-header*)
    (header-load *lispc-source*)
    (header-comment)
    (header-output *lispc-header*)
    (header-output *lispc-source*)
    (pragma-pop)
    (terpri))
  (format *error-output* "~&*** Source File -> ~A~%" file))


;;
;;  lisp.h
;;
(defparameter *lisph-header*
  '("define.h"
    "typedef_basic.h"
    "typedef_integer.h"
    "typedef_stream.h"
    "main_string.h"
    "main_argv.h"
    "main_init.h"
    "extern_control.h"
    "extern_object.h"
    "extern_stream.h"
    "extern_unicode.h"
    ))

(defun output-lisp-h (&optional (file "lisp.h"))
  (with-output-file
    (file)
    (header-load *lisph-header*)
    (header-common)
    (format t "~&#ifndef __LISP_HEADER__~%")
    (format t "~&#define __LISP_HEADER__~%")
    (terpri)
    (header-cplusplus)
    (terpri)
    (dolist (x (sort *include-list* #'string<))
      (format t "#include ~A~%" x))
    (terpri)
    (header-output *lisph-header*)
    (format t "~&#endif~%")
    (terpri))
  (format *error-output* "~&*** Header File -> ~A~%" file))


;;
;;  shell.c
;;
(defun output-shell-c (&optional (input "main.c") (output "shell.c"))
  (with-output-file
    (output)
    (header-common)
    (format t "~&#include \"lisp.h\"~%")
    (dolist (x (remove-if
                 #'include-p
                 (read-line-source input)))
      (write-line x))
    (terpri))
  (format *error-output* "~&*** Source File -> ~A~%" output))


;;
;;  main
;;
(defun main ()
  (set-implementation-name)
  (output-lisp-c)
  (output-lisp-h)
  (output-shell-c))
(main)

