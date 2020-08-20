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
    "execute_typedef.h"
    "execute_object.h"
    "execute_setjmp.h"
    "execute_values.h"
    "execute.h"
    ("file_type.h" :header t)
    "info.h"
    "build_define.h"
    "build.h"
    ("thread.h" :header t)
    "memory.h"
    "character.h"
    "character_check.h"
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
    "array_copy.h"
    "array_inplace.h"
    "array_make.h"
    "array_sequence.h"
    "array_value.h"
    "array_vector.h"
    "bignum_data.h"
    "bignum_cons.h"
    "bignum_equal.h"
    "bignum_multi.h"
    "bignum_object.h"
    "bignum_output.h"
    "bignum_plus.h"
    "bignum.h"
    "bit.h"
    "boole.h"
    "bytespec.h"
    "character_queue.h"
    "callname.h"
    "call.h"
    "call_arrays.h"
    "call_characters.h"
    "call_conditions.h"
    "call_conses.h"
    "call_data.h"
    "call_environment.h"
    "call_eval.h"
    "call_filenames.h"
    "call_files.h"
    "call_hashtables.h"
    "call_iteration.h"
    "call_numbers.h"
    "call_objects.h"
    "call_packages.h"
    "call_printer.h"
    "call_reader.h"
    "call_sequences.h"
    "call_streams.h"
    "call_strings.h"
    "call_structures.h"
    "call_symbols.h"
    "call_system.h"
    "call_types.h"
    ("constant_table.h" :include "intern_const.h")
    "constant.h"
    "clos_define.h"
    "clos_cache.h"
    "clos_class.h"
    "clos_combination.h"
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
    "code_init.h"
    "compile_typedef.h"
    "compile_array.h"
    "compile_eval.h"
    "compile_file.h"
    "compile_load.h"
    "compile_read.h"
    "compile_stream.h"
    "compile_type.h"
    "compile_value.h"
    "compile_write.h"
    "compile.h"
    "condition.h"
    "condition_debugger.h"
    "condition_define.h"
    "cons.h"
    "cons_list.h"
    "cons_plist.h"
    "console.h"
    "control.h"
    "control_callbind.h"
    "control_execute.h"
    "control_object.h"
    "control_operator.h"
    "copy.h"
    "core.h"
    "core_store.h"
    "document.h"
    "eastasian.h"
    "eastasian_unicode.h"
    "encode.h"
    "encode_unicode.h"
    "env_code.h"
    "env_describe.h"
    "env_time.h"
    "env_version.h"
    "environment.h"
    "equal.h"
    "gc.h"
    "gc_check.h"
    "gc_execute.h"
    "hold.h"
    "declare.h"
    "eval_copy.h"
    "eval_execute.h"
    "eval_main.h"
    "eval_object.h"
    "eval.h"
    "eval_stack.h"
    "eval_table.h"
    "float_equal.h"
    "float_multi.h"
    "float_object.h"
    "float_plus.h"
    "parse_typedef.h"
    "parse_function.h"
    "parse_macro.h"
    "parse_object.h"
    "parse.h"
    "scope_object.h"
    "scope_declare.h"
    "scope_let.h"
    "scope_lambda.h"
    "scope_function.h"
    "scope_call.h"
    "scope.h"
    "code_function.h"
    "code_lambda.h"
    "code_make.h"
    "code_object.h"
    "code_queue.h"
    "optimize.h"
    "optimize_common.h"
    "optimize_parse.h"
    "pathname_common.h"
    "pathname_localp.h"
    "pathname_object.h"
    "pathname_table.h"
    "pathname_translate.h"
    "pathname.h"
    "file_memory.h"
    "file.h"
    "file_open.h"
    "files.h"
    "format_typedef.h"
    "format_float.h"
    "format_function.h"
    "format_parse.h"
    "format_print.h"
    "format.h"
    "function.h"
    "hashtable.h"
    "heap_memory.h"
    "heap_core.h"
    "heap.h"
    "input.h"
    "integer.h"
    "load_time_value.h"
    "lambda.h"
    "make_load_form.h"
    "math_exp.h"
    "math_gcd.h"
    "math_isqrt.h"
    "math_power.h"
    "math_type.h"
    "md5encode.h"
    "number_equal.h"
    "number_multi.h"
    "number_plus.h"
    "number_random.h"
    "number.h"
    "package_object.h"
    "package_bittype.h"
    "package_common.h"
    "package_iterator.h"
    "package_symbol.h"
    "package.h"
    "reader_dispatch.h"
    "reader_function.h"
    "reader_info.h"
    "reader_label.h"
    "reader_table.h"
    "reader_token.h"
    "reader_type.h"
    "reader.h"
    "print.h"
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
    "ratio_equal.h"
    "ratio_plus.h"
    "ratio_multi.h"
    "rational_equal.h"
    "rational_multi.h"
    "rational_plus.h"
    "rational.h"
    "real_ceiling.h"
    "real_common.h"
    "real_decode.h"
    "real_division.h"
    "real_equal.h"
    "real_floor.h"
    "real_multi.h"
    "real_plus.h"
    "real_round.h"
    "real_truncate.h"
    "real.h"
    "require.h"
    "restart.h"
    "restart_value.h"
    "rt.h"
    "sequence.h"
    "sequence_iterator.h"
    "sequence_range.h"
    "sequence_write.h"
    "sort.h"
    "setf.h"
    "step.h"
    "step_prompt.h"
    "stream.h"
    "stream_broadcast.h"
    "stream_concat.h"
    "stream_echo.h"
    "stream_error.h"
    "stream_file.h"
    "stream_init.h"
    "stream_pretty.h"
    "stream_prompt.h"
    "stream_string.h"
    "stream_synonym.h"
    "stream_twoway.h"
    "strtype.h"
    "strvect.h"
    "structure.h"
    "sxhash.h"
    "symbol.h"
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
    "extern_error.h"
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
    "array_copy.c"
    "array_inplace.c"
    "array_make.c"
    "array_sequence.c"
    "array_value.c"
    "array_vector.c"
    "array.c"
    "bignum_cons.c"
    "bignum_data.c"
    "bignum_equal.c"
    "bignum_multi.c"
    "bignum_object.c"
    "bignum_output.c"
    "bignum_plus.c"
    "bignum.c"
    "bit.c"
    "boole.c"
    "build.c"
    "bytespec.c"
    "callname.c"
    "call.c"
    "call_arrays.c"
    "call_characters.c"
    "call_conditions.c"
    "call_conses.c"
    "call_data.c"
    "call_environment.c"
    "call_eval.c"
    "call_filenames.c"
    "call_files.c"
    "call_hashtables.c"
    "call_iteration.c"
    "call_numbers.c"
    "call_objects.c"
    "call_packages.c"
    "call_printer.c"
    "call_reader.c"
    "call_sequences.c"
    "call_streams.c"
    "call_strings.c"
    "call_structures.c"
    "call_symbols.c"
    "call_system.c"
    "call_types.c"
    "c99.c"
    "character.c"
    "character_check.c"
    "character_name.c"
    "character_queue.c"
    "clos_cache.c"
    "clos_class.c"
    "clos_combination.c"
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
    "code_init.c"
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
    "compile_typedef.c"
    "compile_array.c"
    "compile_eval.c"
    "compile_file.c"
    "compile_load.c"
    "compile_read.c"
    "compile_stream.c"
    "compile_type.c"
    "compile_value.c"
    "compile_write.c"
    "compile.c"
    "condition.c"
    "condition_debugger.c"
    "condition_define.c"
    "cons.c"
    "cons_list.c"
    "cons_plist.c"
    "constant.c"
    ("console.c" :include ("console_posix.h" "console_ansi.h"))
    ("control.c" :header t)
    "control_callbind.c"
    "control_execute.c"
    "control_object.c"
    "control_operator.c"
    "copy.c"
    "core.c"
    "core_store.c"
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
    ("env_time.c" :header t)
    ("env_version.c" :header t)
    "environment.c"
    "equal.c"
    "declare.c"
    "eval_copy.c"
    "eval_execute.c"
    "eval_main.c"
    "eval_object.c"
    "eval_stack.c"
    "eval_table.c"
    "eval.c"
    "float_equal.c"
    "float_multi.c"
    "float_object.c"
    "float_plus.c"
    "parse_function.c"
    "parse_macro.c"
    "parse_object.c"
    "parse.c"
    "scope_object.c"
    "scope_call.c"
    "scope_declare.c"
    "scope_function.c"
    "scope_lambda.c"
    "scope_let.c"
    "scope.c"
    "code_function.c"
    "code_lambda.c"
    "code_make.c"
    "code_object.c"
    "code_queue.c"
    "optimize.c"
    "optimize_common.c"
    "optimize_parse.c"
    "execute.c"
    "execute_object.c"
    "execute_setjmp.c"
    "execute_values.c"
    ("file_memory.c" :include
     ("file_ansi.h" "file_posix.h" "file_windows.h"))
    "file.c"
    "file_open.c"
    ("files.c" :include
     ("files_posix.h" "files_windows.h" "files_ansi.h"))
    "format_float.c"
    "format_function.c"
    "format_parse.c"
    "format_print.c"
    "format.c"
    "function.c"
    "gc.c"
    "gc_check.c"
    "gc_execute.c"
    "hold.c"
    "hashtable.c"
    "heap_memory.c"
    "heap_core.c"
    "heap.c"
    "info.c"
    "input.c"
    "integer.c"
    ("intern.c" :include ("intern_symbol_64.h" "intern_symbol_32.h"))
    "load_time_value.c"
    "lambda.c"
    "local.c"
    ("localtime.c" :header t)
    "make_load_form.c"
    "math_exp.c"
    "math_gcd.c"
    "math_isqrt.c"
    "math_power.c"
    "math_type.c"
    "md5encode.c"
    "memory.c"
    "mop_class.c"
    "mop_generic.c"
    "mop_protocols.c"
    "mop_reader.c"
    "mop.c"
    "number_equal.c"
    "number_multi.c"
    "number_plus.c"
    "number_random.c"
    "number.c"
    "object.c"
    "package_bittype.c"
    "package_common.c"
    "package_iterator.c"
    "package_object.c"
    "package_symbol.c"
    ("package.c" :include "intern_count.h")
    "pathname_common.c"
    "pathname_localp.c"
    "pathname_logical.c"
    "pathname_object.c"
    "pathname_posix.c"
    "pathname_table.c"
    "pathname_translate.c"
    "pathname_windows.c"
    "pathname.c"
    "pointer.c"
    "print.c"
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
    "ratio_equal.c"
    "ratio_plus.c"
    "ratio_multi.c"
    "rational_equal.c"
    "rational_multi.c"
    "rational_plus.c"
    "rational.c"
    "reader_dispatch.c"
    "reader_function.c"
    "reader_info.c"
    "reader_label.c"
    "reader_table.c"
    "reader_token.c"
    "reader_type.c"
    "reader.c"
    "real_ceiling.c"
    "real_common.c"
    "real_decode.c"
    "real_division.c"
    "real_equal.c"
    "real_floor.c"
    "real_multi.c"
    "real_plus.c"
    "real_round.c"
    "real_truncate.c"
    "real.c"
    "require.c"
    "restart.c"
    "restart_value.c"
    "rt_load.c"
    "rt.c"
    "sequence.c"
    "sequence_iterator.c"
    "sequence_range.c"
    "sequence_write.c"
    "sort.c"
    "setf.c"
    "step.c"
    "step_prompt.c"
    "stream_broadcast.c"
    "stream_concat.c"
    "stream_echo.c"
    "stream_error.c"
    "stream_file.c"
    "stream_init.c"
    "stream_pretty.c"
    "stream_prompt.c"
    "stream_string.c"
    "stream_synonym.c"
    "stream_twoway.c"
    "stream.c"
    "strtype.c"
    "strvect.c"
    "structure.c"
    "sxhash.c"
    "symbol.c"
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
    "loop_main.c"
    "loop_parse.c"
    "loop_symbol.c"
    "loop_variables.c"
    "user.c"
    "extern.c"
    ("extern_argv.c" :header t)
    "extern_init.c"
    "extern_error.c"
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

