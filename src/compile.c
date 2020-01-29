#include "build.h"
#include "clos_class.h"
#include "code.h"
#include "compile.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "control.h"
#include "eval.h"
#include "fasl.h"
#include "file.h"
#include "files.h"
#include "function.h"
#include "gc.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  compile-file
 */
static int compile_file_output(Execute ptr, addr input, addr output, addr rest)
{
	addr verbose, print, external;

	Check(! streamp(input), "type error");
	Check(! streamp(output), "type error");

	/* argument */
	if (getkeyargs(rest, KEYWORD_VERBOSE, &verbose))
		verbose = Unbound;
	if (getkeyargs(rest, KEYWORD_PRINT, &print))
		print = Unbound;
	if (getkeyargs(rest, KEYWORD_EXTERNAL_FORMAT, &external))
		external = Unbound;

	/* header */
	faslwrite_header(output);

	/* load */
	return compile_load(ptr, input, verbose, print, external);
}

static void compile_file_close_stream(addr stream, addr file)
{
	if (! streamp(file))
		close_stream(stream);
}

static int compile_file_open_input(Execute ptr, addr file, addr *ret)
{
	if (streamp(file)) {
		*ret = file;
		return 0;
	}
	return open_input_stream(ptr, ret, file);
}

static int compile_file_open_output(Execute ptr, addr file, addr *ret)
{
	if (streamp(file)) {
		*ret = file;
		return 0;
	}
	return open_output_binary_stream(ptr, ret, file, FileOutput_supersede);
}

static int compile_file_execute(
		Execute ptr, addr input, addr output, addr rest, addr *ret)
{
	int check;
	addr in, out, symbol;

	/* input stream */
	if (compile_file_open_input(ptr, input, &in)) {
		fmte("Cannot open the input file ~S.", input, NULL);
		return 0;
	}

	/* output stream */
	if (compile_file_open_output(ptr, output, &out)) {
		compile_file_close_stream(in, input);
		fmte("Cannot open the output file ~S.", output, NULL);
		return 0;
	}

	/* variable */
	GetConst(SYSTEM_COMPILE_OUTPUT, &symbol);
	pushspecial_control(ptr, symbol, out);
	GetConst(SYSTEM_COMPILE_CODE, &symbol);
	pushspecial_control(ptr, symbol, Nil);

	/* compile */
	check = compile_file_output(ptr, in, out, rest);
	compile_file_close_stream(in, input);
	compile_file_close_stream(out, output);
	if (check)
		return 1;
	truename_files(ptr, input, ret, 0);
	return 0;
}

static void function_handler_compile(Execute ptr, addr condition)
{
	addr check;

	/* warning */
	GetConst(CONDITION_WARNING, &check);
	if (clos_subtype_p(condition, check)) {
		GetConst(SYSTEM_COMPILE_WARNING, &check);
		setlexical_local(ptr, check, T);
	}

	/* stype-warning */
	GetConst(CONDITION_STYLE_WARNING, &check);
	if (clos_subtype_p(condition, check)) {
		GetConst(SYSTEM_COMPILE_STYLE_WARNING, &check);
		setlexical_local(ptr, check, T);
	}
}

_g void handler_compile(Execute ptr)
{
	addr pos, call;

	/* *compiler-macro* */
	GetConst(SYSTEM_COMPILER_MACRO, &pos);
	pushspecial_control(ptr, pos, T);

	/* compile-warning */
	GetConst(SYSTEM_COMPILE_WARNING, &pos);
	pushlexical_control(ptr, pos, Nil);

	/* compile-style-warning */
	GetConst(SYSTEM_COMPILE_STYLE_WARNING, &pos);
	pushlexical_control(ptr, pos, Nil);

	/* handler-bind */
	GetConst(CONDITION_WARNING, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_compile);
	pushhandler_control(ptr, pos, call, 0);
}

_g int compile_file_common(Execute ptr, addr input, addr rest,
		addr *ret1, addr *ret2, addr *ret3)
{
	addr output, control, pos;
	LocalHold hold;

	/* pathname-designer */
	compile_file_pathname_common(ptr, input, rest, &output);
	/* push control */
	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	handler_compile(ptr);
	if (compile_file_execute(ptr, input, output, rest, ret1))
		return free_check_control(ptr, control, 1);
	localhold_set(hold, 0, *ret1);
	/* warning */
	GetConst(SYSTEM_COMPILE_WARNING, &pos);
	getlexicalcheck_local(ptr, pos, ret2);
	/* style-warning */
	GetConst(SYSTEM_COMPILE_STYLE_WARNING, &pos);
	getlexicalcheck_local(ptr, pos, ret3);
	/* free */
	Return1(free_check_control(ptr, control, 0));
	localhold_end(hold);

	return 0;
}


/*
 *  compile-file-pathname
 */
static void compile_file_pathname_from_input(addr input, addr *ret)
{
	addr host, device, name, type, directory, version;

	copy_pathname_heap(&input, input);
	pathname_host(input, &host, 1);
	pathname_directory(input, &directory, 1);
	pathname_name(input, &name, 1);
	strvect_char_heap(&type, "fasl");
	if (pathname_logical_p(input)) {
		pathname_version(input, &version);
		logical_pathname_heap(ret, host, directory, name, type, version);
	}
	else {
		pathname_device(input, &device, 1);
		pathname_heap(ret, host, device, directory, name, type);
	}
}

_g void compile_file_pathname_common(Execute ptr, addr input, addr rest, addr *ret)
{
	addr output;

	if (getkeyargs(rest, KEYWORD_OUTPUT_FILE, &output)) {
		/* input-file -> output-file */
		pathname_designer_heap(ptr, input, &input);
		compile_file_pathname_from_input(input, ret);
	}
	else {
		/* translate output-file */
		physical_pathname_heap(ptr, output, &output);
		merge_pathnames_clang(ptr, output, Nil, Nil, ret);
	}
}


/*
 *  with-compilation-unit
 */
_g void with_compilation_unit_common(addr form, addr *ret)
{
	/* (defmacro with-compilation-unit
	 *     ((&rest args &key &allow-other-keys) &body body)
	 *   `(lisp-system::with-compilation-unit
	 *      override ',args
	 *      (lambda () ,@body)))
	 */
	addr args, body, over, with, lambda;

	getcdr(form, &form);
	if (! consp_getcons(form, &args, &body))
		goto error;
	if (! listp(args))
		goto error;
	if (getkeyargs(args, KEYWORD_OVERRIDE, &over))
		over = Nil;
	else
		over = (over == Nil)? Nil: T;

	GetConst(SYSTEM_WITH_COMPILATION_UNIT, &with);
	GetConst(COMMON_LAMBDA, &lambda);
	lista_heap(&lambda, lambda, Nil, body, NULL);
	quotelist_heap(&args, args);
	list_heap(ret, with, over, args, lambda, NULL);
	return;

error:
	fmte("WITH-COMPILATION-UNIT form ~S "
			"must be a ((&key ...) &body ...) form.", form, NULL);
	*ret = Nil;
}

static void function_handler_delay_warning(Execute ptr, addr condition)
{
	addr pos, list;

	/* push *delay-warning-list* */
	GetConst(SYSTEM_DELAY_WARNING_LIST, &pos);
	getspecialcheck_local(ptr, pos, &list);
	cons_heap(&list, condition, list);
	setspecial_local(ptr, pos, list);

	/* switch */
	GetConst(SYSTEM_DELAY_WARNING_SWITCH, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	if (pos == Nil)
		return;

	/* muffle-warning */
	GetConst(COMMON_MUFFLE_WARNING, &pos);
	if (! find_restart_control(ptr, pos, condition, &condition))
		control_error();
	(void)invoke_restart_control(ptr, condition, Nil);
}

static void handler_delay_warning(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_DELAY_WARNING, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_delay_warning);
	pushhandler_control(ptr, pos, call, 0);
}

static int with_compilation_unit_override(Execute ptr, addr pos)
{
	/* :override t */
	if (pos != Nil)
		return 1;

	/* :override nil */
	GetConst(SYSTEM_DELAY_WARNING_LIST, &pos);
	getspecial_local(ptr, pos, &pos);
	return pos == Unbound;
}

static void function_finalize_delay_warning(Execute ptr)
{
	addr list, x;

	GetConst(SYSTEM_DELAY_WARNING_LIST, &list);
	getspecialcheck_local(ptr, list, &list);
	nreverse_list_unsafe(&list, list);

	GetConst(SYSTEM_DELAY_WARNING_SWITCH, &x);
	setspecial_local(ptr, x, Nil);

	while (list != Nil) {
		GetCons(list, &x, &list);
		Return0(warning_restart_case(ptr, x));
	}
}

_g void syscall_with_compilation_unit(Execute ptr, addr over, addr args, addr call)
{
	addr finalize, restart, pos;

	if (! with_compilation_unit_override(ptr, over)) {
		funcall_control(ptr, call, NULL);
		return;
	}

	/* unwind-protect */
	push_finalize_control(ptr, &finalize);
	GetConst(SYSTEM_DELAY_WARNING_LIST, &pos);
	pushspecial_control(ptr, pos, Nil);
	GetConst(SYSTEM_DELAY_WARNING_SWITCH, &pos);
	pushspecial_control(ptr, pos, T);
	syscall_code(ptr->local, &pos, p_defun_finalize_delay_warning, Nil);
	setfinalize_control(ptr, finalize, pos);

	/* push control */
	push_return_control(ptr, &restart);
	handler_delay_warning(ptr);

	/* funcall */
	if (funcall_control(ptr, call, NULL)) {
		free_check_control(ptr, restart, 1);
		return;
	}
	Return0(free_check_control(ptr, restart, 0));
	return_values_control(ptr, finalize);
}


/*
 *  initialize
 */
_g void init_compile(void)
{
	SetPointerCall(defun, var1, handler_compile);
	SetPointerCall(defun, var1, handler_delay_warning);
	SetPointerCall(defun, empty, finalize_delay_warning);
}

