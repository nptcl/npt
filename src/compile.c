#include "compile.h"
#include "compile_file.h"
#include "compile_read.h"
#include "compile_write.h"
#include "compile_typedef.h"
#include "condition.h"
#include "constant.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "execute_object.h"
#include "function.h"
#include "pathname.h"
#include "pathname_object.h"
#include "pathname_wildcard.h"
#include "stream_object.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  compile-file-pathname
 */
static void compile_file_pathname_from_input(addr input, addr *ret)
{
	addr host, device, directory, name, type, version;

	copy_pathname_heap(&input, input);
	GetHostPathname(input, &host);
	GetDirectoryPathname(input, &directory);
	GetNamePathname(input, &name);
	strvect_char_heap(&type, "fasl");

	if (pathname_logical_p(input)) {
		GetVersionPathname(input, &version);
		logical_pathname_heap(ret, host, directory, name, type, version);
	}
	else {
		GetDevicePathname(input, &device);
		pathname_heap(ret, host, device, directory, name, type);
	}
}

int compile_file_pathname_common_(Execute ptr, addr input, addr rest, addr *ret)
{
	int check;
	addr output, file;

	if (GetKeyArgs(rest, KEYWORD_OUTPUT_FILE, &output)) {
		/* input-file -> output-file */
		Return(pathname_designer_heap_(ptr, input, &input));
		compile_file_pathname_from_input(input, &file);
	}
	else {
		/* translate output-file */
		if (memory_stream_p(output))
			return Result(ret, output);
		Return(physical_pathname_heap_(ptr, output, &output));
		Return(merge_pathnames_clang_(ptr, output, Nil, Nil, &file));
	}

	/* wildcard */
	Return(wild_pathname_boolean_(file, Nil, &check));
	if (check) {
		*ret = Nil;
		return call_simple_file_error_va_(ptr, file,
				"Cannot return a wildcatd pathname, ~S.", file, NULL);
	}

	return Result(ret, file);
}


/*
 *  with-compilation-unit
 */
int with_compilation_unit_common_(addr form, addr *ret)
{
	/* (defmacro with-compilation-unit
	 *     ((&rest args &key &allow-other-keys) &body body)
	 *   `(lisp-system::with-compilation-unit
	 *      override ',args
	 *      (lambda () ,@body)))
	 */
	addr args, body, over, with, lambda;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &args, &body))
		goto error;
	if (! listp(args))
		goto error;
	if (GetKeyArgs(args, KEYWORD_OVERRIDE, &over))
		over = Nil;
	else
		over = (over == Nil)? Nil: T;

	GetConst(SYSTEM_WITH_COMPILATION_UNIT, &with);
	GetConst(COMMON_LAMBDA, &lambda);
	lista_heap(&lambda, lambda, Nil, body, NULL);
	quotelist_heap(&args, args);
	list_heap(ret, with, over, args, lambda, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("WITH-COMPILATION-UNIT form ~S "
			"must be a ((&key ...) &body ...) form.", form, NULL);
}

static int function_handler_delay_warning(Execute ptr, addr condition)
{
	int check;
	addr pos, list;

	/* push *delay-warning-list* */
	GetConst(SYSTEM_DELAY_WARNING_LIST, &pos);
	Return(getspecialcheck_local_(ptr, pos, &list));
	cons_heap(&list, condition, list);
	setspecial_local(ptr, pos, list);

	/* switch */
	GetConst(SYSTEM_DELAY_WARNING_SWITCH, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	if (pos == Nil)
		return 0;

	/* muffle-warning */
	GetConst(COMMON_MUFFLE_WARNING, &pos);
	Return(find_restart_control_(ptr, pos, condition, &condition, &check));
	if (! check)
		return call_control_error_(ptr);
	return invoke_restart_control_(ptr, condition, Nil);
}

static int handler_delay_warning_(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_DELAY_WARNING, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_delay_warning);
	return pushhandler_common_(ptr, pos, call, 0);
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

static int with_compilation_unit_call_(Execute ptr, addr call)
{
	Return(handler_delay_warning_(ptr));
	return funcall_control_(ptr, call, NULL);
}

static int function_finalize_delay_warning_(Execute ptr)
{
	addr list, x;

	GetConst(SYSTEM_DELAY_WARNING_LIST, &list);
	Return(getspecialcheck_local_(ptr, list, &list));
	nreverse(&list, list);

	GetConst(SYSTEM_DELAY_WARNING_SWITCH, &x);
	setspecial_local(ptr, x, Nil);

	while (list != Nil) {
		GetCons(list, &x, &list);
		Return(warning_restart_case_(ptr, x));
	}

	return 0;
}

static int with_compilation_unit_special_(Execute ptr, addr call)
{
	addr control, save;

	/* call */
	(void)with_compilation_unit_call_(ptr, call);

	/* unwind-protect */
	push_control(ptr, &control);
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (function_finalize_delay_warning_(ptr))
		goto escape;
	restore_execute_control(ptr, save);
escape:
	return pop_control_(ptr, control);
}

int syscall_with_compilation_unit(Execute ptr, addr over, addr args, addr call)
{
	addr control, pos;

	if (! with_compilation_unit_override(ptr, over))
		return funcall_control_(ptr, call, NULL);

	push_control(ptr, &control);
	GetConst(SYSTEM_DELAY_WARNING_LIST, &pos);
	pushspecial_control(ptr, pos, Nil);
	GetConst(SYSTEM_DELAY_WARNING_SWITCH, &pos);
	pushspecial_control(ptr, pos, T);
	(void)with_compilation_unit_special_(ptr, call);
	return pop_control_(ptr, control);
}


/*
 *  initialize
 */
void init_compile(void)
{
	SetPointerCall(defun, var1, handler_delay_warning);
	init_compile_file();
	init_compile_read();
	init_compile_typedef();
	init_compile_write();
}

