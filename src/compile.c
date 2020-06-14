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
#include "function.h"
#include "pathname.h"
#include "strvect.h"
#include "symbol.h"

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

	if (GetKeyArgs(rest, KEYWORD_OUTPUT_FILE, &output)) {
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
	if (GetKeyArgs(args, KEYWORD_OVERRIDE, &over))
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

static int function_handler_delay_warning(Execute ptr, addr condition)
{
	int check;
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
		return 0;

	/* muffle-warning */
	GetConst(COMMON_MUFFLE_WARNING, &pos);
	Return(find_restart_control_(ptr, pos, condition, &condition, &check));
	if (! check)
		return call_control_error_(ptr);
	return invoke_restart_control_(ptr, condition, Nil);
}

static void handler_delay_warning(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_DELAY_WARNING, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_delay_warning);
	pushhandler_common(ptr, pos, call, 0);
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

static int function_finalize_delay_warning(Execute ptr)
{
	addr list, x;

	GetConst(SYSTEM_DELAY_WARNING_LIST, &list);
	getspecialcheck_local(ptr, list, &list);
	nreverse(&list, list);

	GetConst(SYSTEM_DELAY_WARNING_SWITCH, &x);
	setspecial_local(ptr, x, Nil);

	while (list != Nil) {
		GetCons(list, &x, &list);
		Return(warning_restart_case(ptr, x));
	}

	return 0;
}

_g int syscall_with_compilation_unit(Execute ptr, addr over, addr args, addr call)
{
	addr control, restart, pos;

	if (! with_compilation_unit_override(ptr, over))
		return funcall_control(ptr, call, NULL);

	/* unwind-protect */
	push_new_control(ptr, &control);
	GetConst(SYSTEM_DELAY_WARNING_LIST, &pos);
	pushspecial_control(ptr, pos, Nil);
	GetConst(SYSTEM_DELAY_WARNING_SWITCH, &pos);
	pushspecial_control(ptr, pos, T);
	setprotect_control(ptr, p_defun_finalize_delay_warning, Nil);

	/* push control */
	push_new_control(ptr, &restart);
	handler_delay_warning(ptr);

	/* funcall */
	Return(funcall_control(ptr, call, NULL));
	Return(free_control_(ptr, restart));
	return free_control_(ptr, control);
}


/*
 *  initialize
 */
_g void init_compile(void)
{
	SetPointerCall(defun, var1, handler_delay_warning);
	SetPointerCall(defun, empty, finalize_delay_warning);
	init_compile_file();
	init_compile_read();
	init_compile_typedef();
	init_compile_write();
}

