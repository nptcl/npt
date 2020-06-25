#include "define.h"

#ifdef LISP_DEGRADE
#include "bignum.h"
#include "build.h"
#include "condition.h"
#include "condition_debugger.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_object.h"
#include "control_operator.h"
#include "declare.h"
#include "degrade.h"
#include "eval_execute.h"
#include "file.h"
#include "format.h"
#include "heap.h"
#include "hold.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  Main
 */
static int rtload_execute(Execute ptr, addr stream, int *ret)
{
	addr pos;

	push_close_stream(ptr, stream);
	Return(eval_stream_toplevel(ptr, stream));
	getresult_control(ptr, &pos);

	return Result(ret, pos != T);
}

static int rtload_pathname(Execute ptr, addr file, int *ret)
{
	addr path, stream;

	/* load name */
	if (open_input_stream(ptr, &stream, file)) {
		/* load "test/" name */
		parse_pathname_char_heap(ptr, "test/", &path);
		merge_pathnames_clang(ptr, file, path, Unbound, &file);
		name_pathname_heap(ptr, file, &file);
		open_input_stream_error(ptr, &stream, file); /* force */
	}

	return rtload_execute(ptr, stream, ret);
}

static int loadrt_init(Execute ptr, const char *name, int *ret)
{
	addr package, symbol, use, file;

	/* title */
	strvect_char_heap(&file, name);
	Return(format_stdout(ptr, "~&[~A]~%", file, NULL));

	/* (let ((*package* (find-package 'common-lisp-user))) ...) */
	find_char_package(LISP_COMMON_USER, &package);
	GetConst(SPECIAL_PACKAGE, &symbol);
	pushspecial_control(ptr, symbol, package);
	/* (use-package 'lisp-rt *package*) */
	find_char_package(LISP_RT, &use);
	use_package(package, use);
	/* load-rt */
	parse_pathname_char_heap(ptr, name, &file);
	return rtload_pathname(ptr, file, ret);
}

static void loadrt_disable_debugger(Execute ptr)
{
	addr symbol;
	GetConst(SYSTEM_ENABLE_DEBUGGER, &symbol);
	pushspecial_control(ptr, symbol, Nil);
}

static void loadrt_declare_optimize(void)
{
	apply_safety_declaim(3);
	apply_speed_declaim(0);
}

static int loadrt_execute(Execute ptr, const char *name)
{
	int check;
	addr control;
	codejump jump;

	begin_switch(ptr, &jump);
	push_new_control(ptr, &control);
	if (codejump_run_p(&jump)) {
		handler_warning(ptr);
		loadrt_disable_debugger(ptr);
		loadrt_declare_optimize();
		Return(loadrt_init(ptr, name, &check));
		if (check)
			fmte("result code error.", NULL);
	}
	end_switch(&jump);
	Return(free_control_(ptr, control));
	throw_switch(&jump);

	return 0;
}

static void loadrt_nickname(const char *str1, const char *str2)
{
	addr name1, name2;

	strvect_char_heap(&name1, str1);
	strvect_char_heap(&name2, str2);
	conscar_heap(&name2, name2);
	rename_package(name1, name1, name2, &name1);
}

static void loadrt_nicknames(void)
{
	addr symbol, keyword, cons;

	loadrt_nickname(LISP_SYSTEM, "LISP-SYSTEM");
	loadrt_nickname(LISP_USER, "LISP-USER");
	loadrt_nickname(LISP_CLOS, "LISP-CLOS");
	loadrt_nickname(LISP_RT, "LISP-RT");

	/* push :rt-degrade */
	GetConst(SPECIAL_FEATURES, &symbol);
	internchar_keyword("RT-DEGRADE", &keyword);
	GetValueSymbol(symbol, &cons);
	Check(find_list_eq_unsafe(keyword, cons), "push error");
	cons_heap(&cons, keyword, cons);
	SetValueSymbol(symbol, cons);
}

static void loadrt_getindex(Execute ptr)
{
	addr symbol, value;

	GetConst(RT_INDEX, &symbol);
	fixnum_heap(&value, (fixnum)DegradeCount);
	setspecial_local(ptr, symbol, value);
}

static void loadrt_setindex(Execute ptr)
{
	addr symbol, value;
	fixnum index;

	GetConst(RT_INDEX, &symbol);
	getspecial_local(ptr, symbol, &value);
	if (value != Unbound) {
		if (! fixnump(value))
			fmte("Invalid fixnum value ~S.", value, NULL);
		GetFixnum(value, &index);
		DegradeCount = (int)index;
	}
}

static int loadrt_lisp(const char *name)
{
	int result;
	lispcode code;
	Execute ptr;

	freelisp();
	alloclisp(0, 0);
	ptr = Execute_Thread;
	result = 1;
	begin_setjmp(ptr, &code);
	if (code_run_p(code)) {
		buildlisp(ptr);
		loadrt_nicknames();
		loadrt_getindex(ptr);
		result = loadrt_execute(ptr, name);
		loadrt_setindex(ptr);
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));

	return result;
}

#include "load.h"
_g int loadrt(void)
{
	TITLE;
#ifdef LISP_DEBUG_FORCE_GC
	GcCounterForce = LISP_DEBUG_FORCE_GC;
#endif
	return loadrt_files();
}
#else
_g int loadrt(void)
{
	return 1;
}
#endif

