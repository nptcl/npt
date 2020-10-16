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
#include "file_open.h"
#include "format.h"
#include "heap.h"
#include "hold.h"
#include "integer.h"
#include "package.h"
#include "package_intern.h"
#include "package_use.h"
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
	Return(open_input_stream_(ptr, &stream, file));
	if (stream == NULL) {
		/* load "test/" name */
		Return(parse_pathname_char_heap_(ptr, "test/", &path));
		Return(merge_pathnames_clang_(ptr, file, path, Unbound, &file));
		Return(name_pathname_heap_(ptr, file, &file));
		Return(open_input_stream_error_(ptr, &stream, file)); /* force */
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
	Return(find_char_package_(LISP_COMMON_USER, &package));
	GetConst(SPECIAL_PACKAGE, &symbol);
	pushspecial_control(ptr, symbol, package);
	/* (use-package 'lisp-rt *package*) */
	Return(find_char_package_(LISP_RT, &use));
	Return(use_package_(package, use));
	/* load-rt */
	Return(parse_pathname_char_heap_(ptr, name, &file));
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

static int loadrt_execute_call_(Execute ptr, const char *name)
{
	int check;

	Return(handler_warning_(ptr));
	loadrt_disable_debugger(ptr);
	loadrt_declare_optimize();
	Return(loadrt_init(ptr, name, &check));
	if (check)
		return fmte_("result code error.", NULL);

	return 0;
}

static int loadrt_execute(Execute ptr, const char *name)
{
	addr control;

	push_control(ptr, &control);
	(void)loadrt_execute_call_(ptr, name);
	return pop_control_(ptr, control);
}

static int loadrt_nickname_(const char *str1, const char *str2)
{
	addr name1, name2;

	strvect_char_heap(&name1, str1);
	strvect_char_heap(&name2, str2);
	conscar_heap(&name2, name2);
	return rename_package_(name1, name1, name2, &name1);
}

static int loadrt_nicknames_(void)
{
	addr symbol, keyword, cons;

	Return(loadrt_nickname_(LISP_SYSTEM, "LISP-SYSTEM"));
	Return(loadrt_nickname_(LISP_CLOS, "LISP-CLOS"));
	Return(loadrt_nickname_(LISP_RT, "LISP-RT"));

	/* push :rt-degrade */
	GetConst(SPECIAL_FEATURES, &symbol);
	Return(internchar_keyword_("RT-DEGRADE", &keyword, NULL));
	GetValueSymbol(symbol, &cons);
	Check(find_list_eq_unsafe(keyword, cons), "push error");
	cons_heap(&cons, keyword, cons);
	SetValueSymbol(symbol, cons);

	return 0;
}

static void loadrt_getindex(Execute ptr)
{
	addr symbol, value;

	GetConst(RT_INDEX, &symbol);
	fixnum_heap(&value, (fixnum)DegradeCount);
	setspecial_local(ptr, symbol, value);
}

static int loadrt_setindex_(Execute ptr)
{
	addr symbol, value;
	fixnum index;

	GetConst(RT_INDEX, &symbol);
	getspecial_local(ptr, symbol, &value);
	if (value != Unbound) {
		if (! fixnump(value))
			return fmte_("Invalid fixnum value ~S.", value, NULL);
		GetFixnum(value, &index);
		DegradeCount = (int)index;
	}

	return 0;
}

static int loadrt_body_lisp_(Execute ptr, const char *name)
{
	Return(loadrt_nicknames_());
	loadrt_getindex(ptr);
	Return(loadrt_execute(ptr, name));
	return loadrt_setindex_(ptr);
}

static int loadrt_lisp(const char *name)
{
	int errorp;
	Execute ptr;

	freelisp();
	alloclisp(0, 0);
	ptr = Execute_Thread;

	buildlisp(ptr);
	errorp = loadrt_body_lisp_(ptr, name);
	freelisp();

	return errorp;
}

#include "load.h"
_g int test_loadrt(void)
{
	DegradeTitle;
#ifdef LISP_DEBUG_FORCE_GC
	GcCounterForce = LISP_DEBUG_FORCE_GC;
#endif
	return loadrt_files();
}
#else
_g int test_loadrt(void)
{
	return 1;
}
#endif

