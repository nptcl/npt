#include "parse_macro.c"
#include "bignum.h"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "declare.h"
#include "degrade.h"
#include "function.h"
#include "ratio.h"
#include "reader.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "syscall.h"
#include "type_table.h"

/*
 *  macro
 */
static int test_findstack_environment(void)
{
	Execute ptr;
	addr control, name, v1, v2, stack, check;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	init_parse_environment(ptr);

	fixnum_heap(&v1, 10);
	name = readr("aaa");
	defmacro_envstack(ptr, name, v1);
	fixnum_heap(&v2, 20);
	name = readr("bbb");
	defmacro_envstack(ptr, name, v2);

	environment_symbol(&stack);
	getspecialcheck_local(ptr, stack, &stack);
	GetArrayA2(stack, 0, &stack); /* root */
	check = NULL;
	test(findstack_environment(readr("aaa"), stack, T, &check), "findstack_environment1");
	test(check == v1, "findstack_environment2");
	check = NULL;
	test(findstack_environment(readr("bbb"), stack, T, &check), "findstack_environment3");
	test(check == v2, "findstack_environment4");
	check = NULL;
	test(! findstack_environment(readr("ccc"), stack, T, &check), "findstack_environment5");

	free_control_(ptr, control);

	RETURN;
}

static int test_check_macro_function(void)
{
	Execute ptr;
	addr control, v1, v2, v3, sym1, sym2, sym3, sym4, check;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	init_parse_environment(ptr);

	sym1 = readr("aaa");
	sym2 = readr("bbb");
	sym3 = readr("ccc");
	sym4 = readr("ddd");

	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	fixnum_heap(&v3, 30);

	defmacro_envstack(ptr, sym1, v1);
	macrolet_envstack(ptr, sym2, v2);
	setmacro_symbol(sym3, v3);
	remmacro_symbol(sym4);

	test(parse_cons_check_macro(ptr, sym1, &check), "parse_cons_check_macro1");
	test(check == v1, "parse_cons_check_macro2");
	test(parse_cons_check_macro(ptr, sym2, &check), "parse_cons_check_macro3");
	test(check == v2, "parse_cons_check_macro4");
	test(parse_cons_check_macro(ptr, sym3, &check), "parse_cons_check_macro5");
	test(check == v3, "parse_cons_check_macro6");
	test(! parse_cons_check_macro(ptr, sym4, &check), "parse_cons_check_macro7");

	remmacro_symbol(sym3);

	free_control_(ptr, control);

	RETURN;
}

static int test_call_macroexpand_hook_function(Execute ptr,
		addr call, addr form, addr env)
{
	fixnum_heap(&form, RefFixnum(form) + 1);
	setresult_control(ptr, form);
	return 0;
}

static int test_call_macroexpand_hook(void)
{
	addr control, call, hook;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	compiled_heap(&call, Nil);
	SetPointer(p_debug1, var3, test_call_macroexpand_hook_function);
	setcompiled_var3(call, p_debug1);
	GetConst(SPECIAL_MACROEXPAND_HOOK, &hook);
	pushspecial_control(ptr, hook, call);
	call_macroexpand_hook(ptr, &call, T, fixnumh(10), Nil);
	test(RefFixnum(call) == 11, "call_macroexpand_hook1");

	free_control_(ptr, control);

	RETURN;
}


/*
 *  Main
 */
static int testbreak_parse_macro(void)
{
	in_package_lisp_package();
	/* macro */
	TestBreak(test_findstack_environment);
	TestBreak(test_check_macro_function);
	TestBreak(test_call_macroexpand_hook);

	return 0;
}

int test_parse_macro(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_setjmp(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_character();
		build_package();
		build_stream();
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_syscall();
		build_common();
		build_reader();
		build_pathname();
		build_declare();
		build_code();
		lisp_initialize = 1;
		result = testbreak_parse_macro();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

