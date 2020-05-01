#include "control_callbind.c"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_operator.h"
#include "copy.h"
#include "degrade.h"
#include "execute.h"
#include "readtable.h"
#include "package.h"
#include "pathname.h"
#include "syscall.h"
#include "symbol.h"
#include "type.h"
#include "type_table.h"

/*
 *  call_compiled_function
 */
static int test_call_callbind_code(void)
{
	test(CallBindTable[CallBind_code] == call_callbind_code,
			"call_callbind_code1");
	RETURN;
}

static int test_function_value;
static int test_function_any(Execute ptr)
{
	test_function_value = 1;
	return 0;
}

static int test_call_callbind_any(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, any, test_function_any);
	setcompiled_any(pos, p_debug1);
	test_function_value = 0;
	call_compiled_function(ptr, pos);
	test(test_function_value, "call_callbind_any1");

	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	test_function_value = 0;
	call_compiled_function(ptr, pos);
	test(test_function_value, "call_callbind_any2");

	free_control_(ptr, control);

	RETURN;
}

static int test_call_callbind_empty(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, empty, test_function_any);
	setcompiled_empty(pos, p_debug1);
	test_function_value = 0;
	call_compiled_function(ptr, pos);
	test(test_function_value, "call_callbind_empty1");

	free_control_(ptr, control);

	RETURN;
}

static int test_function_dynamic(Execute ptr, addr cons)
{
	test_function_value = (int)length_list_unsafe(cons);
	return 0;
}

static int test_call_callbind_dynamic(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, dynamic, test_function_dynamic);
	setcompiled_dynamic(pos, p_debug1);
	test_function_value = -1;
	call_compiled_function(ptr, pos);
	test(test_function_value == 0, "call_callbind_dynamic1");

	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	test_function_value = -1;
	call_compiled_function(ptr, pos);
	test(test_function_value == 2, "call_callbind_dynamic2");

	free_control_(ptr, control);

	RETURN;
}

static int test_function_rest(Execute ptr, addr cons)
{
	if (GetStatusDynamic(cons)) {
		test_function_value = -1;
		return 0;
	}
	test_function_value = (int)length_list_unsafe(cons);
	return 0;
}

static int test_call_callbind_rest(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, rest, test_function_rest);
	setcompiled_rest(pos, p_debug1);
	test_function_value = -1;
	call_compiled_function(ptr, pos);
	test(test_function_value == 0, "call_callbind_rest1");

	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	test_function_value = -1;
	call_compiled_function(ptr, pos);
	test(test_function_value == 2, "call_callbind_rest2");

	free_control_(ptr, control);

	RETURN;
}

static int test_function_var1(Execute ptr, addr var1)
{
	test_function_value = (int)RefFixnum(var1);
	return 0;
}

static int test_call_callbind_var1(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, var1, test_function_var1);
	setcompiled_var1(pos, p_debug1);
	setargs_va_control(ptr, fixnumh(10), NULL);
	call_compiled_function(ptr, pos);
	test(test_function_value == 10, "call_callbind_var1-1");

	free_control_(ptr, control);

	RETURN;
}

static int test_function_var2(Execute ptr, addr var1, addr var2)
{
	test_function_value = (int)RefFixnum(var1) + (int)RefFixnum(var2);
	return 0;
}

static int test_call_callbind_var2(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, var2, test_function_var2);
	setcompiled_var2(pos, p_debug1);
	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	call_compiled_function(ptr, pos);
	test(test_function_value == 30, "call_callbind_var2-1");

	free_control_(ptr, control);

	RETURN;
}

static int test_function_var3(Execute ptr, addr var1, addr var2, addr var3)
{
	test_function_value =
		(int)RefFixnum(var1) + (int)RefFixnum(var2) + (int)RefFixnum(var3);
	return 0;
}

static int test_call_callbind_var3(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, var3, test_function_var3);
	setcompiled_var3(pos, p_debug1);
	setargs_va_control(ptr, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	call_compiled_function(ptr, pos);
	test(test_function_value == 60, "call_callbind_var3-1");

	free_control_(ptr, control);

	RETURN;
}

static int test_function_opt1(Execute ptr, addr var1)
{
	test_function_value = (var1 == Unbound)? -1: (int)RefFixnum(var1);
	return 0;
}

static int test_call_callbind_opt1(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, opt1, test_function_opt1);
	setcompiled_opt1(pos, p_debug1);
	setargs_nil_control(ptr);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_value == -1, "call_callbind_opt1-1");

	setargs_va_control(ptr, fixnumh(10), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_value == 10, "call_callbind_opt1-2");

	free_control_(ptr, control);

	RETURN;
}

static int test_function_opt2(Execute ptr, addr var1, addr var2)
{
	if (var1 == Unbound)
		test_function_value = -1;
	else if (var2 == Unbound)
		test_function_value = -(int)RefFixnum(var1);
	else
		test_function_value = (int)(RefFixnum(var1) + RefFixnum(var2));
	return 0;
}

static int test_call_callbind_opt2(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, opt2, test_function_opt2);
	setcompiled_opt2(pos, p_debug1);
	setargs_nil_control(ptr);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_value == -1, "call_callbind_opt2-1");

	setargs_va_control(ptr, fixnumh(10), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_value == -10, "call_callbind_opt2-2");

	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_value == 30, "call_callbind_opt2-3");

	free_control_(ptr, control);

	RETURN;
}

static int test_function_mode;
static int test_function_opt3(Execute ptr, addr var1, addr var2, addr var3)
{
	if (var1 == Unbound) {
		test_function_mode = 0;
		test_function_value = 0;
	}
	else if (var2 == Unbound) {
		test_function_mode = 1;
		test_function_value = (int)RefFixnum(var1);
	}
	else if (var3 == Unbound) {
		test_function_mode = 2;
		test_function_value = (int)(RefFixnum(var1) + RefFixnum(var2));
	}
	else {
		test_function_mode = 3;
		test_function_value =
			(int)(RefFixnum(var1) + RefFixnum(var2) + RefFixnum(var3));
	}

	return 0;
}

static int test_call_callbind_opt3(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, opt3, test_function_opt3);
	setcompiled_opt3(pos, p_debug1);
	setargs_nil_control(ptr);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 0, "call_callbind_opt3-1");
	test(test_function_value == 0, "call_callbind_opt3-2");

	setargs_va_control(ptr, fixnumh(10), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 1, "call_callbind_opt3-3");
	test(test_function_value == 10, "call_callbind_opt3-4");

	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 2, "call_callbind_opt3-5");
	test(test_function_value == 30, "call_callbind_opt3-6");

	setargs_va_control(ptr, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 3, "call_callbind_opt3-7");
	test(test_function_value == 60, "call_callbind_opt3-8");

	free_control_(ptr, control);

	RETURN;
}

static int test_function_var1opt1(Execute ptr, addr var1, addr var2)
{
	if (var2 == Unbound) {
		test_function_mode = 1;
		test_function_value = (int)RefFixnum(var1);
	}
	else {
		test_function_mode = 2;
		test_function_value = (int)(RefFixnum(var1) + RefFixnum(var2));
	}

	return 0;
}

static int test_call_callbind_var1opt1(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, var1opt1, test_function_var1opt1);
	setcompiled_var1opt1(pos, p_debug1);
	setargs_va_control(ptr, fixnumh(10), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 1, "call_callbind_var1opt1-1");
	test(test_function_value == 10, "call_callbind_var1opt1-2");

	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 2, "call_callbind_var1opt1-3");
	test(test_function_value == 30, "call_callbind_var1opt1-4");

	free_control_(ptr, control);

	RETURN;
}

static int test_function_var2opt1(Execute ptr, addr var1, addr var2, addr var3)
{
	if (var3 == Unbound) {
		test_function_mode = 2;
		test_function_value = (int)(RefFixnum(var1) + RefFixnum(var2));
	}
	else {
		test_function_mode = 3;
		test_function_value =
			(int)(RefFixnum(var1) + RefFixnum(var2) + RefFixnum(var3));
	}

	return 0;
}

static int test_call_callbind_var2opt1(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, var2opt1, test_function_var2opt1);
	setcompiled_var2opt1(pos, p_debug1);
	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 2, "call_callbind_var2opt1-1");
	test(test_function_value == 30, "call_callbind_var2opt1-2");

	setargs_va_control(ptr, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 3, "call_callbind_var2opt1-3");
	test(test_function_value == 60, "call_callbind_var2opt1-4");

	free_control_(ptr, control);

	RETURN;
}

static int test_function_var1opt2(Execute ptr, addr var1, addr var2, addr var3)
{
	if (var2 == Unbound) {
		test_function_mode = 1;
		test_function_value = (int)RefFixnum(var1);
	}
	else if (var3 == Unbound) {
		test_function_mode = 2;
		test_function_value = (int)(RefFixnum(var1) + RefFixnum(var2));
	}
	else {
		test_function_mode = 3;
		test_function_value =
			(int)(RefFixnum(var1) + RefFixnum(var2) + RefFixnum(var3));
	}

	return 0;
}

static int test_call_callbind_var1opt2(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, var1opt2, test_function_var1opt2);
	setcompiled_var1opt2(pos, p_debug1);
	setargs_va_control(ptr, fixnumh(10), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 1, "call_callbind_var1opt2-1");
	test(test_function_value == 10, "call_callbind_var1opt2-2");

	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 2, "call_callbind_var1opt2-3");
	test(test_function_value == 30, "call_callbind_var1opt2-4");

	setargs_va_control(ptr, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 3, "call_callbind_var1opt2-5");
	test(test_function_value == 60, "call_callbind_var1opt2-6");

	free_control_(ptr, control);

	RETURN;
}

static int test_function_var1rest(Execute ptr, addr var1, addr rest)
{
	addr pos;
	fixnum value;

	test_function_mode = (int)length_list_unsafe(rest);
	value = RefFixnum(var1);
	while (rest != Nil) {
		GetCons(rest, &pos, &rest);
		value += RefFixnum(pos);
	}
	test_function_value = (int)value;

	return 0;
}

static int test_call_callbind_var1rest(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, var1rest, test_function_var1rest);
	setcompiled_var1rest(pos, p_debug1);
	setargs_va_control(ptr, fixnumh(10), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 0, "call_callbind_var1rest1");
	test(test_function_value == 10, "call_callbind_var1rest2");

	setargs_va_control(ptr, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 2, "call_callbind_var1rest3");
	test(test_function_value == 60, "call_callbind_var1rest4");

	free_control_(ptr, control);

	RETURN;
}

static int test_function_var2rest(Execute ptr, addr var1, addr var2, addr rest)
{
	addr pos;
	fixnum value;

	test_function_mode = (int)length_list_unsafe(rest);
	value = RefFixnum(var1) + RefFixnum(var2);
	while (rest != Nil) {
		GetCons(rest, &pos, &rest);
		value += RefFixnum(pos);
	}
	test_function_value = (int)value;

	return 0;
}

static int test_call_callbind_var2rest(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	compiled_heap(&pos, Nil);

	SetPointer(p_debug1, var2rest, test_function_var2rest);
	setcompiled_var2rest(pos, p_debug1);
	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 0, "call_callbind_var2rest1");
	test(test_function_value == 30, "call_callbind_var2rest2");

	setargs_va_control(ptr, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	setvalues_nil_control(ptr);
	call_compiled_function(ptr, pos);
	test(test_function_mode == 1, "call_callbind_var2rest3");
	test(test_function_value == 60, "call_callbind_var2rest4");

	free_control_(ptr, control);

	RETURN;
}


/*
 *  Main
 */
static int testbreak_control_callbind(void)
{
	TestBreak(test_call_callbind_code);
	TestBreak(test_call_callbind_any);
	TestBreak(test_call_callbind_empty);
	TestBreak(test_call_callbind_dynamic);
	TestBreak(test_call_callbind_rest);
	TestBreak(test_call_callbind_var1);
	TestBreak(test_call_callbind_var2);
	TestBreak(test_call_callbind_var3);
	TestBreak(test_call_callbind_opt1);
	TestBreak(test_call_callbind_opt2);
	TestBreak(test_call_callbind_opt3);
	TestBreak(test_call_callbind_var1opt1);
	TestBreak(test_call_callbind_var2opt1);
	TestBreak(test_call_callbind_var1opt2);
	TestBreak(test_call_callbind_var1rest);
	TestBreak(test_call_callbind_var2rest);

	return 0;
}

int test_control_callbind(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_character();
		build_package();
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_syscall();
		build_common();
		build_readtable();
		build_pathname();
		build_code();
		lisp_initialize = 1;
		result = testbreak_control_callbind();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

