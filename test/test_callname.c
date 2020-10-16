#include "callname.c"
#include "clos.h"
#include "code.h"
#include "code_object.h"
#include "control_object.h"
#include "control_operator.h"
#include "common.h"
#include "degrade.h"
#include "package.h"
#include "package_intern.h"
#include "sequence.h"
#include "strvect.h"
#include "type.h"
#include "type_table.h"

static int test_getcallname(void)
{
	addr pos, name, check;

	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	pos = Nil;
	parse_callname_heap(&pos, name);
	test(refcallname(pos) == name, "getcallname.1");
	internchar_debug(LISP_COMMON_USER, "AAA", &name);
	setcallname(pos, name);
	getcallname(pos, &check);
	test(check == name, "getcallname.2");

	RETURN;
}

static int test_getcallnametype(void)
{
	CallNameType check;
	addr pos;

	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	parse_callname_heap(&pos, pos);
	test(refcallnametype(pos) == CALLNAME_SYMBOL, "getcallnametype.1");
	setcallnametype(pos, CALLNAME_SETF);
	getcallnametype(pos, &check);
	test(check == CALLNAME_SETF, "getcallnametype.2");

	RETURN;
}

static int test_callname_alloc(void)
{
	addr name, pos, check;
	LocalRoot local;
	LocalStack stack;

	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	callname_alloc(NULL, &pos, name, CALLNAME_SYMBOL);
	test(GetType(pos) == LISPTYPE_CALLNAME, "callname_alloc.1");
	GetCallName_Low(pos, &check);
	test(check == name, "callname_alloc.2");
	test(RefCallNameType(pos) == CALLNAME_SYMBOL, "callname_alloc.3");

	local = Local_Thread;
	push_local(local, &stack);
	callname_local(local, &pos, name, CALLNAME_SETF);
	test(GetType(pos) == LISPTYPE_CALLNAME, "callname_alloc.4");
	test(GetStatusDynamic(pos), "callname_alloc.5");
	rollback_local(local, stack);

	callname_heap(&pos, name, CALLNAME_SETF);
	test(GetType(pos) == LISPTYPE_CALLNAME, "callname_alloc.6");
	test(! GetStatusDynamic(pos), "callname_alloc.7");

	RETURN;
}

static int test_copy_callname_alloc(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	parse_callname_heap(&pos, pos);
	check = NULL;
	copy_callname_alloc(local, &check, pos);
	test(equal_callname(check, pos), "copy_callname_alloc.1");

	SetCallNameType(pos, CALLNAME_SETF);
	check = NULL;
	copy_callname_local(local, &check, pos);
	test(equal_callname(check, pos), "copy_callname_alloc.2");
	test(GetStatusDynamic(check), "copy_callname_alloc.3");

	pos = NULL;
	copy_callname_heap(&pos, check);
	test(equal_callname(check, pos), "copy_callname_alloc.4");
	test(! GetStatusDynamic(pos), "copy_callname_alloc.5");

	rollback_local(local, stack);

	RETURN;
}

static int test_parse_callname(void)
{
	addr pos, check, setf, symbol;

	test(parse_callname(Nil, &check) == CALLNAME_SYMBOL, "parse_callname.1");
	test(check == Nil, "parse_callname.2");
	test(parse_callname(T, &check) == CALLNAME_SYMBOL, "parse_callname.3");
	test(check == T, "parse_callname.4");
	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	test(parse_callname(pos, &check) == CALLNAME_SYMBOL, "parse_callname.5");
	test(check == pos, "parse_callname.6");
	GetConstant(CONSTANT_COMMON_SETF, &setf);
	internchar_debug(LISP_COMMON_USER, "HELLO", &symbol);
	list_heap(&pos, setf, symbol, NULL);
	test(parse_callname(pos, &check) == CALLNAME_SETF, "parse_callname.7");
	test(check == symbol, "parse_callname.8");

	fixnum_heap(&pos, 10);
	test(parse_callname(pos, &check) == CALLNAME_ERROR, "parse_callname.9");
	fixnum_heap(&pos, 10);
	internchar_debug(LISP_COMMON_USER, "HELLO", &symbol);
	list_heap(&pos, pos, symbol, NULL);
	test(parse_callname(pos, &check) == CALLNAME_ERROR, "parse_callname.10");
	list_heap(&pos, setf, NULL);
	test(parse_callname(pos, &check) == CALLNAME_ERROR, "parse_callname.11");
	cons_heap(&pos, setf, T);
	test(parse_callname(pos, &check) == CALLNAME_ERROR, "parse_callname.12");
	fixnum_heap(&pos, 10);
	list_heap(&pos, setf, pos, NULL);
	test(parse_callname(pos, &check) == CALLNAME_ERROR, "parse_callname.13");
	fixnum_heap(&pos, 10);
	list_heap(&pos, setf, symbol, pos, NULL);
	test(parse_callname(pos, &check) == CALLNAME_ERROR, "parse_callname.14");

	RETURN;
}

static int test_parse_callname_alloc(void)
{
	addr name, pos, check;
	LocalRoot local;
	LocalStack stack;

	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	test(parse_callname_alloc(NULL, &pos, name) == 0, "parse_callname_alloc.1");
	test(GetType(pos) == LISPTYPE_CALLNAME, "parse_callname_alloc.2");
	GetCallName(pos, &check);
	test(check == name, "parse_callname_alloc.3");
	test(RefCallNameType(pos) == CALLNAME_SYMBOL, "parse_callname_alloc.4");

	fixnum_heap(&name, 100);
	test(parse_callname_alloc(NULL, &pos, name), "parse_callname_alloc.5");

	local = Local_Thread;
	push_local(local, &stack);
	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	test(parse_callname_local(local, &pos, name) == 0, "parse_callname_alloc.6");
	test(GetType(pos) == LISPTYPE_CALLNAME, "parse_callname_alloc.7");
	test(GetStatusDynamic(pos), "parse_callname_alloc.8");
	rollback_local(local, stack);

	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	test(parse_callname_heap(&pos, name) == 0, "parse_callname_alloc.9");
	test(GetType(pos) == LISPTYPE_CALLNAME, "parse_callname_alloc.10");
	test(! GetStatusDynamic(pos), "parse_callname_alloc.11");

	RETURN;
}

static int test_function_name_p(void)
{
	addr pos, name;

	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	callname_heap(&pos, name, CALLNAME_SYMBOL);
	test(function_name_p(pos), "function_name_p.1");

	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	test(function_name_p(name), "function_name_p.2");

	fixnum_heap(&pos, 100);
	test(! function_name_p(pos), "function_name_p.3");

	RETURN;
}

static int test_constantp_callname(void)
{
	addr pos;

	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	parse_callname_heap(&pos, pos);
	test(! constantp_callname(pos), "constantp_callname.1");

	parse_callname_heap(&pos, Nil);
	test(constantp_callname(pos), "constantp_callname.2");

	RETURN;
}

static int test_equal_callname(void)
{
	addr left, right;

	internchar_debug(LISP_COMMON_USER, "HELLO", &left);
	internchar_debug(LISP_COMMON_USER, "HELLO", &right);
	parse_callname_heap(&left, left);
	parse_callname_heap(&right, right);
	test(equal_callname(left, right), "equal_callname.1");

	internchar_debug(LISP_COMMON_USER, "AAA", &left);
	internchar_debug(LISP_COMMON_USER, "HELLO", &right);
	parse_callname_heap(&left, left);
	parse_callname_heap(&right, right);
	test(! equal_callname(left, right), "equal_callname.2");

	internchar_debug(LISP_COMMON_USER, "HELLO", &left);
	internchar_debug(LISP_COMMON_USER, "HELLO", &right);
	parse_callname_heap(&left, left);
	parse_callname_heap(&right, right);
	SetCallNameType(right, CALLNAME_SETF);
	test(! equal_callname(left, right), "equal_callname.3");

	RETURN;
}

static int test_getfunction_callname_global(void)
{
	addr pos, value1, value2, check;

	fixnum_heap(&value1, 100);
	fixnum_heap(&value2, 200);
	symbol_heap(&pos);
	SetFunctionSymbol(pos, value1);
	setsetf_symbol(pos, value2);
	parse_callname_heap(&pos, pos);
	getglobal_callname(pos, &check);
	test(check == value1, "getglobal_callname.1");
	SetCallNameType(pos, CALLNAME_SETF);
	getglobal_callname(pos, &check);
	test(check == value2, "getglobal_callname.2");

	RETURN;
}

static void test_setfcons(addr *ret, addr symbol)
{
	addr setf;
	GetConstant(CONSTANT_COMMON_SETF, &setf);
	list_heap(ret, setf, symbol, NULL);
}

static int test_callnametype(void)
{
	addr name, pos, check;

	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	parse_callname_heap(&pos, name);
	check = NULL;
	test(callnametype(pos, &check) == CALLNAME_SYMBOL, "callnametype.1");
	test(check == name, "callnametype.2");

	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	test_setfcons(&pos, name);
	check = NULL;
	test(callnametype(pos, &check) == CALLNAME_SETF, "callnametype.3");
	test(check == name, "callnametype.4");

	RETURN;
}

static void test_reinternchar(const char *name, addr *ret)
{
	int check;
	addr package, symbol;

	internchar_debug(LISP_COMMON_USER, name, &symbol);
	Error(find_char_package_(LISP_COMMON_USER, &package));
	unintern_package_(package, symbol, &check);
	internchar_debug(LISP_COMMON_USER, name, ret);
}

static int test_getcallname_global(void)
{
	addr control, pos, call1, call2, symbol, key;
	Execute ptr;

	ptr = Execute_Thread;
	push_control(ptr, &control);
	test_reinternchar("HELLO", &symbol);

	getglobal_parse_callname(symbol, &pos);
	test(pos == Unbound, "getglobal_parse_callname.1");

	fixnum_heap(&call1, 10);
	fixnum_heap(&call2, 20);
	setfunction_symbol(symbol, call1);
	setsetf_symbol(symbol, call2);

	getglobal_parse_callname(symbol, &pos);
	test(pos == call1, "getglobal_parse_callname.2");
	test_setfcons(&key, symbol);
	getglobal_parse_callname(key, &pos);
	test(pos == call2, "getglobal_parse_callname.3");

	setglobal_parse_callname_(symbol, call2);
	setglobal_parse_callname_(key, call1);
	getglobal_parse_callname(symbol, &pos);
	test(pos == call2, "getglobal_parse_callname.4");
	getglobal_parse_callname(key, &pos);
	test(pos == call1, "getglobal_parse_callname.5");

	pop_control_(ptr, control);

	RETURN;
}

static int test_getcallnamecheck_global(void)
{
	addr control, pos, call1, call2, symbol, key;
	Execute ptr;

	ptr = Execute_Thread;
	push_control(ptr, &control);
	test_reinternchar("HELLO", &symbol);

	fixnum_heap(&call1, 10);
	fixnum_heap(&call2, 20);
	setfunction_symbol(symbol, call1);
	setsetf_symbol(symbol, call2);

	getglobalcheck_parse_callname_(symbol, &pos);
	test(pos == call1, "getglobalcheck_parse_callname.1");
	test_setfcons(&key, symbol);
	getglobalcheck_parse_callname_(key, &pos);
	test(pos == call2, "getglobalcheck_parse_callname.2");

	setglobal_parse_callname_(symbol, call2);
	setglobal_parse_callname_(key, call1);
	getglobalcheck_parse_callname_(symbol, &pos);
	test(pos == call2, "getglobalcheck_parse_callname.3");
	getglobalcheck_parse_callname_(key, &pos);
	test(pos == call1, "getglobalcheck_parse_callname.4");

	pop_control_(ptr, control);

	RETURN;
}


/*
 *  callname
 */
static int testcase_callname(void)
{
	TestBreak(test_getcallname);
	TestBreak(test_getcallnametype);
	TestBreak(test_callname_alloc);
	TestBreak(test_copy_callname_alloc);
	TestBreak(test_parse_callname);
	TestBreak(test_parse_callname_alloc);
	TestBreak(test_function_name_p);
	TestBreak(test_constantp_callname);
	TestBreak(test_equal_callname);
	TestBreak(test_getfunction_callname_global);
	TestBreak(test_callnametype);
	TestBreak(test_getcallname_global);
	TestBreak(test_getcallnamecheck_global);

	return 0;
}

static void testinit_callname(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_package();
	build_symbol();
	build_clos(ptr);
	build_condition(ptr);
	build_type();
	build_common();
}

int test_callname(void)
{
	DegradeTitle;
	return DegradeCode(callname);
}

