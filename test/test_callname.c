#include "callname.c"
#include "clos.h"
#include "code.h"
#include "code_object.h"
#include "control_object.h"
#include "control_operator.h"
#include "common.h"
#include "degrade.h"
#include "package.h"
#include "sequence.h"
#include "strvect.h"
#include "type.h"
#include "type_table.h"

static int test_callname_allocr(void)
{
	addr name, pos, check;
	LocalRoot local;
	LocalStack stack;

	internchar(LISP_PACKAGE, "HELLO", &name);
	pos = callname_allocr(NULL, name, CALLNAME_SYMBOL);
	test(GetType(pos) == LISPTYPE_CALLNAME, "callname_allocr1");
	GetCallName_Low(pos, &check);
	test(check == name, "callname_allocr2");
	test(RefCallNameType(pos) == CALLNAME_SYMBOL, "callname_allocr3");

	local = Local_Thread;
	push_local(local, &stack);
	pos = callname_localr(local, name, CALLNAME_SETF);
	test(GetType(pos) == LISPTYPE_CALLNAME, "callname_allocr4");
	test(GetStatusDynamic(pos), "callname_allocr5");
	rollback_local(local, stack);

	pos = callname_heapr(name, CALLNAME_SETF);
	test(GetType(pos) == LISPTYPE_CALLNAME, "callname_allocr6");
	test(! GetStatusDynamic(pos), "callname_allocr7");

	RETURN;
}

static int test_callname_alloc(void)
{
	addr name, pos, check;
	LocalRoot local;
	LocalStack stack;

	internchar(LISP_PACKAGE, "HELLO", &name);
	callname_alloc(NULL, &pos, name, CALLNAME_SYMBOL);
	test(GetType(pos) == LISPTYPE_CALLNAME, "callname_alloc1");
	GetCallName_Low(pos, &check);
	test(check == name, "callname_alloc2");
	test(RefCallNameType(pos) == CALLNAME_SYMBOL, "callname_alloc3");

	local = Local_Thread;
	push_local(local, &stack);
	callname_local(local, &pos, name, CALLNAME_SETF);
	test(GetType(pos) == LISPTYPE_CALLNAME, "callname_alloc4");
	test(GetStatusDynamic(pos), "callname_alloc5");
	rollback_local(local, stack);

	callname_heap(&pos, name, CALLNAME_SETF);
	test(GetType(pos) == LISPTYPE_CALLNAME, "callname_alloc6");
	test(! GetStatusDynamic(pos), "callname_alloc7");

	RETURN;
}

static int test_parse_callname(void)
{
	addr pos, check, setf, symbol;

	test(parse_callname(Nil, &check) == CALLNAME_SYMBOL, "parse_callname1");
	test(check == Nil, "parse_callname2");
	test(parse_callname(T, &check) == CALLNAME_SYMBOL, "parse_callname3");
	test(check == T, "parse_callname4");
	internchar(LISP_PACKAGE, "HELLO", &pos);
	test(parse_callname(pos, &check) == CALLNAME_SYMBOL, "parse_callname5");
	test(check == pos, "parse_callname6");
	GetConstant(CONSTANT_COMMON_SETF, &setf);
	internchar(LISP_PACKAGE, "HELLO", &symbol);
	list_heap(&pos, setf, symbol, NULL);
	test(parse_callname(pos, &check) == CALLNAME_SETF, "parse_callname7");
	test(check == symbol, "parse_callname8");

	fixnum_heap(&pos, 10);
	test(parse_callname(pos, &check) == CALLNAME_ERROR, "parse_callname9");
	fixnum_heap(&pos, 10);
	internchar(LISP_PACKAGE, "HELLO", &symbol);
	list_heap(&pos, pos, symbol, NULL);
	test(parse_callname(pos, &check) == CALLNAME_ERROR, "parse_callname10");
	list_heap(&pos, setf, NULL);
	test(parse_callname(pos, &check) == CALLNAME_ERROR, "parse_callname11");
	cons_heap(&pos, setf, T);
	test(parse_callname(pos, &check) == CALLNAME_ERROR, "parse_callname12");
	fixnum_heap(&pos, 10);
	list_heap(&pos, setf, pos, NULL);
	test(parse_callname(pos, &check) == CALLNAME_ERROR, "parse_callname13");
	fixnum_heap(&pos, 10);
	list_heap(&pos, setf, symbol, pos, NULL);
	test(parse_callname(pos, &check) == CALLNAME_ERROR, "parse_callname14");

	RETURN;
}

static int test_function_name_p(void)
{
	addr pos, name;

	internchar(LISP_PACKAGE, "HELLO", &name);
	callname_heap(&pos, name, CALLNAME_SYMBOL);
	test(function_name_p(pos), "function_name_p1");

	internchar(LISP_PACKAGE, "HELLO", &name);
	test(function_name_p(name), "function_name_p2");

	fixnum_heap(&pos, 100);
	test(! function_name_p(pos), "function_name_p3");

	RETURN;
}

static int test_parse_callname_alloc(void)
{
	addr name, pos, check;
	LocalRoot local;
	LocalStack stack;

	internchar(LISP_PACKAGE, "HELLO", &name);
	test(parse_callname_alloc(NULL, &pos, name) == 0, "parse_callname_alloc1");
	test(GetType(pos) == LISPTYPE_CALLNAME, "parse_callname_alloc2");
	GetCallName(pos, &check);
	test(check == name, "parse_callname_alloc3");
	test(RefCallNameType(pos) == CALLNAME_SYMBOL, "parse_callname_alloc4");

	fixnum_heap(&name, 100);
	test(parse_callname_alloc(NULL, &pos, name), "parse_callname_alloc5");

	local = Local_Thread;
	push_local(local, &stack);
	internchar(LISP_PACKAGE, "HELLO", &name);
	test(parse_callname_local(local, &pos, name) == 0, "parse_callname_alloc6");
	test(GetType(pos) == LISPTYPE_CALLNAME, "parse_callname_alloc7");
	test(GetStatusDynamic(pos), "parse_callname_alloc8");
	rollback_local(local, stack);

	internchar(LISP_PACKAGE, "HELLO", &name);
	test(parse_callname_heap(&pos, name) == 0, "parse_callname_alloc9");
	test(GetType(pos) == LISPTYPE_CALLNAME, "parse_callname_alloc10");
	test(! GetStatusDynamic(pos), "parse_callname_alloc11");

	RETURN;
}

static void setfcons(addr *ret, addr symbol)
{
	addr setf;
	GetConstant(CONSTANT_COMMON_SETF, &setf);
	list_heap(ret, setf, symbol, NULL);
}

static int test_getcallname(void)
{
	addr pos, name, check;

	internchar(LISP_PACKAGE, "HELLO", &name);
	parse_callname_heap(&pos, name);
	test(refcallname(pos) == name, "getcallname1");
	internchar(LISP_PACKAGE, "AAA", &name);
	setcallname(pos, name);
	getcallname(pos, &check);
	test(check == name, "getcallname2");

	RETURN;
}

static int test_getcallnametype(void)
{
	CallNameType check;
	addr pos;

	internchar(LISP_PACKAGE, "HELLO", &pos);
	parse_callname_heap(&pos, pos);
	test(refcallnametype(pos) == CALLNAME_SYMBOL, "getcallnametype1");
	setcallnametype(pos, CALLNAME_SETF);
	getcallnametype(pos, &check);
	test(check == CALLNAME_SETF, "getcallnametype2");

	RETURN;
}

static int test_constantp_callname(void)
{
	addr pos;

	internchar(LISP_PACKAGE, "HELLO", &pos);
	parse_callname_heap(&pos, pos);
	test(! constantp_callname(pos), "constantp_callname1");

	parse_callname_heap(&pos, Nil);
	test(constantp_callname(pos), "constantp_callname2");

	RETURN;
}

static int test_equal_callname(void)
{
	addr left, right;

	internchar(LISP_PACKAGE, "HELLO", &left);
	internchar(LISP_PACKAGE, "HELLO", &right);
	parse_callname_heap(&left, left);
	parse_callname_heap(&right, right);
	test(equal_callname(left, right), "equal_callname1");

	internchar(LISP_PACKAGE, "AAA", &left);
	internchar(LISP_PACKAGE, "HELLO", &right);
	parse_callname_heap(&left, left);
	parse_callname_heap(&right, right);
	test(! equal_callname(left, right), "equal_callname2");

	internchar(LISP_PACKAGE, "HELLO", &left);
	internchar(LISP_PACKAGE, "HELLO", &right);
	parse_callname_heap(&left, left);
	parse_callname_heap(&right, right);
	SetCallNameType(right, CALLNAME_SETF);
	test(! equal_callname(left, right), "equal_callname3");

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
	test(check == value1, "getglobal_callname1");
	SetCallNameType(pos, CALLNAME_SETF);
	getglobal_callname(pos, &check);
	test(check == value2, "getglobal_callname2");

	RETURN;
}

static int test_copy_callname_alloc(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	internchar(LISP_PACKAGE, "HELLO", &pos);
	parse_callname_heap(&pos, pos);
	check = NULL;
	copy_callname_alloc(local, &check, pos);
	test(equal_callname(check, pos), "copy_callname_alloc1");

	SetCallNameType(pos, CALLNAME_SETF);
	check = NULL;
	copy_callname_local(local, &check, pos);
	test(equal_callname(check, pos), "copy_callname_alloc2");
	test(GetStatusDynamic(check), "copy_callname_alloc3");

	pos = NULL;
	copy_callname_heap(&pos, check);
	test(equal_callname(check, pos), "copy_callname_alloc4");
	test(! GetStatusDynamic(pos), "copy_callname_alloc5");

	rollback_local(local, stack);

	RETURN;
}

static int test_callnametype(void)
{
	addr name, pos, check;

	internchar(LISP_PACKAGE, "HELLO", &name);
	parse_callname_heap(&pos, name);
	check = NULL;
	test(callnametype(pos, &check) == CALLNAME_SYMBOL, "callnametype1");
	test(check == name, "callnametype2");

	internchar(LISP_PACKAGE, "HELLO", &name);
	setfcons(&pos, name);
	check = NULL;
	test(callnametype(pos, &check) == CALLNAME_SETF, "callnametype3");
	test(check == name, "callnametype4");

	RETURN;
}

static void reinternchar(const char *name, addr *ret)
{
	addr package, symbol;

	internchar(LISP_PACKAGE, name, &symbol);
	find_char_package(LISP_PACKAGE, &package);
	unintern_package(package, symbol);
	internchar(LISP_PACKAGE, name, ret);
}

static int test_getcallname_global(void)
{
	addr control, pos, call1, call2, symbol, key;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	reinternchar("HELLO", &symbol);

	getglobal_parse_callname(symbol, &pos);
	test(pos == Unbound, "getglobal_parse_callname");

	fixnum_heap(&call1, 10);
	fixnum_heap(&call2, 20);
	setfunction_symbol(symbol, call1);
	setsetf_symbol(symbol, call2);

	getglobal_parse_callname(symbol, &pos);
	test(pos == call1, "getglobal_parse_callname2");
	setfcons(&key, symbol);
	getglobal_parse_callname(key, &pos);
	test(pos == call2, "getglobal_parse_callname3");

	setglobal_parse_callname(symbol, call2);
	setglobal_parse_callname(key, call1);
	getglobal_parse_callname(symbol, &pos);
	test(pos == call2, "getglobal_parse_callname4");
	getglobal_parse_callname(key, &pos);
	test(pos == call1, "getglobal_parse_callname5");

	free_control_(ptr, control);

	RETURN;
}

static int test_getcallnamecheck_global(void)
{
	addr control, pos, call1, call2, symbol, key;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	reinternchar("HELLO", &symbol);

	fixnum_heap(&call1, 10);
	fixnum_heap(&call2, 20);
	setfunction_symbol(symbol, call1);
	setsetf_symbol(symbol, call2);

	getglobalcheck_parse_callname(symbol, &pos);
	test(pos == call1, "getglobalcheck_parse_callname1");
	setfcons(&key, symbol);
	getglobalcheck_parse_callname(key, &pos);
	test(pos == call2, "getglobalcheck_parse_callname2");

	setglobal_parse_callname(symbol, call2);
	setglobal_parse_callname(key, call1);
	getglobalcheck_parse_callname(symbol, &pos);
	test(pos == call2, "getglobalcheck_parse_callname3");
	getglobalcheck_parse_callname(key, &pos);
	test(pos == call1, "getglobalcheck_parse_callname4");

	free_control_(ptr, control);

	RETURN;
}


/*
 *  test
 */
static int testbreak_callname(void)
{
	TestBreak(test_callname_allocr);
	TestBreak(test_callname_alloc);
	TestBreak(test_parse_callname);
	TestBreak(test_function_name_p);
	TestBreak(test_parse_callname_alloc);
	TestBreak(test_getcallname);
	TestBreak(test_getcallnametype);
	TestBreak(test_constantp_callname);
	TestBreak(test_equal_callname);
	TestBreak(test_getfunction_callname_global);
	TestBreak(test_copy_callname_alloc);
	TestBreak(test_callnametype);
	TestBreak(test_getcallname_global);
	TestBreak(test_getcallnamecheck_global);

	return 0;
}

int test_callname(void)
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
		build_package();
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_common();
		lisp_initialize = 1;
		result = testbreak_callname();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

