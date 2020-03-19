#include "function.c"
#include "clos.h"
#include "code.h"
#include "control.h"
#include "common.h"
#include "degrade.h"
#include "package.h"
#include "sequence.h"
#include "strvect.h"
#include "type.h"
#include "type_table.h"

/*
 *  callname
 */
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

static int test_parse_setcallname(void)
{
	addr pos, name, list;

	internchar(LISP_PACKAGE, "HELLO", &name);
	parse_callname_heap(&pos, name);
	setfcons(&list, name);
	test(parse_setcallname(pos, list) == 0, "parse_setcallname1");
	GetCallName(pos, &list);
	test(list == name, "parse_setcallname2");
	test(RefCallNameType(pos) == CALLNAME_SETF, "parse_setcallname3");

	fixnum_heap(&name, 10);
	test(parse_setcallname(pos, name), "parse_setcallname4");

	RETURN;
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
	enum CALLNAME_TYPE check;
	addr pos;

	internchar(LISP_PACKAGE, "HELLO", &pos);
	parse_callname_heap(&pos, pos);
	test(refcallnametype(pos) == CALLNAME_SYMBOL, "getcallnametype1");
	setcallnametype(pos, CALLNAME_SETF);
	getcallnametype(pos, &check);
	test(check == CALLNAME_SETF, "getcallnametype2");

	RETURN;
}

static int test_callname_constant_p(void)
{
	addr pos;

	internchar(LISP_PACKAGE, "HELLO", &pos);
	parse_callname_heap(&pos, pos);
	test(! callname_constant_p(pos), "callname_constant_p1");

	parse_callname_heap(&pos, Nil);
	test(callname_constant_p(pos), "callname_constant_p2");

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
	getfunction_callname_global(pos, &check);
	test(check == value1, "getfunction_callname_global1");
	SetCallNameType(pos, CALLNAME_SETF);
	getfunction_callname_global(pos, &check);
	test(check == value2, "getfunction_callname_global2");

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

static int test_refcallname_local(void)
{
	addr control, pos, call1, call2, symbol, key;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	reinternchar("HELLO", &symbol);

	pos = refcallname_local(ptr, symbol);
	test(pos == Unbound, "refcallname_local1");

	fixnum_heap(&call1, 10);
	fixnum_heap(&call2, 20);
	setfunction_symbol(symbol, call1);
	setsetf_symbol(symbol, call2);
	pos = refcallname_local(ptr, symbol);
	test(pos == call1, "refcallname_local2");
	setfcons(&key, symbol);
	pos = refcallname_local(ptr, key);
	test(pos == call2, "refcallname_local3");

	pushfunction_control(ptr, symbol, Nil);
	pushsetf_control(ptr, symbol, Nil);
	setcallname_local(ptr, symbol, call2);
	setcallname_local(ptr, key, call1);
	getcallname_local(ptr, symbol, &pos);
	test(pos == call2, "refcallname_local4");
	getcallname_local(ptr, key, &pos);
	test(pos == call1, "refcallname_local5");

	free_control(ptr, control);

	RETURN;
}

static int test_refcallnamecheck_local(void)
{
	addr control, pos, call1, call2, symbol, key;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	reinternchar("HELLO", &symbol);

	fixnum_heap(&call1, 10);
	fixnum_heap(&call2, 20);
	setfunction_symbol(symbol, call1);
	setsetf_symbol(symbol, call2);
	pos = refcallnamecheck_local(ptr, symbol);
	test(pos == call1, "refcallnamecheck_local1");
	setfcons(&key, symbol);
	pos = refcallnamecheck_local(ptr, key);
	test(pos == call2, "refcallnamecheck_local2");

	pushfunction_control(ptr, symbol, Nil);
	pushsetf_control(ptr, symbol, Nil);
	setcallname_local(ptr, symbol, call2);
	setcallname_local(ptr, key, call1);
	getcallname_local(ptr, symbol, &pos);
	test(pos == call2, "refcallnamecheck_local3");
	getcallname_local(ptr, key, &pos);
	test(pos == call1, "refcallnamecheck_local4");

	free_control(ptr, control);

	RETURN;
}

static int test_refcallname_global(void)
{
	addr control, pos, call1, call2, call3, call4, symbol, key;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	reinternchar("HELLO", &symbol);

	pos = refcallname_global(symbol);
	test(pos == Unbound, "refcallname_global");

	fixnum_heap(&call1, 10);
	fixnum_heap(&call2, 20);
	setfunction_symbol(symbol, call1);
	setsetf_symbol(symbol, call2);
	fixnum_heap(&call3, 30);
	fixnum_heap(&call4, 40);
	pushfunction_control(ptr, symbol, call3);
	pushsetf_control(ptr, symbol, call4);

	pos = refcallname_global(symbol);
	test(pos == call1, "refcallname_global2");
	setfcons(&key, symbol);
	pos = refcallname_global(key);
	test(pos == call2, "refcallname_global3");

	setcallname_global(symbol, call2);
	setcallname_global(key, call1);
	getcallname_global(symbol, &pos);
	test(pos == call2, "refcallname_global4");
	getcallname_global(key, &pos);
	test(pos == call1, "refcallname_global5");

	free_control(ptr, control);

	RETURN;
}

static int test_refcallnamecheck_global(void)
{
	addr control, pos, call1, call2, call3, call4, symbol, key;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	reinternchar("HELLO", &symbol);

	fixnum_heap(&call1, 10);
	fixnum_heap(&call2, 20);
	setfunction_symbol(symbol, call1);
	setsetf_symbol(symbol, call2);
	fixnum_heap(&call3, 30);
	fixnum_heap(&call4, 40);
	pushfunction_control(ptr, symbol, call3);
	pushsetf_control(ptr, symbol, call4);

	pos = refcallnamecheck_global(symbol);
	test(pos == call1, "refcallnamecheck_global1");
	setfcons(&key, symbol);
	pos = refcallnamecheck_global(key);
	test(pos == call2, "refcallnamecheck_global2");

	setcallname_global(symbol, call2);
	setcallname_global(key, call1);
	getcallnamecheck_global(symbol, &pos);
	test(pos == call2, "refcallnamecheck_global3");
	getcallnamecheck_global(key, &pos);
	test(pos == call1, "refcallnamecheck_global4");

	free_control(ptr, control);

	RETURN;
}


/*
 *  function
 */
static int test_alloc_function(void)
{
	addr pos, name;
	struct function_struct *ptr;

	pos = alloc_function(NULL, Nil, Nil, 1, 0);
	test(GetType(pos) == LISPTYPE_FUNCTION, "alloc_function1");
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "alloc_function2");
	GetNameFunction(pos, &name);
	test(name == Nil, "alloc_function3");
	GetFunction(pos, &name);
	test(name == Nil, "alloc_function4");
	ptr = StructFunction_Low(pos);
	test(ptr->macro, "alloc_function5");
	test(ptr->compiled == 0, "alloc_function6");
	test(ptr->system == 0, "alloc_function7");
	test(ptr->index == p_empty, "alloc_function8");

	RETURN;
}

static void  code_empty(addr *ret)
{
	addr pos;

	vector4_heap(&pos, 0);
	code_heap(&pos, pos);
	*ret = pos;
}

static int test_function_allocr(void)
{
	addr pos, code;
	struct function_struct *ptr;
	LocalRoot local;
	LocalStack stack;

	code_empty(&code);
	pos = function_allocr(NULL, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "function_allocr1");
	ptr = StructFunction_Low(pos);
	test(ptr->macro == 0, "function_allocr2");
	test(ptr->compiled == 0, "function_allocr3");

	local = Local_Thread;
	push_local(local, &stack);
	pos = function_localr(local, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "function_allocr4");
	test(GetStatusDynamic(pos), "function_allocr5");
	rollback_local(local, stack);

	pos = function_heapr(Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "function_allocr6");
	test(! GetStatusDynamic(pos), "function_allocr7");

	RETURN;
}

static int test_function_alloc(void)
{
	addr pos, code;
	struct function_struct *ptr;
	LocalRoot local;
	LocalStack stack;

	code_empty(&code);
	function_alloc(NULL, &pos, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "function_alloc1");
	ptr = StructFunction_Low(pos);
	test(ptr->macro == 0, "function_alloc2");
	test(ptr->compiled == 0, "function_alloc3");

	local = Local_Thread;
	push_local(local, &stack);
	function_local(local, &pos, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "function_alloc4");
	test(GetStatusDynamic(pos), "function_alloc5");
	rollback_local(local, stack);

	function_heap(&pos, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "function_alloc6");
	test(! GetStatusDynamic(pos), "function_alloc7");

	RETURN;
}

static int test_macro_allocr(void)
{
	addr pos, code;
	struct function_struct *ptr;
	LocalRoot local;
	LocalStack stack;

	code_empty(&code);
	pos = macro_allocr(NULL, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "macro_allocr1");
	ptr = StructFunction_Low(pos);
	test(ptr->macro, "macro_allocr2");
	test(ptr->compiled == 0, "macro_allocr3");

	local = Local_Thread;
	push_local(local, &stack);
	pos = macro_localr(local, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "macro_allocr4");
	test(GetStatusDynamic(pos), "macro_allocr5");
	rollback_local(local, stack);

	pos = macro_heapr(Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "macro_allocr6");
	test(! GetStatusDynamic(pos), "macro_allocr7");

	RETURN;
}

static int test_macro_alloc(void)
{
	addr pos, code;
	struct function_struct *ptr;
	LocalRoot local;
	LocalStack stack;

	code_empty(&code);
	macro_alloc(NULL, &pos, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "macro_alloc1");
	ptr = StructFunction_Low(pos);
	test(ptr->macro, "macro_alloc2");
	test(ptr->compiled == 0, "macro_alloc3");

	local = Local_Thread;
	push_local(local, &stack);
	macro_local(local, &pos, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "macro_alloc4");
	test(GetStatusDynamic(pos), "macro_alloc5");
	rollback_local(local, stack);

	macro_heap(&pos, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "macro_alloc6");
	test(! GetStatusDynamic(pos), "macro_alloc7");

	RETURN;
}

static int test_compiled_allocr(void)
{
	addr pos;
	struct function_struct *ptr;
	LocalRoot local;
	LocalStack stack;

	pos = compiled_allocr(NULL, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_allocr1");
	ptr = StructFunction_Low(pos);
	test(ptr->macro == 0, "compiled_allocr2");
	test(ptr->compiled, "compiled_allocr3");

	local = Local_Thread;
	push_local(local, &stack);
	pos = compiled_localr(local, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_allocr4");
	test(GetStatusDynamic(pos), "compiled_allocr5");
	rollback_local(local, stack);

	pos = compiled_heapr(Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_allocr6");
	test(! GetStatusDynamic(pos), "compiled_allocr7");

	RETURN;
}

static int test_compiled_alloc(void)
{
	addr pos;
	struct function_struct *ptr;
	LocalRoot local;
	LocalStack stack;

	compiled_alloc(NULL, &pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_alloc1");
	ptr = StructFunction_Low(pos);
	test(ptr->macro == 0, "compiled_alloc2");
	test(ptr->compiled, "compiled_alloc3");

	local = Local_Thread;
	push_local(local, &stack);
	compiled_local(local, &pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_alloc4");
	test(GetStatusDynamic(pos), "compiled_alloc5");
	rollback_local(local, stack);

	compiled_heap(&pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_alloc6");
	test(! GetStatusDynamic(pos), "compiled_alloc7");

	RETURN;
}

static int test_compiled_macro_allocr(void)
{
	addr pos;
	struct function_struct *ptr;
	LocalRoot local;
	LocalStack stack;

	pos = compiled_macro_allocr(NULL, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_macro_allocr1");
	ptr = StructFunction_Low(pos);
	test(ptr->macro, "compiled_macro_allocr2");
	test(ptr->compiled, "compiled_macro_allocr3");

	local = Local_Thread;
	push_local(local, &stack);
	pos = compiled_macro_localr(local, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_macro_allocr4");
	test(GetStatusDynamic(pos), "compiled_macro_allocr5");
	rollback_local(local, stack);

	pos = compiled_macro_heapr(Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_macro_allocr6");
	test(! GetStatusDynamic(pos), "compiled_macro_allocr7");

	RETURN;
}

static int test_compiled_macro_alloc(void)
{
	addr pos;
	struct function_struct *ptr;
	LocalRoot local;
	LocalStack stack;

	compiled_macro_alloc(NULL, &pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_macro_alloc1");
	ptr = StructFunction_Low(pos);
	test(ptr->macro, "compiled_macro_alloc2");
	test(ptr->compiled, "compiled_macro_alloc3");

	local = Local_Thread;
	push_local(local, &stack);
	compiled_macro_local(local, &pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_macro_alloc4");
	test(GetStatusDynamic(pos), "compiled_macro_alloc5");
	rollback_local(local, stack);

	compiled_macro_heap(&pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_macro_alloc6");
	test(! GetStatusDynamic(pos), "compiled_macro_alloc7");

	RETURN;
}

static int test_function_heap_for_develop(void)
{
	addr pos;

	function_heap_for_develop(&pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "function_heap_for_develop1");

	RETURN;
}

static int test_function_accessor(void)
{
	addr pos, value, check;

	function_heap_for_develop(&pos, Nil);
	test(structfunction(pos) == StructFunction_Low(pos), "function_accessor1");
	fixnum_heap(&value, 10);
	SetFunction_Low(pos, value);
	test(RefFunction(pos) == value, "function_accessor2");
	SetFunction(pos, T);
	GetFunction(pos, &value);
	test(value == T, "function_accessor3");

	internchar(LISP_PACKAGE, "HELLO", &value);
	SetNameFunction_Low(pos, value);
	test(RefNameFunction(pos) == value, "function_accessor6");
	internchar(LISP_PACKAGE, "AAA", &value);
	parse_callname_heap(&check, value);
	SetNameFunction(pos, check);
	GetNameFunction(pos, &check);
	GetCallName(check, &check);
	test(value == check, "function_accessor7");

	strvect_char_heap(&value, "HELLO");
	SetDataFunction_Low(pos, value);
	test(RefDataFunction(pos) == value, "function_accessor8");
	strvect_char_heap(&value, "AAA");
	SetDataFunction(pos, value);
	GetDataFunction(pos, &check);
	test(value == check, "function_accessor9");

	RETURN;
}

static int test_getplist_function(void)
{
	addr pos, list, key, value;

	function_heap_for_develop(&pos, Nil);
	GetConstant(CONSTANT_SYSTEM_VALUE, &key);
	fixnum_heap(&value, 10);
	list_heap(&list, key, value, NULL);
	SetTableFunction_Low(pos, list);
	getplist_function(pos, CONSTANT_SYSTEM_VALUE, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "getplist_function1");
	test(RefFixnum(value) == 10, "getplist_function2");

	RETURN;
}

static int test_setsystem_function(void)
{
	addr pos;

	function_heap_for_develop(&pos, Nil);
	test(StructFunction_Low(pos)->system == 0, "setsystem_function1");
	setsystem_function(pos);
	test(StructFunction_Low(pos)->system, "setsystem_function2");

	RETURN;
}

static int test_functionp(void)
{
	addr pos;

	function_heap_for_develop(&pos, Nil);
	test(functionp(pos), "functionp1");
	fixnum_heap(&pos, 10);
	test(! functionp(pos), "functionp2");

	RETURN;
}

static int test_funcall_function_p(void)
{
	addr pos, code;

	code_empty(&code);
	function_heap(&pos, Nil, code);
	test(funcall_function_p(pos), "funcall_function_p1");
	macro_heap(&pos, Nil, code);
	test(! funcall_function_p(pos), "funcall_function_p2");
	compiled_heap(&pos, Nil);
	test(funcall_function_p(pos), "funcall_function_p3");
	compiled_macro_heap(&pos, Nil);
	test(! funcall_function_p(pos), "funcall_function_p4");

	RETURN;
}

static int test_macro_function_p(void)
{
	addr pos, code;

	code_empty(&code);
	function_heap(&pos, Nil, code);
	test(! macro_function_p(pos), "macro_function_p1");
	macro_heap(&pos, Nil, code);
	test(macro_function_p(pos), "macro_function_p2");
	compiled_heap(&pos, Nil);
	test(! macro_function_p(pos), "macro_function_p3");
	compiled_macro_heap(&pos, Nil);
	test(macro_function_p(pos), "macro_function_p4");

	RETURN;
}

static int test_interpreted_function_p(void)
{
	addr pos, code;

	code_empty(&code);
	function_heap(&pos, Nil, code);
	test(interpreted_function_p(pos), "interpreted_function_p1");
	macro_heap(&pos, Nil, code);
	test(interpreted_function_p(pos), "interpreted_function_p2");
	compiled_heap(&pos, Nil);
	test(! interpreted_function_p(pos), "interpreted_function_p3");
	compiled_macro_heap(&pos, Nil);
	test(! interpreted_function_p(pos), "interpreted_function_p4");

	RETURN;
}

static int test_interpreted_funcall_function_p(void)
{
	addr pos, code;

	code_empty(&code);
	function_heap(&pos, Nil, code);
	test(interpreted_funcall_function_p(pos), "interpreted_funcall_function_p1");
	macro_heap(&pos, Nil, code);
	test(! interpreted_funcall_function_p(pos), "interpreted_funcall_function_p2");
	compiled_heap(&pos, Nil);
	test(! interpreted_funcall_function_p(pos), "interpreted_funcall_function_p3");
	compiled_macro_heap(&pos, Nil);
	test(! interpreted_funcall_function_p(pos), "interpreted_funcall_function_p4");

	RETURN;
}

static int test_interpreted_macro_function_p(void)
{
	addr pos, code;

	code_empty(&code);
	function_heap(&pos, Nil, code);
	test(! interpreted_macro_function_p(pos), "interpreted_macro_function_p1");
	macro_heap(&pos, Nil, code);
	test(interpreted_macro_function_p(pos), "interpreted_macro_function_p2");
	compiled_heap(&pos, Nil);
	test(! interpreted_macro_function_p(pos), "interpreted_macro_function_p3");
	compiled_macro_heap(&pos, Nil);
	test(! interpreted_macro_function_p(pos), "interpreted_macro_function_p4");

	RETURN;
}

static int test_compiled_function_p(void)
{
	addr pos, code;

	code_empty(&code);
	function_heap(&pos, Nil, code);
	test(! compiled_function_p(pos), "compiled_function_p1");
	macro_heap(&pos, Nil, code);
	test(! compiled_function_p(pos), "compiled_function_p2");
	compiled_heap(&pos, Nil);
	test(compiled_function_p(pos), "compiled_function_p3");
	compiled_macro_heap(&pos, Nil);
	test(compiled_function_p(pos), "compiled_function_p4");

	RETURN;
}

static int test_compiled_funcall_function_p(void)
{
	addr pos, code;

	code_empty(&code);
	function_heap(&pos, Nil, code);
	test(! compiled_funcall_function_p(pos), "compiled_funcall_function_p1");
	macro_heap(&pos, Nil, code);
	test(! compiled_funcall_function_p(pos), "compiled_funcall_function_p2");
	compiled_heap(&pos, Nil);
	test(compiled_funcall_function_p(pos), "compiled_funcall_function_p3");
	compiled_macro_heap(&pos, Nil);
	test(! compiled_funcall_function_p(pos), "compiled_funcall_function_p4");

	RETURN;
}

static int test_compiled_macro_function_p(void)
{
	addr pos, code;

	code_empty(&code);
	function_heap(&pos, Nil, code);
	test(! compiled_macro_function_p(pos), "compiled_macro_function_p1");
	macro_heap(&pos, Nil, code);
	test(! compiled_macro_function_p(pos), "compiled_macro_function_p2");
	compiled_heap(&pos, Nil);
	test(! compiled_macro_function_p(pos), "compiled_macro_function_p3");
	compiled_macro_heap(&pos, Nil);
	test(compiled_macro_function_p(pos), "compiled_macro_function_p4");

	RETURN;
}

static int test_system_function_p(void)
{
	addr pos;

	function_heap_for_develop(&pos, Nil);
	test(! system_function_p(pos), "system_function_p1");
	setsystem_function(pos);
	test(system_function_p(pos), "system_function_p2");

	RETURN;
}


/*
 *  test
 */
static int testbreak_function(void)
{
	/* callname */
	TestBreak(test_callname_allocr);
	TestBreak(test_callname_alloc);
	TestBreak(test_parse_callname);
	TestBreak(test_function_name_p);
	TestBreak(test_parse_callname_alloc);
	TestBreak(test_parse_setcallname);
	TestBreak(test_getcallname);
	TestBreak(test_getcallnametype);
	TestBreak(test_callname_constant_p);
	TestBreak(test_equal_callname);
	TestBreak(test_getfunction_callname_global);
	TestBreak(test_copy_callname_alloc);
	TestBreak(test_callnametype);
	TestBreak(test_refcallname_local);
	TestBreak(test_refcallnamecheck_local);
	TestBreak(test_refcallname_global);
	TestBreak(test_refcallnamecheck_global);
	/* function */
	TestBreak(test_alloc_function);
	TestBreak(test_function_allocr);
	TestBreak(test_function_alloc);
	TestBreak(test_macro_allocr);
	TestBreak(test_macro_alloc);
	TestBreak(test_compiled_allocr);
	TestBreak(test_compiled_alloc);
	TestBreak(test_compiled_macro_allocr);
	TestBreak(test_compiled_macro_alloc);
	TestBreak(test_function_heap_for_develop);
	TestBreak(test_function_accessor);
	TestBreak(test_getplist_function);
	TestBreak(test_setsystem_function);
	TestBreak(test_functionp);
	TestBreak(test_funcall_function_p);
	TestBreak(test_macro_function_p);
	TestBreak(test_interpreted_function_p);
	TestBreak(test_interpreted_funcall_function_p);
	TestBreak(test_interpreted_macro_function_p);
	TestBreak(test_compiled_function_p);
	TestBreak(test_compiled_funcall_function_p);
	TestBreak(test_compiled_macro_function_p);
	TestBreak(test_system_function_p);

	return 0;
}

int test_function(void)
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
		result = testbreak_function();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

