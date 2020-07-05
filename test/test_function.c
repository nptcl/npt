#include "function.c"
#include "clos.h"
#include "code.h"
#include "code_object.h"
#include "control_object.h"
#include "control_operator.h"
#include "common.h"
#include "degrade.h"
#include "package.h"
#include "package_symbol.h"
#include "sequence.h"
#include "strvect.h"
#include "type.h"
#include "type_table.h"

static int test_alloc_function(void)
{
	addr pos, name;
	struct function_struct *str;

	alloc_function(NULL, &pos, Nil, Nil, 1, 0);
	test(GetType(pos) == LISPTYPE_FUNCTION, "alloc_function.1");
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "alloc_function.2");
	GetNameFunction(pos, &name);
	test(name == Nil, "alloc_function.3");
	GetCodeFunction(pos, &name);
	test(name == Nil, "alloc_function.4");
	str = StructFunction_Low(pos);
	test(str->macro, "alloc_function.5");
	test(str->compiled == 0, "alloc_function.6");
	test(str->trace == 0, "alloc_function.7");
	test(str->index == p_empty, "alloc_function.8");

	RETURN;
}

static void  code_empty_heap(addr *ret)
{
	addr pos;

	vector4_heap(&pos, 0);
	code_heap(&pos, pos);
	*ret = pos;
}

static int test_function_alloc(void)
{
	addr pos, code;
	struct function_struct *ptr;
	LocalRoot local;
	LocalStack stack;

	code_empty_heap(&code);
	function_alloc(NULL, &pos, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "function_alloc.1");
	ptr = StructFunction_Low(pos);
	test(ptr->macro == 0, "function_alloc.2");
	test(ptr->compiled == 0, "function_alloc.3");
	test(ptr->trace == 0, "function_alloc.4");

	local = Local_Thread;
	push_local(local, &stack);
	function_local(local, &pos, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "function_alloc.5");
	test(GetStatusDynamic(pos), "function_alloc.6");
	rollback_local(local, stack);

	function_heap(&pos, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "function_alloc.7");
	test(! GetStatusDynamic(pos), "function_alloc.8");

	/* for develop */
	function_heap_for_develop(&pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "function_alloc.9");

	RETURN;
}

static int test_macro_alloc(void)
{
	addr pos, code;
	struct function_struct *ptr;
	LocalRoot local;
	LocalStack stack;

	code_empty_heap(&code);
	macro_alloc(NULL, &pos, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "macro_alloc.1");
	ptr = StructFunction_Low(pos);
	test(ptr->macro, "macro_alloc.2");
	test(ptr->compiled == 0, "macro_alloc.3");
	test(ptr->trace == 0, "macro_alloc.4");

	local = Local_Thread;
	push_local(local, &stack);
	macro_local(local, &pos, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "macro_alloc.5");
	test(GetStatusDynamic(pos), "macro_alloc.6");
	rollback_local(local, stack);

	macro_heap(&pos, Nil, code);
	test(GetType(pos) == LISPTYPE_FUNCTION, "macro_alloc.7");
	test(! GetStatusDynamic(pos), "macro_alloc.8");

	RETURN;
}

static int test_compiled_alloc(void)
{
	addr pos;
	struct function_struct *ptr;
	LocalRoot local;
	LocalStack stack;

	compiled_alloc(NULL, &pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_alloc.1");
	ptr = StructFunction_Low(pos);
	test(ptr->macro == 0, "compiled_alloc.2");
	test(ptr->compiled, "compiled_alloc.3");
	test(ptr->trace == 0, "compiled_alloc.4");

	local = Local_Thread;
	push_local(local, &stack);
	compiled_local(local, &pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_alloc.5");
	test(GetStatusDynamic(pos), "compiled_alloc.6");
	rollback_local(local, stack);

	compiled_heap(&pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_alloc.7");
	test(! GetStatusDynamic(pos), "compiled_alloc.8");

	RETURN;
}

static int test_compiled_macro_alloc(void)
{
	addr pos;
	struct function_struct *ptr;
	LocalRoot local;
	LocalStack stack;

	compiled_macro_alloc(NULL, &pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_macro_alloc.1");
	ptr = StructFunction_Low(pos);
	test(ptr->macro, "compiled_macro_alloc.2");
	test(ptr->compiled, "compiled_macro_alloc.3");
	test(ptr->trace == 0, "compiled_macro_alloc.4");

	local = Local_Thread;
	push_local(local, &stack);
	compiled_macro_local(local, &pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_macro_alloc.5");
	test(GetStatusDynamic(pos), "compiled_macro_alloc.6");
	rollback_local(local, stack);

	compiled_macro_heap(&pos, Nil);
	test(GetType(pos) == LISPTYPE_FUNCTION, "compiled_macro_alloc.7");
	test(! GetStatusDynamic(pos), "compiled_macro_alloc.8");

	RETURN;
}

static int test_function_accessor(void)
{
	addr pos, value, check;

	function_heap_for_develop(&pos, Nil);
	test(structfunction(pos) == StructFunction_Low(pos), "function_accessor.1");
	fixnum_heap(&value, 10);
	SetCodeFunction_Low(pos, value);
	GetCodeFunction_Low(pos, &check);
	test(check == value, "function_accessor.2");
	SetCodeFunction(pos, T);
	GetCodeFunction(pos, &value);
	test(value == T, "function_accessor.3");

	internchar(LISP_PACKAGE, "HELLO", &value);
	SetNameFunction_Low(pos, value);
	GetNameFunction_Low(pos, &check);
	test(check == value, "function_accessor.4");
	internchar(LISP_PACKAGE, "AAA", &value);
	parse_callname_heap(&check, value);
	SetNameFunction(pos, check);
	GetNameFunction(pos, &check);
	GetCallName(check, &check);
	test(value == check, "function_accessor.5");

	strvect_char_heap(&value, "HELLO");
	SetDataFunction_Low(pos, value);
	GetDataFunction_Low(pos, &check);
	test(check == value, "function_accessor.6");
	strvect_char_heap(&value, "AAA");
	SetDataFunction(pos, value);
	GetDataFunction(pos, &check);
	test(value == check, "function_accessor.7");

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
	test(GetType(value) == LISPTYPE_FIXNUM, "getplist_function.1");
	test(RefFixnum(value) == 10, "getplist_function.2");

	RETURN;
}

static int test_functionp(void)
{
	addr pos;

	function_heap_for_develop(&pos, Nil);
	test(functionp(pos), "functionp.1");
	fixnum_heap(&pos, 10);
	test(! functionp(pos), "functionp.2");

	RETURN;
}

static int test_funcall_function_p(void)
{
	addr pos, code;

	code_empty_heap(&code);
	function_heap(&pos, Nil, code);
	test(funcall_function_p(pos), "funcall_function_p.1");
	macro_heap(&pos, Nil, code);
	test(! funcall_function_p(pos), "funcall_function_p.2");
	compiled_heap(&pos, Nil);
	test(funcall_function_p(pos), "funcall_function_p.3");
	compiled_macro_heap(&pos, Nil);
	test(! funcall_function_p(pos), "funcall_function_p.4");

	RETURN;
}

static int test_macro_function_p(void)
{
	addr pos, code;

	code_empty_heap(&code);
	function_heap(&pos, Nil, code);
	test(! macro_function_p(pos), "macro_function_p.1");
	macro_heap(&pos, Nil, code);
	test(macro_function_p(pos), "macro_function_p.2");
	compiled_heap(&pos, Nil);
	test(! macro_function_p(pos), "macro_function_p.3");
	compiled_macro_heap(&pos, Nil);
	test(macro_function_p(pos), "macro_function_p.4");

	RETURN;
}

static int test_interpreted_function_p(void)
{
	addr pos, code;

	code_empty_heap(&code);
	function_heap(&pos, Nil, code);
	test(interpreted_function_p(pos), "interpreted_function_p.1");
	macro_heap(&pos, Nil, code);
	test(interpreted_function_p(pos), "interpreted_function_p.2");
	compiled_heap(&pos, Nil);
	test(! interpreted_function_p(pos), "interpreted_function_p.3");
	compiled_macro_heap(&pos, Nil);
	test(! interpreted_function_p(pos), "interpreted_function_p.4");

	RETURN;
}

static int test_interpreted_funcall_function_p(void)
{
	addr pos, code;

	code_empty_heap(&code);
	function_heap(&pos, Nil, code);
	test(interpreted_funcall_function_p(pos), "interpreted_funcall_function_p.1");
	macro_heap(&pos, Nil, code);
	test(! interpreted_funcall_function_p(pos), "interpreted_funcall_function_p.2");
	compiled_heap(&pos, Nil);
	test(! interpreted_funcall_function_p(pos), "interpreted_funcall_function_p.3");
	compiled_macro_heap(&pos, Nil);
	test(! interpreted_funcall_function_p(pos), "interpreted_funcall_function_p.4");

	RETURN;
}

static int test_interpreted_macro_function_p(void)
{
	addr pos, code;

	code_empty_heap(&code);
	function_heap(&pos, Nil, code);
	test(! interpreted_macro_function_p(pos), "interpreted_macro_function_p.1");
	macro_heap(&pos, Nil, code);
	test(interpreted_macro_function_p(pos), "interpreted_macro_function_p.2");
	compiled_heap(&pos, Nil);
	test(! interpreted_macro_function_p(pos), "interpreted_macro_function_p.3");
	compiled_macro_heap(&pos, Nil);
	test(! interpreted_macro_function_p(pos), "interpreted_macro_function_p.4");

	RETURN;
}

static int test_compiled_function_p(void)
{
	addr pos, code;

	code_empty_heap(&code);
	function_heap(&pos, Nil, code);
	test(! compiled_function_p(pos), "compiled_function_p.1");
	macro_heap(&pos, Nil, code);
	test(! compiled_function_p(pos), "compiled_function_p.2");
	compiled_heap(&pos, Nil);
	test(compiled_function_p(pos), "compiled_function_p.3");
	compiled_macro_heap(&pos, Nil);
	test(compiled_function_p(pos), "compiled_function_p.4");

	RETURN;
}

static int test_compiled_funcall_function_p(void)
{
	addr pos, code;

	code_empty_heap(&code);
	function_heap(&pos, Nil, code);
	test(! compiled_funcall_function_p(pos), "compiled_funcall_function_p.1");
	macro_heap(&pos, Nil, code);
	test(! compiled_funcall_function_p(pos), "compiled_funcall_function_p.2");
	compiled_heap(&pos, Nil);
	test(compiled_funcall_function_p(pos), "compiled_funcall_function_p.3");
	compiled_macro_heap(&pos, Nil);
	test(! compiled_funcall_function_p(pos), "compiled_funcall_function_p.4");

	RETURN;
}

static int test_compiled_macro_function_p(void)
{
	addr pos, code;

	code_empty_heap(&code);
	function_heap(&pos, Nil, code);
	test(! compiled_macro_function_p(pos), "compiled_macro_function_p.1");
	macro_heap(&pos, Nil, code);
	test(! compiled_macro_function_p(pos), "compiled_macro_function_p.2");
	compiled_heap(&pos, Nil);
	test(! compiled_macro_function_p(pos), "compiled_macro_function_p.3");
	compiled_macro_heap(&pos, Nil);
	test(compiled_macro_function_p(pos), "compiled_macro_function_p.4");

	RETURN;
}


/*
 *  function
 */
static int testcase_function(void)
{
	TestBreak(test_alloc_function);
	TestBreak(test_function_alloc);
	TestBreak(test_macro_alloc);
	TestBreak(test_compiled_alloc);
	TestBreak(test_compiled_macro_alloc);
	TestBreak(test_function_accessor);
	TestBreak(test_getplist_function);
	TestBreak(test_functionp);
	TestBreak(test_funcall_function_p);
	TestBreak(test_macro_function_p);
	TestBreak(test_interpreted_function_p);
	TestBreak(test_interpreted_funcall_function_p);
	TestBreak(test_interpreted_macro_function_p);
	TestBreak(test_compiled_function_p);
	TestBreak(test_compiled_funcall_function_p);
	TestBreak(test_compiled_macro_function_p);

	return 0;
}

static void testinit_function(Execute ptr)
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

int test_function(void)
{
	TITLE;
	return degrade_code(
			testinit_function,
			testcase_function);
}

