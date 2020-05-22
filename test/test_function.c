#include "function.c"
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

static int test_alloc_function(void)
{
	addr pos, name;
	struct function_struct *ptr;

	pos = alloc_function(NULL, Nil, Nil, 1, 0);
	test(GetType(pos) == LISPTYPE_FUNCTION, "alloc_function1");
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "alloc_function2");
	GetNameFunction(pos, &name);
	test(name == Nil, "alloc_function3");
	GetCodeFunction(pos, &name);
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
	SetCodeFunction_Low(pos, value);
	test(RefCodeFunction(pos) == value, "function_accessor2");
	SetCodeFunction(pos, T);
	GetCodeFunction(pos, &value);
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

