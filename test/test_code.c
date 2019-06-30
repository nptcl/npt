#include "code.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "constant.h"
#include "copy.h"
#include "degrade.h"
#include "execute.h"
#include "readtable.h"
#include "package.h"
#include "pathname.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  code object
 */
static int test_build_code(void)
{
	addr pos;

	build_code();
	internchar(LISP_CODE, "HELLO", &pos);
	GetFunctionSymbol(pos, &pos);
	test(pos != Unbound && pos != Nil, "build_code1");

	RETURN;
}

static int test_make_code_call(void)
{
	addr array, symbol, cons, call, args, check;
	LocalRoot local;
	LocalStack stack;
	const pointer *ptr;

	local = Local_Thread;
	push_local(local, &stack);
	vector4_local(local, &array, 3);
	internchar(LISP_CODE, "HELLO", &symbol);
	list_local(local, &cons, symbol, NULL);
	SetArrayA4(array, 0, cons);
	internchar(LISP_CODE, "NOP", &symbol);
	list_local(local, &cons, symbol, fixnum_heapr(10), NULL);
	SetArrayA4(array, 1, cons);
	internchar(LISP_CODE, "ABORT", &symbol);
	cons_local(local, &cons, symbol, fixnum_heapr(20));
	SetArrayA4(array, 2, cons);

	make_code_call(local, array, 3, &call, &args, &ptr);
	test(GetType(call) == LISPSYSTEM_CODE, "make_code_call1");
	test(GetType(args) == LISPTYPE_VECTOR, "make_code_call2");
	test(GetStatusSize(args) == LISPSIZE_ARRAY4, "make_code_call3");
	test(ptr != NULL, "make_code_call4");
	test(ptr[0] == p_hello_code, "make_code_call5");
	test(ptr[1] == p_nop_code, "make_code_call6");
	test(ptr[2] == p_abort_code, "make_code_call7");

	GetArrayA4(args, 0, &cons);
	test(cons == Nil, "make_code_call8");
	GetArrayA4(args, 1, &cons);
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 10, "make_code_call9");
	test(cons == Nil, "make_code_call10");
	GetArrayA4(args, 2, &check);
	test(RefFixnum(check) == 20, "make_code_call11");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_alloc(void)
{
	addr array, symbol, cons, call, args, check, pos;
	LocalRoot local;
	LocalStack stack;
	struct code_struct *str;

	local = Local_Thread;
	push_local(local, &stack);
	vector4_local(local, &array, 3);
	internchar(LISP_CODE, "HELLO", &symbol);
	list_local(local, &cons, symbol, NULL);
	SetArrayA4(array, 0, cons);
	internchar(LISP_CODE, "NOP", &symbol);
	list_local(local, &cons, symbol, fixnum_heapr(10), NULL);
	SetArrayA4(array, 1, cons);
	internchar(LISP_CODE, "ABORT", &symbol);
	cons_local(local, &cons, symbol, fixnum_heapr(20));
	SetArrayA4(array, 2, cons);

	code_alloc(local, &pos, array);
	test(GetType(pos) == LISPTYPE_CODE, "code_alloc1");
	str = StructCode(pos);
	test(str->size == 3, "code_alloc2");
	test(str->call[0] == p_hello_code, "code_alloc3");

	GetArrayCode(pos, Code_Call, &call);
	test(GetType(call) == LISPSYSTEM_CODE, "code_alloc4");

	GetArrayCode(pos, Code_Argument, &args);
	test(GetType(args) == LISPTYPE_VECTOR, "code_alloc5");
	GetArrayA4(args, 2, &check);
	test(RefFixnum(check) == 20, "code_alloc6");

	test(str->call == getcalltype_code(pos), "getcalltype_code1");
	getargs_code(pos, &check);
	test(check == args, "getargs_code1");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testbreak_code(void)
{
	TestBreak(test_build_code);
	TestBreak(test_make_code_call);
	TestBreak(test_code_alloc);

	return 0;
}

int test_code(void)
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
		lisp_initialize = 1;
		result = testbreak_code();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

