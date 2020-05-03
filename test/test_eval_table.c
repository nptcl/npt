#include "eval_table.c"
#include "character.h"
#include "common.h"
#include "constant.h"
#include "degrade.h"
#include "reader.h"
#include "package.h"
#include "symbol.h"
#include "syscall.h"

/*
 *  tablevalue
 */
static int test_make_tablevalue(void)
{
	addr pos;

	make_tablevalue(NULL, Nil, &pos);
	test(eval_tablevalue_p(pos), "make_tablevalue1");

	RETURN;
}

static int test_gettype_tablevalue(void)
{
	addr pos, value;

	make_tablevalue(NULL, Nil, &pos);
	gettype_tablevalue(pos, &value);
	test(value == Nil, "gettype_tablevalue1");
	settype_tablevalue(pos, T);
	gettype_tablevalue(pos, &value);
	test(value == T, "gettype_tablevalue2");

	RETURN;
}

static int test_getspecialp_tablevalue(void)
{
	addr pos;

	make_tablevalue(NULL, Nil, &pos);
	test(! getspecialp_tablevalue(pos), "getspecialp_tablevalue1");
	setspecialp_tablevalue(pos, 1);
	test(getspecialp_tablevalue(pos), "getspecialp_tablevalue2");
	setspecialp_tablevalue(pos, 2);
	test(getspecialp_tablevalue(pos), "getspecialp_tablevalue3");
	test(StructTableValue(pos)->specialp, "getspecialp_tablevalue4");

	RETURN;
}

static int test_getdynamic_tablevalue(void)
{
	addr pos;

	make_tablevalue(NULL, Nil, &pos);
	test(! getdynamic_tablevalue(pos), "getdynamic_tablevalue1");
	setdynamic_tablevalue(pos, 1);
	test(getdynamic_tablevalue(pos), "getdynamic_tablevalue2");
	setdynamic_tablevalue(pos, 2);
	test(getdynamic_tablevalue(pos), "getdynamic_tablevalue3");
	test(StructTableValue(pos)->dynamic, "getdynamic_tablevalue4");

	RETURN;
}

static int test_getignore_tablevalue(void)
{
	enum IgnoreType type;
	addr pos;

	make_tablevalue(NULL, Nil, &pos);
	type = getignore_tablevalue(pos);
	test(type == IgnoreType_None, "getignore_tablevalue1");
	setignore_tablevalue(pos, IgnoreType_Ignore);
	type = getignore_tablevalue(pos);
	test(type == IgnoreType_Ignore, "getignore_tablevalue2");
	setignore_tablevalue(pos, IgnoreType_Ignorable);
	type = getignore_tablevalue(pos);
	test(type == IgnoreType_Ignorable, "getignore_tablevalue3");
	type = StructTableValue(pos)->ignore;
	test(type == IgnoreType_Ignorable, "getignore_tablevalue4");
	setignore_tablevalue(pos, IgnoreType_None);
	type = getignore_tablevalue(pos);
	test(type == IgnoreType_None, "getignore_tablevalue5");

	RETURN;
}

static int test_getreference_tablevalue(void)
{
	addr pos;

	make_tablevalue(NULL, Nil, &pos);
	test(! getreference_tablevalue(pos), "getreference_tablevalue1");
	setreference_tablevalue(pos, 1);
	test(getreference_tablevalue(pos), "getreference_tablevalue2");
	setreference_tablevalue(pos, 2);
	test(getreference_tablevalue(pos), "getreference_tablevalue3");
	test(StructTableValue(pos)->reference, "getreference_tablevalue4");

	RETURN;
}


/*
 *  Main
 */
static int testbreak_eval_table(void)
{
	/* tablevalue */
	TestBreak(test_make_tablevalue);
	TestBreak(test_gettype_tablevalue);
	TestBreak(test_getspecialp_tablevalue);
	TestBreak(test_getdynamic_tablevalue);
	TestBreak(test_getignore_tablevalue);
	TestBreak(test_getreference_tablevalue);

	return 0;
}

int test_eval_table(void)
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
		lisp_initialize = 1;
		result = testbreak_eval_table();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

