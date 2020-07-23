#include "parse_object.c"
#include "bignum.h"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "declare.h"
#include "degrade.h"
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

static int test_eval_parse_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	eval_parse_alloc(NULL, &pos, EVAL_PARSE_T, 10);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_parse_alloc1");
	test(! GetStatusDynamic(pos), "eval_parse_alloc2");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "eval_parse_alloc3");
	test(lenarrayr(pos) == 10, "eval_parse_alloc4");

	eval_parse_local(local, &pos, EVAL_PARSE_T, 10);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_parse_alloc5");
	test(GetStatusDynamic(pos), "eval_parse_alloc6");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "eval_parse_alloc7");
	test(lenarrayr(pos) == 10, "eval_parse_alloc8");

	eval_parse_heap(&pos, EVAL_PARSE_T, 10);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_parse_alloc9");
	test(! GetStatusDynamic(pos), "eval_parse_alloc10");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "eval_parse_alloc11");
	test(lenarrayr(pos) == 10, "eval_parse_alloc12");

	rollback_local(local, stack);

	RETURN;
}

static int test_eval_single_parse_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	eval_single_parse_alloc(local, &pos, EVAL_PARSE_T, T);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_single_parse_alloc1");
	test(GetStatusDynamic(pos), "eval_single_parse_alloc2");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "eval_single_parse_alloc3");
	test(lenarrayr(pos) == 1, "eval_single_parse_alloc4");
	test(RefEval(pos, 0) == T, "eval_single_parse_alloc5");

	eval_single_parse_local(local, &pos, EVAL_PARSE_T, T);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_single_parse_alloc6");
	test(GetStatusDynamic(pos), "eval_single_parse_alloc7");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "eval_single_parse_alloc8");
	test(lenarrayr(pos) == 1, "eval_single_parse_alloc9");
	test(RefEval(pos, 0) == T, "eval_single_parse_alloc10");

	eval_single_parse_heap(&pos, EVAL_PARSE_T, T);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_single_parse_alloc11");
	test(! GetStatusDynamic(pos), "eval_single_parse_alloc12");
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "eval_single_parse_alloc13");
	test(lenarrayr(pos) == 1, "eval_single_parse_alloc14");
	test(RefEval(pos, 0) == T, "eval_single_parse_alloc15");

	rollback_local(local, stack);

	RETURN;
}

static int test_StructEvalParse(void)
{
	addr pos;
	struct eval_parse *str;

	eval_single_parse_heap(&pos, EVAL_PARSE_T, T);
	str = StructEvalParse(pos);
	test(str->type == EVAL_PARSE_T, "StructEvalParse1");

	RETURN;
}

static int test_RefEvalParse(void)
{
	addr pos, check;

	eval_parse_heap(&pos, EVAL_PARSE_T, 5);
	test(RefEvalParse(pos, 2) == Nil, "RefEvalParse1");
	SetEvalParse(pos, 2, T);
	test(RefEvalParse(pos, 2) == T, "RefEvalParse2");
	GetEvalParse(pos, 2, &check);
	test(check == T, "RefEvalParse3");

	RETURN;
}

static int test_RefEvalParseType(void)
{
	EvalParse check;
	addr pos;

	eval_parse_heap(&pos, EVAL_PARSE_T, 5);
	test(RefEvalParseType(pos) == EVAL_PARSE_T, "RefEvalParseType1");
	SetEvalParseType(pos, EVAL_PARSE_NIL);
	test(RefEvalParseType(pos) == EVAL_PARSE_NIL, "RefEvalParseType2");
	GetEvalParseType(pos, &check);
	test(check == EVAL_PARSE_NIL, "RefEvalParseType3");

	RETURN;
}


/*
 *  Main
 */
static int testbreak_parse_object(void)
{
	Error(in_package_lisp_package_());
	TestBreak(test_eval_parse_alloc);
	TestBreak(test_eval_single_parse_alloc);
	TestBreak(test_StructEvalParse);
	TestBreak(test_RefEvalParse);
	TestBreak(test_RefEvalParseType);

	return 0;
}

int test_parse_object(void)
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
		result = testbreak_parse_object();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

