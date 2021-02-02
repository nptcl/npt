#include "subtypep_andor.c"
#include "condition.h"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "copy.h"
#include "degrade.h"
#include "hashtable.h"
#include "package.h"
#include "package_intern.h"
#include "pathname.h"
#include "random_state.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "subtypep_table.h"
#include "subtypep_optimize.h"
#include "type_copy.h"
#include "type_parse.h"
#include "type_table.h"

static void test_parse_type(addr *ret, addr pos)
{
	if (parse_type(Execute_Thread, ret, pos, Nil)) {
		Error(fmte_("parse-type error.", NULL));
	}
}

static void parse_type_string(addr *ret, const char *code)
{
	readstring_debug(ret, code);
	test_parse_type(ret, *ret);
}


/*
 *  subtypep_call
 */
static int test_subtypep_and_left(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "(and integer)");
	parse_type_string(&right, "real");
	subtypep_and_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left1");

	parse_type_string(&left, "(and integer real)");
	parse_type_string(&right, "real");
	subtypep_and_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left2");

	parse_type_string(&left, "(and real integer)");
	parse_type_string(&right, "integer");
	subtypep_and_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left3");

	parse_type_string(&left, "(and (satisfies hello) integer)");
	parse_type_string(&right, "integer");
	subtypep_and_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left4");

	parse_type_string(&left, "(and real rational)");
	parse_type_string(&right, "integer");
	subtypep_and_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_and_left5");

	parse_type_string(&left, "(and real symbol)");
	parse_type_string(&right, "integer");
	subtypep_and_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left6");

	RETURN;
}

static int test_subtypep_or_left(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "(or integer)");
	parse_type_string(&right, "real");
	subtypep_or_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_left1");

	parse_type_string(&left, "(or (satisfies hello) integer)");
	parse_type_string(&right, "integer");
	subtypep_or_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INVALID, "subtypep_or_left2");

	parse_type_string(&left, "(or rational float)");
	parse_type_string(&right, "real");
	subtypep_or_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_left3");

	parse_type_string(&left, "(or symbol cons)");
	parse_type_string(&right, "real");
	subtypep_or_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_or_left4");

	parse_type_string(&left, "(or symbol integer)");
	parse_type_string(&right, "real");
	subtypep_or_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_or_left5");

	RETURN;
}

static int test_subtypep_andargs_right(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(and real)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_andargs_right1");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and real number)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_andargs_right2");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and integer number)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_andargs_right3");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and symbol cons)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_andargs_right4");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and integer cons)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_andargs_right5");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and real cons)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_andargs_right6");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_andargs_right7");

	RETURN;
}

static int test_subtypep_orargs_right(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or real)");
	subtypep_orargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_orargs_right1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or)");
	subtypep_orargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_orargs_right2");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or integer cons)");
	subtypep_orargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_orargs_right3");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or cons real)");
	subtypep_orargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_orargs_right4");

	parse_type_string(&left, "symbol");
	parse_type_string(&right, "(or cons real)");
	subtypep_orargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_orargs_right5");

	parse_type_string(&left, "number");
	parse_type_string(&right, "(or cons real)");
	subtypep_orargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_orargs_right6");

	RETURN;
}

static int test_subtypep_and_right(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(and integer real)");
	subtypep_and_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_right1");

	parse_type_string(&left, "(or integer real)");
	parse_type_string(&right, "(and real number)");
	subtypep_and_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_right2");

	parse_type_string(&left, "(or integer real symbol)");
	parse_type_string(&right, "(and real number)");
	subtypep_and_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_and_right3");

	parse_type_string(&left, "(or integer real symbol)");
	parse_type_string(&right, "(and cons)");
	subtypep_and_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_and_right4");

	RETURN;
}

static int test_subtypep_or_right(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or real symbol)");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_right1");

	parse_type_string(&left, "(or integer symbol)");
	parse_type_string(&right, "(or real symbol)");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_right2");

	parse_type_string(&left, "(and integer real)");
	parse_type_string(&right, "(or integer symbol)");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_right3");

	parse_type_string(&left, "(or integer cons)");
	parse_type_string(&right, "(or real symbol)");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_or_right4");

	RETURN;
}


/*
 *  main
 */
static int testcase_subtypep_andor(void)
{
	TestBreak(test_subtypep_and_left);
	TestBreak(test_subtypep_or_left);
	TestBreak(test_subtypep_andargs_right);
	TestBreak(test_subtypep_orargs_right);
	TestBreak(test_subtypep_and_right);
	TestBreak(test_subtypep_or_right);

	return 0;
}

static void testinit_subtypep_andor(Execute ptr)
{
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
}

int test_subtypep_andor(void)
{
	DegradeTitle;
	return DegradeCode(subtypep_andor);
}

