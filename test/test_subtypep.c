#include "subtypep.c"
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

static void test_parse_type(addr *ret, addr pos)
{
	if (parse_type_(Execute_Thread, ret, pos, Nil)) {
		Error(fmte_("parse-type error.", NULL));
	}
}

static void parse_type_string(addr *ret, const char *code)
{
	readstring_debug(ret, code);
	test_parse_type(ret, *ret);
}

static int test_subtypep_check_(void)
{
	int result, invalid;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(result);
	aatype(invalid);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test(result && invalid, "subtypep_check_1");

	parse_type_string(&left, "cons");
	parse_type_string(&right, "real");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test((! result) && invalid, "subtypep_check_2");

	parse_type_string(&left, "character");
	parse_type_string(&right, "base-char");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test((! result) && invalid, "subtypep_check_3");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(satisfies hello)");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test((! result) && (! invalid), "subtypep_check_4");

	parse_type_string(&left, "character");
	parse_type_string(&right, "extended-char");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test((! result) && invalid, "subtypep_check_5");

	parse_type_string(&left, "extended-char");
	parse_type_string(&right, "character");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test(result && invalid, "subtypep_check_6");

	parse_type_string(&left, "(integer 20 50)");
	parse_type_string(&right, "(or (integer 10 40) (integer 30 60))");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test(result && invalid, "subtypep_check_7");

	RETURN;
}


/*
 *  main
 */
static int testcase_subtypep(void)
{
	TestBreak(test_subtypep_check_);

	return 0;
}

static void testinit_subtypep(Execute ptr)
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

int test_subtypep(void)
{
	DegradeTitle;
	return DegradeCode(subtypep);
}

