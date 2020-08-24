#include "parse.c"
#include "bignum_object.h"
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
#include "package_symbol.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "syscall.h"
#include "type_table.h"

static int test_check_variable(void)
{
	addr pos;

	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	Error(check_variable_(pos));
	test(1, "check_variable1");

	RETURN;
}

static int test_check_function_variable(void)
{
	addr pos;

	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	Error(check_function_variable_(pos));
	test(1, "check_function_variable1");
	test(parse_callname_heap(&pos, pos) == 0, "check_function_variable2");
	Error(check_function_variable_(pos));
	test(1, "check_function_variable3");

	RETURN;
}

static int test_tagbody_tag_p(void)
{
	addr tag;

	internchar_debug(LISP_COMMON_USER, "HELLO", &tag);
	test(tagbody_tag_p(tag), "tagbody_tag_p1");
	internchar_debug(LISP_KEYWORD, "HELLO", &tag);
	test(tagbody_tag_p(tag), "tagbody_tag_p2");
	fixnum_heap(&tag, 100);
	test(tagbody_tag_p(tag), "tagbody_tag_p3");
	strvect_char_heap(&tag, "HELLO");
	test(! tagbody_tag_p(tag), "tagbody_tag_p4");
	bignum_value_alloc(NULL, &tag, signminus_bignum, 100);
	test(tagbody_tag_p(tag), "tagbody_tag_p5");

	RETURN;
}


/*
 *  Main
 */
static int testcase_parse(void)
{
	TestBreak(test_check_variable);
	TestBreak(test_check_function_variable);
	TestBreak(test_tagbody_tag_p);

	return 0;
}

static void testinit_parse(Execute ptr)
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
	build_pathname();
	build_declare();
	build_code();
}

int test_parse(void)
{
	DegradeTitle;
	return DegradeCode(parse);
}

