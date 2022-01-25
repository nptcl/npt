#include "subtypep_atomic.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "cons.h"
#include "condition.h"
#include "degrade.h"
#include "reader.h"
#include "package.h"
#include "package_intern.h"
#include "stream.h"
#include "strtype.h"
#include "subtypep.h"
#include "subtypep_table.h"
#include "symbol.h"
#include "syscall.h"
#include "type_parse.h"

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

static SubtypepResult subtable_test(addr left, addr right)
{
	SubtypepResult value;
	aatype(value);
	subtypep_table_(Execute_Thread, left, right, &value);
	return value;
}
static SubtypepResult strtable_test(const char *str1, const char *str2)
{
	addr left, right;

	parse_type_string(&left, str1);
	parse_type_string(&right, str2);
	return subtable_test(left, right);
}
static int strtable_true(const char *left, const char *right)
{
	return strtable_test(left, right) == SUBTYPEP_INCLUDE;
}
static int strtable_false(const char *left, const char *right)
{
	return strtable_test(left, right) == SUBTYPEP_FALSE;
}
static int strtable_exclude(const char *left, const char *right)
{
	return strtable_test(left, right) == SUBTYPEP_EXCLUDE;
}
#if 0
static int strtable_invalid(const char *left, const char *right)
{
	return strtable_test(left, right) == SUBTYPEP_INVALID;
}
#endif


/*
 *  subtypep-table
 */
static int test_subtypep_call_clos(void)
{
	test(strtable_true("STANDARD-CLASS", "CLASS"), "subtypep_call_clos.1");
	test(strtable_false("CLASS", "STANDARD-CLASS"), "subtypep_call_clos.2");
	test(strtable_exclude("INTEGER", "STANDARD-CLASS"), "subtypep_call_clos.3");

	RETURN;
}

static int test_subtypep_call_nil(void)
{
	test(strtable_true("nil", "nil"), "subtypep_call_nil.1");
	RETURN;
}

static int test_subtypep_call_t(void)
{
	test(strtable_true("nil", "t"), "subtypep_call_t.1");
	RETURN;
}

static int test_subtypep_call_null(void)
{
	test(strtable_true("null", "null"), "subtypep_call_null.1");
	test(strtable_exclude("cons", "null"), "subtypep_call_null.2");
	test(strtable_exclude("integer", "null"), "subtypep_call_null.3");
	RETURN;
}

static int test_subtypep_call_hash_table(void)
{
	test(strtable_true("hash-table", "hash-table"), "subtypep_call_hash_table.1");
	test(strtable_exclude("cons", "hash-table"), "subtypep_call_hash_table.2");
	RETURN;
}

static int test_subtypep_call_symbol(void)
{
	test(strtable_true("symbol", "symbol"), "subtypep_call_symbol.1");
	test(strtable_true("keyword", "symbol"), "subtypep_call_symbol.2");
	test(strtable_exclude("integer", "symbol"), "subtypep_call_symbol.3");
	RETURN;
}

static int test_subtypep_call_keyword(void)
{
	test(strtable_true("keyword", "keyword"), "subtypep_call_keyword.1");
	test(strtable_false("symbol", "keyword"), "subtypep_call_keyword.2");
	test(strtable_exclude("cons", "keyword"), "subtypep_call_keyword.3");
	RETURN;
}

static int test_subtypep_call_package(void)
{
	test(strtable_true("package", "package"), "subtypep_call_package.1");
	test(strtable_exclude("cons", "package"), "subtypep_call_package.2");
	RETURN;
}

static int test_subtypep_call_random_state(void)
{
	test(strtable_true("random-state", "random-state"), "subtypep_call_random_state.1");
	test(strtable_exclude("cons", "random-state"), "subtypep_call_random_state.2");
	RETURN;
}

static int test_subtypep_call_readtable(void)
{
	test(strtable_true("readtable", "readtable"), "subtypep_call_readtable.1");
	test(strtable_exclude("cons", "readtable"), "subtypep_call_readtable.2");
	RETURN;
}

static int test_subtypep_call_pathname(void)
{
	test(strtable_true("pathname", "pathname"), "subtypep_call_pathname.1");
	test(strtable_true("logical-pathname", "pathname"), "subtypep_call_pathname.2");
	test(strtable_exclude("cons", "pathname"), "subtypep_call_pathname.3");

	RETURN;
}

static int test_subtypep_call_logical_pathname(void)
{
	test(strtable_true("logical-pathname", "logical-pathname"),
			"subtypep_call_logical_pathname.1");
	test(strtable_false("pathname", "logical-pathname"),
			"subtypep_call_logical_pathname.2");
	test(strtable_exclude("cons", "logical-pathname"),
			"subtypep_call_logical-pathname.3");

	RETURN;
}

#define ArrayArrayDimension_true(a,b,c) { \
	test(strtable_test((b), (c)) == SUBTYPEP_INCLUDE, \
			"subtypep_call_array_array_dimension." a); \
}
#define ArrayArrayDimension_false(a,b,c) { \
	test(strtable_test((b), (c)) == SUBTYPEP_FALSE, \
			"subtypep_call_array_array_dimension." a); \
}
#define ArrayArrayDimension_exclude(a,b,c) { \
	test(strtable_test((b), (c)) == SUBTYPEP_EXCLUDE, \
			"subtypep_call_array_array_dimension." a); \
}
static int test_subtypep_call_array_array_dimension(void)
{
	ArrayArrayDimension_true("1", "(array * *)", "(array * *)");
	ArrayArrayDimension_false("2", "(array * *)", "(array * (*))");
	ArrayArrayDimension_exclude("3", "(array * (10 20))", "(array * 1)");
	ArrayArrayDimension_true("4", "(array * (10))", "(array * 1)");
	ArrayArrayDimension_true("5", "(array * (10))", "(array * (*))");
	ArrayArrayDimension_true("6", "(array * 4)", "(array * 4)");
	ArrayArrayDimension_true("7", "(array * (* * * *))", "(array * 4)");
	ArrayArrayDimension_exclude("8", "(array * 4)", "(array * 3)");
	ArrayArrayDimension_exclude("9", "(array * 3)", "(array * 4)");
	ArrayArrayDimension_exclude("10", "(array * (3))", "(array * (3 4))");
	ArrayArrayDimension_exclude("11", "(array * (3 5))", "(array * (3 4))");
	ArrayArrayDimension_true("12", "(array * (3 4))", "(array * (3 4))");
	ArrayArrayDimension_true("13", "(array * (3 4))", "(array * (3 *))");
	ArrayArrayDimension_false("14", "(array * (* 4))", "(array * (3 *))");
	ArrayArrayDimension_exclude("15", "(array * (* 4))", "(array * (* 3))");
	ArrayArrayDimension_true("16", "(array * (* 4 6))", "(array * (* * 6))");

	RETURN;
}

#define ArrayArray_true(a,b,c) { \
	test(strtable_test((b), (c)) == SUBTYPEP_INCLUDE, \
			"subtypep_call_array_array." a); \
}
#define ArrayArray_false(a,b,c) { \
	test(strtable_test((b), (c)) == SUBTYPEP_FALSE, \
			"subtypep_call_array_array." a); \
}
#define ArrayArray_exclude(a,b,c) { \
	test(strtable_test((b), (c)) == SUBTYPEP_EXCLUDE, \
			"subtypep_call_array_array." a); \
}
static int test_subtypep_call_array_array(void)
{
	ArrayArray_true("1", "(array integer)", "array");
	ArrayArray_true("2", "(array integer)", "(array *)");
	ArrayArray_true("3", "(array *)", "(array *)");
	ArrayArray_false("4", "(array *)", "(array integer)");
	ArrayArray_true("5", "(array integer)", "(array integer)");
	ArrayArray_exclude("6", "(array character)", "(array integer)");
	ArrayArray_exclude("7", "(array integer)", "(array character)");
	ArrayArray_true("8", "(array integer 2)", "(array integer 2)");
	ArrayArray_exclude("9", "(array integer 2)", "(array integer 3)");
	ArrayArray_true("10", "(simple-array integer)", "array");
	ArrayArray_true("11", "(simple-array integer)", "(array *)");
	ArrayArray_true("12", "(simple-array *)", "(array *)");
	ArrayArray_false("13", "(simple-array *)", "(array integer)");
	ArrayArray_true("14", "(simple-array integer)", "(array integer)");
	ArrayArray_exclude("15", "(simple-array character)", "(array integer)");
	ArrayArray_exclude("16", "(simple-array integer)", "(array character)");
	ArrayArray_true("17", "(simple-array integer 2)", "(array integer 2)");
	ArrayArray_exclude("18", "(simple-array integer 2)", "(array integer 3)");

	RETURN;
}

static int test_subtypep_call_character(void)
{
	test(strtable_true("character", "character"), "subtypep_call_character.1");
	test(strtable_true("base-char", "character"), "subtypep_call_character.2");
	test(strtable_true("standard-char", "character"), "subtypep_call_character.3");
	test(strtable_exclude("integer", "character"), "subtypep_call_character.4");

	RETURN;
}

static int test_subtypep_call_base_char(void)
{
	test(strtable_true("base-char", "base-char"), "subtypep_call_base_char.1");
	test(strtable_true("standard-char", "base-char"), "subtypep_call_base_char.2");
	test(strtable_false("character", "base-char"), "subtypep_call_base_char.3");
	test(strtable_exclude("cons", "base-char"), "subtypep_call_base_char.4");

	RETURN;
}

static int test_subtypep_call_standard_char(void)
{
	test(strtable_true("standard-char", "standard-char"),
			"subtypep_call_standard_char.1");
	test(strtable_false("base-char", "standard-char"),
			"subtypep_call_standard_char.2");
	test(strtable_false("character", "standard-char"),
			"subtypep_call_standard_char.3");
	test(strtable_exclude("symbol", "standard-char"),
			"subtypep_call_standard_char.4");

	RETURN;
}

static int test_subtypep_real_less(void)
{
	int check;
	addr left, right;

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer * 30)");
	subtypep_real_less_(left, right, &check);
	test(check, "subtypep_real_less1");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer * 20)");
	subtypep_real_less_(left, right, &check);
	test(check, "subtypep_real_less2");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer * 19)");
	subtypep_real_less_(left, right, &check);
	test(! check, "subtypep_real_less3");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer * 5)");
	subtypep_real_less_(left, right, &check);
	test(! check, "subtypep_real_less4");

	parse_type_string(&left, "(integer * 20)");
	parse_type_string(&right, "(integer * 30)");
	subtypep_real_less_(left, right, &check);
	test(check, "subtypep_real_less5");

	parse_type_string(&left, "(integer * 20)");
	parse_type_string(&right, "(integer * 20)");
	subtypep_real_less_(left, right, &check);
	test(check, "subtypep_real_less6");

	parse_type_string(&left, "(integer * 20)");
	parse_type_string(&right, "(integer * 19)");
	subtypep_real_less_(left, right, &check);
	test(! check, "subtypep_real_less7");

	parse_type_string(&left, "(integer * 20)");
	parse_type_string(&right, "(integer * 5)");
	subtypep_real_less_(left, right, &check);
	test(! check, "subtypep_real_less8");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer * 5)");
	subtypep_real_less_(left, right, &check);
	test(! check, "subtypep_real_less9");

	parse_type_string(&left, "(integer 10 (20))");
	parse_type_string(&right, "(integer * 20)");
	subtypep_real_less_(left, right, &check);
	test(check, "subtypep_real_less10");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer * (20))");
	subtypep_real_less_(left, right, &check);
	test(! check, "subtypep_real_less11");

	parse_type_string(&left, "(integer 10 (20))");
	parse_type_string(&right, "(integer * (20))");
	subtypep_real_less_(left, right, &check);
	test(check, "subtypep_real_less12");

	RETURN;
}

static int test_subtypep_real_greater(void)
{
	int check;
	addr left, right;

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 5 *)");
	subtypep_real_greater_(left, right, &check);
	test(check, "subtypep_real_greater1");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 10 *)");
	subtypep_real_greater_(left, right, &check);
	test(check, "subtypep_real_greater2");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 11 *)");
	subtypep_real_greater_(left, right, &check);
	test(! check, "subtypep_real_greater3");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 30 *)");
	subtypep_real_greater_(left, right, &check);
	test(! check, "subtypep_real_greater4");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer 5 *)");
	subtypep_real_greater_(left, right, &check);
	test(check, "subtypep_real_greater5");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer 10 *)");
	subtypep_real_greater_(left, right, &check);
	test(check, "subtypep_real_greater6");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer 11 *)");
	subtypep_real_greater_(left, right, &check);
	test(! check, "subtypep_real_greater7");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer 30 *)");
	subtypep_real_greater_(left, right, &check);
	test(! check, "subtypep_real_greater8");

	parse_type_string(&left, "(integer * 10)");
	parse_type_string(&right, "(integer 30 *)");
	subtypep_real_greater_(left, right, &check);
	test(! check, "subtypep_real_greater9");

	parse_type_string(&left, "(integer (10) 20)");
	parse_type_string(&right, "(integer 10 *)");
	subtypep_real_greater_(left, right, &check);
	test(check, "subtypep_real_greater10");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer (10) *)");
	subtypep_real_greater_(left, right, &check);
	test(! check, "subtypep_real_greater11");

	parse_type_string(&left, "(integer (10) 20)");
	parse_type_string(&right, "(integer (10) *)");
	subtypep_real_greater_(left, right, &check);
	test(check, "subtypep_real_greater12");

	RETURN;
}

static int test_subtypep_real_range(void)
{
	int check;
	addr left, right;

	parse_type_string(&left, "(integer 15 16)");
	parse_type_string(&right, "(integer 10 20)");
	subtypep_real_range_(left, right, &check);
	test(check, "subtypep_real_range1");

	parse_type_string(&left, "(integer 10 16)");
	parse_type_string(&right, "(integer 10 20)");
	subtypep_real_range_(left, right, &check);
	test(check, "subtypep_real_range2");

	parse_type_string(&left, "(integer 15 20)");
	parse_type_string(&right, "(integer 10 20)");
	subtypep_real_range_(left, right, &check);
	test(check, "subtypep_real_range3");

	parse_type_string(&left, "(integer 15 30)");
	parse_type_string(&right, "(integer 10 20)");
	subtypep_real_range_(left, right, &check);
	test(! check, "subtypep_real_range4");

	parse_type_string(&left, "(integer 100 200)");
	parse_type_string(&right, "(integer 10 20)");
	subtypep_real_range_(left, right, &check);
	test(! check, "subtypep_real_range5");

	RETURN;
}

static int test_subtypep_realcheck(void)
{
	int check;
	addr left, right;

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "integer");
	subtypep_realcheck_(left, right, &check);
	test(check, "subtypep_realcheck1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(integer 10 20)");
	subtypep_realcheck_(left, right, &check);
	test(! check, "subtypep_realcheck2");

	parse_type_string(&left, "(integer 15 15)");
	parse_type_string(&right, "(integer 10 20)");
	subtypep_realcheck_(left, right, &check);
	test(check, "subtypep_realcheck3");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer * 30)");
	subtypep_realcheck_(left, right, &check);
	test(check, "subtypep_realcheck4");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 5 *)");
	subtypep_realcheck_(left, right, &check);
	test(check, "subtypep_realcheck5");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 0 100)");
	subtypep_realcheck_(left, right, &check);
	test(check, "subtypep_realcheck6");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 200 300)");
	subtypep_realcheck_(left, right, &check);
	test(! check, "subtypep_realcheck7");

	RETURN;
}

static int test_realexclude_left(void)
{
	int check;
	addr left, right;

	parse_type_string(&left, "(integer * 10)");
	parse_type_string(&right, "(integer 20 *)");
	subtypep_realexlucde_(left, right, &check);
	test(check, "realexclude_left1");

	parse_type_string(&left, "(integer * 30)");
	parse_type_string(&right, "(integer 20 *)");
	subtypep_realexlucde_(left, right, &check);
	test(! check, "realexclude_left2");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 30 40)");
	subtypep_realexlucde_(left, right, &check);
	test(check, "realexclude_left3");

	parse_type_string(&left, "(integer * 20)");
	parse_type_string(&right, "(integer 20 *)");
	subtypep_realexlucde_(left, right, &check);
	test(! check, "realexclude_left4");

	parse_type_string(&left, "(integer * (20))");
	parse_type_string(&right, "(integer 20 *)");
	subtypep_realexlucde_(left, right, &check);
	test(check, "realexclude_left5");

	parse_type_string(&left, "(integer * 20)");
	parse_type_string(&right, "(integer (20) *)");
	subtypep_realexlucde_(left, right, &check);
	test(check, "realexclude_left6");

	parse_type_string(&left, "(integer * (20))");
	parse_type_string(&right, "(integer (20) *)");
	subtypep_realexlucde_(left, right, &check);
	test(check, "realexclude_left7");

	RETURN;
}

static int test_realexclude_right(void)
{
	int check;
	addr left, right;

	parse_type_string(&left, "(integer 20 *)");
	parse_type_string(&right, "(integer * 10)");
	subtypep_realexlucde_(left, right, &check);
	test(check, "realexclude_right1");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer * 20)");
	subtypep_realexlucde_(left, right, &check);
	test(! check, "realexclude_right2");

	parse_type_string(&left, "(integer 30 40)");
	parse_type_string(&right, "(integer 10 20)");
	subtypep_realexlucde_(left, right, &check);
	test(check, "realexclude_right3");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer * 10)");
	subtypep_realexlucde_(left, right, &check);
	test(! check, "realexclude_right4");

	parse_type_string(&left, "(integer (10) *)");
	parse_type_string(&right, "(integer * 10)");
	subtypep_realexlucde_(left, right, &check);
	test(check, "realexclude_right5");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer * (10))");
	subtypep_realexlucde_(left, right, &check);
	test(check, "realexclude_right6");

	parse_type_string(&left, "(integer (10) *)");
	parse_type_string(&right, "(integer * (10))");
	subtypep_realexlucde_(left, right, &check);
	test(check, "realexclude_right7");

	RETURN;
}

static int test_subtypep_realexclude(void)
{
	int check;
	addr left, right;

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 30 40)");
	subtypep_realexlucde_(left, right, &check);
	test(check, "subtypep_realexclude1");

	parse_type_string(&left, "(integer 30 40)");
	parse_type_string(&right, "(integer 10 20)");
	subtypep_realexlucde_(left, right, &check);
	test(check, "subtypep_realexclude2");

	parse_type_string(&left, "(integer 10 30)");
	parse_type_string(&right, "(integer 20 40)");
	subtypep_realexlucde_(left, right, &check);
	test(! check, "subtypep_realexclude3");

	parse_type_string(&left, "(integer 20 40)");
	parse_type_string(&right, "(integer 10 30)");
	subtypep_realexlucde_(left, right, &check);
	test(! check, "subtypep_realexclude4");

	RETURN;
}

static int test_subtypep_realparameter(void)
{
	SubtypepResult value;
	addr left, right;

	aatype(value);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "integer");
	subtypep_realparameter_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_realparameter1");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 30 *)");
	subtypep_realparameter_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_realparameter2");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 15 *)");
	subtypep_realparameter_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_realparameter3");

	RETURN;
}

static int test_subtypep_integer(void)
{
	test(strtable_true("integer", "integer"), "subtypep_integer1");
	test(strtable_false("rational", "integer"), "subtypep_integer2");
	test(strtable_false("real", "integer"), "subtypep_integer3");
	test(strtable_false("number", "integer"), "subtypep_integer4");
	test(strtable_exclude("cons", "integer"), "subtypep_integer5");
	test(strtable_true("(integer 10 10)", "(integer 0 100)"), "subtypep_integer6");

	RETURN;
}

static int test_subtypep_rational(void)
{
	test(strtable_false("real", "rational"), "subtypep_rational1");
	test(strtable_false("number", "rational"), "subtypep_rational2");
	test(strtable_true("ratio", "rational"), "subtypep_rational3");
	test(strtable_false("ratio", "(rational 10 *)"), "subtypep_rational4");
	test(strtable_true("rational", "rational"), "subtypep_rational5");
	test(strtable_true("integer", "rational"), "subtypep_rational6");
	test(strtable_exclude("symbol", "rational"), "subtypep_rational7");
	test(strtable_true("(integer 10 20)", "(rational 5 100)"),
			"subtypep_rational8");
	test(strtable_false("(integer 10 20)", "(rational 15 100)"),
			"subtypep_rational9");
	test(strtable_exclude("(integer 10 20)", "(rational 50 100)"),
			"subtypep_rational10");

	RETURN;
}

static int test_subtypep_real(void)
{
	test(strtable_true("real", "real"), "subtypep_real1");
	test(strtable_false("number", "real"), "subtypep_real2");
	test(strtable_true("ratio", "real"), "subtypep_real3");
	test(strtable_false("ratio", "(real 10 20)"), "subtypep_real4");
	test(strtable_true("float", "real"), "subtypep_real5");
	test(strtable_exclude("symbol", "real"), "subtypep_real6");

	RETURN;
}

static int test_subtypep_number(void)
{
	test(strtable_true("number", "number"), "subtypep_number1");
	test(strtable_true("complex", "number"), "subtypep_number2");
	test(strtable_true("ratio", "number"), "subtypep_number3");
	test(strtable_true("float", "number"), "subtypep_number4");
	test(strtable_exclude("symbol", "number"), "subtypep_number5");

	RETURN;
}

static int test_subtypep_float(void)
{
	SubtypepResult value;
	addr left, right, pos1, pos2;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	test(strtable_false("number", "float"), "subtypep_float1");
	test(strtable_false("real", "float"), "subtypep_float2");
	test(strtable_true("float", "float"), "subtypep_float3");
	test(strtable_true("short-float", "float"), "subtypep_float4");
	test(strtable_exclude("symbol", "float"), "subtypep_float5");

	interncommon_debug("DOUBLE-FLOAT", &left);
	double_float_heap(&pos1, 10.0);
	double_float_heap(&pos2, 20.0);
	list_heap(&left, left, pos1, pos2, NULL);
	test_parse_type(&left, left);
	interncommon_debug("FLOAT", &right);
	double_float_heap(&pos1, 15.0);
	list_heap(&right, right, pos1, NULL);
	test_parse_type(&right, right);
	subtypep_table_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_float6");

	RETURN;
}

static int test_subtypep_float_type(void)
{
	SubtypepResult value;
	addr left, right, pos1, pos2;

	aatype(value);

	parse_type_string(&left, "number");
	parse_type_string(&right, "single-float");
	subtypep_float_type_(left, right, &value, LISPDECL_SINGLE_FLOAT);
	test(value == SUBTYPEP_FALSE, "subtypep_float_type1");

	parse_type_string(&left, "real");
	parse_type_string(&right, "single-float");
	subtypep_float_type_(left, right, &value, LISPDECL_SINGLE_FLOAT);
	test(value == SUBTYPEP_FALSE, "subtypep_float_type2");

	parse_type_string(&left, "float");
	parse_type_string(&right, "single-float");
	subtypep_float_type_(left, right, &value, LISPDECL_SINGLE_FLOAT);
	test(value == SUBTYPEP_FALSE, "subtypep_float_type3");

	parse_type_string(&left, "single-float");
	parse_type_string(&right, "single-float");
	subtypep_float_type_(left, right, &value, LISPDECL_SINGLE_FLOAT);
	test(value == SUBTYPEP_INCLUDE, "subtypep_float_type4");

	parse_type_string(&left, "cons");
	parse_type_string(&right, "single-float");
	subtypep_float_type_(left, right, &value, LISPDECL_SINGLE_FLOAT);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_float_type5");

	interncommon_debug("SINGLE-FLOAT", &left);
	single_float_heap(&pos1, 10.0);
	single_float_heap(&pos2, 20.0);
	list_heap(&left, left, pos1, pos2, NULL);
	test_parse_type(&left, left);
	interncommon_debug("SINGLE-FLOAT", &right);
	single_float_heap(&pos1, 5.0);
	list_heap(&right, right, pos1, NULL);
	test_parse_type(&right, right);
	subtypep_float_type_(left, right, &value, LISPDECL_SINGLE_FLOAT);
	test(value == SUBTYPEP_INCLUDE, "subtypep_float_type6");

	RETURN;
}

static int test_subtypep_short_float(void)
{
	test(strtable_true("short-float", "short-float"), "subtypep_short_float1");
	RETURN;
}

static int test_subtypep_single_float(void)
{
	test(strtable_true("single-float", "single-float"), "subtypep_single_float1");
	RETURN;
}

static int test_subtypep_double_float(void)
{
	test(strtable_true("double-float", "double-float"), "subtypep_double_float1");
	RETURN;
}

static int test_subtypep_long_float(void)
{
	test(strtable_true("long-float", "long-float"), "subtypep_long_float1");
	RETURN;
}

static int test_subtypep_ratio(void)
{
	test(strtable_false("number", "ratio"), "subtypep_ratio1");
	test(strtable_false("real", "ratio"), "subtypep_ratio2");
	test(strtable_false("rational", "ratio"), "subtypep_ratio3");
	test(strtable_true("ratio", "ratio"), "subtypep_ratio4");
	test(strtable_exclude("integer", "ratio"), "subtypep_ratio5");
	test(strtable_exclude("symbol", "ratio"), "subtypep_ratio6");

	RETURN;
}


/*
 *  main
 */
static int testcase_subtypep_atomic(void)
{
	/* subtypep-table */
	TestBreak(test_subtypep_call_clos);
	TestBreak(test_subtypep_call_nil);
	TestBreak(test_subtypep_call_t);
	TestBreak(test_subtypep_call_null);
	TestBreak(test_subtypep_call_hash_table);
	TestBreak(test_subtypep_call_symbol);
	TestBreak(test_subtypep_call_keyword);
	TestBreak(test_subtypep_call_package);
	TestBreak(test_subtypep_call_random_state);
	TestBreak(test_subtypep_call_readtable);
	TestBreak(test_subtypep_call_pathname);
	TestBreak(test_subtypep_call_logical_pathname);
	TestBreak(test_subtypep_call_array_array_dimension);
	TestBreak(test_subtypep_call_array_array);
	TestBreak(test_subtypep_call_character);
	TestBreak(test_subtypep_call_base_char);
	TestBreak(test_subtypep_call_standard_char);
	TestBreak(test_subtypep_real_less);
	TestBreak(test_subtypep_real_greater);
	TestBreak(test_subtypep_real_range);
	TestBreak(test_subtypep_realcheck);
	TestBreak(test_realexclude_left);
	TestBreak(test_realexclude_right);
	TestBreak(test_subtypep_realexclude);
	TestBreak(test_subtypep_realparameter);
	TestBreak(test_subtypep_integer);
	TestBreak(test_subtypep_rational);
	TestBreak(test_subtypep_real);
	TestBreak(test_subtypep_number);
	TestBreak(test_subtypep_float);
	TestBreak(test_subtypep_float_type);
	TestBreak(test_subtypep_short_float);
	TestBreak(test_subtypep_single_float);
	TestBreak(test_subtypep_double_float);
	TestBreak(test_subtypep_long_float);
	TestBreak(test_subtypep_ratio);

	return 0;
}

static void testinit_subtypep_atomic(Execute ptr)
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

int test_subtypep_atomic(void)
{
	DegradeTitle;
	return DegradeCode(subtypep_atomic);
}

