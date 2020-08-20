#include "type_range.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "degrade.h"
#include "hashtable.h"
#include "package.h"
#include "pathname.h"
#include "random_state.h"
#include "ratio.h"
#include "reader.h"
#include "sequence.h"
#include "stream.h"
#include "syscall.h"
#include "symbol.h"
#include "type.h"
#include "type_table.h"

/*
 *  test tools
 */
static void parse_type_string(addr *ret, const char *code)
{
	readstring_debug(ret, code);
	parse_type_unsafe(ret, *ret);
}


/*
 *  range
 */
static int test_range_asterisk_p(void)
{
	addr type;

	parse_type_string(&type, "(integer * *)");
	test(range_asterisk_p(type), "range_asterisk_p1");

	parse_type_string(&type, "(integer 10 *)");
	test(! range_asterisk_p(type), "range_asterisk_p2");

	parse_type_string(&type, "(integer * (20))");
	test(! range_asterisk_p(type), "range_asterisk_p3");

	parse_type_string(&type, "(integer 10 20)");
	test(! range_asterisk_p(type), "range_asterisk_p4");

	parse_type_string(&type, "real");
	test(range_asterisk_p(type), "range_asterisk_p5");

	RETURN;
}

static int test_range_left_p(void)
{
	addr type;

	parse_type_string(&type, "(integer 10 *)");
	test(range_left_p(type), "range_left_p1");

	parse_type_string(&type, "(integer (10) *)");
	test(range_left_p(type), "range_left_p2");

	parse_type_string(&type, "(integer * *)");
	test(! range_left_p(type), "range_left_p3");

	parse_type_string(&type, "(integer * 20)");
	test(! range_left_p(type), "range_left_p4");

	parse_type_string(&type, "(integer 10 20)");
	test(! range_left_p(type), "range_left_p5");

	RETURN;
}

static int test_range_left_any_p(void)
{
	addr type;

	parse_type_string(&type, "(integer 10 *)");
	test(range_left_any_p(type), "range_left_any_p1");

	parse_type_string(&type, "(integer (10) 20)");
	test(range_left_any_p(type), "range_left_any_p2");

	parse_type_string(&type, "(integer * 20)");
	test(! range_left_any_p(type), "range_left_any_p3");

	parse_type_string(&type, "(integer * *)");
	test(! range_left_any_p(type), "range_left_any_p4");

	RETURN;
}

static int test_range_right_p(void)
{
	addr type;

	parse_type_string(&type, "(integer * 10)");
	test(range_right_p(type), "range_right_p1");

	parse_type_string(&type, "(integer * (20))");
	test(range_right_p(type), "range_right_p2");

	parse_type_string(&type, "(integer 10 *)");
	test(! range_right_p(type), "range_right_p3");

	parse_type_string(&type, "(integer 10 20)");
	test(! range_right_p(type), "range_right_p4");

	parse_type_string(&type, "(integer * *)");
	test(! range_right_p(type), "range_right_p5");

	RETURN;
}

static int test_range_any_right_p(void)
{
	addr type;

	parse_type_string(&type, "(real * 10)");
	test(range_any_right_p(type), "range_any_right_p1");

	parse_type_string(&type, "(real 10 (20))");
	test(range_any_right_p(type), "range_any_right_p2");

	parse_type_string(&type, "(real 10 *)");
	test(! range_any_right_p(type), "range_any_right_p3");

	parse_type_string(&type, "(real * *)");
	test(! range_any_right_p(type), "range_any_right_p4");

	RETURN;
}

static int test_range_between_p(void)
{
	addr type;

	parse_type_string(&type, "(real 10 20)");
	test(range_between_p(type), "range_between_p1");

	parse_type_string(&type, "(real (10) 20)");
	test(range_between_p(type), "range_between_p2");

	parse_type_string(&type, "(real * 20)");
	test(! range_between_p(type), "range_between_p3");

	parse_type_string(&type, "(real 10 *)");
	test(! range_between_p(type), "range_between_p4");

	parse_type_string(&type, "(real * *)");
	test(! range_between_p(type), "range_between_p5");

	RETURN;
}

static int test_range_left_value(void)
{
	addr type, left1, left2;

	parse_type_string(&type, "(real (10) 20)");
	range_left_value(type, &left1, &left2);
	test(left1 == T, "range_left_value1");
	test(RefFixnum(left2) == 10, "range_left_value2");

	RETURN;
}

static int test_range_right_value(void)
{
	addr type, right1, right2;

	parse_type_string(&type, "(real (10) 20)");
	range_right_value(type, &right1, &right2);
	test(right1 == Nil, "range_right_value1");
	test(RefFixnum(right2) == 20, "range_right_value2");

	RETURN;
}


/*
 *  greate / less
 */
#define range_true(call,number,left,right) { \
	int __check; \
	addr __left, __right; \
	parse_type_string(&__left, "(integer " left ")"); \
	parse_type_string(&__right, "(integer " right ")"); \
	__check = 0; \
	call(__left, __right, &__check); \
	test(__check, #call number); \
}

#define range_false(call,number,left,right) { \
	int __check; \
	addr __left, __right; \
	parse_type_string(&__left, "(integer " left ")"); \
	parse_type_string(&__right, "(integer " right ")"); \
	__check = 0; \
	call(__left, __right, &__check); \
	test(! __check, #call number); \
}

static int test_range_left_left_less(void)
{
	range_true (range_left_left_less_, "1", "10 *", "20 *");
	range_false(range_left_left_less_, "2", "50 *", "20 *");
	range_false(range_left_left_less_, "3", "10 *", "10 *");
	range_false(range_left_left_less_, "4", "(10) *", "10 *");
	range_true (range_left_left_less_, "5", "10 *", "(10) *");
	range_false(range_left_left_less_, "6", "(10) *", "(10) *");

	RETURN;
}

static int test_range_left_left_less_equal(void)
{
	range_true (range_left_left_less_equal_, "1", "10 *",   "(20) *");
	range_false(range_left_left_less_equal_, "2", "50 *",   "10 *");
	range_true (range_left_left_less_equal_, "3", "10 *",   "10 *");
	range_false(range_left_left_less_equal_, "4", "(10) *", "10 *");
	range_true (range_left_left_less_equal_, "5", "10 *",   "(10) *");
	range_true (range_left_left_less_equal_, "6", "(10) *", "(10) *");

	RETURN;
}

static int test_range_left_left_greater(void)
{
	range_true (range_left_left_greater_, "1", "50 *", "10 *");
	range_false(range_left_left_greater_, "2", "10 *", "40 *");
	range_false(range_left_left_greater_, "3", "10 *", "10 *");
	range_true (range_left_left_greater_, "4", "(10) *", "10 *");
	range_false(range_left_left_greater_, "5", "10 *", "(10) *");
	range_false(range_left_left_greater_, "6", "(10) *", "(10) *");

	RETURN;
}

static int test_range_left_left_greater_equal(void)
{
	range_true (range_left_left_greater_equal_, "1", "50 *",   "10 *");
	range_false(range_left_left_greater_equal_, "2", "10 *",   "50 *");
	range_true (range_left_left_greater_equal_, "3", "10 *",   "10 *");
	range_true (range_left_left_greater_equal_, "4", "(10) *", "10 *");
	range_false(range_left_left_greater_equal_, "5", "10 *",   "(10) *");
	range_true (range_left_left_greater_equal_, "6", "(10) *", "(10) *");

	RETURN;
}

static int test_range_left_right_less(void)
{
	range_true (range_left_right_less_, "1", "10 *",   "* 40");
	range_false(range_left_right_less_, "2", "40 *",   "* 10");
	range_false(range_left_right_less_, "3", "10 *",   "* 10");
	range_false(range_left_right_less_, "4", "(10) *", "* 10");
	range_false(range_left_right_less_, "5", "10 *",   "* (10)");
	range_false(range_left_right_less_, "6", "(10) *", "* (10)");

	RETURN;
}

static int test_range_left_right_less_equal(void)
{
	range_true (range_left_right_less_equal_, "1", "10 *",   "* 40");
	range_false(range_left_right_less_equal_, "2", "40 *",   "* 10");
	range_true (range_left_right_less_equal_, "3", "10 *",   "* 10");
	range_false(range_left_right_less_equal_, "4", "(10) *", "* 10");
	range_false(range_left_right_less_equal_, "5", "10 *",   "* (10)");
	range_false(range_left_right_less_equal_, "6", "(10) *", "* (10)");

	RETURN;
}

static int test_range_left_right_greater(void)
{
	range_true (range_left_right_greater_, "1", "50 *",   "* 10");
	range_false(range_left_right_greater_, "2", "10 *",   "* 50");
	range_false(range_left_right_greater_, "3", "10 *",   "* 10");
	range_true (range_left_right_greater_, "4", "(10) *", "* 10");
	range_true (range_left_right_greater_, "5", "10 *",   "* (10)");
	range_true (range_left_right_greater_, "6", "(10) *", "* (10)");

	RETURN;
}

static int test_range_left_right_greater_equal(void)
{
	range_true (range_left_right_greater_equal_, "1", "50 *",   "* 10");
	range_false(range_left_right_greater_equal_, "2", "10 *",   "* 50");
	range_true (range_left_right_greater_equal_, "3", "10 *",   "* 10");
	range_true (range_left_right_greater_equal_, "4", "(10) *", "* 10");
	range_true (range_left_right_greater_equal_, "5", "10 *",   "* (10)");
	range_true (range_left_right_greater_equal_, "6", "(10) *", "* (10)");

	RETURN;
}

static int test_range_right_left_less(void)
{
	range_true (range_right_left_less_, "1", "* 10",   "50 *");
	range_false(range_right_left_less_, "2", "* 50",   "10 *");
	range_false(range_right_left_less_, "3", "* 10",   "10 *");
	range_true (range_right_left_less_, "4", "* (10)", "10 *");
	range_true (range_right_left_less_, "5", "* 10",   "(10) *");
	range_true (range_right_left_less_, "6", "* (10)", "(10) *");

	RETURN;
}

static int test_range_right_left_less_equal(void)
{
	range_true (range_right_left_less_equal_, "1", "* 10",   "50 *");
	range_false(range_right_left_less_equal_, "2", "* 50",   "10 *");
	range_true (range_right_left_less_equal_, "3", "* 10",   "10 *");
	range_true (range_right_left_less_equal_, "4", "* (10)", "10 *");
	range_true (range_right_left_less_equal_, "5", "* 10",   "(10) *");
	range_true (range_right_left_less_equal_, "6", "* (10)", "(10) *");

	RETURN;
}

static int test_range_right_left_greater(void)
{
	range_true (range_right_left_greater_, "1", "* 50",   "10 *");
	range_false(range_right_left_greater_, "2", "* 10",   "50 *");
	range_false(range_right_left_greater_, "3", "* 10",   "10 *");
	range_false(range_right_left_greater_, "4", "* (10)", "10 *");
	range_false(range_right_left_greater_, "5", "* 10",   "(10) *");
	range_false(range_right_left_greater_, "6", "* (10)", "(10) *");

	RETURN;
}

static int test_range_right_left_greater_equal(void)
{
	range_true (range_right_left_greater_equal_, "1", "* 50",   "10 *");
	range_false(range_right_left_greater_equal_, "2", "* 10",   "50 *");
	range_true (range_right_left_greater_equal_, "3", "* 10",   "10 *");
	range_false(range_right_left_greater_equal_, "4", "* (10)", "10 *");
	range_false(range_right_left_greater_equal_, "5", "* 10",   "(10) *");
	range_false(range_right_left_greater_equal_, "6", "* (10)", "(10) *");

	RETURN;
}

static int test_range_right_right_less(void)
{
	range_true (range_right_right_less_, "1", "* 10",   "* 50");
	range_false(range_right_right_less_, "2", "* 50",   "* 10");
	range_false(range_right_right_less_, "3", "* 10",   "* 10");
	range_true (range_right_right_less_, "4", "* (10)", "* 10");
	range_false(range_right_right_less_, "5", "* 10",   "* (10)");
	range_false(range_right_right_less_, "6", "* (10)", "* (10)");

	RETURN;
}

static int test_range_right_right_less_equal(void)
{
	range_true (range_right_right_less_equal_, "1", "* 10",   "* 50");
	range_false(range_right_right_less_equal_, "2", "* 50",   "* 10");
	range_true (range_right_right_less_equal_, "3", "* 10",   "* 10");
	range_true (range_right_right_less_equal_, "4", "* (10)", "* 10");
	range_false(range_right_right_less_equal_, "5", "* 10",   "* (10)");
	range_true (range_right_right_less_equal_, "6", "* (10)", "* (10)");

	RETURN;
}

static int test_range_right_right_greater(void)
{
	range_true (range_right_right_greater_, "1", "* 50",   "* 10");
	range_false(range_right_right_greater_, "2", "* 10",   "* 50");
	range_false(range_right_right_greater_, "3", "* 10",   "* 10");
	range_false(range_right_right_greater_, "4", "* (10)", "* 10");
	range_true (range_right_right_greater_, "5", "* 10",   "* (10)");
	range_false(range_right_right_greater_, "6", "* (10)", "* (10)");

	RETURN;
}

static int test_range_right_right_greater_equal(void)
{
	range_true (range_right_right_greater_equal_, "1", "* 50",   "* 10");
	range_false(range_right_right_greater_equal_, "2", "* 10",   "* 50");
	range_true (range_right_right_greater_equal_, "3", "* 10",   "* 10");
	range_false(range_right_right_greater_equal_, "4", "* (10)", "* 10");
	range_true (range_right_right_greater_equal_, "5", "* 10",   "* (10)");
	range_true (range_right_right_greater_equal_, "6", "* (10)", "* (10)");

	RETURN;
}

static int test_range_between_left(void)
{
	range_true (range_between_left_, "1", "10 30",   "20 *");
	range_false(range_between_left_, "2", "10 30",   "5 *");
	range_false(range_between_left_, "3", "10 30",   "50 *");
	range_true (range_between_left_, "4", "10 30",   "10 *");
	range_false(range_between_left_, "5", "(10) 30", "10 *");
	range_true (range_between_left_, "6", "10 30",   "(10) *");
	range_true (range_between_left_, "7", "(10) 30", "(10) *");
	range_true (range_between_left_, "8", "10 30",   "30 *");
	range_false(range_between_left_, "9", "10 (30)", "30 *");
	range_false(range_between_left_, "10", "10 30",  "(30) *");
	range_false(range_between_left_, "11", "10 (30)","(30) *");

	RETURN;
}

static int test_range_left_between(void)
{
	range_true (range_left_between_, "1", "20 *",    "10 30");
	range_false(range_left_between_, "2", "5 *",     "10 30");
	range_false(range_left_between_, "3", "50 *",    "10 30");
	range_true (range_left_between_, "4", "10 *",    "10 30");
	range_false(range_left_between_, "5", "10 *",    "(10) 30");
	range_true (range_left_between_, "6", "(10) *",  "10 30");
	range_true (range_left_between_, "7", "(10) *",  "(10) 30");
	range_true (range_left_between_, "8", "30 *",    "10 30");
	range_false(range_left_between_, "9", "30 *",    "10 (30)");
	range_false(range_left_between_, "10", "(30) *", "10 30");
	range_false(range_left_between_, "11", "(30) *", "10 (30)");

	RETURN;
}

static int test_range_between_right(void)
{
	range_true (range_between_right_, "1", "10 30",    "* 20");
	range_false(range_between_right_, "2", "10 30",    "* 5");
	range_false(range_between_right_, "3", "10 30",    "* 50");
	range_true (range_between_right_, "4", "10 30",    "* 10");
	range_false(range_between_right_, "5", "(10) 30",  "* 10");
	range_false(range_between_right_, "6", "10 30",    "* (10)");
	range_false(range_between_right_, "7", "(10) 30",  "* (10)");
	range_true (range_between_right_, "8", "10 30",    "* 30");
	range_false(range_between_right_, "9", "10 (30)",  "* 30");
	range_true (range_between_right_, "10", "10 30",   "* (30)");
	range_true (range_between_right_, "11", "10 (30)", "* (30)");

	RETURN;
}

static int test_range_right_between(void)
{
	range_true (range_right_between_, "1", "* 20",    "10 30");
	range_false(range_right_between_, "2", "* 5",     "10 30");
	range_false(range_right_between_, "3", "* 50",    "10 30");
	range_true (range_right_between_, "4", "* 10",    "10 30");
	range_false(range_right_between_, "5", "* 10",    "(10) 30");
	range_false(range_right_between_, "6", "* (10)",  "10 30");
	range_false(range_right_between_, "7", "* (10)",  "(10) 30");
	range_true (range_right_between_, "8", "* 30",    "10 30");
	range_false(range_right_between_, "9", "* 30",    "10 (30)");
	range_true (range_right_between_, "10", "* (30)", "10 30");
	range_true (range_right_between_, "11", "* (30)", "10 (30)");

	RETURN;
}

static int test_range_between_in(void)
{
	range_true (range_between_in_, "1", "20 50",    "30 40");
	range_false(range_between_in_, "2", "20 50",    "10 15");
	range_false(range_between_in_, "3", "20 50",    "60 70");
	range_false(range_between_in_, "4", "20 50",    "10 40");
	range_false(range_between_in_, "5", "20 50",    "40 100");
	range_false(range_between_in_, "6", "20 50",    "10 100");
	range_true (range_between_in_, "7", "20 50",    "20 40");
	range_false(range_between_in_, "8", "(20) 50",  "20 40");
	range_true (range_between_in_, "9", "20 50",    "(20) 40");
	range_true (range_between_in_, "10", "(20) 50", "(20) 40");
	range_true (range_between_in_, "11", "20 50",   "30 50");
	range_false(range_between_in_, "12", "20 (50)", "30 50");
	range_true (range_between_in_, "13", "20 50",   "30 (50)");
	range_true (range_between_in_, "14", "20 (50)", "30 (50)");

	RETURN;
}

static int test_range_in_between(void)
{
	range_true (range_in_between_, "1", "30 40",    "20 50");
	range_false(range_in_between_, "2", "10 15",    "20 50");
	range_false(range_in_between_, "3", "60 70",    "20 50");
	range_false(range_in_between_, "4", "10 40",    "20 50");
	range_false(range_in_between_, "5", "40 100",   "20 50");
	range_false(range_in_between_, "6", "10 100",   "20 50");
	range_true (range_in_between_, "7", "20 40",    "20 50");
	range_false(range_in_between_, "8", "20 40",    "(20) 50");
	range_true (range_in_between_, "9", "(20) 40",  "20 50");
	range_true (range_in_between_, "10", "(20) 40", "(20) 50");
	range_true (range_in_between_, "11", "30 50",   "20 50");
	range_false(range_in_between_, "12", "30 50",   "20 (50)");
	range_true (range_in_between_, "13", "30 (50)", "20 50");
	range_true (range_in_between_, "14", "30 (50)", "20 (50)");

	RETURN;
}

static int test_range_connect_right_left(void)
{
	range_true (range_connect_right_left_, "1", "* 20",   "10 *");
	range_false(range_connect_right_left_, "2", "* 10",   "20 *");
	range_true (range_connect_right_left_, "3", "* 10",   "10 *");
	range_true (range_connect_right_left_, "4", "* (10)", "10 *");
	range_true (range_connect_right_left_, "5", "* 10",   "(10) *");
	range_false(range_connect_right_left_, "6", "* (10)", "(10) *");

	RETURN;
}

static int test_range_connect_between_left(void)
{
	range_true (range_connect_between_left_, "1", "10 30",   "20 *");
	range_false(range_connect_between_left_, "2", "10 30",   "5 *");
	range_false(range_connect_between_left_, "3", "10 30",   "50 *");
	range_true (range_connect_between_left_, "4", "10 30",   "30 *");
	range_true (range_connect_between_left_, "5", "10 (30)", "30 *");
	range_true (range_connect_between_left_, "6", "10 30",   "(30) *");
	range_false(range_connect_between_left_, "7", "10 (30)", "(30) *");

	RETURN;
}

static int test_range_connect_between_right(void)
{
	range_true (range_connect_between_right_, "1", "10 30",   "* 20");
	range_false(range_connect_between_right_, "2", "10 30",   "* 5");
	range_false(range_connect_between_right_, "3", "10 30",   "* 50");
	range_true (range_connect_between_right_, "4", "10 30",   "* 10");
	range_true (range_connect_between_right_, "5", "(10) 30", "* 10");
	range_true (range_connect_between_right_, "6", "10 30",   "* (10)");
	range_false(range_connect_between_right_, "7", "(10) 30", "* (10)");

	RETURN;
}


/*
 *  main
 */
static int testcase_type_range(void)
{
	TestBreak(test_range_asterisk_p);
	TestBreak(test_range_left_p);
	TestBreak(test_range_left_any_p);
	TestBreak(test_range_right_p);
	TestBreak(test_range_any_right_p);
	TestBreak(test_range_between_p);
	TestBreak(test_range_left_value);
	TestBreak(test_range_right_value);
	/* greate / less */
	TestBreak(test_range_left_left_less);
	TestBreak(test_range_left_left_less_equal);
	TestBreak(test_range_left_left_greater);
	TestBreak(test_range_left_left_greater_equal);
	TestBreak(test_range_left_right_less);
	TestBreak(test_range_left_right_less_equal);
	TestBreak(test_range_left_right_greater);
	TestBreak(test_range_left_right_greater_equal);
	TestBreak(test_range_right_left_less);
	TestBreak(test_range_right_left_less_equal);
	TestBreak(test_range_right_left_greater);
	TestBreak(test_range_right_left_greater_equal);
	TestBreak(test_range_right_right_less);
	TestBreak(test_range_right_right_less_equal);
	TestBreak(test_range_right_right_greater);
	TestBreak(test_range_right_right_greater_equal);
	TestBreak(test_range_between_left);
	TestBreak(test_range_left_between);
	TestBreak(test_range_between_right);
	TestBreak(test_range_right_between);
	TestBreak(test_range_between_in);
	TestBreak(test_range_in_between);
	TestBreak(test_range_connect_right_left);
	TestBreak(test_range_connect_between_left);
	TestBreak(test_range_connect_between_right);

	return 0;
}

static void testinit_type_range(Execute ptr)
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

int test_type_range(void)
{
	DegradeTitle;
	return DegradeCode(type_range);
}

