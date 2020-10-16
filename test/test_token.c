#include "token.c"
#include "bignum_cons.h"
#include "bignum_equal.h"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "control_object.h"
#include "constant.h"
#include "degrade.h"
#include "object.h"
#include "print.h"
#include "package.h"
#include "package_intern.h"
#include "pathname.h"
#include "ratio.h"
#include "reader.h"
#include "stream.h"
#include "strtype.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  integer
 */
static int test_getvalue_digit(void)
{
	unsigned value;

	test(getvalue_digit(10, '0', &value) == 0, "getvalue_digit1");
	test(value == 0, "getvalue_digit2");
	test(getvalue_digit(8, '3', &value) == 0, "getvalue_digit3");
	test(value == 3, "getvalue_digit4");
	test(getvalue_digit(8, '7', &value) == 0, "getvalue_digit5");
	test(value == 7, "getvalue_digit6");
	test(getvalue_digit(8, '8', &value), "getvalue_digit7");
	test(getvalue_digit(16, 'a', &value) == 0, "getvalue_digit8");
	test(value == 10, "getvalue_digit9");
	test(getvalue_digit(16, 'F', &value) == 0, "getvalue_digit10");
	test(value == 15, "getvalue_digit11");
	test(getvalue_digit(16, 'g', &value), "getvalue_digit12");

	RETURN;
}

static int test_getchar_digit(void)
{
	unicode u;

	test(getchar_digit(100, 0, &u), "getchar_digit1");
	test(! getchar_digit(4, 0, &u), "getchar_digit2");
	test(u == '4', "getchar_digit3");
	test(! getchar_digit(9, 0, &u), "getchar_digit4");
	test(u == '9', "getchar_digit5");
	test(! getchar_digit(10, 0, &u), "getchar_digit6");
	test(u == 'a', "getchar_digit7");
	test(! getchar_digit(15, 1, &u), "getchar_digit8");
	test(u == 'F', "getchar_digit9");

	RETURN;
}

static int test_checkvalue_digit(void)
{
	test(checkvalue_digit(16, 'B') == 11, "checkvalue_digit1");
	RETURN;
}

static int test_maketoken_fixnum(const char *str, unsigned base, fixnum value)
{
	LocalRoot local;
	LocalStack stack;
	addr queue, pos;

	local = Local_Thread;
	push_local(local, &stack);
	charqueue_heap(&queue, 4);
	pushchar_charqueue_heap_(queue, str);
	maketoken_integer(local, queue, base, &pos);
	if (GetType(pos) != LISPTYPE_FIXNUM) {
		degrade_printf("fixnum type error.\n");
		goto error;
	}
	if (RefFixnum(pos) != value) {
		degrade_printf("fixnum value error.\n");
		goto error;
	}
	rollback_local(local, stack);
	return 1;
error:
	rollback_local(local, stack);
	return 0;
}

static int test_maketoken_integer(void)
{
	test(test_maketoken_fixnum("0", 10, 0), "maketoken_integer1");
	test(test_maketoken_fixnum("123", 10, 123), "maketoken_integer2");
	test(test_maketoken_fixnum("234", 8, 0234), "maketoken_integer3");
	test(test_maketoken_fixnum("ABCD", 16, 0xABCD), "maketoken_integer4");
	test(test_maketoken_fixnum("+987", 10, 987), "maketoken_integer5");
	test(test_maketoken_fixnum("+765", 8, 0765), "maketoken_integer6");
	test(test_maketoken_fixnum("+F123", 16, 0xF123), "maketoken_integer7");
	test(test_maketoken_fixnum("-123", 10, -123), "maketoken_integer8");
	test(test_maketoken_fixnum("-234", 8, -0234), "maketoken_integer9");
	test(test_maketoken_fixnum("-ABCD", 16, -0xABCD), "maketoken_integer10");

	RETURN;
}

static int test_maketoken_integer2(void)
{
	LocalRoot local;
	LocalStack stack;
	addr queue, pos;

	local = Local_Thread;
	push_local(local, &stack);
	charqueue_heap(&queue, 4);
	pushchar_charqueue_heap_(queue, "-FFFFFFFFFFFFFFAAAAAAAAAAACCCCCCCCCCCDDDDD");
	maketoken_integer(local, queue, 16, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "maketoken_integer2-1");

	RETURN;
}


/*
 *  float
 */
static int test_makefloat_buffer(void)
{
	char buffer[FLOATBUFFER];

	makefloat_buffer(0, "", 10, buffer);
	test(strcmp(buffer, "+0.0e0") == 0, "makefloat_buffer1");
	makefloat_buffer(1, "", 10, buffer);
	test(strcmp(buffer, "-0.0e0") == 0, "makefloat_buffer2");
	makefloat_buffer(0, "123", 10, buffer);
	test(strcmp(buffer, "+1.23e10") == 0, "makefloat_buffer3");
	makefloat_buffer(1, "5678", -4, buffer);
	test(strcmp(buffer, "-5.678e-4") == 0, "makefloat_buffer4");
	makefloat_buffer(0, "3", 9, buffer);
	test(strcmp(buffer, "+3.0e9") == 0, "makefloat_buffer5");

	RETURN;
}

static int test_makefloat_single(void)
{
	addr pos;

	test(makefloat_single_("+1.23", &pos) == 0, "makefloat_single1");
	test(GetType(pos) == LISPTYPE_SINGLE_FLOAT, "makefloat_single2");
	test(RefSingleFloat(pos) == 1.23f, "makefloat_single3");

	RETURN;
}

static int test_makefloat_double(void)
{
	addr pos;

	test(makefloat_double_("+1.23", &pos) == 0, "makefloat_double1");
	test(GetType(pos) == LISPTYPE_DOUBLE_FLOAT, "makefloat_double2");
	test(RefDoubleFloat(pos) == 1.23, "makefloat_double3");

	RETURN;
}

static int test_makefloat_long(void)
{
	addr pos;

	test(makefloat_long_("+1.23", &pos) == 0, "makefloat_long1");
	test(GetType(pos) == LISPTYPE_LONG_FLOAT, "makefloat_long2");
	test(RefLongFloat(pos) == 1.23L, "makefloat_long3");

	RETURN;
}

static int test_read_default_float_format(void)
{
	int check;
	Execute ptr;
	addr control, symbol, pos;

	ptr = Execute_Thread;
	read_default_float_format_(ptr, &check);
	test(check == 'f', "read_default_float_format1");

	push_control(ptr, &control);
	GetConstant(CONSTANT_SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &symbol);
	pushspecial_control(ptr, symbol, Nil);

	interncommon_debug("DOUBLE-FLOAT", &pos);
	setspecial_local(ptr, symbol, pos);
	read_default_float_format_(ptr, &check);
	test(check == 'd', "read_default_float_format2");

	interncommon_debug("LONG-FLOAT", &pos);
	setspecial_local(ptr, symbol, pos);
	read_default_float_format_(ptr, &check);
	test(check == 'l', "read_default_float_format3");

	interncommon_debug("SINGLE-FLOAT", &pos);
	setspecial_local(ptr, symbol, pos);
	read_default_float_format_(ptr, &check);
	test(check == 'f', "read_default_float_format4");

	interncommon_debug("SHORT-FLOAT", &pos);
	setspecial_local(ptr, symbol, pos);
	read_default_float_format_(ptr, &check);
	test(check == 's', "read_default_float_format5");

	interncommon_debug("CONS", &pos);
	setspecial_local(ptr, symbol, pos);
	read_default_float_format_(ptr, &check);
	test(check == 0, "read_default_float_format6");

	pop_control_(ptr, control);

	read_default_float_format_(ptr, &check);
	test(check == 'f', "read_default_float_format7");

	RETURN;
}


static int test_makefloat(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	test(makefloat_(ptr, 0, "0", 0, 'f', &pos) == 0, "makefloat1");
	test(RefSingleFloat(pos) == 0.0, "makefloat2");
	test(makefloat_(ptr, 0, "123", 4, 'd', &pos) == 0, "makefloat3");
	test(RefDoubleFloat(pos) == 1.23e4, "makefloat4");
	test(makefloat_(ptr, 1, "2", 3, 'l', &pos) == 0, "makefloat5");
	test(RefLongFloat(pos) == -2.0e3L, "makefloat6");
	test(makefloat_(ptr, 1, "123", -4, 'e', &pos) == 0, "makefloat7");
	test(RefSingleFloat(pos) == -1.23e-4f, "makefloat8");
	test(makefloat_(ptr, 1, "123", -4, 's', &pos) == 0, "makefloat9");
	test(RefSingleFloat(pos) == -1.23e-4f, "makefloat10");
	test(makefloat_(ptr, 1, "123", -4, 0, &pos) == 0, "makefloat11");
	test(pos == Unbound, "makefloat12");

	RETURN;
}

static int test_atolcheck(void)
{
	long value;

	test(atolcheck("0", &value) == 0, "atolcheck1");
	test(value == 0, "atolcheck2");
	test(atolcheck("10", &value) == 0, "atolcheck3");
	test(value == 10, "atolcheck4");
	test(atolcheck("+987", &value) == 0, "atolcheck5");
	test(value == 987, "atolcheck6");
	test(atolcheck("-2000", &value) == 0, "atolcheck7");
	test(value == -2000, "atolcheck8");
	test(atolcheck("Hello", &value), "atolcheck9");
	test(atolcheck("-99999999999999999999999999", &value), "atolcheck10");

	RETURN;
}

static int test_plus_safe(void)
{
	long value;

	test(plus_safe(10, 20, &value) == 0, "plus_safe1");
	test(value == 30, "plus_safe2");
	test(plus_safe(-10, 30, &value) == 0, "plus_safe3");
	test(value == 20, "plus_safe4");
	test(plus_safe(LONG_MAX, 100, &value), "plus_safe5");
	test(plus_safe(LONG_MIN, -100, &value), "plus_safe6");

	RETURN;
}

static int test_isexponent(void)
{
	test(isexponent('e') == 'e', "isexponent1");
	test(isexponent('E') == 'e', "isexponent2");
	test(isexponent('f') == 'f', "isexponent3");
	test(isexponent('S') == 's', "isexponent4");
	test(isexponent('d') == 'd', "isexponent5");
	test(isexponent('L') == 'l', "isexponent6");
	test(isexponent('a') == 0, "isexponent7");

	RETURN;
}

static int check_floattable(addr *ret, const char *str)
{
	addr pos;
	const unicode *body;
	size_t size;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, str);
	strvect_posbodylen(pos, &body, &size);
	floattable_(ptr, body, size, ret);
	return *ret == Unbound;
}

static int test_floattable(void)
{
	addr pos;

	test(check_floattable(&pos, "0") == 0, "floattable1");
	test(RefSingleFloat(pos) == 0.0f, "floattable2");
	test(check_floattable(&pos, "1.23e4") == 0, "floattable3");
	test(RefSingleFloat(pos) == 1.23e4f, "floattable4");
	test(check_floattable(&pos, "123.4e5") == 0, "floattable5");
	test(RefSingleFloat(pos) == 123.4e5f, "floattable6");
	test(check_floattable(&pos, "-12.3d4") == 0, "floattable7");
	test(RefDoubleFloat(pos) == -12.3e4, "floattable8");
	test(check_floattable(&pos, "+3l4") == 0, "floattable9");
	test(RefLongFloat(pos) == 3.0e4L, "floattable10");

	RETURN;
}

static int test_maketoken_float(void)
{
	addr queue, pos;
	Execute ptr;

	ptr = Execute_Thread;
	charqueue_heap(&queue, 3);
	pushchar_charqueue_heap_(queue, "-123.45d-6");
	maketoken_float_(ptr, queue, &pos);
	test(GetType(pos) == LISPTYPE_DOUBLE_FLOAT, "maketoken_float1");
	test(RefDoubleFloat(pos) == -123.45e-6, "maketoken_float2");

	RETURN;
}


/*
 *  ratio
 */
static int test_maketoken_ratio(void)
{
	addr queue, pos, left, right;
	LocalRoot local;

	local = Local_Thread;
	charqueue_heap(&queue, 32);

	pushchar_charqueue_heap_(queue, "4/6");
	maketoken_ratio(local, queue, 10, &pos);
	test(GetType(pos) == LISPTYPE_RATIO, "maketoken_ratio1");
	test(RefSignRatio(pos) == signplus_bignum, "maketoken_ratio2");

	GetNumerRatio(pos, &left);
	bignum_value_alloc(NULL, &right, signplus_bignum, 2);
	test(equal_bb_real(left, right), "maketoken_ratio3");

	GetDenomRatio(pos, &left);
	bignum_value_alloc(NULL, &right, signplus_bignum, 3);
	test(equal_bb_real(left, right), "maketoken_ratio4");

	clear_charqueue(queue);
	pushchar_charqueue_heap_(queue, "-ABC/ABD");
	maketoken_ratio(local, queue, 16, &pos);
	test(GetType(pos) == LISPTYPE_RATIO, "maketoken_ratio5");
	test(RefSignRatio(pos) == signminus_bignum, "maketoken_ratio6");

	GetNumerRatio(pos, &left);
	bignum_value_alloc(NULL, &right, signplus_bignum, 0xABC);
	test(equal_bb_real(left, right), "maketoken_ratio7");

	GetDenomRatio(pos, &left);
	bignum_value_alloc(NULL, &right, signplus_bignum, 0xABD);
	test(equal_bb_real(left, right), "maketoken_ratio8");

	RETURN;
}


/*
 *  main
 */
static int testcase_token(void)
{
	/* integer */
	TestBreak(test_getvalue_digit);
	TestBreak(test_getchar_digit);
	TestBreak(test_checkvalue_digit);
	TestBreak(test_maketoken_integer);
	TestBreak(test_maketoken_integer2);
	/* float */
	TestBreak(test_makefloat_buffer);
	TestBreak(test_makefloat_single);
	TestBreak(test_makefloat_double);
	TestBreak(test_makefloat_long);
	TestBreak(test_read_default_float_format);
	TestBreak(test_makefloat);
	TestBreak(test_atolcheck);
	TestBreak(test_plus_safe);
	TestBreak(test_isexponent);
	TestBreak(test_floattable);
	TestBreak(test_maketoken_float);
	/* ratio */
	TestBreak(test_maketoken_ratio);

	return 0;
}

static void testinit_token(Execute ptr)
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
}

int test_token(void)
{
	DegradeTitle;
	return DegradeCode(token);
}

