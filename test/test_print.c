#include "print.c"
#include "bigcons.h"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "control.h"
#include "degrade.h"
#include "package.h"
#include "pathname.h"
#include "ratio.h"
#include "readtable.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

#if 0
#define testString(stream, string, name) { \
	addr __check; \
	string_stream_heap(stream, &__check); \
	test(string_equal_char(__check, string), name); \
	clear_output_string_stream(stream); \
}

static int radix_check_call(addr stream, int base, const char *str, const char *name)
{
	write_radix_front(stream, base);
	testString(stream, str, name);
	RETURN;
}
#define radix_check(s,b,a1,a2) { \
	if (radix_check_call(s,b,a1,a2)) goto error; \
}

static int test_write_radix_front(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	radix_check(stream, 2, "#b", "write_radix_front1");
	radix_check(stream, 3, "#3r", "write_radix_front2");
	radix_check(stream, 8, "#o", "write_radix_front3");
	radix_check(stream, 10, "#10r", "write_radix_front4");
	radix_check(stream, 16, "#x", "write_radix_front5");
	radix_check(stream, 32, "#32r", "write_radix_front6");
	close_stream(stream);

	RETURN;
}

static int fixnum_value_check_call(addr stream, int base, fixnum value,
		const char *str, const char *name)
{
	addr check;

	write_fixnum_value(stream, value, base);
	string_stream_heap(stream, &check);
	test(string_equal_char(check, str), name);
	clear_output_string_stream(stream);

	RETURN;
}
#define fixnum_value_check(s,b,v,a1,a2) { \
	if (fixnum_value_check_call(s,b,v,a1,a2)) goto error; \
}

static int test_write_radix_value(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	fixnum_value_check(stream, 10, 100, "100", "write_radix_value1");
	fixnum_value_check(stream, 10, 0, "0", "write_radix_value2");
	fixnum_value_check(stream, 10, -10, "-10", "write_radix_value3");
	fixnum_value_check(stream, 16, 254, "FE", "write_radix_value4");
	fixnum_value_check(stream, 16, -254, "-FE", "write_radix_value5");
#ifdef LISP_64BIT
	fixnum_value_check(stream, 16, FIXNUM_MAX, "7FFFFFFFFFFFFFFF", "write_radix_value6");
	fixnum_value_check(stream, 16, FIXNUM_MIN, "-8000000000000000", "write_radix_value7");
#else
	fixnum_value_check(stream, 16, FIXNUM_MAX, "7FFFFFFF", "write_radix_value6");
	fixnum_value_check(stream, 16, FIXNUM_MIN, "-80000000", "write_radix_value7");
#endif
	close_stream(stream);

	RETURN;
}

static int fixnum_check_call(addr stream, int radix, int base, fixnum value,
		const char *str, const char *name)
{
	addr arg, check;
	struct PrintFormat fmt;

	/* format */
	format_print(Execute_Thread, &fmt);
	fmt.radix = radix;
	fmt.base = base;

	/* output */
	fixnum_heap(&arg, value);
	write_fixnum(&fmt, stream, arg);
	string_stream_heap(stream, &check);
	test(string_equal_char(check, str), name);
	clear_output_string_stream(stream);

	RETURN;
}
#define fixnum_check(s,r,b,v,a1,a2) { \
	if (fixnum_check_call(s,r,b,v,a1,a2)) goto error; \
}
static int test_write_fixnum(void)
{
	addr pos;

	open_output_string_stream(&pos, 0);
	fixnum_check(pos,1,10, 100, "100.", "write_fixnum1");
	fixnum_check(pos,1,10, 0, "0.", "write_fixnum2");
	fixnum_check(pos,1,10, -10, "-10.", "write_fixnum3");
	fixnum_check(pos,1,16, 254, "#xFE", "write_fixnum4");
	fixnum_check(pos,1,16, -254, "#x-FE", "write_fixnum5");
#ifdef LISP_64BIT
	fixnum_check(pos,1,16, FIXNUM_MAX, "#x7FFFFFFFFFFFFFFF", "write_fixnum6");
	fixnum_check(pos,1,16, FIXNUM_MIN, "#x-8000000000000000", "write_fixnum7");
#else
	fixnum_check(pos,1,16, FIXNUM_MAX, "#x7FFFFFFF", "write_fixnum6");
	fixnum_check(pos,1,16, FIXNUM_MIN, "#x-80000000", "write_fixnum7");
#endif

	fixnum_check(pos,0,10, 100, "100", "write_fixnum1");
	fixnum_check(pos,0,10, 0, "0", "write_fixnum2");
	fixnum_check(pos,0,10, -10, "-10", "write_fixnum3");
	fixnum_check(pos,0,16, 254, "FE", "write_fixnum4");
	fixnum_check(pos,0,16, -254, "-FE", "write_fixnum5");
#ifdef LISP_64BIT
	fixnum_check(pos,0,16, FIXNUM_MAX, "7FFFFFFFFFFFFFFF", "write_fixnum6");
	fixnum_check(pos,0,16, FIXNUM_MIN, "-8000000000000000", "write_fixnum7");
#else
	fixnum_check(pos,0,16, FIXNUM_MAX, "7FFFFFFF", "write_fixnum6");
	fixnum_check(pos,0,16, FIXNUM_MIN, "-80000000", "write_fixnum7");
#endif
	close_stream(pos);

	RETURN;
}

static int test_write_bignum_value(void)
{
	addr stream, pos, check, cons;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	open_output_string_stream(&stream, 0);
	bignum_zero_alloc(local, &pos);
	write_bignum_value(stream, signplus_bignum, pos, 10);
	string_stream_local(local, stream, &check);
	test(string_equal_char(check, "0"), "write_bignum_value1");

	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16, "abcdef123456789abcdef123456789");
	clear_output_string_stream(stream);
	bignum_cons_alloc(local, &pos, signplus_bignum, cons);
	write_bignum_value(stream, signplus_bignum, pos, 16);
	string_stream_local(local, stream, &check);
	test(string_equal_char(check, "ABCDEF123456789ABCDEF123456789"),
			"write_bignum_value1");

	clear_output_string_stream(stream);
	bignum_cons_alloc(local, &pos, signminus_bignum, cons);
	write_bignum_value(stream, signminus_bignum, pos, 16);
	string_stream_local(local, stream, &check);
	test(string_equal_char(check, "-ABCDEF123456789ABCDEF123456789"),
			"write_bignum_value2");

	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_write_bignum(void)
{
	addr stream, pos, check, cons;
	LocalRoot local;
	LocalStack stack;
	struct PrintFormat fmt;
	Execute ptr;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	format_print(ptr, &fmt);
	bigcons_local(local, &cons);
	open_output_string_stream(&stream, 0);

	fmt.radix = 1;
	fmt.base = 16;
	setchar_bigcons(local, cons, 16, "abcd");
	clear_output_string_stream(stream);
	bignum_cons_alloc(local, &pos, signplus_bignum, cons);
	write_bignum(&fmt, stream, pos);
	string_stream_local(local, stream, &check);
	test(string_equal_char(check, "#xABCD"), "write_bignum1");

	fmt.radix = 1;
	fmt.base = 8;
	setchar_bigcons(local, cons, 8, "1234");
	clear_output_string_stream(stream);
	bignum_cons_alloc(local, &pos, signminus_bignum, cons);
	write_bignum(&fmt, stream, pos);
	string_stream_local(local, stream, &check);
	test(string_equal_char(check, "#o-1234"), "write_bignum2");

	fmt.radix = 0;
	fmt.base = 8;
	setchar_bigcons(local, cons, 8, "1234");
	clear_output_string_stream(stream);
	bignum_cons_alloc(local, &pos, signminus_bignum, cons);
	write_bignum(&fmt, stream, pos);
	string_stream_local(local, stream, &check);
	test(string_equal_char(check, "-1234"), "write_bignum3");

	fmt.radix = 1;
	fmt.base = 10;
	setchar_bigcons(local, cons, 10, "1234");
	clear_output_string_stream(stream);
	bignum_cons_alloc(local, &pos, signminus_bignum, cons);
	write_bignum(&fmt, stream, pos);
	string_stream_local(local, stream, &check);
	test(string_equal_char(check, "-1234."), "write_bignum4");

	fmt.radix = 0;
	fmt.base = 10;
	setchar_bigcons(local, cons, 10, "1234");
	clear_output_string_stream(stream);
	bignum_cons_alloc(local, &pos, signminus_bignum, cons);
	write_bignum(&fmt, stream, pos);
	string_stream_local(local, stream, &check);
	test(string_equal_char(check, "-1234"), "write_bignum5");

	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static void test_ratio_alloc(LocalRoot local,
		addr *ret, int sign, bigtype v1, bigtype v2)
{
	addr numer, denom;
	bignum_value_alloc(local, &numer, signplus_bignum, v1);
	bignum_value_alloc(local, &denom, signplus_bignum, v2);
	make_ratio_alloc_unsafe(local, ret, sign, numer, denom);
}

static int test_write_ratio(void)
{
	addr stream, pos, check;
	LocalRoot local;
	LocalStack stack;
	struct PrintFormat fmt;
	Execute ptr;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	format_print(ptr, &fmt);
	open_output_string_stream(&stream, 0);

	fmt.radix = 1;
	fmt.base = 16;
	clear_output_string_stream(stream);
	test_ratio_alloc(local, &pos, signplus_bignum, 0, 10);
	write_ratio(&fmt, stream, pos);
	string_stream_local(local, stream, &check);
	test(string_equal_char(check, "0"), "write_ratio1");

	fmt.radix = 1;
	fmt.base = 16;
	clear_output_string_stream(stream);
	test_ratio_alloc(local, &pos, signminus_bignum, 0x23, 1);
	write_ratio(&fmt, stream, pos);
	string_stream_local(local, stream, &check);
	test(string_equal_char(check, "#x-23"), "write_ratio2");

	fmt.radix = 1;
	fmt.base = 10;
	clear_output_string_stream(stream);
	test_ratio_alloc(local, &pos, signminus_bignum, 11, 12);
	write_ratio(&fmt, stream, pos);
	string_stream_local(local, stream, &check);
	test(string_equal_char(check, "#10r-11/12"), "write_ratio3");

	fmt.radix = 0;
	fmt.base = 10;
	clear_output_string_stream(stream);
	test_ratio_alloc(local, &pos, signplus_bignum, 33, 34);
	write_ratio(&fmt, stream, pos);
	string_stream_local(local, stream, &check);
	test(string_equal_char(check, "33/34"), "write_ratio4");

	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_write_character_name(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	write_character_name(stream, 'a');
	testString(stream, "a", "write_character_name1");
	write_character_name(stream, 'Z');
	testString(stream, "Z", "write_character_name2");
	write_character_name(stream, '*');
	testString(stream, "*", "write_character_name3");
	write_character_name(stream, 0xFFFF);
	testString(stream, "uFFFF", "write_character_name4");
	close_stream(stream);

	RETURN;
}

static int test_write_character_string(void)
{
	addr stream, str;

	open_output_string_stream(&stream, 0);
	strvect_char_heap(&str, "abc");
	write_character_string(stream, str);
	testString(stream, "abc", "write_character_string1");

	print_ascii_stream(stream, "Hello");
	write_character_string(stream, str);
	testString(stream, "Helloabc", "write_character_string2");
	close_stream(stream);

	RETURN;
}

static int write_character_call(addr stream, int escape, unicode u,
		const char *str, const char *name)
{
	addr arg;
	struct PrintFormat fmt;

	format_print(Execute_Thread, &fmt);
	fmt.escape = escape;
	character_heap(&arg, u);
	write_character(&fmt, stream, arg);
	testString(stream, str, name);

	RETURN;
}
#define write_character_check(s,e,u,a1,a2) { \
	if (write_character_call(s,e,u,a1,a2)) goto error; \
}

static int write_character_unicode(addr stream)
{
	addr arg, check;
	const unicode *u;
	size_t size;
	struct PrintFormat fmt;

	format_print(Execute_Thread, &fmt);
	fmt.escape = 0;
	character_heap(&arg, 0xFFFF);
	write_character(&fmt, stream, arg);

	string_stream_heap(stream, &check);
	test(GetType(check) == LISPTYPE_STRING, "write_character10");
	string_posbodylen(check, &u, &size);
	test(size == 1, "write_character11");
	test(u[0] == 0xFFFF, "write_character12");
	clear_output_string_stream(stream);

	RETURN;
}

static int test_write_character(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	write_character_check(stream, 1, 'a', "#\\a", "write_character1");
	write_character_check(stream, 1, 'Z', "#\\Z", "write_character2");
	write_character_check(stream, 1, ' ', "#\\Space", "write_character3");
	write_character_check(stream, 1, '*', "#\\*", "write_character4");
	write_character_check(stream, 1, 0xFFFF, "#\\uFFFF", "write_character5");

	write_character_check(stream, 0, 'a', "a", "write_character6");
	write_character_check(stream, 0, 'Z', "Z", "write_character7");
	write_character_check(stream, 0, ' ', " ", "write_character8");
	write_character_check(stream, 0, '*', "*", "write_character9");
	if (write_character_unicode(stream)) goto error;
	close_stream(stream);

	RETURN;
}

/* string */
static int write_string_call(addr stream, int escape,
		const char *input, const char *output, const char *name)
{
	addr str, check;
	struct PrintFormat fmt;

	/* call */
	format_print(Execute_Thread, &fmt);
	fmt.escape = escape;
	strvect_char_heap(&str, input);
	write_strtype(&fmt, stream, str);

	/* check */
	string_stream_heap(stream, &check);
	test(string_equal_char(check, output), name);
	clear_output_string_stream(stream);

	RETURN;
}
#define write_string_check(s,e,a1,a2,a3) { \
	if (write_string_call(s,e,a1,a2,a3)) goto error; \
}

static int test_write_string(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	write_string_check(stream, 0, "", "", "write_string11");
	write_string_check(stream, 0, "a", "a", "write_string12");
	write_string_check(stream, 0, "ABC", "ABC", "write_string13");
	write_string_check(stream, 0, "ZZA\"BC", "ZZA\"BC", "write_string14");
	write_string_check(stream, 0, "ZZA\\BC", "ZZA\\BC", "write_string15");
	write_string_check(stream, 1, "", "\"\"", "write_string16");
	write_string_check(stream, 1, "a", "\"a\"", "write_string17");
	write_string_check(stream, 1, "ABC", "\"ABC\"", "write_string18");
	write_string_check(stream, 1, "ZZA\"BC", "\"ZZA\\\"BC\"", "write_string19");
	write_string_check(stream, 1, "ZZA\\BC", "\"ZZA\\\\BC\"", "write_string20");
	close_stream(stream);

	RETURN;
}

static int symbol_case_call(
		int escape, int gensym,
		enum PrintCase readcase, enum PrintCase printcase,
		addr stream,
		const char *pname, const char *sname,
		const char *result, const char *name)
{
	addr check, package, symbol;
	struct PrintFormat fmt;

	format_print(Execute_Thread, &fmt);
	fmt.escape = escape;
	fmt.gensym = gensym;
	fmt.readcase = readcase;
	fmt.printcase = printcase;

	symbol_heap(&symbol);
	strvect_char_heap(&check, sname);
	SetNameSymbol(symbol, check);
	if (pname) {
		find_char_package(pname, &package);
		if (package == Nil) {
			strvect_char_heap(&package, pname);
			make_package(package, Nil, Nil, &package);
		}
		SetPackageSymbol(symbol, package);
	}

	write_symbol(&fmt, stream, symbol);
	string_stream_heap(stream, &check);
	test(string_equal_char(check, result), name);
	clear_output_string_stream(stream);

	RETURN;
}

#define norm_up_up(s,s1,s2,s3,s4) { \
	if (symbol_case_call(0, 1, PrintCase_upcase, PrintCase_upcase, \
				s, s1,s2,s3,s4)) goto error; \
}
static int test_write_norm_up_up(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	norm_up_up(stream, "TEST", "A", "A", "norm_up_up1");
	norm_up_up(stream, "TEST", "ABC", "ABC", "norm_up_up2");
	norm_up_up(stream, "TEST", "abc", "abc", "norm_up_up3");
	norm_up_up(stream, "TEST", "Abc", "Abc", "norm_up_up4");
	close_stream(stream);

	RETURN;
}

#define norm_up_down(s,s1,s2,s3,s4) { \
	if (symbol_case_call(0, 1, PrintCase_upcase, PrintCase_downcase, \
				s, s1,s2,s3,s4)) goto error; \
}
static int test_write_norm_up_down(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	norm_up_down(stream, "TEST", "A", "a", "norm_up_down1");
	norm_up_down(stream, "TEST", "ABC", "abc", "norm_up_down2");
	norm_up_down(stream, "TEST", "abc", "abc", "norm_up_down3");
	norm_up_down(stream, "TEST", "Abc", "abc", "norm_up_down4");
	close_stream(stream);

	RETURN;
}

#define norm_up_cap(s,s1,s2,s3,s4) { \
	if (symbol_case_call(0, 1, PrintCase_upcase, PrintCase_capitalize, \
				s, s1,s2,s3,s4)) goto error; \
}
static int test_write_norm_up_cap(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	norm_up_cap(stream, "TEST", "A", "A", "norm_up_cap1");
	norm_up_cap(stream, "TEST", "ABC", "Abc", "norm_up_cap2");
	norm_up_cap(stream, "TEST", "abc", "abc", "norm_up_cap3");
	norm_up_cap(stream, "TEST", "Abc", "Abc", "norm_up_cap4");
	norm_up_cap(stream, "TEST", "abC", "abc", "norm_up_cap5");
	norm_up_cap(stream, "TEST", "DON'T", "Don'T", "norm_up_cap6");
	norm_up_cap(stream, "TEST", "abc-0ef'ghi", "abc-0ef'ghi", "norm_up_cap7");
	norm_up_cap(stream, "TEST", "ABC-0EF'GHI", "Abc-0ef'Ghi", "norm_up_cap8");
	close_stream(stream);

	RETURN;
}

#define norm_down_up(s,s1,s2,s3,s4) { \
	if (symbol_case_call(0, 1, PrintCase_downcase, PrintCase_upcase, \
				s, s1,s2,s3,s4)) goto error; \
}
static int test_write_norm_down_up(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	norm_down_up(stream, "TEST", "A", "A", "norm_down_up1");
	norm_down_up(stream, "TEST", "ABC", "ABC", "norm_down_up2");
	norm_down_up(stream, "TEST", "abc", "ABC", "norm_down_up3");
	norm_down_up(stream, "TEST", "Abc", "ABC", "norm_down_up4");
	norm_down_up(stream, "TEST", "abc0def", "ABC0DEF", "norm_down_up5");
	close_stream(stream);

	RETURN;
}

#define norm_down_down(s,s1,s2,s3,s4) { \
	if (symbol_case_call(0, 1, PrintCase_downcase, PrintCase_downcase, \
				s, s1,s2,s3,s4)) goto error; \
}
static int test_write_norm_down_down(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	norm_down_down(stream, "TEST", "A", "A", "norm_down_down1");
	norm_down_down(stream, "TEST", "ABC", "ABC", "norm_down_down2");
	norm_down_down(stream, "TEST", "abc", "abc", "norm_down_down3");
	norm_down_down(stream, "TEST", "Abc", "Abc", "norm_down_down4");
	norm_down_down(stream, "TEST", "abc0def", "abc0def", "norm_down_down5");
	norm_down_down(stream, "TEST", "AbcDef", "AbcDef", "norm_down_down6");
	close_stream(stream);

	RETURN;
}

#define norm_down_cap(s,s1,s2,s3,s4) { \
	if (symbol_case_call(0, 1, PrintCase_downcase, PrintCase_capitalize, \
				s, s1,s2,s3,s4)) goto error; \
}
static int test_write_norm_down_cap(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	norm_down_cap(stream, "TEST", "A", "A", "norm_down_cap1");
	norm_down_cap(stream, "TEST", "ABC", "ABC", "norm_down_cap2");
	norm_down_cap(stream, "TEST", "abc", "Abc", "norm_down_cap3");
	norm_down_cap(stream, "TEST", "Abc", "Abc", "norm_down_cap4");
	norm_down_cap(stream, "TEST", "aBC", "ABC", "norm_down_cap5");
	norm_down_cap(stream, "TEST", "DON'T", "DON'T", "norm_down_cap6");
	norm_down_cap(stream, "TEST", "don't", "Don'T", "norm_down_cap7");
	norm_down_cap(stream, "TEST", "Abc0def", "Abc0def", "norm_down_cap8");
	norm_down_cap(stream, "TEST", "ABCDEF", "ABCDEF", "norm_down_cap9");
	norm_down_cap(stream, "TEST", "abc-0ef'ghi", "Abc-0ef'Ghi", "norm_down_cap10");
	norm_down_cap(stream, "TEST", "ABC-0EF'GHI", "ABC-0EF'GHI", "norm_down_cap11");
	close_stream(stream);

	RETURN;
}

#define norm_preserve(s,s1,s2,s3,s4) { \
	if (symbol_case_call(0, 1, PrintCase_preserve, PrintCase_upcase, \
				s, s1,s2,s3,s4)) goto error; \
}
static int test_write_norm_preserve(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	norm_preserve(stream, "TEST", "A", "A", "norm_preserve1");
	norm_preserve(stream, "TEST", "ABC", "ABC", "norm_preserve2");
	norm_preserve(stream, "TEST", "abc", "abc", "norm_preserve3");
	norm_preserve(stream, "TEST", "Abc", "Abc", "norm_preserve4");
	norm_preserve(stream, "TEST", "aBC", "aBC", "norm_preserve5");
	norm_preserve(stream, "TEST", "DON'T", "DON'T", "norm_preserve6");
	norm_preserve(stream, "TEST", "don't", "don't", "norm_preserve7");
	norm_preserve(stream, "TEST", "Abc0def", "Abc0def", "norm_preserve8");
	norm_preserve(stream, "TEST", "ABCDEF", "ABCDEF", "norm_preserve9");
	norm_preserve(stream, "TEST", "abc-0ef'ghi", "abc-0ef'ghi", "norm_preserve10");
	norm_preserve(stream, "TEST", "ABC-0EF'GHI", "ABC-0EF'GHI", "norm_preserve11");
	close_stream(stream);

	RETURN;
}

#define norm_invert(s,s1,s2,s3,s4) { \
	if (symbol_case_call(0, 1, PrintCase_invert, PrintCase_upcase, \
				s, s1,s2,s3,s4)) goto error; \
}
static int test_write_norm_invert(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	norm_invert(stream, "TEST", "A", "a", "norm_invert1");
	norm_invert(stream, "TEST", "ABC", "abc", "norm_invert2");
	norm_invert(stream, "TEST", "abc", "ABC", "norm_invert3");
	norm_invert(stream, "TEST", "Abc", "Abc", "norm_invert4");
	norm_invert(stream, "TEST", "aBC", "aBC", "norm_invert5");
	norm_invert(stream, "TEST", "DON'T", "don't", "norm_invert6");
	norm_invert(stream, "TEST", "don't", "DON'T", "norm_invert7");
	norm_invert(stream, "TEST", "Abc0def", "Abc0def", "norm_invert8");
	norm_invert(stream, "TEST", "ABCDEF", "abcdef", "norm_invert9");
	norm_invert(stream, "TEST", "abc-0Ef'ghi", "abc-0Ef'ghi", "norm_invert10");
	norm_invert(stream, "TEST", "abc-0ef'ghi", "ABC-0EF'GHI", "norm_invert11");
	norm_invert(stream, "TEST", "ABC-0EF'GHI", "abc-0ef'ghi", "norm_invert12");
	close_stream(stream);

	RETURN;
}

#define esc_up_up(s,s1,s2,s3,s4) { \
	if (symbol_case_call(1, 1, PrintCase_upcase, PrintCase_upcase, \
				s, s1,s2,s3,s4)) goto error; \
}
#define esc_up_up_gensym_off(s,s1,s2,s3,s4) { \
	if (symbol_case_call(1, 0, PrintCase_upcase, PrintCase_upcase, \
				s, s1,s2,s3,s4)) goto error; \
}
static int test_write_esc_up_up(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	esc_up_up(stream, "TEST", "A", "TEST::A", "esc_up_up1");
	esc_up_up(stream, LISPNAME, "ABC", "ABC", "esc_up_up2");
	esc_up_up(stream, LISPNAME, "abc", "|abc|", "esc_up_up3");
	esc_up_up(stream, LISPNAME, "Abc", "|Abc|", "esc_up_up4");
	esc_up_up(stream, LISPNAME, "AB-C", "AB-C", "esc_up_up5");
	esc_up_up(stream, LISPNAME, "Ab-C", "|Ab-C|", "esc_up_up6");
	esc_up_up(stream, LISPNAME, "ab(C", "|ab(C|", "esc_up_up7");
	esc_up_up(stream, LISPNAME, "AB(C", "|AB(C|", "esc_up_up8");
	esc_up_up(stream, LISPNAME, "AB|C", "|AB\\|C|", "esc_up_up9");
	esc_up_up(stream, LISPNAME, "AB\\C", "|AB\\\\C|", "esc_up_up10");
	esc_up_up(stream, NULL, "HELLO100", "#:HELLO100", "esc_up_up11");
	esc_up_up_gensym_off(stream, NULL, "HELLO100", "HELLO100", "esc_up_up12");
	close_stream(stream);

	RETURN;
}

#define esc_up_down(s,s1,s2,s3,s4) { \
	if (symbol_case_call(1, 1, PrintCase_upcase, PrintCase_downcase, \
				s, s1,s2,s3,s4)) goto error; \
}
static int test_write_esc_up_down(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	esc_up_down(stream, "TEST", "A", "test::a", "esc_up_down1");
	esc_up_down(stream, LISPNAME, "ABC", "abc", "esc_up_down2");
	esc_up_down(stream, LISPNAME, "abc", "|abc|", "esc_up_down3");
	esc_up_down(stream, LISPNAME, "Abc", "|Abc|", "esc_up_down4");
	esc_up_down(stream, LISPNAME, "AB-C", "ab-c", "esc_up_down5");
	esc_up_down(stream, LISPNAME, "Ab-C", "|Ab-C|", "esc_up_down6");
	esc_up_down(stream, LISPNAME, "ab(C", "|ab(C|", "esc_up_down7");
	esc_up_down(stream, LISPNAME, "AB(C", "|AB(C|", "esc_up_down8");
	esc_up_down(stream, LISPNAME, "ab(c", "|ab(c|", "esc_up_down9");
	esc_up_down(stream, LISPNAME, "AB|C", "|AB\\|C|", "esc_up_down10");
	esc_up_down(stream, LISPNAME, "AB\\C", "|AB\\\\C|", "esc_up_down11");
	close_stream(stream);

	RETURN;
}

static int test_write_cons(void)
{
	addr stream, cons, left, right, check;
	struct PrintFormat fmt;

	format_print(Execute_Thread, &fmt);
	fmt.length = -1;
	fmt.level = -1;
	fmt.escape = 0;

	open_output_string_stream(&stream, 0);
	consnil_heap(&cons);
	fixnum_heap(&left, 100);
	SetCons(cons, left, Nil);
	write_cons(&fmt, stream, cons);
	string_stream_heap(stream, &check);
	test(string_equal_char(check, "(100)"), "write_cons1");
	clear_output_string_stream(stream);

	fixnum_heap(&left, 100);
	fixnum_heap(&right, 22);
	SetCons(cons, left, right);
	write_cons(&fmt, stream, cons);
	string_stream_heap(stream, &check);
	test(string_equal_char(check, "(100 . 22)"), "write_cons2");
	clear_output_string_stream(stream);

	fixnum_heap(&left, 100);
	consnil_heap(&right);
	SetCons(cons, left, right);
	fixnum_heap(&left, 22);
	SetCar(right, left);
	write_cons(&fmt, stream, cons);
	string_stream_heap(stream, &check);
	test(string_equal_char(check, "(100 22)"), "write_cons3");
	clear_output_string_stream(stream);

	fmt.length = 1;
	write_cons(&fmt, stream, cons);
	string_stream_heap(stream, &check);
	test(string_equal_char(check, "(100 ...)"), "write_cons4");
	clear_output_string_stream(stream);
	fmt.length = -1;

	fixnum_heap(&left, 100);
	consnil_heap(&right);
	SetCons(cons, left, right);
	consnil_heap(&left);
	consnil_heap(&check);
	SetCons(right, left, check);
	right = check;
	fixnum_heap(&check, 200);
	SetCar(left, check);
	fixnum_heap(&check, 300);
	SetCar(right, check);

	write_cons(&fmt, stream, cons);
	string_stream_heap(stream, &check);
	test(string_equal_char(check, "(100 (200) 300)"), "write_cons5");
	clear_output_string_stream(stream);

	fmt.level = 1;
	write_cons(&fmt, stream, cons);
	string_stream_heap(stream, &check);
	test(string_equal_char(check, "(100 # 300)"), "write_cons6");
	clear_output_string_stream(stream);

	close_stream(stream);

	RETURN;
}

/*
 *  print
 */
static int test_getboolean(void)
{
	addr symbol, control;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(SPECIAL_PRINT_ARRAY, &symbol);
	push_close_control(ptr, &control);
	pushspecial_control(ptr, symbol, Nil);
	test(! getboolean(ptr, CONSTANT_SPECIAL_PRINT_ARRAY), "getboolean1");
	free_control(ptr, control);

	RETURN;
}
#endif


/*
 *  main
 */
static int testbreak_print(void)
{
#if 0
	in_package_lisp_package();

	TestBreak(test_write_radix_front);
	TestBreak(test_write_radix_value);
	TestBreak(test_write_fixnum);
	TestBreak(test_write_bignum_value);
	TestBreak(test_write_bignum);
	TestBreak(test_write_ratio);
	/* float */

	TestBreak(test_write_character_name);
	TestBreak(test_write_character_string);
	TestBreak(test_write_character);
	TestBreak(test_write_string);
	TestBreak(test_write_norm_up_up);
	TestBreak(test_write_norm_up_down);
	TestBreak(test_write_norm_up_cap);
	TestBreak(test_write_norm_down_up);
	TestBreak(test_write_norm_down_down);
	TestBreak(test_write_norm_down_cap);
	TestBreak(test_write_norm_preserve);
	TestBreak(test_write_norm_invert);
	TestBreak(test_write_esc_up_up);
	TestBreak(test_write_esc_up_down);
	TestBreak(test_write_cons);
	/* print */
	TestBreak(test_getboolean);
#endif

	return 0;
}

int test_print(void)
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
		build_stream();
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_syscall();
		build_common();
		build_readtable();
		build_pathname();
		build_code();
		lisp_initialize = 1;
		result = testbreak_print();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

