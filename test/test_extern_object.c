#include "extern_object.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "constant.h"
#include "control.h"
#include "degrade.h"
#include "extern_sequence.h"
#include "integer.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "strvect.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  object
 */
static int test_lisp_character(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	unicode c;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_character_(&y, 'A');
	test(characterp(y), "lisp_character.1");
	GetCharacter(y, &c);
	test(c == 'A', "lisp_character.2");

	lisp_character_(x, 'a');
	test(lisp_character_p(x), "lisp_character.3");
	lisp_hold_value(x, &y);
	GetCharacter(y, &c);
	test(c == 'a', "lisp_character.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_fixnum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	fixnum v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_fixnum(&y, 123);
	test(fixnump(y), "lisp_fixnum.1");
	GetFixnum(y, &v);
	test(v == 123, "lisp_fixnum.2");

	lisp_fixnum(x, 456);
	test(lisp_fixnum_p(x), "lisp_fixnum.3");
	lisp_hold_value(x, &y);
	GetFixnum(y, &v);
	test(v == 456, "lisp_fixnum.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_float(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	single_float v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_float_(&y, 1.23f);
	test(single_float_p(y), "lisp_single_float.1");
	GetSingleFloat(y, &v);
	test(v == 1.23f, "lisp_single_float.2");

	lisp_float_(x, 4.56f);
	test(lisp_single_float_p(x), "lisp_single_float.3");
	lisp_hold_value(x, &y);
	GetSingleFloat(y, &v);
	test(v == 4.56f, "lisp_single_float.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_double(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	double_float v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_double_(&y, 1.23);
	test(double_float_p(y), "lisp_double_float.1");
	GetDoubleFloat(y, &v);
	test(v == 1.23, "lisp_double_float.2");

	lisp_double_(x, 4.56);
	test(lisp_double_float_p(x), "lisp_double_float.3");
	lisp_hold_value(x, &y);
	GetDoubleFloat(y, &v);
	test(v == 4.56, "lisp_double_float.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_long_double(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	long_float v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_long_double_(&y, 1.23L);
	test(long_float_p(y), "lisp_long_float.1");
	GetLongFloat(y, &v);
	test(v == 1.23L, "lisp_long_float.2");

	lisp_long_double_(x, 4.56L);
	test(lisp_long_float_p(x), "lisp_long_float.3");
	lisp_hold_value(x, &y);
	GetLongFloat(y, &v);
	test(v == 4.56L, "lisp_long_float.4");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  access
 */
static int test_lisp_zero_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	test(lisp_zero_p(T) == 0, "lisp_zero_p.1");
	fixnum_heap(&y, 0);
	test(lisp_zero_p(y), "lisp_zero_p.2");
	lisp_hold_set(x, y);
	test(lisp_zero_p(x), "lisp_zero_p.3");
	fixnum_heap(&y, 10);
	test(! lisp_zero_p(y), "lisp_zero_p.4");
	fixnum_heap(&y, -20);
	test(! lisp_zero_p(y), "lisp_zero_p.5");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_plus_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	test(lisp_plus_p(T) == 0, "lisp_plus_p.1");
	fixnum_heap(&y, 10);
	test(lisp_plus_p(y), "lisp_plus_p.2");
	lisp_hold_set(x, y);
	test(lisp_plus_p(x), "lisp_plus_p.3");
	fixnum_heap(&y, 0);
	test(! lisp_plus_p(y), "lisp_plus_p.4");
	fixnum_heap(&y, -20);
	test(! lisp_plus_p(y), "lisp_plus_p.5");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_minus_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	test(lisp_minus_p(T) == 0, "lisp_minus_p.1");
	fixnum_heap(&y, -10);
	test(lisp_minus_p(y), "lisp_minus_p.2");
	lisp_hold_set(x, y);
	test(lisp_minus_p(x), "lisp_minus_p.3");
	fixnum_heap(&y, 0);
	test(! lisp_minus_p(y), "lisp_minus_p.4");
	fixnum_heap(&y, 10);
	test(! lisp_minus_p(y), "lisp_minus_p.5");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_get_character(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	unicode c;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	character_heap(&y, 'A');
	lisp_get_character(y, &c);
	test(c == 'A', "lisp_get_character.1");

	character_heap(&y, 'z');
	lisp_hold_set(x, y);
	lisp_get_character(x, &c);
	test(c == 'z', "lisp_get_character.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_get_fixnum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	fixnum v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	fixnum_heap(&y, 123);
	lisp_get_fixnum(y, &v);
	test(v == 123, "lisp_get_fixnum.1");

	fixnum_heap(&y, 456);
	lisp_hold_set(x, y);
	lisp_get_fixnum(x, &v);
	test(v == 456, "lisp_get_fixnum.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_get_float(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	single_float v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	single_float_heap(&y, 1.23f);
	lisp_get_float_(y, &v);
	test(v == 1.23f, "lisp_get_float.1");

	fixnum_heap(&y, 456);
	lisp_hold_set(x, y);
	lisp_get_float_(x, &v);
	test(v == 456.0f, "lisp_get_float.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_get_double(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	double_float v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	double_float_heap(&y, 1.23);
	lisp_get_double_(y, &v);
	test(v == 1.23, "lisp_get_double.1");

	fixnum_heap(&y, 456);
	lisp_hold_set(x, y);
	lisp_get_double_(x, &v);
	test(v == 456.0, "lisp_get_double.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_get_long_double(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	long_float v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	long_float_heap(&y, 1.23L);
	lisp_get_long_double_(y, &v);
	test(v == 1.23L, "lisp_get_long_double.1");

	fixnum_heap(&y, 456);
	lisp_hold_set(x, y);
	lisp_get_long_double_(x, &v);
	test(v == 456.0L, "lisp_get_long_double.2");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  package
 */
static int test_lisp_package(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	strvect_char_heap(&y, "COMMON-LISP-USER");
	lisp_package_(x, y);
	test(lisp_package_p(x), "lisp_package.1");

	strvect_char_heap(&y, "COMMON-LISP");
	lisp_hold_set(x, y);
	lisp_package_(x, x);
	test(lisp_package_p(x), "lisp_package.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_package8(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_package8_(x, "COMMON-LISP-USER");
	test(lisp_package_p(x), "lisp_package8.1");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_package16(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;
	const byte16 str[] = { 'C','O','M','M','O','N','-','L','I','S','P',0 };

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_package16_(x, (const void *)str);
	test(lisp_package_p(x), "lisp_package16.1");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_package32(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;
	const unicode str[] = { 'C','O','M','M','O','N','-','L','I','S','P',0 };

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_package32_(x, (const void *)str);
	test(lisp_package_p(x), "lisp_package32.1");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  intern
 */
static int test_lisp_intern(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z, a, b;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();
	a = Lisp_hold();
	b = Lisp_hold();

	strvect_char_heap(&y, "COMMON-LISP-USER");
	strvect_char_heap(&z, "HELLO");
	lisp_intern_(x, y, z);
	test(lisp_symbol_p(x), "lisp_intern.1");
	lisp_hold_value(x, &y);
	internchar_("COMMON-LISP-USER", "HELLO", &z, NULL);
	test(y == z, "lisp_intern.2");

	lisp_package8_(a, "COMMON-LISP-USER");
	lisp_string8_(b, "ABC");
	lisp_intern_(x, a, b);
	test(lisp_symbol_p(x), "lisp_intern.3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_intern8(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_intern8_(x, "COMMON-LISP-USER", "HELLO");
	test(lisp_symbol_p(x), "lisp_intern8.1");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_intern16(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;
	const byte16 p[] = { 'C','O','M','M','O','N','-','L','I','S','P',0 };
	const byte16 n[] = { 'H','E','L','L','O',0 };

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_intern16_(x, (const void *)p, (const void *)n);
	test(lisp_symbol_p(x), "lisp_intern16.1");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_intern32(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;
	const unicode p[] = { 'C','O','M','M','O','N','-','L','I','S','P',0 };
	const unicode n[] = { 'H','E','L','L','O',0 };

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_intern32_(x, (const void *)p, (const void *)n);
	test(lisp_symbol_p(x), "lisp_intern32.1");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  reader
 */
static int test_lisp_reader(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	strvect_char_heap(&y, "(10 20 30)");
	lisp_reader_(x, y);
	test(lisp_cons_p(x), "lisp_reader.1");

	strvect_char_heap(&y, "123 abc");
	lisp_hold_set(x, y);
	lisp_reader_(x, x);
	test(lisp_fixnum_p(x), "lisp_reader.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_reader8(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_reader8_(x, "(10 20 30)");
	test(lisp_cons_p(x), "lisp_reader8.1");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_reader16(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;
	const byte16 str[] = { '1','2','3',0 };

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_reader16_(x, (const void *)str);
	test(lisp_fixnum_p(x), "lisp_reader16.1");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_reader32(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;
	const unicode str[] = { '1','2','3',0 };

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_reader32_(x, (const void *)str);
	test(lisp_fixnum_p(x), "lisp_reader32.1");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  pathname
 */
static int test_lisp_pathname(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	strvect_char_heap(&y, "/usr/local/bin/");
	lisp_pathname_(x, y);
	test(lisp_pathname_p(x), "lisp_pathname.1");

	strvect_char_heap(&y, "/usr/local/bin/");
	lisp_hold_set(x, y);
	lisp_pathname_(x, y);
	test(lisp_pathname_p(x), "lisp_pathname.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_pathname8(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_pathname8_(x, "/usr/local/bin/");
	test(lisp_pathname_p(x), "lisp_pathname8.1");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_pathname16(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;
	const byte16 str[] = { '/','u','s','r','/',0 };

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_pathname16_(x, (const void *)str);
	test(lisp_pathname_p(x), "lisp_pathname16.1");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_pathname32(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;
	const unicode str[] = { '/','u','s','r','/',0 };

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_pathname32_(x, (const void *)str);
	test(lisp_pathname_p(x), "lisp_pathname32.1");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_namestring(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_pathname8_(x, "/usr/local/bin/");
	lisp_hold_value(x, &y);
	lisp_namestring_(x, y);
	lisp_hold_value(x, &y);
	test(lisp_string_p(y), "lisp_namestring.1");

	lisp_pathname8_(x, "/usr/local/");
	lisp_namestring_(x, x);
	lisp_hold_value(x, &y);
	test(lisp_string_p(y), "lisp_namestring.2");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  paper
 */
static int test_lisp_paper(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, x;
	size_t size;

	lisp0_paper_(&pos, 10, 0);
	test(paperp(pos), "lisp_paper.1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "lisp_paper.2");
	lenarray(pos, &size);
	test(size == 10, "lisp_paper.3");

	lisp0_paper_(&pos, 0, 0x010000);
	test(GetStatusSize(pos) == LISPSIZE_BODY4, "lisp_paper.4");
	lenbody(pos, &size);
	test(size == 0x010000, "lisp_paper.5");

	lisp0_paper_(&pos, 4, 5);
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "lisp_paper.6");
	lenarray(pos, &size);
	test(size == 4, "lisp_paper.7");
	lenbody(pos, &size);
	test(size == 5, "lisp_paper.8");

	lisp0_paper_(&pos, 4, 0x0100);
	test(GetStatusSize(pos) == LISPSIZE_ARRAYBODY, "lisp_paper.9");
	lenarray(pos, &size);
	test(size == 4, "lisp_paper.10");
	lenbody(pos, &size);
	test(size == 0x0100, "lisp_paper.11");


	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp_paper_(x, 10, 20);
	test(holdp(x), "lisp_paper.12");
	test(lisp_paper_p(x), "lisp_paper.13");
	lisp_hold_value(x, &pos);
	lenarray(pos, &size);
	test(size == 10, "lisp_paper.14");
	lenbody(pos, &size);
	test(size == 20, "lisp_paper.15");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_paper_gettype(void)
{
	byte c;
	LocalRoot local;
	LocalStack stack;
	addr x, pos;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_paper_(&pos, 10, 20);
	c = 99;
	lisp_paper_gettype_(pos, &c);
	test(c == 0, "lisp_paper_gettype.1");
	lisp_paper_settype_(pos, 88);
	lisp_paper_gettype_(pos, &c);
	test(c == 88, "lisp_paper_gettype.2");

	lisp_paper_(x, 30, 40);
	c = 99;
	lisp_paper_gettype_(x, &c);
	test(c == 0, "lisp_paper_gettype.3");
	lisp_paper_settype_(x, 77);
	lisp_paper_gettype_(x, &c);
	test(c == 77, "lisp_paper_gettype.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_paper_lenarray(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, pos;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_paper_(&pos, 10, 20);
	size = 99;
	lisp_paper_lenarray_(pos, &size);
	test(size == 10, "lisp_paper_lenarray.1");
	lisp_paper_lenbody_(pos, &size);
	test(size == 20, "lisp_paper_lenarray.2");

	lisp_paper_(x, 30, 40);
	size = 99;
	lisp_paper_lenarray_(x, &size);
	test(size == 30, "lisp_paper_lenarray.3");
	lisp_paper_lenbody_(x, &size);
	test(size == 40, "lisp_paper_lenarray.4");

	lisp_paper_(x, 0, 40);
	size = 99;
	lisp_paper_lenarray_(x, &size);
	test(size == 0, "lisp_paper_lenarray.5");
	lisp_paper_lenbody_(x, &size);
	test(size == 40, "lisp_paper_lenarray.6");

	lisp_paper_(x, 30, 0);
	size = 99;
	lisp_paper_lenarray_(x, &size);
	test(size == 30, "lisp_paper_lenarray.7");
	lisp_paper_lenbody_(x, &size);
	test(size == 0, "lisp_paper_lenarray.8");

	lisp_paper_(x, 0, 0);
	size = 99;
	lisp_paper_lenarray_(x, &size);
	test(size == 0, "lisp_paper_lenarray.9");
	lisp_paper_lenbody_(x, &size);
	test(size == 0, "lisp_paper_lenarray.10");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_paper_getarray(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, v, pos, value;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();
	v = Lisp_hold();

	lisp0_paper_(&pos, 10, 20);
	lisp_paper_getarray_(v, pos, 1);
	lisp_hold_value(v, &value);
	test(value == Nil, "lisp_paper_getarray.1");
	value = NULL;
	lisp0_paper_getarray_(&value, pos, 1);
	test(value == Nil, "lisp_paper_getarray.2");

	lisp_paper_setarray_(pos, 2, T);
	lisp_paper_getarray_(v, pos, 2);
	lisp_hold_value(v, &value);
	test(value == T, "lisp_paper_getarray.3");
	value = NULL;
	lisp0_paper_getarray_(&value, pos, 2);
	test(value == T, "lisp_paper_getarray.4");

	lisp_paper_(x, 5, 0);
	lisp_paper_getarray_(v, x, 3);
	lisp_hold_value(v, &value);
	test(value == Nil, "lisp_paper_getarray.5");
	value = NULL;
	lisp0_paper_getarray_(&value, x, 3);
	test(value == Nil, "lisp_paper_getarray.6");

	lisp_paper_setarray_(x, 4, T);
	lisp_paper_getarray_(v, x, 4);
	lisp_hold_value(v, &value);
	test(value == T, "lisp_paper_getarray.7");
	value = NULL;
	lisp0_paper_getarray_(&value, x, 4);
	test(value == T, "lisp_paper_getarray.8");

	lisp_string8_(v, "Hello");
	lisp_paper_setarray_(x, 4, v);
	lisp_paper_getarray_(v, x, 4);
	lisp_hold_value(v, &value);
	test(strvectp(value), "lisp_paper_getarray.9");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_paper_getbody(void)
{
	byte value;
	LocalRoot local;
	LocalStack stack;
	addr x, pos;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_paper_(&pos, 10, 20);
	lisp_paper_setbody_(pos, 2, 123);
	lisp_paper_getbody_(pos, 2, &value);
	test(value == 123, "lisp_paper_getbody.1");

	lisp_paper_(x, 0, 5);
	lisp_paper_setbody_(x, 4, 45);
	lisp_paper_getbody_(x, 4, &value);
	test(value == 45, "lisp_paper_getbody.2");

	rollback_local(local, stack);

	RETURN;
}

static void test_lisp_paper_getmemory_data(addr x)
{
	byte c;
	size_t size, i;

	lisp_paper_lenbody_(x, &size);
	c = 0;
	for (i = 0; i < size; i++)
		lisp_paper_setbody_(x, i, c++);
}

static int test_lisp_paper_getmemory(void)
{
	byte data[100];
	const char *str;
	int check;
	LocalRoot local;
	LocalStack stack;
	addr x, pos;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_paper_(&pos, 10, 20);
	lisp_paper_getmemory_(pos, 0, 20, data, &size);
	test(size == 20, "lisp_paper_getmemory.1");
	check = lisp_paper_getmemory_(pos, 0, 20, data, NULL);
	test(check == 0, "lisp_paper_getmemory.2");

	lisp_paper_(x, 30, 40);
	lisp_paper_getmemory_(x, 0, 40, data, &size);
	test(size == 40, "lisp_paper_getmemory.3");
	check = lisp_paper_getmemory_(x, 0, 20, data, NULL);
	test(check == 0, "lisp_paper_getmemory.4");

	lisp_paper_(x, 0, 5);
	test_lisp_paper_getmemory_data(x);
	memset(data, 0xAA, 100);
	lisp_paper_getmemory_(x, 0, 5, data, &size);
	test(size == 5, "lisp_paper_getmemory.5");
	str = "\x00\x01\x02\x03\x04\xAA";
	test(memcmp(data, str, 6) == 0, "lisp_paper_getmemory.6");

	memset(data, 0xAA, 100);
	lisp_paper_getmemory_(x, 3, 6, data, &size);
	test(size == 2, "lisp_paper_getmemory.7");
	str = "\x03\x04\xAA\xAA\xAA\xAA";
	test(memcmp(data, str, 6) == 0, "lisp_paper_getmemory.8");

	lisp_paper_getmemory_(x, 4, 2, data, &size);
	test(size == 0, "lisp_paper_getmemory.9");
	lisp_paper_getmemory_(x, 2, 2, data, &size);
	test(size == 0, "lisp_paper_getmemory.10");
	lisp_paper_getmemory_(x, 20, 30, data, &size);
	test(size == 0, "lisp_paper_getmemory.11");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_paper_setmemory(void)
{
	byte data[100];
	const char *str;
	int check;
	LocalRoot local;
	LocalStack stack;
	addr x, pos;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_paper_(&pos, 10, 20);
	test_lisp_paper_getmemory_data(pos);
	memset(data, 0xAA, 100);
	lisp_paper_setmemory_(pos, 0, 20, data, &size);
	test(size == 20, "lisp_paper_setmemory.1");
	lisp_paper_getmemory_(pos, 0, 5, data, &size);
	str = "\xAA\xAA\xAA\xAA\xAA";
	test(memcmp(data, str, 5) == 0, "lisp_paper_setmemory.2");
	check = lisp_paper_setmemory_(pos, 0, 20, data, NULL);
	test(check == 0, "lisp_paper_setmemory.3");

	lisp_paper_(x, 10, 20);
	test_lisp_paper_getmemory_data(x);
	memset(data, 0xAA, 100);
	lisp_paper_setmemory_(x, 0, 20, data, &size);
	test(size == 20, "lisp_paper_setmemory.4");
	lisp_paper_getmemory_(x, 0, 5, data, &size);
	str = "\xAA\xAA\xAA\xAA\xAA";
	test(memcmp(data, str, 5) == 0, "lisp_paper_setmemory.5");
	check = lisp_paper_setmemory_(x, 0, 20, data, NULL);
	test(check == 0, "lisp_paper_setmemory.6");

	memset(data, 0xAA, 100);
	lisp_paper_setmemory_(x, 0, 20, data, NULL);
	str = "\xB1\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBB";
	lisp_paper_setmemory_(x, 2, 5, str, &size);
	test(size == 3, "lisp_paper_setmemory.7");
	lisp_paper_getmemory_(x, 0, 6, data, &size);
	str = "\xAA\xAA\xB1\xB2\xB3\xAA\xAA\xAA";
	test(memcmp(data, str, 6) == 0, "lisp_paper_setmemory.8");

	lisp_paper_setmemory_(x, 4, 2, data, &size);
	test(size == 0, "lisp_paper_setmemory.9");
	lisp_paper_setmemory_(x, 2, 2, data, &size);
	test(size == 0, "lisp_paper_setmemory.10");
	lisp_paper_setmemory_(x, 20, 30, data, &size);
	test(size == 0, "lisp_paper_setmemory.11");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_paper_body_unsafe(void)
{
	const char *str;
	int check;
	LocalRoot local;
	LocalStack stack;
	addr x, pos;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_paper_(&pos, 10, 20);
	lisp_paper_setmemory_(pos, 0, 6, "Hello", NULL);
	lisp_paper_body_unsafe_(pos, (byte **)&str, &size);
	test(strcmp(str, "Hello") == 0, "lisp_paper_body_unsafe.1");
	test(size == 20, "lisp_paper_body_unsafe.2");
	check = lisp_paper_body_unsafe_(pos, NULL, NULL);
	test(check == 0, "lisp_paper_body_unsafe.3");

	lisp_paper_(x, 10, 20);
	lisp_paper_setmemory_(x, 0, 6, "Hello", NULL);
	lisp_paper_body_unsafe_(x, (byte **)&str, &size);
	test(strcmp(str, "Hello") == 0, "lisp_paper_body_unsafe.4");
	test(size == 20, "lisp_paper_body_unsafe.5");
	check = lisp_paper_body_unsafe_(x, NULL, NULL);
	test(check == 0, "lisp_paper_body_unsafe.6");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testcase_extern_object(void)
{
	/* object */
	TestBreak(test_lisp_character);
	TestBreak(test_lisp_fixnum);
	TestBreak(test_lisp_float);
	TestBreak(test_lisp_double);
	TestBreak(test_lisp_long_double);
	/* access */
	TestBreak(test_lisp_zero_p);
	TestBreak(test_lisp_plus_p);
	TestBreak(test_lisp_minus_p);
	TestBreak(test_lisp_get_character);
	TestBreak(test_lisp_get_fixnum);
	TestBreak(test_lisp_get_float);
	TestBreak(test_lisp_get_double);
	TestBreak(test_lisp_get_long_double);
	/* package */
	TestBreak(test_lisp_package);
	TestBreak(test_lisp_package8);
	TestBreak(test_lisp_package16);
	TestBreak(test_lisp_package32);
	/* intern */
	TestBreak(test_lisp_intern);
	TestBreak(test_lisp_intern8);
	TestBreak(test_lisp_intern16);
	TestBreak(test_lisp_intern32);
	/* reader */
	TestBreak(test_lisp_reader);
	TestBreak(test_lisp_reader8);
	TestBreak(test_lisp_reader16);
	TestBreak(test_lisp_reader32);
	/* pathname */
	TestBreak(test_lisp_pathname);
	TestBreak(test_lisp_pathname8);
	TestBreak(test_lisp_pathname16);
	TestBreak(test_lisp_pathname32);
	TestBreak(test_lisp_namestring);
	/* paper */
	TestBreak(test_lisp_paper);
	TestBreak(test_lisp_paper_gettype);
	TestBreak(test_lisp_paper_lenarray);
	TestBreak(test_lisp_paper_getarray);
	TestBreak(test_lisp_paper_getbody);
	TestBreak(test_lisp_paper_getmemory);
	TestBreak(test_lisp_paper_setmemory);
	TestBreak(test_lisp_paper_body_unsafe);

	return 0;
}

static void testinit_extern_object(Execute ptr)
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

int test_extern_object(void)
{
	DegradeTitle;
	return DegradeCode(extern_object);
}

