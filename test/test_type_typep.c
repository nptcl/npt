#include "type_typep.c"
#include "array.h"
#include "array_make.h"
#include "clos.h"
#include "common.h"
#include "eval_declare.h"
#include "degrade.h"
#include "hashtable.h"
#include "package.h"
#include "random_state.h"
#include "readtable.h"
#include "stream.h"
#include "stream_broadcast.h"
#include "stream_concat.h"
#include "stream_echo.h"
#include "stream_file.h"
#include "stream_string.h"
#include "stream_synonym.h"
#include "stream_twoway.h"
#include "syscall.h"
#include "type_copy.h"

/*
 *  test tools
 */
static void test_parse_type_error(addr *ret, addr pos)
{
	if (parse_type(Execute_Thread, ret, pos, Nil))
		fmte("parse-type error.");
}

static void test_parse_char(addr *ret, const char *str)
{
	test_parse_type_error(ret, readr(str));
}

static int TypepClang(addr value, addr type, int *ret)
{
	return typep_clang(Execute_Thread, value, type, ret);
}

static int typep_object(addr x, addr y) 
{
	int check = 0;
	if (TypepClang(x, y, &check))
		fmte("typep error");
	return check;
}

static int typep_char(addr x, const char *str)
{
	addr y;
	test_parse_char(&y, str);
	return typep_object(x, y);
}

static int TypepAsteriskClang(addr x, addr y, int *ret)
{
	return typep_asterisk_clang(Execute_Thread, x, y, ret);
}

static int typep_asterisk_char(addr x, const char *str)
{
	int check;
	addr y;

	test_parse_char(&y, str);
	check = 0;
	if (TypepAsteriskClang(x, y, &check))
		fmte("typep error");

	return check;
}


/*
 *  system type
 */
static int test_typep_clos(void)
{
	addr v;

	v = readr("standard-class");
	clos_find_class(v, &v);
	test(typep_char(v, "class"), "typep_clos1");
	fixnum_heap(&v, 100);
	test(! typep_char(v, "class"), "typep_clos2");

	RETURN;
}

static int test_typep_asterisk(void)
{
	addr v;

	fixnum_heap(&v, 100);
	test(typep_asterisk_char(v, "*"), "typep_asterisk1");

	RETURN;
}


/*
 *  Compound-type
 */
static int test_typep_and(void)
{
	addr v;

	fixnum_heap(&v, 10);
	test(typep_char(v, "(and integer real rational)"), "typep_and1");
	test(! typep_char(v, "(and string real rational)"), "typep_and2");
	test(! typep_char(v, "(and integer real cons)"), "typep_and3");
	test(typep_char(v, "(and)"), "typep_and4");

	RETURN;
}

static int test_typep_or(void)
{
	addr v;

	fixnum_heap(&v, 10);
	test(typep_char(v, "(or integer real)"), "typep_or1");
	test(typep_char(v, "(or integer string real)"), "typep_or2");
	test(! typep_char(v, "(or cons symbol string)"), "typep_or3");
	test(! typep_char(v, "(or)"), "typep_or4");

	RETURN;
}

static int test_typep_eql(void)
{
	addr v;

	fixnum_heap(&v, 10);
	test(typep_char(v, "(eql 10)"), "typep_eql1");

	single_float_heap(&v, 10.0f);
	test(! typep_char(v, "(eql 10)"), "typep_eql2");

	character_heap(&v, 'A');
	test(typep_char(v, "(eql #\\A)"), "typep_eql3");

	v = readr("(10)");
	test(! typep_char(v, "(eql (10))"), "typep_eql4");

	RETURN;
}

static int test_typep_member(void)
{
	addr v;

	fixnum_heap(&v, 10);
	test(typep_char(v, "(member 10 20 #\\Z 30)"), "typep_member1");

	character_heap(&v, 'Z');
	test(typep_char(v, "(member 10 20 #\\Z 30)"), "typep_member2");

	fixnum_heap(&v, 11);
	test(! typep_char(v, "(member 10 20 #\\Z 30)"), "typep_member3");

	character_heap(&v, 'a');
	test(! typep_char(v, "(member 10 20 #\\Z 30)"), "typep_member4");

	RETURN;
}

static int test_typep_mod(void)
{
	addr v;

	fixnum_heap(&v, 10);
	test(typep_char(v, "(mod 256)"), "typep_mod1");

	fixnum_heap(&v, 255);
	test(typep_char(v, "(mod 256)"), "typep_mod2");

	fixnum_heap(&v, 256);
	test(! typep_char(v, "(mod 256)"), "typep_mod3");

	fixnum_heap(&v, 0);
	test(typep_char(v, "(mod 256)"), "typep_mod4");

	fixnum_heap(&v, -1);
	test(! typep_char(v, "(mod 256)"), "typep_mod5");

	v = readr(":hello");
	test(! typep_char(v, "(mod 256)"), "typep_mod6");

	RETURN;
}

static int test_typep_not(void)
{
	addr v;

	v = readr(":hello");
	test(typep_char(v, "(not integer)"), "typep_not1");

	fixnum_heap(&v, 10);
	test(! typep_char(v, "(not integer)"), "typep_not2");

	RETURN;
}

static int test_typep_satisfies(void)
{
	addr v;

	fixnum_heap(&v, 10);
	test(typep_char(v, "(satisfies plusp)"), "typep_satisfies1");

	fixnum_heap(&v, -10);
	test(! typep_char(v, "(satisfies plusp)"), "typep_satisfies2");

	RETURN;
}


/*
 *  Extract-type
 */
static int test_typep_atom(void)
{
	addr v;

	consnil_heap(&v);
	test(! typep_char(v, "atom"), "typep_atom1");

	fixnum_heap(&v, 100);
	test(typep_char(v, "atom"), "typep_atom2");
	test(typep_char(Nil, "atom"), "typep_atom3");
	test(typep_char(T, "atom"), "typep_atom4");

	RETURN;
}

static int test_typep_list(void)
{
	addr v;

	consnil_heap(&v);
	test(typep_char(v, "list"), "typep_list1");

	fixnum_heap(&v, 100);
	test(! typep_char(v, "list"), "typep_list2");
	test(typep_char(Nil, "list"), "typep_list3");
	test(! typep_char(T, "list"), "typep_list4");

	RETURN;
}

static int test_typep_boolean(void)
{
	addr v;

	consnil_heap(&v);
	test(! typep_char(v, "boolean"), "typep_boolean1");
	test(typep_char(Nil, "boolean"), "typep_boolean2");
	test(typep_char(T, "boolean"), "typep_boolean3");

	RETURN;
}

static int test_typep_vector_vector(void)
{
	int check;
	addr x, y;

	vector4_heap(&x, 3);
	SetArrayA4(x, 0, fixnum_heapr(10));
	SetArrayA4(x, 1, fixnum_heapr(20));
	SetArrayA4(x, 2, fixnum_heapr(30));
	test_parse_char(&y, "vector");
	test(! typep_vector_vector(x, y, &check), "tytep_vector_vector1");
	test(check, "tytep_vector_vector2");

	test_parse_char(&y, "(vector integer)"); /* integer -> t */
	test(! typep_vector_vector(x, y, &check), "typep_vector_vector3");
	test(check, "typep_vector_vector4");

	test_parse_char(&y, "(vector * 3)");
	test(! typep_vector_vector(x, y, &check), "typep_vector_vector5");
	test(check, "typep_vector_vector6");

	test_parse_char(&y, "(vector * 4)");
	test(! typep_vector_vector(x, y, &check), "typep_vector_vector7");
	test(! check, "typep_vector_vector8");

	vector4_heap(&x, 3);
	SetArrayA4(x, 0, character_heapr('A'));
	SetArrayA4(x, 1, character_heapr('B'));
	SetArrayA4(x, 2, character_heapr('C'));
	test_parse_char(&y, "(vector character)");
	test(! typep_vector_vector(x, y, &check), "typep_vector_vector9");
	test(! check, "typep_vector_vector10");

	RETURN;
}

static int test_typep_vector_string(void)
{
	int check;
	addr x, y;

	strvect_char_heap(&x, "Hello");
	test_parse_char(&y, "vector");
	test(! typep_vector_string(x, y, &check), "typep_vector_string1");
	test(check, "typep_vector_string2");

	test_parse_char(&y, "(vector character)");
	test(! typep_vector_string(x, y, &check), "typep_vector_string3");
	test(check, "typep_vector_string4");

	test_parse_char(&y, "(vector integer)");
	test(! typep_vector_string(x, y, &check), "typep_vector_string5");
	test(! check, "typep_vector_string6");

	test_parse_char(&y, "(vector * 5)");
	test(! typep_vector_string(x, y, &check), "typep_vector_string7");
	test(check, "typep_vector_string8");

	test_parse_char(&y, "(vector * 6)");
	test(! typep_vector_string(x, y, &check), "typep_vector_string9");
	test(! check, "typep_vector_string10");

	strarray_char_heap(&x, "Hello");
	test_parse_char(&y, "(vector * 5)");
	test(! typep_vector_string(x, y, &check), "typep_vector_string11");
	test(check, "typep_vector_string12");

	RETURN;
}

static int test_typep_vector_bitvector(void)
{
	int check;
	addr x, y;

	x = readr("#*10011");
	test_parse_char(&y, "vector");
	test(! typep_vector_bitvector(x, y, &check), "typep_vector_bitvector1");
	test(check, "typep_vector_bitvector2");

	test_parse_char(&y, "(vector bit)");
	test(! typep_vector_bitvector(x, y, &check), "typep_vector_bitvector3");
	test(check, "typep_vector_bitvector4");

	test_parse_char(&y, "(vector integer)");
	test(! typep_vector_bitvector(x, y, &check), "typep_vector_bitvector5");
	test(! check, "typep_vector_bitvector6");

	test_parse_char(&y, "(vector * 5)");
	test(! typep_vector_bitvector(x, y, &check), "typep_vector_bitvector7");
	test(check, "typep_vector_bitvector8");

	test_parse_char(&y, "(vector * 6)");
	test(! typep_vector_bitvector(x, y, &check), "typep_vector_bitvector9");
	test(! check, "typep_vector_bitvector10");

	test_parse_char(&y, "(vector bit 5)");
	test(! typep_vector_bitvector(x, y, &check), "typep_vector_bitvector11");
	test(check, "typep_vector_bitvector12");

	RETURN;
}

static int test_typep_vector_dimension(void)
{
	int check;
	addr x, y;

	array_va_heap(&x, 10, 0);
	test_parse_char(&y, "(vector * *)");
	GetArrayType(y, 1, &y);
	test(! typep_vector_dimension(x, y, &check), "typep_vector_dimension1");
	test(check, "typep_vector_dimension2");

	array_va_heap(&x, 10, 0);
	test_parse_char(&y, "(vector * 10)");
	GetArrayType(y, 1, &y);
	test(! typep_vector_dimension(x, y, &check), "typep_vector_dimension3");
	test(check, "typep_vector_dimension4");

	array_va_heap(&x, 0);
	test_parse_char(&y, "(vector * 0)");
	GetArrayType(y, 1, &y);
	test(! typep_vector_dimension(x, y, &check), "typep_vector_dimension5");
	test(! check, "typep_vector_dimension6");

	array_va_heap(&x, 10, 10, 0);
	test_parse_char(&y, "(vector * 2)");
	GetArrayType(y, 1, &y);
	test(! typep_vector_dimension(x, y, &check), "typep_vector_dimension7");
	test(! check, "typep_vector_dimension8");

	array_va_heap(&x, 10, 10, 0);
	test_parse_char(&y, "(vector * *)");
	GetArrayType(y, 1, &y);
	test(! typep_vector_dimension(x, y, &check), "typep_vector_dimension9");
	test(! check, "typep_vector_dimension10");

	RETURN;
}

static int test_typep_vector_array(void)
{
	int check;
	addr x, y;

	array_va_heap(&x, 10, 0);
	test_parse_char(&y, "vector");
	test(! typep_vector_array(x, y, &check), "typep_vector_array1");
	test(check, "typep_vector_array2");

	array_va_heap(&x, 10, 20, 0);
	test_parse_char(&y, "vector");
	test(! typep_vector_array(x, y, &check), "typep_vector_array3");
	test(! check, "typep_vector_array4");

	array_va_heap(&x, 10, 0);
	test_parse_char(&y, "(vector t 10)");
	test(! typep_vector_array(x, y, &check), "typep_vector_array5");
	test(check, "typep_vector_array6");

	array_va_heap(&x, 10, 0);
	test_parse_char(&y, "(vector character *)");
	test(! typep_vector_array(x, y, &check), "typep_vector_array7");
	test(! check, "typep_vector_array8");

	array_va_heap(&x, 10, 0);
	test_parse_char(&y, "(vector * 9)");
	test(! typep_vector_array(x, y, &check), "typep_vector_array9");
	test(! check, "typep_vector_array10");

	RETURN;
}

static int test_typep_vector(void)
{
	addr v;

	/* vector */
	vector4_heap(&v, 3);
	SetArrayA4(v, 0, fixnum_heapr(10));
	SetArrayA4(v, 1, fixnum_heapr(20));
	SetArrayA4(v, 2, fixnum_heapr(30));
	test(typep_char(v, "(vector t 3)"), "typep_vector1");

	/* string */
	strvect_char_heap(&v, "Hello");
	test(typep_char(v, "(vector character 5)"), "typep_vector2");

	/* array */
	array_va_heap(&v, 10, 0);
	test(typep_char(v, "(vector t 10)"), "typep_vector3");

	/* bit-vector */
	v = readr("#*1100110");
	test(typep_char(v, "(vector bit 7)"), "typep_vector4");

	RETURN;
}

static int test_typep_simple_vector_vector(void)
{
	int check;
	addr x, y;

	vector4_heap(&x, 3);
	SetArrayA4(x, 0, fixnum_heapr(10));
	SetArrayA4(x, 1, fixnum_heapr(20));
	SetArrayA4(x, 2, fixnum_heapr(30));

	test_parse_char(&y, "simple-vector");
	test(! typep_simple_vector_vector(x, y, &check), "typep_simple_vector_vector1");
	test(check, "typep_simple_vector_vector2");

	test_parse_char(&y, "(simple-vector 3)");
	test(! typep_simple_vector_vector(x, y, &check), "typep_simple_vector_vector3");
	test(check, "typep_simple_vector_vector4");

	test_parse_char(&y, "(simple-vector 4)");
	test(! typep_simple_vector_vector(x, y, &check), "typep_simple_vector_vector5");
	test(! check, "typep_simple_vector_vector6");

	RETURN;
}

static int test_typep_type_vector_array(void)
{
	int check;
	addr x, y;

	array_va_heap(&x, 10, 0);
	array_build(x);
	test_parse_char(&y, "simple-vector");
	test(! typep_type_vector_array(x, y, LISPDECL_T, &check),
			"typep_type_vector_array1");
	test(check, "typep_type_vector_array2");

	array_va_heap(&x, 10, 20, 0);
	array_build(x);
	test_parse_char(&y, "simple-vector");
	test(! typep_type_vector_array(x, y, LISPDECL_T, &check),
			"typep_type_vector_array3");
	test(! check, "typep_type_vector_array4");

	array_va_heap(&x, 10, 0);
	array_build(x);
	test_parse_char(&y, "(simple-vector 10)");
	test(! typep_type_vector_array(x, y, LISPDECL_T, &check),
			"typep_type_vector_array5");
	test(check, "typep_type_vector_array6");

	array_va_heap(&x, 10, 0);
	array_build(x);
	test_parse_char(&y, "(simple-vector *)");
	test(! typep_type_vector_array(x, y, LISPDECL_T, &check),
			"typep_type_vector_array7");
	test(check, "typep_type_vector_array8");

	array_va_heap(&x, 10, 0);
	array_build(x);
	test_parse_char(&y, "(simple-vector 9)");
	test(! typep_type_vector_array(x, y, LISPDECL_T, &check),
			"typep_type_vector_array9");
	test(! check, "typep_type_vector_array10");

	strarray_char_heap(&x, "Hello");
	test_parse_char(&y, "(simple-vector *)");
	test(! typep_type_vector_array(x, y, LISPDECL_T, &check),
			"typep_type_vector_array11");
	test(! check, "typep_type_vector_array12");

	test(! typep_type_vector_array(x, y, LISPDECL_CHARACTER, &check),
			"typep_type_vector_array13");
	test(check, "typep_type_vector_array14");

	RETURN;
}

static int test_typep_simple_vector(void)
{
	addr v;

	/* vector */
	vector4_heap(&v, 3);
	SetArrayA4(v, 0, fixnum_heapr(10));
	SetArrayA4(v, 1, fixnum_heapr(20));
	SetArrayA4(v, 2, fixnum_heapr(30));
	test(typep_char(v, "(simple-vector 3)"), "typep_simple_vector1");

	/* array */
	array_va_heap(&v, 10, 0);
	array_build(v);
	test(typep_char(v, "(simple-vector 10)"), "typep_simple_vector2");

	/* not simple */
	array_va_heap(&v, 10, 0);
	ArrayInfoStruct(v)->adjustable = 1;
	array_build(v);
	test(! typep_char(v, "(simple-vector 10)"), "typep_simple_vector3");

	RETURN;
}

static int test_typep_bit_vector(void)
{
	addr v;

	array_va_heap(&v, 10, 0);
	ArrayInfoStruct(v)->type = ARRAY_TYPE_BIT;
	array_build(v);
	test(typep_char(v, "(bit-vector 10)"), "typep_bit_vector1");
	test(typep_char(v, "(bit-vector *)"), "typep_bit_vector2");
	test(! typep_char(v, "(bit-vector 9)"), "typep_bit_vector3");

	strarray_char_heap(&v, "Hello");
	test(! typep_char(v, "(bit-vector *)"), "typep_bit_vector4");

	RETURN;
}

static int test_typep_simple_bit_vector(void)
{
	addr v;

	array_va_heap(&v, 10, 0);
	ArrayInfoStruct(v)->type = ARRAY_TYPE_BIT;
	array_build(v);
	test(typep_char(v, "(simple-bit-vector 10)"), "typep_simple_bit_vector1");
	test(typep_char(v, "(simple-bit-vector *)"), "typep_simple_bit_vector2");
	test(! typep_char(v, "(simple-bit-vector 9)"), "typep_simple_bit_vector3");

	strarray_char_heap(&v, "Hello");
	test(! typep_char(v, "(simple-bit-vector *)"), "typep_simple_bit_vector4");

	array_va_heap(&v, 10, 0);
	ArrayInfoStruct(v)->type = ARRAY_TYPE_BIT;
	ArrayInfoStruct(v)->adjustable = 1; /* not simple */
	array_build(v);
	test(! typep_char(v, "(simple-bit-vector *)"), "typep_simple_bit_vector5");

	RETURN;
}

static int test_typep_extended_char(void)
{
	addr v;

	character_heap(&v, 0x80001000);
	test(typep_char(v, "extended-char"), "typep_extended_char1");

	character_heap(&v, 'A');
	test(! typep_char(v, "extended-char"), "typep_extended_char2");

	fixnum_heap(&v, 'A');
	test(! typep_char(v, "extended-char"), "typep_extended_char3");

	RETURN;
}

static int test_typep_string(void)
{
	addr v;

	strvect_char_heap(&v, "Hello");
	test(typep_char(v, "string"), "typep_string1");
	strarray_char_heap(&v, "Hello");
	test(typep_char(v, "string"), "typep_string2");

	strvect_char_heap(&v, "Hello");
	test(typep_char(v, "(string 5)"), "typep_string3");
	strarray_char_heap(&v, "Hello");
	test(typep_char(v, "(string 5)"), "typep_string4");

	strvect_char_heap(&v, "Hello");
	test(! typep_char(v, "(string 6)"), "typep_string5");
	strarray_char_heap(&v, "Hello");
	test(! typep_char(v, "(string 6)"), "typep_string6");

	fixnum_heap(&v, 10);
	test(! typep_char(v, "string"), "typep_string7");

	strvect_char_heap(&v, "Hello");
	string_setc(v, 3, 0x80001000);
	test(typep_char(v, "string"), "typep_string8");

	RETURN;
}

static int test_typep_base_string(void)
{
	addr v;

	strvect_char_heap(&v, "Hello");
	test(typep_char(v, "base-string"), "typep_base_string1");
	strarray_char_heap(&v, "Hello");
	test(typep_char(v, "base-string"), "typep_base_string2");

	strvect_char_heap(&v, "Hello");
	test(typep_char(v, "(base-string 5)"), "typep_base_string3");
	strarray_char_heap(&v, "Hello");
	test(typep_char(v, "(base-string 5)"), "typep_base_string4");

	strvect_char_heap(&v, "Hello");
	test(! typep_char(v, "(base-string 6)"), "typep_base_string5");
	strarray_char_heap(&v, "Hello");
	test(! typep_char(v, "(base-string 6)"), "typep_base_string6");

	fixnum_heap(&v, 10);
	test(! typep_char(v, "base-string"), "typep_base_string7");

	strvect_char_heap(&v, "Hello");
	string_setc(v, 3, 0x80001000);
	test(! typep_char(v, "base-string"), "typep_base_string8");

	RETURN;
}

static int test_typep_simple_string(void)
{
	addr v;

	strvect_char_heap(&v, "Hello");
	test(typep_char(v, "simple-string"), "typep_simple_string1");
	strarray_char_heap(&v, "Hello");
	test(typep_char(v, "simple-string"), "typep_simple_string2");

	strvect_char_heap(&v, "Hello");
	test(typep_char(v, "(simple-string 5)"), "typep_simple_string3");
	strarray_char_heap(&v, "Hello");
	test(typep_char(v, "(simple-string 5)"), "typep_simple_string4");

	strvect_char_heap(&v, "Hello");
	test(! typep_char(v, "(simple-string 6)"), "typep_simple_string5");
	strarray_char_heap(&v, "Hello");
	test(! typep_char(v, "(simple-string 6)"), "typep_simple_string6");

	fixnum_heap(&v, 10);
	test(! typep_char(v, "simple-string"), "typep_simple_string7");

	strvect_char_heap(&v, "Hello");
	string_setc(v, 3, 0x80001000);
	test(typep_char(v, "simple-string"), "typep_simple_string8");

	strarray_char_heap(&v, "Hello");
	ArrayInfoStruct(v)->adjustable = 1; /* not simple */
	array_build(v);
	test(! typep_char(v, "simple-string"), "typep_simple_string9");

	RETURN;
}

static int test_typep_simple_base_string(void)
{
	addr v;

	strvect_char_heap(&v, "Hello");
	test(typep_char(v, "simple-base-string"), "typep_simple_base_string1");
	strarray_char_heap(&v, "Hello");
	test(typep_char(v, "simple-base-string"), "typep_simple_base_string2");

	strvect_char_heap(&v, "Hello");
	test(typep_char(v, "(simple-base-string 5)"), "typep_simple_base_string3");
	strarray_char_heap(&v, "Hello");
	test(typep_char(v, "(simple-base-string 5)"), "typep_simple_base_string4");

	strvect_char_heap(&v, "Hello");
	test(! typep_char(v, "(simple-base-string 6)"), "typep_simple_base_string5");
	strarray_char_heap(&v, "Hello");
	test(! typep_char(v, "(simple-base-string 6)"), "typep_simple_base_string6");

	fixnum_heap(&v, 10);
	test(! typep_char(v, "simple-base-string"), "typep_simple_base_string7");

	strvect_char_heap(&v, "Hello");
	string_setc(v, 3, 0x80001000);
	test(! typep_char(v, "simple-base-string"), "typep_simple_base_string8");

	strarray_char_heap(&v, "Hello");
	ArrayInfoStruct(v)->adjustable = 1; /* not simple */
	array_build(v);
	test(! typep_char(v, "simple-base-string"), "typep_simple_base_string9");

	RETURN;
}

static int test_typep_signed_byte(void)
{
	addr v;

	fixnum_heap(&v, 10);
	test(typep_char(v, "signed-byte"), "typep_signed_byte1");
	fixnum_heap(&v, -10);
	test(typep_char(v, "signed-byte"), "typep_signed_byte2");

	bignum_value_heap(&v, signplus_bignum, 10);
	test(typep_char(v, "signed-byte"), "typep_signed_byte3");
	bignum_value_heap(&v, signminus_bignum, 10);
	test(typep_char(v, "signed-byte"), "typep_signed_byte4");

	fixnum_heap(&v, -129);
	test(! typep_char(v, "(signed-byte 8)"), "typep_signed_byte5");
	fixnum_heap(&v, -128);
	test(typep_char(v, "(signed-byte 8)"), "typep_signed_byte6");
	fixnum_heap(&v, 127);
	test(typep_char(v, "(signed-byte 8)"), "typep_signed_byte7");
	fixnum_heap(&v, 128);
	test(! typep_char(v, "(signed-byte 8)"), "typep_signed_byte8");

	bignum_value_heap(&v, signminus_bignum, 129);
	test(! typep_char(v, "(signed-byte 8)"), "typep_signed_byte9");
	bignum_value_heap(&v, signminus_bignum, 128);
	test(typep_char(v, "(signed-byte 8)"), "typep_signed_byte10");
	bignum_value_heap(&v, signplus_bignum, 127);
	test(typep_char(v, "(signed-byte 8)"), "typep_signed_byte11");
	bignum_value_heap(&v, signplus_bignum, 128);
	test(! typep_char(v, "(signed-byte 8)"), "typep_signed_byte12");

	character_heap(&v, 'A');
	test(! typep_char(v, "(signed-byte 8)"), "typep_signed_byte13");

	RETURN;
}

static int test_typep_unsigned_byte(void)
{
	addr v;

	fixnum_heap(&v, 10);
	test(typep_char(v, "unsigned-byte"), "typep_unsigned_byte1");
	fixnum_heap(&v, -10);
	test(! typep_char(v, "unsigned-byte"), "typep_unsigned_byte2");

	bignum_value_heap(&v, signplus_bignum, 10);
	test(typep_char(v, "unsigned-byte"), "typep_unsigned_byte3");
	bignum_value_heap(&v, signminus_bignum, 10);
	test(! typep_char(v, "unsigned-byte"), "typep_unsigned_byte4");

	fixnum_heap(&v, -10);
	test(! typep_char(v, "(unsigned-byte 8)"), "typep_unsigned_byte5");
	fixnum_heap(&v, 0);
	test(typep_char(v, "(unsigned-byte 8)"), "typep_unsigned_byte6");
	fixnum_heap(&v, 255);
	test(typep_char(v, "(unsigned-byte 8)"), "typep_unsigned_byte7");
	fixnum_heap(&v, 256);
	test(! typep_char(v, "(unsigned-byte 8)"), "typep_unsigned_byte8");

	bignum_value_heap(&v, signminus_bignum, 10);
	test(! typep_char(v, "(unsigned-byte 8)"), "typep_unsigned_byte9");
	bignum_value_heap(&v, signplus_bignum, 0);
	test(typep_char(v, "(unsigned-byte 8)"), "typep_unsigned_byte10");
	bignum_value_heap(&v, signplus_bignum, 255);
	test(typep_char(v, "(unsigned-byte 8)"), "typep_unsigned_byte11");
	bignum_value_heap(&v, signplus_bignum, 256);
	test(! typep_char(v, "(unsigned-byte 8)"), "typep_unsigned_byte12");

	character_heap(&v, 'A');
	test(! typep_char(v, "(unsigned-byte 8)"), "typep_unsigned_byte13");

	RETURN;
}

static int test_typep_bit(void)
{
	addr v;

	fixnum_heap(&v, -1);
	test(! typep_char(v, "bit"), "typep_bit1");
	fixnum_heap(&v, 0);
	test(typep_char(v, "bit"), "typep_bit2");
	fixnum_heap(&v, 1);
	test(typep_char(v, "bit"), "typep_bit3");

	bignum_value_heap(&v, signminus_bignum, 1);
	test(! typep_char(v, "bit"), "typep_bit4");
	bignum_value_heap(&v, signplus_bignum, 0);
	test(typep_char(v, "bit"), "typep_bit5");
	bignum_value_heap(&v, signplus_bignum, 1);
	test(typep_char(v, "bit"), "typep_bit6");

	character_heap(&v, 0);
	test(! typep_char(v, "bit"), "typep_bit7");

	RETURN;
}

static int test_typep_fixnum(void)
{
	addr v;

	fixnum_heap(&v, 0);
	test(typep_char(v, "fixnum"), "typep_fixnum1");
	fixnum_heap(&v, 30);
	test(typep_char(v, "fixnum"), "typep_fixnum2");
	fixnum_heap(&v, -40);
	test(typep_char(v, "fixnum"), "typep_fixnum3");
	fixnum_heap(&v, FIXNUM_MAX);
	test(typep_char(v, "fixnum"), "typep_fixnum4");
	fixnum_heap(&v, FIXNUM_MIN);
	test(typep_char(v, "fixnum"), "typep_fixnum5");

	bignum_value_heap(&v, signminus_bignum, FIXNUM_UMIN);
	test(typep_char(v, "fixnum"), "typep_fixnum6");
	bignum_value_heap(&v, signplus_bignum, FIXNUM_MAX);
	test(typep_char(v, "fixnum"), "typep_fixnum7");
	bignum_value_heap(&v, signminus_bignum, FIXNUM_UMIN + 1UL);
	test(! typep_char(v, "fixnum"), "typep_fixnum8");
	bignum_value_heap(&v, signplus_bignum, ((bigtype)FIXNUM_MAX) + 1UL);
	test(! typep_char(v, "fixnum"), "typep_fixnum9");

	bignum_value2_heap(&v, signplus_bignum, 1, 1);
	test(! typep_char(v, "fixnum"), "typep_fixnum10");
	character_heap(&v, 'A');
	test(! typep_char(v, "fixnum"), "typep_fixnum11");

	RETURN;
}

static int test_typep_bignum(void)
{
	addr v;

	fixnum_heap(&v, 0);
	test(! typep_char(v, "bignum"), "typep_bignum1");
	fixnum_heap(&v, 30);
	test(! typep_char(v, "bignum"), "typep_bignum2");
	fixnum_heap(&v, -40);
	test(! typep_char(v, "bignum"), "typep_bignum3");
	fixnum_heap(&v, FIXNUM_MAX);
	test(! typep_char(v, "bignum"), "typep_bignum4");
	fixnum_heap(&v, FIXNUM_MIN);
	test(! typep_char(v, "bignum"), "typep_bignum5");

	bignum_value_heap(&v, signminus_bignum, FIXNUM_UMIN);
	test(! typep_char(v, "bignum"), "typep_bignum6");
	bignum_value_heap(&v, signplus_bignum, FIXNUM_MAX);
	test(! typep_char(v, "bignum"), "typep_bignum7");
	bignum_value_heap(&v, signminus_bignum, FIXNUM_UMIN + 1UL);
	test(typep_char(v, "bignum"), "typep_bignum8");
	bignum_value_heap(&v, signplus_bignum, ((bigtype)FIXNUM_MAX) + 1UL);
	test(typep_char(v, "bignum"), "typep_bignum9");

	bignum_value2_heap(&v, signplus_bignum, 1, 1);
	test(typep_char(v, "bignum"), "typep_bignum10");
	character_heap(&v, 'A');
	test(! typep_char(v, "bignum"), "typep_bignum11");

	RETURN;
}


/*
 *  Atomic-type
 */
static int test_typep_nil(void)
{
	addr v;

	fixnum_heap(&v, 100);
	test(! typep_char(v, "nil"), "typep_nil1");
	test(! typep_char(Nil, "nil"), "typep_nil2");
	test(! typep_char(T, "nil"), "typep_nil3");

	RETURN;
}

static int test_typep_t(void)
{
	addr v;

	fixnum_heap(&v, 100);
	test(typep_char(v, "t"), "typep_t1");
	test(typep_char(Nil, "t"), "typep_t2");
	test(typep_char(T, "t"), "typep_t3");

	RETURN;
}

static int test_typep_null(void)
{
	addr v;

	consnil_heap(&v);
	test(! typep_char(v, "null"), "typep_null1");
	fixnum_heap(&v, 100);
	test(! typep_char(v, "null"), "typep_null2");
	test(typep_char(Nil, "null"), "typep_null3");
	test(! typep_char(T, "null"), "typep_null4");

	RETURN;
}

static int test_typep_cons(void)
{
	addr v;

	/* asterisk */
	consnil_heap(&v);
	test(typep_char(v, "cons"), "typep_cons1");
	fixnum_heap(&v, 100);
	test(! typep_char(v, "cons"), "typep_cons2");
	test(! typep_char(Nil, "cons"), "typep_cons3");
	test(! typep_char(T, "cons"), "typep_cons4");

	/* parameter */
	consnil_heap(&v);
	test(typep_char(v, "(cons *)"), "typep_cons5");
	fixnum_heap(&v, 100);
	test(! typep_char(v, "(cons *)"), "typep_cons6");
	test(! typep_char(Nil, "(cons *)"), "typep_cons7");
	test(! typep_char(T, "(cons *)"), "typep_cons8");

	consnil_heap(&v);
	test(typep_char(v, "(cons * *)"), "typep_cons9");
	fixnum_heap(&v, 100);
	test(! typep_char(v, "(cons * *)"), "typep_cons10");
	test(! typep_char(Nil, "(cons * *)"), "typep_cons11");
	test(! typep_char(T, "(cons * *)"), "typep_cons12");

	consnil_heap(&v);
	test(! typep_char(v, "(cons integer)"), "typep_cons13");
	fixnum_heap(&v, 100);
	test(! typep_char(v, "(cons integer)"), "typep_cons14");
	test(! typep_char(Nil, "(cons integer)"), "typep_cons15");
	test(! typep_char(T, "(cons integer)"), "typep_cons16");

	v = readr("(10)");
	test(typep_char(v, "(cons integer)"), "typep_cons17");

	v = readr("(10 . \"Hello\")");
	test(typep_char(v, "(cons integer)"), "typep_cons18");
	v = readr("(\"Hello\")");
	test(! typep_char(v, "(cons integer)"), "typep_cons19");
	v = readr("(\"Hello\" . 10)");
	test(! typep_char(v, "(cons integer)"), "typep_cons20");

	v = readr("(200)");
	test(typep_char(v, "(cons integer *)"), "typep_cons21");
	v = readr("(200)");
	test(! typep_char(v, "(cons * integer)"), "typep_cons22");
	v = readr("(nil . 200)");
	test(typep_char(v, "(cons * integer)"), "typep_cons23");
	v = readr("(200 . 200)");
	test(typep_char(v, "(cons * integer)"), "typep_cons24");

	v = readr("(200)");
	test(! typep_char(v, "(cons string integer)"), "typep_cons25");
	v = readr("(nil . 200)");
	test(! typep_char(v, "(cons string integer)"), "typep_cons26");
	v = readr("(200 . 200)");
	test(! typep_char(v, "(cons string integer)"), "typep_cons27");
	v = readr("(200 . \"Hello\")");
	test(! typep_char(v, "(cons string integer)"), "typep_cons28");
	v = readr("(\"Hello\" . 200)");
	test(typep_char(v, "(cons string integer)"), "typep_cons29");

	RETURN;
}

static int test_typep_hash_table(void)
{
	addr v;

	hashtable_heap(&v);
	test(typep_char(v, "hash-table"), "typep_hash_table1");
	fixnum_heap(&v, 100);
	test(! typep_char(v, "hash-table"), "typep_hash_table2");
	test(! typep_char(Nil, "hash-table"), "typep_hash_table3");
	test(! typep_char(T, "hash-table"), "typep_hash_table4");

	RETURN;
}

static int test_typep_symbol(void)
{
	addr v;

	v = readr("hello");
	test(typep_char(v, "symbol"), "typep_symbol1");
	v = readr(":aaa");
	test(typep_char(v, "symbol"), "typep_symbol2");
	symbol_heap(&v);
	test(typep_char(v, "symbol"), "typep_symbol3");

	fixnum_heap(&v, 100);
	test(! typep_char(v, "symbol"), "typep_symbol4");
	test(typep_char(Nil, "symbol"), "typep_symbol5");
	test(typep_char(T, "symbol"), "typep_symbol6");

	RETURN;
}

static int test_typep_keyword(void)
{
	addr v;

	v = readr("hello");
	test(! typep_char(v, "keyword"), "typep_keyword1");
	v = readr(":aaa");
	test(typep_char(v, "keyword"), "typep_keyword2");
	symbol_heap(&v);
	test(! typep_char(v, "keyword"), "typep_keyword3");

	fixnum_heap(&v, 100);
	test(! typep_char(v, "keyword"), "typep_keyword4");
	test(! typep_char(Nil, "keyword"), "typep_keyword5");
	test(! typep_char(T, "keyword"), "typep_keyword6");

	RETURN;
}

static int test_typep_package(void)
{
	addr v;

	find_char_package(LISP_COMMON, &v);
	test(typep_char(v, "package"), "typep_package1");
	fixnum_heap(&v, 100);
	test(! typep_char(v, "package"), "typep_package2");
	test(! typep_char(Nil, "package"), "typep_package3");
	test(! typep_char(T, "package"), "typep_package4");

	RETURN;
}

static int test_typep_random_state(void)
{
	addr v;

	make_random_state_heap(Execute_Thread, &v, Nil);
	test(typep_char(v, "random-state"), "typep_random_state1");
	fixnum_heap(&v, 100);
	test(! typep_char(v, "random-state"), "typep_random_state2");
	test(! typep_char(Nil, "random-state"), "typep_random_state3");
	test(! typep_char(T, "random-state"), "typep_random_state4");

	RETURN;
}

static int test_typep_readtable(void)
{
	addr v;

	GetConst(SPECIAL_READTABLE, &v);
	getspecialcheck_local(Execute_Thread, v, &v);
	test(typep_char(v, "readtable"), "typep_readtable1");
	fixnum_heap(&v, 100);
	test(! typep_char(v, "readtable"), "typep_readtable2");
	test(! typep_char(Nil , "readtable"), "typep_readtable3");
	test(! typep_char(T, "readtable"), "typep_readtable4");

	RETURN;
}

static int test_typep_function(void)
{
	addr v;

	GetConst(COMMON_CAR, &v);
	test(! typep_char(v, "function"), "typep_function1");
	getfunctioncheck_local(Execute_Thread, v, &v);
	test(typep_char(v, "function"), "typep_function2");

	RETURN;
}

static int test_typep_compiled(void)
{
	addr v;

	GetConst(COMMON_CAR, &v);
	test(! typep_char(v, "compiled-function"), "typep_compiled1");
	getfunctioncheck_local(Execute_Thread, v, &v);
	test(typep_char(v, "compiled-function"), "typep_compiled2");

	RETURN;
}

static int test_typep_pathname(void)
{
	addr v;

	parse_pathname_char_heap(Execute_Thread, "/usr/local/", &v);
	test(typep_char(v, "pathname"), "typep_pathname1");
	SetLogicalPathname(v, 1);
	test(typep_char(v, "pathname"), "typep_pathname2");
	fixnum_heap(&v, 100);
	test(! typep_char(v, "pathname"), "typep_pathname3");
	test(! typep_char(Nil, "pathname"), "typep_pathname4");
	test(! typep_char(T, "pathname"), "typep_pathname5");

	RETURN;
}

static int test_typep_logical_pathname(void)
{
	addr v;

	parse_pathname_char_heap(Execute_Thread, "/usr/local/", &v);
	test(! typep_char(v, "logical-pathname"), "typep_logical_pathname1");
	SetLogicalPathname(v, 1);
	test(typep_char(v, "logical-pathname"), "typep_logical_pathname2");
	fixnum_heap(&v, 100);
	test(! typep_char(v, "logical-pathname"), "typep_logical_pathname3");
	test(! typep_char(Nil, "logical-pathname"), "typep_logical_pathname4");
	test(! typep_char(T, "logical-pathname"), "typep_logical_pathname5");

	RETURN;
}

static int test_typep_sequence(void)
{
	addr v;

	test(typep_char(Nil, "sequence"), "typep_sequence1");
	consnil_heap(&v);
	test(typep_char(v, "sequence"), "typep_sequence2");
	vector4_heap(&v, 10);
	test(typep_char(v, "sequence"), "typep_sequence3");

	fixnum_heap(&v, 100);
	test(! typep_char(v, "sequence"), "typep_sequence3");
	test(! typep_char(T, "sequence"), "typep_sequence5");

	array_va_heap(&v, 4, 0);
	test(typep_char(v, "sequence"), "typep_sequence6");
	array_va_heap(&v, 4, 5, 0);
	test(! typep_char(v, "sequence"), "typep_sequence7");

	RETURN;
}

static int test_equal_array_dimension(void)
{
	addr value, type;

	array_va_heap(&value, 3, 0);
	test_parse_char(&type, "(array * (3))");
	GetArrayType(type, 1, &type);
	test(equal_array_dimension(value, type), "equal_array_dimension1");

	array_va_heap(&value, 3, 4, 0);
	test_parse_char(&type, "(array * (3))");
	GetArrayType(type, 1, &type);
	test(! equal_array_dimension(value, type), "equal_array_dimension2");

	array_va_heap(&value, 3, 0);
	test_parse_char(&type, "(array * (3 *))");
	GetArrayType(type, 1, &type);
	test(! equal_array_dimension(value, type), "equal_array_dimension3");

	array_va_heap(&value, 3, 4, 0);
	test_parse_char(&type, "(array * (3 *))");
	GetArrayType(type, 1, &type);
	test(equal_array_dimension(value, type), "equal_array_dimension4");

	array_va_heap(&value, 3, 4, 0);
	test_parse_char(&type, "(array * (3 5))");
	GetArrayType(type, 1, &type);
	test(! equal_array_dimension(value, type), "equal_array_dimension5");

	array_va_heap(&value, 3, 4, 5, 0);
	test_parse_char(&type, "(array * (3 4 5))");
	GetArrayType(type, 1, &type);
	test(equal_array_dimension(value, type), "equal_array_dimension6");

	array_va_heap(&value, 0);
	test_parse_char(&type, "(array * (3))");
	GetArrayType(type, 1, &type);
	test(! equal_array_dimension(value, type), "equal_array_dimension7");

	RETURN;
}

static int test_typep_array_dimension(void)
{
	addr value, type;

	array_va_heap(&value, 3, 0);
	test_parse_char(&type, "(array * (3))");
	GetArrayType(type, 1, &type);
	test(typep_array_dimension(value, type), "typep_array_dimension1");

	array_va_heap(&value, 3, 4, 5, 0);
	test_parse_char(&type, "(array * (3 * 5))");
	GetArrayType(type, 1, &type);
	test(typep_array_dimension(value, type), "typep_array_dimension2");

	array_va_heap(&value, 3, 4, 5, 0);
	test_parse_char(&type, "(array * 3)");
	GetArrayType(type, 1, &type);
	test(typep_array_dimension(value, type), "typep_array_dimension3");

	array_va_heap(&value, 3, 4, 5, 0);
	test_parse_char(&type, "(array * 4)");
	GetArrayType(type, 1, &type);
	test(! typep_array_dimension(value, type), "typep_array_dimension4");

	array_va_heap(&value, 3, 4, 5, 0);
	test_parse_char(&type, "(array * *)");
	GetArrayType(type, 1, &type);
	test(typep_array_dimension(value, type), "typep_array_dimension5");

	array_va_heap(&value, 0);
	test_parse_char(&type, "(array * ())");
	GetArrayType(type, 1, &type);
	test(typep_array_dimension(value, type), "typep_array_dimension6");

	RETURN;
}

static int test_typep_array_array(void)
{
	addr value, type;

	array_va_heap(&value, 3, 0);
	test_parse_char(&type, "(array * (3))");
	test(typep_array_array(value, type), "typep_array_array1");

	array_va_heap(&value, 3, 0);
	test_parse_char(&type, "(array t (3))");
	test(typep_array_array(value, type), "typep_array_array2");

	array_va_heap(&value, 3, 0);
	test_parse_char(&type, "(array character (3))");
	test(! typep_array_array(value, type), "typep_array_array3");

	array_va_heap(&value, 3, 4, 5, 0);
	test_parse_char(&type, "(array t (3 4 5))");
	test(typep_array_array(value, type), "typep_array_array4");

	array_va_heap(&value, 3, 4, 5, 0);
	test_parse_char(&type, "(array t *)");
	test(typep_array_array(value, type), "typep_array_array5");

	array_va_heap(&value, 3, 4, 5, 0);
	test_parse_char(&type, "array");
	test(typep_array_array(value, type), "typep_array_array6");

	RETURN;
}

static int test_typep_array_vector(void)
{
	addr value, type;

	vector_heap(&value, 10);
	test_parse_char(&type, "(array * *)");
	test(typep_array_vector(value, type), "typep_array_vector1");

	test_parse_char(&type, "(array * (10))");
	test(typep_array_vector(value, type), "typep_array_vector2");

	test_parse_char(&type, "(array * 1)");
	test(typep_array_vector(value, type), "typep_array_vector3");

	test_parse_char(&type, "(array * 3)");
	test(! typep_array_vector(value, type), "typep_array_vector4");

	test_parse_char(&type, "(array * (10 *))");
	test(! typep_array_vector(value, type), "typep_array_vector5");

	test_parse_char(&type, "(array * ())");
	test(! typep_array_vector(value, type), "typep_array_vector6");

	test_parse_char(&type, "(array t (10))");
	test(typep_array_vector(value, type), "typep_array_vector7");

	test_parse_char(&type, "(array character (10))");
	test(! typep_array_vector(value, type), "typep_array_vector8");

	RETURN;
}

static int test_typep_array_string(void)
{
	addr value, type;

	strvect_char_heap(&value, "HelloHello");
	test_parse_char(&type, "(array * *)");
	test(typep_array_string(value, type), "typep_array_string1");

	test_parse_char(&type, "(array * (10))");
	test(typep_array_string(value, type), "typep_array_string2");

	test_parse_char(&type, "(array * 1)");
	test(typep_array_string(value, type), "typep_array_string3");

	test_parse_char(&type, "(array * 3)");
	test(! typep_array_string(value, type), "typep_array_string4");

	test_parse_char(&type, "(array * (10 *))");
	test(! typep_array_string(value, type), "typep_array_string5");

	test_parse_char(&type, "(array * ())");
	test(! typep_array_string(value, type), "typep_array_string6");

	test_parse_char(&type, "(array character (10))");
	test(typep_array_string(value, type), "typep_array_string7");

	test_parse_char(&type, "(array t (10))");
	test(! typep_array_string(value, type), "typep_array_string8");

	RETURN;
}

static int test_typep_array_bitvector(void)
{
	addr value, type;

	value = readr("#*1101100100");
	test_parse_char(&type, "(array * *)");
	test(typep_array_bitvector(value, type), "typep_array_bitvector1");

	test_parse_char(&type, "(array * (10))");
	test(typep_array_bitvector(value, type), "typep_array_bitvector2");

	test_parse_char(&type, "(array * 1)");
	test(typep_array_bitvector(value, type), "typep_array_bitvector3");

	test_parse_char(&type, "(array * 3)");
	test(! typep_array_bitvector(value, type), "typep_array_bitvector4");

	test_parse_char(&type, "(array * (10 *))");
	test(! typep_array_bitvector(value, type), "typep_array_bitvector5");

	test_parse_char(&type, "(array * ())");
	test(! typep_array_bitvector(value, type), "typep_array_bitvector6");

	test_parse_char(&type, "(array bit (10))");
	test(typep_array_bitvector(value, type), "typep_array_bitvector7");

	test_parse_char(&type, "(array t (10))");
	test(! typep_array_bitvector(value, type), "typep_array_bitvector8");

	RETURN;
}

static int test_typep_array(void)
{
	addr v;

	array_va_heap(&v, 3, 4, 5, 0);
	test(typep_char(v, "(array t 3)"), "typep_array1");

	vector_heap(&v, 10);
	test(typep_char(v, "(array t (10))"), "typep_array2");

	strvect_char_heap(&v, "HelloHello");
	test(typep_char(v, "(array character *)"), "typep_array3");

	character_heap(&v, 'A');
	test(! typep_char(v, "array"), "typep_array4");

	RETURN;
}

static int test_typep_simple_array(void)
{
	addr v;

	array_va_heap(&v, 3, 4, 5, 0);
	array_build(v); /* simple */
	test(typep_char(v, "(simple-array t 3)"), "typep_simple_array1");

	vector_heap(&v, 10);
	test(typep_char(v, "(simple-array t (10))"), "typep_simple_array2");

	strvect_char_heap(&v, "HelloHello");
	test(typep_char(v, "(simple-array character *)"), "typep_simple_array3");

	character_heap(&v, 'A');
	test(! typep_char(v, "simple-array"), "typep_simple_array4");

	array_va_heap(&v, 5, 0);
	ArrayInfoStruct(v)->adjustable = 1; /* not simple */
	array_build(v);
	test(! typep_char(v, "simple-array"), "typep_simple_array5");

	RETURN;
}

static int test_typep_character(void)
{
	addr v;

	character_heap(&v, 'A');
	test(typep_char(v, "character"), "typep_character1");

	character_heap(&v, 0x0D);
	test(typep_char(v, "character"), "typep_character2");

	character_heap(&v, 0x80001000);
	test(typep_char(v, "character"), "typep_character3");

	fixnum_heap(&v, 'A');
	test(! typep_char(v, "character"), "typep_character4");

	RETURN;
}

static int test_typep_base_char(void)
{
	addr v;

	character_heap(&v, 'A');
	test(typep_char(v, "base-char"), "typep_base_char1");

	character_heap(&v, 0x0D);
	test(typep_char(v, "base-char"), "typep_base_char2");

	character_heap(&v, 0x80001000);
	test(! typep_char(v, "base-char"), "typep_base_char3");

	fixnum_heap(&v, 'A');
	test(! typep_char(v, "base-char"), "typep_base_char4");

	RETURN;
}

static int test_typep_standard_char(void)
{
	addr v;

	character_heap(&v, 'A');
	test(typep_char(v, "standard-char"), "typep_standard_char1");

	character_heap(&v, 0x0D);
	test(! typep_char(v, "standard-char"), "typep_standard_char2");

	character_heap(&v, 0x80001000);
	test(! typep_char(v, "standard-char"), "typep_standard_char3");

	fixnum_heap(&v, 'A');
	test(! typep_char(v, "standard-char"), "typep_standard_char4");

	RETURN;
}

static int test_typep_number(void)
{
	addr v;

	fixnum_heap(&v, 10);
	test(typep_char(v, "number"), "typep_number1");

	single_float_heap(&v, 10.0f);
	test(typep_char(v, "number"), "typep_number2");

	consnil_heap(&v);
	test(! typep_char(v, "number"), "typep_number3");

	RETURN;
}

static int test_typep_ratio(void)
{
	addr v;

	v = readr("12/7");
	test(typep_char(v, "ratio"), "typep_ratio1");

	fixnum_heap(&v, 10);
	test(! typep_char(v, "ratio"), "typep_ratio2");

	v = readr("hello");
	test(! typep_char(v, "ratio"), "typep_ratio3");

	RETURN;
}

static int test_typep_complex(void)
{
	addr real, imag, v;

	double_float_heap(&real, 10.0);
	double_float_heap(&imag, 20.0);
	complex_heap(&v, real, imag);
	test(typep_char(v, "complex"), "typep_complex1");

	test(typep_char(v, "(complex double-float)"), "typep_complex2");

	test(! typep_char(v, "(complex integer)"), "typep_complex3");

	fixnum_heap(&v, 10);
	test(! typep_char(v, "complex"), "typep_complex4");

	RETURN;
}

static int test_typep_restart(void)
{
	addr v;

	restart_heap(&v, Nil);
	test(typep_char(v, "restart"), "typep_retart1");
	test(! typep_char(Nil, "restart"), "typep_retart2");

	RETURN;
}

static int test_typep_stream(void)
{
	addr v, x;

	/* stream */
	stream_heap(&v, StreamType_BincharIO, 0);
	test(typep_char(v, "stream"), "typep_stream1");
	test(! typep_char(Nil, "stream"), "typep_stream2");

	/* broadcast */
	open_broadcast_stream(&v, Nil);
	test(typep_char(v, "broadcast-stream"), "typep_broadcast_stream1");
	test(typep_char(v, "stream"), "typep_broadcast_stream2");
	test(! typep_char(Nil, "broadcast-stream"), "typep_broadcast_stream3");

	/* concatenated */
	open_concatenated_stream(&v, Nil);
	test(typep_char(v, "concatenated-stream"), "typep_concatenated_stream1");
	test(typep_char(v, "stream"), "typep_concatenated_stream2");
	test(! typep_char(Nil, "concatenated-stream"), "typep_concatenated_stream3");

	/* echo */
	open_concatenated_stream(&x, Nil);
	open_echo_stream(&v, x, x);
	test(typep_char(v, "echo-stream"), "typep_echo_stream1");
	test(typep_char(v, "stream"), "typep_echo_stream2");
	test(! typep_char(Nil, "echo-stream"), "typep_echo_stream3");

	/* file */
	GetConst(SYSTEM_STANDARD_INPUT, &v);
	getspecialcheck_local(Execute_Thread, v, &v);
	test(typep_char(v, "file-stream"), "typep_file_stream1");
	test(typep_char(v, "stream"), "typep_file_stream2");
	test(! typep_char(Nil, "file-stream"), "typep_file_stream3");

	/* string */
	strvect_char_heap(&v, "Hello");
	open_input_string_stream(&v, v);
	test(typep_char(v, "string-stream"), "typep_string_stream1");
	test(typep_char(v, "stream"), "typep_string_stream2");
	test(! typep_char(Nil, "string-stream"), "typep_string_stream3");

	/* synonym */
	GetConst(SYSTEM_STANDARD_INPUT, &v);
	open_synonym_stream(&v, v);
	test(typep_char(v, "synonym-stream"), "typep_synonym_stream1");
	test(typep_char(v, "stream"), "typep_synonym_stream2");
	test(! typep_char(Nil, "synonym-stream"), "typep_synonym_stream3");

	/* two-way */
	GetConst(SYSTEM_STANDARD_INPUT, &v);
	getspecialcheck_local(Execute_Thread, v, &v);
	GetConst(SYSTEM_STANDARD_OUTPUT, &x);
	getspecialcheck_local(Execute_Thread, x, &x);
	open_twoway_stream(&v, v, x);
	test(typep_char(v, "two-way-stream"), "typep_two_way_stream1");
	test(typep_char(v, "stream"), "typep_two_way_stream2");
	test(! typep_char(Nil, "two-way-stream"), "typep_two_way_stream3");

	RETURN;
}


/*
 *  range
 */
static int test_less_integer(addr mode, fixnum left, fixnum right)
{
	return less_mode_nolocal(mode,
			fixnum_heapr(left),
			fixnum_heapr(right),
			less_integer_clang,
			less_equal_integer_clang);
}

static int test_less_mode_nolocal(void)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	test(test_less_integer(Nil, 10, 20), "less_mode_nolocal1");
	test(test_less_integer(Nil, 10, 10), "less_mode_nolocal2");
	test(! test_less_integer(Nil, 11, 10), "less_mode_nolocal3");
	test(test_less_integer(T, 10, 20), "less_mode_nolocal4");
	test(! test_less_integer(T, 10, 10), "less_mode_nolocal5");
	test(! test_less_integer(T, 11, 10), "less_mode_nolocal6");
	test(test_less_integer(aster, 10, 5), "less_mode_nolocal7");

	RETURN;
}

static int test_typep_range_nolocal(void)
{
	addr value, type;

	fixnum_heap(&value, 10);
	test_parse_char(&type, "(integer * *)");
	test(typep_object(value, type), "typep_range_nolocal1");

	test_parse_char(&type, "(integer 9 *)");
	test(typep_object(value, type), "typep_range_nolocal2");

	test_parse_char(&type, "(integer 10 *)");
	test(typep_object(value, type), "typep_range_nolocal3");

	test_parse_char(&type, "(integer 11 *)");
	test(! typep_object(value, type), "typep_range_nolocal4");

	test_parse_char(&type, "(integer (9) *)");
	test(typep_object(value, type), "typep_range_nolocal5");

	test_parse_char(&type, "(integer (10) *)");
	test(! typep_object(value, type), "typep_range_nolocal6");

	test_parse_char(&type, "(integer (11) *)");
	test(! typep_object(value, type), "typep_range_nolocal7");

	test_parse_char(&type, "(integer * 9)");
	test(! typep_object(value, type), "typep_range_nolocal8");

	test_parse_char(&type, "(integer * 10)");
	test(typep_object(value, type), "typep_range_nolocal9");

	test_parse_char(&type, "(integer * 11)");
	test(typep_object(value, type), "typep_range_nolocal10");

	test_parse_char(&type, "(integer * (9))");
	test(! typep_object(value, type), "typep_range_nolocal11");

	test_parse_char(&type, "(integer * (10))");
	test(! typep_object(value, type), "typep_range_nolocal12");

	test_parse_char(&type, "(integer * (11))");
	test(typep_object(value, type), "typep_range_nolocal13");

	test_parse_char(&type, "(integer 5 15)");
	test(typep_object(value, type), "typep_range_nolocal14");

	test_parse_char(&type, "(integer 10 10)");
	test(typep_object(value, type), "typep_range_nolocal15");

	test_parse_char(&type, "(integer 20 30)");
	test(! typep_object(value, type), "typep_range_nolocal16");

	test_parse_char(&type, "(integer 20 100)");
	test(! typep_object(value, type), "typep_range_nolocal17");

	RETURN;
}

static int test_less_real(addr mode, fixnum left, fixnum right)
{
	return less_mode_local(Local_Thread, mode,
			fixnum_heapr(left),
			fixnum_heapr(right),
			less_real_clang,
			less_equal_real_clang);
}

static int test_less_mode_local(void)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	test(test_less_real(Nil, 10, 20), "less_mode_local1");
	test(test_less_real(Nil, 10, 10), "less_mode_local2");
	test(! test_less_real(Nil, 11, 10), "less_mode_local3");
	test(test_less_real(T, 10, 20), "less_mode_local4");
	test(! test_less_real(T, 10, 10), "less_mode_local5");
	test(! test_less_real(T, 11, 10), "less_mode_local6");
	test(test_less_real(aster, 10, 5), "less_mode_local7");

	RETURN;
}

static int test_typep_range_local(void)
{
	addr value, type;

	fixnum_heap(&value, 10);
	test_parse_char(&type, "(real * *)");
	test(typep_object(value, type), "typep_range_local1");

	test_parse_char(&type, "(real 9 *)");
	test(typep_object(value, type), "typep_range_local2");

	test_parse_char(&type, "(real 10 *)");
	test(typep_object(value, type), "typep_range_local3");

	test_parse_char(&type, "(real 11 *)");
	test(! typep_object(value, type), "typep_range_local4");

	test_parse_char(&type, "(real (9) *)");
	test(typep_object(value, type), "typep_range_local5");

	test_parse_char(&type, "(real (10) *)");
	test(! typep_object(value, type), "typep_range_local6");

	test_parse_char(&type, "(real (11) *)");
	test(! typep_object(value, type), "typep_range_local7");

	test_parse_char(&type, "(real * 9)");
	test(! typep_object(value, type), "typep_range_local8");

	test_parse_char(&type, "(real * 10)");
	test(typep_object(value, type), "typep_range_local9");

	test_parse_char(&type, "(real * 11)");
	test(typep_object(value, type), "typep_range_local10");

	test_parse_char(&type, "(real * (9))");
	test(! typep_object(value, type), "typep_range_local11");

	test_parse_char(&type, "(real * (10))");
	test(! typep_object(value, type), "typep_range_local12");

	test_parse_char(&type, "(real * (11))");
	test(typep_object(value, type), "typep_range_local13");

	test_parse_char(&type, "(real 5 15)");
	test(typep_object(value, type), "typep_range_local14");

	test_parse_char(&type, "(real 10 10)");
	test(typep_object(value, type), "typep_range_local15");

	test_parse_char(&type, "(real 20 30)");
	test(! typep_object(value, type), "typep_range_local16");

	test_parse_char(&type, "(real 20 100)");
	test(! typep_object(value, type), "typep_range_local17");

	RETURN;
}

static int test_typep_integer(void)
{
	addr value, type;

	fixnum_heap(&value, 20);
	test_parse_char(&type, "(integer 10 30)");
	test(typep_object(value, type), "typep_integer1");

	RETURN;
}

static int test_typep_rational(void)
{
	addr value, type;

	fixnum_heap(&value, 20);
	test_parse_char(&type, "(rational 10 30)");
	test(typep_object(value, type), "typep_rational1");

	RETURN;
}

static int test_typep_real(void)
{
	addr value, type;

	fixnum_heap(&value, 20);
	test_parse_char(&type, "(real 10 30)");
	test(typep_object(value, type), "typep_real1");

	RETURN;
}

static int test_typep_float(void)
{
	addr value, type;

	single_float_heap(&value, 20.0f);
	test_parse_char(&type, "(float 10.0 30.0)");
	test(typep_object(value, type), "typep_float1");

	RETURN;
}

static int test_typep_single_float(void)
{
	addr value, type;

	single_float_heap(&value, 20.0f);
	test_parse_char(&type, "(single-float 10.0e0 30.0e0)");
	test(typep_object(value, type), "typep_single_float1");

	RETURN;
}

static int test_typep_double_float(void)
{
	addr value, type;

	double_float_heap(&value, 20.0);
	test_parse_char(&type, "(double-float 10.0d0 30.0d0)");
	test(typep_object(value, type), "typep_double_float1");

	RETURN;
}

static int test_typep_long_float(void)
{
	addr value, type;

	long_float_heap(&value, 20.0L);
	test_parse_char(&type, "(long-float 10.0l0 30.0l0)");
	test(typep_object(value, type), "typep_long_float1");

	RETURN;
}


/*
 *  typep
 */
static int test_typep_table(void)
{
	int check;
	addr x, y;

	fixnum_heap(&x, 20);
	test_parse_char(&y, "fixnum");
	test(! typep_table(Execute_Thread, x, y, &check), "typep_table1");
	test(check, "typep_table2");

	RETURN;
}

static int TypepCall(addr value, addr type, int aster, int *ret)
{
	return typep_call(Execute_Thread, value, type, aster, ret);
}

static int test_typep_call(void)
{
	int check;
	addr value, type;

	fixnum_heap(&value, 20);
	test_parse_char(&type, "fixnum");
	test(! TypepCall(value, type, 0, &check), "typep_call1");
	test(check, "typep_call2");

	test_parse_char(&type, "fixnum");
	test(! TypepCall(value, type, 1, &check), "typep_call3");
	test(check, "typep_call4");

	test_parse_char(&type, "*");
	test(! TypepCall(value, type, 1, &check), "typep_call5");
	test(check, "typep_call6");

	test_parse_char(&type, "cons");
	test(! TypepCall(value, type, 0, &check), "typep_call7");
	test(! check, "typep_call8");

	test_parse_char(&type, "fixnum");
	type_copy_heap(&type, type);
	SetNotDecl(type, 1);
	test(! TypepCall(value, type, 0, &check), "typep_call9");
	test(! check, "typep_call10");

	test_parse_char(&type, "fixnum");
	type_copy_heap(&type, type);
	SetNotDecl(type, 1);
	test(! TypepCall(value, type, 1, &check), "typep_call11");
	test(! check, "typep_call12");

	test_parse_char(&type, "*");
	type_copy_heap(&type, type);
	SetNotDecl(type, 1);
	test(! TypepCall(value, type, 1, &check), "typep_call13");
	test(! check, "typep_call14");

	test_parse_char(&type, "cons");
	type_copy_heap(&type, type);
	SetNotDecl(type, 1);
	test(! TypepCall(value, type, 0, &check), "typep_call15");
	test(check, "typep_call16");

	RETURN;
}

static int test_typep_clang(void)
{
	int check;
	addr value, type;

	fixnum_heap(&value, 20);
	test_parse_char(&type, "fixnum");
	check = 0;
	test(! TypepClang(value, type, &check), "typep_clang1");
	test(check, "typep_clang2");

	RETURN;
}

static int test_typep_asterisk_clang(void)
{
	int check;
	addr value, type;

	fixnum_heap(&value, 20);
	test_parse_char(&type, "fixnum");
	check = 0;
	test(! TypepAsteriskClang(value, type, &check), "typep_asterisk_heap1");
	test(check, "typep_asterisk_heap2");

	test_parse_char(&type, "*");
	test(! TypepAsteriskClang(value, type, &check), "typep_asterisk_heap3");
	test(check, "typep_asterisk_heap4");

	RETURN;
}


/*
 *  main
 */
static int testbreak_type_typep(void)
{
	TestBreak(test_typep_clos);
	TestBreak(test_typep_asterisk);

	/* Compound-type */
	TestBreak(test_typep_and);
	TestBreak(test_typep_or);
	TestBreak(test_typep_eql);
	TestBreak(test_typep_member);
	TestBreak(test_typep_mod);
	TestBreak(test_typep_not);
	TestBreak(test_typep_satisfies);

	/* Extract-type */
	TestBreak(test_typep_atom);
	TestBreak(test_typep_list);
	TestBreak(test_typep_boolean);
	TestBreak(test_typep_vector_vector);
	TestBreak(test_typep_vector_string);
	TestBreak(test_typep_vector_bitvector);
	TestBreak(test_typep_vector_dimension);
	TestBreak(test_typep_vector_array);
	TestBreak(test_typep_vector);
	TestBreak(test_typep_simple_vector_vector);
	TestBreak(test_typep_type_vector_array);
	TestBreak(test_typep_simple_vector);
	TestBreak(test_typep_bit_vector);
	TestBreak(test_typep_simple_bit_vector);
	TestBreak(test_typep_extended_char);
	TestBreak(test_typep_string);
	TestBreak(test_typep_base_string);
	TestBreak(test_typep_simple_string);
	TestBreak(test_typep_simple_base_string);
	TestBreak(test_typep_signed_byte);
	TestBreak(test_typep_unsigned_byte);
	TestBreak(test_typep_bit);
	TestBreak(test_typep_fixnum);
	TestBreak(test_typep_bignum);

	/* Atomic-type */
	TestBreak(test_typep_nil);
	TestBreak(test_typep_t);
	TestBreak(test_typep_null);
	TestBreak(test_typep_cons);
	TestBreak(test_typep_hash_table);
	TestBreak(test_typep_symbol);
	TestBreak(test_typep_keyword);
	TestBreak(test_typep_package);
	TestBreak(test_typep_random_state);
	TestBreak(test_typep_readtable);
	TestBreak(test_typep_function);
	TestBreak(test_typep_compiled);
	TestBreak(test_typep_pathname);
	TestBreak(test_typep_logical_pathname);
	TestBreak(test_typep_sequence);
	TestBreak(test_equal_array_dimension);
	TestBreak(test_typep_array_dimension);
	TestBreak(test_typep_array_array);
	TestBreak(test_typep_array_vector);
	TestBreak(test_typep_array_string);
	TestBreak(test_typep_array_bitvector);
	TestBreak(test_typep_array);
	TestBreak(test_typep_simple_array);
	TestBreak(test_typep_character);
	TestBreak(test_typep_base_char);
	TestBreak(test_typep_standard_char);
	TestBreak(test_typep_number);
	TestBreak(test_typep_ratio);
	TestBreak(test_typep_complex);
	TestBreak(test_typep_restart);
	TestBreak(test_typep_stream);

	/* range */
	TestBreak(test_less_mode_nolocal);
	TestBreak(test_typep_range_nolocal);
	TestBreak(test_less_mode_local);
	TestBreak(test_typep_range_local);
	TestBreak(test_typep_integer);
	TestBreak(test_typep_rational);
	TestBreak(test_typep_real);
	TestBreak(test_typep_float);
	TestBreak(test_typep_single_float);
	TestBreak(test_typep_double_float);
	TestBreak(test_typep_long_float);

	/* typep */
	TestBreak(test_typep_table);
	TestBreak(test_typep_call);
	TestBreak(test_typep_clang);
	TestBreak(test_typep_asterisk_clang);

	return 0;
}

int test_type_typep(void)
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
		build_eval_declare();
		lisp_initialize = 1;
		result = testbreak_type_typep();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

