#include "type_typep.c"
#include "calltype.h"
#include "clos.h"
#include "clos_object.h"
#include "common.h"
#include "condition.h"
#include "degrade.h"
#include "hashtable.h"
#include "package.h"
#include "pathname.h"
#include "random_state.h"
#include "ratio.h"
#include "readtable.h"
#include "sequence.h"
#include "stream.h"
#include "syscall.h"

/*
 *  test tools
 */
static void parse_type_string(addr *ret, const char *code)
{
	readstring(ret, code);
	parse_type_heap(ret, *ret);
}

static int typep_heap(addr value, addr type)
{
	type_throw_heap(&type, type);
	return typep_clang(value, type);
}

static int typep_asterisk_heap(addr value, addr type)
{
	type_throw_heap(&type, type);
	return typep_asterisk_clang(value, type);
}


/*
 *  system type
 */
static int test_typep_clos(void)
{
	addr value, type;

	interncommon("STANDARD", &value);
	value = find_method_combination(value);
	interncommon("METHOD-COMBINATION", &type);
	test(typep_heap(value, type), "typep_clos1");

	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_clos2");

	RETURN;
}

static int test_typep_asterisk(void)
{
	addr type, value;

	fixnum_heap(&value, 100);
	interncommon("*", &type);
	test(typep_asterisk_heap(value, type), "typep_asterisk1");

	RETURN;
}

static int call_typep_table(addr value, addr type)
{
	type_throw_heap(&type, type);
	return typep_table(value, type);
}


/*
 *  Compound-type
 */
static int test_typep_and(void)
{
	addr value, type;

	fixnum_heap(&value, 10);
	parse_type_string(&type, "(and integer real rational)");
	test(call_typep_table(value, type), "typep_and1");
	parse_type_string(&type, "(and string real rational)");
	test(! call_typep_table(value, type), "typep_and2");
	parse_type_string(&type, "(and integer real cons)");
	test(! call_typep_table(value, type), "typep_and3");
	parse_type_string(&type, "(and)");
	test(call_typep_table(value, type), "typep_and4");

	RETURN;
}

static int test_typep_or(void)
{
	addr value, type;

	fixnum_heap(&value, 10);
	parse_type_string(&type, "(or integer real)");
	test(call_typep_table(value, type), "typep_or1");
	parse_type_string(&type, "(or integer string real)");
	test(call_typep_table(value, type), "typep_or2");
	parse_type_string(&type, "(or cons symbol string)");
	test(! call_typep_table(value, type), "typep_or3");
	parse_type_string(&type, "(or)");
	test(! call_typep_table(value, type), "typep_or4");

	RETURN;
}

static int test_typep_eql(void)
{
	addr value, type;

	fixnum_heap(&value, 10);
	parse_type_string(&type, "(eql 10)");
	test(call_typep_table(value, type), "typep_eql1");

	single_float_heap(&value, 10.0f);
	parse_type_string(&type, "(eql 10)");
	test(! call_typep_table(value, type), "typep_eql2");

	interncommon("EQL", &type);
	character_heap(&value, 'A');
	list_heap(&type, type, value, NULL);
	parse_type_heap(&type, type);
	character_heap(&value, 'A');
	test(call_typep_table(value, type), "typep_eql3");

	fixnum_heap(&value, 10);
	conscar_heap(&value, value);
	parse_type_string(&type, "(eql (10))");
	test(! call_typep_table(value, type), "typep_eql4");

	RETURN;
}

static int test_typep_member(void)
{
	addr value, type;

	fixnum_heap(&value, 10);
	list_heap(&type, interncommonr("MEMBER"), fixnum_heapr(10),
			fixnum_heapr(20), character_heapr('Z'), fixnum_heapr(30), NULL);
	test(call_typep_table(value, type), "typep_member1");

	character_heap(&value, 'Z');
	test(call_typep_table(value, type), "typep_member2");

	fixnum_heap(&value, 11);
	test(! call_typep_table(value, type), "typep_member3");

	character_heap(&value, 'a');
	test(! call_typep_table(value, type), "typep_member4");

	RETURN;
}

static int test_typep_mod(void)
{
	addr value, type;

	parse_type_string(&type, "(mod 256)");
	fixnum_heap(&value, 10);
	test(call_typep_table(value, type), "typep_mod1");

	fixnum_heap(&value, 255);
	test(call_typep_table(value, type), "typep_mod2");

	fixnum_heap(&value, 256);
	test(! call_typep_table(value, type), "typep_mod3");

	fixnum_heap(&value, 0);
	test(call_typep_table(value, type), "typep_mod4");

	fixnum_heap(&value, -1);
	test(! call_typep_table(value, type), "typep_mod5");

	internchar_keyword("HELLO", &value);
	test(! call_typep_table(value, type), "typep_mod6");

	RETURN;
}

static int test_typep_not(void)
{
	addr value, type;

	internchar_keyword("HELLO", &value);
	parse_type_string(&type, "(not integer)");
	test(call_typep_table(value, type), "typep_not1");

	fixnum_heap(&value, 10);
	parse_type_string(&type, "(not integer)");
	test(! call_typep_table(value, type), "typep_not2");

	RETURN;
}

static int test_typep_satisfies(void)
{
	addr value, type;

	fixnum_heap(&value, 10);
	parse_type_string(&type, "(satisfies plusp)");
	test(call_typep_table(value, type), "typep_satisfies1");

	fixnum_heap(&value, -10);
	parse_type_string(&type, "(satisfies plusp)");
	test(! call_typep_table(value, type), "typep_satisfies2");

	RETURN;
}


/*
 *  Extract-type
 */
static int test_typep_atom(void)
{
	addr value, type;

	interncommon("ATOM", &type);
	consnil_heap(&value);
	test(! typep_heap(value, type), "typep_atom1");
	fixnum_heap(&value, 100);
	test(typep_heap(value, type), "typep_atom2");
	test(typep_heap(Nil, type), "typep_atom3");
	test(typep_heap(T, type), "typep_atom4");

	RETURN;
}

static int test_typep_list(void)
{
	addr value, type;

	interncommon("LIST", &type);
	consnil_heap(&value);
	test(typep_heap(value, type), "typep_list1");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_list2");
	test(typep_heap(Nil, type), "typep_list3");
	test(! typep_heap(T, type), "typep_list4");

	RETURN;
}

static int test_typep_boolean(void)
{
	addr value, type;

	interncommon("BOOLEAN", &type);
	consnil_heap(&value);
	test(! typep_heap(value, type), "typep_boolean1");
	test(typep_heap(Nil, type), "typep_boolean2");
	test(typep_heap(T, type), "typep_boolean3");

	RETURN;
}

static int test_typep_vector_vector(void)
{
	addr value, type;

	vector4_heap(&value, 3);
	SetArrayA4(value, 0, fixnum_heapr(10));
	SetArrayA4(value, 1, fixnum_heapr(20));
	SetArrayA4(value, 2, fixnum_heapr(30));
	parse_type_string(&type, "vector");
	test(typep_vector_vector(value, type), "typep_vector_vector1");

	parse_type_string(&type, "(vector integer)"); /* integer -> t */
	test(typep_vector_vector(value, type), "typep_vector_vector2");

	parse_type_string(&type, "(vector * 3)");
	test(typep_vector_vector(value, type), "typep_vector_vector3");

	parse_type_string(&type, "(vector * 4)");
	test(! typep_vector_vector(value, type), "typep_vector_vector4");

	vector4_heap(&value, 3);
	SetArrayA4(value, 0, character_heapr('A'));
	SetArrayA4(value, 1, character_heapr('B'));
	SetArrayA4(value, 2, character_heapr('C'));
	parse_type_string(&type, "(vector character)");
	test(! typep_vector_vector(value, type), "typep_vector_vector5");

	RETURN;
}

static int test_character_decl_p(void)
{
	test(character_decl_p(LISPDECL_CHARACTER), "character_decl_p1");
	test(character_decl_p(LISPDECL_EXTENDED_CHAR), "character_decl_p2");
	test(! character_decl_p(LISPDECL_CONS), "character_decl_p3");

	RETURN;
}

static int test_typep_vector_string(void)
{
	addr value, type;

	strvect_char_heap(&value, "Hello");
	parse_type_string(&type, "vector");
	test(typep_vector_string(value, type), "typep_vector_string1");

	parse_type_string(&type, "(vector character)");
	test(typep_vector_string(value, type), "typep_vector_string2");

	parse_type_string(&type, "(vector integer)");
	test(! typep_vector_string(value, type), "typep_vector_string3");

	parse_type_string(&type, "(vector * 5)");
	test(typep_vector_string(value, type), "typep_vector_string4");

	parse_type_string(&type, "(vector * 6)");
	test(! typep_vector_string(value, type), "typep_vector_string5");

	strarray_char_heap(&value, "Hello");
	parse_type_string(&type, "(vector * 5)");
	test(typep_vector_string(value, type), "typep_vector_string6");

	RETURN;
}

static int test_typep_vector_dimension(void)
{
	addr value, type;

	array_alloc_stdarg(NULL, &value, 10, 0);
	parse_type_string(&type, "(vector * *)");
	GetArrayType(type, 1, &type);
	test(typep_vector_dimension(value, type), "typep_vector_dimension1");

	array_alloc_stdarg(NULL, &value, 10, 0);
	parse_type_string(&type, "(vector * 10)");
	GetArrayType(type, 1, &type);
	test(typep_vector_dimension(value, type), "typep_vector_dimension2");

	array_alloc_stdarg(NULL, &value, 0);
	parse_type_string(&type, "(vector * 0)");
	GetArrayType(type, 1, &type);
	test(! typep_vector_dimension(value, type), "typep_vector_dimension3");

	array_alloc_stdarg(NULL, &value, 10, 10, 0);
	parse_type_string(&type, "(vector * 2)");
	GetArrayType(type, 1, &type);
	test(! typep_vector_dimension(value, type), "typep_vector_dimension4");

	array_alloc_stdarg(NULL, &value, 10, 10, 0);
	parse_type_string(&type, "(vector * *)");
	GetArrayType(type, 1, &type);
	test(! typep_vector_dimension(value, type), "typep_vector_dimension5");

	RETURN;
}

static int test_typep_vector_array(void)
{
	addr value, type;

	array_alloc_stdarg(NULL, &value, 10, 0);
	parse_type_string(&type, "vector");
	test(typep_vector_array(value, type), "typep_vector_array1");

	array_alloc_stdarg(NULL, &value, 10, 20, 0);
	parse_type_string(&type, "vector");
	test(! typep_vector_array(value, type), "typep_vector_array2");

	array_alloc_stdarg(NULL, &value, 10, 0);
	parse_type_string(&type, "(vector t 10)");
	test(typep_vector_array(value, type), "typep_vector_array3");

	array_alloc_stdarg(NULL, &value, 10, 0);
	parse_type_string(&type, "(vector character *)");
	test(! typep_vector_array(value, type), "typep_vector_array4");

	array_alloc_stdarg(NULL, &value, 10, 0);
	parse_type_string(&type, "(vector * 9)");
	test(! typep_vector_array(value, type), "typep_vector_array5");

	RETURN;
}

static int test_typep_vector(void)
{
	addr value, type;

	/* vector */
	vector4_heap(&value, 3);
	SetArrayA4(value, 0, fixnum_heapr(10));
	SetArrayA4(value, 1, fixnum_heapr(20));
	SetArrayA4(value, 2, fixnum_heapr(30));
	parse_type_string(&type, "(vector t 3)");
	test(call_typep_table(value, type), "typep_vector1");

	/* string */
	strvect_char_heap(&value, "Hello");
	parse_type_string(&type, "(vector character 5)");
	test(call_typep_table(value, type), "typep_vector2");

	/* array */
	array_alloc_stdarg(NULL, &value, 10, 0);
	parse_type_string(&type, "(vector t 10)");
	test(call_typep_table(value, type), "typep_vector3");

	RETURN;
}

static int test_typep_simple_vector_vector(void)
{
	addr value, type;

	vector4_heap(&value, 3);
	SetArrayA4(value, 0, fixnum_heapr(10));
	SetArrayA4(value, 1, fixnum_heapr(20));
	SetArrayA4(value, 2, fixnum_heapr(30));
	parse_type_string(&type, "simple-vector");
	test(typep_simple_vector_vector(value, type), "typep_simple_vector_vector1");

	parse_type_string(&type, "(simple-vector 3)");
	test(typep_simple_vector_vector(value, type), "typep_simple_vector_vector2");

	parse_type_string(&type, "(simple-vector 4)");
	test(! typep_simple_vector_vector(value, type), "typep_simple_vector_vector3");

	RETURN;
}

static int test_typep_type_vector_array(void)
{
	addr value, type;

	array_alloc_stdarg(NULL, &value, 10, 0);
	allocate_array_alloc(NULL, value);
	parse_type_string(&type, "simple-vector");
	test(typep_type_vector_array(value, type, LISPDECL_T),
			"typep_type_vector_array1");

	array_alloc_stdarg(NULL, &value, 10, 20, 0);
	allocate_array_alloc(NULL, value);
	parse_type_string(&type, "simple-vector");
	test(! typep_type_vector_array(value, type, LISPDECL_T),
			"typep_type_vector_array2");

	array_alloc_stdarg(NULL, &value, 10, 0);
	allocate_array_alloc(NULL, value);
	parse_type_string(&type, "(simple-vector 10)");
	test(typep_type_vector_array(value, type, LISPDECL_T),
			"typep_type_vector_array3");

	array_alloc_stdarg(NULL, &value, 10, 0);
	allocate_array_alloc(NULL, value);
	parse_type_string(&type, "(simple-vector *)");
	test(typep_type_vector_array(value, type, LISPDECL_T),
			"typep_type_vector_array4");

	array_alloc_stdarg(NULL, &value, 10, 0);
	allocate_array_alloc(NULL, value);
	parse_type_string(&type, "(simple-vector 9)");
	test(! typep_type_vector_array(value, type, LISPDECL_T),
			"typep_type_vector_array5");

	strarray_char_heap(&value, "Hello");
	parse_type_string(&type, "(simple-vector *)");
	test(! typep_type_vector_array(value, type, LISPDECL_T),
			"typep_type_vector_array6");
	test(typep_type_vector_array(value, type, LISPDECL_CHARACTER),
			"typep_type_vector_array7");

	RETURN;
}

static int test_typep_simple_vector(void)
{
	addr value, type;

	/* vector */
	vector4_heap(&value, 3);
	SetArrayA4(value, 0, fixnum_heapr(10));
	SetArrayA4(value, 1, fixnum_heapr(20));
	SetArrayA4(value, 2, fixnum_heapr(30));
	parse_type_string(&type, "(simple-vector 3)");
	test(call_typep_table(value, type), "typep_simple_vector1");

	/* array */
	array_alloc_stdarg(NULL, &value, 10, 0);
	allocate_array_alloc(NULL, value);
	parse_type_string(&type, "(simple-vector 10)");
	test(call_typep_table(value, type), "typep_simple_vector2");

	/* not simple */
	array_alloc_stdarg(NULL, &value, 10, 0);
	ArrayInfoStruct(value)->adjustable = 1;
	allocate_array_alloc(NULL, value);
	parse_type_string(&type, "(simple-vector 10)");
	test(! call_typep_table(value, type), "typep_simple_vector3");

	RETURN;
}

static int test_typep_bit_vector(void)
{
	addr value, type;

	array_alloc_stdarg(NULL, &value, 10, 0);
	parse_type_string(&type, "bit");
	SetArrayInfo(value, ARRAY_INFO_TYPE, type);
	allocate_array_alloc(NULL, value);
	parse_type_string(&type, "(bit-vector 10)");
	test(call_typep_table(value, type), "typep_bit_vector1");

	parse_type_string(&type, "(bit-vector *)");
	test(call_typep_table(value, type), "typep_bit_vector2");

	parse_type_string(&type, "(bit-vector 9)");
	test(! call_typep_table(value, type), "typep_bit_vector3");

	strarray_char_heap(&value, "Hello");
	parse_type_string(&type, "(bit-vector *)");
	test(! call_typep_table(value, type), "typep_bit_vector4");

	RETURN;
}

static int test_typep_simple_bit_vector(void)
{
	addr value, type;

	array_alloc_stdarg(NULL, &value, 10, 0);
	parse_type_string(&type, "bit");
	SetArrayInfo(value, ARRAY_INFO_TYPE, type);
	allocate_array_alloc(NULL, value);
	parse_type_string(&type, "(simple-bit-vector 10)");
	test(call_typep_table(value, type), "typep_simple_bit_vector1");

	parse_type_string(&type, "(simple-bit-vector *)");
	test(call_typep_table(value, type), "typep_simple_bit_vector2");

	parse_type_string(&type, "(simple-bit-vector 9)");
	test(! call_typep_table(value, type), "typep_simple_bit_vector3");

	strarray_char_heap(&value, "Hello");
	parse_type_string(&type, "(simple-bit-vector *)");
	test(! call_typep_table(value, type), "typep_simple_bit_vector4");

	array_alloc_stdarg(NULL, &value, 10, 0);
	parse_type_string(&type, "bit");
	SetArrayInfo(value, ARRAY_INFO_TYPE, type);
	ArrayInfoStruct(value)->adjustable = 1; /* not simple */
	allocate_array_alloc(NULL, value);
	parse_type_string(&type, "(simple-bit-vector *)");
	test(! call_typep_table(value, type), "typep_simple_bit_vector5");

	RETURN;
}

static int test_typep_extended_char(void)
{
	addr value, type;

	character_heap(&value, 0x80001000);
	parse_type_string(&type, "extended-char");
	test(call_typep_table(value, type), "typep_extended_char1");

	character_heap(&value, 'A');
	test(! call_typep_table(value, type), "typep_extended_char2");

	fixnum_heap(&value, 'A');
	test(! call_typep_table(value, type), "typep_extended_char3");

	RETURN;
}

static int test_typep_string(void)
{
	addr value, type;

	parse_type_string(&type, "string");
	strvect_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_string1");
	strarray_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_string2");

	parse_type_string(&type, "(string 5)");
	strvect_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_string3");
	strarray_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_string4");

	parse_type_string(&type, "(string 6)");
	strvect_char_heap(&value, "Hello");
	test(! call_typep_table(value, type), "typep_string5");
	strarray_char_heap(&value, "Hello");
	test(! call_typep_table(value, type), "typep_string6");

	parse_type_string(&type, "string");
	fixnum_heap(&value, 10);
	test(! call_typep_table(value, type), "typep_string7");

	parse_type_string(&type, "string");
	strvect_char_heap(&value, "Hello");
	string_setc(value, 3, 0x80001000);
	test(call_typep_table(value, type), "typep_string8");

	RETURN;
}

static int test_typep_base_string(void)
{
	addr value, type;

	parse_type_string(&type, "base-string");
	strvect_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_base_string1");
	strarray_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_base_string2");

	parse_type_string(&type, "(base-string 5)");
	strvect_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_base_string3");
	strarray_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_base_string4");

	parse_type_string(&type, "(base-string 6)");
	strvect_char_heap(&value, "Hello");
	test(! call_typep_table(value, type), "typep_base_string5");
	strarray_char_heap(&value, "Hello");
	test(! call_typep_table(value, type), "typep_base_string6");

	parse_type_string(&type, "base-string");
	fixnum_heap(&value, 10);
	test(! call_typep_table(value, type), "typep_base_string7");

	parse_type_string(&type, "base-string");
	strvect_char_heap(&value, "Hello");
	string_setc(value, 3, 0x80001000);
	test(! call_typep_table(value, type), "typep_base_string8");

	RETURN;
}

static int test_typep_simple_string(void)
{
	addr value, type;

	parse_type_string(&type, "simple-string");
	strvect_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_simple_string1");
	strarray_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_simple_string2");

	parse_type_string(&type, "(simple-string 5)");
	strvect_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_simple_string3");
	strarray_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_simple_string4");

	parse_type_string(&type, "(simple-string 6)");
	strvect_char_heap(&value, "Hello");
	test(! call_typep_table(value, type), "typep_simple_string5");
	strarray_char_heap(&value, "Hello");
	test(! call_typep_table(value, type), "typep_simple_string6");

	parse_type_string(&type, "simple-string");
	fixnum_heap(&value, 10);
	test(! call_typep_table(value, type), "typep_simple_string7");

	parse_type_string(&type, "simple-string");
	strvect_char_heap(&value, "Hello");
	string_setc(value, 3, 0x80001000);
	test(call_typep_table(value, type), "typep_simple_string8");

	parse_type_string(&type, "simple-string");
	strarray_char_heap(&value, "Hello");
	ArrayInfoStruct(value)->adjustable = 1; /* not simple */
	allocate_array_alloc(NULL, value);
	test(! call_typep_table(value, type), "typep_simple_string9");

	RETURN;
}

static int test_typep_simple_base_string(void)
{
	addr value, type;

	parse_type_string(&type, "simple-base-string");
	strvect_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_simple_base_string1");
	strarray_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_simple_base_string2");

	parse_type_string(&type, "(simple-base-string 5)");
	strvect_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_simple_base_string3");
	strarray_char_heap(&value, "Hello");
	test(call_typep_table(value, type), "typep_simple_base_string4");

	parse_type_string(&type, "(simple-base-string 6)");
	strvect_char_heap(&value, "Hello");
	test(! call_typep_table(value, type), "typep_simple_base_string5");
	strarray_char_heap(&value, "Hello");
	test(! call_typep_table(value, type), "typep_simple_base_string6");

	parse_type_string(&type, "simple-base-string");
	fixnum_heap(&value, 10);
	test(! call_typep_table(value, type), "typep_simple_base_string7");

	parse_type_string(&type, "simple-base-string");
	strvect_char_heap(&value, "Hello");
	string_setc(value, 3, 0x80001000);
	test(! call_typep_table(value, type), "typep_simple_base_string8");

	parse_type_string(&type, "simple-base-string");
	strarray_char_heap(&value, "Hello");
	ArrayInfoStruct(value)->adjustable = 1; /* not simple */
	allocate_array_alloc(NULL, value);
	test(! call_typep_table(value, type), "typep_simple_base_string9");

	RETURN;
}

static int test_typep_signed_byte(void)
{
	addr value, type;

	parse_type_string(&type, "signed-byte");
	fixnum_heap(&value, 10);
	test(call_typep_table(value, type), "typep_signed_byte1");
	fixnum_heap(&value, -10);
	test(call_typep_table(value, type), "typep_signed_byte2");

	bignum_value_alloc(NULL, &value, signplus_bignum, 10);
	test(call_typep_table(value, type), "typep_signed_byte3");
	bignum_value_alloc(NULL, &value, signminus_bignum, 10);
	test(call_typep_table(value, type), "typep_signed_byte4");

	parse_type_string(&type, "(signed-byte 8)");
	fixnum_heap(&value, -129);
	test(! call_typep_table(value, type), "typep_signed_byte5");
	fixnum_heap(&value, -128);
	test(call_typep_table(value, type), "typep_signed_byte6");
	fixnum_heap(&value, 127);
	test(call_typep_table(value, type), "typep_signed_byte7");
	fixnum_heap(&value, 128);
	test(! call_typep_table(value, type), "typep_signed_byte8");

	bignum_value_alloc(NULL, &value, signminus_bignum, 129);
	test(! call_typep_table(value, type), "typep_signed_byte9");
	bignum_value_alloc(NULL, &value, signminus_bignum, 128);
	test(call_typep_table(value, type), "typep_signed_byte10");
	bignum_value_alloc(NULL, &value, signplus_bignum, 127);
	test(call_typep_table(value, type), "typep_signed_byte11");
	bignum_value_alloc(NULL, &value, signplus_bignum, 128);
	test(! call_typep_table(value, type), "typep_signed_byte12");

	character_heap(&value, 'A');
	test(! call_typep_table(value, type), "typep_signed_byte13");

	RETURN;
}

static int test_typep_unsigned_byte(void)
{
	addr value, type;

	parse_type_string(&type, "unsigned-byte");
	fixnum_heap(&value, 10);
	test(call_typep_table(value, type), "typep_unsigned_byte1");
	fixnum_heap(&value, -10);
	test(! call_typep_table(value, type), "typep_unsigned_byte2");

	bignum_value_alloc(NULL, &value, signplus_bignum, 10);
	test(call_typep_table(value, type), "typep_unsigned_byte3");
	bignum_value_alloc(NULL, &value, signminus_bignum, 10);
	test(! call_typep_table(value, type), "typep_unsigned_byte4");

	parse_type_string(&type, "(unsigned-byte 8)");
	fixnum_heap(&value, -10);
	test(! call_typep_table(value, type), "typep_unsigned_byte6");
	fixnum_heap(&value, 0);
	test(call_typep_table(value, type), "typep_unsigned_byte6");
	fixnum_heap(&value, 255);
	test(call_typep_table(value, type), "typep_unsigned_byte7");
	fixnum_heap(&value, 256);
	test(! call_typep_table(value, type), "typep_unsigned_byte8");

	bignum_value_alloc(NULL, &value, signminus_bignum, 10);
	test(! call_typep_table(value, type), "typep_unsigned_byte9");
	bignum_value_alloc(NULL, &value, signplus_bignum, 0);
	test(call_typep_table(value, type), "typep_unsigned_byte10");
	bignum_value_alloc(NULL, &value, signplus_bignum, 255);
	test(call_typep_table(value, type), "typep_unsigned_byte11");
	bignum_value_alloc(NULL, &value, signplus_bignum, 256);
	test(! call_typep_table(value, type), "typep_unsigned_byte12");

	character_heap(&value, 'A');
	test(! call_typep_table(value, type), "typep_unsigned_byte13");

	RETURN;
}

static int test_typep_bit(void)
{
	addr value, type;

	parse_type_string(&type, "bit");
	fixnum_heap(&value, -1);
	test(! call_typep_table(value, type), "typep_bit1");
	fixnum_heap(&value, 0);
	test(call_typep_table(value, type), "typep_bit2");
	fixnum_heap(&value, 1);
	test(call_typep_table(value, type), "typep_bit3");

	parse_type_string(&type, "bit");
	bignum_value_alloc(NULL, &value, signminus_bignum, 1);
	test(! call_typep_table(value, type), "typep_bit4");
	bignum_value_alloc(NULL, &value, signplus_bignum, 0);
	test(call_typep_table(value, type), "typep_bit5");
	bignum_value_alloc(NULL, &value, signplus_bignum, 1);
	test(call_typep_table(value, type), "typep_bit6");

	character_heap(&value, 0);
	test(! call_typep_table(value, type), "typep_bit7");

	RETURN;
}

static int test_typep_fixnum(void)
{
	addr value, type;

	parse_type_string(&type, "fixnum");
	fixnum_heap(&value, 0);
	test(call_typep_table(value, type), "typep_fixnum1");
	fixnum_heap(&value, 30);
	test(call_typep_table(value, type), "typep_fixnum2");
	fixnum_heap(&value, -40);
	test(call_typep_table(value, type), "typep_fixnum3");
	fixnum_heap(&value, FIXNUM_MAX);
	test(call_typep_table(value, type), "typep_fixnum4");
	fixnum_heap(&value, FIXNUM_MIN);
	test(call_typep_table(value, type), "typep_fixnum5");

	bignum_value_alloc(NULL, &value, signminus_bignum, FIXNUM_UMIN);
	test(call_typep_table(value, type), "typep_fixnum6");
	bignum_value_alloc(NULL, &value, signplus_bignum, FIXNUM_MAX);
	test(call_typep_table(value, type), "typep_fixnum7");
	bignum_value_alloc(NULL, &value, signminus_bignum, FIXNUM_UMIN + 1UL);
	test(! call_typep_table(value, type), "typep_fixnum6");
	bignum_value_alloc(NULL, &value, signplus_bignum, ((bigtype)FIXNUM_MAX) + 1UL);
	test(! call_typep_table(value, type), "typep_fixnum7");

	bignum_value2_alloc(NULL, &value, signplus_bignum, 1, 1);
	test(! call_typep_table(value, type), "typep_fixnum8");
	character_heap(&value, 'A');
	test(! call_typep_table(value, type), "typep_fixnum9");

	RETURN;
}

static int test_typep_bignum(void)
{
	addr value, type;

	parse_type_string(&type, "bignum");
	fixnum_heap(&value, 0);
	test(! call_typep_table(value, type), "typep_bignum1");
	fixnum_heap(&value, 30);
	test(! call_typep_table(value, type), "typep_bignum2");
	fixnum_heap(&value, -40);
	test(! call_typep_table(value, type), "typep_bignum3");
	fixnum_heap(&value, FIXNUM_MAX);
	test(! call_typep_table(value, type), "typep_bignum4");
	fixnum_heap(&value, FIXNUM_MIN);
	test(! call_typep_table(value, type), "typep_bignum5");

	bignum_value_alloc(NULL, &value, signminus_bignum, FIXNUM_UMIN);
	test(! call_typep_table(value, type), "typep_bignum6");
	bignum_value_alloc(NULL, &value, signplus_bignum, FIXNUM_MAX);
	test(! call_typep_table(value, type), "typep_bignum7");
	bignum_value_alloc(NULL, &value, signminus_bignum, FIXNUM_UMIN + 1UL);
	test(call_typep_table(value, type), "typep_bignum6");
	bignum_value_alloc(NULL, &value, signplus_bignum, ((bigtype)FIXNUM_MAX) + 1UL);
	test(call_typep_table(value, type), "typep_bignum7");

	bignum_value2_alloc(NULL, &value, signplus_bignum, 1, 1);
	test(call_typep_table(value, type), "typep_bignum8");
	character_heap(&value, 'A');
	test(! call_typep_table(value, type), "typep_bignum9");

	RETURN;
}


/*
 *  Atomic-type
 */
static int test_typep_nil(void)
{
	addr value, type;

	interncommon("NIL", &type);
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_nil1");
	test(! typep_heap(Nil, type), "typep_nil2");
	test(! typep_heap(T, type), "typep_nil3");

	RETURN;
}

static int test_typep_t(void)
{
	addr value, type;

	interncommon("T", &type);
	fixnum_heap(&value, 100);
	test(typep_heap(value, type), "typep_t1");
	test(typep_heap(Nil, type), "typep_t2");
	test(typep_heap(T, type), "typep_t3");

	RETURN;
}

static int test_typep_null(void)
{
	addr value, type;

	interncommon("NULL", &type);
	consnil_heap(&value);
	test(! typep_heap(value, type), "typep_null1");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_null2");
	test(typep_heap(Nil, type), "typep_null3");
	test(! typep_heap(T, type), "typep_null4");

	RETURN;
}

static int test_typep_cons(void)
{
	addr value, type, pos1, pos2;

	/* asterisk */
	interncommon("CONS", &type);
	consnil_heap(&value);
	test(typep_heap(value, type), "typep_cons1");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_cons2");
	test(! typep_heap(Nil, type), "typep_cons3");
	test(! typep_heap(T, type), "typep_cons4");

	/* parameter */
	interncommon("CONS", &type);
	interncommon("*", &pos1);
	list_heap(&type, type, pos1, NULL);
	consnil_heap(&value);
	test(typep_heap(value, type), "typep_cons5");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_cons6");
	test(! typep_heap(Nil, type), "typep_cons7");
	test(! typep_heap(T, type), "typep_cons8");

	interncommon("CONS", &type);
	interncommon("*", &pos1);
	interncommon("*", &pos2);
	list_heap(&type, type, pos1, pos2, NULL);
	consnil_heap(&value);
	test(typep_heap(value, type), "typep_cons9");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_cons10");
	test(! typep_heap(Nil, type), "typep_cons11");
	test(! typep_heap(T, type), "typep_cons12");

	interncommon("CONS", &type);
	interncommon("INTEGER", &pos1);
	list_heap(&type, type, pos1, NULL);
	consnil_heap(&value);
	test(! typep_heap(value, type), "typep_cons13");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_cons14");
	test(! typep_heap(Nil, type), "typep_cons15");
	test(! typep_heap(T, type), "typep_cons16");

	fixnum_heap(&pos1, 100);
	cons_heap(&value, pos1, Nil);
	test(typep_heap(value, type), "typep_cons17");
	strvect_char_heap(&pos2, "Hello");
	cons_heap(&value, pos1, pos2);
	test(typep_heap(value, type), "typep_cons18");
	cons_heap(&value, pos2, Nil);
	test(! typep_heap(value, type), "typep_cons19");
	cons_heap(&value, pos2, pos1);
	test(! typep_heap(value, type), "typep_cons20");

	interncommon("CONS", &type);
	interncommon("INTEGER", &pos1);
	interncommon("*", &pos2);
	list_heap(&type, type, pos1, NULL);
	fixnum_heap(&pos1, 200);
	cons_heap(&value, pos1, Nil);
	test(typep_heap(value, type), "typep_cons21");

	interncommon("CONS", &type);
	interncommon("*", &pos1);
	interncommon("INTEGER", &pos2);
	list_heap(&type, type, pos1, pos2, NULL);
	fixnum_heap(&pos1, 200);
	cons_heap(&value, pos1, Nil);
	test(! typep_heap(value, type), "typep_cons22");
	cons_heap(&value, Nil, pos1);
	test(typep_heap(value, type), "typep_cons23");
	cons_heap(&value, pos1, pos1);
	test(typep_heap(value, type), "typep_cons24");

	interncommon("CONS", &type);
	interncommon("STRING", &pos1);
	interncommon("INTEGER", &pos2);
	list_heap(&type, type, pos1, pos2, NULL);
	fixnum_heap(&pos1, 200);
	cons_heap(&value, pos1, Nil);
	test(! typep_heap(value, type), "typep_cons25");
	cons_heap(&value, Nil, pos1);
	test(! typep_heap(value, type), "typep_cons26");
	cons_heap(&value, pos1, pos1);
	test(! typep_heap(value, type), "typep_cons27");
	strvect_char_heap(&pos2, "Hello");
	cons_heap(&value, pos1, pos2);
	test(! typep_heap(value, type), "typep_cons28");
	cons_heap(&value, pos2, pos1);
	test(typep_heap(value, type), "typep_cons29");

	RETURN;
}

static int test_typep_hash_table(void)
{
	addr value, type;

	interncommon("HASH-TABLE", &type);
	hashtable_heap(&value);
	test(typep_heap(value, type), "typep_hash_table1");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_hash_table2");
	test(! typep_heap(Nil, type), "typep_hash_table3");
	test(! typep_heap(T, type), "typep_hash_table4");

	RETURN;
}

static int test_typep_symbol(void)
{
	addr value, type;

	interncommon("SYMBOL", &type);
	internchar(LISP_PACKAGE, "HELLO", &value);
	test(typep_heap(value, type), "typep_symbol1");
	internchar(LISP_KEYWORD, "AAA", &value);
	test(typep_heap(value, type), "typep_symbol2");
	symbol_heap(&value);
	test(typep_heap(value, type), "typep_symbol3");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_symbol4");
	test(typep_heap(Nil, type), "typep_symbol5");
	test(typep_heap(T, type), "typep_symbol6");

	RETURN;
}

static int test_typep_keyword(void)
{
	addr value, type;

	interncommon("KEYWORD", &type);
	internchar(LISP_PACKAGE, "HELLO", &value);
	test(! typep_heap(value, type), "typep_keyword1");
	internchar(LISP_KEYWORD, "AAA", &value);
	test(typep_heap(value, type), "typep_keyword2");
	symbol_heap(&value);
	test(! typep_heap(value, type), "typep_keyword3");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_keyword4");
	test(! typep_heap(Nil, type), "typep_keyword5");
	test(! typep_heap(T, type), "typep_keyword6");

	RETURN;
}

static int test_typep_package(void)
{
	addr value, type;

	interncommon("PACKAGE", &type);
	find_char_package(LISP_COMMON, &value);
	test(typep_heap(value, type), "typep_package1");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_package2");
	test(! typep_heap(Nil, type), "typep_package3");
	test(! typep_heap(T, type), "typep_package4");

	RETURN;
}

static int test_typep_random_state(void)
{
	addr value, type;

	interncommon("RANDOM-STATE", &type);
	make_random_state_heap(Execute_Thread, &value, Nil);
	test(typep_heap(value, type), "typep_random_state1");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_random_state2");
	test(! typep_heap(Nil, type), "typep_random_state3");
	test(! typep_heap(T, type), "typep_random_state4");

	RETURN;
}

static int test_typep_readtable(void)
{
	addr value, type;

	interncommon("READTABLE", &type);
	interncommon("*READTABLE*", &value);
	getspecialcheck_local(Execute_Thread, value, &value);
	test(typep_heap(value, type), "typep_readtable1");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_readtable2");
	test(! typep_heap(Nil, type), "typep_readtable3");
	test(! typep_heap(T, type), "typep_readtable4");

	RETURN;
}

static int test_typep_function(void)
{
	addr value, type;

	interncommon("FUNCTION", &type);
	interncommon("CAR", &value);
	test(! typep_heap(value, type), "typep_function1");
	getfunctioncheck_local(Execute_Thread, value, &value);
	test(typep_heap(value, type), "typep_function2");

	RETURN;
}

static int test_typep_compiled(void)
{
	addr value, type;

	interncommon("COMPILED-FUNCTION", &type);
	interncommon("CAR", &value);
	test(! typep_heap(value, type), "typep_compiled1");
	getfunctioncheck_local(Execute_Thread, value, &value);
	test(typep_heap(value, type), "typep_compiled2");

	RETURN;
}

static int test_typep_pathname(void)
{
	addr value, type;

	interncommon("PATHNAME", &type);
	parse_pathname_char_heap(Execute_Thread, "/usr/local/", &value);
	test(typep_heap(value, type), "typep_pathname1");
	SetLogicalPathname(value, 1);
	test(typep_heap(value, type), "typep_pathname2");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_pathname3");
	test(! typep_heap(Nil, type), "typep_pathname4");
	test(! typep_heap(T, type), "typep_pathname5");

	RETURN;
}

static int test_typep_logical_pathname(void)
{
	addr value, type;

	interncommon("LOGICAL-PATHNAME", &type);
	parse_pathname_char_heap(Execute_Thread, "/usr/local/", &value);
	test(! typep_heap(value, type), "typep_logical_pathname1");
	SetLogicalPathname(value, 1);
	test(typep_heap(value, type), "typep_logical_pathname2");
	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_logical_pathname3");
	test(! typep_heap(Nil, type), "typep_logical_pathname4");
	test(! typep_heap(T, type), "typep_logical_pathname5");

	RETURN;
}

static int test_typep_sequence(void)
{
	addr value, type;

	interncommon("SEQUENCE", &type);
	test(typep_heap(Nil, type), "typep_sequence1");
	consnil_heap(&value);
	test(typep_heap(value, type), "typep_sequence2");
	vector4_heap(&value, 10);
	test(typep_heap(value, type), "typep_sequence3");

	fixnum_heap(&value, 100);
	test(! typep_heap(value, type), "typep_sequence3");
	test(! typep_heap(T, type), "typep_sequence5");

	array_alloc_stdarg(NULL, &value, 4, 0);
	test(typep_heap(value, type), "typep_sequence6");
	array_alloc_stdarg(NULL, &value, 4, 5, 0);
	test(! typep_heap(value, type), "typep_sequence7");

	RETURN;
}

static int test_equal_array_dimension(void)
{
	addr value, type;

	array_alloc_stdarg(NULL, &value, 3, 0);
	parse_type_string(&type, "(array * (3))");
	GetArrayType(type, 1, &type);
	test(equal_array_dimension(value, type), "equal_array_dimension1");

	array_alloc_stdarg(NULL, &value, 3, 4, 0);
	parse_type_string(&type, "(array * (3))");
	GetArrayType(type, 1, &type);
	test(! equal_array_dimension(value, type), "equal_array_dimension2");

	array_alloc_stdarg(NULL, &value, 3, 0);
	parse_type_string(&type, "(array * (3 *))");
	GetArrayType(type, 1, &type);
	test(! equal_array_dimension(value, type), "equal_array_dimension3");

	array_alloc_stdarg(NULL, &value, 3, 4, 0);
	parse_type_string(&type, "(array * (3 *))");
	GetArrayType(type, 1, &type);
	test(equal_array_dimension(value, type), "equal_array_dimension4");

	array_alloc_stdarg(NULL, &value, 3, 4, 0);
	parse_type_string(&type, "(array * (3 5))");
	GetArrayType(type, 1, &type);
	test(! equal_array_dimension(value, type), "equal_array_dimension5");

	array_alloc_stdarg(NULL, &value, 3, 4, 5, 0);
	parse_type_string(&type, "(array * (3 4 5))");
	GetArrayType(type, 1, &type);
	test(equal_array_dimension(value, type), "equal_array_dimension6");

	array_alloc_stdarg(NULL, &value, 0);
	parse_type_string(&type, "(array * (3))");
	GetArrayType(type, 1, &type);
	test(! equal_array_dimension(value, type), "equal_array_dimension7");

	RETURN;
}

static int test_typep_array_dimension(void)
{
	addr value, type;

	array_alloc_stdarg(NULL, &value, 3, 0);
	parse_type_string(&type, "(array * (3))");
	GetArrayType(type, 1, &type);
	test(typep_array_dimension(value, type), "typep_array_dimension1");

	array_alloc_stdarg(NULL, &value, 3, 4, 5, 0);
	parse_type_string(&type, "(array * (3 * 5))");
	GetArrayType(type, 1, &type);
	test(typep_array_dimension(value, type), "typep_array_dimension2");

	array_alloc_stdarg(NULL, &value, 3, 4, 5, 0);
	parse_type_string(&type, "(array * 3)");
	GetArrayType(type, 1, &type);
	test(typep_array_dimension(value, type), "typep_array_dimension3");

	array_alloc_stdarg(NULL, &value, 3, 4, 5, 0);
	parse_type_string(&type, "(array * 4)");
	GetArrayType(type, 1, &type);
	test(! typep_array_dimension(value, type), "typep_array_dimension4");

	array_alloc_stdarg(NULL, &value, 3, 4, 5, 0);
	parse_type_string(&type, "(array * *)");
	GetArrayType(type, 1, &type);
	test(typep_array_dimension(value, type), "typep_array_dimension5");

	array_alloc_stdarg(NULL, &value, 0);
	parse_type_string(&type, "(array * ())");
	GetArrayType(type, 1, &type);
	test(typep_array_dimension(value, type), "typep_array_dimension6");

	RETURN;
}

static int test_typep_array_array(void)
{
	addr value, type;

	array_alloc_stdarg(NULL, &value, 3, 0);
	parse_type_string(&type, "(array * (3))");
	test(typep_array_array(value, type), "typep_array_array1");

	array_alloc_stdarg(NULL, &value, 3, 0);
	parse_type_string(&type, "(array t (3))");
	test(typep_array_array(value, type), "typep_array_array2");

	array_alloc_stdarg(NULL, &value, 3, 0);
	parse_type_string(&type, "(array character (3))");
	test(! typep_array_array(value, type), "typep_array_array3");

	array_alloc_stdarg(NULL, &value, 3, 4, 5, 0);
	parse_type_string(&type, "(array t (3 4 5))");
	test(typep_array_array(value, type), "typep_array_array4");

	array_alloc_stdarg(NULL, &value, 3, 4, 5, 0);
	parse_type_string(&type, "(array t *)");
	test(typep_array_array(value, type), "typep_array_array5");

	array_alloc_stdarg(NULL, &value, 3, 4, 5, 0);
	parse_type_string(&type, "array");
	test(typep_array_array(value, type), "typep_array_array6");

	RETURN;
}

static int test_typep_array_vector(void)
{
	addr value, type;

	vector_heap(&value, 10);
	parse_type_string(&type, "(array * *)");
	test(typep_array_vector(value, type), "typep_array_vector1");

	parse_type_string(&type, "(array * (10))");
	test(typep_array_vector(value, type), "typep_array_vector2");

	parse_type_string(&type, "(array * 1)");
	test(typep_array_vector(value, type), "typep_array_vector3");

	parse_type_string(&type, "(array * 3)");
	test(! typep_array_vector(value, type), "typep_array_vector4");

	parse_type_string(&type, "(array * (10 *))");
	test(! typep_array_vector(value, type), "typep_array_vector5");

	parse_type_string(&type, "(array * ())");
	test(! typep_array_vector(value, type), "typep_array_vector6");

	parse_type_string(&type, "(array t (10))");
	test(typep_array_vector(value, type), "typep_array_vector7");

	parse_type_string(&type, "(array character (10))");
	test(! typep_array_vector(value, type), "typep_array_vector8");

	RETURN;
}

static int test_typep_array_string(void)
{
	addr value, type;

	strvect_char_heap(&value, "HelloHello");
	parse_type_string(&type, "(array * *)");
	test(typep_array_string(value, type), "typep_array_string1");

	parse_type_string(&type, "(array * (10))");
	test(typep_array_string(value, type), "typep_array_string2");

	parse_type_string(&type, "(array * 1)");
	test(typep_array_string(value, type), "typep_array_string3");

	parse_type_string(&type, "(array * 3)");
	test(! typep_array_string(value, type), "typep_array_string4");

	parse_type_string(&type, "(array * (10 *))");
	test(! typep_array_string(value, type), "typep_array_string5");

	parse_type_string(&type, "(array * ())");
	test(! typep_array_string(value, type), "typep_array_string6");

	parse_type_string(&type, "(array character (10))");
	test(typep_array_string(value, type), "typep_array_string7");

	parse_type_string(&type, "(array t (10))");
	test(! typep_array_string(value, type), "typep_array_string8");

	RETURN;
}

static int test_typep_array(void)
{
	addr value, type;

	array_alloc_stdarg(NULL, &value, 3, 4, 5, 0);
	parse_type_string(&type, "(array t 3)");
	test(typep_table(value, type), "typep_array1");

	vector_heap(&value, 10);
	parse_type_string(&type, "(array t (10))");
	test(typep_table(value, type), "typep_array2");

	strvect_char_heap(&value, "HelloHello");
	parse_type_string(&type, "(array character *)");
	test(typep_table(value, type), "typep_array3");

	character_heap(&value, 'A');
	parse_type_string(&type, "array");
	test(! typep_table(value, type), "typep_array4");

	RETURN;
}

static int test_typep_simple_array(void)
{
	addr value, type;

	array_alloc_stdarg(NULL, &value, 3, 4, 5, 0);
	allocate_array_alloc(NULL, value); /* simple */
	parse_type_string(&type, "(simple-array t 3)");
	test(typep_table(value, type), "typep_simple_array1");

	vector_heap(&value, 10);
	parse_type_string(&type, "(simple-array t (10))");
	test(typep_table(value, type), "typep_simple_array2");

	strvect_char_heap(&value, "HelloHello");
	parse_type_string(&type, "(simple-array character *)");
	test(typep_table(value, type), "typep_simple_array3");

	character_heap(&value, 'A');
	parse_type_string(&type, "simple-array");
	test(! typep_table(value, type), "typep_simple_array4");

	array_alloc_stdarg(NULL, &value, 5, 0);
	ArrayInfoStruct(value)->adjustable = 1; /* not simple */
	allocate_array_alloc(NULL, value);
	parse_type_string(&type, "simple-array");
	test(! typep_table(value, type), "typep_simple_array5");

	RETURN;
}

static int test_typep_character(void)
{
	addr value, type;

	parse_type_string(&type, "character");
	character_heap(&value, 'A');
	test(typep_table(value, type), "typep_character1");

	character_heap(&value, 0x0D);
	test(typep_table(value, type), "typep_character2");

	character_heap(&value, 0x80001000);
	test(typep_table(value, type), "typep_character3");

	fixnum_heap(&value, 'A');
	test(! typep_table(value, type), "typep_character4");

	RETURN;
}

static int test_typep_base_char(void)
{
	addr value, type;

	parse_type_string(&type, "base-char");
	character_heap(&value, 'A');
	test(typep_table(value, type), "typep_base_char1");

	character_heap(&value, 0x0D);
	test(typep_table(value, type), "typep_base_char2");

	character_heap(&value, 0x80001000);
	test(! typep_table(value, type), "typep_base_char3");

	fixnum_heap(&value, 'A');
	test(! typep_table(value, type), "typep_base_char4");

	RETURN;
}

static int test_typep_standard_char(void)
{
	addr value, type;

	parse_type_string(&type, "standard-char");
	character_heap(&value, 'A');
	test(typep_table(value, type), "typep_standard_char1");

	character_heap(&value, 0x0D);
	test(! typep_table(value, type), "typep_standard_char2");

	character_heap(&value, 0x80001000);
	test(! typep_table(value, type), "typep_standard_char3");

	fixnum_heap(&value, 'A');
	test(! typep_table(value, type), "typep_standard_char4");

	RETURN;
}

static int test_typep_number(void)
{
	addr value, type;

	parse_type_string(&type, "number");
	fixnum_heap(&value, 10);
	test(typep_table(value, type), "typep_number1");

	single_float_heap(&value, 10.0f);
	test(typep_table(value, type), "typep_number2");

	consnil_heap(&value);
	test(! typep_table(value, type), "typep_number3");

	RETURN;
}

static int test_typep_ratio(void)
{
	addr numer, denom, value, type;
	LocalRoot local;
	LocalStack stack;

	parse_type_string(&type, "ratio");
	local = Local_Thread;
	push_local(local, &stack);
	bignum_value_alloc(local, &numer, signplus_bignum, 12);
	bignum_value_alloc(local, &denom, signplus_bignum, 7);
	make_ratio_reduction_local(local, &value, signplus_bignum, numer, denom);
	test(typep_table(value, type), "typep_ratio1");
	rollback_local(local, stack);

	fixnum_heap(&value, 10);
	test(! typep_table(value, type), "typep_ratio2");

	RETURN;
}

static int test_typep_complex(void)
{
	addr real, imag, value, type;

	parse_type_string(&type, "complex");
	double_float_heap(&real, 10.0);
	double_float_heap(&imag, 20.0);
	complex_heap(&value, real, imag);
	test(typep_table(value, type), "typep_complex1");

	parse_type_string(&type, "(complex double-float)");
	test(typep_table(value, type), "typep_complex2");

	parse_type_string(&type, "(complex integer)");
	test(! typep_table(value, type), "typep_complex3");

	parse_type_string(&type, "complex");
	fixnum_heap(&value, 10);
	test(! typep_table(value, type), "typep_complex4");

	RETURN;
}


/*
 *  range
 */
static int test_less_integer(addr mode, fixnum left, fixnum right)
{
	return less_mode_nolocal(mode,
			fixnum_heapr(left), fixnum_heapr(right),
			less_integer_clang, less_equal_integer_clang);
}

static int test_less_mode_nolocal(void)
{
	test(test_less_integer(Nil, 10, 20), "less_mode_nolocal1");
	test(test_less_integer(Nil, 10, 10), "less_mode_nolocal2");
	test(! test_less_integer(Nil, 11, 10), "less_mode_nolocal3");
	test(test_less_integer(T, 10, 20), "less_mode_nolocal4");
	test(! test_less_integer(T, 10, 10), "less_mode_nolocal5");
	test(! test_less_integer(T, 11, 10), "less_mode_nolocal6");
	test(test_less_integer(type_asterisk_heapr(), 10, 5), "less_mode_nolocal7");

	RETURN;
}

static int test_typep_range_nolocal(void)
{
	addr value, type;

	fixnum_heap(&value, 10);
	parse_type_string(&type, "(integer * *)");
	test(typep_integer(value, type), "typep_range_nolocal1");

	parse_type_string(&type, "(integer 9 *)");
	test(typep_integer(value, type), "typep_range_nolocal2");

	parse_type_string(&type, "(integer 10 *)");
	test(typep_integer(value, type), "typep_range_nolocal3");

	parse_type_string(&type, "(integer 11 *)");
	test(! typep_integer(value, type), "typep_range_nolocal4");

	parse_type_string(&type, "(integer (9) *)");
	test(typep_integer(value, type), "typep_range_nolocal5");

	parse_type_string(&type, "(integer (10) *)");
	test(! typep_integer(value, type), "typep_range_nolocal6");

	parse_type_string(&type, "(integer (11) *)");
	test(! typep_integer(value, type), "typep_range_nolocal7");

	parse_type_string(&type, "(integer * 9)");
	test(! typep_integer(value, type), "typep_range_nolocal8");

	parse_type_string(&type, "(integer * 10)");
	test(typep_integer(value, type), "typep_range_nolocal9");

	parse_type_string(&type, "(integer * 11)");
	test(typep_integer(value, type), "typep_range_nolocal10");

	parse_type_string(&type, "(integer * (9))");
	test(! typep_integer(value, type), "typep_range_nolocal11");

	parse_type_string(&type, "(integer * (10))");
	test(! typep_integer(value, type), "typep_range_nolocal12");

	parse_type_string(&type, "(integer * (11))");
	test(typep_integer(value, type), "typep_range_nolocal13");

	parse_type_string(&type, "(integer 5 15)");
	test(typep_integer(value, type), "typep_range_nolocal14");

	parse_type_string(&type, "(integer 10 10)");
	test(typep_integer(value, type), "typep_range_nolocal15");

	parse_type_string(&type, "(integer 20 30)");
	test(! typep_integer(value, type), "typep_range_nolocal16");

	parse_type_string(&type, "(integer 20 100)");
	test(! typep_integer(value, type), "typep_range_nolocal17");

	RETURN;
}

static int test_less_real(addr mode, fixnum left, fixnum right)
{
	return less_mode_local(Local_Thread, mode,
			fixnum_heapr(left), fixnum_heapr(right),
			less_real_clang, less_equal_real_clang);
}

static int test_less_mode_local(void)
{
	test(test_less_real(Nil, 10, 20), "less_mode_local1");
	test(test_less_real(Nil, 10, 10), "less_mode_local2");
	test(! test_less_real(Nil, 11, 10), "less_mode_local3");
	test(test_less_real(T, 10, 20), "less_mode_local4");
	test(! test_less_real(T, 10, 10), "less_mode_local5");
	test(! test_less_real(T, 11, 10), "less_mode_local6");
	test(test_less_real(type_asterisk_heapr(), 10, 5), "less_mode_local7");

	RETURN;
}

static int test_typep_range_local(void)
{
	addr value, type;

	fixnum_heap(&value, 10);
	parse_type_string(&type, "(real * *)");
	test(typep_real(value, type), "typep_range_local1");

	parse_type_string(&type, "(real 9 *)");
	test(typep_real(value, type), "typep_range_local2");

	parse_type_string(&type, "(real 10 *)");
	test(typep_real(value, type), "typep_range_local3");

	parse_type_string(&type, "(real 11 *)");
	test(! typep_real(value, type), "typep_range_local4");

	parse_type_string(&type, "(real (9) *)");
	test(typep_real(value, type), "typep_range_local5");

	parse_type_string(&type, "(real (10) *)");
	test(! typep_real(value, type), "typep_range_local6");

	parse_type_string(&type, "(real (11) *)");
	test(! typep_real(value, type), "typep_range_local7");

	parse_type_string(&type, "(real * 9)");
	test(! typep_real(value, type), "typep_range_local8");

	parse_type_string(&type, "(real * 10)");
	test(typep_real(value, type), "typep_range_local9");

	parse_type_string(&type, "(real * 11)");
	test(typep_real(value, type), "typep_range_local10");

	parse_type_string(&type, "(real * (9))");
	test(! typep_real(value, type), "typep_range_local11");

	parse_type_string(&type, "(real * (10))");
	test(! typep_real(value, type), "typep_range_local12");

	parse_type_string(&type, "(real * (11))");
	test(typep_real(value, type), "typep_range_local13");

	parse_type_string(&type, "(real 5 15)");
	test(typep_real(value, type), "typep_range_local14");

	parse_type_string(&type, "(real 10 10)");
	test(typep_real(value, type), "typep_range_local15");

	parse_type_string(&type, "(real 20 30)");
	test(! typep_real(value, type), "typep_range_local16");

	parse_type_string(&type, "(real 20 100)");
	test(! typep_real(value, type), "typep_range_local17");

	RETURN;
}

static int test_typep_integer(void)
{
	addr value, type;

	fixnum_heap(&value, 20);
	parse_type_string(&type, "(integer 10 30)");
	test(typep_heap(value, type), "typep_integer1");

	RETURN;
}

static int test_typep_rational(void)
{
	addr value, type;

	fixnum_heap(&value, 20);
	parse_type_string(&type, "(rational 10 30)");
	test(typep_heap(value, type), "typep_rational1");

	RETURN;
}

static int test_typep_real(void)
{
	addr value, type;

	fixnum_heap(&value, 20);
	parse_type_string(&type, "(real 10 30)");
	test(typep_heap(value, type), "typep_real1");

	RETURN;
}

static int test_typep_float(void)
{
	addr value, type;

	single_float_heap(&value, 20.0f);
	parse_type_string(&type, "(float 10.0 30.0)");
	test(typep_heap(value, type), "typep_float1");

	RETURN;
}

static int test_typep_single_float(void)
{
	addr value, type;

	single_float_heap(&value, 20.0f);
	parse_type_string(&type, "(single-float 10.0e0 30.0e0)");
	test(typep_heap(value, type), "typep_single_float1");

	RETURN;
}

static int test_typep_double_float(void)
{
	addr value, type;

	double_float_heap(&value, 20.0);
	parse_type_string(&type, "(double-float 10.0d0 30.0d0)");
	test(typep_heap(value, type), "typep_double_float1");

	RETURN;
}

static int test_typep_long_float(void)
{
	addr value, type;

	long_float_heap(&value, 20.0L);
	parse_type_string(&type, "(long-float 10.0l0 30.0l0)");
	test(typep_heap(value, type), "typep_long_float1");

	RETURN;
}


/*
 *  typep-clang
 */
static int test_typep_table(void)
{
	addr value, type;

	fixnum_heap(&value, 20);
	parse_type_string(&type, "fixnum");
	test(typep_table(value, type), "typep_table1");

	RETURN;
}

static int test_typep_call(void)
{
	addr value, type;

	fixnum_heap(&value, 20);
	parse_type_string(&type, "fixnum");
	test(typep_call(value, type, 0), "typep_call1");

	parse_type_string(&type, "fixnum");
	test(typep_call(value, type, 1), "typep_call2");

	parse_type_string(&type, "*");
	test(typep_call(value, type, 1), "typep_call3");

	parse_type_string(&type, "cons");
	test(! typep_call(value, type, 0), "typep_call4");

	parse_type_string(&type, "fixnum");
	SetNotDecl(type, 1);
	test(! typep_call(value, type, 0), "typep_call5");

	parse_type_string(&type, "fixnum");
	SetNotDecl(type, 1);
	test(! typep_call(value, type, 1), "typep_call6");

	parse_type_string(&type, "*");
	SetNotDecl(type, 1);
	test(! typep_call(value, type, 1), "typep_call7");

	parse_type_string(&type, "cons");
	SetNotDecl(type, 1);
	test(typep_call(value, type, 0), "typep_call8");

	RETURN;
}

static int test_typep_clang(void)
{
	addr value, type;

	fixnum_heap(&value, 20);
	parse_type_string(&type, "fixnum");
	type_throw_heap(&type, type);
	test(typep_clang(value, type), "typep_clang1");

	RETURN;
}

static int test_typep_asterisk_clang(void)
{
	addr value, type;

	fixnum_heap(&value, 20);
	parse_type_string(&type, "fixnum");
	type_throw_heap(&type, type);
	test(typep_asterisk_clang(value, type), "typep_asterisk_heap1");

	parse_type_string(&type, "*");
	type_throw_heap(&type, type);
	test(typep_asterisk_clang(value, type), "typep_asterisk_heap3");

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
	TestBreak(test_character_decl_p);
	TestBreak(test_typep_vector_string);
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
	TestBreak(test_typep_array);
	TestBreak(test_typep_simple_array);
	TestBreak(test_typep_character);
	TestBreak(test_typep_base_char);
	TestBreak(test_typep_standard_char);
	TestBreak(test_typep_number);
	TestBreak(test_typep_ratio);
	TestBreak(test_typep_complex);
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
	/* typep-clang */
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
		build_calltype();
		build_syscall();
		build_common();
		build_readtable();
		lisp_init = 1;
		result = testbreak_type_typep();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

