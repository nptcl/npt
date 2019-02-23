#include "type_value.c"
#include "calltype.h"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "degrade.h"
#include "package.h"
#include "readtable.h"
#include "strtype.h"
#include "symbol.h"
#include "syscall.h"
#include "unicode.h"

static int test_type_value_integer(void)
{
	addr pos, check;

	fixnum_heap(&pos, 10);
	type_value_integer(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "type_value_integer1");
	GetArrayType(pos, 0, &check);
	test(check == Nil, "type_value_integer2");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 10, "type_value_integer3");
	GetArrayType(pos, 2, &check);
	test(check == Nil, "type_value_integer4");
	GetArrayType(pos, 3, &check);
	test(RefFixnum(check) == 10, "type_value_integer5");

	RETURN;
}

static int test_type_value_rational(void)
{
	addr pos, check;

	fixnum_heap(&pos, 10);
	type_value_rational(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_RATIONAL, "type_value_rational1");
	GetArrayType(pos, 0, &check);
	test(check == Nil, "type_value_rational2");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 10, "type_value_rational3");
	GetArrayType(pos, 2, &check);
	test(check == Nil, "type_value_rational4");
	GetArrayType(pos, 3, &check);
	test(RefFixnum(check) == 10, "type_value_rational5");

	RETURN;
}

static int test_type_value_real(void)
{
	addr pos, check;

	fixnum_heap(&pos, 10);
	type_value_real(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_REAL, "type_value_real1");
	GetArrayType(pos, 0, &check);
	test(check == Nil, "type_value_real2");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 10, "type_value_real3");
	GetArrayType(pos, 2, &check);
	test(check == Nil, "type_value_real4");
	GetArrayType(pos, 3, &check);
	test(RefFixnum(check) == 10, "type_value_real5");

	RETURN;
}

static int test_type_value_character(void)
{
	addr pos;

	character_heap(&pos, 'A');
	type_value_character(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_STANDARD_CHAR, "type_value_character1");

	character_heap(&pos, 0x0D);
	type_value_character(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_BASE_CHAR, "type_value_character2");

	character_heap(&pos, 0xF0000000);
	type_value_character(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_EXTENDED_CHAR, "type_value_character3");

	RETURN;
}

static int test_type_value_vector(void)
{
	addr pos;

	vector_heap(&pos, 10);
	type_value_vector(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_VECTOR, "type_value_vector1");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 10, "type_value_vector2");

	RETURN;
}

static int test_type_value_strvect(void)
{
	addr pos, check;

	strvect_char_heap(&pos, "Hello");
	type_value_strvect(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_BASE_STRING, "type_value_strvect1");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_strvect2");

	strvect_setc(pos, 2, 0x0D);
	type_value_strvect(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_BASE_STRING, "type_value_strvect3");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_strvect4");

	strvect_setc(pos, 3, 0xF0000000);
	type_value_strvect(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_STRING, "type_value_strvect5");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_strvect6");

	RETURN;
}

static int test_type_value_strarray(void)
{
	addr pos, check;

	strarray_char_heap(&pos, "Hello");
	type_value_strarray(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_BASE_STRING, "type_value_strarray1");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_strarray2");

	strarray_setc(pos, 2, 0x0D);
	type_value_strarray(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_BASE_STRING, "type_value_strarray3");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_strarray4");

	strarray_setc(pos, 3, 0xF0000000);
	type_value_strarray(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_STRING, "type_value_strarray5");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_strarray6");

	strarray_char_heap(&pos, "AAABBB");
	ArrayInfoStruct(pos)->adjustable = 1;
	allocate_array_alloc(NULL, pos);
	type_value_strarray(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_BASE_STRING, "type_value_strarray7");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 6, "type_value_strarray8");

	strarray_setc(pos, 3, 0xF0000000);
	type_value_strarray(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_STRING, "type_value_strarray9");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 6, "type_value_strarray10");

	RETURN;
}

static int test_type_value_string(void)
{
	addr pos, check;

	strvect_char_heap(&pos, "Hello");
	type_value_string(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_BASE_STRING, "type_value_string1");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_string2");

	strarray_char_heap(&pos, "Hello");
	type_value_string(NULL, &check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_BASE_STRING, "type_value_string3");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_string4");

	RETURN;
}

static int test_type_value_array_nil(void)
{
	addr pos, check;

	array_alloc_stdarg(NULL, &pos, 0);
	allocate_array_alloc(NULL, pos);
	type_value_array_nil(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "type_value_array_nil1");
	GetArrayType(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "type_value_array_nil2");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 0, "type_value_array_nil3");

	array_alloc_stdarg(NULL, &pos, 0);
	ArrayInfoStruct(pos)->adjustable = 1;
	allocate_array_alloc(NULL, pos);
	type_value_array_nil(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_ARRAY, "type_value_array_nil4");
	GetArrayType(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "type_value_array_nil5");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 0, "type_value_array_nil6");

	RETURN;
}

static int test_type_value_array_single(void)
{
	addr pos, check;

	array_alloc_stdarg(NULL, &pos, 11, 0);
	allocate_array_alloc(NULL, pos);
	type_value_array_single(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "type_value_array_single1");
	GetArrayType(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "type_value_array_single2");
	GetArrayType(pos, 1, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "type_value_array_single3");
	test(lenarrayr(check) == 1, "type_value_array_single4");
	GetArrayA4(check, 0, &check);
	test(RefFixnum(check) == 11, "type_value_array_single5");

	array_alloc_stdarg(NULL, &pos, 11, 0);
	ArrayInfoStruct(pos)->adjustable = 1;
	allocate_array_alloc(NULL, pos);
	type_value_array_single(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_ARRAY, "type_value_array_single6");
	GetArrayType(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "type_value_array_single7");
	GetArrayType(pos, 1, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "type_value_array_single8");
	test(lenarrayr(check) == 1, "type_value_array_single9");
	GetArrayA4(check, 0, &check);
	test(RefFixnum(check) == 11, "type_value_array_single10");

	RETURN;
}

static int test_type_value_array_multiple(void)
{
	addr pos, check;

	array_alloc_stdarg(NULL, &pos, 11, 12, 0);
	allocate_array_alloc(NULL, pos);
	type_value_array_multiple(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "type_value_array_multiple1");
	GetArrayType(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "type_value_array_multiple2");
	GetArrayType(pos, 1, &pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "type_value_array_multiple3");
	test(lenarrayr(pos) == 2, "type_value_array_multiple4");
	GetArrayA4(pos, 0, &check);
	test(RefFixnum(check) == 11, "type_value_array_multiple5");
	GetArrayA4(pos, 1, &check);
	test(RefFixnum(check) == 12, "type_value_array_multiple6");

	array_alloc_stdarg(NULL, &pos, 11, 12, 0);
	ArrayInfoStruct(pos)->adjustable = 1;
	allocate_array_alloc(NULL, pos);
	type_value_array_multiple(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_ARRAY, "type_value_array_multiple7");
	GetArrayType(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "type_value_array_multiple8");
	GetArrayType(pos, 1, &pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "type_value_array_multiple9");
	test(lenarrayr(pos) == 2, "type_value_array_multiple10");
	GetArrayA4(pos, 0, &check);
	test(RefFixnum(check) == 11, "type_value_array_multiple11");
	GetArrayA4(pos, 1, &check);
	test(RefFixnum(check) == 12, "type_value_array_multiple12");

	RETURN;
}

static int test_type_value_array(void)
{
	addr pos, check;

	array_alloc_stdarg(NULL, &pos, 0);
	allocate_array_alloc(NULL, pos);
	type_value_array(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "type_value_array1");
	GetArrayType(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "type_value_array2");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 0, "type_value_array3");

	array_alloc_stdarg(NULL, &pos, 11, 0);
	allocate_array_alloc(NULL, pos);
	type_value_array(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "type_value_array4");
	GetArrayType(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "type_value_array5");
	GetArrayType(pos, 1, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "type_value_array6");
	test(lenarrayr(check) == 1, "type_value_array7");
	GetArrayA4(check, 0, &check);
	test(RefFixnum(check) == 11, "type_value_array8");

	array_alloc_stdarg(NULL, &pos, 11, 12, 0);
	allocate_array_alloc(NULL, pos);
	type_value_array(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "type_value_array9");
	GetArrayType(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "type_value_array10");
	GetArrayType(pos, 1, &pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "type_value_array11");
	test(lenarrayr(pos) == 2, "type_value_array12");
	GetArrayA4(pos, 0, &check);
	test(RefFixnum(check) == 11, "type_value_array13");
	GetArrayA4(pos, 1, &check);
	test(RefFixnum(check) == 12, "type_value_array14");

	RETURN;
}

static int test_type_value_symbol(void)
{
	addr pos;

	interncommon("CAR", &pos);
	type_value_symbol(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SYMBOL, "type_value_symbol1");

	internchar_keyword("START", &pos);
	type_value_symbol(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_KEYWORD, "type_value_symbol2");

	RETURN;
}

static int test_type_value_float(void)
{
	addr pos, check;

	single_float_heap(&pos, 10.2f);
	type_value_float(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SINGLE_FLOAT, "type_value_float1");
	GetArrayType(pos, 0, &check);
	test(check == Nil, "type_value_float2");
	GetArrayType(pos, 1, &check);
	test(RefSingleFloat(check) == 10.2f, "type_value_float3");
	GetArrayType(pos, 2, &check);
	test(check == Nil, "type_value_float4");
	GetArrayType(pos, 3, &check);
	test(RefSingleFloat(check) == 10.2f, "type_value_float5");

	double_float_heap(&pos, 10.2);
	type_value_float(NULL, &pos, pos);
	test(RefLispDecl(pos) == LISPDECL_DOUBLE_FLOAT, "type_value_float6");

	RETURN;
}


/*
 *  main
 */
static int testbreak_type_value(void)
{
	TestBreak(test_type_value_integer);
	TestBreak(test_type_value_rational);
	TestBreak(test_type_value_real);
	TestBreak(test_type_value_character);
	TestBreak(test_type_value_vector);
	TestBreak(test_type_value_strvect);
	TestBreak(test_type_value_strarray);
	TestBreak(test_type_value_string);
	TestBreak(test_type_value_array_nil);
	TestBreak(test_type_value_array_single);
	TestBreak(test_type_value_array_multiple);
	TestBreak(test_type_value_array);
	TestBreak(test_type_value_symbol);
	TestBreak(test_type_value_float);

	return 0;
}

int test_type_value(void)
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
		result = testbreak_type_value();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

