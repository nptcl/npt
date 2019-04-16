#include "type_value.c"
#include "array.h"
#include "array_object.h"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "degrade.h"
#include "file.h"
#include "hashtable.h"
#include "package.h"
#include "readtable.h"
#include "stream.h"
#include "stream_broadcast.h"
#include "stream_concat.h"
#include "stream_echo.h"
#include "stream_file.h"
#include "stream_string.h"
#include "stream_synonym.h"
#include "stream_twoway.h"
#include "strtype.h"
#include "symbol.h"
#include "syscall.h"
#include "type_table.h"
#include "unicode.h"

static int test_type_value_nil(void)
{
	addr x;

	type_value_nil(&x);
	test(GetType(x) == LISPTYPE_TYPE, "type_value_nil1");
	test(LispDecl(x) == LISPDECL_NULL, "type_value_nil2");

	RETURN;
}

static int test_type_value_t(void)
{
	addr x;

	type_value_t(&x);
	test(GetType(x) == LISPTYPE_TYPE, "type_value_t1");
	test(LispDecl(x) == LISPDECL_BOOLEAN, "type_value_t2");

	RETURN;
}

static int test_type_value_nil_call(void)
{
	addr x;

	type_value(&x, Nil);
	test(GetType(x) == LISPTYPE_TYPE, "type_value_nil_call1");
	test(LispDecl(x) == LISPDECL_NULL, "type_value_nil_call2");

	RETURN;
}

static int test_type_value_t_call(void)
{
	addr x;

	type_value(&x, T);
	test(GetType(x) == LISPTYPE_TYPE, "type_value_t_call1");
	test(LispDecl(x) == LISPDECL_BOOLEAN, "type_value_t_call2");

	RETURN;
}

static int test_type_value_type(void)
{
	addr x;

	GetTypeTable(&x, Atom);
	type_value(&x, x);
	test(GetType(x) == LISPTYPE_TYPE, "type_value_type1");
	test(LispDecl(x) == LISPDECL_TYPE, "type_value_type2");

	RETURN;
}

static int test_type_value_clos(void)
{
	addr x, y;

	GetConst(COMMON_STANDARD_CLASS, &x);
	clos_find_class(x, &y);
	type_value(&x, y);
	test(GetType(x) == LISPTYPE_TYPE, "type_value_clos1");
	test(LispDecl(x) == LISPDECL_CLOS, "type_value_clos2");
	GetArrayType(x, 0, &x);
	GetConst(COMMON_STANDARD_CLASS, &y);
	clos_find_class(y, &y);
	test(x == y, "type_value_clos3");

	RETURN;
}

static int test_type_value_cons(void)
{
	addr x;

	cons_heap(&x, Nil, T);
	type_value(&x, x);
	test(GetType(x) == LISPTYPE_TYPE, "type_value_cons1");
	test(LispDecl(x) == LISPDECL_CONS, "type_value_cons2");

	RETURN;
}

static int test_type_value_strarray(void)
{
	addr pos, check;

	strarray_char_heap(&pos, "Hello");
	type_value_strarray(&check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_BASE_STRING, "type_value_strarray1");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_strarray2");

	strarray_setc(pos, 2, 0x0D);
	type_value_strarray(&check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_BASE_STRING, "type_value_strarray3");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_strarray4");

	strarray_setc(pos, 3, 0xF0000000);
	type_value_strarray(&check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_STRING, "type_value_strarray5");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_strarray6");

	strarray_char_heap(&pos, "AAABBB");
	ArrayInfoStruct(pos)->adjustable = 1;
	array_build_heap(pos);
	type_value_strarray(&check, pos);
	test(RefLispDecl(check) == LISPDECL_BASE_STRING, "type_value_strarray7");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 6, "type_value_strarray8");

	strarray_setc(pos, 3, 0xF0000000);
	type_value_strarray(&check, pos);
	test(RefLispDecl(check) == LISPDECL_STRING, "type_value_strarray9");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 6, "type_value_strarray10");

	RETURN;
}

static int test_type_value_array_nil(void)
{
	addr pos, check;

	array_va_heap(&pos, 0);
	array_build_heap(pos);
	type_value_array_nil(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "type_value_array_nil1");
	GetArrayType(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "type_value_array_nil2");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 0, "type_value_array_nil3");

	array_va_heap(&pos, 0);
	ArrayInfoStruct(pos)->adjustable = 1;
	array_build_heap(pos);
	type_value_array_nil(&pos, pos);
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

	array_va_heap(&pos, 11, 0);
	array_build_heap(pos);
	type_value_array_single(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "type_value_array_single1");
	GetArrayType(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "type_value_array_single2");
	GetArrayType(pos, 1, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "type_value_array_single3");
	test(lenarrayr(check) == 1, "type_value_array_single4");
	GetArrayA4(check, 0, &check);
	test(RefFixnum(check) == 11, "type_value_array_single5");

	array_va_heap(&pos, 11, 0);
	ArrayInfoStruct(pos)->adjustable = 1;
	array_build_heap(pos);
	type_value_array_single(&pos, pos);
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

	array_va_heap(&pos, 11, 12, 0);
	array_build_heap(pos);
	type_value_array_multiple(&pos, pos);
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

	array_va_heap(&pos, 11, 12, 0);
	ArrayInfoStruct(pos)->adjustable = 1;
	array_build_heap(pos);
	type_value_array_multiple(&pos, pos);
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

	array_va_heap(&pos, 0);
	array_build_heap(pos);
	type_value_array(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "type_value_array1");
	GetArrayType(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "type_value_array2");
	GetArrayType(pos, 1, &check);
	test(RefFixnum(check) == 0, "type_value_array3");

	array_va_heap(&pos, 11, 0);
	array_build_heap(pos);
	type_value_array(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "type_value_array4");
	GetArrayType(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "type_value_array5");
	GetArrayType(pos, 1, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "type_value_array6");
	test(lenarrayr(check) == 1, "type_value_array7");
	GetArrayA4(check, 0, &check);
	test(RefFixnum(check) == 11, "type_value_array8");

	array_va_heap(&pos, 11, 12, 0);
	array_build_heap(pos);
	type_value_array(&pos, pos);
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

static int test_type_value_vector(void)
{
	addr pos;

	vector_heap(&pos, 10);
	type_value_vector(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_VECTOR, "type_value_vector1");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 10, "type_value_vector2");

	RETURN;
}

static int test_type_value_character(void)
{
	addr pos;

	character_heap(&pos, 'A');
	type_value_character(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_STANDARD_CHAR, "type_value_character1");

	character_heap(&pos, 0x0D);
	type_value_character(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_BASE_CHAR, "type_value_character2");

	character_heap(&pos, 0xF0000000);
	type_value_character(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_EXTENDED_CHAR, "type_value_character3");

	RETURN;
}

static int test_type_value_strvect(void)
{
	addr pos, check;

	strvect_char_heap(&pos, "Hello");
	type_value_strvect(&check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_BASE_STRING, "type_value_strvect1");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_strvect2");

	strvect_setc(pos, 2, 0x0D);
	type_value_strvect(&check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_BASE_STRING, "type_value_strvect3");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_strvect4");

	strvect_setc(pos, 3, 0xF0000000);
	type_value_strvect(&check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_STRING, "type_value_strvect5");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_strvect6");

	RETURN;
}

static int test_type_value_string(void)
{
	addr pos, check;

	strvect_char_heap(&pos, "Hello");
	type_value_string(&check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_BASE_STRING, "type_value_string1");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_string2");

	strarray_char_heap(&pos, "Hello");
	type_value_string(&check, pos);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_BASE_STRING, "type_value_string3");
	GetArrayType(check, 0, &check);
	test(RefFixnum(check) == 5, "type_value_string4");

	RETURN;
}

static int test_type_value_hashtable(void)
{
	addr x;

	hashtable_heap(&x);
	type_value(&x, x);
	test(GetType(x) == LISPTYPE_TYPE, "type_value_hashtable1");
	test(LispDecl(x) == LISPDECL_HASH_TABLE, "type_value_hashtable2");

	RETURN;
}

static int test_type_value_readtable(void)
{
	addr x;

	GetConst(SPECIAL_READTABLE, &x);
	getspecialcheck_local(Execute_Thread, x, &x);
	type_value(&x, x);
	test(GetType(x) == LISPTYPE_TYPE, "type_value_readtable1");
	test(LispDecl(x) == LISPDECL_READTABLE, "type_value_readtable2");

	RETURN;
}

static int test_type_value_symbol(void)
{
	addr pos;

	interncommon("CAR", &pos);
	type_value_symbol(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SYMBOL, "type_value_symbol1");

	internchar_keyword("START", &pos);
	type_value_symbol(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_KEYWORD, "type_value_symbol2");

	RETURN;
}

static int test_type_value_integer(void)
{
	addr pos, check;

	fixnum_heap(&pos, 10);
	type_value_integer(&pos, pos);
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
	type_value_rational(&pos, pos);
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

static int test_type_value_single(void)
{
	addr pos, check;

	single_float_heap(&pos, 10.0f);
	type_value(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_SINGLE_FLOAT, "type_value_single1");
	GetArrayType(pos, 0, &check);
	test(check == Nil, "type_value_single2");
	GetArrayType(pos, 1, &check);
	test(RefSingleFloat(check) == 10.0f, "type_value_single3");
	GetArrayType(pos, 2, &check);
	test(check == Nil, "type_value_single4");
	GetArrayType(pos, 3, &check);
	test(RefSingleFloat(check) == 10.0f, "type_value_single5");

	RETURN;
}

static int test_type_value_double(void)
{
	addr pos, check;

	double_float_heap(&pos, 10.0);
	type_value(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_DOUBLE_FLOAT, "type_value_double1");
	GetArrayType(pos, 0, &check);
	test(check == Nil, "type_value_double2");
	GetArrayType(pos, 1, &check);
	test(RefDoubleFloat(check) == 10.0, "type_value_double3");
	GetArrayType(pos, 2, &check);
	test(check == Nil, "type_value_double4");
	GetArrayType(pos, 3, &check);
	test(RefDoubleFloat(check) == 10.0, "type_value_double5");

	RETURN;
}

static int test_type_value_long(void)
{
	addr pos, check;

	long_float_heap(&pos, 10.0L);
	type_value(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_LONG_FLOAT, "type_value_long1");
	GetArrayType(pos, 0, &check);
	test(check == Nil, "type_value_long2");
	GetArrayType(pos, 1, &check);
	test(RefLongFloat(check) == 10.0L, "type_value_long3");
	GetArrayType(pos, 2, &check);
	test(check == Nil, "type_value_long4");
	GetArrayType(pos, 3, &check);
	test(RefLongFloat(check) == 10.0L, "type_value_long5");

	RETURN;
}

static int test_type_value_float(void)
{
	addr pos, check;

	single_float_heap(&pos, 10.2f);
	type_value_float(&pos, pos);
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
	type_value_float(&pos, pos);
	test(RefLispDecl(pos) == LISPDECL_DOUBLE_FLOAT, "type_value_float6");

	RETURN;
}

static int test_type_value_complex(void)
{
	addr x, y;

	x = readr("#c(10 20)");
	type_value(&x, x);
	test(LispDecl(x) == LISPDECL_COMPLEX, "type_value_complex1");
	GetArrayType(x, 0, &y);
	test(LispDecl(y) == LISPDECL_OR, "type_value_complex2");

	RETURN;
}

static int test_type_value_function(void)
{
	addr x;

	GetConst(COMMON_CAR, &x);
	getfunctioncheck_local(Execute_Thread, x, &x);
	type_value(&x, x);
	test(LispDecl(x) == LISPDECL_COMPILED_FUNCTION, "type_value_function1");

	RETURN;
}

static int test_type_value_package(void)
{
	addr x;

	GetConst(SPECIAL_PACKAGE, &x);
	getspecialcheck_local(Execute_Thread, x, &x);
	type_value(&x, x);
	test(LispDecl(x) == LISPDECL_PACKAGE, "type_value_package1");

	RETURN;
}

static int test_type_value_random_state(void)
{
	addr x;

	GetConst(SPECIAL_RANDOM_STATE, &x);
	getspecialcheck_local(Execute_Thread, x, &x);
	type_value(&x, x);
	test(LispDecl(x) == LISPDECL_RANDOM_STATE, "type_value_random_state1");

	RETURN;
}

static int test_type_value_pathname(void)
{
	addr x;

	x = readr("#p\"test/\"");
	type_value(&x, x);
	test(LispDecl(x) == LISPDECL_PATHNAME, "type_value_pathname1");

	RETURN;
}

static int test_type_value_stream(void)
{
	addr x, y;

	/* broadcast */
	open_broadcast_stream(&x, Nil);
	type_value(&x, x);
	test(LispDecl(x) == LISPDECL_BROADCAST_STREAM, "type_value_stream1");

	/* concatenated */
	open_concatenated_stream(&x, Nil);
	type_value(&x, x);
	test(LispDecl(x) == LISPDECL_CONCATENATED_STREAM, "type_value_stream2");

	/* echo */
	open_concatenated_stream(&y, Nil);
	open_echo_stream(&x, y, y);
	type_value(&x, x);
	test(LispDecl(x) == LISPDECL_ECHO_STREAM, "type_value_stream3");

	/* string */
	strvect_char_heap(&x, "Hello");
	open_input_string_stream(&x, x);
	type_value(&x, x);
	test(LispDecl(x) == LISPDECL_STRING_STREAM, "type_value_stream4");

	/* synonym */
	GetConst(SYSTEM_STANDARD_INPUT, &x);
	open_synonym_stream(&x, x);
	type_value(&x, x);
	test(LispDecl(x) == LISPDECL_SYNONYM_STREAM, "type_value_stream5");

	/* two-way */
	GetConst(SYSTEM_STANDARD_INPUT, &x);
	getspecialcheck_local(Execute_Thread, x, &x);
	GetConst(SYSTEM_STANDARD_OUTPUT, &y);
	getspecialcheck_local(Execute_Thread, y, &y);
	open_twoway_stream(&x, x, y);
	type_value(&x, x);
	test(LispDecl(x) == LISPDECL_TWO_WAY_STREAM, "type_value_stream6");

	/* file */
	strvect_char_heap(&y, "test/empty.file");
	open_input_stream_error(Execute_Thread, &y, y);
	type_value(&x, y);
	test(LispDecl(x) == LISPDECL_FILE_STREAM, "type_value_stream7");

	RETURN;
}

static int test_type_value_bitvector(void)
{
	addr x;

	x = readr("#*10011");
	type_value(&x, x);
	test(LispDecl(x) == LISPDECL_SIMPLE_BIT_VECTOR, "type_value_bitvector1");
	GetArrayType(x, 0, &x);
	test(RefFixnum(x) == 5, "type_value_bitvector2");

	RETURN;
}


/*
 *  main
 */
static int testbreak_type_value(void)
{
	TestBreak(test_type_value_nil);
	TestBreak(test_type_value_t);
	TestBreak(test_type_value_nil_call);
	TestBreak(test_type_value_t_call);
	TestBreak(test_type_value_type);
	TestBreak(test_type_value_clos);
	TestBreak(test_type_value_cons);
	TestBreak(test_type_value_strarray);
	TestBreak(test_type_value_array_nil);
	TestBreak(test_type_value_array_single);
	TestBreak(test_type_value_array_multiple);
	TestBreak(test_type_value_array);
	TestBreak(test_type_value_vector);
	TestBreak(test_type_value_character);
	TestBreak(test_type_value_strvect);
	TestBreak(test_type_value_string);
	TestBreak(test_type_value_hashtable);
	TestBreak(test_type_value_readtable);
	TestBreak(test_type_value_symbol);
	TestBreak(test_type_value_integer);
	TestBreak(test_type_value_rational);
	TestBreak(test_type_value_single);
	TestBreak(test_type_value_double);
	TestBreak(test_type_value_long);
	TestBreak(test_type_value_float);
	TestBreak(test_type_value_complex);
	TestBreak(test_type_value_function);
	TestBreak(test_type_value_package);
	TestBreak(test_type_value_random_state);
	TestBreak(test_type_value_pathname);
	TestBreak(test_type_value_stream);
	TestBreak(test_type_value_bitvector);

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

