#include "type_object.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "eval_declare.h"
#include "equal.h"
#include "degrade.h"
#include "package.h"
#include "pathname.h"
#include "readtable.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_copy.h"
#include "type_parse.h"
#include "type_table.h"

static int check_type_object(addr x, const char *str)
{
	addr y;

	type_object(&x, x);
	y = readr(str);
	return equal(x, y);
}

static int test_type_object_optimized(void)
{
	addr x;

	GetTypeTable(&x, Atom);
	type1_heap(LISPDECL_OPTIMIZED, x, &x);
	test(check_type_object(x, "atom"), "type_object_optimized1");

	RETURN;
}

static int test_type_object_subtypep(void)
{
	addr x;

	GetTypeTable(&x, Atom);
	type1_heap(LISPDECL_SUBTYPEP, x, &x);
	test(check_type_object(x, "atom"), "type_object_subtypep1");

	RETURN;
}

static int test_type_object_clos(void)
{
	addr x;

	GetConst(COMMON_STANDARD_CLASS, &x);
	clos_find_class(x, &x);
	type_clos_heap(x, &x);
	test(check_type_object(x, "standard-class"), "type_object_clos1");

	RETURN;
}

static int test_type_object_asterisk(void)
{
	addr x;

	GetTypeTable(&x, Asterisk);
	test(check_type_object(x, "*"), "type_object_asterisk1");

	RETURN;
}

static int check_type_objectc(const char *str)
{
	addr x, y;

	x = readr(str);
	parse_type_unsafe(&y, x);
	type_object(&y, y);

	return equal(x, y);
}

static int test_type_object_name(void)
{
	test(check_type_objectc("atom"), "type_object_bit_atom");
	test(check_type_objectc("list"), "type_object_bit_list");
	test(check_type_objectc("boolean"), "type_object_bit_boolean");
	test(check_type_objectc("extended-char"), "type_object_extended_char");
	test(check_type_objectc("bit"), "type_object_bit");
	test(check_type_objectc("fixnum"), "type_object_fixnum");
	test(check_type_objectc("bignum"), "type_object_bignum");
	test(check_type_objectc("nil"), "type_object_nil");
	test(check_type_objectc("t"), "type_object_t");
	test(check_type_objectc("null"), "type_object_null");
	test(check_type_objectc("hash-table"), "type_object_hash_table");
	test(check_type_objectc("symbol"), "type_object_symbol");
	test(check_type_objectc("keyword"), "type_object_keyword");
	test(check_type_objectc("package"), "type_object_package");
	test(check_type_objectc("random-state"), "type_object_random_state");
	test(check_type_objectc("readtable"), "type_object_readtable");
	test(check_type_objectc("pathname"), "type_object_pathname");
	test(check_type_objectc("logical-pathname"), "type_object_logical_pathname");
	test(check_type_objectc("sequence"), "type_object_sequence");
	test(check_type_objectc("character"), "type_object_character");
	test(check_type_objectc("base-char"), "type_object_base_char");
	test(check_type_objectc("standard-char"), "type_object_standard_char");
	test(check_type_objectc("number"), "type_object_number");
	test(check_type_objectc("restart"), "type_object_restart");
	test(check_type_objectc("stream"), "type_object_stream");
	test(check_type_objectc("broadcast-stream"), "type_object_broadcast_stream");
	test(check_type_objectc("concatenated-stream"), "type_object_concatenated_stream");
	test(check_type_objectc("echo-stream"), "type_object_echo_stream");
	test(check_type_objectc("file-stream"), "type_object_file_stream");
	test(check_type_objectc("string-stream"), "type_object_string_stream");
	test(check_type_objectc("synonym-stream"), "type_object_synonym_stream");
	test(check_type_objectc("two-way-stream"), "type_object_two_way_stream");
	RETURN;
}


/*
 *  Compound-type
 */
static int test_type_object_and(void)
{
	test(check_type_objectc("(and string integer fixnum)"), "type_object_and1");
	RETURN;
}

static int test_type_object_or(void)
{
	test(check_type_objectc("(or string integer fixnum)"), "type_object_or1");
	RETURN;
}

static int test_type_object_eql(void)
{
	test(check_type_objectc("(eql 100)"), "type_object_eql1");
	RETURN;
}

static int test_type_object_member(void)
{
	test(check_type_objectc("(member 10 20 #\\a)"), "type_object_member1");
	RETURN;
}

static int test_type_object_mod(void)
{
	test(check_type_objectc("(mod 10)"), "type_object_mod1");
	RETURN;
}

static int test_type_object_not(void)
{
	test(check_type_objectc("(not integer)"), "type_object_not1");
	RETURN;
}

static int test_type_object_satisfies(void)
{
	test(check_type_objectc("(satisfies hello)"), "type_object_satisfies1");
	RETURN;
}

static void parse_type_values_unsafe(addr *ret, addr pos)
{
	if (parse_type_values(Execute_Thread, ret, pos, Nil))
		_fmte("parse-type-values error.", NULL);
}

static int test_type_object_values(void)
{
	addr x, y;

	x = readr("(values)");
	parse_type_values_unsafe(&x, x);
	type_object(&x, x);
	y = readr("(values &rest t)");
	test(equal(x, y), "type_object_values1");

	x = readr("(values fixnum integer symbol)");
	parse_type_values_unsafe(&x, x);
	type_object(&x, x);
	y = readr("(values fixnum integer symbol &rest t)");
	test(equal(x, y), "type_object_values2");

	x = readr("(values &optional fixnum integer symbol)");
	parse_type_values_unsafe(&x, x);
	type_object(&x, x);
	y = readr("(values &optional fixnum integer symbol &rest t)");
	test(equal(x, y), "type_object_values3");

	x = readr("(values &optional fixnum integer &rest symbol)");
	parse_type_values_unsafe(&x, x);
	type_object(&x, x);
	y = readr("(values &optional fixnum integer &rest symbol)");
	test(equal(x, y), "type_object_values4");

	x = readr("(values fixnum &optional integer &rest symbol)");
	parse_type_values_unsafe(&x, x);
	type_object(&x, x);
	y = readr("(values fixnum &optional integer &rest symbol)");
	test(equal(x, y), "type_object_values5");

	RETURN;
}


/*
 *  Extract-type
 */
static int test_type_object_boolean(void)
{
	test(check_type_objectc("boolean"), "type_object_boolean1");
	RETURN;
}

static int test_type_object_vector(void)
{
	test(check_type_objectc("vector"), "type_object_vector1");
	test(check_type_objectc("(vector t *)"), "type_object_vector2");
	test(check_type_objectc("(vector * 20)"), "type_object_vector3");
	test(check_type_objectc("(vector character 20)"), "type_object_vector4");
	RETURN;
}

static int test_type_object_simple_vector(void)
{
	test(check_type_objectc("simple-vector"), "type_object_simple_vector1");
	test(check_type_objectc("(simple-vector 20)"), "type_object_simple_vector2");
	RETURN;
}

static int test_type_object_bit_vector(void)
{
	test(check_type_objectc("bit-vector"), "type_object_bit_vector1");
	test(check_type_objectc("(bit-vector 20)"), "type_object_bit_vector2");
	RETURN;
}

static int test_type_object_simple_bit_vector(void)
{
	test(check_type_objectc("simple-bit-vector"),
			"type_object_simple_bit_vector1");
	test(check_type_objectc("(simple-bit-vector 20)"),
			"type_object_simple_bit_vector2");
	RETURN;
}

static int test_type_object_string(void)
{
	test(check_type_objectc("string"), "type_object_string1");
	test(check_type_objectc("(string 20)"), "type_object_string2");
	RETURN;
}

static int test_type_object_base_string(void)
{
	test(check_type_objectc("base-string"), "type_object_base_string1");
	test(check_type_objectc("(base-string 20)"), "type_object_base_string2");
	RETURN;
}

static int test_type_object_simple_string(void)
{
	test(check_type_objectc("simple-string"), "type_object_simple_string1");
	test(check_type_objectc("(simple-string 20)"), "type_object_simple_string2");
	RETURN;
}

static int test_type_object_simple_base_string(void)
{
	test(check_type_objectc("simple-base-string"),
			"type_object_simple_base_string1");
	test(check_type_objectc("(simple-base-string 20)"),
			"type_object_simple_base_string2");
	RETURN;
}

static int test_type_object_signed_byte(void)
{
	test(check_type_objectc("signed-byte"), "type_object_signed_byte1");
	test(check_type_objectc("(signed-byte 20)"), "type_object_signed_byte2");
	RETURN;
}

static int test_type_object_unsigned_byte(void)
{
	test(check_type_objectc("unsigned-byte"), "type_object_unsigned_byte1");
	test(check_type_objectc("(unsigned-byte 20)"), "type_object_unsigned_byte2");
	RETURN;
}


/*
 *  Atomic-type
 */
static int test_type_object_cons(void)
{
	test(check_type_objectc("cons"), "type_object_cons1");
	test(check_type_objectc("(cons integer *)"), "type_object_cons2");
	test(check_type_objectc("(cons * integer)"), "type_object_cons3");
	test(check_type_objectc("(cons fixnum integer)"), "type_object_cons4");
	RETURN;
}

static int test_type_object_function(void)
{
	test(check_type_objectc("function"),
			"type_object_function1");
	test(check_type_objectc("(function () *)"),
			"type_object_function2");
	test(check_type_objectc("(function (atom) *)"),
			"type_object_function3");
	test(check_type_objectc("(function (atom integer) *)"),
			"type_object_function4");
	test(check_type_objectc("(function () (values integer &rest t))"),
			"type_object_function5");
	test(check_type_objectc("(function (&optional integer atom) *)"),
			"type_object_function6");
	test(check_type_objectc("(function (&rest integer) *)"),
			"type_object_function7");
	test(check_type_objectc("(function (&key (hello integer) (aaa fixnum)) *)"),
			"type_object_function8");
	test(check_type_objectc("(function "
				"(integer &optional fixnum &key (hello integer) (aaa fixnum))"
				"*)"),
			"type_object_function9");
	RETURN;
}

static int test_type_object_compiled_function(void)
{
	test(check_type_objectc("compiled-function"),
			"type_object_compiled_function1");
	test(check_type_objectc("(compiled-function () *)"),
			"type_object_compiled_function2");
	test(check_type_objectc("(compiled-function (atom) *)"),
			"type_object_compiled_function3");
	test(check_type_objectc("(compiled-function (atom integer) *)"),
			"type_object_compiled_function4");
	test(check_type_objectc("(compiled-function () (values integer &rest t))"),
			"type_object_compiled_function5");
	test(check_type_objectc("(compiled-function (&optional integer atom) *)"),
			"type_object_compiled_function6");
	test(check_type_objectc("(compiled-function (&rest integer) *)"),
			"type_object_compiled_function7");
	test(check_type_objectc("(compiled-function (&key (hello integer) (aaa fixnum)) *)"),
			"type_object_compiled_function8");
	test(check_type_objectc("(compiled-function "
				"(integer &optional fixnum &key (hello integer) (aaa fixnum))"
				"*)"),
			"type_object_compiled_function9");
	RETURN;
}

static int test_type_object_array(void)
{
	test(check_type_objectc("array"), "type_object_array1");
	test(check_type_objectc("(array t *)"), "type_object_array2");
	test(check_type_objectc("(array character 5)"), "type_object_array3");
	test(check_type_objectc("(array t (4))"), "type_object_array4");
	test(check_type_objectc("(array t (1 2 3))"), "type_object_array5");
	RETURN;
}

static int test_type_object_simple_array(void)
{
	test(check_type_objectc("simple-array"), "type_object_simple_array1");
	test(check_type_objectc("(simple-array t *)"), "type_object_simple_array2");
	test(check_type_objectc("(simple-array character 5)"), "type_object_simple_array3");
	test(check_type_objectc("(simple-array t (4))"), "type_object_simple_array4");
	test(check_type_objectc("(simple-array t (1 2 3))"), "type_object_simple_array5");
	RETURN;
}

static int test_type_object_real(void)
{
	test(check_type_objectc("real"), "type_object_real1");
	test(check_type_objectc("(real 10 20)"), "type_object_real2");
	test(check_type_objectc("(real (10) 20)"), "type_object_real3");
	test(check_type_objectc("(real 10 (20))"), "type_object_real4");
	test(check_type_objectc("(real * 20)"), "type_object_real5");
	test(check_type_objectc("(real (10) *)"), "type_object_real6");
	RETURN;
}

static int test_type_object_rational(void)
{
	test(check_type_objectc("rational"), "type_object_rational1");
	test(check_type_objectc("(rational 10 20)"), "type_object_rational2");
	test(check_type_objectc("(rational (10) 20)"), "type_object_rational3");
	test(check_type_objectc("(rational 10 (20))"), "type_object_rational4");
	test(check_type_objectc("(rational * 20)"), "type_object_rational5");
	test(check_type_objectc("(rational (10) *)"), "type_object_rational6");
	RETURN;
}

static int test_type_object_integer(void)
{
	test(check_type_objectc("integer"), "type_object_integer1");
	test(check_type_objectc("(integer 10 20)"), "type_object_integer2");
	test(check_type_objectc("(integer (10) 20)"), "type_object_integer3");
	test(check_type_objectc("(integer 10 (20))"), "type_object_integer4");
	test(check_type_objectc("(integer * 20)"), "type_object_integer5");
	test(check_type_objectc("(integer (10) *)"), "type_object_integer6");
	RETURN;
}

static int test_type_object_float(void)
{
	test(check_type_objectc("float"), "type_object_float1");
	test(check_type_objectc("(float 10.0 20.0)"), "type_object_float2");
	test(check_type_objectc("(float (10.0) 20.0)"), "type_object_float3");
	test(check_type_objectc("(float 10.0 (20.0))"), "type_object_float4");
	test(check_type_objectc("(float * 20.0)"), "type_object_float5");
	test(check_type_objectc("(float (10.0) *)"), "type_object_float6");
	RETURN;
}

static int test_type_object_single_float(void)
{
	test(check_type_objectc("single-float"),
			"type_object_single_float1");
	test(check_type_objectc("(single-float 10.0 20.0)"),
			"type_object_single_float2");
	test(check_type_objectc("(single-float (10.0) 20.0)"),
			"type_object_single_float3");
	test(check_type_objectc("(single-float 10.0 (20.0))"),
			"type_object_single_float4");
	test(check_type_objectc("(single-float * 20.0)"),
			"type_object_single_float5");
	test(check_type_objectc("(single-float (10.0) *)"),
			"type_object_single_float6");
	RETURN;
}

static int test_type_object_complex(void)
{
	test(check_type_objectc("complex"), "type_object_complex1");
	test(check_type_objectc("(complex integer)"), "type_object_complex2");
	RETURN;
}

static int test_type_object_call(void)
{
	addr x;

	GetTypeTable(&x, Atom);
	type_object(&x, x);
	test(equal(x, readr("atom")), "type_object1");

	GetTypeTable(&x, Atom);
	type_copy_heap(&x, x);
	type_setnotdecl(x, 1);
	type_object(&x, x);
	test(equal(x, readr("(not atom)")), "type_object2");

	RETURN;
}


/*
 *  main
 */
static int testbreak_type_object(void)
{
	TestBreak(test_type_object_optimized);
	TestBreak(test_type_object_subtypep);
	TestBreak(test_type_object_clos);
	TestBreak(test_type_object_asterisk);
	TestBreak(test_type_object_name);
	/* Compound-type */
	TestBreak(test_type_object_and);
	TestBreak(test_type_object_or);
	TestBreak(test_type_object_eql);
	TestBreak(test_type_object_member);
	TestBreak(test_type_object_mod);
	TestBreak(test_type_object_not);
	TestBreak(test_type_object_satisfies);
	TestBreak(test_type_object_values);
	/* Extract-type */
	TestBreak(test_type_object_boolean);
	TestBreak(test_type_object_vector);
	TestBreak(test_type_object_simple_vector);
	TestBreak(test_type_object_bit_vector);
	TestBreak(test_type_object_simple_bit_vector);
	TestBreak(test_type_object_string);
	TestBreak(test_type_object_base_string);
	TestBreak(test_type_object_simple_string);
	TestBreak(test_type_object_simple_base_string);
	TestBreak(test_type_object_signed_byte);
	TestBreak(test_type_object_unsigned_byte);
	/* Atomic-type */
	TestBreak(test_type_object_cons);
	TestBreak(test_type_object_function);
	TestBreak(test_type_object_compiled_function);
	TestBreak(test_type_object_array);
	TestBreak(test_type_object_simple_array);
	TestBreak(test_type_object_real);
	TestBreak(test_type_object_rational);
	TestBreak(test_type_object_integer);
	TestBreak(test_type_object_float);
	TestBreak(test_type_object_single_float);
	TestBreak(test_type_object_complex);
	TestBreak(test_type_object_call);

	return 0;
}

int test_type_object(void)
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
		result = testbreak_type_object();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

