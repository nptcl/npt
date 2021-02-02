#include "subtypep_table.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "copy.h"
#include "degrade.h"
#include "hashtable.h"
#include "package.h"
#include "package_intern.h"
#include "pathname.h"
#include "random_state.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "subtypep_number.h"
#include "subtypep_optimize.h"
#include "type_copy.h"
#include "type_parse.h"
#include "type_table.h"

static void test_parse_type(addr *ret, addr pos)
{
	if (parse_type(Execute_Thread, ret, pos, Nil)) {
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
	Execute ptr;

	ptr = Execute_Thread;
	/* real_extract(&left, left); */
	/* real_extract(&right, right); */
	aatype(value);
	subtypep_table_(ptr, left, right, &value);

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
static int strtable_invalid(const char *left, const char *right)
{
	return strtable_test(left, right) == SUBTYPEP_INVALID;
}


/*
 *  cons
 */
static int test_subtypep_call_cons_p(void)
{
	addr pos;

	GetTypeTable(&pos, Asterisk);
	test(subtypep_call_cons_p(pos), "subtypep_call_cons_p1");
	GetTypeTable(&pos, T);
	test(subtypep_call_cons_p(pos), "subtypep_call_cons_p2");
	GetTypeTable(&pos, Null);
	test(! subtypep_call_cons_p(pos), "subtypep_call_cons_p3");

	RETURN;
}

static int test_subtypep_call_cons_t_(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "NIL");
	parse_type_string(&right, "*");
	subtypep_call_cons_t_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_call_cons_t_1");

	parse_type_string(&left, "T");
	parse_type_string(&right, "*");
	subtypep_call_cons_t_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_call_cons_t_2");

	parse_type_string(&left, "T");
	parse_type_string(&right, "CONS");
	subtypep_call_cons_t_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_call_cons_t_3");

	parse_type_string(&left, "INTEGER");
	parse_type_string(&right, "CONS");
	subtypep_call_cons_t_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_call_cons_t_4");

	RETURN;
}

static int test_subtypep_call_cons(void)
{
	test(strtable_exclude("integer", "symbol"),
			"subtypep_cons1");
	test(strtable_true("(cons * *)", "(cons * *)"),
			"subtypep_cons2");
	test(strtable_false("(cons symbol *)", "(cons cons *)"),
			"subtypep_cons3");
	test(strtable_false("(cons integer symbol)", "(cons integer cons)"),
			"subtypep_cons4");
	test(strtable_true("(cons integer symbol)", "(cons integer symbol)"),
			"subtypep_cons5");
	test(strtable_invalid(
				"(cons (satisfies hello) symbol)",
				"(cons integer symbol)"),
			"subtypep_cons6");
	test(strtable_invalid(
				"(cons integer symbol)",
				"(cons integer (satisfies hello))"),
			"subtypep_cons7");

	RETURN;
}


/*
 *  vector
 */
static SubtypepResult subtypep_value(const char *str1, const char *str2)
{
	int ignore;
	SubtypepResult value;
	addr left, right;
	LocalRoot local;
	Execute ptr;

	ptr = Execute_Thread;
	local = ptr->local;
	aatype(value);
	parse_type_string(&left, str1);
	parse_type_string(&right, str2);
	type_optimize_heap_(local, left, &left, &ignore);
	type_optimize_heap_(local, right, &right, &ignore);
	get_type_optimized(&left, left);
	get_type_optimized(&right, right);

	subtypep_compound_(ptr, left, right, &value);
	return value;
}

#define VectorArray_true(a,b,c) { \
	test(subtypep_value((b), (c)) == SUBTYPEP_INCLUDE, "subtypep_vector_array" a); \
}
#define VectorArray_false(a,b,c) { \
	test(subtypep_value((b), (c)) == SUBTYPEP_FALSE, "subtypep_vector_array" a); \
}
static int test_subtypep_vector_array(void)
{
	VectorArray_true("1", "vector", "array");
	VectorArray_true("2", "(vector integer)", "array");
	VectorArray_true("3", "(vector integer)", "(array integer)");
	VectorArray_true("4", "(vector integer)", "(array number)");
	VectorArray_true("5", "(vector integer *)", "(array number *)");
	VectorArray_true("6", "(vector real *)", "(array t *)");
	VectorArray_true("7", "(vector real 10)", "(array t *)");
	VectorArray_false("8", "(vector real *)", "(array t 10)");
	VectorArray_true("9a", "(array * (10))", "(array * (*))");
	VectorArray_true("9b", "(vector real 10)", "(array t (*))");
	VectorArray_true("10", "(vector real 10)", "(array t 1)");
	VectorArray_true("11", "(vector real 10)", "(array t (10))");
	VectorArray_false("12", "(vector real 10)", "(array t (10 *))");
	VectorArray_false("13", "(vector real 10)", "(array t 2)");
	VectorArray_false("14", "(vector real *)", "(array t 2)");
	VectorArray_false("15", "(vector real *)", "(array t (10 20))");
	VectorArray_false("16", "(vector real *)", "(array t (* *))");

	RETURN;
}

#define SimpleVectorArray_true(a,b,c) { \
	test(subtypep_value((b), (c)) == SUBTYPEP_INCLUDE, \
			"subtypep_simple_vector_array" a); \
}
#define SimpleVectorArray_false(a,b,c) { \
	test(subtypep_value((b), (c)) == SUBTYPEP_FALSE, \
			"subtypep_simple_vector_array" a); \
}
static int test_subtypep_simple_vector_array(void)
{
	SimpleVectorArray_true("1", "simple-vector", "array");
	SimpleVectorArray_true("2", "(simple-vector)", "array");
	SimpleVectorArray_true("3", "simple-vector", "(array)");
	SimpleVectorArray_true("4", "(simple-vector)", "(array)");
	SimpleVectorArray_true("5", "(simple-vector *)", "(array)");
	SimpleVectorArray_true("6", "(simple-vector)", "(array integer)");
	SimpleVectorArray_true("7", "(simple-vector)", "(array t)");
	SimpleVectorArray_true("8", "(simple-vector)", "(array *)");
	SimpleVectorArray_true("9", "(simple-vector 10)", "(array t *)");
	SimpleVectorArray_true("10", "(simple-vector 10)", "(array t 1)");
	SimpleVectorArray_true("11", "(simple-vector 10)", "(array t (*))");
	SimpleVectorArray_true("12", "(simple-vector 10)", "(array t (10))");
	SimpleVectorArray_false("13", "(simple-vector 10)", "(array t (11))");
	SimpleVectorArray_false("14", "(simple-vector 10)", "(array t (10 11))");
	SimpleVectorArray_false("15", "(simple-vector 10)", "(array t 2)");
	SimpleVectorArray_false("16", "(simple-vector *)", "(array t 2)");
	SimpleVectorArray_false("17", "(simple-vector *)", "(array t (10))");

	test(subtypep_value("array", "simple-array") == SUBTYPEP_FALSE,
			"subtypep_simple_vector_array18");
	test(subtypep_value("cons", "simple-array") == SUBTYPEP_EXCLUDE,
			"subtypep_simple_vector_array19");

	RETURN;
}

#define StringArray_true(a,b,c) { \
	test(subtypep_value((b), (c)) == SUBTYPEP_INCLUDE, "subtypep_string_array" a); \
}
#define StringArray_false(a,b,c) { \
	test(subtypep_value((b), (c)) == SUBTYPEP_FALSE, "subtypep_string_array" a); \
}
static int test_subtypep_string_array(void)
{
	StringArray_true("1", "string", "array");
	StringArray_true("2", "(string)", "array");
	StringArray_true("3", "string", "(array)");
	StringArray_true("4", "(string)", "(array)");
	StringArray_true("5", "(string *)", "(array)");
	StringArray_false("6", "(string)", "(array integer)");
	StringArray_true("7", "(string)", "(array character)");
	StringArray_true("8", "(string)", "(array *)");
	StringArray_true("9", "(string 10)", "(array character *)");
	StringArray_true("10", "(string 10)", "(array character 1)");
	StringArray_true("11", "(string 10)", "(array character (*))");
	StringArray_true("12", "(string 10)", "(array character (10))");
	StringArray_false("13", "(string 10)", "(array character (11))");
	StringArray_false("14", "(string 10)", "(array character (10 11))");
	StringArray_false("15", "(string 10)", "(array character 2)");
	StringArray_false("16", "(string *)", "(array character 2)");
	StringArray_false("17", "(string *)", "(array character (10))");
	StringArray_true("18", "base-string", "array");
	StringArray_true("19", "simple-string", "array");
	StringArray_true("20", "simple-base-string", "array");

	RETURN;
}

#define BitVectorArray_true(a,b,c) { \
	test(subtypep_value((b), (c)) == SUBTYPEP_INCLUDE, \
			"subtypep_bit_vector_array" a); \
}
#define BitVectorArray_false(a,b,c) { \
	test(subtypep_value((b), (c)) == SUBTYPEP_FALSE, \
			"subtypep_bit_vector_array" a); \
}
static int test_subtypep_bit_vector_array(void)
{
	BitVectorArray_true("1", "bit-vector", "array");
	BitVectorArray_true("2", "(bit-vector)", "array");
	BitVectorArray_true("3", "bit-vector", "(array)");
	BitVectorArray_true("4", "(bit-vector)", "(array)");
	BitVectorArray_true("5", "(bit-vector *)", "(array)");
	BitVectorArray_true("6", "(bit-vector)", "(array (integer 0 1))");
	BitVectorArray_true("7", "(bit-vector)", "(array bit)");
	BitVectorArray_true("8", "(bit-vector)", "(array *)");
	BitVectorArray_true("9", "(bit-vector 10)", "(array bit *)");
	BitVectorArray_true("10", "(bit-vector 10)", "(array bit 1)");
	BitVectorArray_true("11", "(bit-vector 10)", "(array bit (*))");
	BitVectorArray_true("12", "(bit-vector 10)", "(array bit (10))");
	BitVectorArray_false("13", "(bit-vector 10)", "(array bit (11))");
	BitVectorArray_false("14", "(bit-vector 10)", "(array bit (10 11))");
	BitVectorArray_false("15", "(bit-vector 10)", "(array bit 2)");
	BitVectorArray_false("16", "(bit-vector *)", "(array bit 2)");
	BitVectorArray_false("17", "(bit-vector *)", "(array bit (10))");
	BitVectorArray_true("18", "bit-vector", "(array (integer 0 1))");

	RETURN;
}


/*
 *  call complex
 */
static int test_subtypep_complex(void)
{
	test(strtable_false("number", "complex"), "subtypep_complex1");
	test(strtable_true("complex", "complex"), "subtypep_complex2");
	test(strtable_exclude("real", "complex"), "subtypep_complex3");

	RETURN;
}


/*
 *  function
 */
static void parse_args(addr *ret, const char *str)
{
	addr pos;

	parse_type_string(&pos, str);
	type_optimize_throw_heap_(Local_Thread, pos, &pos);
	GetArrayType(pos, 0, ret);
}

static int test_ordinary_subtypep(void)
{
	int value;
	addr pos;
	ordargs str1, str2;
	ordtype type1, type2;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_args(&pos, "(function (atom integer))");
	make_ordargs(&str1, pos);
	parse_args(&pos, "(function (real string))");
	make_ordargs(&str2, pos);
	gettype_ordargs_(&str1, 1, &type1);
	gettype_ordargs_(&str2, 0, &type2);
	ordinary_subtypep_(ptr, &str1, &type1, &str2, &type2, &value);
	test(value, "ordinary_subtypep1");

	gettype_ordargs_(&str1, 0, &type1);
	gettype_ordargs_(&str2, 1, &type2);
	ordinary_subtypep_(ptr, &str1, &type1, &str2, &type2, &value);
	test(! value, "ordinary_subtypep2");

	RETURN;
}

static void parse_values_string(addr *ret, const char *code)
{
	readstring_debug(ret, code);
	if (parse_type_values(Execute_Thread, ret, *ret, Nil))
		Error(fmte_("parse-type-values error.", NULL));
}

static void extractchar(addr *ret, const char *str)
{
	LocalRoot local;
	addr type;

	local = Local_Thread;
	parse_values_string(&type, str);
	type_copy_local(local, &type, type);
	real_extract_local_(local, &type, type);
	get_type_subtypep(ret, type);
}

static void argschar(ordargs *ret, const char *str)
{
	addr pos;

	extractchar(&pos, str);
	GetArrayType(pos, 0, &pos);
	make_ordargs(ret, pos);
}

static int test_ordinary_size(void)
{
	int value;
	ordargs args1, args2;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	argschar(&args1, "(function (integer))");
	argschar(&args2, "(function (real))");
	ordinary_size_(ptr, &args1, &args2, 1, &value);
	test(value, "ordinary_size1");
	ordinary_size_(ptr, &args1, &args2, 2, &value);
	test(value, "ordinary_size2");

	argschar(&args1, "(function (integer integer))");
	argschar(&args2, "(function (real))");
	ordinary_size_(ptr, &args1, &args2, 2, &value);
	test(! value, "ordinary_size3");

	argschar(&args1, "(function (integer))");
	argschar(&args2, "(function (real real))");
	ordinary_size_(ptr, &args1, &args2, 2, &value);
	test(value, "ordinary_size4");

	argschar(&args1, "(function (integer string))");
	argschar(&args2, "(function (real real))");
	ordinary_size_(ptr, &args1, &args2, 2, &value);
	test(! value, "ordinary_size5");

	RETURN;
}

static void extractargs(addr *ret, const char *str)
{
	extractchar(ret, str);
	GetArrayType(*ret, 0, ret);
}

static int test_ordinary_simple(void)
{
	int value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	extractargs(&left, "(function (integer integer))");
	extractargs(&right, "(function (t real))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(value, "ordinary_simple1");

	extractargs(&left, "(function (integer))");
	extractargs(&right, "(function (t real))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(! value, "ordinary_simple2");

	extractargs(&left, "(function (integer integer))");
	extractargs(&right, "(function (t &optional real))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(value, "ordinary_simple3");

	extractargs(&left, "(function (integer &optional integer))");
	extractargs(&right, "(function (t real))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(! value, "ordinary_simple4");

	extractargs(&left, "(function (integer integer))");
	extractargs(&right, "(function (t &optional real real))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(value, "ordinary_simple5");

	extractargs(&left, "(function (integer integer &optional integer))");
	extractargs(&right, "(function (t &optional real real))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(value, "ordinary_simple6");

	extractargs(&left, "(function (integer))");
	extractargs(&right, "(function (t &optional real real))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(value, "ordinary_simple7");

	extractargs(&left, "(function (integer))");
	extractargs(&right, "(function (t t &optional real real))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(! value, "ordinary_simple8");

	RETURN;
}

static int test_ordinary_simple_left(void)
{
	int value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	extractargs(&left, "(function (integer integer))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(value, "ordinary_simple_left1");

	extractargs(&left, "(function (integer))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(! value, "ordinary_simple_left2");

	extractargs(&left, "(function (integer integer integer))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(value, "ordinary_simple_left3");

	extractargs(&left, "(function (integer integer integer fixnum fixnum))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(value, "ordinary_simple_left4");

	extractargs(&left, "(function (integer integer integer fixnum string))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(! value, "ordinary_simple_left5");

	RETURN;
}

static int test_ordinary_check(void)
{
	int value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	extractargs(&left, "(function (integer integer &rest integer))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(value, "ordinary_check1");

	extractargs(&left, "(function (integer integer &rest string))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(! value, "ordinary_check2");

	RETURN;
}

static int test_subtypep_function_ordinary(void)
{
	int value;
	addr left, right, aster;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	GetTypeTable(&aster, Asterisk);
	extractargs(&left, "(function (integer integer &rest integer))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(value, "subtypep_function_ordinary1");

	subtypep_function_ordinary_(ptr, aster, aster, &value);
	test(value, "subtypep_function_ordinary2");
	subtypep_function_ordinary_(ptr, left, aster, &value);
	test(value, "subtypep_function_ordinary3");
	subtypep_function_ordinary_(ptr, aster, right, &value);
	test(! value, "subtypep_function_ordinary4");

	extractargs(&left, "(function (integer integer &rest string))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(ptr, left, right, &value);
	test(! value, "subtypep_function_ordinary5");

	RETURN;
}

static int test_subtypep_function_check(void)
{
	SubtypepResult result;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(result);

	extractchar(&left, "(function (fixnum) cons)");
	extractchar(&right, "(function (integer) list)");
	subtypep_function_check_(ptr, left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_function_check1");

	extractchar(&left, "(function * cons)");
	extractchar(&right, "(function (integer) list)");
	subtypep_function_check_(ptr, left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_function_check2");

	extractchar(&left, "(function * string)");
	extractchar(&right, "(function * list)");
	subtypep_function_check_(ptr, left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_function_check3");

	extractchar(&left, "(function * real)");
	extractchar(&right, "(function * integer)");
	subtypep_function_check_(ptr, left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_function_check4");

	extractchar(&left, "(function * *)");
	extractchar(&right, "(function * *)");
	subtypep_function_check_(ptr, left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_function_check5");

	RETURN;
}

static int test_subtypep_call_function(void)
{
	SubtypepResult result;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(result);

	extractchar(&left, "(function (fixnum) cons)");
	extractchar(&right, "(function (integer) list)");
	subtypep_call_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_call_function.1");

	extractchar(&left, "(compiled-function (fixnum) cons)");
	extractchar(&right, "(function (integer) list)");
	subtypep_call_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_call_function.2");

	extractchar(&left, "(compiled-function (string) cons)");
	extractchar(&right, "(function (integer) list)");
	subtypep_call_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_call_function.3");

	extractchar(&left, "integer");
	extractchar(&right, "(function (integer) list)");
	subtypep_call_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_EXCLUDE, "subtypep_call_function.4");

	RETURN;
}

static int test_subtypep_call_compiled_function(void)
{
	SubtypepResult result;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(result);

	extractchar(&left, "(compiled-function (fixnum) cons)");
	extractchar(&right, "(compiled-function (integer) list)");
	subtypep_call_compiled_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_call_compiled_function.1");

	extractchar(&left, "(function (fixnum) cons)");
	extractchar(&right, "(compiled-function (integer) list)");
	subtypep_call_compiled_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_call_compiled_function.2");

	extractchar(&left, "(compiled-function (string) cons)");
	extractchar(&right, "(compiled-function (integer) list)");
	subtypep_call_compiled_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_call_compiled_function.3");

	extractchar(&left, "integer");
	extractchar(&right, "(compiled-function (integer) list)");
	subtypep_call_compiled_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_EXCLUDE, "subtypep_call_compiled_function.4");

	RETURN;
}


/*
 *  main
 */
static int testcase_subtypep_table(void)
{
	/* subtypep-table */
	TestBreak(test_subtypep_call_cons_p);
	TestBreak(test_subtypep_call_cons_t_);
	TestBreak(test_subtypep_call_cons);
	TestBreak(test_subtypep_vector_array);
	TestBreak(test_subtypep_simple_vector_array);
	TestBreak(test_subtypep_string_array);
	TestBreak(test_subtypep_bit_vector_array);
	TestBreak(test_subtypep_complex);
	/* function */
	TestBreak(test_ordinary_subtypep);
	TestBreak(test_ordinary_size);
	TestBreak(test_ordinary_simple);
	TestBreak(test_ordinary_simple_left);
	TestBreak(test_ordinary_check);
	TestBreak(test_subtypep_function_ordinary);
	TestBreak(test_subtypep_function_check);
	TestBreak(test_subtypep_call_function);
	TestBreak(test_subtypep_call_compiled_function);

	return 0;
}

static void testinit_subtypep_table(Execute ptr)
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

int test_subtypep_table(void)
{
	DegradeTitle;
	return DegradeCode(subtypep_table);
}

