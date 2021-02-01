#include "type_subtypep.c"
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
#include "type_optimize.h"
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
	subtypep_array_(ptr, left, right, &value);

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
static SubtypepResult subtypep_value(const char *str1, const char *str2, int asterisk)
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

	subtypep_call_(ptr, left, right, asterisk, &value);
	return value;
}

#define VectorArray_true(a,b,c) { \
	test(subtypep_value((b), (c), 0) == SUBTYPEP_INCLUDE, "subtypep_vector_array" a); \
}
#define VectorArray_false(a,b,c) { \
	test(subtypep_value((b), (c), 0) == SUBTYPEP_FALSE, "subtypep_vector_array" a); \
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
	test(subtypep_value((b), (c), 1) == SUBTYPEP_INCLUDE, \
			"subtypep_simple_vector_array" a); \
}
#define SimpleVectorArray_false(a,b,c) { \
	test(subtypep_value((b), (c), 1) == SUBTYPEP_FALSE, \
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

	test(subtypep_value("array", "simple-array", 0) == SUBTYPEP_FALSE,
			"subtypep_simple_vector_array18");
	test(subtypep_value("cons", "simple-array", 0) == SUBTYPEP_EXCLUDE,
			"subtypep_simple_vector_array19");

	RETURN;
}

#define StringArray_true(a,b,c) { \
	test(subtypep_value((b), (c), 1) == SUBTYPEP_INCLUDE, "subtypep_string_array" a); \
}
#define StringArray_false(a,b,c) { \
	test(subtypep_value((b), (c), 1) == SUBTYPEP_FALSE, "subtypep_string_array" a); \
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
	test(subtypep_value((b), (c), 1) == SUBTYPEP_INCLUDE, \
			"subtypep_bit_vector_array" a); \
}
#define BitVectorArray_false(a,b,c) { \
	test(subtypep_value((b), (c), 1) == SUBTYPEP_FALSE, \
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
	parse_type_string(ret, str);
	GetArrayType(*ret, 0, ret);
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

	local = Local_Thread;
	parse_values_string(ret, str);
	real_extract_subtypep_(local, ret, *ret);
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

static int test_subtypep_function(void)
{
	SubtypepResult result;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(result);

	extractchar(&left, "(function (fixnum) cons)");
	extractchar(&right, "(function (integer) list)");
	subtypep_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_function1");

	extractchar(&left, "(compiled-function (fixnum) cons)");
	extractchar(&right, "(function (integer) list)");
	subtypep_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_function2");

	extractchar(&left, "(compiled-function (string) cons)");
	extractchar(&right, "(function (integer) list)");
	subtypep_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_function3");

	extractchar(&left, "integer");
	extractchar(&right, "(function (integer) list)");
	subtypep_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_EXCLUDE, "subtypep_function4");

	RETURN;
}

static int test_subtypep_compiled_function(void)
{
	SubtypepResult result;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(result);

	extractchar(&left, "(compiled-function (fixnum) cons)");
	extractchar(&right, "(compiled-function (integer) list)");
	subtypep_compiled_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_compiled_function1");

	extractchar(&left, "(function (fixnum) cons)");
	extractchar(&right, "(compiled-function (integer) list)");
	subtypep_compiled_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_compiled_function2");

	extractchar(&left, "(compiled-function (string) cons)");
	extractchar(&right, "(compiled-function (integer) list)");
	subtypep_compiled_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_compiled_function3");

	extractchar(&left, "integer");
	extractchar(&right, "(compiled-function (integer) list)");
	subtypep_compiled_function_(ptr, left, right, &result);
	test(result == SUBTYPEP_EXCLUDE, "subtypep_compiled_function4");

	RETURN;
}


/*
 *  subtypep_lisptype
 */
static void parse_type_string_not(addr *ret, const char *code)
{
	readstring_debug(ret, code);
	test_parse_type(ret, *ret);
	type_copy_heap(ret, *ret);
	SetNotDecl(*ret, 1);
}

static int test_subtypep_lisptype_normal(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_lisptype_normal_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype_normal1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_lisptype_normal_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_lisptype_normal2");

	parse_type_string(&left, "real");
	parse_type_string(&right, "integer");
	subtypep_lisptype_normal_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_normal3");

	parse_type_string(&left, "integer");
	parse_type_string_not(&right, "real");
	subtypep_lisptype_normal_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_lisptype_normal4");

	parse_type_string(&left, "integer");
	parse_type_string_not(&right, "symbol");
	subtypep_lisptype_normal_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype_normal5");

	parse_type_string(&left, "real");
	parse_type_string_not(&right, "integer");
	subtypep_lisptype_normal_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_normal6");

	RETURN;
}

static int test_subtypep_lisptype_not(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string_not(&left, "real");
	parse_type_string(&right, "integer");
	subtypep_lisptype_not_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_lisptype_not1");

	parse_type_string_not(&left, "real");
	parse_type_string_not(&right, "integer");
	subtypep_lisptype_not_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype_not2");

	parse_type_string_not(&left, "real");
	parse_type_string(&right, "symbol");
	subtypep_lisptype_not_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_not3");

	parse_type_string_not(&left, "real");
	parse_type_string_not(&right, "symbol");
	subtypep_lisptype_not_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_not4");

	parse_type_string_not(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_lisptype_not_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_not5");

	parse_type_string_not(&left, "integer");
	parse_type_string_not(&right, "real");
	subtypep_lisptype_not_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_not6");

	RETURN;
}

static int test_subtypep_lisptype(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_lisptype_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_lisptype_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_lisptype2");

	parse_type_string(&left, "integer");
	parse_type_string_not(&right, "symbol");
	subtypep_lisptype_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype3");

	parse_type_string_not(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_lisptype_(ptr, left, right, &value, subtypep_array_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype4");

	RETURN;
}


/*
 *  subtypep_eql
 */
static void test_eql_character(addr *ret, unicode u, int notp)
{
	addr left, pos;

	interncommon_debug("EQL", &left);
	character_heap(&pos, u);
	list_heap(&left, left, pos, NULL);
	test_parse_type(&left, left);
	SetNotDecl(left, notp);
	*ret = left;
}

static int test_subtypep_eql_eql(void)
{
	SubtypepResult value;
	addr left, right;

	aatype(value);

	test_eql_character(&left, 'a', 0);
	test_eql_character(&right, 'a', 0);
	subtypep_eql_eql_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql_eql1");

	test_eql_character(&left, 'b', 0);
	test_eql_character(&right, 'a', 0);
	subtypep_eql_eql_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_eql_eql2");

	RETURN;
}

static int test_subtypep_eql_type(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	test_eql_character(&left, 'a', 0);
	parse_type_string(&right, "character");
	subtypep_eql_type_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql_type1");

	test_eql_character(&left, 'a', 0);
	parse_type_string(&right, "symbol");
	subtypep_eql_type_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_eql_type2");

	RETURN;
}

static int test_subtypep_type_eql(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "character");
	test_eql_character(&right, 'a', 0);
	subtypep_type_eql_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_type_eql1");

	parse_type_string(&left, "symbol");
	test_eql_character(&right, 'a', 0);
	subtypep_type_eql_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_type_eql2");

	RETURN;
}

static int test_subtypep_eql_call(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	test_eql_character(&left, 'a', 0);
	test_eql_character(&right, 'a', 0);
	subtypep_eql_call_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql_call1");

	test_eql_character(&left, 'a', 0);
	parse_type_string(&right, "character");
	subtypep_eql_call_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql_call2");

	parse_type_string(&left, "character");
	test_eql_character(&right, 'a', 0);
	subtypep_eql_call_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_eql_call3");

	RETURN;
}

static int test_subtypep_eql(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	test_eql_character(&left, 'a', 0);
	test_eql_character(&right, 'a', 0);
	subtypep_eql_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql1");

	test_eql_character(&left, 'a', 0);
	test_eql_character(&right, 'b', 0);
	subtypep_eql_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_eql2");

	test_eql_character(&left, 'a', 1);
	parse_type_string(&right, "character");
	subtypep_eql_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_eql3");

	test_eql_character(&left, 'a', 0);
	parse_type_string_not(&right, "character");
	subtypep_eql_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_eql4");

	test_eql_character(&left, 'a', 1);
	parse_type_string_not(&right, "character");
	subtypep_eql_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_eql5");

	test_eql_character(&left, 'a', 0);
	parse_type_string_not(&right, "cons");
	subtypep_eql_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql6");

	parse_type_string(&left, "cons");
	test_eql_character(&right, 'a', 1);
	subtypep_eql_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql7");

	test_eql_character(&left, 'a', 0);
	test_eql_character(&right, 'b', 1);
	subtypep_eql_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql8");

	RETURN;
}


/*
 *  subtypep_values
 */
static int test_getsize_values(void)
{
	addr pos;

	parse_values_string(&pos, "(values)");
	test(getsize_values(pos) == 0, "getsize_values1");
	parse_values_string(&pos, "(values integer real string)");
	test(getsize_values(pos) == 3, "getsize_values2");
	parse_values_string(&pos, "(values integer &optional real string t)");
	test(getsize_values(pos) == 4, "getsize_values3");
	parse_values_string(&pos, "(values integer &optional t t &rest string)");
	test(getsize_values(pos) == 3, "getsize_values4");

	RETURN;
}

static int test_gettype_values(void)
{
	addr pos, check;

	parse_values_string(&pos, "(values)");
	gettype_values_(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "gettype_values1");
	gettype_values_(pos, 1, &check);
	test(RefLispDecl(check) == LISPDECL_T, "gettype_values2");

	parse_values_string(&pos, "(values integer string)");
	gettype_values_(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_INTEGER, "gettype_values3");
	gettype_values_(pos, 1, &check);
	test(RefLispDecl(check) == LISPDECL_STRING, "gettype_values4");
	gettype_values_(pos, 2, &check);
	test(RefLispDecl(check) == LISPDECL_T, "gettype_values5");

	parse_values_string(&pos, "(values t &optional string &rest integer)");
	gettype_values_(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "gettype_values6");
	gettype_values_(pos, 1, &check);
	test(RefLispDecl(check) == LISPDECL_STRING, "gettype_values7");
	gettype_values_(pos, 2, &check);
	test(RefLispDecl(check) == LISPDECL_INTEGER, "gettype_values8");
	gettype_values_(pos, 3, &check);
	test(RefLispDecl(check) == LISPDECL_INTEGER, "gettype_values9");

	RETURN;
}

static int test_subtypep_boolean(void)
{
	int value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	extractchar(&left, "integer");
	extractchar(&right, "real");
	subtypep_boolean_(ptr, left, right, &value);
	test(value, "subtypep_boolean1");

	extractchar(&left, "real");
	extractchar(&right, "integer");
	subtypep_boolean_(ptr, left, right, &value);
	test(! value, "subtypep_boolean2");

	extractchar(&left, "real");
	extractchar(&right, "string");
	subtypep_boolean_(ptr, left, right, &value);
	test(! value, "subtypep_boolean3");

	extractchar(&left, "real");
	extractchar(&right, "*");
	subtypep_boolean_(ptr, left, right, &value);
	test(value, "subtypep_boolean4");

	RETURN;
}

static int test_subtypep_values_values(void)
{
	int result;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(result);

	extractchar(&left, "(values)");
	extractchar(&right, "(values)");
	subtypep_values_values_(ptr, left, right, &result);
	test(result, "subtypep_values_values1");

	extractchar(&left, "(values integer)");
	extractchar(&right, "(values)");
	subtypep_values_values_(ptr, left, right, &result);
	test(result, "subtypep_values_values2");

	extractchar(&left, "(values)");
	extractchar(&right, "(values integer)");
	subtypep_values_values_(ptr, left, right, &result);
	test(! result, "subtypep_values_values3");

	extractchar(&left, "(values string)");
	extractchar(&right, "(values integer)");
	subtypep_values_values_(ptr, left, right, &result);
	test(! result, "subtypep_values_values4");

	extractchar(&left, "(values integer)");
	extractchar(&right, "(values integer)");
	subtypep_values_values_(ptr, left, right, &result);
	test(result, "subtypep_values_values5");

	extractchar(&left, "(values integer &rest string)");
	extractchar(&right, "(values integer)");
	subtypep_values_values_(ptr, left, right, &result);
	test(result, "subtypep_values_values6");

	extractchar(&left, "(values integer)");
	extractchar(&right, "(values integer &rest string)");
	subtypep_values_values_(ptr, left, right, &result);
	test(! result, "subtypep_values_values7");

	extractchar(&left, "(values real fixnum)");
	extractchar(&right, "(values real &optional integer)");
	subtypep_values_values_(ptr, left, right, &result);
	test(result, "subtypep_values_values8");

	extractchar(&left, "(values real &optional fixnum)");
	extractchar(&right, "(values real integer)");
	subtypep_values_values_(ptr, left, right, &result);
	test(result, "subtypep_values_values9");

	/*
	 *  sbcl, ccl -> true.
	 *  but (subtypep 't 'integer) -> false in second arcument.
	 */
	extractchar(&left, "(values &optional integer)");
	extractchar(&right, "(values &rest real)");
	subtypep_values_values_(ptr, left, right, &result);
	test(! result, "subtypep_values_values10");

	extractchar(&left, "(values &optional integer)");
	extractchar(&right, "(values &rest string)");
	subtypep_values_values_(ptr, left, right, &result);
	test(! result, "subtypep_values_values11");

	RETURN;
}

static int test_subtypep_values_type(void)
{
	int value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	extractchar(&left, "(values)");
	extractchar(&right, "integer");
	subtypep_values_type_(ptr, left, right, &value);
	test(! value, "subtypep_values_type1");

	extractchar(&left, "(values integer &rest nil)");
	extractchar(&right, "real");
	subtypep_values_type_(ptr, left, right, &value);
	test(value, "subtypep_values_type2");

	extractchar(&left, "(values real &rest nil)");
	extractchar(&right, "integer");
	subtypep_values_type_(ptr, left, right, &value);
	test(! value, "subtypep_values_type3");

	extractchar(&left, "(values fixnum string &rest nil)");
	extractchar(&right, "real");
	subtypep_values_type_(ptr, left, right, &value);
	test(! value, "subtypep_values_type4");

	extractchar(&left, "(values real string &rest nil)");
	extractchar(&right, "integer");
	subtypep_values_type_(ptr, left, right, &value);
	test(! value, "subtypep_values_type5");

	RETURN;
}

static int test_subtypep_type_values(void)
{
	int value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	extractchar(&left, "integer");
	extractchar(&right, "(values)");
	subtypep_type_values_(ptr, left, right, &value);
	test(value, "subtypep_type_values1");

	extractchar(&left, "real");
	extractchar(&right, "(values integer)");
	subtypep_type_values_(ptr, left, right, &value);
	test(! value, "subtypep_type_values2");

	extractchar(&left, "integer");
	extractchar(&right, "(values real)");
	subtypep_type_values_(ptr, left, right, &value);
	test(value, "subtypep_type_values3");

	extractchar(&left, "real");
	extractchar(&right, "(values fixnum string)");
	subtypep_type_values_(ptr, left, right, &value);
	test(! value, "subtypep_type_values4");

	extractchar(&left, "integer");
	extractchar(&right, "(values real string)");
	subtypep_type_values_(ptr, left, right, &value);
	test(! value, "subtypep_type_values5");

	RETURN;
}

static int test_subtypep_values_call(void)
{
	int value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	extractchar(&left, "(values integer fixnum)");
	extractchar(&right, "(values real integer)");
	subtypep_values_call_(ptr, left, right, &value);
	test(value, "subtypep_values_call1");

	extractchar(&left, "(values integer fixnum)");
	extractchar(&right, "string");
	subtypep_values_call_(ptr, left, right, &value);
	test(! value, "subtypep_values_call2");

	extractchar(&left, "integer");
	extractchar(&right, "(values real fixnum)");
	subtypep_values_call_(ptr, left, right, &value);
	test(! value, "subtypep_values_call3");

	RETURN;
}

static int test_subtypep_values(void)
{
	SubtypepResult result;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(result);

	extractchar(&left, "(values integer fixnum)");
	extractchar(&right, "(values real integer)");
	subtypep_values_(ptr, left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_values1");

	extractchar(&left, "(values integer fixnum)");
	extractchar(&right, "string");
	subtypep_values_(ptr, left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_values2");

	RETURN;
}


/*
 *  subtypep_call
 */
static int test_subtypep_leftright(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "cons");
	parse_type_string(&right, "cons");
	subtypep_leftright_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_leftright1");

	parse_type_string(&left, "cons");
	parse_type_string(&right, "symbol");
	subtypep_leftright_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_leftright2");

	parse_type_string_not(&left, "cons");
	parse_type_string(&right, "symbol");
	subtypep_leftright_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_leftright3");

	parse_type_string(&left, "cons");
	parse_type_string_not(&right, "symbol");
	subtypep_leftright_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_leftright4");

	parse_type_string_not(&left, "cons");
	parse_type_string_not(&right, "symbol");
	subtypep_leftright_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_leftright5");

	RETURN;
}

static int test_subtypep_and_left(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "(and integer)");
	parse_type_string(&right, "real");
	subtypep_and_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left1");

	parse_type_string(&left, "(and integer real)");
	parse_type_string(&right, "real");
	subtypep_and_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left2");

	parse_type_string(&left, "(and real integer)");
	parse_type_string(&right, "integer");
	subtypep_and_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left3");

	parse_type_string(&left, "(and (satisfies hello) integer)");
	parse_type_string(&right, "integer");
	subtypep_and_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left4");

	parse_type_string(&left, "(and real rational)");
	parse_type_string(&right, "integer");
	subtypep_and_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_and_left5");

	parse_type_string(&left, "(and real symbol)");
	parse_type_string(&right, "integer");
	subtypep_and_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left6");

	RETURN;
}

static int test_subtypep_or_left(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "(or integer)");
	parse_type_string(&right, "real");
	subtypep_or_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_left1");

	parse_type_string(&left, "(or (satisfies hello) integer)");
	parse_type_string(&right, "integer");
	subtypep_or_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INVALID, "subtypep_or_left2");

	parse_type_string(&left, "(or rational float)");
	parse_type_string(&right, "real");
	subtypep_or_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_left3");

	parse_type_string(&left, "(or symbol cons)");
	parse_type_string(&right, "real");
	subtypep_or_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_or_left4");

	parse_type_string(&left, "(or symbol integer)");
	parse_type_string(&right, "real");
	subtypep_or_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_or_left5");

	RETURN;
}

static int test_subtypep_satisfies_left(void)
{
	SubtypepResult value;
	addr left, right;

	aatype(value);

	parse_type_string(&left, "(satisfies hello)");
	parse_type_string(&right, "t");
	subtypep_satisfies_left_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_satisfies_left1");

	parse_type_string(&left, "(satisfies hello)");
	parse_type_string(&right, "integer");
	subtypep_satisfies_left_(left, right, &value);
	test(value == SUBTYPEP_INVALID, "subtypep_satisfies_left2");

	RETURN;
}

static int test_subtypep_left(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "(and integer)");
	parse_type_string(&right, "real");
	subtypep_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_left1");

	parse_type_string(&left, "(or integer rational)");
	parse_type_string(&right, "real");
	subtypep_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_left2");

	parse_type_string(&left, "(satisfies hello)");
	parse_type_string(&right, "real");
	subtypep_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INVALID, "subtypep_left3");

	parse_type_string(&left, "nil");
	parse_type_string(&right, "real");
	subtypep_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_left4");

	parse_type_string(&left, "t");
	parse_type_string(&right, "real");
	subtypep_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_left5");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_left_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_left6");

	RETURN;
}

static int test_subtypep_andargs_right(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(and real)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_andargs_right1");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and real number)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_andargs_right2");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and integer number)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_andargs_right3");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and symbol cons)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_andargs_right4");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and integer cons)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_andargs_right5");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and real cons)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_andargs_right6");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and)");
	subtypep_andargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_andargs_right7");

	RETURN;
}

static int test_subtypep_orargs_right(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or real)");
	subtypep_orargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_orargs_right1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or)");
	subtypep_orargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_orargs_right2");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or integer cons)");
	subtypep_orargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_orargs_right3");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or cons real)");
	subtypep_orargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_orargs_right4");

	parse_type_string(&left, "symbol");
	parse_type_string(&right, "(or cons real)");
	subtypep_orargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_orargs_right5");

	parse_type_string(&left, "number");
	parse_type_string(&right, "(or cons real)");
	subtypep_orargs_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_orargs_right6");

	RETURN;
}

static int test_subtypep_and_right(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(and integer real)");
	subtypep_and_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_right1");

	parse_type_string(&left, "(or integer real)");
	parse_type_string(&right, "(and real number)");
	subtypep_and_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_right2");

	parse_type_string(&left, "(or integer real symbol)");
	parse_type_string(&right, "(and real number)");
	subtypep_and_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_and_right3");

	parse_type_string(&left, "(or integer real symbol)");
	parse_type_string(&right, "(and cons)");
	subtypep_and_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_and_right4");

	RETURN;
}

static int test_subtypep_or_right(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or real symbol)");
	subtypep_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_right1");

	parse_type_string(&left, "(or integer symbol)");
	parse_type_string(&right, "(or real symbol)");
	subtypep_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_right2");

	parse_type_string(&left, "(and integer real)");
	parse_type_string(&right, "(or integer symbol)");
	subtypep_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_right3");

	parse_type_string(&left, "(or integer cons)");
	parse_type_string(&right, "(or real symbol)");
	subtypep_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_or_right4");

	RETURN;
}

static int test_subtypep_satisfies_right(void)
{
	SubtypepResult value;
	addr left, right;

	aatype(value);

	parse_type_string(&left, "nil");
	parse_type_string(&right, "(satisfies hello)");
	subtypep_satisfies_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_satisfies_right1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(satisfies hello)");
	subtypep_satisfies_right_(left, right, &value);
	test(value == SUBTYPEP_INVALID, "subtypep_satisfies_right2");

	RETURN;
}

static int test_subtypep_nil_right(void)
{
	SubtypepResult value;
	addr left;

	aatype(value);

	parse_type_string(&left, "nil");
	subtypep_nil_right_(left, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_nil_right1");
	parse_type_string(&left, "integer");
	subtypep_nil_right_(left, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_nil_right2");

	RETURN;
}

static int test_subtypep_right(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(and real number)");
	subtypep_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_right1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or real symbol)");
	subtypep_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_right2");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(satisfies hello)");
	subtypep_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INVALID, "subtypep_right3");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "nil");
	subtypep_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_right4");

	parse_type_string(&left, "nil");
	parse_type_string(&right, "nil");
	subtypep_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_right5");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "t");
	subtypep_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_right6");

	parse_type_string(&left, "t");
	parse_type_string(&right, "t");
	subtypep_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_right7");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_right8");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_right_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_right9");

	RETURN;
}


/*
 *  subtypep_check_
 */
static int test_subtypep_call(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "*");
	parse_type_string(&right, "*");
	subtypep_call_(ptr, left, right, 1, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_call1");

	parse_type_string(&left, "cons");
	parse_type_string(&right, "*");
	subtypep_call_(ptr, left, right, 1, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_call2");

	parse_type_string(&left, "*");
	parse_type_string(&right, "cons");
	subtypep_call_(ptr, left, right, 1, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_call3");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_call_(ptr, left, right, 1, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_call4");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_call_(ptr, left, right, 0, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_call5");

	RETURN;
}

static int test_subtypep_check_(void)
{
	int result, invalid;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(result);
	aatype(invalid);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test(result && invalid, "subtypep_check_1");

	parse_type_string(&left, "cons");
	parse_type_string(&right, "real");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test((! result) && invalid, "subtypep_check_2");

	parse_type_string(&left, "character");
	parse_type_string(&right, "base-char");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test((! result) && invalid, "subtypep_check_3");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(satisfies hello)");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test((! result) && (! invalid), "subtypep_check_4");

	parse_type_string(&left, "character");
	parse_type_string(&right, "extended-char");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test((! result) && invalid, "subtypep_check_5");

	parse_type_string(&left, "extended-char");
	parse_type_string(&right, "character");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test(result && invalid, "subtypep_check_6");

	parse_type_string(&left, "(integer 20 50)");
	parse_type_string(&right, "(or (integer 10 40) (integer 30 60))");
	subtypep_check_(ptr, left, right, Nil, &result, &invalid);
	test(result && invalid, "subtypep_check_7");

	RETURN;
}


/*
 *  main
 */
static int testcase_type_subtypep(void)
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
	TestBreak(test_subtypep_function);
	TestBreak(test_subtypep_compiled_function);
	/* subtypep_lisptype */
	TestBreak(test_subtypep_lisptype_normal);
	TestBreak(test_subtypep_lisptype_not);
	TestBreak(test_subtypep_lisptype);
	/* subtypep_eql */
	TestBreak(test_subtypep_eql_eql);
	TestBreak(test_subtypep_eql_type);
	TestBreak(test_subtypep_type_eql);
	TestBreak(test_subtypep_eql_call);
	TestBreak(test_subtypep_eql);
	/* subtypep_values */
	TestBreak(test_getsize_values);
	TestBreak(test_gettype_values);
	TestBreak(test_subtypep_boolean);
	TestBreak(test_subtypep_values_values);
	TestBreak(test_subtypep_values_type);
	TestBreak(test_subtypep_type_values);
	TestBreak(test_subtypep_values_call);
	TestBreak(test_subtypep_values);
	/* subtypep_call */
	TestBreak(test_subtypep_leftright);
	TestBreak(test_subtypep_and_left);
	TestBreak(test_subtypep_or_left);
	TestBreak(test_subtypep_satisfies_left);
	TestBreak(test_subtypep_left);
	TestBreak(test_subtypep_andargs_right);
	TestBreak(test_subtypep_orargs_right);
	TestBreak(test_subtypep_and_right);
	TestBreak(test_subtypep_or_right);
	TestBreak(test_subtypep_satisfies_right);
	TestBreak(test_subtypep_nil_right);
	TestBreak(test_subtypep_right);
	/* subtypep_check_ */
	TestBreak(test_subtypep_call);
	TestBreak(test_subtypep_check_);

	return 0;
}

static void testinit_type_subtypep(Execute ptr)
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

int test_type_subtypep(void)
{
	DegradeTitle;
	return DegradeCode(type_subtypep);
}

