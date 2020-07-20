#include "type_subtypep.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "copy.h"
#include "degrade.h"
#include "hashtable.h"
#include "package.h"
#include "package_symbol.h"
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
	if (parse_type(Execute_Thread, ret, pos, Nil))
		fmte("parse-type error.", NULL);
}

static void parse_type_string(addr *ret, const char *code)
{
	readstring(ret, code);
	test_parse_type(ret, *ret);
}

static SubtypepResult subtable_test(addr left, addr right)
{
	SubtypepResult value;
	/* real_extract(&left, left); */
	/* real_extract(&right, right); */
	subtypep_table_(left, right, &value);
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
 *  subtypep-table
 */
static int test_subtypep_clos(void)
{
	test(strtable_true("STANDARD-CLASS", "CLASS"), "subtypep_clos1");
	test(strtable_false("CLASS", "STANDARD-CLASS"), "subtypep_clos2");
	test(strtable_exclude("INTEGER", "STANDARD-CLASS"), "subtypep_clos3");

	RETURN;
}

static int test_subtypep_nil(void)
{
	test(strtable_exclude("nil", "nil"), "subtypep_nil1");
	RETURN;
}

static int test_subtypep_t(void)
{
	test(strtable_true("nil", "t"), "subtypep_t1");
	RETURN;
}

static int test_subtypep_null(void)
{
	test(strtable_true("null", "null"), "subtypep_null1");
	test(strtable_exclude("cons", "null"), "subtypep_null2");
	RETURN;
}

static int test_asterisk_or_t(void)
{
	addr pos;

	GetTypeTable(&pos, Asterisk);
	test(asterisk_or_t(pos), "asterisk_or_t1");
	GetTypeTable(&pos, T);
	test(asterisk_or_t(pos), "asterisk_or_t2");
	GetTypeTable(&pos, Null);
	test(! asterisk_or_t(pos), "asterisk_or_t3");

	RETURN;
}

static int test_subtypep_asterisk_or_t(void)
{
	SubtypepResult value;
	addr left, right;

	parse_type_string(&left, "NIL");
	parse_type_string(&right, "*");
	subtypep_asterisk_or_t_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_asterisk_or_t1");

	parse_type_string(&left, "T");
	parse_type_string(&right, "*");
	subtypep_asterisk_or_t_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_asterisk_or_t2");

	parse_type_string(&left, "T");
	parse_type_string(&right, "CONS");
	subtypep_asterisk_or_t_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_asterisk_or_t3");

	parse_type_string(&left, "INTEGER");
	parse_type_string(&right, "CONS");
	subtypep_asterisk_or_t_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_asterisk_or_t4");

	RETURN;
}

static int test_subtypep_cons(void)
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

static int test_subtypep_hash_table(void)
{
	test(strtable_true("hash-table", "hash-table"), "subtypep_hash_table1");
	test(strtable_exclude("cons", "hash-table"), "subtypep_hash_table2");
	RETURN;
}

static int test_subtypep_symbol(void)
{
	test(strtable_true("symbol", "symbol"), "subtypep_symbol1");
	test(strtable_true("keyword", "symbol"), "subtypep_symbol2");
	test(strtable_exclude("integer", "symbol"), "subtypep_symbol3");
	RETURN;
}

static int test_subtypep_keyword(void)
{
	test(strtable_true("keyword", "keyword"), "subtypep_keyword1");
	test(strtable_false("symbol", "keyword"), "subtypep_keyword2");
	test(strtable_exclude("cons", "keyword"), "subtypep_keyword3");
	RETURN;
}

static int test_subtypep_package(void)
{
	test(strtable_true("package", "package"), "subtypep_package1");
	test(strtable_exclude("cons", "package"), "subtypep_package2");
	RETURN;
}

static int test_subtypep_random_state(void)
{
	test(strtable_true("random-state", "random-state"), "subtypep_random_state1");
	test(strtable_exclude("cons", "random-state"), "subtypep_random_state2");
	RETURN;
}

static int test_subtypep_readtable(void)
{
	test(strtable_true("readtable", "readtable"), "subtypep_readtable1");
	test(strtable_exclude("cons", "readtable"), "subtypep_readtable2");
	RETURN;
}

static int test_subtypep_pathname(void)
{
	test(strtable_true("pathname", "pathname"), "subtypep_pathname1");
	test(strtable_true("logical-pathname", "pathname"), "subtypep_pathname2");
	test(strtable_exclude("cons", "pathname"), "subtypep_pathname3");

	RETURN;
}

static int test_subtypep_logical_pathname(void)
{
	test(strtable_true("logical-pathname", "logical-pathname"),
			"subtypep_logical_pathname1");
	test(strtable_false("pathname", "logical-pathname"),
			"subtypep_logical_pathname2");
	test(strtable_exclude("cons", "logical-pathname"),
			"subtypep_logical-pathname3");

	RETURN;
}

static int test_subtypep_sequence(void)
{
	test(strtable_true("sequence", "sequence"), "subtypep_sequence1");
	test(strtable_false("array", "sequence"), "subtypep_sequence2");
	test(strtable_false("simple-array", "sequence"), "subtypep_sequence3");
	test(strtable_true("cons", "sequence"), "subtypep_sequence4");
	test(strtable_exclude("symbol", "sequence"), "subtypep_sequence5");
	test(strtable_true("(array * (4))", "sequence"), "subtypep_sequence6");
	test(strtable_false("(array * (4 4))", "sequence"), "subtypep_sequence7");
	test(strtable_false("(simple-array * 0)", "sequence"), "subtypep_sequence8");
	test(strtable_true("(simple-array * 1)", "sequence"), "subtypep_sequence9");
	test(strtable_false("(simple-array * 2)", "sequence"), "subtypep_sequence10");

	RETURN;
}

static SubtypepResult subtypep_value(const char *str1, const char *str2, int asterisk)
{
	SubtypepResult value;
	addr left, right;
	LocalRoot local;

	local = Local_Thread;
	parse_type_string(&left, str1);
	parse_type_string(&right, str2);
	type_optimize_heap(local, &left, left);
	type_optimize_heap(local, &right, right);
	get_type_optimized(&left, left);
	get_type_optimized(&right, right);

	subtypep_call_(left, right, asterisk, &value);
	return value;
}

#define AA_dimension_true(a,b,c) { \
	test(subtypep_value((b), (c), 1) == SUBTYPEP_INCLUDE, "array_array_dimension" a); \
}
#define AA_dimension_false(a,b,c) { \
	test(subtypep_value((b), (c), 1) == SUBTYPEP_FALSE, "array_array_dimension" a); \
}
static int test_array_array_dimension(void)
{
	AA_dimension_true("1", "(array * *)", "(array * *)");
	AA_dimension_false("2", "(array * *)", "(array * (*))");
	AA_dimension_false("3", "(array * (10 20))", "(array * 1)");
	AA_dimension_true("4", "(array * (10))", "(array * 1)");
	AA_dimension_true("5", "(array * (10))", "(array * (*))");
	AA_dimension_true("6", "(array * 4)", "(array * 4)");
	AA_dimension_true("7", "(array * (* * * *))", "(array * 4)");
	AA_dimension_false("8", "(array * 4)", "(array * 3)");
	AA_dimension_false("9", "(array * 3)", "(array * 4)");
	AA_dimension_false("10", "(array * (3))", "(array * (3 4))");
	AA_dimension_false("11", "(array * (3 5))", "(array * (3 4))");
	AA_dimension_true("12", "(array * (3 4))", "(array * (3 4))");
	AA_dimension_true("13", "(array * (3 4))", "(array * (3 *))");
	AA_dimension_false("14", "(array * (* 4))", "(array * (3 *))");
	AA_dimension_false("15", "(array * (* 4))", "(array * (* 3))");
	AA_dimension_true("16", "(array * (* 4 6))", "(array * (* * 6))");

	RETURN;
}

#define ArrayArray_true(a,b,c) { \
	test(subtypep_value((b), (c), 1) == SUBTYPEP_INCLUDE, "subtypep_array_array" a); \
}
#define ArrayArray_false(a,b,c) { \
	test(subtypep_value((b), (c), 1) == SUBTYPEP_FALSE, "subtypep_array_array" a); \
}
static int test_subtypep_array_array(void)
{
	ArrayArray_true("1", "(array integer)", "array");
	ArrayArray_true("2", "(array integer)", "(array *)");
	ArrayArray_true("3", "(array *)", "(array *)");
	ArrayArray_false("4", "(array *)", "(array integer)");
	ArrayArray_true("5", "(array integer)", "(array integer)");
	ArrayArray_false("6", "(array character)", "(array integer)");
	ArrayArray_false("7", "(array integer)", "(array character)");
	ArrayArray_true("8", "(array integer 2)", "(array integer 2)");
	ArrayArray_false("9", "(array integer 2)", "(array integer 3)");
	ArrayArray_true("10", "(simple-array integer)", "array");
	ArrayArray_true("11", "(simple-array integer)", "(array *)");
	ArrayArray_true("12", "(simple-array *)", "(array *)");
	ArrayArray_false("13", "(simple-array *)", "(array integer)");
	ArrayArray_true("14", "(simple-array integer)", "(array integer)");
	ArrayArray_false("15", "(simple-array character)", "(array integer)");
	ArrayArray_false("16", "(simple-array integer)", "(array character)");
	ArrayArray_true("17", "(simple-array integer 2)", "(array integer 2)");
	ArrayArray_false("18", "(simple-array integer 2)", "(array integer 3)");

	RETURN;
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

static int test_subtypep_character(void)
{
	test(strtable_true("character", "character"), "subtypep_character1");
	test(strtable_true("base-char", "character"), "subtypep_character2");
	test(strtable_true("standard-char", "character"), "subtypep_character3");
	test(strtable_exclude("integer", "character"), "subtypep_character4");

	RETURN;
}

static int test_subtypep_base_char(void)
{
	test(strtable_true("base-char", "base-char"), "subtypep_base_char1");
	test(strtable_true("standard-char", "base-char"), "subtypep_base_char2");
	test(strtable_false("character", "base-char"), "subtypep_base_char3");
	test(strtable_exclude("cons", "base-char"), "subtypep_base_char4");

	RETURN;
}

static int test_subtypep_standard_char(void)
{
	test(strtable_true("standard-char", "standard-char"), "subtypep_standard_char1");
	test(strtable_false("base-char", "standard-char"), "subtypep_standard_char2");
	test(strtable_false("character", "standard-char"), "subtypep_standard_char3");
	test(strtable_exclude("symbol", "standard-char"), "subtypep_standard_char4");

	RETURN;
}

static int test_subtypep_real_less(void)
{
	addr left, right;

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer * 30)");
	test(subtypep_real_less(left, right), "subtypep_real_less1");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer * 20)");
	test(subtypep_real_less(left, right), "subtypep_real_less2");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer * 19)");
	test(! subtypep_real_less(left, right), "subtypep_real_less3");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer * 5)");
	test(! subtypep_real_less(left, right), "subtypep_real_less4");

	parse_type_string(&left, "(integer * 20)");
	parse_type_string(&right, "(integer * 30)");
	test(subtypep_real_less(left, right), "subtypep_real_less5");

	parse_type_string(&left, "(integer * 20)");
	parse_type_string(&right, "(integer * 20)");
	test(subtypep_real_less(left, right), "subtypep_real_less6");

	parse_type_string(&left, "(integer * 20)");
	parse_type_string(&right, "(integer * 19)");
	test(! subtypep_real_less(left, right), "subtypep_real_less7");

	parse_type_string(&left, "(integer * 20)");
	parse_type_string(&right, "(integer * 5)");
	test(! subtypep_real_less(left, right), "subtypep_real_less8");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer * 5)");
	test(! subtypep_real_less(left, right), "subtypep_real_less9");

	parse_type_string(&left, "(integer 10 (20))");
	parse_type_string(&right, "(integer * 20)");
	test(subtypep_real_less(left, right), "subtypep_real_less10");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer * (20))");
	test(! subtypep_real_less(left, right), "subtypep_real_less11");

	parse_type_string(&left, "(integer 10 (20))");
	parse_type_string(&right, "(integer * (20))");
	test(subtypep_real_less(left, right), "subtypep_real_less12");

	RETURN;
}

static int test_subtypep_real_greater(void)
{
	addr left, right;

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 5 *)");
	test(subtypep_real_greater(left, right), "subtypep_real_greater1");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 10 *)");
	test(subtypep_real_greater(left, right), "subtypep_real_greater2");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 11 *)");
	test(! subtypep_real_greater(left, right), "subtypep_real_greater3");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 30 *)");
	test(! subtypep_real_greater(left, right), "subtypep_real_greater4");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer 5 *)");
	test(subtypep_real_greater(left, right), "subtypep_real_greater5");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer 10 *)");
	test(subtypep_real_greater(left, right), "subtypep_real_greater6");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer 11 *)");
	test(! subtypep_real_greater(left, right), "subtypep_real_greater7");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer 30 *)");
	test(! subtypep_real_greater(left, right), "subtypep_real_greater8");

	parse_type_string(&left, "(integer * 10)");
	parse_type_string(&right, "(integer 30 *)");
	test(! subtypep_real_greater(left, right), "subtypep_real_greater9");

	parse_type_string(&left, "(integer (10) 20)");
	parse_type_string(&right, "(integer 10 *)");
	test(subtypep_real_greater(left, right), "subtypep_real_greater10");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer (10) *)");
	test(! subtypep_real_greater(left, right), "subtypep_real_greater11");

	parse_type_string(&left, "(integer (10) 20)");
	parse_type_string(&right, "(integer (10) *)");
	test(subtypep_real_greater(left, right), "subtypep_real_greater12");

	RETURN;
}

static int test_subtypep_real_range(void)
{
	addr left, right;

	parse_type_string(&left, "(integer 15 16)");
	parse_type_string(&right, "(integer 10 20)");
	test(subtypep_real_range(left, right), "subtypep_real_range1");

	parse_type_string(&left, "(integer 10 16)");
	parse_type_string(&right, "(integer 10 20)");
	test(subtypep_real_range(left, right), "subtypep_real_range2");

	parse_type_string(&left, "(integer 15 20)");
	parse_type_string(&right, "(integer 10 20)");
	test(subtypep_real_range(left, right), "subtypep_real_range3");

	parse_type_string(&left, "(integer 15 30)");
	parse_type_string(&right, "(integer 10 20)");
	test(! subtypep_real_range(left, right), "subtypep_real_range4");

	parse_type_string(&left, "(integer 100 200)");
	parse_type_string(&right, "(integer 10 20)");
	test(! subtypep_real_range(left, right), "subtypep_real_range5");

	RETURN;
}

static int test_subtypep_realcheck(void)
{
	addr left, right;

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "integer");
	test(subtypep_realcheck(left, right), "subtypep_realcheck1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(integer 10 20)");
	test(! subtypep_realcheck(left, right), "subtypep_realcheck2");

	parse_type_string(&left, "(integer 15 15)");
	parse_type_string(&right, "(integer 10 20)");
	test(subtypep_realcheck(left, right), "subtypep_realcheck3");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer * 30)");
	test(subtypep_realcheck(left, right), "subtypep_realcheck4");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 5 *)");
	test(subtypep_realcheck(left, right), "subtypep_realcheck5");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 0 100)");
	test(subtypep_realcheck(left, right), "subtypep_realcheck6");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 200 300)");
	test(! subtypep_realcheck(left, right), "subtypep_realcheck7");

	RETURN;
}

static int test_realexclude_left(void)
{
	addr left, right;

	parse_type_string(&left, "(integer * 10)");
	parse_type_string(&right, "(integer 20 *)");
	test(subtypep_realexlucde(left, right), "realexclude_left1");

	parse_type_string(&left, "(integer * 30)");
	parse_type_string(&right, "(integer 20 *)");
	test(! subtypep_realexlucde(left, right), "realexclude_left2");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 30 40)");
	test(subtypep_realexlucde(left, right), "realexclude_left3");

	parse_type_string(&left, "(integer * 20)");
	parse_type_string(&right, "(integer 20 *)");
	test(! subtypep_realexlucde(left, right), "realexclude_left4");

	parse_type_string(&left, "(integer * (20))");
	parse_type_string(&right, "(integer 20 *)");
	test(subtypep_realexlucde(left, right), "realexclude_left5");

	parse_type_string(&left, "(integer * 20)");
	parse_type_string(&right, "(integer (20) *)");
	test(subtypep_realexlucde(left, right), "realexclude_left6");

	parse_type_string(&left, "(integer * (20))");
	parse_type_string(&right, "(integer (20) *)");
	test(subtypep_realexlucde(left, right), "realexclude_left7");

	RETURN;
}

static int test_realexclude_right(void)
{
	addr left, right;

	parse_type_string(&left, "(integer 20 *)");
	parse_type_string(&right, "(integer * 10)");
	test(subtypep_realexlucde(left, right), "realexclude_right1");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer * 20)");
	test(! subtypep_realexlucde(left, right), "realexclude_right2");

	parse_type_string(&left, "(integer 30 40)");
	parse_type_string(&right, "(integer 10 20)");
	test(subtypep_realexlucde(left, right), "realexclude_right3");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer * 10)");
	test(! subtypep_realexlucde(left, right), "realexclude_right4");

	parse_type_string(&left, "(integer (10) *)");
	parse_type_string(&right, "(integer * 10)");
	test(subtypep_realexlucde(left, right), "realexclude_right5");

	parse_type_string(&left, "(integer 10 *)");
	parse_type_string(&right, "(integer * (10))");
	test(subtypep_realexlucde(left, right), "realexclude_right6");

	parse_type_string(&left, "(integer (10) *)");
	parse_type_string(&right, "(integer * (10))");
	test(subtypep_realexlucde(left, right), "realexclude_right7");

	RETURN;
}

static int test_subtypep_realexclude(void)
{
	addr left, right;

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 30 40)");
	test(subtypep_realexlucde(left, right), "subtypep_realexclude1");

	parse_type_string(&left, "(integer 30 40)");
	parse_type_string(&right, "(integer 10 20)");
	test(subtypep_realexlucde(left, right), "subtypep_realexclude2");

	parse_type_string(&left, "(integer 10 30)");
	parse_type_string(&right, "(integer 20 40)");
	test(! subtypep_realexlucde(left, right), "subtypep_realexclude3");

	parse_type_string(&left, "(integer 20 40)");
	parse_type_string(&right, "(integer 10 30)");
	test(! subtypep_realexlucde(left, right), "subtypep_realexclude4");

	RETURN;
}

static int test_subtypep_realparameter(void)
{
	SubtypepResult value;
	addr left, right;

	parse_type_string(&left, "integer");
	parse_type_string(&right, "integer");
	subtypep_realparameter_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_realparameter1");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 30 *)");
	subtypep_realparameter_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_realparameter2");

	parse_type_string(&left, "(integer 10 20)");
	parse_type_string(&right, "(integer 15 *)");
	subtypep_realparameter_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_realparameter3");

	RETURN;
}

static int test_subtypep_integer(void)
{
	test(strtable_true("integer", "integer"), "subtypep_integer1");
	test(strtable_false("rational", "integer"), "subtypep_integer2");
	test(strtable_false("real", "integer"), "subtypep_integer3");
	test(strtable_false("number", "integer"), "subtypep_integer4");
	test(strtable_exclude("cons", "integer"), "subtypep_integer5");
	test(strtable_true("(integer 10 10)", "(integer 0 100)"), "subtypep_integer6");

	RETURN;
}

static int test_subtypep_rational(void)
{
	test(strtable_false("real", "rational"), "subtypep_rational1");
	test(strtable_false("number", "rational"), "subtypep_rational2");
	test(strtable_true("ratio", "rational"), "subtypep_rational3");
	test(strtable_false("ratio", "(rational 10 *)"), "subtypep_rational4");
	test(strtable_true("rational", "rational"), "subtypep_rational5");
	test(strtable_true("integer", "rational"), "subtypep_rational6");
	test(strtable_exclude("symbol", "rational"), "subtypep_rational7");
	test(strtable_true("(integer 10 20)", "(rational 5 100)"),
			"subtypep_rational8");
	test(strtable_false("(integer 10 20)", "(rational 15 100)"),
			"subtypep_rational9");
	test(strtable_exclude("(integer 10 20)", "(rational 50 100)"),
			"subtypep_rational10");

	RETURN;
}

static int test_subtypep_real(void)
{
	test(strtable_true("real", "real"), "subtypep_real1");
	test(strtable_false("number", "real"), "subtypep_real2");
	test(strtable_true("ratio", "real"), "subtypep_real3");
	test(strtable_false("ratio", "(real 10 20)"), "subtypep_real4");
	test(strtable_true("float", "real"), "subtypep_real5");
	test(strtable_exclude("symbol", "real"), "subtypep_real6");

	RETURN;
}

static int test_subtypep_number(void)
{
	test(strtable_true("number", "number"), "subtypep_number1");
	test(strtable_true("complex", "number"), "subtypep_number2");
	test(strtable_true("ratio", "number"), "subtypep_number3");
	test(strtable_true("float", "number"), "subtypep_number4");
	test(strtable_exclude("symbol", "number"), "subtypep_number5");

	RETURN;
}

static int test_subtypep_float(void)
{
	SubtypepResult value;
	addr left, right, pos1, pos2;

	test(strtable_false("number", "float"), "subtypep_float1");
	test(strtable_false("real", "float"), "subtypep_float2");
	test(strtable_true("float", "float"), "subtypep_float3");
	test(strtable_true("short-float", "float"), "subtypep_float4");
	test(strtable_exclude("symbol", "float"), "subtypep_float5");

	interncommon("DOUBLE-FLOAT", &left);
	double_float_heap(&pos1, 10.0);
	double_float_heap(&pos2, 20.0);
	list_heap(&left, left, pos1, pos2, NULL);
	test_parse_type(&left, left);
	interncommon("FLOAT", &right);
	double_float_heap(&pos1, 15.0);
	list_heap(&right, right, pos1, NULL);
	test_parse_type(&right, right);
	subtypep_table_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_float6");

	RETURN;
}

static int test_subtypep_float_type(void)
{
	SubtypepResult value;
	addr left, right, pos1, pos2;

	parse_type_string(&left, "number");
	parse_type_string(&right, "single-float");
	subtypep_float_type_(left, right, &value, LISPDECL_SINGLE_FLOAT);
	test(value == SUBTYPEP_FALSE, "subtypep_float_type1");

	parse_type_string(&left, "real");
	parse_type_string(&right, "single-float");
	subtypep_float_type_(left, right, &value, LISPDECL_SINGLE_FLOAT);
	test(value == SUBTYPEP_FALSE, "subtypep_float_type2");

	parse_type_string(&left, "float");
	parse_type_string(&right, "single-float");
	subtypep_float_type_(left, right, &value, LISPDECL_SINGLE_FLOAT);
	test(value == SUBTYPEP_FALSE, "subtypep_float_type3");

	parse_type_string(&left, "single-float");
	parse_type_string(&right, "single-float");
	subtypep_float_type_(left, right, &value, LISPDECL_SINGLE_FLOAT);
	test(value == SUBTYPEP_INCLUDE, "subtypep_float_type4");

	parse_type_string(&left, "cons");
	parse_type_string(&right, "single-float");
	subtypep_float_type_(left, right, &value, LISPDECL_SINGLE_FLOAT);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_float_type5");

	interncommon("SINGLE-FLOAT", &left);
	single_float_heap(&pos1, 10.0);
	single_float_heap(&pos2, 20.0);
	list_heap(&left, left, pos1, pos2, NULL);
	test_parse_type(&left, left);
	interncommon("SINGLE-FLOAT", &right);
	single_float_heap(&pos1, 5.0);
	list_heap(&right, right, pos1, NULL);
	test_parse_type(&right, right);
	subtypep_float_type_(left, right, &value, LISPDECL_SINGLE_FLOAT);
	test(value == SUBTYPEP_INCLUDE, "subtypep_float_type6");

	RETURN;
}

static int test_subtypep_short_float(void)
{
	test(strtable_true("short-float", "short-float"), "subtypep_short_float1");
	RETURN;
}

static int test_subtypep_single_float(void)
{
	test(strtable_true("single-float", "single-float"), "subtypep_single_float1");
	RETURN;
}

static int test_subtypep_double_float(void)
{
	test(strtable_true("double-float", "double-float"), "subtypep_double_float1");
	RETURN;
}

static int test_subtypep_long_float(void)
{
	test(strtable_true("long-float", "long-float"), "subtypep_long_float1");
	RETURN;
}

static int test_subtypep_ratio(void)
{
	test(strtable_false("number", "ratio"), "subtypep_ratio1");
	test(strtable_false("real", "ratio"), "subtypep_ratio2");
	test(strtable_false("rational", "ratio"), "subtypep_ratio3");
	test(strtable_true("ratio", "ratio"), "subtypep_ratio4");
	test(strtable_exclude("integer", "ratio"), "subtypep_ratio5");
	test(strtable_exclude("symbol", "ratio"), "subtypep_ratio6");

	RETURN;
}

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

static int test_make_function_ordinary(void)
{
	addr pos, a, b, c, d;
	ordargs str;

	parse_args(&pos, "(function "
			"(integer nil nil &optional t null &rest real &key (hello string)))");
	GetArrayA2(pos, 0, &a);
	GetArrayA2(pos, 1, &b);
	GetArrayA2(pos, 2, &c);
	GetArrayA2(pos, 3, &d);
	make_function_ordinary(&str, pos);
	test(str.var == a, "make_function_ordinary1");
	test(str.opt == b, "make_function_ordinary2");
	test(str.rest == c, "make_function_ordinary3");
	test(str.key == d, "make_function_ordinary4");
	test(length_list_unsafe(a) == 3, "make_function_ordinary5");
	test(length_list_unsafe(b) == 2, "make_function_ordinary6");
	test(RefLispDecl(c) == LISPDECL_REAL, "make_function_ordinary7");
	test(length_list_unsafe(d) == 1, "make_function_ordinary8");
	test(str.size_var == 3, "make_function_ordinary9");
	test(str.size_opt == 2, "make_function_ordinary10");
	test(str.size_key == 1, "make_function_ordinary11");
	test(str.pos_rest == 5, "make_function_ordinary12");
	test(str.size == 7, "make_function_ordinary13");

	parse_args(&pos, "(function (nil &rest real))");
	make_function_ordinary(&str, pos);
	test(str.size == 2, "make_function_ordinary14");

	parse_args(&pos, "(function (nil &optional real))");
	make_function_ordinary(&str, pos);
	test(str.size == 2, "make_function_ordinary15");

	parse_args(&pos, "(function (nil &key (name integer)))");
	make_function_ordinary(&str, pos);
	test(str.size == 3, "make_function_ordinary16");

	parse_args(&pos, "(function (nil &optional t null))");
	make_function_ordinary(&str, pos);
	test(str.size == 3, "make_function_ordinary17");

	/* &allow-other-keys */
	parse_args(&pos, "(function (nil &key (name integer)))");
	SetArrayType(pos, 3, T);
	make_function_ordinary(&str, pos);
	test(str.key == T, "make_function_ordinary18");
	test(str.size_key == 0, "make_function_ordinary19");
	test(str.size == 3, "make_function_ordinary20");

	RETURN;
}

static int test_gettype_ordinary(void)
{
	addr pos;
	ordargs str;
	ordtype type;

	parse_args(&pos, "(function (null atom))");
	make_function_ordinary(&str, pos);

	gettype_ordinary(&str, 0, &type);
	test(type.var, "gettype_ordinary1");
	test(RefLispDecl(type.type) == LISPDECL_NULL, "gettype_ordinary2");

	gettype_ordinary(&str, 1, &type);
	test(type.var, "gettype_ordinary3");
	test(RefLispDecl(type.type) == LISPDECL_ATOM, "gettype_ordinary4");

	gettype_ordinary(&str, 2, &type);
	test(! type.var, "gettype_ordinary4");
	test(type.nil, "gettype_ordinary5");
	test(type.type == Nil, "gettype_ordinary6");

	parse_args(&pos, "(function (null atom &optional integer))");
	make_function_ordinary(&str, pos);

	gettype_ordinary(&str, 1, &type);
	test(type.var, "gettype_ordinary7");
	test(RefLispDecl(type.type) == LISPDECL_ATOM, "gettype_ordinary8");

	gettype_ordinary(&str, 2, &type);
	test(type.var, "gettype_ordinary9");
	test(RefLispDecl(type.type) == LISPDECL_INTEGER, "gettype_ordinary10");

	gettype_ordinary(&str, 3, &type);
	test(! type.var, "gettype_ordinary11");
	test(type.nil, "gettype_ordinary12");
	test(type.type == Nil, "gettype_ordinary13");

	parse_args(&pos, "(function (&optional integer &rest cons))");
	make_function_ordinary(&str, pos);

	gettype_ordinary(&str, 0, &type);
	test(type.var, "gettype_ordinary14");
	test(RefLispDecl(type.type) == LISPDECL_INTEGER, "gettype_ordinary15");

	gettype_ordinary(&str, 1, &type);
	test(type.var, "gettype_ordinary15");
	test(type.rest, "gettype_ordinary16");
	test(RefLispDecl(type.type) == LISPDECL_CONS, "gettype_ordinary17");

	parse_args(&pos, "(function (atom &rest cons &key (hello real)))");
	make_function_ordinary(&str, pos);

	gettype_ordinary(&str, 0, &type);
	test(type.var, "gettype_ordinary18");
	test(RefLispDecl(type.type) == LISPDECL_ATOM, "gettype_ordinary19");

	gettype_ordinary(&str, 1, &type);
	test(type.var, "gettype_ordinary20");
	test(type.key, "gettype_ordinary21");
	test(! type.value, "gettype_ordinary22");
	test(type.type != Nil, "gettype_ordinary23");

	gettype_ordinary(&str, 2, &type);
	test(type.var, "gettype_ordinary24");
	test(! type.key, "gettype_ordinary25");
	test(type.value, "gettype_ordinary26");
	test(type.type != Nil, "gettype_ordinary27");

	gettype_ordinary(&str, 1, &type);
	test(type.var, "gettype_ordinary28");
	test(type.key, "gettype_ordinary29");
	test(! type.value, "gettype_ordinary30");
	test(type.type != Nil, "gettype_ordinary31");

	parse_args(&pos, "(function (atom &key (hello real)))");
	make_function_ordinary(&str, pos);

	gettype_ordinary(&str, 1, &type);
	test(! type.var, "gettype_ordinary32");
	test(type.key, "gettype_ordinary33");
	test(! type.value, "gettype_ordinary34");
	test(type.type == Nil, "gettype_ordinary35");

	/* allow-other-keys */
	parse_args(&pos, "(function (atom &key (hello real)))");
	SetArrayType(pos, 3, T);
	make_function_ordinary(&str, pos);
	gettype_ordinary(&str, 1, &type);
	test(! type.var, "gettype_ordinary36");
	test(type.key, "gettype_ordinary37");
	test(! type.value, "gettype_ordinary38");
	test(type.type == Nil, "gettype_ordinary39");

	RETURN;
}

static int test_ordargs_simple_p(void)
{
	addr pos;
	ordargs str;

	parse_args(&pos, "(function (atom t))");
	make_function_ordinary(&str, pos);
	test(ordargs_simple_p(&str), "ordargs_simple_p1");

	parse_args(&pos, "(function (atom &rest t))");
	make_function_ordinary(&str, pos);
	test(! ordargs_simple_p(&str), "ordargs_simple_p2");

	parse_args(&pos, "(function (atom &key (a t)))");
	make_function_ordinary(&str, pos);
	test(! ordargs_simple_p(&str), "ordargs_simple_p3");

	RETURN;
}

static int test_ordinary_keytype(void)
{
	addr pos, check, array;
	ordargs str;
	LocalRoot local = Local_Thread;

	/* &allow-other-keys */
	parse_args(&pos, "(function (atom &key (a t)))");
	SetArrayType(pos, 3, T);
	make_function_ordinary(&str, pos);
	ordinary_keytype(local, &pos, &str);
	test(RefLispDecl(pos) == LISPDECL_SYMBOL, "ordinary_keytype1");

	/* (eql key) */
	parse_args(&pos, "(function (atom &key (name integer)))");
	make_function_ordinary(&str, pos);
	ordinary_keytype(local, &pos, &str);
	test(RefLispDecl(pos) == LISPDECL_EQL, "ordinary_keytype2");
	GetArrayType(pos, 0, &pos);
	readstring(&check, "name");
	test(pos == check, "ordinary_keytype3");

	/* (or (eql key) ...) */
	parse_args(&pos, "(function (atom &key (aa real) (bb t) (cc atom)))");
	make_function_ordinary(&str, pos);
	ordinary_keytype(local, &pos, &str);
	test(RefLispDecl(pos) == LISPDECL_OR, "ordinary_keytype4");
	GetArrayType(pos, 0, &array);
	test(lenarrayr(array) == 3, "ordinary_keytype5");
	getarray(array, 0, &pos);
	test(RefLispDecl(pos) == LISPDECL_EQL, "ordinary_keytype6");
	GetArrayType(pos, 0, &pos);
	readstring(&check, "aa");
	test(pos == check, "ordinary_keytype7");

	getarray(array, 1, &pos);
	test(RefLispDecl(pos) == LISPDECL_EQL, "ordinary_keytype8");
	GetArrayType(pos, 0, &pos);
	readstring(&check, "bb");
	test(pos == check, "ordinary_keytype9");

	getarray(array, 2, &pos);
	test(RefLispDecl(pos) == LISPDECL_EQL, "ordinary_keytype10");
	GetArrayType(pos, 0, &pos);
	readstring(&check, "cc");
	test(pos == check, "ordinary_keytype11");

	RETURN;
}

static int test_ordinary_valuetype(void)
{
	addr pos, array;
	ordargs str;
	LocalRoot local = Local_Thread;

	/* &allow-other-keys */
	parse_args(&pos, "(function (atom &key (a t)))");
	SetArrayType(pos, 3, T);
	make_function_ordinary(&str, pos);
	ordinary_valuetype(local, &pos, &str);
	test(RefLispDecl(pos) == LISPDECL_T, "ordinary_valuetype1");

	/* (eql type) */
	parse_args(&pos, "(function (atom &key (name integer)))");
	make_function_ordinary(&str, pos);
	ordinary_valuetype(local, &pos, &str);
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "ordinary_valuetype2");

	/* (or type ...) */
	parse_args(&pos, "(function (atom &key (aa real) (bb t) (cc atom)))");
	make_function_ordinary(&str, pos);
	ordinary_valuetype(local, &pos, &str);
	test(RefLispDecl(pos) == LISPDECL_OR, "ordinary_valuetype3");
	GetArrayType(pos, 0, &array);
	test(lenarrayr(array) == 3, "ordinary_valuetype4");
	getarray(array, 0, &pos);
	test(RefLispDecl(pos) == LISPDECL_REAL, "ordinary_valuetype5");
	getarray(array, 1, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "ordinary_valuetype6");
	getarray(array, 2, &pos);
	test(RefLispDecl(pos) == LISPDECL_ATOM, "ordinary_valuetype7");

	RETURN;
}

static int test_make_ordinary_type(void)
{
	addr pos, check;
	ordargs str;
	ordtype type;
	LocalRoot local = Local_Thread;

	parse_args(&pos, "(function (atom list))");
	make_function_ordinary(&str, pos);
	gettype_ordinary(&str, 0, &type);
	make_ordinary_type(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_ATOM, "gettype_ordinary1");

	parse_args(&pos, "(function (atom &optional list))");
	make_function_ordinary(&str, pos);
	gettype_ordinary(&str, 1, &type);
	make_ordinary_type(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_LIST, "gettype_ordinary2");

	parse_args(&pos, "(function (atom &rest real))");
	make_function_ordinary(&str, pos);
	gettype_ordinary(&str, 1, &type);
	make_ordinary_type(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_REAL, "gettype_ordinary3");

	parse_args(&pos, "(function (atom &key (name real)))");
	make_function_ordinary(&str, pos);
	gettype_ordinary(&str, 1, &type);
	make_ordinary_type(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_EQL, "gettype_ordinary4");

	parse_args(&pos, "(function (atom &key (name real)))");
	make_function_ordinary(&str, pos);
	gettype_ordinary(&str, 2, &type);
	make_ordinary_type(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_REAL, "gettype_ordinary5");

	parse_args(&pos, "(function (atom &rest cons &key (name real)))");
	make_function_ordinary(&str, pos);
	gettype_ordinary(&str, 1, &type);
	make_ordinary_type(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_AND, "gettype_ordinary6");
	GetArrayType(pos, 0, &pos);
	getarray(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_CONS, "gettype_ordinary7");
	getarray(pos, 1, &check);
	test(RefLispDecl(check) == LISPDECL_EQL, "gettype_ordinary8");

	parse_args(&pos, "(function (atom &rest cons &key (name real)))");
	make_function_ordinary(&str, pos);
	gettype_ordinary(&str, 2, &type);
	make_ordinary_type(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_AND, "gettype_ordinary9");
	GetArrayType(pos, 0, &pos);
	getarray(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_CONS, "gettype_ordinary10");
	getarray(pos, 1, &check);
	test(RefLispDecl(check) == LISPDECL_REAL, "gettype_ordinary11");

	parse_args(&pos, "(function (atom &rest cons &key (name real)))");
	SetArrayType(pos, 3, T);
	make_function_ordinary(&str, pos);
	gettype_ordinary(&str, 1, &type);
	make_ordinary_type(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_AND, "gettype_ordinary12");
	GetArrayType(pos, 0, &pos);
	getarray(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_CONS, "gettype_ordinary13");
	getarray(pos, 1, &check);
	test(RefLispDecl(check) == LISPDECL_SYMBOL, "gettype_ordinary14");

	RETURN;
}

static int test_ordinary_subtypep(void)
{
	int value;
	addr pos;
	ordargs str1, str2;
	ordtype type1, type2;

	parse_args(&pos, "(function (atom integer))");
	make_function_ordinary(&str1, pos);
	parse_args(&pos, "(function (real string))");
	make_function_ordinary(&str2, pos);
	gettype_ordinary(&str1, 1, &type1);
	gettype_ordinary(&str2, 0, &type2);
	ordinary_subtypep_(&str1, &type1, &str2, &type2, &value);
	test(value, "ordinary_subtypep1");

	gettype_ordinary(&str1, 0, &type1);
	gettype_ordinary(&str2, 1, &type2);
	ordinary_subtypep_(&str1, &type1, &str2, &type2, &value);
	test(! value, "ordinary_subtypep2");

	RETURN;
}

static void parse_values_string(addr *ret, const char *code)
{
	readstring(ret, code);
	if (parse_type_values(Execute_Thread, ret, *ret, Nil))
		Error(fmte_("parse-type-values error.", NULL));
}

static void extractchar(addr *ret, const char *str)
{
	LocalRoot local;

	local = Local_Thread;
	parse_values_string(ret, str);
	real_extract_subtypep(local, ret, *ret);
}

static void argschar(ordargs *ret, const char *str)
{
	addr pos;

	extractchar(&pos, str);
	GetArrayType(pos, 0, &pos);
	make_function_ordinary(ret, pos);
}

static int test_ordinary_size(void)
{
	int value;
	ordargs args1, args2;

	argschar(&args1, "(function (integer))");
	argschar(&args2, "(function (real))");
	ordinary_size_(&args1, &args2, 1, &value);
	test(value, "ordinary_size1");
	ordinary_size_(&args1, &args2, 2, &value);
	test(value, "ordinary_size2");

	argschar(&args1, "(function (integer integer))");
	argschar(&args2, "(function (real))");
	ordinary_size_(&args1, &args2, 2, &value);
	test(! value, "ordinary_size3");

	argschar(&args1, "(function (integer))");
	argschar(&args2, "(function (real real))");
	ordinary_size_(&args1, &args2, 2, &value);
	test(value, "ordinary_size4");

	argschar(&args1, "(function (integer string))");
	argschar(&args2, "(function (real real))");
	ordinary_size_(&args1, &args2, 2, &value);
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

	extractargs(&left, "(function (integer integer))");
	extractargs(&right, "(function (t real))");
	subtypep_function_ordinary_(left, right, &value);
	test(value, "ordinary_simple1");

	extractargs(&left, "(function (integer))");
	extractargs(&right, "(function (t real))");
	subtypep_function_ordinary_(left, right, &value);
	test(! value, "ordinary_simple2");

	extractargs(&left, "(function (integer integer))");
	extractargs(&right, "(function (t &optional real))");
	subtypep_function_ordinary_(left, right, &value);
	test(value, "ordinary_simple3");

	extractargs(&left, "(function (integer &optional integer))");
	extractargs(&right, "(function (t real))");
	subtypep_function_ordinary_(left, right, &value);
	test(! value, "ordinary_simple4");

	extractargs(&left, "(function (integer integer))");
	extractargs(&right, "(function (t &optional real real))");
	subtypep_function_ordinary_(left, right, &value);
	test(value, "ordinary_simple5");

	extractargs(&left, "(function (integer integer &optional integer))");
	extractargs(&right, "(function (t &optional real real))");
	subtypep_function_ordinary_(left, right, &value);
	test(value, "ordinary_simple6");

	extractargs(&left, "(function (integer))");
	extractargs(&right, "(function (t &optional real real))");
	subtypep_function_ordinary_(left, right, &value);
	test(value, "ordinary_simple7");

	extractargs(&left, "(function (integer))");
	extractargs(&right, "(function (t t &optional real real))");
	subtypep_function_ordinary_(left, right, &value);
	test(! value, "ordinary_simple8");

	RETURN;
}

static int test_ordinary_simple_left(void)
{
	int value;
	addr left, right;

	extractargs(&left, "(function (integer integer))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(left, right, &value);
	test(value, "ordinary_simple_left1");

	extractargs(&left, "(function (integer))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(left, right, &value);
	test(! value, "ordinary_simple_left2");

	extractargs(&left, "(function (integer integer integer))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(left, right, &value);
	test(value, "ordinary_simple_left3");

	extractargs(&left, "(function (integer integer integer fixnum fixnum))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(left, right, &value);
	test(value, "ordinary_simple_left4");

	extractargs(&left, "(function (integer integer integer fixnum string))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(left, right, &value);
	test(! value, "ordinary_simple_left5");

	RETURN;
}

static int test_ordinary_check(void)
{
	int value;
	addr left, right;

	extractargs(&left, "(function (integer integer &rest integer))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(left, right, &value);
	test(value, "ordinary_check1");

	extractargs(&left, "(function (integer integer &rest string))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(left, right, &value);
	test(! value, "ordinary_check2");

	RETURN;
}

static int test_subtypep_function_ordinary(void)
{
	int value;
	addr left, right, aster;

	GetTypeTable(&aster, Asterisk);
	extractargs(&left, "(function (integer integer &rest integer))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(left, right, &value);
	test(value, "subtypep_function_ordinary1");

	subtypep_function_ordinary_(aster, aster, &value);
	test(value, "subtypep_function_ordinary2");
	subtypep_function_ordinary_(left, aster, &value);
	test(value, "subtypep_function_ordinary3");
	subtypep_function_ordinary_(aster, right, &value);
	test(! value, "subtypep_function_ordinary4");

	extractargs(&left, "(function (integer integer &rest string))");
	extractargs(&right, "(function (t real &optional t &rest integer))");
	subtypep_function_ordinary_(left, right, &value);
	test(! value, "subtypep_function_ordinary5");

	RETURN;
}

static int test_subtypep_function_check(void)
{
	SubtypepResult result;
	addr left, right;

	extractchar(&left, "(function (fixnum) cons)");
	extractchar(&right, "(function (integer) list)");
	subtypep_function_check_(left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_function_check1");

	extractchar(&left, "(function * cons)");
	extractchar(&right, "(function (integer) list)");
	subtypep_function_check_(left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_function_check2");

	extractchar(&left, "(function * string)");
	extractchar(&right, "(function * list)");
	subtypep_function_check_(left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_function_check3");

	extractchar(&left, "(function * real)");
	extractchar(&right, "(function * integer)");
	subtypep_function_check_(left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_function_check4");

	extractchar(&left, "(function * *)");
	extractchar(&right, "(function * *)");
	subtypep_function_check_(left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_function_check5");

	RETURN;
}

static int test_subtypep_function(void)
{
	SubtypepResult result;
	addr left, right;

	extractchar(&left, "(function (fixnum) cons)");
	extractchar(&right, "(function (integer) list)");
	subtypep_function_(left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_function1");

	extractchar(&left, "(compiled-function (fixnum) cons)");
	extractchar(&right, "(function (integer) list)");
	subtypep_function_(left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_function2");

	extractchar(&left, "(compiled-function (string) cons)");
	extractchar(&right, "(function (integer) list)");
	subtypep_function_(left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_function3");

	extractchar(&left, "integer");
	extractchar(&right, "(function (integer) list)");
	subtypep_function_(left, right, &result);
	test(result == SUBTYPEP_EXCLUDE, "subtypep_function4");

	RETURN;
}

static int test_subtypep_compiled_function(void)
{
	SubtypepResult result;
	addr left, right;

	extractchar(&left, "(compiled-function (fixnum) cons)");
	extractchar(&right, "(compiled-function (integer) list)");
	subtypep_compiled_function_(left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_compiled_function1");

	extractchar(&left, "(function (fixnum) cons)");
	extractchar(&right, "(compiled-function (integer) list)");
	subtypep_compiled_function_(left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_compiled_function2");

	extractchar(&left, "(compiled-function (string) cons)");
	extractchar(&right, "(compiled-function (integer) list)");
	subtypep_compiled_function_(left, right, &result);
	test(result == SUBTYPEP_FALSE, "subtypep_compiled_function3");

	extractchar(&left, "integer");
	extractchar(&right, "(compiled-function (integer) list)");
	subtypep_compiled_function_(left, right, &result);
	test(result == SUBTYPEP_EXCLUDE, "subtypep_compiled_function4");

	RETURN;
}


/*
 *  subtypep_lisptype
 */
static void parse_type_string_not(addr *ret, const char *code)
{
	readstring(ret, code);
	test_parse_type(ret, *ret);
	type_copy_heap(ret, *ret);
	SetNotDecl(*ret, 1);
}

static int test_subtypep_lisptype_normal(void)
{
	SubtypepResult value;
	addr left, right;

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_lisptype_normal_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype_normal1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_lisptype_normal_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_lisptype_normal2");

	parse_type_string(&left, "real");
	parse_type_string(&right, "integer");
	subtypep_lisptype_normal_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_normal3");

	parse_type_string(&left, "integer");
	parse_type_string_not(&right, "real");
	subtypep_lisptype_normal_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_lisptype_normal4");

	parse_type_string(&left, "integer");
	parse_type_string_not(&right, "symbol");
	subtypep_lisptype_normal_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype_normal5");

	parse_type_string(&left, "real");
	parse_type_string_not(&right, "integer");
	subtypep_lisptype_normal_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_normal6");

	RETURN;
}

static int test_subtypep_lisptype_not(void)
{
	SubtypepResult value;
	addr left, right;

	parse_type_string_not(&left, "real");
	parse_type_string(&right, "integer");
	subtypep_lisptype_not_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_lisptype_not1");

	parse_type_string_not(&left, "real");
	parse_type_string_not(&right, "integer");
	subtypep_lisptype_not_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype_not2");

	parse_type_string_not(&left, "real");
	parse_type_string(&right, "symbol");
	subtypep_lisptype_not_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_not3");

	parse_type_string_not(&left, "real");
	parse_type_string_not(&right, "symbol");
	subtypep_lisptype_not_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_not4");

	parse_type_string_not(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_lisptype_not_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_not5");

	parse_type_string_not(&left, "integer");
	parse_type_string_not(&right, "real");
	subtypep_lisptype_not_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_not6");

	RETURN;
}

static int test_subtypep_lisptype(void)
{
	SubtypepResult value;
	addr left, right;

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_lisptype_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_lisptype_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_lisptype2");

	parse_type_string(&left, "integer");
	parse_type_string_not(&right, "symbol");
	subtypep_lisptype_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype3");

	parse_type_string_not(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_lisptype_(left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype4");

	RETURN;
}


/*
 *  subtypep_eql
 */
static void test_eql_character(addr *ret, unicode u, int notp)
{
	addr left, pos;

	interncommon("EQL", &left);
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

	test_eql_character(&left, 'a', 0);
	parse_type_string(&right, "character");
	subtypep_eql_type_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql_type1");

	test_eql_character(&left, 'a', 0);
	parse_type_string(&right, "symbol");
	subtypep_eql_type_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_eql_type2");

	RETURN;
}

static int test_subtypep_type_eql(void)
{
	SubtypepResult value;
	addr left, right;

	parse_type_string(&left, "character");
	test_eql_character(&right, 'a', 0);
	subtypep_type_eql_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_type_eql1");

	parse_type_string(&left, "symbol");
	test_eql_character(&right, 'a', 0);
	subtypep_type_eql_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_type_eql2");

	RETURN;
}

static int test_subtypep_eql_call(void)
{
	SubtypepResult value;
	addr left, right;

	test_eql_character(&left, 'a', 0);
	test_eql_character(&right, 'a', 0);
	subtypep_eql_call_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql_call1");

	test_eql_character(&left, 'a', 0);
	parse_type_string(&right, "character");
	subtypep_eql_call_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql_call2");

	parse_type_string(&left, "character");
	test_eql_character(&right, 'a', 0);
	subtypep_eql_call_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_eql_call3");

	RETURN;
}

static int test_subtypep_eql(void)
{
	SubtypepResult value;
	addr left, right;

	test_eql_character(&left, 'a', 0);
	test_eql_character(&right, 'a', 0);
	subtypep_eql_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql1");

	test_eql_character(&left, 'a', 0);
	test_eql_character(&right, 'b', 0);
	subtypep_eql_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_eql2");

	test_eql_character(&left, 'a', 1);
	parse_type_string(&right, "character");
	subtypep_eql_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_eql3");

	test_eql_character(&left, 'a', 0);
	parse_type_string_not(&right, "character");
	subtypep_eql_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_eql4");

	test_eql_character(&left, 'a', 1);
	parse_type_string_not(&right, "character");
	subtypep_eql_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_eql5");

	test_eql_character(&left, 'a', 0);
	parse_type_string_not(&right, "cons");
	subtypep_eql_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql6");

	parse_type_string(&left, "cons");
	test_eql_character(&right, 'a', 1);
	subtypep_eql_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_eql7");

	test_eql_character(&left, 'a', 0);
	test_eql_character(&right, 'b', 1);
	subtypep_eql_(left, right, &value);
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
	gettype_values(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "gettype_values1");
	gettype_values(pos, 1, &check);
	test(RefLispDecl(check) == LISPDECL_T, "gettype_values2");

	parse_values_string(&pos, "(values integer string)");
	gettype_values(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_INTEGER, "gettype_values3");
	gettype_values(pos, 1, &check);
	test(RefLispDecl(check) == LISPDECL_STRING, "gettype_values4");
	gettype_values(pos, 2, &check);
	test(RefLispDecl(check) == LISPDECL_T, "gettype_values5");

	parse_values_string(&pos, "(values t &optional string &rest integer)");
	gettype_values(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_T, "gettype_values6");
	gettype_values(pos, 1, &check);
	test(RefLispDecl(check) == LISPDECL_STRING, "gettype_values7");
	gettype_values(pos, 2, &check);
	test(RefLispDecl(check) == LISPDECL_INTEGER, "gettype_values8");
	gettype_values(pos, 3, &check);
	test(RefLispDecl(check) == LISPDECL_INTEGER, "gettype_values9");

	RETURN;
}

static int test_subtypep_boolean(void)
{
	int value;
	addr left, right;

	extractchar(&left, "integer");
	extractchar(&right, "real");
	subtypep_boolean_(left, right, &value);
	test(value, "subtypep_boolean1");

	extractchar(&left, "real");
	extractchar(&right, "integer");
	subtypep_boolean_(left, right, &value);
	test(! value, "subtypep_boolean2");

	extractchar(&left, "real");
	extractchar(&right, "string");
	subtypep_boolean_(left, right, &value);
	test(! value, "subtypep_boolean3");

	extractchar(&left, "real");
	extractchar(&right, "*");
	subtypep_boolean_(left, right, &value);
	test(value, "subtypep_boolean4");

	RETURN;
}

static int test_subtypep_values_values(void)
{
	int result;
	addr left, right;

	extractchar(&left, "(values)");
	extractchar(&right, "(values)");
	subtypep_values_values_(left, right, &result);
	test(result, "subtypep_values_values1");

	extractchar(&left, "(values integer)");
	extractchar(&right, "(values)");
	subtypep_values_values_(left, right, &result);
	test(result, "subtypep_values_values2");

	extractchar(&left, "(values)");
	extractchar(&right, "(values integer)");
	subtypep_values_values_(left, right, &result);
	test(! result, "subtypep_values_values3");

	extractchar(&left, "(values string)");
	extractchar(&right, "(values integer)");
	subtypep_values_values_(left, right, &result);
	test(! result, "subtypep_values_values4");

	extractchar(&left, "(values integer)");
	extractchar(&right, "(values integer)");
	subtypep_values_values_(left, right, &result);
	test(result, "subtypep_values_values5");

	extractchar(&left, "(values integer &rest string)");
	extractchar(&right, "(values integer)");
	subtypep_values_values_(left, right, &result);
	test(result, "subtypep_values_values6");

	extractchar(&left, "(values integer)");
	extractchar(&right, "(values integer &rest string)");
	subtypep_values_values_(left, right, &result);
	test(! result, "subtypep_values_values7");

	extractchar(&left, "(values real fixnum)");
	extractchar(&right, "(values real &optional integer)");
	subtypep_values_values_(left, right, &result);
	test(result, "subtypep_values_values8");

	extractchar(&left, "(values real &optional fixnum)");
	extractchar(&right, "(values real integer)");
	subtypep_values_values_(left, right, &result);
	test(result, "subtypep_values_values9");

	/*
	 *  sbcl, ccl -> true.
	 *  but (subtypep 't 'integer) -> false in second arcument.
	 */
	extractchar(&left, "(values &optional integer)");
	extractchar(&right, "(values &rest real)");
	subtypep_values_values_(left, right, &result);
	test(! result, "subtypep_values_values10");

	extractchar(&left, "(values &optional integer)");
	extractchar(&right, "(values &rest string)");
	subtypep_values_values_(left, right, &result);
	test(! result, "subtypep_values_values11");

	RETURN;
}

static int test_subtypep_values_type(void)
{
	int value;
	addr left, right;

	extractchar(&left, "(values)");
	extractchar(&right, "integer");
	subtypep_values_type_(left, right, &value);
	test(! value, "subtypep_values_type1");

	extractchar(&left, "(values integer)");
	extractchar(&right, "real");
	subtypep_values_type_(left, right, &value);
	test(value, "subtypep_values_type2");

	extractchar(&left, "(values real)");
	extractchar(&right, "integer");
	subtypep_values_type_(left, right, &value);
	test(! value, "subtypep_values_type3");

	extractchar(&left, "(values fixnum string)");
	extractchar(&right, "real");
	subtypep_values_type_(left, right, &value);
	test(value, "subtypep_values_type4");

	extractchar(&left, "(values real string)");
	extractchar(&right, "integer");
	subtypep_values_type_(left, right, &value);
	test(! value, "subtypep_values_type5");

	RETURN;
}

static int test_subtypep_type_values(void)
{
	int value;
	addr left, right;

	extractchar(&left, "integer");
	extractchar(&right, "(values)");
	subtypep_type_values_(left, right, &value);
	test(value, "subtypep_type_values1");

	extractchar(&left, "real");
	extractchar(&right, "(values integer)");
	subtypep_type_values_(left, right, &value);
	test(! value, "subtypep_type_values2");

	extractchar(&left, "integer");
	extractchar(&right, "(values real)");
	subtypep_type_values_(left, right, &value);
	test(value, "subtypep_type_values3");

	extractchar(&left, "real");
	extractchar(&right, "(values fixnum string)");
	subtypep_type_values_(left, right, &value);
	test(! value, "subtypep_type_values4");

	extractchar(&left, "integer");
	extractchar(&right, "(values real string)");
	subtypep_type_values_(left, right, &value);
	test(value, "subtypep_type_values5");

	RETURN;
}

static int test_subtypep_values_call(void)
{
	int value;
	addr left, right;

	extractchar(&left, "(values integer fixnum)");
	extractchar(&right, "(values real integer)");
	subtypep_values_call_(left, right, &value);
	test(value, "subtypep_values_call1");

	extractchar(&left, "(values integer fixnum)");
	extractchar(&right, "string");
	subtypep_values_call_(left, right, &value);
	test(! value, "subtypep_values_call2");

	extractchar(&left, "integer");
	extractchar(&right, "(values real fixnum)");
	subtypep_values_call_(left, right, &value);
	test(value, "subtypep_values_call3");

	RETURN;
}

static int test_subtypep_values(void)
{
	SubtypepResult result;
	addr left, right;

	extractchar(&left, "(values integer fixnum)");
	extractchar(&right, "(values real integer)");
	subtypep_values_(left, right, &result);
	test(result == SUBTYPEP_INCLUDE, "subtypep_values1");

	extractchar(&left, "(values integer fixnum)");
	extractchar(&right, "string");
	subtypep_values_(left, right, &result);
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

	parse_type_string(&left, "cons");
	parse_type_string(&right, "cons");
	subtypep_leftright_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_leftright1");

	parse_type_string(&left, "cons");
	parse_type_string(&right, "symbol");
	subtypep_leftright_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_leftright2");

	parse_type_string_not(&left, "cons");
	parse_type_string(&right, "symbol");
	subtypep_leftright_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_leftright3");

	parse_type_string(&left, "cons");
	parse_type_string_not(&right, "symbol");
	subtypep_leftright_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_leftright4");

	parse_type_string_not(&left, "cons");
	parse_type_string_not(&right, "symbol");
	subtypep_leftright_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_leftright5");

	RETURN;
}

static int test_subtypep_and_left(void)
{
	SubtypepResult value;
	addr left, right;

	parse_type_string(&left, "(and integer)");
	parse_type_string(&right, "real");
	subtypep_and_left_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left1");

	parse_type_string(&left, "(and integer real)");
	parse_type_string(&right, "real");
	subtypep_and_left_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left2");

	parse_type_string(&left, "(and real integer)");
	parse_type_string(&right, "integer");
	subtypep_and_left_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left3");

	parse_type_string(&left, "(and (satisfies hello) integer)");
	parse_type_string(&right, "integer");
	subtypep_and_left_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left4");

	parse_type_string(&left, "(and real rational)");
	parse_type_string(&right, "integer");
	subtypep_and_left_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_and_left5");

	parse_type_string(&left, "(and real symbol)");
	parse_type_string(&right, "integer");
	subtypep_and_left_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_left6");

	RETURN;
}

static int test_subtypep_or_left(void)
{
	SubtypepResult value;
	addr left, right;

	parse_type_string(&left, "(or integer)");
	parse_type_string(&right, "real");
	subtypep_or_left_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_left1");

	parse_type_string(&left, "(or (satisfies hello) integer)");
	parse_type_string(&right, "integer");
	subtypep_or_left_(left, right, &value);
	test(value == SUBTYPEP_INVALID, "subtypep_or_left2");

	parse_type_string(&left, "(or rational float)");
	parse_type_string(&right, "real");
	subtypep_or_left_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_left3");

	parse_type_string(&left, "(or symbol cons)");
	parse_type_string(&right, "real");
	subtypep_or_left_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_or_left4");

	parse_type_string(&left, "(or symbol integer)");
	parse_type_string(&right, "real");
	subtypep_or_left_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_or_left5");

	RETURN;
}

static int test_subtypep_satisfies_left(void)
{
	SubtypepResult value;
	addr left, right;

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

	parse_type_string(&left, "(and integer)");
	parse_type_string(&right, "real");
	subtypep_left_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_left1");

	parse_type_string(&left, "(or integer rational)");
	parse_type_string(&right, "real");
	subtypep_left_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_left2");

	parse_type_string(&left, "(satisfies hello)");
	parse_type_string(&right, "real");
	subtypep_left_(left, right, &value);
	test(value == SUBTYPEP_INVALID, "subtypep_left3");

	parse_type_string(&left, "nil");
	parse_type_string(&right, "real");
	subtypep_left_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_left4");

	parse_type_string(&left, "t");
	parse_type_string(&right, "real");
	subtypep_left_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_left5");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_left_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_left6");

	RETURN;
}

static int test_subtypep_andargs_right(void)
{
	SubtypepResult value;
	addr left, right;

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(and real)");
	subtypep_andargs_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_andargs_right1");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and real number)");
	subtypep_andargs_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_andargs_right2");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and integer number)");
	subtypep_andargs_right_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_andargs_right3");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and symbol cons)");
	subtypep_andargs_right_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_andargs_right4");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and integer cons)");
	subtypep_andargs_right_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_andargs_right5");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and real cons)");
	subtypep_andargs_right_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_andargs_right6");

	parse_type_string(&left, "rational");
	parse_type_string(&right, "(and)");
	subtypep_andargs_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_andargs_right7");

	RETURN;
}

static int test_subtypep_orargs_right(void)
{
	SubtypepResult value;
	addr left, right;

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or real)");
	subtypep_orargs_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_orargs_right1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or)");
	subtypep_orargs_right_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_orargs_right2");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or integer cons)");
	subtypep_orargs_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_orargs_right3");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or cons real)");
	subtypep_orargs_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_orargs_right4");

	parse_type_string(&left, "symbol");
	parse_type_string(&right, "(or cons real)");
	subtypep_orargs_right_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_orargs_right5");

	parse_type_string(&left, "number");
	parse_type_string(&right, "(or cons real)");
	subtypep_orargs_right_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_orargs_right6");

	RETURN;
}

static int test_subtypep_and_right(void)
{
	SubtypepResult value;
	addr left, right;

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(and integer real)");
	subtypep_and_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_right1");

	parse_type_string(&left, "(or integer real)");
	parse_type_string(&right, "(and real number)");
	subtypep_and_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_and_right2");

	parse_type_string(&left, "(or integer real symbol)");
	parse_type_string(&right, "(and real number)");
	subtypep_and_right_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_and_right3");

	parse_type_string(&left, "(or integer real symbol)");
	parse_type_string(&right, "(and cons)");
	subtypep_and_right_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_and_right4");

	RETURN;
}

static int test_subtypep_or_right(void)
{
	SubtypepResult value;
	addr left, right;

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or real symbol)");
	subtypep_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_right1");

	parse_type_string(&left, "(or integer symbol)");
	parse_type_string(&right, "(or real symbol)");
	subtypep_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_right2");

	parse_type_string(&left, "(and integer real)");
	parse_type_string(&right, "(or integer symbol)");
	subtypep_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_or_right3");

	parse_type_string(&left, "(or integer cons)");
	parse_type_string(&right, "(or real symbol)");
	subtypep_right_(left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_or_right4");

	RETURN;
}

static int test_subtypep_satisfies_right(void)
{
	SubtypepResult value;
	addr left, right;

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

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(and real number)");
	subtypep_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_right1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or real symbol)");
	subtypep_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_right2");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(satisfies hello)");
	subtypep_right_(left, right, &value);
	test(value == SUBTYPEP_INVALID, "subtypep_right3");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "nil");
	subtypep_right_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_right4");

	parse_type_string(&left, "nil");
	parse_type_string(&right, "nil");
	subtypep_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_right5");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "t");
	subtypep_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_right6");

	parse_type_string(&left, "t");
	parse_type_string(&right, "t");
	subtypep_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_right7");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_right_(left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_right8");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_right_(left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_right9");

	RETURN;
}


/*
 *  subtypep_clang
 */
static int test_subtypep_call(void)
{
	SubtypepResult value;
	addr left, right;

	parse_type_string(&left, "*");
	parse_type_string(&right, "*");
	subtypep_call_(left, right, 1, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_call1");

	parse_type_string(&left, "cons");
	parse_type_string(&right, "*");
	subtypep_call_(left, right, 1, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_call2");

	parse_type_string(&left, "*");
	parse_type_string(&right, "cons");
	subtypep_call_(left, right, 1, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_call3");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_call_(left, right, 1, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_call4");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_call_(left, right, 0, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_call5");

	RETURN;
}

static int test_subtypep_execute(void)
{
	int result, invalid;
	addr left, right;

	parse_type_string(&left, "cons");
	parse_type_string(&right, "*");
	subtypep_execute_(left, right, 1, &result, &invalid);
	test(result && invalid, "subtypep_execute1");

	parse_type_string(&left, "real");
	parse_type_string(&right, "integer");
	subtypep_execute_(left, right, 0, &result, &invalid);
	test((! result) && invalid, "subtypep_execute2");

	parse_type_string(&left, "real");
	parse_type_string(&right, "symbol");
	subtypep_execute_(left, right, 0, &result, &invalid);
	test((! result) && invalid, "subtypep_execute3");

	parse_type_string(&left, "real");
	parse_type_string(&right, "(satisfies hello)");
	subtypep_execute_(left, right, 0, &result, &invalid);
	test((! result) && (! invalid), "subtypep_execute4");

	RETURN;
}

static int test_subtypep_asterisk_clang(void)
{
	int result, invalid;
	addr left, right;

	parse_type_string(&left, "cons");
	parse_type_string(&right, "*");
	subtypep_asterisk_clang_(left, right, &result, &invalid);
	test(result && invalid, "subtypep_asterisk_clang1");

	RETURN;
}

static int test_subtypep_clang(void)
{
	int result, invalid;
	addr left, right;

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_clang_(left, right, &result, &invalid);
	test(result && invalid, "subtypep_clang1");

	parse_type_string(&left, "cons");
	parse_type_string(&right, "real");
	subtypep_clang_(left, right, &result, &invalid);
	test((! result) && invalid, "subtypep_clang2");

	parse_type_string(&left, "character");
	parse_type_string(&right, "base-char");
	subtypep_clang_(left, right, &result, &invalid);
	test((! result) && invalid, "subtypep_clang3");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(satisfies hello)");
	subtypep_clang_(left, right, &result, &invalid);
	test((! result) && (! invalid), "subtypep_clang4");

	parse_type_string(&left, "character");
	parse_type_string(&right, "extended-char");
	subtypep_clang_(left, right, &result, &invalid);
	test((! result) && invalid, "subtypep_clang5");

	parse_type_string(&left, "extended-char");
	parse_type_string(&right, "character");
	subtypep_clang_(left, right, &result, &invalid);
	test(result && invalid, "subtypep_clang6");

	parse_type_string(&left, "(integer 20 50)");
	parse_type_string(&right, "(or (integer 10 40) (integer 30 60))");
	subtypep_clang_(left, right, &result, &invalid);
	test(result && invalid, "subtypep_clang7");

	RETURN;
}


/*
 *  main
 */
static int testbreak_type_subtypep(void)
{
	/* subtypep-table */
	TestBreak(test_subtypep_clos);
	TestBreak(test_subtypep_nil);
	TestBreak(test_subtypep_t);
	TestBreak(test_subtypep_null);
	TestBreak(test_asterisk_or_t);
	TestBreak(test_subtypep_asterisk_or_t);
	TestBreak(test_subtypep_cons);
	TestBreak(test_subtypep_hash_table);
	TestBreak(test_subtypep_symbol);
	TestBreak(test_subtypep_keyword);
	TestBreak(test_subtypep_package);
	TestBreak(test_subtypep_random_state);
	TestBreak(test_subtypep_readtable);
	TestBreak(test_subtypep_pathname);
	TestBreak(test_subtypep_logical_pathname);
	TestBreak(test_subtypep_sequence);
	TestBreak(test_array_array_dimension);
	TestBreak(test_subtypep_array_array);
	TestBreak(test_subtypep_vector_array);
	TestBreak(test_subtypep_simple_vector_array);
	TestBreak(test_subtypep_string_array);
	TestBreak(test_subtypep_bit_vector_array);
	TestBreak(test_subtypep_character);
	TestBreak(test_subtypep_base_char);
	TestBreak(test_subtypep_standard_char);
	TestBreak(test_subtypep_real_less);
	TestBreak(test_subtypep_real_greater);
	TestBreak(test_subtypep_real_range);
	TestBreak(test_subtypep_realcheck);
	TestBreak(test_realexclude_left);
	TestBreak(test_realexclude_right);
	TestBreak(test_subtypep_realexclude);
	TestBreak(test_subtypep_realparameter);
	TestBreak(test_subtypep_integer);
	TestBreak(test_subtypep_rational);
	TestBreak(test_subtypep_real);
	TestBreak(test_subtypep_number);
	TestBreak(test_subtypep_float);
	TestBreak(test_subtypep_float_type);
	TestBreak(test_subtypep_short_float);
	TestBreak(test_subtypep_single_float);
	TestBreak(test_subtypep_double_float);
	TestBreak(test_subtypep_long_float);
	TestBreak(test_subtypep_ratio);
	TestBreak(test_subtypep_complex);
	/* function */
	TestBreak(test_make_function_ordinary);
	TestBreak(test_gettype_ordinary);
	TestBreak(test_ordargs_simple_p);
	TestBreak(test_ordinary_keytype);
	TestBreak(test_ordinary_valuetype);
	TestBreak(test_make_ordinary_type);
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
	/* subtypep_clang */
	TestBreak(test_subtypep_call);
	TestBreak(test_subtypep_execute);
	TestBreak(test_subtypep_asterisk_clang);
	TestBreak(test_subtypep_clang);

	return 0;
}

int test_type_subtypep(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;
	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_setjmp(ptr, &code);
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
		build_reader();
		lisp_initialize = 1;
		result = testbreak_type_subtypep();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

