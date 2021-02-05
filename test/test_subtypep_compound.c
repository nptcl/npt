#include "subtypep_compound.c"
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
#include "subtypep_optimize.h"
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
	subtypep_lisptype_normal_(ptr, left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype_normal1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_lisptype_normal_(ptr, left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_lisptype_normal2");

	parse_type_string(&left, "real");
	parse_type_string(&right, "integer");
	subtypep_lisptype_normal_(ptr, left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_normal3");

	parse_type_string(&left, "integer");
	parse_type_string_not(&right, "real");
	subtypep_lisptype_normal_(ptr, left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_lisptype_normal4");

	parse_type_string(&left, "integer");
	parse_type_string_not(&right, "symbol");
	subtypep_lisptype_normal_(ptr, left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype_normal5");

	parse_type_string(&left, "real");
	parse_type_string_not(&right, "integer");
	subtypep_lisptype_normal_(ptr, left, right, &value, subtypep_table_);
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
	subtypep_lisptype_not_(ptr, left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_lisptype_not1");

	parse_type_string_not(&left, "real");
	parse_type_string_not(&right, "integer");
	subtypep_lisptype_not_(ptr, left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype_not2");

	parse_type_string_not(&left, "real");
	parse_type_string(&right, "symbol");
	subtypep_lisptype_not_(ptr, left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_not3");

	parse_type_string_not(&left, "real");
	parse_type_string_not(&right, "symbol");
	subtypep_lisptype_not_(ptr, left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_not4");

	parse_type_string_not(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_lisptype_not_(ptr, left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_FALSE, "subtypep_lisptype_not5");

	parse_type_string_not(&left, "integer");
	parse_type_string_not(&right, "real");
	subtypep_lisptype_not_(ptr, left, right, &value, subtypep_table_);
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
	subtypep_lisptype_(ptr, left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_lisptype_(ptr, left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_lisptype2");

	parse_type_string(&left, "integer");
	parse_type_string_not(&right, "symbol");
	subtypep_lisptype_(ptr, left, right, &value, subtypep_table_);
	test(value == SUBTYPEP_INCLUDE, "subtypep_lisptype3");

	parse_type_string_not(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_lisptype_(ptr, left, right, &value, subtypep_table_);
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
	extractchar(&right, "t");
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
	test(! result, "subtypep_values_values9");

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
static int test_subtypep_atomic_not(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "cons");
	parse_type_string(&right, "cons");
	subtypep_atomic_not_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_atomic_not1");

	parse_type_string(&left, "cons");
	parse_type_string(&right, "symbol");
	subtypep_atomic_not_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_atomic_not2");

	parse_type_string_not(&left, "cons");
	parse_type_string(&right, "symbol");
	subtypep_atomic_not_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_atomic_not3");

	parse_type_string(&left, "cons");
	parse_type_string_not(&right, "symbol");
	subtypep_atomic_not_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_atomic_not4");

	parse_type_string_not(&left, "cons");
	parse_type_string_not(&right, "symbol");
	subtypep_atomic_not_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_atomic_not5");

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

static int test_subtypep_compound_right(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(and real number)");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_compound_right.1");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(or real symbol)");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_compound_right.2");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "(satisfies hello)");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INVALID, "subtypep_compound_right.3");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "nil");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_compound_right.4");

	parse_type_string(&left, "nil");
	parse_type_string(&right, "nil");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_compound_right.5");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "t");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_compound_right.6");

	parse_type_string(&left, "t");
	parse_type_string(&right, "t");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_compound_right.7");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_compound_right.8");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_compound_right.9");

	RETURN;
}


/*
 *  subtypep_check_
 */
static int test_subtypep_compound_call(void)
{
	SubtypepResult value;
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	aatype(value);

	parse_type_string(&left, "t");
	parse_type_string(&right, "t");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_compound_call.1");

	parse_type_string(&left, "cons");
	parse_type_string(&right, "t");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_compound_call.2");

	parse_type_string(&left, "t");
	parse_type_string(&right, "cons");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_FALSE, "subtypep_compound_call.3");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "real");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_compound_call.4");

	parse_type_string(&left, "integer");
	parse_type_string(&right, "symbol");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_EXCLUDE, "subtypep_compound_call.5");

	parse_type_string(&left, "(and integer)");
	parse_type_string(&right, "real");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_left1");

	parse_type_string(&left, "(or integer rational)");
	parse_type_string(&right, "real");
	subtypep_compound_(ptr, left, right, &value);
	test(value == SUBTYPEP_INCLUDE, "subtypep_left2");

	RETURN;
}


/*
 *  main
 */
static int testcase_subtypep_compound(void)
{
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
	TestBreak(test_subtypep_atomic_not);
	TestBreak(test_subtypep_satisfies_left);
	TestBreak(test_subtypep_left);
	TestBreak(test_subtypep_satisfies_right);
	TestBreak(test_subtypep_nil_right);
	TestBreak(test_subtypep_compound_right);
	/* subtypep_check_ */
	TestBreak(test_subtypep_compound_call);

	return 0;
}

static void testinit_subtypep_compound(Execute ptr)
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

int test_subtypep_compound(void)
{
	DegradeTitle;
	return DegradeCode(subtypep_compound);
}

