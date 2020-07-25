#include "type_symbol.c"
#include "character.h"
#include "control_operator.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "condition.h"
#include "degrade.h"
#include "function.h"
#include "package.h"
#include "package_symbol.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"

static int test_find_symbol_type_call(Execute ptr, addr args)
{
	GetConst(COMMON_FIXNUM, &args);
	setresult_control(ptr, args);
	return 0;
}

static int test_find_symbol_type(void)
{
	int check;
	addr pos, symbol;
	Execute ptr;

	ptr = Execute_Thread;

	/* symbol */
	GetConst(COMMON_SYMBOL, &pos);
	check = find_symbol_type(ptr, &pos, pos, Nil);
	test(check == 0, "find_symbol_type1");
	test(GetType(pos) == LISPTYPE_TYPE, "find_symbol_type2");
	test(LispDecl(pos) == LISPDECL_SYMBOL, "find_symbol_type3");

	/* clos */
	GetConst(COMMON_STANDARD_CLASS, &pos);
	check = find_symbol_type(ptr, &pos, pos, Nil);
	test(check == 0, "find_symbol_type4");
	test(GetType(pos) == LISPTYPE_TYPE, "find_symbol_type5");
	test(LispDecl(pos) == LISPDECL_CLOS, "find_symbol_type6");

	/* deftype */
	compiled_system(&pos, Nil);
	SetPointer(p_debug1, dynamic, test_find_symbol_type_call);
	setcompiled_dynamic(pos, p_debug1);
	symbol = readr("TEST-FIND-SYMBOL-TYPE");
	setdeftype_symbol(symbol, pos);
	check = find_symbol_type(ptr, &pos, symbol, Nil);
	test(check == 0, "find_symbol_type7");
	test(GetType(pos) == LISPTYPE_TYPE, "find_symbol_type8");
	test(LispDecl(pos) == LISPDECL_FIXNUM, "find_symbol_type9");

	/* no-type */
	symbol = readr("TEST-FIND-SYMBOL-NO-SUCH-TYPE");
	check = find_symbol_type(ptr, &pos, symbol, Nil);
	test(check == 0, "find_symbol_type10");
	test(pos == NULL, "find_symbol_type11");

	RETURN;
}

static int test_getdeclname(void)
{
	constindex check;

	check = getdeclname(LISPDECL_NIL);
	test(check == CONSTANT_COMMON_NIL, "getdeclname1");
	check = getdeclname(LISPDECL_FUNCTION);
	test(check == CONSTANT_COMMON_FUNCTION, "getdeclname2");
	check = getdeclname(LISPDECL_SYMBOL);
	test(check == CONSTANT_COMMON_SYMBOL, "getdeclname3");
	check = getdeclname(LISPDECL_CLOS);
	test(check == CONSTANT_EMPTY, "getdeclname4");

	RETURN;
}

static int test_type_symbol_p(void)
{
	addr pos, call;

	/* not symbol */
	fixnum_heap(&pos, 10);
	test(! type_symbol_p(pos), "type_symbol_p1");

	/* symbol type */
	pos = readr("INTEGER");
	test(type_symbol_p(pos), "type_symbol_p2");

	/* symbol no-type */
	pos = readr("VALUES");
	test(! type_symbol_p(pos), "type_symbol_p3");

	/* symbol clos */
	pos = readr("STANDARD-CLASS");
	test(type_symbol_p(pos), "type_symbol_p4");

	/* deftype */
	compiled_system(&call, Nil);
	SetPointer(p_debug1, dynamic, test_find_symbol_type_call);
	setcompiled_dynamic(call, p_debug1);
	pos = readr("TEST-FIND-SYMBOL-TYPE");
	setdeftype_symbol(pos, call);
	test(type_symbol_p(pos), "type_symbol_p5");

	RETURN;
}


/*
 *  symbol-type
 */
static int test_emptycheck(const char *name, enum LISPDECL type)
{
	int check;
	addr pos;

	internchar_debug(LISP_COMMON, name, &pos);
	check = find_symbol_type(Execute_Thread, &pos, pos, Nil);
	if (check) {
		degrade_printf("find-symbol-type error\n");
		return 0;
	}
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != type) {
		degrade_printf("lispdecl error\n");
		return 0;
	}
	if (lenarrayr(pos) != 0) {
		degrade_printf("array size error\n");
		return 0;
	}

	return 1;
}

static int type_symbol_empty(void)
{
	test(test_emptycheck("NIL", LISPDECL_NIL), "symbol_empty_nil");
	test(test_emptycheck("T", LISPDECL_T), "symbol_empty_t");
	test(test_emptycheck("ATOM", LISPDECL_ATOM), "symbol_empty_atom");
	test(test_emptycheck("LIST", LISPDECL_LIST), "symbol_empty_list");
	test(test_emptycheck("BOOLEAN", LISPDECL_BOOLEAN), "symbol_empty_boolean");
	test(test_emptycheck("NULL", LISPDECL_NULL), "symbol_empty_null");
	test(test_emptycheck("HASH-TABLE", LISPDECL_HASH_TABLE), "symbol_empty_hash_table");
	test(test_emptycheck("SYMBOL", LISPDECL_SYMBOL), "symbol_empty_symbol");
	test(test_emptycheck("KEYWORD", LISPDECL_KEYWORD), "symbol_empty_keyword");
	test(test_emptycheck("PACKAGE", LISPDECL_PACKAGE), "symbol_empty_package");
	test(test_emptycheck("RANDOM-STATE", LISPDECL_RANDOM_STATE),
			"symbol_empty_random_state");
	test(test_emptycheck("READTABLE", LISPDECL_READTABLE), "symbol_empty_readtable");
	test(test_emptycheck("PATHNAME", LISPDECL_PATHNAME), "symbol_empty_pathname");
	test(test_emptycheck("LOGICAL-PATHNAME", LISPDECL_LOGICAL_PATHNAME),
			"symbol_empty_logical_pathname");
	test(test_emptycheck("SEQUENCE", LISPDECL_SEQUENCE), "symbol_empty_sequence");
	test(test_emptycheck("CHARACTER", LISPDECL_CHARACTER), "symbol_empty_character");
	test(test_emptycheck("BASE-CHAR", LISPDECL_BASE_CHAR), "symbol_empty_base_char");
	test(test_emptycheck("EXTENDED-CHAR", LISPDECL_EXTENDED_CHAR),
			"symbol_empty_extended_char");
	test(test_emptycheck("STANDARD-CHAR", LISPDECL_STANDARD_CHAR),
			"symbol_empty_standard_char");
	test(test_emptycheck("NUMBER", LISPDECL_NUMBER), "symbol_empty_number");
	test(test_emptycheck("RATIO", LISPDECL_RATIO), "symbol_empty_ratio");
	test(test_emptycheck("BIT", LISPDECL_BIT), "symbol_empty_bit");
	test(test_emptycheck("FIXNUM", LISPDECL_FIXNUM), "symbol_empty_fixnum");
	test(test_emptycheck("BIGNUM", LISPDECL_BIGNUM), "symbol_empty_bignum");

	RETURN;
}

static int test_aster1check(const char *name, enum LISPDECL type)
{
	int check;
	addr pos;

	internchar_debug(LISP_COMMON, name, &pos);
	check = find_symbol_type(Execute_Thread, &pos, pos, Nil);
	if (check) {
		degrade_printf("find-symbol-type error\n");
		return 0;
	}
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != type) {
		degrade_printf("lispdecl error\n");
		return 0;
	}
	if (lenarrayr(pos) != 1) {
		degrade_printf("array size error\n");
		return 0;
	}
	GetArrayType(pos, 0, &pos);
	if (! type_asterisk_p(pos)) {
		degrade_printf("asterisk error\n");
		return 0;
	}

	return 1;
}

static int type_symbol_aster1(void)
{
	test(test_aster1check("SIMPLE-VECTOR",  LISPDECL_SIMPLE_VECTOR),
			"symbol_aster1_vector");
	test(test_aster1check("BIT-VECTOR",  LISPDECL_BIT_VECTOR),
			"symbol_aster1_bit_vector");
	test(test_aster1check("SIMPLE-BIT-VECTOR",  LISPDECL_SIMPLE_BIT_VECTOR),
			"symbol_aster1_simple_bit_vector");
	test(test_aster1check("STRING",  LISPDECL_STRING),
			"symbol_aster1_string");
	test(test_aster1check("BASE-STRING",  LISPDECL_BASE_STRING),
			"symbol_aster1_base_string");
	test(test_aster1check("SIMPLE-STRING",  LISPDECL_SIMPLE_STRING),
			"symbol_aster1_simple_string");
	test(test_aster1check("SIMPLE-BASE-STRING",  LISPDECL_SIMPLE_BASE_STRING),
			"symbol_aster1_simple_base_string");
	test(test_aster1check("SIGNED-BYTE",  LISPDECL_SIGNED_BYTE),
			"symbol_aster1_signed_byte");
	test(test_aster1check("UNSIGNED-BYTE",  LISPDECL_UNSIGNED_BYTE),
			"symbol_aster1_unsigned_byte");
	test(test_aster1check("COMPLEX",  LISPDECL_COMPLEX),
			"symbol_aster1_complex");

	RETURN;
}

static int test_aster2check(const char *name, enum LISPDECL type)
{
	const int size = 2;
	int check, i;
	addr pos, right;

	internchar_debug(LISP_COMMON, name, &pos);
	check = find_symbol_type(Execute_Thread, &pos, pos, Nil);
	if (check) {
		degrade_printf("find-symbol-type error\n");
		return 0;
	}
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != type) {
		degrade_printf("lispdecl error\n");
		return 0;
	}
	if (lenarrayr(pos) != (size_t)size) {
		degrade_printf("array size error\n");
		return 0;
	}
	for (i = 0; i < size; i++) {
		GetArrayType(pos, i, &right);
		if (! type_asterisk_p(right)) {
			degrade_printf("asterisk error\n");
			return 0;
		}
	}

	return 1;
}

static int type_symbol_aster2(void)
{
	test(test_aster2check("CONS", LISPDECL_CONS),
			"symbol_aster2_cons");
	test(test_aster2check("ARRAY", LISPDECL_ARRAY),
			"symbol_aster2_array");
	test(test_aster2check("SIMPLE-ARRAY", LISPDECL_SIMPLE_ARRAY),
			"symbol_aster2_simple_array");
	test(test_aster2check("VECTOR", LISPDECL_VECTOR),
			"symbol_aster2_vector");

	RETURN;
}

static int test_aster3check(const char *name, enum LISPDECL type)
{
	const int size = 3;
	int check, i;
	addr pos, right;

	internchar_debug(LISP_COMMON, name, &pos);
	check = find_symbol_type(Execute_Thread, &pos, pos, Nil);
	if (check) {
		degrade_printf("find-symbol-type error\n");
		return 0;
	}
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != type) {
		degrade_printf("lispdecl error\n");
		return 0;
	}
	if (lenarrayr(pos) != (size_t)size) {
		degrade_printf("array size error\n");
		return 0;
	}
	for (i = 0; i < size; i++) {
		GetArrayType(pos, i, &right);
		if (! type_asterisk_p(right)) {
			degrade_printf("asterisk error\n");
			return 0;
		}
	}

	return 1;
}

static int type_symbol_aster3(void)
{
	test(test_aster3check("FUNCTION", LISPDECL_FUNCTION),
			"symbol_empty_function");
	test(test_aster3check("COMPILED-FUNCTION", LISPDECL_COMPILED_FUNCTION),
			"symbol_empty_compiled_function");

	RETURN;
}

static int test_numbercheck(const char *name, enum LISPDECL type)
{
	const int size = 4;
	int check, i;
	addr pos, right;

	internchar_debug(LISP_COMMON, name, &pos);
	check = find_symbol_type(Execute_Thread, &pos, pos, Nil);
	if (check) {
		degrade_printf("find-symbol-type error\n");
		return 0;
	}
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != type) {
		degrade_printf("lispdecl error\n");
		return 0;
	}
	if (lenarrayr(pos) != (size_t)size) {
		degrade_printf("array size error\n");
		return 0;
	}
	for (i = 0; i < size; i++) {
		GetArrayType(pos, i, &right);
		if (! type_asterisk_p(right)) {
			degrade_printf("asterisk error\n");
			return 0;
		}
	}

	return 1;
}

static int type_symbol_number(void)
{
	test(test_numbercheck("REAL", LISPDECL_REAL), "symbol_number_real");
	test(test_numbercheck("RATIONAL", LISPDECL_RATIONAL), "symbol_number_rational");
	test(test_numbercheck("INTEGER", LISPDECL_INTEGER), "symbol_number_integer");
	test(test_numbercheck("FLOAT", LISPDECL_FLOAT), "symbol_number_float");
	test(test_numbercheck("SHORT-FLOAT", LISPDECL_SHORT_FLOAT),
			"symbol_number_short_float");
	test(test_numbercheck("SINGLE-FLOAT", LISPDECL_SINGLE_FLOAT),
			"symbol_number_single_float");
	test(test_numbercheck("DOUBLE-FLOAT", LISPDECL_DOUBLE_FLOAT),
			"symbol_number_double_float");
	test(test_numbercheck("LONG-FLOAT", LISPDECL_LONG_FLOAT),
			"symbol_number_long_float");

	RETURN;
}


/*
 *  main
 */
static int testbreak_type_symbol(void)
{
	TestBreak(test_find_symbol_type);
	TestBreak(test_getdeclname);
	TestBreak(test_type_symbol_p);
	/* symbol-type */
	TestBreak(type_symbol_empty);
	TestBreak(type_symbol_aster1);
	TestBreak(type_symbol_aster2);
	TestBreak(type_symbol_aster3);
	TestBreak(type_symbol_number);

	return 0;
}

int test_type_symbol(void)
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
		build_code();
		lisp_initialize = 1;
		result = testbreak_type_symbol();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

