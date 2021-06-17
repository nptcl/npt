#include "type_upgraded.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "control.h"
#include "declare.h"
#include "equal.h"
#include "degrade.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type_table.h"

/*
 *  upgraded-array-element-type
 */
static int test_build_type_upgraded(void)
{
	addr x, y;

	GetConst(ARRAY_T, &x);
	GetConst(COMMON_T, &y);
	test(x == y, "build_type_upgraded1");

	GetConst(ARRAY_BIT, &x);
	GetConst(COMMON_BIT, &y);
	test(x == y, "build_type_upgraded2");

	GetConst(ARRAY_CHARACTER, &x);
	GetConst(COMMON_CHARACTER, &y);
	test(x == y, "build_type_upgraded3");

	GetConst(ARRAY_SINGLE_FLOAT, &x);
	GetConst(COMMON_SINGLE_FLOAT, &y);
	test(x == y, "build_type_upgraded4");

	GetConst(ARRAY_DOUBLE_FLOAT, &x);
	GetConst(COMMON_DOUBLE_FLOAT, &y);
	test(x == y, "build_type_upgraded5");

	GetConst(ARRAY_LONG_FLOAT, &x);
	GetConst(COMMON_LONG_FLOAT, &y);
	test(x == y, "build_type_upgraded6");

	GetConst(ARRAY_SIGNED8, &x);
	y = readr_debug("(signed-byte 8)");
	test(equal_debug(x, y), "build_type_upgraded7");

	GetConst(ARRAY_SIGNED16, &x);
	y = readr_debug("(signed-byte 16)");
	test(equal_debug(x, y), "build_type_upgraded8");

	GetConst(ARRAY_SIGNED32, &x);
	y = readr_debug("(signed-byte 32)");
	test(equal_debug(x, y), "build_type_upgraded9");

	GetConst(ARRAY_UNSIGNED8, &x);
	y = readr_debug("(unsigned-byte 8)");
	test(equal_debug(x, y), "build_type_upgraded10");

	GetConst(ARRAY_UNSIGNED16, &x);
	y = readr_debug("(unsigned-byte 16)");
	test(equal_debug(x, y), "build_type_upgraded11");

	GetConst(ARRAY_UNSIGNED32, &x);
	y = readr_debug("(unsigned-byte 32)");
	test(equal_debug(x, y), "build_type_upgraded12");

#ifdef LISP_64BIT
	GetConst(ARRAY_SIGNED64, &x);
	y = readr_debug("(signed-byte 64)");
	test(equal_debug(x, y), "build_type_upgraded13");

	GetConst(ARRAY_UNSIGNED64, &x);
	y = readr_debug("(unsigned-byte 64)");
	test(equal_debug(x, y), "build_type_upgraded14");
#endif

	RETURN;
}

static int test_upgraded_array0_equal(void)
{
	addr x, y;

	type0_heap(LISPDECL_SYMBOL, &x);
	type0_heap(LISPDECL_KEYWORD, &y);
	test(! upgraded_array0_equal(x, y), "upgraded_array0_equal1");

	parse_type_unsafe(&x, readr_debug("integer"));
	parse_type_unsafe(&y, readr_debug("(integer 10 20)"));
	test(upgraded_array0_equal(x, y), "upgraded_array0_equal2");

	parse_type_unsafe(&x, readr_debug("(signed-byte 8)"));
	parse_type_unsafe(&y, readr_debug("(signed-byte 5)"));
	test(! upgraded_array0_equal(x, y), "upgraded_array0_equal3");

	test(upgraded_array0_equal(y, y), "upgraded_array0_equal4");

	parse_type_unsafe(&x, readr_debug("(unsigned-byte 5)"));
	parse_type_unsafe(&y, readr_debug("(signed-byte 5)"));
	test(! upgraded_array0_equal(x, y), "upgraded_array0_equal5");

	test(upgraded_array0_equal(y, y), "upgraded_array0_equal6");

	RETURN;
}

static int test_upgraded_array_unsigned(void)
{
	test(upgraded_array_unsigned(0) == 8, "upgraded_array_unsigned1");
	test(upgraded_array_unsigned(1) == 8, "upgraded_array_unsigned2");
	test(upgraded_array_unsigned(0xFF) == 8, "upgraded_array_unsigned3");
	test(upgraded_array_unsigned(0x100) == 16, "upgraded_array_unsigned4");
	test(upgraded_array_unsigned(0xFFFE) == 16, "upgraded_array_unsigned5");
	test(upgraded_array_unsigned(0xFFFF) == 16, "upgraded_array_unsigned6");
	test(upgraded_array_unsigned(0x10000) == 32, "upgraded_array_unsigned7");
	test(upgraded_array_unsigned(0xFFFFFFFF) == 32, "upgraded_array_unsigned8");
#ifdef LISP_64BIT
	test(upgraded_array_unsigned(0x100000000ULL) == 64, "upgraded_array_unsigned9");
	test(upgraded_array_unsigned(0x800000000ULL) == 64, "upgraded_array_unsigned10");
#endif

	RETURN;
}

static int test_upgraded_array_signed(void)
{
	int p, m;

	p = signplus_bignum;
	m = signminus_bignum;
	test(upgraded_array_signed(p,0) == 8, "upgraded_array_signed1p");
	test(upgraded_array_signed(p,1) == 8, "upgraded_array_signed2p");
	test(upgraded_array_signed(p,0x7F) == 8, "upgraded_array_signed3p");
	test(upgraded_array_signed(p,0x80) == 16, "upgraded_array_signed4p");
	test(upgraded_array_signed(p,0x7FFF) == 16, "upgraded_array_signed5p");
	test(upgraded_array_signed(p,0x8000) == 32, "upgraded_array_signed6p");
	test(upgraded_array_signed(p,0x7FFFFFFF) == 32, "upgraded_array_signed7p");
#ifdef LISP_64BIT
	test(upgraded_array_signed(p,0x80000000) == 64, "upgraded_array_signed8p");
	test(upgraded_array_signed(p,0x100000000ULL) == 64, "upgraded_array_signed9p");
	test(upgraded_array_signed(p,0x7FFFFFFFFFFFFFFFULL) == 64,
			"upgraded_array_signed10p");
	test(upgraded_array_signed(p,0x8000000000000000ULL) == 0,
			"upgraded_array_signed11p");
#else
	test(upgraded_array_signed(p,0x80000000) == 0, "upgraded_array_signed8p");
#endif
	test(upgraded_array_signed(m,0) == 8, "upgraded_array_signed1m");
	test(upgraded_array_signed(m,10) == 8, "upgraded_array_signed2m");
	test(upgraded_array_signed(m,0x80) == 8, "upgraded_array_signed3m");
	test(upgraded_array_signed(m,0x81) == 16, "upgraded_array_signed4m");
	test(upgraded_array_signed(m,0x8000) == 16, "upgraded_array_signed5m");
	test(upgraded_array_signed(m,0x8001) == 32, "upgraded_array_signed6m");
	test(upgraded_array_signed(m,0x80000000) == 32, "upgraded_array_signed7m");
#ifdef LISP_64BIT
	test(upgraded_array_signed(m,0x80000001) == 64, "upgraded_array_signed8m");
	test(upgraded_array_signed(m,0x100000000ULL) == 64, "upgraded_array_signed9m");
	test(upgraded_array_signed(m,0x8000000000000000ULL) == 64,
			"upgraded_array_signed10m");
	test(upgraded_array_signed(m,0x8000000000000001ULL) == 0,
			"upgraded_array_signed11m");
#else
	test(upgraded_array_signed(m,0x80000001) == 0, "upgraded_array_signed8m");
#endif

	RETURN;
}

static int test_upgraded_array_integer(void)
{
	enum ARRAY_TYPE result;
	int size;
	addr pos;

	/* asterisk */
	size = -1;
	type2integer_cd_heap(Nil, 0, &pos);
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_T, "upgraded_array_integer1");
	type2integer_ab_heap(Nil, 0, &pos);
	test(result == ARRAY_TYPE_T, "upgraded_array_integer2");

	/* left */
	type4_heap(LISPDECL_INTEGER, Nil, Nil, Nil, fixnumh(10), &pos);
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_T, "upgraded_array_integer3");

	/* right */
	type4_heap(LISPDECL_INTEGER, Nil, fixnumh(10), Nil, Nil, &pos);
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_T, "upgraded_array_integer4");

	/* signed */
	type4integer_heap(Nil, -10, Nil, 0xAAAA, &pos);
	size = -1;
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_SIGNED, "upgraded_array_integer5");
	test(size == 32, "upgraded_array_integer6");

	type4integer_heap(T, -0xAAAA, T, 20, &pos);
	size = -1;
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_SIGNED, "upgraded_array_integer7");
	test(size == 32, "upgraded_array_integer8");

	/* unsigned */
	type4integer_heap(Nil, 0, Nil, 1, &pos);
	size = -1;
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_BIT, "upgraded_array_integer9");

	type4integer_heap(T, 10, T, 20, &pos);
	size = -1;
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_UNSIGNED, "upgraded_array_integer10");
	test(size == 8, "upgraded_array_integer11");

	type4integer_heap(T, 4000, T, 20, &pos);
	size = -1;
	result = upgraded_array_integer(pos, &size);
	test(result == ARRAY_TYPE_UNSIGNED, "upgraded_array_integer12");
	test(size == 16, "upgraded_array_integer13");

	RETURN;
}

static int test_upgraded_array_decl(void)
{
	enum ARRAY_TYPE type;
	int size;
	addr pos;

	type0_heap(LISPDECL_CHARACTER, &pos);
	type = upgraded_array_decl(pos, &size);
	test(type == ARRAY_TYPE_CHARACTER, "upgraded_array_decl1");

	type0_heap(LISPDECL_BASE_CHAR, &pos);
	type = upgraded_array_decl(pos, &size);
	test(type == ARRAY_TYPE_CHARACTER, "upgraded_array_decl2");

	type0_heap(LISPDECL_STANDARD_CHAR, &pos);
	type = upgraded_array_decl(pos, &size);
	test(type == ARRAY_TYPE_CHARACTER, "upgraded_array_decl3");

	type4integer_heap(Nil, 0, Nil, 1, &pos);
	type = upgraded_array_decl(pos, &size);
	test(type == ARRAY_TYPE_BIT, "upgraded_array_decl4");

	type4integer_heap(Nil, 0, Nil, 40, &pos);
	size = -1;
	type = upgraded_array_decl(pos, &size);
	test(type == ARRAY_TYPE_UNSIGNED, "upgraded_array_decl5");
	test(size == 8, "upgraded_array_decl6");

	type4integer_heap(Nil, -70000, Nil, 40, &pos);
	size = -1;
	type = upgraded_array_decl(pos, &size);
	test(type == ARRAY_TYPE_SIGNED, "upgraded_array_decl7");
	test(size == 32, "upgraded_array_decl8");

	type4aster_heap(LISPDECL_SINGLE_FLOAT, &pos);
	type = upgraded_array_decl(pos, &size);
	test(type == ARRAY_TYPE_SINGLE_FLOAT, "upgraded_array_decl9");

	type4aster_heap(LISPDECL_DOUBLE_FLOAT, &pos);
	type = upgraded_array_decl(pos, &size);
	test(type == ARRAY_TYPE_DOUBLE_FLOAT, "upgraded_array_decl10");

	type4aster_heap(LISPDECL_LONG_FLOAT, &pos);
	type = upgraded_array_decl(pos, &size);
	test(type == ARRAY_TYPE_LONG_FLOAT, "upgraded_array_decl11");

	type0_heap(LISPDECL_SEQUENCE, &pos);
	type = upgraded_array_decl(pos, &size);
	test(type == ARRAY_TYPE_T, "upgraded_array_decl12");

	type0not_heap(LISPDECL_STANDARD_CHAR, &pos);
	type = upgraded_array_decl(pos, &size);
	test(type == ARRAY_TYPE_T, "upgraded_array_decl13");

	RETURN;
}

static int test_upgraded_array_optimize(void)
{
	int size;
	enum ARRAY_TYPE type;
	addr pos;
	LocalRoot local;

	local = Local_Thread;
	GetTypeTable(&pos, Character);
	upgraded_array_optimize_(local, pos, &type, &size);
	test(type == ARRAY_TYPE_CHARACTER, "upgraded_array_optimize1");
	pos = readr_debug("(or (integer -3 5))");
	parse_type_unsafe(&pos, pos);
	upgraded_array_optimize_(local, pos, &type, &size);
	test(type == ARRAY_TYPE_SIGNED, "upgraded_array_optimize2");
	test(size == 8, "upgraded_array_optimize3");

	RETURN;
}

static int test_upgraded_array_value(void)
{
	int size;
	enum ARRAY_TYPE type;
	addr pos;

	GetTypeTable(&pos, Character);
	upgraded_array_value_(pos, &type, &size);
	test(type == ARRAY_TYPE_CHARACTER, "upgraded_array_value1");
	pos = readr_debug("(or (integer -3 5))");
	parse_type_unsafe(&pos, pos);
	upgraded_array_value_(pos, &type, &size);
	test(type == ARRAY_TYPE_SIGNED, "upgraded_array_value2");
	test(size == 8, "upgraded_array_value3");

	RETURN;
}

static int test_upgraded_array_type_signed(void)
{
	addr pos;

	upgraded_array_type_signed(10, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "upgraded_array_type_signed1");
	test(LowLispDecl(pos) == LISPDECL_T, "upgraded_array_type_signed2");

	upgraded_array_type_signed(8, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "upgraded_array_type_signed3");
	test(LowLispDecl(pos) == LISPDECL_SIGNED_BYTE, "upgraded_array_type_signed4");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 8, "upgraded_array_type_signed5");

	upgraded_array_type_signed(16, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "upgraded_array_type_signed6");
	test(LowLispDecl(pos) == LISPDECL_SIGNED_BYTE, "upgraded_array_type_signed7");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 16, "upgraded_array_type_signed8");

	upgraded_array_type_signed(32, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "upgraded_array_type_signed9");
	test(LowLispDecl(pos) == LISPDECL_SIGNED_BYTE, "upgraded_array_type_signed10");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 32, "upgraded_array_type_signed11");

#ifdef LISP_64BIT
	upgraded_array_type_signed(64, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "upgraded_array_type_signed12");
	test(LowLispDecl(pos) == LISPDECL_SIGNED_BYTE, "upgraded_array_type_signed13");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 64, "upgraded_array_type_signed14");
#endif

	RETURN;
}

static int test_upgraded_array_type_unsigned(void)
{
	addr pos;

	upgraded_array_type_unsigned(10, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "upgraded_array_type_unsigned1");
	test(LowLispDecl(pos) == LISPDECL_T, "upgraded_array_type_unsigned2");

	upgraded_array_type_unsigned(8, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "upgraded_array_type_unsigned3");
	test(LowLispDecl(pos) == LISPDECL_UNSIGNED_BYTE, "upgraded_array_type_unsigned4");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 8, "upgraded_array_type_unsigned5");

	upgraded_array_type_unsigned(16, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "upgraded_array_type_unsigned6");
	test(LowLispDecl(pos) == LISPDECL_UNSIGNED_BYTE, "upgraded_array_type_unsigned7");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 16, "upgraded_array_type_unsigned8");

	upgraded_array_type_unsigned(32, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "upgraded_array_type_unsigned9");
	test(LowLispDecl(pos) == LISPDECL_UNSIGNED_BYTE, "upgraded_array_type_unsigned10");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 32, "upgraded_array_type_unsigned11");

#ifdef LISP_64BIT
	upgraded_array_type_unsigned(64, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "upgraded_array_type_unsigned12");
	test(LowLispDecl(pos) == LISPDECL_UNSIGNED_BYTE, "upgraded_array_type_unsigned13");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 64, "upgraded_array_type_unsigned14");
#endif

	RETURN;
}

static int test_upgraded_array_object(void)
{
	addr pos;

	upgraded_array_object(ARRAY_TYPE_CHARACTER, 999, &pos);
	test(LowLispDecl(pos) == LISPDECL_CHARACTER, "upgraded_array_object1");

	upgraded_array_object(ARRAY_TYPE_UNSIGNED, 8, &pos);
	test(LowLispDecl(pos) == LISPDECL_UNSIGNED_BYTE, "upgraded_array_object2");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 8, "upgraded_array_object3");

	RETURN;
}

static int test_upgraded_array_type(void)
{
	addr pos;

	pos = readr_debug("integer");
	parse_type_unsafe(&pos, pos);
	upgraded_array_type_(pos, &pos);
	test(LowLispDecl(pos) == LISPDECL_T, "upgraded_array_type1");

	pos = readr_debug("single-float");
	parse_type_unsafe(&pos, pos);
	upgraded_array_type_(pos, &pos);
	test(LowLispDecl(pos) == LISPDECL_SINGLE_FLOAT, "upgraded_array_type2");

	pos = readr_debug("(signed-byte 16)");
	parse_type_unsafe(&pos, pos);
	upgraded_array_type_(pos, &pos);
	test(LowLispDecl(pos) == LISPDECL_SIGNED_BYTE, "upgraded_array_type3");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 16, "upgraded_array_type4");

	pos = readr_debug("(unsigned-byte 22)");
	parse_type_unsafe(&pos, pos);
	upgraded_array_type_(pos, &pos);
	test(LowLispDecl(pos) == LISPDECL_UNSIGNED_BYTE, "upgraded_array_type5");
	GetArrayType(pos, 0, &pos);
	test(RefFixnum(pos) == 32, "upgraded_array_type6");

	RETURN;
}

static int test_upgraded_array_const_signed(void)
{
	addr x, y;

	upgraded_array_const_signed(11, &x);
	test(x == T, "upgraded_array_const_signed1");

	upgraded_array_const_signed(8, &x);
	y = readr_debug("(signed-byte 8)");
	test(equal_debug(x, y), "upgraded_array_const_signed2");

	upgraded_array_const_signed(32, &x);
	y = readr_debug("(signed-byte 32)");
	test(equal_debug(x, y), "upgraded_array_const_signed3");

	RETURN;
}

static int test_upgraded_array_const_unsigned(void)
{
	addr x, y;

	upgraded_array_const_unsigned(11, &x);
	test(x == T, "upgraded_array_const_unsigned1");

	upgraded_array_const_unsigned(8, &x);
	y = readr_debug("(unsigned-byte 8)");
	test(equal_debug(x, y), "upgraded_array_const_unsigned2");

	upgraded_array_const_unsigned(16, &x);
	y = readr_debug("(unsigned-byte 16)");
	test(equal_debug(x, y), "upgraded_array_const_unsigned3");

	RETURN;
}

static int test_upgraded_array_const(void)
{
	addr x, y;

	upgraded_array_const(ARRAY_TYPE_BIT, 999, &x);
	y = readr_debug("bit");
	test(equal_debug(x, y), "upgraded_array_const1");

	upgraded_array_const(ARRAY_TYPE_SIGNED, 8, &x);
	y = readr_debug("(signed-byte 8)");
	test(equal_debug(x, y), "upgraded_array_const2");

	RETURN;
}

static int test_upgraded_array_common(void)
{
	int check;
	addr x, y;

	x = readr_debug("(integer 10 20)");
	check = upgraded_array_common(Execute_Thread, Nil, x, &x);
	test(check == 0, "upgraded_array_common1");
	y = readr_debug("(unsigned-byte 8)");
	test(equal_debug(x, y), "upgraded_array_common2");

	x = readr_debug("readtable");
	check = upgraded_array_common(Execute_Thread, Nil, x, &x);
	test(check == 0, "upgraded_array_common3");
	test(x == T,"upgraded_array_common4");

	RETURN;
}

static int test_upgraded_array_t_local(void)
{
	addr pos;

	upgraded_array_t_local(Local_Thread, &pos);
	test(LowLispDecl(pos) == LISPDECL_T, "upgraded_array_t_local1");

	RETURN;
}

static int test_upgraded_array_bit_local(void)
{
	addr pos;

	upgraded_array_bit_local(Local_Thread, &pos);
	test(LowLispDecl(pos) == LISPDECL_BIT, "upgraded_array_bit_local1");

	RETURN;
}

static int test_upgraded_array_character_local(void)
{
	addr pos;

	upgraded_array_character_local(Local_Thread, &pos);
	test(LowLispDecl(pos) == LISPDECL_CHARACTER, "upgraded_array_character_local1");

	RETURN;
}


/*
 *  upgraded-complex-part-type
 */
static int test_upgraded_complex_type(void)
{
	addr x;
	Execute ptr;

	ptr = Execute_Thread;
	x = readr_debug("(integer 10 20)");
	parse_type_unsafe(&x, x);
	upgraded_complex_type_(ptr, Nil, x, &x);
	test(LowLispDecl(x) == LISPDECL_INTEGER, "upgraded_complex_type1");
	GetArrayType(x, 0, &x);
	test(type_asterisk_p(x), "upgraded_complex_type2");

	x = readr_debug("ratio");
	parse_type_unsafe(&x, x);
	upgraded_complex_type_(ptr, Nil, x, &x);
	test(LowLispDecl(x) == LISPDECL_RATIONAL, "upgraded_complex_type3");

	x = readr_debug("(long-float 10.0L0)");
	parse_type_unsafe(&x, x);
	upgraded_complex_type_(ptr, Nil, x, &x);
	test(LowLispDecl(x) == LISPDECL_LONG_FLOAT, "upgraded_complex_type4");

	RETURN;
}

static int test_upgraded_complex_const(void)
{
	addr x, y;
	Execute ptr;

	ptr = Execute_Thread;
	x = readr_debug("(integer 10 20)");
	parse_type_unsafe(&x, x);
	upgraded_complex_const_(ptr, Nil, x, &x);
	y = readr_debug("integer");
	test(equal_debug(x, y), "upgraded_complex_const1");

	x = readr_debug("ratio");
	parse_type_unsafe(&x, x);
	upgraded_complex_const_(ptr, Nil, x, &x);
	y = readr_debug("rational");
	test(equal_debug(x, y), "upgraded_complex_const2");

	x = readr_debug("(long-float 10.0L0)");
	parse_type_unsafe(&x, x);
	upgraded_complex_const_(ptr, Nil, x, &x);
	y = readr_debug("long-float");
	test(equal_debug(x, y), "upgraded_complex_const3");

	RETURN;
}

static int test_upgraded_complex_common(void)
{
	int check;
	addr x, y;

	x = readr_debug("(integer 10 20)");
	check = upgraded_complex_common(Execute_Thread, Nil, x, &x);
	test(check == 0, "upgraded_complex_common1");
	y = readr_debug("integer");
	test(equal_debug(x, y), "upgraded_complex_common2");

	x = readr_debug("ratio");
	parse_type_unsafe(&x, x);
	check = upgraded_complex_common(Execute_Thread, Nil, x, &x);
	test(check == 0, "upgraded_complex_common3");
	y = readr_debug("rational");
	test(equal_debug(x, y), "upgraded_complex_common4");

	x = readr_debug("(long-float 10.0L0)");
	parse_type_unsafe(&x, x);
	check = upgraded_complex_common(Execute_Thread, Nil, x, &x);
	test(check == 0, "upgraded_complex_common5");
	y = readr_debug("long-float");
	test(equal_debug(x, y), "upgraded_complex_common6");

	RETURN;
}


/*
 *  main
 */
static int testcase_type_upgraded(void)
{
	/* upgraded-array-element-type */
	TestBreak(test_build_type_upgraded);
	TestBreak(test_upgraded_array0_equal);
	TestBreak(test_upgraded_array_unsigned);
	TestBreak(test_upgraded_array_signed);
	TestBreak(test_upgraded_array_integer);
	TestBreak(test_upgraded_array_decl);
	TestBreak(test_upgraded_array_optimize);
	TestBreak(test_upgraded_array_value);
	TestBreak(test_upgraded_array_type_signed);
	TestBreak(test_upgraded_array_type_unsigned);
	TestBreak(test_upgraded_array_object);
	TestBreak(test_upgraded_array_type);
	TestBreak(test_upgraded_array_const_signed);
	TestBreak(test_upgraded_array_const_unsigned);
	TestBreak(test_upgraded_array_const);
	TestBreak(test_upgraded_array_common);
	TestBreak(test_upgraded_array_t_local);
	TestBreak(test_upgraded_array_bit_local);
	TestBreak(test_upgraded_array_character_local);

	/* upgraded-complex-part-type */
	TestBreak(test_upgraded_complex_type);
	TestBreak(test_upgraded_complex_const);
	TestBreak(test_upgraded_complex_common);

	return 0;
}

static void testinit_type_upgraded(Execute ptr)
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
	build_pathname();
	build_declare();
}

int test_type_upgraded(void)
{
	DegradeTitle;
	return DegradeCode(type_upgraded);
}

