#include "type_optimize.c"
#include "bignum_cons.h"
#include "bignum_data.h"
#include "bignum_equal.h"
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
#include "ratio.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type_table.h"

static void type_asterisk_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	type0_local(local, LISPDECL_ASTERISK, ret);
}


/*
 *  type-extract
 */
static int testlispdecl(addr pos, enum LISPDECL decl)
{
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (RefLispDecl(pos) != decl) {
		decl = RefLispDecl(pos);
		degrade_printf("lispdecl error\n");
		return 0;
	}
	return 1;
}

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

static int test_optimize_optimized(void)
{
	int check;
	addr aster, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	type_asterisk_local(local, &aster);
	type1_local(local, LISPDECL_OPTIMIZED, aster, &pos);
	optimize_optimized_(local, pos, &pos, &check);
	test(check, "optimize_optimized1");
	test(pos == aster, "optimize_optimized2");

	type_asterisk_local(local, &aster);
	type1_local(local, LISPDECL_SUBTYPEP, aster, &pos);
	optimize_optimized_(local, pos, &pos, &check);
	test(check, "optimize_optimized3");
	test(pos == aster, "optimize_optimized4");

	optimize_optimized_(local, pos, &pos, &check);
	test(! check, "optimize_optimized5");

	rollback_local(local, stack);

	RETURN;
}

static int test_optimize_not_asterisk(void)
{
	int check;
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	type_asterisk_local(local, &pos);
	optimize_not_asterisk_(local, pos, &pos, &check);
	test(! check, "optimize_not_asterisk1");

	rollback_local(local, stack);

	RETURN;
}

static int test_optimize_not_nil(void)
{
	int check;
	addr pos;
	LocalRoot local = Local_Thread;

	type0_local(local, LISPDECL_NIL, &pos);
	SetNotDecl(pos, 1);
	optimize_not_nil_(local, pos, &pos, &check);
	test(check, "optimize_not_nil1");
	test(testlispdecl(pos, LISPDECL_T), "optimize_not_nil2");

	type0_local(local, LISPDECL_NIL, &pos);
	optimize_not_nil_(local, pos, &pos, &check);
	test(! check, "optimize_not_nil3");

	type0_local(local, LISPDECL_CONS, &pos);
	optimize_not_nil_(local, pos, &pos, &check);
	test(! check, "optimize_not_nil4");

	RETURN;
}

static int test_optimize_not_t(void)
{
	int check;
	addr pos;
	LocalRoot local = Local_Thread;

	type0_local(local, LISPDECL_T, &pos);
	SetNotDecl(pos, 1);
	optimize_not_t_(local, pos, &pos, &check);
	test(check, "optimize_not_t1");
	test(testlispdecl(pos, LISPDECL_NIL), "optimize_not_t2");

	type0_local(local, LISPDECL_T, &pos);
	optimize_not_t_(local, pos, &pos, &check);
	test(! check, "optimize_not_t3");

	type0_local(local, LISPDECL_CONS, &pos);
	optimize_not_t_(local, pos, &pos, &check);
	test(! check, "optimize_not_t4");

	RETURN;
}

static int test_optimize_mod(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "(mod 32)");
	optimize_mod_(local, pos, &pos, &check);
	test(check, "optimize_mod1");
	test(testlispdecl(pos, LISPDECL_INTEGER), "optimize_mod2");
	GetArrayType(pos, 0, &value);
	test(value == Nil, "optimize_mod3");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_mod4");
	test(RefFixnum(value) == 0, "optimize_mod5");
	GetArrayType(pos, 2, &value);
	test(value == T, "optimize_mod6");
	GetArrayType(pos, 3, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_mod7");
	test(RefFixnum(value) == 32, "optimize_mod8");
	test(! RefNotDecl(pos), "optimize_mod9");

	parse_type_string(&pos, "(mod 32)");
	optimize_mod_(local, pos, &pos, &check);
	test(check, "optimize_mod10");
	SetNotDecl(pos, 1);
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "optimize_mod11");
	test(RefNotDecl(pos), "optimize_mod12");

	RETURN;
}

static int test_optimize_atom(void)
{
	int check;
	addr pos;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "atom");
	optimize_atom_(local, pos, &pos, &check);
	test(check, "optimize_atom1");
	test(RefLispDecl(pos) == LISPDECL_CONS, "optimize_atom2");
	test(RefNotDecl(pos), "optimize_atom3");

	RETURN;
}

static int test_optimize_list(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "list");
	optimize_list_(local, pos, &pos, &check);
	test(check, "optimize_list1");
	test(RefLispDecl(pos) == LISPDECL_OR, "optimize_list2");
	test(! RefNotDecl(pos), "optimize_list3");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "optimize_list4");
	GetArrayA4(pos, 0, &value);
	test(RefLispDecl(value) == LISPDECL_NULL, "optimize_list5");
	test(! RefNotDecl(value), "optimize_list6");
	GetArrayA4(pos, 1, &value);
	test(RefLispDecl(value) == LISPDECL_CONS, "optimize_list7");
	test(! RefNotDecl(value), "optimize_list8");

	RETURN;
}

static int test_optimize_boolean(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "boolean");
	optimize_boolean_(local, pos, &pos, &check);
	test(check, "optimize_boolean1");
	test(RefLispDecl(pos) == LISPDECL_OR, "optimize_boolean2");
	test(! RefNotDecl(pos), "optimize_boolean3");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "optimize_boolean4");
	GetArrayA4(pos, 0, &value);
	test(RefLispDecl(value) == LISPDECL_NULL, "optimize_boolean5");
	test(! RefNotDecl(value), "optimize_boolean6");
	GetArrayA4(pos, 1, &value);
	test(RefLispDecl(value) == LISPDECL_EQL, "optimize_boolean7");
	test(! RefNotDecl(value), "optimize_boolean8");
	GetArrayType(value, 0, &value);
	test(value == T, "optimize_boolean9");

	RETURN;
}

static int test_optimize_vector(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "vector");
	optimize_vector_(local, pos, &pos, &check);
	test(check, "optimize_vector1");
	test(RefLispDecl(pos) == LISPDECL_ARRAY, "optimize_vector2");
	test(! RefNotDecl(pos), "optimize_vector3");
	GetArrayType(pos, 0, &value);
	test(type_asterisk_p(value), "optimize_vector4");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_vector5");
	test(RefFixnum(value) == 1, "optimize_vector6");

	parse_type_string(&pos, "(vector integer 5)");
	optimize_vector_(local, pos, &pos, &check);
	test(check, "optimize_vector7");
	test(RefLispDecl(pos) == LISPDECL_ARRAY, "optimize_vector8");
	test(! RefNotDecl(pos), "optimize_vector9");
	GetArrayType(pos, 0, &value);
	test(RefLispDecl(value) == LISPDECL_T, "optimize_vector10");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_VECTOR, "optimize_vector11");
	test(lenarrayr(value) == 1, "optimize_vector12");
	GetArrayA4(value, 0, &value);
	test(RefFixnum(value) == 5, "optimize_vector13");

	RETURN;
}

static int test_optimize_simple_vector(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "simple-vector");
	optimize_simple_vector_(local, pos, &pos, &check);
	test(check, "optimize_simple_vector1");
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "optimize_simple_vector2");
	test(! RefNotDecl(pos), "optimize_simple_vector3");
	GetArrayType(pos, 0, &value);
	test(RefLispDecl(value) == LISPDECL_T, "optimize_simple_vector4");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_simple_vector5");
	test(RefFixnum(value) == 1, "optimize_simple_vector6");

	parse_type_string(&pos, "(simple-vector 5)");
	optimize_simple_vector_(local, pos, &pos, &check);
	test(check, "optimize_simple_vector7");
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "optimize_simple_vector8");
	test(! RefNotDecl(pos), "optimize_simple_vector9");
	GetArrayType(pos, 0, &value);
	test(RefLispDecl(value) == LISPDECL_T, "optimize_simple_vector10");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_VECTOR, "optimize_simple_vector11");
	test(lenarrayr(value) == 1, "optimize_simple_vector12");
	GetArrayA4(value, 0, &value);
	test(RefFixnum(value) == 5, "optimize_simple_vector13");

	RETURN;
}

static int test_optimize_bit_vector(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "bit-vector");
	optimize_bit_vector_(local, pos, &pos, &check);
	test(check, "optimize_bit_vector1");
	test(RefLispDecl(pos) == LISPDECL_ARRAY, "optimize_bit_vector2");
	test(! RefNotDecl(pos), "optimize_bit_vector3");
	GetArrayType(pos, 0, &value);
	test(testlispdecl(value, LISPDECL_BIT), "optimize_bit_vector4");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_bit_vector5");
	test(RefFixnum(value) == 1, "optimize_bit_vector6");

	parse_type_string(&pos, "(bit-vector 5)");
	optimize_bit_vector_(local, pos, &pos, &check);
	test(check, "optimize_bit_vector7");
	test(RefLispDecl(pos) == LISPDECL_ARRAY, "optimize_bit_vector8");
	test(! RefNotDecl(pos), "optimize_bit_vector9");
	GetArrayType(pos, 0, &value);
	test(testlispdecl(value, LISPDECL_BIT), "optimize_bit_vector10");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_VECTOR, "optimize_bit_vector11");
	test(lenarrayr(value) == 1, "optimize_bit_vector12");
	GetArrayA4(value, 0, &value);
	test(RefFixnum(value) == 5, "optimize_bit_vector13");

	RETURN;
}

static int test_optimize_simple_bit_vector(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "simple-bit-vector");
	optimize_simple_bit_vector_(local, pos, &pos, &check);
	test(check, "optimize_simple_bit_vector1");
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "optimize_simple_bit_vector2");
	test(! RefNotDecl(pos), "optimize_simple_bit_vector3");
	GetArrayType(pos, 0, &value);
	test(testlispdecl(value, LISPDECL_BIT), "optimize_simple_bit_vector4");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_simple_bit_vector5");
	test(RefFixnum(value) == 1, "optimize_simple_bit_vector6");

	parse_type_string(&pos, "(simple-bit-vector 5)");
	optimize_simple_bit_vector_(local, pos, &pos, &check);
	test(check, "optimize_simple_bit_vector7");
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "optimize_simple_bit_vector8");
	test(! RefNotDecl(pos), "optimize_simple_bit_vector9");
	GetArrayType(pos, 0, &value);
	test(testlispdecl(value, LISPDECL_BIT), "optimize_simple_bit_vector10");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_VECTOR, "optimize_simple_bit_vector11");
	test(lenarrayr(value) == 1, "optimize_simple_bit_vector12");
	GetArrayA4(value, 0, &value);
	test(RefFixnum(value) == 5, "optimize_simple_bit_vector13");

	RETURN;
}

static int test_optimize_extended_char(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "extended-char");
	type_optimize_(local, pos, &pos, &check);
	test(check, "optimize_extended_char1");
	test(GetType(pos) == LISPTYPE_TYPE, "optimize_extended_char2");
	test(RefLispDecl(pos) == LISPDECL_AND, "optimize_extended_char3");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "optimize_extended_char4");
	GetArrayA4(pos, 0, &value);
	test(RefLispDecl(value) == LISPDECL_CHARACTER, "optimize_extended_char5");
	test(! RefNotDecl(value), "optimize_extended_char6");
	GetArrayA4(pos, 1, &value);
	test(RefLispDecl(value) == LISPDECL_BASE_CHAR, "optimize_extended_char7");
	test(RefNotDecl(value), "optimize_extended_char8");

	parse_type_string(&pos, "extended-char");
	type_copy_local(local, &pos, pos);
	SetNotDecl(pos, 1);
	type_optimize_(local, pos, &pos, &check);
	test(check, "optimize_extended_char9");
	test(GetType(pos) == LISPTYPE_TYPE, "optimize_extended_char10");
	test(RefLispDecl(pos) == LISPDECL_OR, "optimize_extended_char11");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "optimize_extended_char12");
	GetArrayA4(pos, 0, &value);
	test(RefLispDecl(value) == LISPDECL_CHARACTER, "optimize_extended_char13");
	test(RefNotDecl(value), "optimize_extended_char14");
	GetArrayA4(pos, 1, &value);
	test(RefLispDecl(value) == LISPDECL_BASE_CHAR, "optimize_extended_char15");
	test(! RefNotDecl(value), "optimize_extended_char16");

	RETURN;
}

static int test_optimize_string(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "string");
	type_optimize_(local, pos, &pos, &check);
	test(RefLispDecl(pos) == LISPDECL_ARRAY, "optimize_string1");
	GetArrayType(pos, 0, &value);
	test(LispDecl(value) == LISPDECL_CHARACTER, "optimize_string2");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_string3");
	test(RefFixnum(value) == 1, "optimize_string4");

	parse_type_string(&pos, "(string 10)");
	type_optimize_(local, pos, &pos, &check);
	test(RefLispDecl(pos) == LISPDECL_ARRAY, "optimize_string5");
	GetArrayType(pos, 0, &value);
	test(LispDecl(value) == LISPDECL_CHARACTER, "optimize_string6");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_VECTOR, "optimize_string7");
	test(lenarrayr(value) == 1, "optimize_string8");
	GetArrayA4(value, 0, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_string9");
	test(RefFixnum(value) == 10, "optimize_string10");

	RETURN;
}

static int test_optimize_base_string(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "base-string");
	type_optimize_(local, pos, &pos, &check);
	test(RefLispDecl(pos) == LISPDECL_ARRAY, "optimize_base_string1");
	GetArrayType(pos, 0, &value);
	test(LispDecl(value) == LISPDECL_CHARACTER, "optimize_base_string2");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_base_string3");
	test(RefFixnum(value) == 1, "optimize_base_string4");

	parse_type_string(&pos, "(base-string 10)");
	type_copy_local(local, &pos, pos);
	SetNotDecl(pos, 1);
	type_optimize_(local, pos, &pos, &check);
	test(RefLispDecl(pos) == LISPDECL_ARRAY, "optimize_base_string5");
	GetArrayType(pos, 0, &value);
	test(LispDecl(value) == LISPDECL_CHARACTER, "optimize_base_string6");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_VECTOR, "optimize_base_string7");
	test(lenarrayr(value) == 1, "optimize_base_string8");
	GetArrayA4(value, 0, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_base_string9");
	test(RefFixnum(value) == 10, "optimize_base_string10");

	RETURN;
}

static int test_optimize_simple_string(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "simple-string");
	type_optimize_(local, pos, &pos, &check);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "optimize_simple_string1");
	GetArrayType(pos, 0, &value);
	test(LispDecl(value) == LISPDECL_CHARACTER, "optimize_simple_string2");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_simple_string3");
	test(RefFixnum(value) == 1, "optimize_simple_string4");

	parse_type_string(&pos, "(simple-string 10)");
	type_copy_local(local, &pos, pos);
	SetNotDecl(pos, 1);
	type_optimize_(local, pos, &pos, &check);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "optimize_simple_string5");
	GetArrayType(pos, 0, &value);
	test(LispDecl(value) == LISPDECL_CHARACTER, "optimize_simple_string6");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_VECTOR, "optimize_simple_string7");
	test(lenarrayr(value) == 1, "optimize_simple_string8");
	GetArrayA4(value, 0, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_simple_string9");
	test(RefFixnum(value) == 10, "optimize_simple_string10");

	RETURN;
}

static int test_optimize_simple_base_string(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "simple-base-string");
	type_optimize_(local, pos, &pos, &check);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "optimize_simple_base_string1");
	GetArrayType(pos, 0, &value);
	test(LispDecl(value) == LISPDECL_CHARACTER, "optimize_simple_base_string2");
	GetArrayType(pos, 1, &value);
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_simple_base_string3");
	test(RefFixnum(value) == 1, "optimize_simple_base_string4");

	parse_type_string(&pos, "(simple-base-string 10)");
	type_copy_local(local, &pos, pos);
	SetNotDecl(pos, 1);
	type_optimize_(local, pos, &pos, &check);
	test(RefLispDecl(pos) == LISPDECL_SIMPLE_ARRAY, "optimize_simple_base_string5");
	GetArrayType(pos, 0, &value);
	test(LispDecl(value) == LISPDECL_CHARACTER, "optimize_simple_base_string6");
	GetArrayType(pos, 1, &value);
	test(GetType(value) == LISPTYPE_VECTOR, "optimize_simple_base_string7");
	test(lenarrayr(value) == 1, "optimize_simple_base_string8");
	GetArrayA4(value, 0, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_simple_base_string9");
	test(RefFixnum(value) == 10, "optimize_simple_base_string10");

	RETURN;
}

static int test_integernilnil(addr pos, fixnum a, fixnum b)
{
	addr check;

	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error.\n");
		return 0;
	}
	if (RefLispDecl(pos) != LISPDECL_INTEGER) {
		degrade_printf("type decl error.\n");
		return 0;
	}
	GetArrayType(pos, 0, &check);
	if (check != Nil) {
		degrade_printf("left nil error.\n");
		return 0;
	}
	GetArrayType(pos, 1, &check);
	if (GetType(check) != LISPTYPE_FIXNUM) {
		degrade_printf("left fixnum type error.\n");
		return 0;
	}
	if (RefFixnum(check) != a) {
		degrade_printf("left fixnum value error.\n");
		return 0;
	}
	GetArrayType(pos, 2, &check);
	if (check != Nil) {
		degrade_printf("right nil error.\n");
		return 0;
	}
	GetArrayType(pos, 3, &check);
	if (GetType(check) != LISPTYPE_FIXNUM) {
		degrade_printf("right fixnum type error.\n");
		return 0;
	}
	if (RefFixnum(check) != b) {
		degrade_printf("right fixnum value error.\n");
		return 0;
	}

	return 1;
}

static int equal_bignum_char(addr left, int sign, unsigned base, const char *str)
{
	int result;
	addr right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bigcons_char_local_(local, &right, base, str);
	bignum_cons_alloc(local, &right, sign, right);
	result = equal_bb_real(left, right);
	rollback_local(local, stack);

	return result;
}

static int test_integernilnil_bignum(addr pos, const char *str1, const char *str2)
{
	addr check;

	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error.\n");
		return 0;
	}
	if (RefLispDecl(pos) != LISPDECL_INTEGER) {
		degrade_printf("type decl error.\n");
		return 0;
	}
	GetArrayType(pos, 0, &check);
	if (check != Nil) {
		degrade_printf("left nil error.\n");
		return 0;
	}
	GetArrayType(pos, 1, &check);
	if (GetType(check) != LISPTYPE_BIGNUM) {
		degrade_printf("left bignum type error.\n");
		return 0;
	}
	if (! equal_bignum_char(check, SignMinus, 16, str1)) {
		degrade_printf("left bignum value error.\n");
		return 0;
	}
	GetArrayType(pos, 2, &check);
	if (check != Nil) {
		degrade_printf("right nil error.\n");
		return 0;
	}
	GetArrayType(pos, 3, &check);
	if (GetType(check) != LISPTYPE_BIGNUM) {
		degrade_printf("right bignum type error.\n");
		return 0;
	}
	if (! equal_bignum_char(check, SignPlus, 16, str2)) {
		degrade_printf("right bignum value error.\n");
		return 0;
	}

	return 1;
}

static int integer_asterisk_p(addr pos)
{
	addr check;

	if (! testlispdecl(pos, LISPDECL_INTEGER))
		return 0;
	GetArrayType(pos, 0, &check);
	if (RefLispDecl(check) != LISPDECL_ASTERISK)
		return 0;
	GetArrayType(pos, 1, &check);
	if (RefLispDecl(check) != LISPDECL_ASTERISK)
		return 0;
	GetArrayType(pos, 2, &check);
	if (RefLispDecl(check) != LISPDECL_ASTERISK)
		return 0;
	GetArrayType(pos, 3, &check);
	if (RefLispDecl(check) != LISPDECL_ASTERISK)
		return 0;

	return 1;
}

static int test_optimize_signed_byte(void)
{
	int check;
	addr pos, pos1, pos2;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	parse_type_string(&pos, "signed-byte");
	optimize_signed_byte_(local, pos, &pos, &check);
	test(integer_asterisk_p(pos), "optimize_signed_byte1");

	parse_type_string(&pos, "(signed-byte *)");
	optimize_signed_byte_(local, pos, &pos, &check);
	test(integer_asterisk_p(pos), "optimize_signed_byte2");

	parse_type_string(&pos, "(signed-byte 8)");
	optimize_signed_byte_(local, pos, &pos, &check);
	test(test_integernilnil(pos, -128, 127), "optimize_signed_byte3");

	parse_type_string(&pos, "(signed-byte 1)");
	optimize_signed_byte_(local, pos, &pos, &check);
	test(test_integernilnil(pos, -1, 0), "optimize_signed_byte4");
	test(! RefNotDecl(pos), "optimize_signed_byte5");

	interncommon_debug("SIGNED-BYTE", &pos1);
	fixnum_heap(&pos2, BIGNUM_FULLBIT);
	list_heap(&pos, pos1, pos2, NULL);
	test_parse_type(&pos, pos);
	optimize_signed_byte_(local, pos, &pos, &check);
	test(test_integernilnil(pos, FIXNUM_MIN, FIXNUM_MAX), "optimize_signed_byte6");

	parse_type_string(&pos, "(signed-byte 65)");
	optimize_signed_byte_(local, pos, &pos, &check);
	check = test_integernilnil_bignum(pos, "10000000000000000", "FFFFFFFFFFFFFFFF");
	test(check, "optimize_signed_byte7");

	parse_type_string(&pos, "(signed-byte 200)");
	optimize_signed_byte_(local, pos, &pos, &check);
	check = test_integernilnil_bignum(pos,
			"80000000000000000000000000000000000000000000000000",
			"7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
	test(check, "optimize_signed_byte8");

	rollback_local(local, stack);

	RETURN;
}

static int integer_zero_check(addr pos)
{
	addr check;

	if (RefLispDecl(pos) != LISPDECL_INTEGER)
		return 0;
	GetArrayType(pos, 0, &check);
	if (check != Nil)
		return 0;
	GetArrayType(pos, 1, &check);
	if (! zerop_integer_debug(check))
		return 0;
	GetArrayType(pos, 2, &check);
	if (RefLispDecl(check) != LISPDECL_ASTERISK)
		return 0;
	GetArrayType(pos, 3, &check);
	if (RefLispDecl(check) != LISPDECL_ASTERISK)
		return 0;

	return 1;
}

static int test_integerhalf_bignum(addr pos, const char *str)
{
	addr check;

	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error.\n");
		return 0;
	}
	if (RefLispDecl(pos) != LISPDECL_INTEGER) {
		degrade_printf("type decl error.\n");
		return 0;
	}
	GetArrayType(pos, 0, &check);
	if (check != Nil) {
		degrade_printf("left nil error.\n");
		return 0;
	}
	GetArrayType(pos, 1, &check);
	if (! zerop_integer_debug(check)) {
		degrade_printf("left value error.\n");
		return 0;
	}
	GetArrayType(pos, 2, &check);
	if (check != Nil) {
		degrade_printf("right nil error.\n");
		return 0;
	}
	GetArrayType(pos, 3, &check);
	if (GetType(check) != LISPTYPE_BIGNUM) {
		degrade_printf("right bignum type error.\n");
		return 0;
	}
	if (! equal_bignum_char(check, SignPlus, 16, str)) {
		degrade_printf("right bignum value error.\n");
		return 0;
	}

	return 1;
}

static int test_optimize_unsigned_byte(void)
{
	int check;
	addr pos, pos1, pos2;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	parse_type_string(&pos, "unsigned-byte");
	optimize_unsigned_byte_(local, pos, &pos, &check);
	test(integer_zero_check(pos), "optimize_unsigned_byte1");

	parse_type_string(&pos, "(unsigned-byte *)");
	optimize_unsigned_byte_(local, pos, &pos, &check);
	test(integer_zero_check(pos), "optimize_unsigned_byte2");

	parse_type_string(&pos, "(unsigned-byte 8)");
	optimize_unsigned_byte_(local, pos, &pos, &check);
	test(test_integernilnil(pos, 0, 255), "optimize_unsigned_byte3");

	parse_type_string(&pos, "(unsigned-byte 1)");
	optimize_unsigned_byte_(local, pos, &pos, &check);
	test(test_integernilnil(pos, 0, 1), "optimize_unsigned_byte4");

	interncommon_debug("UNSIGNED-BYTE", &pos1);
	fixnum_heap(&pos2, BIGNUM_FULLBIT - 1UL);
	list_heap(&pos, pos1, pos2, NULL);
	test_parse_type(&pos, pos);
	optimize_unsigned_byte_(local, pos, &pos, &check);
	test(test_integernilnil(pos, 0, FIXNUM_MAX), "optimize_unsigned_byte5");

	parse_type_string(&pos, "(unsigned-byte 65)");
	optimize_unsigned_byte_(local, pos, &pos, &check);
	check = test_integerhalf_bignum(pos, "1FFFFFFFFFFFFFFFF");
	test(check, "optimize_unsigned_byte6");

	parse_type_string(&pos, "(unsigned-byte 123)");
	optimize_unsigned_byte_(local, pos, &pos, &check);
	check = test_integerhalf_bignum(pos, "7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
	test(check, "optimize_unsigned_byte7");

	rollback_local(local, stack);

	RETURN;
}

static int test_optimize_bit(void)
{
	int check;
	addr pos;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "bit");
	optimize_bit_(local, pos, &pos, &check);
	test(test_integernilnil(pos, 0, 1), "optimize_bit1");
	test(! RefNotDecl(pos), "optimize_bit2");

	parse_type_string(&pos, "bit");
	optimize_bit_(local, pos, &pos, &check);
	test(test_integernilnil(pos, 0, 1), "optimize_bit3");
	test(! RefNotDecl(pos), "optimize_bit4");

	RETURN;
}

static int integer_fixnum_p(addr pos)
{
	return test_integernilnil(pos, FIXNUM_MIN, FIXNUM_MAX);
}

static int test_optimize_fixnum(void)
{
	int check;
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	parse_type_string(&pos, "fixnum");
	optimize_fixnum_(local, pos, &pos, &check);
	test(integer_fixnum_p(pos), "optimize_fixnum1");
	test(! RefNotDecl(pos), "optimize_fixnum2");

	parse_type_string(&pos, "fixnum");
	type_copy_local(local, &pos, pos);
	SetNotDecl(pos, 1);
	type_optimize_heap_(local, pos, &pos, &check);

	parse_type_string(&pos, "fixnum");
	optimize_fixnum_(local, pos, &pos, &check);
	test(integer_fixnum_p(pos), "optimize_fixnum3");
	test(! RefNotDecl(pos), "optimize_fixnum4");

	rollback_local(local, stack);

	RETURN;
}

static int test_optimize_bignum(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "bignum");
	optimize_bignum_(local, pos, &pos, &check);
	test(testlispdecl(pos, LISPDECL_AND), "optimize_bignum1");
	test(! RefNotDecl(pos), "optimize_bignum2");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "optimize_bignum3");
	GetArrayA4(pos, 0, &value);
	test(integer_asterisk_p(value), "optimize_bignum4");
	test(! RefNotDecl(value), "optimize_bignum5");
	GetArrayA4(pos, 1, &value);
	test(integer_fixnum_p(value), "optimize_bignum6");
	test(RefNotDecl(value), "optimize_bignum7");

	parse_type_string(&pos, "bignum");
	type_copy_local(local, &pos, pos);
	SetNotDecl(pos, 1);

	parse_type_string(&pos, "bignum");
	optimize_bignum_(local, pos, &pos, &check);
	test(testlispdecl(pos, LISPDECL_AND), "optimize_bignum8");
	test(! RefNotDecl(pos), "optimize_bignum9");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "optimize_bignum10");
	GetArrayA4(pos, 0, &value);
	test(integer_asterisk_p(value), "optimize_bignum11");
	test(! RefNotDecl(value), "optimize_bignum12");
	GetArrayA4(pos, 1, &value);
	test(integer_fixnum_p(value), "optimize_bignum13");
	test(RefNotDecl(value), "optimize_bignum14");

	RETURN;
}

static int test_optimize_eql(void)
{
	int check;
	addr pos;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "(eql nil)");
	optimize_eql_(local, pos, &pos, &check);
	test(check, "optimize_eql1");
	test(testlispdecl(pos, LISPDECL_NULL), "optimize_eql2");
	test(! RefNotDecl(pos), "optimize_eql3");

	parse_type_string(&pos, "(eql 100)");
	optimize_eql_(local, pos, &pos, &check);
	test(! check, "optimize_eql4");

	RETURN;
}

static int test_optimize_eql_range(void)
{
	int check;
	addr pos, value, numer, denom;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	list_heap(&pos, interncommonr_debug("EQL"), fixnumh(10), NULL);
	test_parse_type(&pos, pos);
	optimize_eql_range_(local, pos, &pos, &check);
	test(check, "optimize_eql_range1");
	test(testlispdecl(pos, LISPDECL_INTEGER), "optimize_eql_range2");
	test(! RefNotDecl(pos), "optimize_eql_range3");
	GetArrayType(pos, 0, &value);
	test(value == Nil, "optimize_eql_range4");
	GetArrayType(pos, 1, &value);
	test(RefFixnum(value) == 10, "optimize_eql_range5");
	GetArrayType(pos, 2, &value);
	test(value == Nil, "optimize_eql_range6");
	GetArrayType(pos, 3, &value);
	test(RefFixnum(value) == 10, "optimize_eql_range7");

	bignum_value_alloc(local, &numer, signplus_bignum, 12);
	bignum_value_alloc(local, &denom, signplus_bignum, 7);
	make_ratio_reduction_local(local, &pos, signplus_bignum, numer, denom);
	list_local(local, &pos, interncommonr_debug("EQL"), pos, NULL);
	test_parse_type(&pos, pos);
	optimize_eql_range_(local, pos, &pos, &check);
	test(check, "optimize_eql_range8");
	test(testlispdecl(pos, LISPDECL_RATIONAL), "optimize_eql_range9");
	rollback_local(local, stack);

	single_float_heap(&pos, 20.0f);
	list_heap(&pos, interncommonr_debug("EQL"), pos, NULL);
	test_parse_type(&pos, pos);
	optimize_eql_range_(local, pos, &pos, &check);
	test(check, "optimize_eql_range10");
	test(testlispdecl(pos, LISPDECL_SINGLE_FLOAT), "optimize_eql_range11");

	double_float_heap(&pos, 20.0);
	list_heap(&pos, interncommonr_debug("EQL"), pos, NULL);
	test_parse_type(&pos, pos);
	optimize_eql_range_(local, pos, &pos, &check);
	test(check, "optimize_eql_range12");
	test(testlispdecl(pos, LISPDECL_DOUBLE_FLOAT), "optimize_eql_range13");

	long_float_heap(&pos, 20.0L);
	list_heap(&pos, interncommonr_debug("EQL"), pos, NULL);
	test_parse_type(&pos, pos);
	optimize_eql_range_(local, pos, &pos, &check);
	test(check, "optimize_eql_range14");
	test(testlispdecl(pos, LISPDECL_LONG_FLOAT), "optimize_eql_range15");

	character_heap(&pos, 'A');
	list_heap(&pos, interncommonr_debug("EQL"), pos, NULL);
	test_parse_type(&pos, pos);
	optimize_eql_range_(local, pos, &pos, &check);
	test(! check, "optimize_eql_range16");

	RETURN;
}

static int test_optimize_member1(void)
{
	int check;
	addr pos;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "(member)");
	optimize_member1_(local, pos, &pos, &check);
	test(check, "optimize_member1-1");
	test(testlispdecl(pos, LISPDECL_NIL), "optimize_member1-2");
	test(! RefNotDecl(pos), "optimize_member1-3");

	parse_type_string(&pos, "(member 100)");
	optimize_member1_(local, pos, &pos, &check);
	test(! check, "optimize_member1-4");

	RETURN;
}

static int test_optimize_member2(void)
{
	int check;
	addr pos;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "(member 10)");
	optimize_member2_(local, pos, &pos, &check);
	test(check, "optimize_member2-1");
	test(testlispdecl(pos, LISPDECL_EQL), "optimize_member2-2");
	test(! RefNotDecl(pos), "optimize_member2-3");
	GetArrayType(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "optimize_member2-4");
	test(RefFixnum(pos) == 10, "optimize_member2-5");

	parse_type_string(&pos, "(member)");
	optimize_member2_(local, pos, &pos, &check);
	test(! check, "optimize_member2-6");

	RETURN;
}

static int test_optimize_member3(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "(member 10 20)");
	optimize_member3_(local, pos, &pos, &check);
	test(check, "optimize_member3-1");
	test(testlispdecl(pos, LISPDECL_OR), "optimize_member3-2");
	test(! RefNotDecl(pos), "optimize_member3-3");
	GetArrayType(pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "optimize_member3-4");
	test(lenarrayr(pos) == 2, "optimize_member3-5");

	GetArrayA4(pos, 0, &value);
	test(testlispdecl(value, LISPDECL_EQL), "optimize_member3-6");
	GetArrayType(value, 0, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_member3-7");
	test(RefFixnum(value) == 10, "optimize_member3-8");

	GetArrayA4(pos, 1, &value);
	test(testlispdecl(value, LISPDECL_EQL), "optimize_member3-9");
	GetArrayType(value, 0, &value);
	test(GetType(value) == LISPTYPE_FIXNUM, "optimize_member3-10");
	test(RefFixnum(value) == 20, "optimize_member3-11");

	parse_type_string(&pos, "(member 10)");
	optimize_member3_(local, pos, &pos, &check);
	test(! check, "optimize_member3-12");

	RETURN;
}

static int test_optimize_not(void)
{
	int check;
	addr pos;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "(not cons)");
	type_optimize_(local, pos, &pos, &check);
	test(testlispdecl(pos, LISPDECL_CONS), "optimize_not1");
	test(RefNotDecl(pos), "optimize_not2");

	parse_type_string(&pos, "(not cons)");
	type_copy_local(local, &pos, pos);
	SetNotDecl(pos, 1);
	type_optimize_(local, pos, &pos, &check);
	test(testlispdecl(pos, LISPDECL_CONS), "optimize_not3");
	test(! RefNotDecl(pos), "optimize_not4");

	RETURN;
}

static int test_optimize_result(void)
{
	int check;
	addr pos;
	LocalRoot local = Local_Thread;

	check = 0;

	parse_type_string(&pos, "nil");
	optimize_result_(local, pos, &pos, &check);
	test(! check, "optimize_result1");
	test(testlispdecl(pos, LISPDECL_NIL), "optimize_result2");

	parse_type_string(&pos, "atom");
	optimize_result_(local, pos, &pos, &check);
	test(check, "optimize_result3");
	test(testlispdecl(pos, LISPDECL_CONS), "optimize_result4");
	test(RefNotDecl(pos), "optimize_result5");

	RETURN;
}

static int test_extract_not_andor(void)
{
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "(and cons null)");
	extract_not_andor_(local, &pos, pos, LISPDECL_OR);
	test(testlispdecl(pos, LISPDECL_OR), "extract_not_andor1");
	test(! RefNotDecl(pos), "extract_not_andor2");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "extract_not_andor3");
	GetArrayA4(pos, 0, &value);
	test(testlispdecl(value, LISPDECL_CONS), "extract_not_andor4");
	test(RefNotDecl(value), "extract_not_andor5");
	GetArrayA4(pos, 1, &value);
	test(testlispdecl(value, LISPDECL_NULL), "extract_not_andor6");

	RETURN;
}

static int test_extract_array_andor(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "(and cons (not null))");
	extract_array_andor_(local, pos, &pos, &check);
	test(check, "extract_array_andor1");
	test(testlispdecl(pos, LISPDECL_AND), "extract_array_andor2");
	test(! RefNotDecl(pos), "extract_array_andor3");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "extract_array_andor4");
	GetArrayA4(pos, 0, &value);
	test(testlispdecl(value, LISPDECL_CONS), "extract_array_andor5");
	test(! RefNotDecl(value), "extract_array_andor6");
	GetArrayA4(pos, 1, &value);
	test(testlispdecl(value, LISPDECL_NULL), "extract_array_andor7");
	test(RefNotDecl(value), "extract_array_andor8");

	parse_type_string(&pos, "(and cons null)");
	extract_array_andor_(local, pos, &pos, &check);
	test(check == 0, "extract_array_andor9");

	parse_type_string(&pos, "(or cons (not null))");
	extract_array_andor_(local, pos, &pos, &check);
	test(check, "extract_array_andor10");
	test(testlispdecl(pos, LISPDECL_OR), "extract_array_andor11");
	test(! RefNotDecl(pos), "extract_array_andor12");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "extract_array_andor13");
	GetArrayA4(pos, 0, &value);
	test(testlispdecl(value, LISPDECL_CONS), "extract_array_andor14");
	test(! RefNotDecl(value), "extract_array_andor15");
	GetArrayA4(pos, 1, &value);
	test(testlispdecl(value, LISPDECL_NULL), "extract_array_andor16");
	test(RefNotDecl(value), "extract_array_andor17");

	RETURN;
}

static int test_normlispdecl(void)
{
	addr pos;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "integer");
	test(normlispdecl(pos, LISPDECL_INTEGER), "normlispinteger1");
	parse_type_string(&pos, "cons");
	test(normlispdecl(pos, LISPDECL_CONS), "normlispinteger2");
	type_copy_local(local, &pos, pos);
	SetNotDecl(pos, 1);
	test(! normlispdecl(pos, LISPDECL_CONS), "normlispinteger3");

	RETURN;
}

static int test_check_typeand(void)
{
	addr array, pos, check;
	size_t size;
	LocalRoot local = Local_Thread;

	vector4_local(local, &array, 10);
	type1_local(local, LISPDECL_AND, array, &pos);
	size = 0;
	check = NULL;
	test(! check_typeand(pos, &check, &size), "check_typeand1");
	test(check == array, "check_typeand2");
	test(size == 10, "check_typeand3");
	SetNotDecl(pos, 1);
	test(check_typeand(pos, &check, &size), "check_typeand4");
	type1_local(local, LISPDECL_OR, array, &pos);
	test(check_typeand(pos, &check, &size), "check_typeand5");

	RETURN;
}

static int test_optimize_and1(void)
{
	int check;
	addr pos, array, value;
	LocalRoot local = Local_Thread;

	vector4_local(local, &array, 0);
	type1_local(local, LISPDECL_AND, array, &pos);
	optimize_and1_(local, pos, &value, &check);
	test(check, "optimize_and1-1");
	test(normlispdecl(value, LISPDECL_T), "optimize_and1-2");

	SetNotDecl(pos, 1);
	optimize_and1_(local, pos, &value, &check);
	test(! check, "optimize_and1-3");

	type1_local(local, LISPDECL_OR, array, &pos);
	optimize_and1_(local, pos, &value, &check);
	test(! check, "optimize_and1-4");

	vector4_local(local, &array, 1);
	type1_local(local, LISPDECL_AND, array, &pos);
	optimize_and1_(local, pos, &value, &check);
	test(! check, "optimize_and1-5");

	RETURN;
}

static int test_optimize_and2(void)
{
	int check;
	addr pos, array, value, aster;
	LocalRoot local = Local_Thread;

	vector4_local(local, &array, 1);
	type_asterisk_local(local, &aster);
	SetArrayA4(array, 0, aster);
	type1_local(local, LISPDECL_AND, array, &pos);
	value = 0;
	optimize_and2_(local, pos, &value, &check);
	test(check, "optimize_and2-1");
	test(value == aster, "optimize_and2-2");

	SetNotDecl(pos, 1);
	optimize_and2_(local, pos, &value, &check);
	test(! check, "optimize_and2-3");

	type1_local(local, LISPDECL_OR, array, &pos);
	optimize_and2_(local, pos, &value, &check);
	test(! check, "optimize_and2-4");

	vector4_local(local, &array, 0);
	type1_local(local, LISPDECL_AND, array, &pos);
	optimize_and2_(local, pos, &value, &check);
	test(! check, "optimize_and2-5");

	RETURN;
}

static int test_optimize_and3(void)
{
	int check;
	addr pos, array, value, temp;
	size_t i, size;
	LocalRoot local = Local_Thread;

	size = 5;
	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		type_asterisk_local(local, &temp);
		SetArrayA4(array, i, temp);
	}
	type0_local(local, LISPDECL_NIL, &temp);
	SetArrayA4(array, size / 2, temp);
	type1_local(local, LISPDECL_AND, array, &pos);
	optimize_and3_(local, pos, &value, &check);
	test(check, "optimize_and3-1");
	test(normlispdecl(value, LISPDECL_NIL), "optimize_and3-2");

	SetNotDecl(pos, 1);
	optimize_and3_(local, pos, &value, &check);
	test(! check, "optimize_and3-3");

	type1_local(local, LISPDECL_OR, array, &pos);
	optimize_and3_(local, pos, &value, &check);
	test(! check, "optimize_and3-4");

	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		type_asterisk_local(local, &temp);
		SetArrayA4(array, i, temp);
	}
	type1_local(local, LISPDECL_AND, array, &pos);
	optimize_and3_(local, pos, &value, &check);
	test(! check, "optimize_and3-5");

	RETURN;
}

static int test_optimize_and4(void)
{
	int check;
	addr pos, array, value1, value2, temp;
	size_t size;
	LocalRoot local = Local_Thread;

	size = 3;
	vector4_local(local, &array, size);
	type0_local(local, LISPDECL_CONS, &value1);
	SetArrayA4(array, 0, value1);
	type0_local(local, LISPDECL_T, &temp);
	SetArrayA4(array, 1, temp);
	type0_local(local, LISPDECL_NIL, &temp);
	SetArrayA4(array, 2, temp);
	type1_local(local, LISPDECL_AND, array, &pos);
	optimize_and4_(local, pos, &value1, &check);
	test(check, "optimize_and4-1");
	test(normlispdecl(value1, LISPDECL_AND), "optimize_and4-2");
	GetArrayType(value1, 0, &value1);
	test(lenarrayr(value1) == 2, "optimize_and4-3");
	GetArrayA4(value1, 0, &value2);
	test(normlispdecl(value2, LISPDECL_CONS), "optimize_and4-4");
	GetArrayA4(value1, 1, &value2);
	test(normlispdecl(value2, LISPDECL_NIL), "optimize_and4-5");

	SetNotDecl(pos, 1);
	optimize_and4_(local, pos, &value1, &check);
	test(! check, "optimize_and4-6");

	type1_local(local, LISPDECL_OR, array, &pos);
	optimize_and4_(local, pos, &value1, &check);
	test(! check, "optimize_and4-7");

	type0_local(local, LISPDECL_CONS, &value1);
	SetArrayA4(array, 0, value1);
	type0_local(local, LISPDECL_NULL, &value1);
	SetArrayA4(array, 1, value1);
	type0_local(local, LISPDECL_NIL, &temp);
	SetArrayA4(array, 2, temp);
	type1_local(local, LISPDECL_AND, array, &pos);
	optimize_and4_(local, pos, &value1, &check);
	test(! check, "optimize_and4-8");

	RETURN;
}

static int real_left_fixnum_p(addr pos, fixnum value)
{
	addr check;

	if (! normlispdecl(pos, LISPDECL_REAL))
		return 0;
	GetArrayType(pos, 0, &check);
	if (check != Nil)
		return 0;
	GetArrayType(pos, 1, &check);
	if (GetType(check) != LISPTYPE_FIXNUM)
		return 0;

	return RefFixnum(check) == value;
}

static int test_optimize_and5(void)
{
	int check;
	addr pos, value, array;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos,
			"(and (real 1) (real 2) (and (and (real 3)) (real 4)) (real 5))");
	optimize_and5_(local, pos, &value, &check);
	test(check, "optimize_and5-1");
	test(normlispdecl(value, LISPDECL_AND), "optimize_and5-2");
	GetArrayType(value, 0, &array);
	test(lenarrayr(array) == 5, "optimize_and5-3");
	GetArrayA4(array, 0, &value);
	test(real_left_fixnum_p(value, 1), "optimize_and5-4");
	GetArrayA4(array, 1, &value);
	test(real_left_fixnum_p(value, 2), "optimize_and5-5");
	GetArrayA4(array, 2, &value);
	test(real_left_fixnum_p(value, 3), "optimize_and5-6");
	GetArrayA4(array, 3, &value);
	test(real_left_fixnum_p(value, 4), "optimize_and5-7");
	GetArrayA4(array, 4, &value);
	test(real_left_fixnum_p(value, 5), "optimize_and5-8");

	SetNotDecl(pos, 1);
	type_copy_local(local, &pos, pos);
	optimize_and5_(local, pos, &value, &check);
	test(! check, "optimize_and5-9");

	parse_type_string(&pos,
			"(or (real 1) (real 2) (and (and (real 3)) (real 4)) (real 5))");
	optimize_and5_(local, pos, &value, &check);
	test(! check, "optimize_and5-10");

	parse_type_string(&pos,
			"(and (real 1) (real 2) (or (or (real 3)) (real 4)) (real 5))");
	optimize_and5_(local, pos, &value, &check);
	test(! check, "optimize_and5-11");

	RETURN;
}

static int test_check_typeor(void)
{
	addr array, pos, check;
	size_t size;
	LocalRoot local = Local_Thread;

	vector4_local(local, &array, 10);
	type1_local(local, LISPDECL_OR, array, &pos);
	size = 0;
	check = NULL;
	test(! check_typeor(pos, &check, &size), "check_typeor1");
	test(check == array, "check_typeor2");
	test(size == 10, "check_typeor3");
	SetNotDecl(pos, 1);
	test(check_typeor(pos, &check, &size), "check_typeor4");
	type1_local(local, LISPDECL_AND, array, &pos);
	test(check_typeor(pos, &check, &size), "check_typeor5");

	RETURN;
}

static int test_optimize_or1(void)
{
	int check;
	addr pos, array, value;
	LocalRoot local = Local_Thread;

	vector4_local(local, &array, 0);
	type1_local(local, LISPDECL_OR, array, &pos);
	optimize_or1_(local, pos, &value, &check);
	test(check, "optimize_or1-1");
	test(normlispdecl(value, LISPDECL_NIL), "optimize_or1-2");

	SetNotDecl(pos, 1);
	optimize_or1_(local, pos, &value, &check);
	test(! check, "optimize_or1-3");

	type1_local(local, LISPDECL_AND, array, &pos);
	optimize_or1_(local, pos, &value, &check);
	test(! check, "optimize_or1-4");

	vector4_local(local, &array, 1);
	type1_local(local, LISPDECL_OR, array, &pos);
	optimize_or1_(local, pos, &value, &check);
	test(! check, "optimize_or1-5");

	RETURN;
}

static int test_optimize_or2(void)
{
	int check;
	addr pos, array, value, aster;
	LocalRoot local = Local_Thread;

	vector4_local(local, &array, 1);
	type_asterisk_local(local, &aster);
	SetArrayA4(array, 0, aster);
	type1_local(local, LISPDECL_OR, array, &pos);
	value = 0;
	optimize_or2_(local, pos, &value, &check);
	test(check, "optimize_or2-1");
	test(value == aster, "optimize_or2-2");

	SetNotDecl(pos, 1);
	optimize_or2_(local, pos, &value, &check);
	test(! check, "optimize_or2-3");

	type1_local(local, LISPDECL_AND, array, &pos);
	optimize_or2_(local, pos, &value, &check);
	test(! check, "optimize_or2-4");

	vector4_local(local, &array, 0);
	type1_local(local, LISPDECL_OR, array, &pos);
	optimize_or2_(local, pos, &value, &check);
	test(! check, "optimize_or2-5");

	RETURN;
}

static int test_optimize_or3(void)
{
	int check;
	addr pos, array, value, temp;
	size_t i, size;
	LocalRoot local = Local_Thread;

	size = 5;
	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		type_asterisk_local(local, &temp);
		SetArrayA4(array, i, temp);
	}
	type0_local(local, LISPDECL_T, &temp);
	SetArrayA4(array, size / 2, temp);
	type1_local(local, LISPDECL_OR, array, &pos);
	optimize_or3_(local, pos, &value, &check);
	test(check, "optimize_or3-1");
	test(normlispdecl(value, LISPDECL_T), "optimize_or3-2");

	SetNotDecl(pos, 1);
	optimize_or3_(local, pos, &value, &check);
	test(! check, "optimize_or3-3");

	type1_local(local, LISPDECL_AND, array, &pos);
	optimize_or3_(local, pos, &value, &check);
	test(! check, "optimize_or3-4");

	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		type_asterisk_local(local, &temp);
		SetArrayA4(array, i, temp);
	}
	type1_local(local, LISPDECL_OR, array, &pos);
	optimize_or3_(local, pos, &value, &check);
	test(! check, "optimize_or3-5");

	RETURN;
}

static int test_optimize_or4(void)
{
	int check;
	addr pos, array, value1, value2, temp;
	size_t size;
	LocalRoot local = Local_Thread;

	size = 3;
	vector4_local(local, &array, size);
	type0_local(local, LISPDECL_CONS, &value1);
	SetArrayA4(array, 0, value1);
	type0_local(local, LISPDECL_NIL, &temp);
	SetArrayA4(array, 1, temp);
	type0_local(local, LISPDECL_T, &temp);
	SetArrayA4(array, 2, temp);
	type1_local(local, LISPDECL_OR, array, &pos);
	optimize_or4_(local, pos, &value1, &check);
	test(check, "optimize_or4-1");
	test(normlispdecl(value1, LISPDECL_OR), "optimize_or4-2");
	GetArrayType(value1, 0, &value1);
	test(lenarrayr(value1) == 2, "optimize_or4-3");
	GetArrayA4(value1, 0, &value2);
	test(normlispdecl(value2, LISPDECL_CONS), "optimize_or4-4");
	GetArrayA4(value1, 1, &value2);
	test(normlispdecl(value2, LISPDECL_T), "optimize_or4-5");

	SetNotDecl(pos, 1);
	optimize_or4_(local, pos, &value1, &check);
	test(! check, "optimize_or4-6");

	type1_local(local, LISPDECL_AND, array, &pos);
	optimize_or4_(local, pos, &value1, &check);
	test(! check, "optimize_or4-7");

	type0_local(local, LISPDECL_CONS, &value1);
	SetArrayA4(array, 0, value1);
	type0_local(local, LISPDECL_NULL, &value1);
	SetArrayA4(array, 1, value1);
	type0_local(local, LISPDECL_T, &temp);
	SetArrayA4(array, 2, temp);
	type1_local(local, LISPDECL_OR, array, &pos);
	optimize_or4_(local, pos, &value1, &check);
	test(! check, "optimize_or4-8");

	RETURN;
}

static int test_optimize_or5(void)
{
	int check;
	addr pos, value, array;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos,
			"(or (real 1) (real 2) (or (or (real 3)) (real 4)) (real 5))");
	optimize_or5_(local, pos, &value, &check);
	test(check, "optimize_or5-1");
	test(normlispdecl(value, LISPDECL_OR), "optimize_or5-2");
	GetArrayType(value, 0, &array);
	test(lenarrayr(array) == 5, "optimize_or5-3");
	GetArrayA4(array, 0, &value);
	test(real_left_fixnum_p(value, 1), "optimize_or5-4");
	GetArrayA4(array, 1, &value);
	test(real_left_fixnum_p(value, 2), "optimize_or5-5");
	GetArrayA4(array, 2, &value);
	test(real_left_fixnum_p(value, 3), "optimize_or5-6");
	GetArrayA4(array, 3, &value);
	test(real_left_fixnum_p(value, 4), "optimize_or5-7");
	GetArrayA4(array, 4, &value);
	test(real_left_fixnum_p(value, 5), "optimize_or5-8");

	SetNotDecl(pos, 1);
	optimize_or5_(local, pos, &value, &check);
	test(! check, "optimize_or5-9");

	parse_type_string(&pos,
			"(and (real 1) (real 2) (or (or (real 3)) (real 4)) (real 5))");
	optimize_or5_(local, pos, &value, &check);
	test(! check, "optimize_or5-10");

	parse_type_string(&pos,
			"(or (real 1) (real 2) (and (and (real 3)) (real 4)) (real 5))");
	optimize_or5_(local, pos, &value, &check);
	test(! check, "optimize_or5-11");

	RETURN;
}

static int range_valid_p_degrade(addr pos)
{
	int check;
	check = 0;
	range_valid_p_(pos, &check);
	return check;
}

static int test_range_valid_p(void)
{
	addr pos;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "(real * *)");
	test(range_valid_p_degrade(pos), "range_valid_p1");

	parse_type_string(&pos, "(real 10 *)");
	test(range_valid_p_degrade(pos), "range_valid_p2");

	parse_type_string(&pos, "(real * 10)");
	type_copy_local(local, &pos, pos);
	SetNotDecl(pos, 1);
	test(range_valid_p_degrade(pos), "range_valid_p3");

	parse_type_string(&pos, "(real (10) 20)");
	type_copy_local(local, &pos, pos);
	SetNotDecl(pos, 1);
	test(range_valid_p_degrade(pos), "range_valid_p4");

	parse_type_string(&pos, "(real (20) 10)");
	test(! range_valid_p_degrade(pos), "range_valid_p5");

	parse_type_string(&pos, "(real 10 10)");
	test(range_valid_p_degrade(pos), "range_valid_p6");

	parse_type_string(&pos, "(real (10) 10)");
	test(! range_valid_p_degrade(pos), "range_valid_p7");

	parse_type_string(&pos, "(real 10 (10))");
	test(! range_valid_p_degrade(pos), "range_valid_p8");

	parse_type_string(&pos, "(real (10) (10))");
	test(! range_valid_p_degrade(pos), "range_valid_p9");

	RETURN;
}

static int test_optimize_range(void)
{
	int check;
	addr pos;
	LocalRoot local = Local_Thread;

	check = 0;

	parse_type_string(&pos, "(integer 20 10)");
	optimize_range_(local, pos, &pos, &check);
	test(check, "optimize_range1");
	test(testlispdecl(pos, LISPDECL_NIL), "optimize_range2");

	parse_type_string(&pos, "(integer 10 20)");
	optimize_range_(local, pos, &pos, &check);
	test(! check, "optimize_range3");

	parse_type_string(&pos, "cons");
	optimize_range_(local, pos, &pos, &check);
	test(! check, "optimize_range4");

	RETURN;
}


/*
 *  wake optimize
 */
static int test_extract_values_var(void)
{
	addr pos, pos1, pos2, pos3;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	extract_values_var_(local, Nil, &pos);

	parse_type_string(&pos1, "(and symbol cons)");
	parse_type_string(&pos2, "cons");
	parse_type_string(&pos3, "array");
	list_heap(&pos, pos1, pos2, pos3, NULL);
	extract_values_var_(local, pos, &pos);

	parse_type_string(&pos1, "list");
	parse_type_string(&pos2, "array");
	list_heap(&pos, pos1, pos2, NULL);
	extract_values_var_(local, pos, &pos);
	GetCons(pos, &pos1, &pos);
	test(testlispdecl(pos1, LISPDECL_OR), "extract_values_var4");
	GetCons(pos, &pos1, &pos);
	test(testlispdecl(pos1, LISPDECL_ARRAY), "extract_values_var5");
	test(pos == Nil, "extract_values_var6");

	rollback_local(local, stack);

	RETURN;
}

static void parse_type_values_string(addr *ret, const char *code)
{
	readstring_debug(ret, code);
	if (parse_type_values(Execute_Thread, ret, *ret, Nil)) {
		Error(fmte_("parse_type_values error.", NULL));
	}
}

static int test_optimize_values(void)
{
	int check;
	addr pos, value;
	LocalRoot local = Local_Thread;

	parse_type_values_string(&pos, "(values)");
	type_optimize_(local, pos, &pos, &check);
	test(check == 0, "optimize_values1");
	parse_type_values_string(&pos, "(values *)");
	type_optimize_(local, pos, &pos, &check);
	test(check == 0, "optimize_values2");
	parse_type_values_string(&pos, "(values integer)");
	type_optimize_(local, pos, &pos, &check);
	test(check == 0, "optimize_values3");
	parse_type_values_string(&pos, "(values list)");
	type_optimize_(local, pos, &pos, &check);
	test(check, "optimize_values4");
	GetArrayType(pos, 0, &pos);
	GetCons(pos, &value, &pos);
	test(testlispdecl(value, LISPDECL_OR), "optimize_values5");
	test(pos == Nil, "optimize_values6");

	parse_type_values_string(&pos, "(values &optional cons)");
	type_optimize_(local, pos, &pos, &check);
	test(check == 0, "optimize_values7");
	parse_type_values_string(&pos, "(values &optional list)");
	type_optimize_(local, pos, &pos, &check);
	test(check, "optimize_values8");

	parse_type_values_string(&pos, "(values &rest cons)");
	type_optimize_(local, pos, &pos, &check);
	test(check == 0, "optimize_values9");
	parse_type_values_string(&pos, "(values &rest list)");
	type_optimize_(local, pos, &pos, &check);
	test(check, "optimize_values10");

	RETURN;
}

static int test_extract_function_key(void)
{
	addr pos, pos1, pos2, key1, key2;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	extract_function_key_(local, Nil, &pos);

	readstring_debug(&key1, "aaa");
	parse_type_string(&key2, "cons");
	cons_heap(&pos1, key1, key2);
	readstring_debug(&key1, "bbb");
	parse_type_string(&key2, "array");
	cons_heap(&pos2, key1, key2);
	list_heap(&pos, pos1, pos2, NULL);
	extract_function_key_(local, pos, &pos);

	readstring_debug(&key1, "aaa");
	parse_type_string(&key2, "list");
	cons_heap(&pos1, key1, key2);
	readstring_debug(&key1, "bbb");
	parse_type_string(&key2, "array");
	cons_heap(&pos2, key1, key2);
	list_heap(&pos, pos1, pos2, NULL);
	extract_function_key_(local, pos, &pos);

	GetCons(pos, &pos1, &pos);
	GetCons(pos1, &key1, &key2);
	test(GetType(key1) == LISPTYPE_SYMBOL, "extract_function_key4");
	test(testlispdecl(key2, LISPDECL_OR), "extract_function_key5");
	GetCons(pos, &pos1, &pos);
	GetCons(pos1, &key1, &key2);
	test(GetType(key1) == LISPTYPE_SYMBOL, "extract_function_key6");
	test(testlispdecl(key2, LISPDECL_ARRAY), "extract_function_key7");
	test(pos == Nil, "extract_function_key8");

	rollback_local(local, stack);

	RETURN;
}

static int test_extract_function(void)
{
	int check;
	addr pos;
	LocalRoot local = Local_Thread;

	parse_type_string(&pos, "(function (integer))");
	type_optimize_(local, pos, &pos, &check);
	test(check == 0, "optimize_function1");
	parse_type_string(&pos, "(function (list))");
	type_optimize_(local, pos, &pos, &check);
	test(check, "optimize_function2");
	parse_type_string(&pos, "(function (&optional list))");
	type_optimize_(local, pos, &pos, &check);
	test(check, "optimize_function3");
	parse_type_string(&pos, "(function (&key (hello list)))");
	type_optimize_(local, pos, &pos, &check);
	test(check, "optimize_function4");

	parse_type_string(&pos, "function");
	type_optimize_(local, pos, &pos, &check);
	test(check == 0, "optimize_function5");
	parse_type_string(&pos, "(function *)");
	type_optimize_(local, pos, &pos, &check);
	test(check == 0, "optimize_function6");
	parse_type_string(&pos, "(function * *)");
	type_optimize_(local, pos, &pos, &check);
	test(check == 0, "optimize_function7");
	parse_type_string(&pos, "(function * integer)");
	type_optimize_(local, pos, &pos, &check);
	test(check == 0, "optimize_function8");
	parse_type_string(&pos, "(function * list)");
	type_optimize_(local, pos, &pos, &check);
	test(check, "optimize_function9");

	RETURN;
}


/*
 *  main
 */
static int testcase_type_optimize(void)
{
	/* type-extract */
	TestBreak(test_optimize_optimized);
	TestBreak(test_optimize_not_asterisk);
	TestBreak(test_optimize_not_nil);
	TestBreak(test_optimize_not_t);
	TestBreak(test_optimize_mod);
	TestBreak(test_optimize_atom);
	TestBreak(test_optimize_list);
	TestBreak(test_optimize_boolean);
	TestBreak(test_optimize_vector);
	TestBreak(test_optimize_simple_vector);
	TestBreak(test_optimize_bit_vector);
	TestBreak(test_optimize_simple_bit_vector);
	TestBreak(test_optimize_extended_char);
	TestBreak(test_optimize_string);
	TestBreak(test_optimize_base_string);
	TestBreak(test_optimize_simple_string);
	TestBreak(test_optimize_simple_base_string);
	TestBreak(test_optimize_signed_byte);
	TestBreak(test_optimize_unsigned_byte);
	TestBreak(test_optimize_bit);
	TestBreak(test_optimize_fixnum);
	TestBreak(test_optimize_bignum);
	TestBreak(test_optimize_eql);
	TestBreak(test_optimize_eql_range);
	TestBreak(test_optimize_member1);
	TestBreak(test_optimize_member2);
	TestBreak(test_optimize_member3);
	TestBreak(test_optimize_not);
	TestBreak(test_optimize_result);
	TestBreak(test_extract_not_andor);
	TestBreak(test_extract_array_andor);
	TestBreak(test_normlispdecl);
	TestBreak(test_check_typeand);
	TestBreak(test_optimize_and1);
	TestBreak(test_optimize_and2);
	TestBreak(test_optimize_and3);
	TestBreak(test_optimize_and4);
	TestBreak(test_optimize_and5);
	TestBreak(test_check_typeor);
	TestBreak(test_optimize_or1);
	TestBreak(test_optimize_or2);
	TestBreak(test_optimize_or3);
	TestBreak(test_optimize_or4);
	TestBreak(test_optimize_or5);
	TestBreak(test_range_valid_p);
	TestBreak(test_optimize_range);
	/* wake optimize */
	TestBreak(test_extract_values_var);
	TestBreak(test_optimize_values);
	TestBreak(test_extract_function_key);
	TestBreak(test_extract_function);

	return 0;
}

static void testinit_type_optimize(Execute ptr)
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

int test_type_optimize(void)
{
	DegradeTitle;
	return DegradeCode(type_optimize);
}

