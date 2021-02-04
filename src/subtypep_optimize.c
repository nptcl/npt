#include "bignum_data.h"
#include "bignum_object.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "integer.h"
#include "number.h"
#include "object.h"
#include "real_equal.h"
#include "sequence.h"
#include "subtypep_optimize.h"
#include "type.h"
#include "type_copy.h"
#include "type_parse.h"
#include "type_upgraded.h"

/*
 *  macro
 */
static int type_optimize_(LocalRoot local, addr type, addr *value, int *ret);
static int check_optimize_(addr type, int *ret);
typedef int (*extractcalltype)(addr *, addr);

#define CheckNotDecl(x, type) (RefLispDecl(x) == type && RefNotDecl(x))

#define Return_or_optimize(call, type, ret) { \
	Return(call(type, ret)); \
	if (*ret) { \
		return 0; \
	} \
};

#define Return_check_optimize(call, type, ret) { \
	Return(call(type, ret)); \
	if (*ret == 0) { \
		return 0; \
	} \
};

#define extractcall(local, call, pos, update) { \
	int __check = 0; \
	addr __value = 0; \
	Check(GetType(pos) != LISPTYPE_TYPE, "type error"); \
	for (;;) { \
		Return(call(local, pos, &__value, &__check)); \
		if (! __check) { \
			break; \
		} \
		update = 1; \
		pos = __value; \
	} \
}

#define extractcallnot(local, call, pos, update) { \
	int __check = 0; \
	addr __value = 0; \
	Check(GetType(pos) != LISPTYPE_TYPE, "type error"); \
	for (;;) { \
		Return(call(local, pos, &__value, &__check)); \
		if (! __check) { \
			break; \
		}\
		update = 1; \
		if (RefNotDecl(pos)) { \
			type_revnotdecl(__value); \
		} \
		pos = __value; \
	} \
}


/*
 *  optimize
 */
static int check_optimized_(addr right, int *ret)
{
	enum LISPDECL type;

	GetLispDecl(right, &type);
	*ret = (type == LISPDECL_OPTIMIZED || type == LISPDECL_SUBTYPEP);

	return 0;
}

static int optimize_optimized_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_optimized_, right, ret);

	if (! RefNotDecl(right)) {
		GetArrayType(right, 0, value);
	}
	else {
		GetArrayType(right, 0, &right);
		type_copy_unsafe_local(local, &right, right);
		type_revnotdecl(right);
		*value = right;
	}

	return Result(ret, 1);
}

static int check_not_asterisk_(addr right, int *ret)
{
	*ret = CheckNotDecl(right, LISPDECL_ASTERISK);
	return 0;
}
static int optimize_not_asterisk_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_not_asterisk_, right, ret);

	/* error */
	return fmte_("Don't allow to use (not *).", NULL);
}

static int check_not_nil_(addr right, int *ret)
{
	*ret = CheckNotDecl(right, LISPDECL_NIL);
	return 0;
}
static int optimize_not_nil_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_not_nil_, right, ret);
	type0_local(local, LISPDECL_T, value);
	return Result(ret, 1);
}

static int check_not_t_(addr right, int *ret)
{
	*ret = CheckNotDecl(right, LISPDECL_T);
	return 0;
}
static int optimize_not_t_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_not_t_, right, ret);
	type0_local(local, LISPDECL_NIL, value);
	return Result(ret, 1);
}

/* (mod size) -> (integer 0 (size)) */
static int check_mod_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_MOD);
	return 0;
}
static int optimize_mod_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left, pos;

	Return_check_optimize(check_mod_, right, ret);
	GetArrayType(right, 0, &left);
	Check(! integerp(left), "type error");
	Check(! plusp_integer_debug(left), "plusp error");
	fixnum_local(local, &pos, 0);
	type4_local(local, LISPDECL_INTEGER, Nil, pos, T, left, value);

	return Result(ret, 1);
}

/* atom -> (not cons) */
static int check_atom_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_ATOM);
	return 0;
}
static int optimize_atom_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left;

	Return_check_optimize(check_atom_, right, ret);
	type2aster_localall(local, LISPDECL_CONS, &left);
	SetNotDecl(left, 1);
	*value = left;

	return Result(ret, 1);
}

/* list -> (or null cons) */
static int check_list_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_LIST);
	return 0;
}
static int optimize_list_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr pos, array;

	Return_check_optimize(check_list_, right, ret);
	vector4_local(local, &array, 2);
	/* null */
	type0_local(local, LISPDECL_NULL, &pos);
	SetArrayA4(array, 0, pos);
	/* cons */
	type2aster_localall(local, LISPDECL_CONS, &pos);
	SetArrayA4(array, 1, pos);
	/* result */
	type1_local(local, LISPDECL_OR, array, value);

	return Result(ret, 1);
}

/* boolean -> (or null (eql t)) */
static int check_boolean_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_BOOLEAN);
	return 0;
}
static int optimize_boolean_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr pos, array;

	Return_check_optimize(check_boolean_, right, ret);
	vector4_local(local, &array, 2);
	/* null */
	type0_local(local, LISPDECL_NULL, &pos);
	SetArrayA4(array, 0, pos);
	/* (eql t) */
	type_eql_local(local, T, &pos);
	SetArrayA4(array, 1, pos);
	/* result */
	type1_local(local, LISPDECL_OR, array, value);

	return Result(ret, 1);
}

/*
 *  (... type *) -> (array type 1)
 *  (... type size) -> (array type (size))
 */
static int extract_vector(LocalRoot local,
		addr *ret, enum LISPDECL decl, addr type, addr size)
{
	addr array;

	if (type_asterisk_p(size)) {
		fixnum_local(local, &size, 1);
		type2_local(local, decl, type, size, ret);
		return 1;
	}
	if (GetType(size) == LISPTYPE_FIXNUM) {
		vector4_local(local, &array, 1);
		SetArrayA4(array, 0, size);
		type2_local(local, decl, type, array, ret);
		return 1;
	}

	return 0;
}

/* sequence -> (or null cons (array * 1)) */
static int check_sequence_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_SEQUENCE);
	return 0;
}

static int optimize_sequence_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr pos, array, type;

	Return_check_optimize(check_sequence_, right, ret);
	vector4_local(local, &array, 3);
	/* null */
	type0_local(local, LISPDECL_NULL, &pos);
	SetArrayA4(array, 0, pos);
	/* cons */
	type2aster_localall(local, LISPDECL_CONS, &pos);
	SetArrayA4(array, 1, pos);
	/* (array * 1) */
	type0_local(local, LISPDECL_ASTERISK, &type);
	fixnum_local(local, &pos, 1);
	type2_local(local, LISPDECL_ARRAY, type, pos, &pos);
	SetArrayA4(array, 2, pos);
	/* result */
	type1_local(local, LISPDECL_OR, array, value);

	return Result(ret, 1);
}

/* (vector type size) -> (array type (size)) */
static int check_vector_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_VECTOR)
		return Result(ret, 0);
	GetArrayType(right, 1, &right);
	*ret = (type_asterisk_p(right) || GetType(right) == LISPTYPE_FIXNUM);

	return 0;
}
static int optimize_vector_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_vector_, right, ret);
	GetArrayType(right, 0, &type);
	GetArrayType(right, 1, &right);
	extract_vector(local, value, LISPDECL_ARRAY, type, right);

	return Result(ret, 1);
}

/* (simple-vector size) -> (simple-array t (size)) */
static int check_vector_type(enum LISPDECL decl, addr right)
{
	if (RefLispDecl(right) != decl)
		return 0;
	GetArrayType(right, 0, &right);
	return type_asterisk_p(right) || GetType(right) == LISPTYPE_FIXNUM;
}
static int check_simple_vector_(addr right, int *ret)
{
	*ret = check_vector_type(LISPDECL_SIMPLE_VECTOR, right);
	return 0;
}
static int optimize_simple_vector_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_simple_vector_, right, ret);
	upgraded_array_t_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_SIMPLE_ARRAY, type, right);

	return Result(ret, 1);
}

/* (bit-vector size) -> (array bit (size)) */
static int check_bit_vector_(addr right, int *ret)
{
	*ret = check_vector_type(LISPDECL_BIT_VECTOR, right);
	return 0;
}
static int optimize_bit_vector_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_bit_vector_, right, ret);
	upgraded_array_bit_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_ARRAY, type, right);

	return Result(ret, 1);
}

/* (simple-bit-vector size) -> (simple-array bit (size)) */
static int check_simple_bit_vector_(addr right, int *ret)
{
	*ret = check_vector_type(LISPDECL_SIMPLE_BIT_VECTOR, right);
	return 0;
}
static int optimize_simple_bit_vector_(
		LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_simple_bit_vector_, right, ret);
	upgraded_array_bit_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_SIMPLE_ARRAY, type, right);

	return Result(ret, 1);
}

/* extended-char -> (and character (not base-char)) */
static int check_extended_char_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_EXTENDED_CHAR);
	return 0;
}
static int optimize_extended_char_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr array;

	Return_check_optimize(check_extended_char_, right, ret);
	vector4_local(local, &array, 2);
	/* character */
	type0_local(local, LISPDECL_CHARACTER, &right);
	SetArrayA4(array, 0, right);
	/* (not base-char) */
	type0_local(local, LISPDECL_BASE_CHAR, &right);
	SetNotDecl(right, 1);
	SetArrayA4(array, 1, right);
	/* result */
	type1_local(local, LISPDECL_AND, array, value);

	return Result(ret, 1);
}

/* (string size) -> (vector character size) */
static void extract_string(LocalRoot local, addr *value, addr right, addr type)
{
	GetArrayType(right, 0, &right);
	type2_local(local, LISPDECL_VECTOR, type, right, value);
}

static int check_string_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_STRING);
	return 0;
}
static int optimize_string_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_string_, right, ret);
	upgraded_array_character_local(local, &type);
	extract_string(local, value, right, type);

	return Result(ret, 1);
}

/* (base-string size) -> (vector base-char size) */
static int check_base_string_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_BASE_STRING);
	return 0;
}
static int optimize_base_string_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_base_string_, right, ret);
	upgraded_array_character_local(local, &type);
	extract_string(local, value, right, type);

	return Result(ret, 1);
}

/* (simple-string size) -> (simple-array character (size)) */
static int check_simple_string_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_SIMPLE_STRING);
	return 0;
}
static int optimize_simple_string_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_simple_string_, right, ret);
	upgraded_array_character_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_SIMPLE_ARRAY, type, right);

	return Result(ret, 1);
}

/* (simple-base-string size) -> (simple-array base-char (size)) */
static int check_simple_base_string_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_SIMPLE_BASE_STRING);
	return 0;
}
static int optimize_simple_base_string_(
		LocalRoot local, addr right, addr *value, int *ret)
{
	addr type;

	Return_check_optimize(check_simple_base_string_, right, ret);
	upgraded_array_character_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, value, LISPDECL_SIMPLE_ARRAY, type, right);

	return Result(ret, 1);
}

/* (signed-byte *) -> integer */
/* (signed-byte size) -> (integer -2^size-1 2^(size-1)-1) */
static int check_signed_byte_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_SIGNED_BYTE);
	return 0;
}
static int optimize_signed_byte_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left;
	fixnum num;
	fixed fixedvalue;
	size_t size;

	Return_check_optimize(check_signed_byte_, right, ret);

	GetArrayType(right, 0, &right);
	if (type_asterisk_p(right)) {
		/* (signed-byte *) */
		type4aster_localall(local, LISPDECL_INTEGER, value);
		return Result(ret, 1);
	}

	/*
	 *  size  : -(2^{size-1}) --- +(2^{size-1} - 1)
	 *  size=8: -(2^7)        --- +(2^7 - 1)
	 *       -> -128 --- 127
	 */
	if (GetType(right) == LISPTYPE_FIXNUM) {
		GetFixnum(right, &num);
		if (num == BIGNUM_FULLBIT) {
			GetConst(FIXNUM_MIN, &left);
			GetConst(FIXNUM_MAX, &right);
			type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, value);
			return Result(ret, 1);
		}
		if (num < BIGNUM_FULLBIT) {
			num = 1LL << (num - 1LL);
			fixnum_local(local, &left, -num);
			fixnum_local(local, &right, num - 1LL);
			type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, value);
			return Result(ret, 1);
		}
		size = (size_t)num;
	}
	else {
		Check(GetType(right) != LISPTYPE_BIGNUM, "type error");
		if (1 < RefSizeBignum(right)) {
			/* Abort("Too large signed-byte value."); */
			return Result(ret, 0);
		}
		getfixed_bignum(right, 0, &fixedvalue);
		size = (size_t)fixedvalue;
	}

	/* bignum */
	power2_bigdata_alloc(local, &left, size - 1UL);
	size = RefAllocBignum(left);
	alloc_bignum(local, &right, size);
	setminusvalue_bigdata(right, left, SignPlus, 1);
	SetSignBignum(left, SignMinus);
	type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, value);

	return Result(ret, 1);
}

/* (unsigned-byte *) -> (integer 0 *) */
/* (unsigned-byte size) -> (integer 0 2^size-1) */
static int check_unsigned_byte_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_UNSIGNED_BYTE);
	return 0;
}
static int optimize_unsigned_byte_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left;
	fixnum num;
	fixed fixedvalue;
	size_t size;

	Return_check_optimize(check_unsigned_byte_, right, ret);

	GetArrayType(right, 0, &right);
	if (type_asterisk_p(right)) {
		/* (unsigned-byte *) */
		fixnum_local(local, &left, 0);
		type4_local(local, LISPDECL_INTEGER, Nil, left, right, right, value);
		return Result(ret, 1);
	}

	/*
	 *  size  : 0 --- +(2^{size} - 1)
	 *  size=8: 0 --- +(2^8 - 1)
	 *       -> 0 --- 255
	 */
	if (GetType(right) == LISPTYPE_FIXNUM) {
		GetFixnum(right, &num);
		if (num == BIGNUM_FULLBIT - 1L) {
			fixnum_local(local, &left, 0);
			GetConst(FIXNUM_MAX, &right);
			type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, value);
			return Result(ret, 1);
		}
		if (num < BIGNUM_FULLBIT - 1L) {
			num = 1LL << num;
			fixnum_local(local, &left, 0);
			fixnum_local(local, &right, num - 1L);
			type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, value);
			return Result(ret, 1);
		}
		size = (size_t)num;
	}
	else {
		Check(GetType(right) != LISPTYPE_BIGNUM, "type error");
		if (1 < RefSizeBignum(right)) {
			/* Abort("Too large unsigned-byte value."); */
			return Result(ret, 0);
		}
		getfixed_bignum(right, 0, &fixedvalue);
		size = (size_t)fixedvalue;
	}

	/* bignum */
	power2_bigdata_alloc(local, &right, size);
	setminusvalue_bigdata(right, right, SignPlus, 1);
	fixnum_local(local, &left, 0);
	type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, value);

	return Result(ret, 1);
}

/* bit -> (integer 0 1) */
static int check_bit_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_BIT);
	return 0;
}
static int optimize_bit_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left;

	Return_check_optimize(check_bit_, right, ret);
	fixnum_local(local, &left, 0);
	fixnum_local(local, &right, 1);
	type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, value);

	return Result(ret, 1);
}

/* fixnum -> (integer most-negative-fixnum most-positive-fixnum) */
static int check_fixnum_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_FIXNUM);
	return 0;
}
static int optimize_fixnum_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr left;

	Return_check_optimize(check_fixnum_, right, ret);
	GetConst(FIXNUM_MIN, &left);
	GetConst(FIXNUM_MAX, &right);
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type left error");
	type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, value);

	return Result(ret, 1);
}

/* bignum -> (and integer (not fixnum)) */
static int check_bignum_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_BIGNUM);
	return 0;
}
static int optimize_bignum_(LocalRoot local, addr right, addr *value, int *ret)
{
	int ignore;
	addr array, pos;

	Return_check_optimize(check_bignum_, right, ret);
	vector4_local(local, &array, 2);
	/* integer */
	type4aster_localall(local, LISPDECL_INTEGER, &pos);
	SetArrayA4(array, 0, pos);
	/* (not fixnum) */
	type0_local(local, LISPDECL_FIXNUM, &pos);
	SetNotDecl(pos, 1);
	Return(type_optimize_(local, pos, &pos, &ignore));
	SetArrayA4(array, 1, pos);
	/* bignum */
	type1_local(local, LISPDECL_AND, array, value);

	return Result(ret, 1);
}

/* (eql nil) -> null */
static int check_eql_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_EQL)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	return Result(ret, right == Nil);
}
static int optimize_eql_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_eql_, right, ret);
	type0_local(local, LISPDECL_NULL, value);
	return Result(ret, 1);
}

/* (eql 10) -> (integer 10 10) */
static void optimize_eql_range_object(
		LocalRoot local, enum LISPDECL decl, addr pos, addr *value)
{
	type4_local(local, decl, Nil, pos, Nil, pos, value);
}

static int optimize_eql_range_type_(LocalRoot local, addr pos, addr *value, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			optimize_eql_range_object(local, LISPDECL_INTEGER, pos, value);
			break;

		case LISPTYPE_RATIO:
			optimize_eql_range_object(local, LISPDECL_RATIONAL, pos, value);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			optimize_eql_range_object(local, LISPDECL_SINGLE_FLOAT, pos, value);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			optimize_eql_range_object(local, LISPDECL_DOUBLE_FLOAT, pos, value);
			break;

		case LISPTYPE_LONG_FLOAT:
			optimize_eql_range_object(local, LISPDECL_LONG_FLOAT, pos, value);
			break;

		default:
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int check_eql_range_(addr right, int *ret)
{
	enum LISPTYPE type;

	if (RefLispDecl(right) != LISPDECL_EQL)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	type = GetType(right);
	*ret = type == LISPTYPE_FIXNUM
		|| type == LISPTYPE_BIGNUM
		|| type == LISPTYPE_RATIO
		|| type == LISPTYPE_SINGLE_FLOAT
		|| type == LISPTYPE_DOUBLE_FLOAT
		|| type == LISPTYPE_LONG_FLOAT;
	return 0;
}
static int optimize_eql_range_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_eql_range_, right, ret);
	GetArrayType(right, 0, &right);
	return optimize_eql_range_type_(local, right, value, ret);
}

/* (member) -> nil */
static int check_member1_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_MEMBER)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	return Result(ret, LenArrayA4r(right) == 0);
}
static int optimize_member1_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_member1_, right, ret);
	type0_local(local, LISPDECL_NIL, value);
	return Result(ret, 1);
}

/* (member arg) -> (eql arg) */
static int check_member2_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_MEMBER)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	return Result(ret, LenArrayA4r(right) == 1);
}
static int optimize_member2_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_member2_, right, ret);
	GetArrayType(right, 0, &right);
	GetArrayA4(right, 0, &right);
	type_eql_local(local, right, value);
	return Result(ret, 1);
}

/* (member ...) -> (or (eql arg1) (eql arg2) ...) */
static int check_member3_(addr right, int *ret)
{
	if (RefLispDecl(right) != LISPDECL_MEMBER)
		return Result(ret, 0);
	GetArrayType(right, 0, &right);
	return Result(ret, 2 <= LenArrayA4r(right));
}
static int optimize_member3_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr array, child;
	size_t i, size;

	Return_check_optimize(check_member3_, right, ret);
	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &child);
		type_eql_local(local, child, &child);
		SetArrayA4(array, i, child);
	}
	type1_local(local, LISPDECL_OR, array, value);

	return Result(ret, 1);
}

/* (not x) -> x.not */
static int check_not_(addr right, int *ret)
{
	*ret = (RefLispDecl(right) == LISPDECL_NOT);
	return 0;
}
static int optimize_not_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_not_, right, ret);
	if (RefNotDecl(right)) {
		/* not not */
		GetArrayType(right, 0, value);
	}
	else {
		/* not */
		GetArrayType(right, 0, &right);
		type_copy_unsafe_local(local, &right, right);
		type_revnotdecl(right);
		*value = right;
	}

	return Result(ret, 1);
}

/* (not (and ... )) -> (or (not ...) (not ...) ...) */
/* (not (or ... )) -> (and (not ...) (not ...) ...) */
static int optimize_result_(LocalRoot local, addr pos, addr *value, int *ret)
{
	int check;
	addr opt;

	Return(type_optimize_(local, pos, &opt, &check));
	*value = check? opt: pos;

	if (ret)
		return Result(ret, check);
	else
		return 0;
}

static int extract_not_andor_(LocalRoot local,
		addr *value, addr right, enum LISPDECL decl)
{
	addr array, pos;
	size_t size, i;

	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &pos);
		type_copy_unsafe_local(local, &pos, pos);
		type_revnotdecl(pos);
		Return(optimize_result_(local, pos, &pos, NULL));
		SetArrayA4(array, i, pos);
	}
	type1_local(local, decl, array, value);

	return 0;
}

static int extract_array_andor_(LocalRoot local, addr right, addr *value, int *ret)
{
	int update, check;
	addr array, temp, pos;
	size_t size, i;

	GetArrayType(right, 0, &array);
	LenArrayA4(array, &size);
	vector4_local(local, &temp, size);
	update = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &pos);
		Return(optimize_result_(local, pos, &pos, &check));
		update |= check;
		SetArrayA4(temp, i, pos);
	}

	if (update) {
		type_copy_unsafe_local(local, &right, right);
		vector4_local(local, &array, size);
		for (i = 0; i < size; i++) {
			GetArrayA4(temp, i, &pos);
			SetArrayA4(array, i, pos);
		}
		SetArrayType(right, 0, array);
		*value = right;
	}

	return Result(ret, update);
}

static int extract_andor_(LocalRoot local,
		addr right, addr *value, int *ret,
		enum LISPDECL fromdecl, enum LISPDECL todecl)
{
	if (RefLispDecl(right) != fromdecl)
		return Result(ret, 0);
	if (RefNotDecl(right)) {
		Return(extract_not_andor_(local, value, right, todecl));
		return Result(ret, 1);
	}

	return extract_array_andor_(local, right, value, ret);
}

static int check_andor_(enum LISPDECL decl, addr right, int *ret)
{
	int check;
	addr pos;
	size_t size, i;

	if (RefLispDecl(right) != decl)
		return Result(ret, 0);
	if (RefNotDecl(right))
		return Result(ret, 1);
	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &pos);
		Return(check_optimize_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int check_and_(addr right, int *ret)
{
	return check_andor_(LISPDECL_AND, right, ret);
}
static int optimize_and_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_and_, right, ret);
	return extract_andor_(local, right, value, ret, LISPDECL_AND, LISPDECL_OR);
}

static int check_or_(addr right, int *ret)
{
	return check_andor_(LISPDECL_OR, right, ret);
}
static int optimize_or_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_or_, right, ret);
	return extract_andor_(local, right, value, ret, LISPDECL_OR, LISPDECL_AND);
}


/*
 *  and
 */
static int normlispdecl(addr pos, enum LISPDECL type)
{
	return RefLispDecl(pos) == type && (! RefNotDecl(pos));
}

static int check_typeand(addr type, addr *array, size_t *size)
{
	if (! normlispdecl(type, LISPDECL_AND))
		return 1;
	GetArrayType(type, 0, array);
	LenArrayA4(*array, size);
	return 0;
}

/* (and) -> t */
static int check_and1_(addr type, int *ret)
{
	if (! normlispdecl(type, LISPDECL_AND))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	return Result(ret, LenArrayA4r(type) == 0);
}
static int optimize_and1_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_and1_, type, ret);
	type0_local(local, LISPDECL_T, value);
	return Result(ret, 1);
}

/* (and type) -> type */
static int check_and2_(addr type, int *ret)
{
	if (! normlispdecl(type, LISPDECL_AND))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	return Result(ret, LenArrayA4r(type) == 1);
}
static int optimize_and2_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_and2_, type, ret);
	GetArrayType(type, 0, &type);
	GetArrayA4(type, 0, value);
	return Result(ret, 1);
}

/* (and ... nil ...) -> nil */
static int check_and_vector_(enum LISPDECL decl, addr type, int *ret)
{
	addr check;
	size_t size, i;

	if (check_typeand(type, &type, &size))
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int check_and3_(addr type, int *ret)
{
	return check_and_vector_(LISPDECL_NIL, type, ret);
}
static int optimize_and3_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_and3_, type, ret);
	type0_local(local, LISPDECL_NIL, value);
	return Result(ret, 1);
}

/* (and ... t ...) -> (and ...)  remove t */
static void remove_type_vector(LocalRoot local,
		enum LISPDECL decl, enum LISPDECL checktype,
		addr array, size_t size1, size_t size2, addr *value)
{
	addr pos, check;
	size_t i, k;

	vector4_local(local, &pos, size2);
	k = 0;
	for (i = 0; i < size1; i++) {
		GetArrayA4(array, i, &check);
		if (! normlispdecl(check, checktype)) {
			Check(size2 <= k, "size2 error1");
			SetArrayA4(pos, k++, check);
		}
	}
	Check(k != size2, "size2 error2");
	type1_local(local, decl, pos, value);
}

static int check_and4_(addr type, int *ret)
{
	return check_and_vector_(LISPDECL_T, type, ret);
}
static int optimize_and4_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr check;
	size_t size, i, count;

	Return_check_optimize(check_and4_, type, ret);
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	count = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, LISPDECL_T))
			count++;
	}
	Check(count == 0, "size error");
	remove_type_vector(local, LISPDECL_AND, LISPDECL_T,
			type, size, size - count, value);

	return Result(ret, 1);
}

/* (and ... (and ...) ...) -> (and ...) */
static int count_andor(addr type, enum LISPDECL decl, size_t *index)
{
	int result;
	addr check;
	size_t size, i;

	Check(! normlispdecl(type, decl), "type error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	result = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl)) {
			result = 1;
			count_andor(check, decl, index);
		}
		else {
			(*index)++;
		}
	}

	return result;
}

static void replace_andor(addr type, enum LISPDECL decl, addr array, size_t *index)
{
	addr check;
	size_t size, i;

	Check(! normlispdecl(type, decl), "type error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl)) {
			replace_andor(check, decl, array, index);
		}
		else {
			SetArrayA4(array, (*index)++, check);
		}
	}
}

static int check_andor_type_(enum LISPDECL decl, addr type, int *ret)
{
	addr pos;
	size_t size, i;

	if (! normlispdecl(type, decl))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &pos);
		if (normlispdecl(pos, decl))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int check_and5_(addr type, int *ret)
{
	return check_andor_type_(LISPDECL_AND, type, ret);
}
static int optimize_and5_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr array;
	size_t size;

	/* check */
	Return_check_optimize(check_and5_, type, ret);
	GetArrayType(type, 0, &array);
	size = 0;
	count_andor(type, LISPDECL_AND, &size);
	Check(size == 0, "size error");

	/* make type */
	vector4_local(local, &array, size);
	type1_local(local, LISPDECL_AND, array, value);
	size = 0;
	replace_andor(type, LISPDECL_AND, array, &size);

	return Result(ret, 1);
}


/*
 *  or
 */
/* (or) -> nil */
static int check_typeor(addr type, addr *array, size_t *size)
{
	if (! normlispdecl(type, LISPDECL_OR))
		return 1;
	GetArrayType(type, 0, array);
	LenArrayA4(*array, size);
	return 0;
}

static int check_or1_(addr type, int *ret)
{
	if (! normlispdecl(type, LISPDECL_OR))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	return Result(ret, LenArrayA4r(type) == 0);
}
static int optimize_or1_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_or1_, type, ret);
	type0_local(local, LISPDECL_NIL, value);
	return Result(ret, 1);
}

/* (or type) -> type */
static int check_or2_(addr type, int *ret)
{
	if (! normlispdecl(type, LISPDECL_OR))
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	return Result(ret, LenArrayA4r(type) == 1);
}
static int optimize_or2_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_or2_, type, ret);
	GetArrayType(type, 0, &type);
	GetArrayA4(type, 0, value);
	return Result(ret, 1);
}

/* (or ... t ...) -> t */
static int check_or_vector_(enum LISPDECL decl, addr type, int *ret)
{
	addr check;
	size_t size, i;

	if (check_typeor(type, &type, &size))
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int check_or3_(addr type, int *ret)
{
	return check_or_vector_(LISPDECL_T, type, ret);
}
static int optimize_or3_(LocalRoot local, addr type, addr *value, int *ret)
{
	Return_check_optimize(check_or3_, type, ret);
	type0_local(local, LISPDECL_T, value);
	return Result(ret, 1);
}

/* (or ... nil ...) -> (or ...)  remove nil */
static int check_or4_(addr type, int *ret)
{
	return check_or_vector_(LISPDECL_NIL, type, ret);
}
static int optimize_or4_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr check;
	size_t size, i, count;

	Return_check_optimize(check_or4_, type, ret);
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	count = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, LISPDECL_NIL))
			count++;
	}
	Check(count == 0, "size error");
	remove_type_vector(local, LISPDECL_OR, LISPDECL_NIL,
			type, size, size - count, value);

	return Result(ret, 1);
}

/* (or ... (or ...) ...) -> (or ...) */
static int check_or5_(addr type, int *ret)
{
	return check_andor_type_(LISPDECL_OR, type, ret);
}
static int optimize_or5_(LocalRoot local, addr type, addr *value, int *ret)
{
	addr array;
	size_t size;

	/* check */
	Return_check_optimize(check_or5_, type, ret);
	GetArrayType(type, 0, &array);
	LenArrayA4(array, &size);
	size = 0;
	count_andor(type, LISPDECL_OR, &size);
	Check(size == 0, "size error");

	/* make type */
	vector4_local(local, &array, size);
	type1_local(local, LISPDECL_OR, array, value);
	size = 0;
	replace_andor(type, LISPDECL_OR, array, &size);

	return Result(ret, 1);
}

/* range check */
static int range_valid_p_(addr type, int *ret)
{
	addr left1, left2, right1, right2;
	LocalRoot local;

	local = Local_Thread;
	GetArrayType(type, 0, &left1);
	GetArrayType(type, 1, &left2);
	GetArrayType(type, 2, &right1);
	GetArrayType(type, 3, &right2);
	if (type_asterisk_p(left1) || type_asterisk_p(right1))
		return Result(ret, 1);
	if (left1 == Nil && right1 == Nil)
		return less_equal_real_(local, left2, right2, ret);
	else
		return less_real_(local, left2, right2, ret);
}
static int check_range_(addr right, int *ret)
{
	int check;

	if (! type_range_p(right))
		return Result(ret, 0);
	Return(range_valid_p_(right, &check));
	return Result(ret, ! check);
}
static int optimize_range_(LocalRoot local, addr right, addr *value, int *ret)
{
	Return_check_optimize(check_range_, right, ret);
	if (RefNotDecl(right))
		type0_local(local, LISPDECL_T, value);
	else
		type0_local(local, LISPDECL_NIL, value);

	return Result(ret, 1);
}


/*
 *  wake optimize
 */
static int extract_values_var_(LocalRoot local, addr right, addr *value)
{
	addr root, left;

	for (root = Nil; right != Nil; ) {
		GetCons(right, &left, &right);
		Return(optimize_result_(local, left, &left, NULL));
		cons_local(local, &root, left, root);
	}
	nreverse(value, root);

	return 0;
}

static int extract_values_rest_(LocalRoot local, addr right, addr *value)
{
	int ignore;

	if (right == Nil)
		return 0;
	else
		return optimize_result_(local, right, value, &ignore);
}

static int check_some_(addr right, int *ret)
{
	int check;
	addr pos;

	while (right != Nil) {
		GetCons(right, &pos, &right);
		Return(check_optimize_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int check_values_rest_(addr right, int *ret)
{
	if (right == Nil)
		return Result(ret, 0);
	else
		return check_optimize_(right, ret);
}

static int check_values_(addr right, int *ret)
{
	int check;
	addr value;

	if (RefLispDecl(right) != LISPDECL_VALUES)
		return Result(ret, 0);
	GetArrayType(right, 0, &value);
	Return(check_some_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayType(right, 1, &value);
	Return(check_some_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayType(right, 2, &value);
	return check_values_rest_(value, ret);
}

static int optimize_values_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr var, opt, rest;

	/* extract */
	Return_check_optimize(check_values_, right, ret);
	GetArrayType(right, 0, &var);
	GetArrayType(right, 1, &opt);
	GetArrayType(right, 2, &rest);
	Return(extract_values_var_(local, var, &var));
	Return(extract_values_var_(local, opt, &opt));
	Return(extract_values_rest_(local, rest, &rest));

	/* result */
	type_copy_unsafe_local(local, &right, right);
	SetArrayType(right, 0, var);
	SetArrayType(right, 1, opt);
	SetArrayType(right, 2, rest);
	*value = right;

	return Result(ret, 1);
}

static int extract_function_key_(LocalRoot local, addr right, addr *value)
{
	addr root, left, key, type;

	if (right == T)
		return Result(value, T);
	for (root = Nil; right != Nil; ) {
		GetCons(right, &left, &right);
		GetCons(left, &key, &type);
		Return(optimize_result_(local, type, &type, NULL));
		cons_local(local, &left, key, type);
		cons_local(local, &root, left, root);
	}
	nreverse(value, root);

	return 0;
}

static int extract_function_(LocalRoot local, addr right, addr *value)
{
	addr var, opt, rest, key;

	/* extract */
	if (type_asterisk_p(right))
		return 0;
	GetArrayA2(right, 0, &var);
	GetArrayA2(right, 1, &opt);
	GetArrayA2(right, 2, &rest);
	GetArrayA2(right, 3, &key);
	Return(extract_values_var_(local, var, &var));
	Return(extract_values_var_(local, opt, &opt));
	Return(extract_values_rest_(local, rest, &rest));
	Return(extract_function_key_(local, key, &key));

	/* result */
	vector2_local(local, &right, 4);
	SetArrayA2(right, 0, var);
	SetArrayA2(right, 1, opt);
	SetArrayA2(right, 2, rest);
	SetArrayA2(right, 3, key);

	return Result(value, right);
}

static int check_function_key_(addr right, int *ret)
{
	int check;
	addr type;

	if (right == T)
		return Result(ret, 0);
	while (right != Nil) {
		GetCons(right, &type, &right);
		GetCdr(type, &type);
		Return(check_optimize_(type, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int check_function_args_(addr right, int *ret)
{
	int check;
	addr value;

	if (type_asterisk_p(right))
		return Result(ret, 0);
	GetArrayA2(right, 0, &value);
	Return(check_some_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayA2(right, 1, &value);
	Return(check_some_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayA2(right, 2, &value);
	Return(check_values_rest_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayA2(right, 3, &value);
	return check_function_key_(value, ret);
}
static int check_function_(addr right, int *ret)
{
	int check;
	enum LISPDECL decl;
	addr value;

	decl = RefLispDecl(right);
	if (decl != LISPDECL_FUNCTION && decl != LISPDECL_COMPILED_FUNCTION)
		return Result(ret, 0);
	GetArrayType(right, 0, &value);
	Return(check_function_args_(value, &check));
	if (check)
		return Result(ret, 1);
	GetArrayType(right, 1, &value);

	return check_optimize_(value, ret);
}
static int optimize_function_(LocalRoot local, addr right, addr *value, int *ret)
{
	addr args, values;

	Return_check_optimize(check_function_, right, ret);
	GetArrayType(right, 0, &args);
	GetArrayType(right, 1, &values);
	Return(extract_function_(local, args, &args));
	Return(optimize_result_(local, values, &values, NULL));
	type_copydecl_unsafe_local(local, &right, right);
	SetArrayType(right, 0, args);
	SetArrayType(right, 1, values);
	*value = right;

	return Result(ret, 1);
}

static int check_cons_(addr right, int *ret)
{
	int check;
	addr value;

	if (RefLispDecl(right) != LISPDECL_CONS)
		return Result(ret, 0);
	GetArrayType(right, 0, &value);
	if (! type_asterisk_p(value)) {
		Return(check_optimize_(value, &check));
		if (check)
			return Result(ret, 1);
	}
	GetArrayType(right, 1, &value);
	if (! type_asterisk_p(value)) {
		Return(check_optimize_(value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
static int optimize_cons(LocalRoot local, addr right, addr *value, int *ret)
{
	addr car, cdr;

	Return_check_optimize(check_cons_, right, ret);
	GetArrayType(right, 0, &car);
	GetArrayType(right, 1, &cdr);
	if (! type_asterisk_p(car)) {
		Return(optimize_result_(local, car, &car, NULL));
	}
	if (! type_asterisk_p(cdr)) {
		Return(optimize_result_(local, cdr, &cdr, NULL));
	}
	type2_local(local, LISPDECL_CONS, car, cdr, value);

	return Result(ret, 1);
}


/*
 *  type-optimize
 */
static int check_optimize_(addr type, int *ret)
{
	Return_or_optimize(check_optimized_, type, ret);
	Return_or_optimize(check_not_asterisk_, type, ret);
	Return_or_optimize(check_not_nil_, type, ret);
	Return_or_optimize(check_not_t_, type, ret);
	Return_or_optimize(check_mod_, type, ret);
	Return_or_optimize(check_atom_, type, ret);
	Return_or_optimize(check_list_, type, ret);
	Return_or_optimize(check_boolean_, type, ret);
	Return_or_optimize(check_sequence_, type, ret);
	Return_or_optimize(check_vector_, type, ret);
	Return_or_optimize(check_simple_vector_, type, ret);
	Return_or_optimize(check_bit_vector_, type, ret);
	Return_or_optimize(check_simple_bit_vector_, type, ret);
	Return_or_optimize(check_extended_char_, type, ret);
	Return_or_optimize(check_string_, type, ret);
	Return_or_optimize(check_base_string_, type, ret);
	Return_or_optimize(check_simple_string_, type, ret);
	Return_or_optimize(check_simple_base_string_, type, ret);
	Return_or_optimize(check_signed_byte_, type, ret);
	Return_or_optimize(check_unsigned_byte_, type, ret);
	Return_or_optimize(check_bit_, type, ret);
	Return_or_optimize(check_fixnum_, type, ret);
	Return_or_optimize(check_bignum_, type, ret);
	Return_or_optimize(check_eql_, type, ret);
	Return_or_optimize(check_eql_range_, type, ret);
	Return_or_optimize(check_member1_, type, ret);
	Return_or_optimize(check_member2_, type, ret);
	Return_or_optimize(check_member3_, type, ret);
	Return_or_optimize(check_not_, type, ret);
	Return_or_optimize(check_and_, type, ret);
	Return_or_optimize(check_or_, type, ret);
	Return_or_optimize(check_and1_, type, ret);
	Return_or_optimize(check_and2_, type, ret);
	Return_or_optimize(check_and3_, type, ret);
	Return_or_optimize(check_and4_, type, ret);
	Return_or_optimize(check_and5_, type, ret);
	Return_or_optimize(check_or1_, type, ret);
	Return_or_optimize(check_or2_, type, ret);
	Return_or_optimize(check_or3_, type, ret);
	Return_or_optimize(check_or4_, type, ret);
	Return_or_optimize(check_or5_, type, ret);
	Return_or_optimize(check_range_, type, ret);
	Return_or_optimize(check_values_, type, ret);
	Return_or_optimize(check_function_, type, ret);
	Return_or_optimize(check_cons_, type, ret);

	return Result(ret, 0);
}

static int type_optimize_(LocalRoot local, addr type, addr *value, int *ret)
{
	int update, loop;

	CheckType(type, LISPTYPE_TYPE);
	for (loop = 0; ; loop |= update) {
		update = 0;
		/* extract */
		extractcall(local, optimize_optimized_, type, update);
		extractcall(local, optimize_not_asterisk_, type, update);
		extractcall(local, optimize_not_nil_, type, update);
		extractcall(local, optimize_not_t_, type, update);
		extractcallnot(local, optimize_mod_, type, update);
		extractcallnot(local, optimize_atom_, type, update);
		extractcallnot(local, optimize_list_, type, update);
		extractcallnot(local, optimize_boolean_, type, update);
		extractcallnot(local, optimize_sequence_, type, update);
		extractcallnot(local, optimize_vector_, type, update);
		extractcallnot(local, optimize_simple_vector_, type, update);
		extractcallnot(local, optimize_bit_vector_, type, update);
		extractcallnot(local, optimize_simple_bit_vector_, type, update);
		extractcallnot(local, optimize_extended_char_, type, update);
		extractcallnot(local, optimize_string_, type, update);
		extractcallnot(local, optimize_base_string_, type, update);
		extractcallnot(local, optimize_simple_string_, type, update);
		extractcallnot(local, optimize_simple_base_string_, type, update);
		extractcallnot(local, optimize_signed_byte_, type, update);
		extractcallnot(local, optimize_unsigned_byte_, type, update);
		extractcallnot(local, optimize_bit_, type, update);
		extractcallnot(local, optimize_fixnum_, type, update);
		extractcallnot(local, optimize_bignum_, type, update);
		extractcallnot(local, optimize_eql_, type, update);
		extractcallnot(local, optimize_eql_range_, type, update);
		extractcallnot(local, optimize_member1_, type, update);
		extractcallnot(local, optimize_member2_, type, update);
		extractcallnot(local, optimize_member3_, type, update);
		extractcall(local, optimize_not_, type, update);
		extractcall(local, optimize_and_, type, update);
		extractcall(local, optimize_or_, type, update);
		extractcall(local, optimize_and1_, type, update);
		extractcall(local, optimize_and2_, type, update);
		extractcall(local, optimize_and3_, type, update);
		extractcall(local, optimize_and4_, type, update);
		extractcall(local, optimize_and5_, type, update);
		extractcall(local, optimize_or1_, type, update);
		extractcall(local, optimize_or2_, type, update);
		extractcall(local, optimize_or3_, type, update);
		extractcall(local, optimize_or4_, type, update);
		extractcall(local, optimize_or5_, type, update);
		extractcall(local, optimize_range_, type, update);
		extractcallnot(local, optimize_values_, type, update);
		extractcallnot(local, optimize_function_, type, update);
		extractcallnot(local, optimize_cons, type, update);
		if (update == 0)
			break;
	}
	*value = type;

	return Result(ret, loop);
}

int type_optimize_local_(LocalRoot local, addr type, addr *value, int *ret)
{
	CheckLocal(local);
	CheckType(type, LISPTYPE_TYPE);
	if (RefLispDecl(type) == LISPDECL_OPTIMIZED) {
		*value = type;
		return Result(ret, 0);
	}

	Return(type_optimize_(local, type, &type, ret));
	type1_local(local, LISPDECL_OPTIMIZED, type, value);
	return 0;
}

int type_optimize_heap_(LocalRoot local, addr type, addr *value, int *ret)
{
	LocalStack stack;

	CheckLocal(local);
	CheckType(type, LISPTYPE_TYPE);
	push_local(local, &stack);
	Return(type_optimize_local_(local, type, &type, ret));
	type_copy_heap(value, type);
	rollback_local(local, stack);

	return 0;
}

int type_optimized_p(addr type)
{
	CheckType(type, LISPTYPE_TYPE);
	return RefLispDecl(type) == LISPDECL_OPTIMIZED;
}

void get_type_optimized(addr *ret, addr type)
{
	if (type_optimized_p(type)) {
		GetArrayType(type, 0, ret);
	}
	else {
		*ret = type;
	}
}

int type_optimize_throw_heap_(LocalRoot local, addr type, addr *ret)
{
	int check;

	Return(type_optimize_heap_(local, type, &type, &check));
	get_type_optimized(ret, type);

	return 0;
}

