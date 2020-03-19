#include "bigdata.h"
#include "bignum.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "integer.h"
#include "number.h"
#include "object.h"
#include "sequence.h"
#include "type.h"
#include "type_copy.h"
#include "type_optimize.h"
#include "type_parse.h"
#include "type_upgraded.h"

/*
 *  macro
 */
static int type_optimize(LocalRoot local, addr *ret, addr type);
static int check_optimize(addr type);
typedef int (*extractcalltype)(addr *, addr);

#define CheckNotDecl(x, type) (RefLispDecl(x) == type && RefNotDecl(x))

#define extractcall(local, call, pos, update) { \
	addr __check = 0; \
	Check(GetType(pos) != LISPTYPE_TYPE, "type error"); \
	while (call(local, &__check, pos)) { \
		update = 1; \
		pos = __check; \
	} \
}

#define extractcallnot(local, call, pos, update) { \
	addr __check = 0; \
	Check(GetType(pos) != LISPTYPE_TYPE, "type error"); \
	while (call(local, &__check, pos)) { \
		update = 1; \
		if (RefNotDecl(pos)) { \
			type_revnotdecl(__check); \
		} \
		pos = __check; \
	} \
}


/*
 *  optimize
 */
static int check_optimized(addr right)
{
	enum LISPDECL check;
	GetLispDecl(right, &check);
	return check == LISPDECL_OPTIMIZED || check == LISPDECL_SUBTYPEP;
}
static int optimize_optimized(LocalRoot local, addr *ret, addr right)
{
	if (! check_optimized(right)) return 0;
	if (! RefNotDecl(right)) {
		GetArrayType(right, 0, ret);
	}
	else {
		GetArrayType(right, 0, &right);
		type_copy_unsafe_local(local, &right, right);
		type_revnotdecl(right);
		*ret = right;
	}

	return 1;
}

static int check_not_asterisk(addr right)
{
	return CheckNotDecl(right, LISPDECL_ASTERISK);
}
static int optimize_not_asterisk(LocalRoot local, addr *ret, addr right)
{
	if (! check_not_asterisk(right)) return 0;
	_fmte("Don't allow to use (not *).", NULL);
	return 1;
}

static int check_not_nil(addr right)
{
	return CheckNotDecl(right, LISPDECL_NIL);
}
static int optimize_not_nil(LocalRoot local, addr *ret, addr right)
{
	if (! check_not_nil(right)) return 0;
	type0_local(local, LISPDECL_T, ret);
	return 1;
}

static int check_not_t(addr right)
{
	return CheckNotDecl(right, LISPDECL_T);
}
static int optimize_not_t(LocalRoot local, addr *ret, addr right)
{
	if (! check_not_t(right)) return 0;
	type0_local(local, LISPDECL_NIL, ret);
	return 1;
}

/* (mod size) -> (integer 0 (size)) */
static int check_mod(addr right)
{
	return RefLispDecl(right) == LISPDECL_MOD;
}
static int optimize_mod(LocalRoot local, addr *ret, addr right)
{
	addr left, pos;

	if (! check_mod(right)) return 0;
	GetArrayType(right, 0, &left);
	Check(! integerp(left), "type error");
	Check(! plusp_integer(left), "plusp error");
	fixnum_local(local, &pos, 0);
	type4_local(local, LISPDECL_INTEGER, Nil, pos, T, left, ret);

	return 1;
}

/* atom -> (not cons) */
static int check_atom(addr right)
{
	return RefLispDecl(right) == LISPDECL_ATOM;
}
static int optimize_atom(LocalRoot local, addr *ret, addr right)
{
	addr left;

	if (! check_atom(right)) return 0;
	type2aster_localall(local, LISPDECL_CONS, &left);
	SetNotDecl(left, 1);
	*ret = left;

	return 1;
}

/* list -> (or null cons) */
static int check_list(addr right)
{
	return RefLispDecl(right) == LISPDECL_LIST;
}
static int optimize_list(LocalRoot local, addr *ret, addr right)
{
	addr pos, array;

	if (! check_list(right)) return 0;
	vector4_local(local, &array, 2);
	/* null */
	type0_local(local, LISPDECL_NULL, &pos);
	SetArrayA4(array, 0, pos);
	/* cons */
	type2aster_localall(local, LISPDECL_CONS, &pos);
	SetArrayA4(array, 1, pos);
	/* result */
	type1_local(local, LISPDECL_OR, array, ret);

	return 1;
}

/* boolean -> (or null (eql t)) */
static int check_boolean(addr right)
{
	return RefLispDecl(right) == LISPDECL_BOOLEAN;
}
static int optimize_boolean(LocalRoot local, addr *ret, addr right)
{
	addr pos, array;

	if (! check_boolean(right)) return 0;
	vector4_local(local, &array, 2);
	/* null */
	type0_local(local, LISPDECL_NULL, &pos);
	SetArrayA4(array, 0, pos);
	/* (eql t) */
	type_eql_local(local, T, &pos);
	SetArrayA4(array, 1, pos);
	/* result */
	type1_local(local, LISPDECL_OR, array, ret);

	return 1;
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

/* (vector type size) -> (array type (size)) */
static int check_vector(addr right)
{
	if (RefLispDecl(right) != LISPDECL_VECTOR) return 0;
	GetArrayType(right, 1, &right);
	return type_asterisk_p(right) || GetType(right) == LISPTYPE_FIXNUM;
}
static int optimize_vector(LocalRoot local, addr *ret, addr right)
{
	addr type;

	if (! check_vector(right)) return 0;
	GetArrayType(right, 0, &type);
	GetArrayType(right, 1, &right);
	extract_vector(local, ret, LISPDECL_ARRAY, type, right);
	return 1;
}

/* (simple-vector size) -> (simple-array t (size)) */
static int check_vector_type(enum LISPDECL decl, addr right)
{
	if (RefLispDecl(right) != decl) return 0;
	GetArrayType(right, 0, &right);
	return type_asterisk_p(right) || GetType(right) == LISPTYPE_FIXNUM;
}
static int check_simple_vector(addr right)
{
	return check_vector_type(LISPDECL_SIMPLE_VECTOR, right);
}
static int optimize_simple_vector(LocalRoot local, addr *ret, addr right)
{
	addr type;

	if (! check_simple_vector(right)) return 0;
	upgraded_array_t_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, ret, LISPDECL_SIMPLE_ARRAY, type, right);
	return 1;
}

/* (bit-vector size) -> (array bit (size)) */
static int check_bit_vector(addr right)
{
	return check_vector_type(LISPDECL_BIT_VECTOR, right);
}
static int optimize_bit_vector(LocalRoot local, addr *ret, addr right)
{
	addr type;

	if (! check_bit_vector(right)) return 0;
	upgraded_array_bit_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, ret, LISPDECL_ARRAY, type, right);
	return 1;
}

/* (simple-bit-vector size) -> (simple-array bit (size)) */
static int check_simple_bit_vector(addr right)
{
	return check_vector_type(LISPDECL_SIMPLE_BIT_VECTOR, right);
}
static int optimize_simple_bit_vector(LocalRoot local, addr *ret, addr right)
{
	addr type;

	if (! check_simple_bit_vector(right)) return 0;
	upgraded_array_bit_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, ret, LISPDECL_SIMPLE_ARRAY, type, right);
	return 1;
}

/* extended-char -> (and character (not base-char)) */
static int check_extended_char(addr right)
{
	return RefLispDecl(right) == LISPDECL_EXTENDED_CHAR;
}
static int optimize_extended_char(LocalRoot local, addr *ret, addr right)
{
	addr array;

	if (! check_extended_char(right)) return 0;
	vector4_local(local, &array, 2);
	/* character */
	type0_local(local, LISPDECL_CHARACTER, &right);
	SetArrayA4(array, 0, right);
	/* (not base-char) */
	type0_local(local, LISPDECL_BASE_CHAR, &right);
	SetNotDecl(right, 1);
	SetArrayA4(array, 1, right);
	/* result */
	type1_local(local, LISPDECL_AND, array, ret);

	return 1;
}

/* (string size) -> (vector character size) */
static int extract_string(LocalRoot local, addr *ret, addr right, addr type)
{
	GetArrayType(right, 0, &right);
	type2_local(local, LISPDECL_VECTOR, type, right, ret);
	return 1;
}

static int check_string(addr right)
{
	return RefLispDecl(right) == LISPDECL_STRING;
}
static int optimize_string(LocalRoot local, addr *ret, addr right)
{
	addr type;

	if (! check_string(right)) return 0;
	upgraded_array_character_local(local, &type);
	extract_string(local, ret, right, type);
	return 1;
}

/* (base-string size) -> (vector base-char size) */
static int check_base_string(addr right)
{
	return RefLispDecl(right) == LISPDECL_BASE_STRING;
}
static int optimize_base_string(LocalRoot local, addr *ret, addr right)
{
	addr type;

	if (! check_base_string(right)) return 0;
	upgraded_array_character_local(local, &type);
	extract_string(local, ret, right, type);
	return 1;
}

/* (simple-string size) -> (simple-array character (size)) */
static int check_simple_string(addr right)
{
	return RefLispDecl(right) == LISPDECL_SIMPLE_STRING;
}
static int optimize_simple_string(LocalRoot local, addr *ret, addr right)
{
	addr type;

	if (! check_simple_string(right)) return 0;
	upgraded_array_character_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, ret, LISPDECL_SIMPLE_ARRAY, type, right);
	return 1;
}

/* (simple-base-string size) -> (simple-array base-char (size)) */
static int check_simple_base_string(addr right)
{
	return RefLispDecl(right) == LISPDECL_SIMPLE_BASE_STRING;
}
static int optimize_simple_base_string(LocalRoot local, addr *ret, addr right)
{
	addr type;

	if (! check_simple_base_string(right)) return 0;
	upgraded_array_character_local(local, &type);
	GetArrayType(right, 0, &right);
	extract_vector(local, ret, LISPDECL_SIMPLE_ARRAY, type, right);
	return 1;
}

/* (signed-byte *) -> integer */
/* (signed-byte size) -> (integer -2^size-1 2^(size-1)-1) */
static int check_signed_byte(addr right)
{
	return RefLispDecl(right) == LISPDECL_SIGNED_BYTE;
}
static int optimize_signed_byte(LocalRoot local, addr *ret, addr right)
{
	addr left;
	fixnum value;
	fixed fixedvalue;
	size_t size;

	if (! check_signed_byte(right)) return 0;
	GetArrayType(right, 0, &right);
	if (type_asterisk_p(right)) {
		/* (signed-byte *) */
		type4aster_localall(local, LISPDECL_INTEGER, ret);
		return 1;
	}

	/*
	 *  size  : -(2^{size-1}) --- +(2^{size-1} - 1)
	 *  size=8: -(2^7)        --- +(2^7 - 1)
	 *       -> -128 --- 127
	 */
	if (GetType(right) == LISPTYPE_FIXNUM) {
		GetFixnum(right, &value);
		if (value == BIGNUM_FULLBIT) {
			GetConst(FIXNUM_MIN, &left);
			GetConst(FIXNUM_MAX, &right);
			type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, ret);
			return 1;
		}
		if (value < BIGNUM_FULLBIT) {
			value = 1LL << (value - 1LL);
			fixnum_local(local, &left, -value);
			fixnum_local(local, &right, value - 1LL);
			type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, ret);
			return 1;
		}
		size = (size_t)value;
	}
	else {
		Check(GetType(right) != LISPTYPE_BIGNUM, "type error");
		if (1 < RefSizeBignum(right))
			_fmte("Too large signed-byte value.", NULL);
		getfixed_bignum(right, 0, &fixedvalue);
		size = (size_t)fixedvalue;
	}

	/* bignum */
	power2_bigdata_alloc(local, &left, size - 1UL);
	size = RefAllocBignum(left);
	alloc_bignum(local, &right, size);
	setminusvalue_bigdata(right, left, SignPlus, 1);
	SetSignBignum(left, SignMinus);
	type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, ret);

	return 1;
}

/* (unsigned-byte *) -> (integer 0 *) */
/* (unsigned-byte size) -> (integer 0 2^size-1) */
static int check_unsigned_byte(addr right)
{
	return RefLispDecl(right) == LISPDECL_UNSIGNED_BYTE;
}
static int optimize_unsigned_byte(LocalRoot local, addr *ret, addr right)
{
	addr left;
	fixnum value;
	fixed fixedvalue;
	size_t size;

	if (! check_unsigned_byte(right)) return 0;
	GetArrayType(right, 0, &right);
	if (type_asterisk_p(right)) {
		/* (unsigned-byte *) */
		fixnum_local(local, &left, 0);
		type4_local(local, LISPDECL_INTEGER, Nil, left, right, right, ret);
		return 1;
	}

	/*
	 *  size  : 0 --- +(2^{size} - 1)
	 *  size=8: 0 --- +(2^8 - 1)
	 *       -> 0 --- 255
	 */
	if (GetType(right) == LISPTYPE_FIXNUM) {
		GetFixnum(right, &value);
		if (value == BIGNUM_FULLBIT - 1L) {
			fixnum_local(local, &left, 0);
			GetConst(FIXNUM_MAX, &right);
			type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, ret);
			return 1;
		}
		if (value < BIGNUM_FULLBIT - 1L) {
			value = 1LL << value;
			fixnum_local(local, &left, 0);
			fixnum_local(local, &right, value - 1L);
			type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, ret);
			return 1;
		}
		size = (size_t)value;
	}
	else {
		Check(GetType(right) != LISPTYPE_BIGNUM, "type error");
		if (1 < RefSizeBignum(right))
			_fmte("Too large signed-byte value.", NULL);
		getfixed_bignum(right, 0, &fixedvalue);
		size = (size_t)fixedvalue;
	}

	/* bignum */
	power2_bigdata_alloc(local, &right, size);
	setminusvalue_bigdata(right, right, SignPlus, 1);
	fixnum_local(local, &left, 0);
	type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, ret);

	return 1;
}

/* bit -> (integer 0 1) */
static int check_bit(addr right)
{
	return RefLispDecl(right) == LISPDECL_BIT;
}
static int optimize_bit(LocalRoot local, addr *ret, addr right)
{
	addr left;

	if (! check_bit(right)) return 0;
	fixnum_local(local, &left, 0);
	fixnum_local(local, &right, 1);
	type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, ret);

	return 1;
}

/* fixnum -> (integer most-negative-fixnum most-positive-fixnum) */
static int check_fixnum(addr right)
{
	return RefLispDecl(right) == LISPDECL_FIXNUM;
}
static int optimize_fixnum(LocalRoot local, addr *ret, addr right)
{
	addr left;

	if (! check_fixnum(right)) return 0;
	GetConst(FIXNUM_MIN, &left);
	GetConst(FIXNUM_MAX, &right);
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type left error");
	type4_local(local, LISPDECL_INTEGER, Nil, left, Nil, right, ret);

	return 1;
}

/* bignum -> (and integer (not fixnum)) */
static int check_bignum(addr right)
{
	return RefLispDecl(right) == LISPDECL_BIGNUM;
}
static int optimize_bignum(LocalRoot local, addr *ret, addr right)
{
	addr array, pos;

	if (! check_bignum(right)) return 0;
	vector4_local(local, &array, 2);
	/* integer */
	type4aster_localall(local, LISPDECL_INTEGER, &pos);
	SetArrayA4(array, 0, pos);
	/* (not fixnum) */
	type0_local(local, LISPDECL_FIXNUM, &pos);
	SetNotDecl(pos, 1);
	type_optimize(local, &pos, pos);
	SetArrayA4(array, 1, pos);
	/* bignum */
	type1_local(local, LISPDECL_AND, array, ret);

	return 1;
}

/* (eql nil) -> null */
static int check_eql(addr right)
{
	if (RefLispDecl(right) != LISPDECL_EQL) return 0;
	GetArrayType(right, 0, &right);
	return right == Nil;
}
static int optimize_eql(LocalRoot local, addr *ret, addr right)
{
	if (! check_eql(right)) return 0;
	type0_local(local, LISPDECL_NULL, ret);
	return 1;
}

/* (eql 10) -> (integer 10 10) */
static void eql_real_type(LocalRoot local, enum LISPDECL decl, addr value, addr *ret)
{
	type4_local(local, decl, Nil, value, Nil, value, ret);
}

static int range_to_type(LocalRoot local, addr value, addr *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			eql_real_type(local, LISPDECL_INTEGER, value, ret);
			break;

		case LISPTYPE_RATIO:
			eql_real_type(local, LISPDECL_RATIONAL, value, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			eql_real_type(local, LISPDECL_SINGLE_FLOAT, value, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			eql_real_type(local, LISPDECL_DOUBLE_FLOAT, value, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			eql_real_type(local, LISPDECL_LONG_FLOAT, value, ret);
			break;

		default:
			return 0;
	}

	return 1;
}

static int check_eql_range(addr right)
{
	enum LISPTYPE type;

	if (RefLispDecl(right) != LISPDECL_EQL) return 0;
	GetArrayType(right, 0, &right);
	type = GetType(right);
	return
		type == LISPTYPE_FIXNUM ||
		type == LISPTYPE_BIGNUM ||
		type == LISPTYPE_RATIO ||
		type == LISPTYPE_SINGLE_FLOAT ||
		type == LISPTYPE_DOUBLE_FLOAT ||
		type == LISPTYPE_LONG_FLOAT;
}
static int optimize_eql_range(LocalRoot local, addr *ret, addr right)
{
	if (! check_eql_range(right)) return 0;
	GetArrayType(right, 0, &right);
	range_to_type(local, right, ret);
	return 1;
}

/* (member) -> nil */
static int check_member1(addr right)
{
	if (RefLispDecl(right) != LISPDECL_MEMBER) return 0;
	GetArrayType(right, 0, &right);
	return LenArrayA4r(right) == 0;
}
static int optimize_member1(LocalRoot local, addr *ret, addr right)
{
	if (! check_member1(right)) return 0;
	type0_local(local, LISPDECL_NIL, ret);
	return 1;
}

/* (member arg) -> (eql arg) */
static int check_member2(addr right)
{
	if (RefLispDecl(right) != LISPDECL_MEMBER) return 0;
	GetArrayType(right, 0, &right);
	return LenArrayA4r(right) == 1;
}
static int optimize_member2(LocalRoot local, addr *ret, addr right)
{
	if (! check_member2(right)) return 0;
	GetArrayType(right, 0, &right);
	GetArrayA4(right, 0, &right);
	type_eql_local(local, right, ret);
	return 1;
}

/* (member ...) -> (or (eql arg1) (eql arg2) ...) */
static int check_member3(addr right)
{
	if (RefLispDecl(right) != LISPDECL_MEMBER) return 0;
	GetArrayType(right, 0, &right);
	return 2 <= LenArrayA4r(right);
}
static int optimize_member3(LocalRoot local, addr *ret, addr right)
{
	addr array, child;
	size_t i, size;

	if (! check_member3(right)) return 0;
	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &child);
		type_eql_local(local, child, &child);
		SetArrayA4(array, i, child);
	}
	type1_local(local, LISPDECL_OR, array, ret);

	return 1;
}

/* (not x) -> x.not */
static int check_not(addr right)
{
	return RefLispDecl(right) == LISPDECL_NOT;
}
static int optimize_not(LocalRoot local, addr *ret, addr right)
{
	if (! check_not(right)) return 0;
	if (RefNotDecl(right)) {
		/* not not */
		GetArrayType(right, 0, ret);
	}
	else {
		/* not */
		GetArrayType(right, 0, &right);
		type_copy_unsafe_local(local, &right, right);
		type_revnotdecl(right);
		*ret = right;
	}

	return 1;
}

/* (not (and ... )) -> (or (not ...) (not ...) ...) */
/* (not (or ... )) -> (and (not ...) (not ...) ...) */
static int optimize_result(LocalRoot local, addr *ret, addr pos)
{
	int result;
	addr check;

	result = type_optimize(local, &check, pos);
	*ret = result? check: pos;

	return result;
}

static void extract_not_andor(LocalRoot local,
		addr *ret, addr right, enum LISPDECL decl)
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
		optimize_result(local, &pos, pos);
		SetArrayA4(array, i, pos);
	}
	type1_local(local, decl, array, ret);
}

static int extract_array_andor(LocalRoot local, addr *ret, addr right)
{
	int update;
	addr array, temp, pos;
	size_t size, i;

	GetArrayType(right, 0, &array);
	LenArrayA4(array, &size);
	vector4_local(local, &temp, size);
	update = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &pos);
		update |= optimize_result(local, &pos, pos);
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
		*ret = right;
	}

	return update;
}

static int extract_andor(LocalRoot local,
		addr *ret, addr right, enum LISPDECL fromdecl, enum LISPDECL todecl)
{
	if (RefLispDecl(right) != fromdecl) return 0;
	if (RefNotDecl(right)) {
		extract_not_andor(local, ret, right, todecl);
		return 1;
	}

	return extract_array_andor(local, ret, right);
}

static int check_andor(enum LISPDECL decl, addr right)
{
	addr pos;
	size_t size, i;

	if (RefLispDecl(right) != decl) return 0;
	if (RefNotDecl(right)) return 1;
	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &pos);
		if (check_optimize(pos)) return 1;
	}

	return 0;
}

static int check_and(addr right)
{
	return check_andor(LISPDECL_AND, right);
}
static int optimize_and(LocalRoot local, addr *ret, addr right)
{
	if (! check_and(right)) return 0;
	extract_andor(local, ret, right, LISPDECL_AND, LISPDECL_OR);
	return 1;
}

static int check_or(addr right)
{
	return check_andor(LISPDECL_OR, right);
}
static int optimize_or(LocalRoot local, addr *ret, addr right)
{
	if (! check_or(right)) return 0;
	extract_andor(local, ret, right, LISPDECL_OR, LISPDECL_AND);
	return 1;
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
	if (! normlispdecl(type, LISPDECL_AND)) return 1;
	GetArrayType(type, 0, array);
	LenArrayA4(*array, size);
	return 0;
}

/* (and) -> t */
static int check_and1(addr type)
{
	if (! normlispdecl(type, LISPDECL_AND)) return 0;
	GetArrayType(type, 0, &type);
	return LenArrayA4r(type) == 0;
}
static int optimize_and1(LocalRoot local, addr *ret, addr type)
{
	if (! check_and1(type)) return 0;
	type0_local(local, LISPDECL_T, ret);
	return 1;
}

/* (and type) -> type */
static int check_and2(addr type)
{
	if (! normlispdecl(type, LISPDECL_AND)) return 0;
	GetArrayType(type, 0, &type);
	return LenArrayA4r(type) == 1;
}
static int optimize_and2(LocalRoot local, addr *ret, addr type)
{
	if (! check_and2(type)) return 0;
	GetArrayType(type, 0, &type);
	GetArrayA4(type, 0, ret);
	return 1;
}

/* (and ... nil ...) -> nil */
static int check_and_vector(enum LISPDECL decl, addr type)
{
	addr check;
	size_t size, i;

	if (check_typeand(type, &type, &size)) return 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl)) {
			return 1;
		}
	}

	return 0;
}
static int check_and3(addr type)
{
	return check_and_vector(LISPDECL_NIL, type);
}
static int optimize_and3(LocalRoot local, addr *ret, addr type)
{
	if (! check_and3(type)) return 0;
	type0_local(local, LISPDECL_NIL, ret);
	return 1;
}

/* (and ... t ...) -> (and ...)  remove t */
static void remove_type_vector(LocalRoot local,
		enum LISPDECL decl, enum LISPDECL checktype,
		addr array, size_t size1, size_t size2, addr *ret)
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
	type1_local(local, decl, pos, ret);
}

static int check_and4(addr type)
{
	return check_and_vector(LISPDECL_T, type);
}
static int optimize_and4(LocalRoot local, addr *ret, addr type)
{
	addr check;
	size_t size, i, count;

	if (! check_and4(type)) return 0;
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	count = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, LISPDECL_T)) count++;
	}
	Check(count == 0, "size error");
	remove_type_vector(local, LISPDECL_AND, LISPDECL_T, type, size, size - count, ret);

	return 1;
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

static int check_andor_type(enum LISPDECL decl, addr type)
{
	addr pos;
	size_t size, i;

	if (! normlispdecl(type, decl)) return 0;
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &pos);
		if (normlispdecl(pos, decl)) {
			return 1;
		}
	}

	return 0;
}
static int check_and5(addr type)
{
	return check_andor_type(LISPDECL_AND, type);
}
static int optimize_and5(LocalRoot local, addr *ret, addr type)
{
	addr array;
	size_t size;

	/* check */
	if (! check_and5(type)) return 0;
	GetArrayType(type, 0, &array);
	size = 0;
	count_andor(type, LISPDECL_AND, &size);
	Check(size == 0, "size error");

	/* make type */
	vector4_local(local, &array, size);
	type1_local(local, LISPDECL_AND, array, ret);
	size = 0;
	replace_andor(type, LISPDECL_AND, array, &size);

	return 1;
}


/*
 *  or
 */
/* (or) -> nil */
static int check_typeor(addr type, addr *array, size_t *size)
{
	if (! normlispdecl(type, LISPDECL_OR)) return 1;
	GetArrayType(type, 0, array);
	LenArrayA4(*array, size);
	return 0;
}

static int check_or1(addr type)
{
	if (! normlispdecl(type, LISPDECL_OR)) return 0;
	GetArrayType(type, 0, &type);
	return LenArrayA4r(type) == 0;
}
static int optimize_or1(LocalRoot local, addr *ret, addr type)
{
	if (! check_or1(type)) return 0;
	type0_local(local, LISPDECL_NIL, ret);
	return 1;
}

/* (or type) -> type */
static int check_or2(addr type)
{
	if (! normlispdecl(type, LISPDECL_OR)) return 0;
	GetArrayType(type, 0, &type);
	return LenArrayA4r(type) == 1;
}
static int optimize_or2(LocalRoot local, addr *ret, addr type)
{
	if (! check_or2(type)) return 0;
	GetArrayType(type, 0, &type);
	GetArrayA4(type, 0, ret);
	return 1;
}

/* (or ... t ...) -> t */
static int check_or_vector(enum LISPDECL decl, addr type)
{
	addr check;
	size_t size, i;

	if (check_typeor(type, &type, &size)) return 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, decl)) {
			return 1;
		}
	}

	return 0;
}
static int check_or3(addr type)
{
	return check_or_vector(LISPDECL_T, type);
}
static int optimize_or3(LocalRoot local, addr *ret, addr type)
{
	if (! check_or3(type)) return 0;
	type0_local(local, LISPDECL_T, ret);
	return 1;
}

/* (or ... nil ...) -> (or ...)  remove nil */
static int check_or4(addr type)
{
	return check_or_vector(LISPDECL_NIL, type);
}
static int optimize_or4(LocalRoot local, addr *ret, addr type)
{
	addr check;
	size_t size, i, count;

	if (! check_or4(type)) return 0;
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	count = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (normlispdecl(check, LISPDECL_NIL)) count++;
	}
	Check(count == 0, "size error");
	remove_type_vector(local, LISPDECL_OR, LISPDECL_NIL, type, size, size - count, ret);

	return 1;
}

/* (or ... (or ...) ...) -> (or ...) */
static int check_or5(addr type)
{
	return check_andor_type(LISPDECL_OR, type);
}
static int optimize_or5(LocalRoot local, addr *ret, addr type)
{
	addr array;
	size_t size;

	/* check */
	if (! check_or5(type)) return 0;
	GetArrayType(type, 0, &array);
	LenArrayA4(array, &size);
	size = 0;
	count_andor(type, LISPDECL_OR, &size);
	Check(size == 0, "size error");

	/* make type */
	vector4_local(local, &array, size);
	type1_local(local, LISPDECL_OR, array, ret);
	size = 0;
	replace_andor(type, LISPDECL_OR, array, &size);

	return 1;
}

/* range check */
static int range_valid_p(addr type)
{
	addr left1, left2, right1, right2;
	LocalRoot local;

	local = Local_Thread;
	GetArrayType(type, 0, &left1);
	GetArrayType(type, 1, &left2);
	GetArrayType(type, 2, &right1);
	GetArrayType(type, 3, &right2);
	if (type_asterisk_p(left1) || type_asterisk_p(right1)) return 1;
	if (left1 == Nil && right1 == Nil)
		return less_equal_real(local, left2, right2);
	else
		return less_real(local, left2, right2);
}
static int check_range(addr right)
{
	return type_range_p(right) && ! range_valid_p(right);
}
static int optimize_range(LocalRoot local, addr *ret, addr right)
{
	if (! check_range(right)) return 0;
	if (RefNotDecl(right))
		type0_local(local, LISPDECL_T, ret);
	else
		type0_local(local, LISPDECL_NIL, ret);
	return 1;
}


/*
 *  wake optimize
 */
static void extract_values_var(LocalRoot local, addr *ret, addr right)
{
	addr root, left;

	for (root = Nil; right != Nil; ) {
		GetCons(right, &left, &right);
		optimize_result(local, &left, left);
		cons_local(local, &root, left, root);
	}
	nreverse_list_unsafe(ret, root);
}

static int extract_values_rest(LocalRoot local, addr *ret, addr right)
{
	if (right == Nil) return 0;
	return optimize_result(local, ret, right);
}

static int check_some(addr right)
{
	addr pos;

	while (right != Nil) {
		GetCons(right, &pos, &right);
		if (check_optimize(pos)) return 1;
	}

	return 0;
}

static int check_values_rest(addr right)
{
	if (right == Nil) return 0;
	return check_optimize(right);
}

static int check_values(addr right)
{
	addr check;

	if (RefLispDecl(right) != LISPDECL_VALUES) return 0;
	GetArrayType(right, 0, &check);
	if (check_some(check)) return 1;
	GetArrayType(right, 1, &check);
	if (check_some(check)) return 1;
	GetArrayType(right, 2, &check);
	if (check_values_rest(check)) return 1;

	return 0;
}

static int optimize_values(LocalRoot local, addr *ret, addr right)
{
	addr var, opt, rest;

	/* extract */
	if (! check_values(right)) return 0;
	GetArrayType(right, 0, &var);
	GetArrayType(right, 1, &opt);
	GetArrayType(right, 2, &rest);
	extract_values_var(local, &var, var);
	extract_values_var(local, &opt, opt);
	extract_values_rest(local, &rest, rest);

	/* result */
	type_copy_unsafe_local(local, &right, right);
	SetArrayType(right, 0, var);
	SetArrayType(right, 1, opt);
	SetArrayType(right, 2, rest);
	*ret = right;

	return 1;
}

static void extract_function_key(LocalRoot local, addr *ret, addr right)
{
	addr root, left, key, type;

	if (right == T) {
		*ret = T;
		return;
	}
	for (root = Nil; right != Nil; ) {
		GetCons(right, &left, &right);
		GetCons(left, &key, &type);
		optimize_result(local, &type, type);
		cons_local(local, &left, key, type);
		cons_local(local, &root, left, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void extract_function(LocalRoot local, addr *ret, addr right)
{
	addr var, opt, rest, key;

	/* extract */
	if (type_asterisk_p(right)) return;
	GetArrayA2(right, 0, &var);
	GetArrayA2(right, 1, &opt);
	GetArrayA2(right, 2, &rest);
	GetArrayA2(right, 3, &key);
	extract_values_var(local, &var, var);
	extract_values_var(local, &opt, opt);
	extract_values_rest(local, &rest, rest);
	extract_function_key(local, &key, key);

	/* result */
	vector2_local(local, &right, 4);
	SetArrayA2(right, 0, var);
	SetArrayA2(right, 1, opt);
	SetArrayA2(right, 2, rest);
	SetArrayA2(right, 3, key);
	*ret = right;
}

static int check_function_key(addr right)
{
	addr type;

	if (right == T) return 0;
	while (right != Nil) {
		GetCons(right, &type, &right);
		GetCdr(type, &type);
		if (check_optimize(type)) {
			return 1;
		}
	}

	return 0;
}
static int check_function_args(addr right)
{
	addr check;

	if (type_asterisk_p(right)) return 0;
	GetArrayA2(right, 0, &check);
	if (check_some(check)) return 1;
	GetArrayA2(right, 1, &check);
	if (check_some(check)) return 1;
	GetArrayA2(right, 2, &check);
	if (check_values_rest(check)) return 1;
	GetArrayA2(right, 3, &check);
	if (check_function_key(check)) return 1;

	return 0;
}
static int check_function(addr right)
{
	enum LISPDECL decl;
	addr check;

	decl = RefLispDecl(right);
	if (decl != LISPDECL_FUNCTION && decl != LISPDECL_COMPILED_FUNCTION) return 0;
	GetArrayType(right, 0, &check);
	if (check_function_args(check)) return 1;
	GetArrayType(right, 1, &check);

	return check_optimize(check);
}
static int optimize_function(LocalRoot local, addr *ret, addr right)
{
	addr args, values;

	if (! check_function(right)) return 0;
	GetArrayType(right, 0, &args);
	GetArrayType(right, 1, &values);
	extract_function(local, &args, args);
	optimize_result(local, &values, values);
	type_copydecl_unsafe_local(local, &right, right);
	SetArrayType(right, 0, args);
	SetArrayType(right, 1, values);
	*ret = right;

	return 1;
}

static int check_cons(addr right)
{
	addr check;

	if (RefLispDecl(right) != LISPDECL_CONS) return 0;
	GetArrayType(right, 0, &check);
	if (! type_asterisk_p(check) && check_optimize(check)) return 1;
	GetArrayType(right, 1, &check);
	if (! type_asterisk_p(check) && check_optimize(check)) return 1;

	return 0;
}
static int optimize_cons(LocalRoot local, addr *ret, addr right)
{
	addr car, cdr;

	if (! check_cons(right)) return 0;
	GetArrayType(right, 0, &car);
	GetArrayType(right, 1, &cdr);
	if (! type_asterisk_p(car))
		optimize_result(local, &car, car);
	if (! type_asterisk_p(cdr))
		optimize_result(local, &cdr, cdr);
	type2_local(local, LISPDECL_CONS, car, cdr, ret);

	return 1;
}


/*
 *  type-optimize
 */
static int check_optimize(addr type)
{
	return check_optimized(type)
		|| check_not_asterisk(type)
		|| check_not_nil(type)
		|| check_not_t(type)
		|| check_mod(type)
		|| check_atom(type)
		|| check_list(type)
		|| check_boolean(type)
		|| check_vector(type)
		|| check_simple_vector(type)
		|| check_bit_vector(type)
		|| check_simple_bit_vector(type)
		|| check_extended_char(type)
		|| check_string(type)
		|| check_base_string(type)
		|| check_simple_string(type)
		|| check_simple_base_string(type)
		|| check_signed_byte(type)
		|| check_unsigned_byte(type)
		|| check_bit(type)
		|| check_fixnum(type)
		|| check_bignum(type)
		|| check_eql(type)
		|| check_eql_range(type)
		|| check_member1(type)
		|| check_member2(type)
		|| check_member3(type)
		|| check_not(type)
		|| check_and(type)
		|| check_or(type)
		|| check_and1(type)
		|| check_and2(type)
		|| check_and3(type)
		|| check_and4(type)
		|| check_and5(type)
		|| check_or1(type)
		|| check_or2(type)
		|| check_or3(type)
		|| check_or4(type)
		|| check_or5(type)
		|| check_range(type)
		|| check_values(type)
		|| check_function(type)
		|| check_cons(type);
}

static int type_optimize(LocalRoot local, addr *ret, addr type)
{
	int update, result;

	CheckType(type, LISPTYPE_TYPE);
	for (result = 0; ; result |= update) {
		update = 0;
		/* extract */
		extractcall(local, optimize_optimized, type, update);
		extractcall(local, optimize_not_asterisk, type, update);
		extractcall(local, optimize_not_nil, type, update);
		extractcall(local, optimize_not_t, type, update);
		extractcallnot(local, optimize_mod, type, update);
		extractcallnot(local, optimize_atom, type, update);
		extractcallnot(local, optimize_list, type, update);
		extractcallnot(local, optimize_boolean, type, update);
		extractcallnot(local, optimize_vector, type, update);
		extractcallnot(local, optimize_simple_vector, type, update);
		extractcallnot(local, optimize_bit_vector, type, update);
		extractcallnot(local, optimize_simple_bit_vector, type, update);
		extractcallnot(local, optimize_extended_char, type, update);
		extractcallnot(local, optimize_string, type, update);
		extractcallnot(local, optimize_base_string, type, update);
		extractcallnot(local, optimize_simple_string, type, update);
		extractcallnot(local, optimize_simple_base_string, type, update);
		extractcallnot(local, optimize_signed_byte, type, update);
		extractcallnot(local, optimize_unsigned_byte, type, update);
		extractcallnot(local, optimize_bit, type, update);
		extractcallnot(local, optimize_fixnum, type, update);
		extractcallnot(local, optimize_bignum, type, update);
		extractcallnot(local, optimize_eql, type, update);
		extractcallnot(local, optimize_eql_range, type, update);
		extractcallnot(local, optimize_member1, type, update);
		extractcallnot(local, optimize_member2, type, update);
		extractcallnot(local, optimize_member3, type, update);
		extractcall(local, optimize_not, type, update);
		extractcall(local, optimize_and, type, update);
		extractcall(local, optimize_or, type, update);
		extractcall(local, optimize_and1, type, update);
		extractcall(local, optimize_and2, type, update);
		extractcall(local, optimize_and3, type, update);
		extractcall(local, optimize_and4, type, update);
		extractcall(local, optimize_and5, type, update);
		extractcall(local, optimize_or1, type, update);
		extractcall(local, optimize_or2, type, update);
		extractcall(local, optimize_or3, type, update);
		extractcall(local, optimize_or4, type, update);
		extractcall(local, optimize_or5, type, update);
		extractcall(local, optimize_range, type, update);
		extractcallnot(local, optimize_values, type, update);
		extractcallnot(local, optimize_function, type, update);
		extractcallnot(local, optimize_cons, type, update);
		if (update == 0) break;                                                               }
	*ret = type;

	return result;
}

_g int type_optimize_local(LocalRoot local, addr *ret, addr type)
{
	int result;

	CheckLocal(local);
	CheckType(type, LISPTYPE_TYPE);
	if (RefLispDecl(type) == LISPDECL_OPTIMIZED) {
		result = 0;
		*ret = type;
	}
	else {
		result = type_optimize(local, &type, type);
		type1_local(local, LISPDECL_OPTIMIZED, type, ret);
	}

	return result;
}

_g int type_optimize_heap(LocalRoot local, addr *ret, addr type)
{
	int result;
	LocalStack stack;

	CheckLocal(local);
	CheckType(type, LISPTYPE_TYPE);
	push_local(local, &stack);
	result = type_optimize_local(local, &type, type);
	type_copy_heap(ret, type);
	rollback_local(local, stack);

	return result;
}

_g int type_optimized_p(addr type)
{
	CheckType(type, LISPTYPE_TYPE);
	return RefLispDecl(type) == LISPDECL_OPTIMIZED;
}

_g void get_type_optimized(addr *ret, addr type)
{
	if (type_optimized_p(type)) {
		GetArrayType(type, 0, ret);
	}
	else {
		*ret = type;
	}
}

