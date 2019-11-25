#include "bigdata.h"
#include "bignum.h"
#include "condition.h"
#include "constant.h"
#include "cons.h"
#include "type.h"
#include "type_object.h"
#include "type_optimize.h"
#include "type_parse.h"
#include "type_subtypep.h"
#include "type_table.h"
#include "type_upgraded.h"

/*
 *  build
 */
#define SetConstCommon(a) { \
	addr __pos; \
	GetConst(COMMON_##a, &__pos); \
	SetConst(ARRAY_##a, __pos); \
}
#define SetConstCommonN(a,b,n) { \
	addr __pos, __value; \
	GetConst(COMMON_##a, &__pos); \
	fixnum_heap(&__value, n); \
	list_heap(&__pos, __pos, __value, NULL); \
	SetStatusReadOnly(__pos); \
	SetConst(ARRAY_##b##n, __pos); \
}
_g void build_type_upgraded(void)
{
	SetConstCommon(T);
	SetConstCommon(BIT);
	SetConstCommon(CHARACTER);
	SetConstCommon(SINGLE_FLOAT);
	SetConstCommon(DOUBLE_FLOAT);
	SetConstCommon(LONG_FLOAT);
	SetConstCommonN(SIGNED_BYTE, SIGNED, 8);
	SetConstCommonN(SIGNED_BYTE, SIGNED, 16);
	SetConstCommonN(SIGNED_BYTE, SIGNED, 32);
	SetConstCommonN(UNSIGNED_BYTE, UNSIGNED, 8);
	SetConstCommonN(UNSIGNED_BYTE, UNSIGNED, 16);
	SetConstCommonN(UNSIGNED_BYTE, UNSIGNED, 32);
#ifdef LISP_64BIT
	SetConstCommonN(SIGNED_BYTE, SIGNED, 64);
	SetConstCommonN(UNSIGNED_BYTE, UNSIGNED, 64);
#endif
}


/*
 *  upgraded-array-element-type
 */
_g int upgraded_array0_equal(addr left, addr right)
{
	enum LISPDECL decl;

	decl = LispDecl(left);
	if (decl != LispDecl(right)) {
		return 0;
	}
	if (decl == LISPDECL_SIGNED_BYTE || decl == LISPDECL_UNSIGNED_BYTE) {
		GetArrayType(left, 0, &left);
		GetArrayType(right, 0, &right);
		CheckType(left, LISPTYPE_FIXNUM);
		CheckType(right, LISPTYPE_FIXNUM);
		return RefFixnum(left) == RefFixnum(right);
	}

	return 1;
}

static int upgraded_array_unsigned(bigtype value)
{
	if (value <= UINT8_MAX) return 8;
	if (value <= UINT16_MAX) return 16;
#ifdef LISP_64BIT
	if (value <= UINT32_MAX) return 32;
	return 64;
#else
	return 32;
#endif
}

static int upgraded_array_signed(int sign, bigtype value)
{
	if (IsPlus(sign)) {
		if (value <= INT8_MAX) return 8;
		if (value <= INT16_MAX) return 16;
		if (value <= INT32_MAX) return 32;
#ifdef LISP_64BIT
		if (value <= INT64_MAX) return 64;
#endif
	}
	else {
		if (value <= ((bigtype)INT8_MAX) + 1UL) return 8;
		if (value <= ((bigtype)INT16_MAX) + 1UL) return 16;
		if (value <= ((bigtype)INT32_MAX) + 1UL) return 32;
#ifdef LISP_64BIT
		if (value <= ((bigtype)INT64_MAX) + 1ULL) return 64;
#endif
	}
	return 0;
}

static enum ARRAY_TYPE upgraded_array_integer(addr type, int *size)
{
	int sign1, sign2, size1, size2;
	addr left1, left2, right1, right2;
	bigtype value1, value2;

	/* asterisk check */
	GetArrayType(type, 0, &left1);
	if (type_asterisk_p(left1))
		return ARRAY_TYPE_T;
	GetArrayType(type, 2, &right1);
	if (type_asterisk_p(right1))
		return ARRAY_TYPE_T;

	/* left */
	GetArrayType(type, 1, &left2);
	if (castfixed_integer(left2, &sign1, &value1))
		return ARRAY_TYPE_T;
	if (left1 == T)
		value1++;

	/* right */
	GetArrayType(type, 3, &right2);
	if (castfixed_integer(right2, &sign2, &value2))
		return ARRAY_TYPE_T;
	if (right1 == T)
		value2--;

	/* value */
	if (IsPlus(sign1)) {
		if (value1 == 0 && value2 == 1)
			return ARRAY_TYPE_BIT;
		size1 = upgraded_array_unsigned(value1);
		size2 = upgraded_array_unsigned(value2);
		*size = (size1 < size2)? size2: size1;
		return ARRAY_TYPE_UNSIGNED;
	}
	else {
		size1 = upgraded_array_signed(sign1, value1);
		size2 = upgraded_array_signed(sign2, value2);
		if (size1 == 0 || size2 == 0)
			return ARRAY_TYPE_T;
		*size = (size1 < size2)? size2: size1;
		return ARRAY_TYPE_SIGNED;
	}
}

static enum ARRAY_TYPE upgraded_array_decl(addr type, int *size)
{
	/* not */
	if (RefNotDecl(type))
		return ARRAY_TYPE_T;
	/* upgraded */
	switch (LispDecl(type)) {
		case LISPDECL_CHARACTER:
		case LISPDECL_BASE_CHAR:
		case LISPDECL_STANDARD_CHAR:
			return ARRAY_TYPE_CHARACTER;

		case LISPDECL_INTEGER:
			return upgraded_array_integer(type, size);

		case LISPDECL_SINGLE_FLOAT:
			return ARRAY_TYPE_SINGLE_FLOAT;

		case LISPDECL_DOUBLE_FLOAT:
			return ARRAY_TYPE_DOUBLE_FLOAT;

		case LISPDECL_LONG_FLOAT:
			return ARRAY_TYPE_LONG_FLOAT;

		default:
			break;
	}

	return ARRAY_TYPE_T;
}

static void upgraded_array_optimize(LocalRoot local,
		addr type, enum ARRAY_TYPE *ret, int *size)
{
	LocalStack stack;

	CheckType(type, LISPTYPE_TYPE);
	/* local */
	push_local(local, &stack);
	/* upgraded-array */
	type_optimize_local(local, &type, type);
	Check(! type_optimized_p(type), "optimize error");
	get_type_optimized(&type, type);
	*size = 0;
	*ret = upgraded_array_decl(type, size);
	/* free */
	rollback_local(local, stack);
}

_g void upgraded_array_value(addr type, enum ARRAY_TYPE *ret, int *size)
{
	upgraded_array_optimize(Local_Thread, type, ret, size);
}

static void upgraded_array_type_signed(int size, addr *ret)
{
	switch (size) {
		case 8:
			GetTypeTable(ret, Array_Signed8);
			break;

		case 16:
			GetTypeTable(ret, Array_Signed16);
			break;

		case 32:
			GetTypeTable(ret, Array_Signed32);
			break;

#ifdef LISP_64BIT
		case 64:
			GetTypeTable(ret, Array_Signed64);
			break;
#endif
		default:
			GetTypeTable(ret, Array_T);
			break;
	}
}

static void upgraded_array_type_unsigned(int size, addr *ret)
{
	switch (size) {
		case 8:
			GetTypeTable(ret, Array_Unsigned8);
			break;

		case 16:
			GetTypeTable(ret, Array_Unsigned16);
			break;

		case 32:
			GetTypeTable(ret, Array_Unsigned32);
			break;

#ifdef LISP_64BIT
		case 64:
			GetTypeTable(ret, Array_Unsigned64);
			break;
#endif
		default:
			GetTypeTable(ret, Array_T);
			break;
	}
}

_g void upgraded_array_object(enum ARRAY_TYPE type, int size, addr *ret)
{
	switch (type) {
		case ARRAY_TYPE_BIT:
			GetTypeTable(ret, Array_Bit);
			break;

		case ARRAY_TYPE_CHARACTER:
			GetTypeTable(ret, Array_Character);
			break;

		case ARRAY_TYPE_SIGNED:
			upgraded_array_type_signed(size, ret);
			break;

		case ARRAY_TYPE_UNSIGNED:
			upgraded_array_type_unsigned(size, ret);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			GetTypeTable(ret, Array_SingleFloat);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			GetTypeTable(ret, Array_DoubleFloat);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			GetTypeTable(ret, Array_LongFloat);
			break;

		default:
			GetTypeTable(ret, Array_T);
			break;
	}
}

static void type_upgraded_type_local(LocalRoot local, addr type, addr *ret)
{
	enum ARRAY_TYPE value;
	int size;

	CheckType(type, LISPTYPE_TYPE);
	size = 0;
	upgraded_array_optimize(local, type, &value, &size);
	upgraded_array_object(value, size, ret);
}

_g void upgraded_array_type(addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	type_upgraded_type_local(Local_Thread, type, ret);
}

static void upgraded_array_const_signed(int size, addr *ret)
{
	switch (size) {
		case 8:
			GetConst(ARRAY_SIGNED8, ret);
			break;

		case 16:
			GetConst(ARRAY_SIGNED16, ret);
			break;

		case 32:
			GetConst(ARRAY_SIGNED32, ret);
			break;

#ifdef LISP_64BIT
		case 64:
			GetConst(ARRAY_SIGNED64, ret);
			break;
#endif
		default:
			GetConst(ARRAY_T, ret);
			break;
	}
}

static void upgraded_array_const_unsigned(int size, addr *ret)
{
	switch (size) {
		case 8:
			GetConst(ARRAY_UNSIGNED8, ret);
			break;

		case 16:
			GetConst(ARRAY_UNSIGNED16, ret);
			break;

		case 32:
			GetConst(ARRAY_UNSIGNED32, ret);
			break;

#ifdef LISP_64BIT
		case 64:
			GetConst(ARRAY_UNSIGNED64, ret);
			break;
#endif
		default:
			GetConst(ARRAY_T, ret);
			break;
	}
}

_g void upgraded_array_const(enum ARRAY_TYPE type, int size, addr *ret)
{
	switch (type) {
		case ARRAY_TYPE_BIT:
			GetConst(ARRAY_BIT, ret);
			break;

		case ARRAY_TYPE_CHARACTER:
			GetConst(ARRAY_CHARACTER, ret);
			break;

		case ARRAY_TYPE_SIGNED:
			upgraded_array_const_signed(size, ret);
			break;

		case ARRAY_TYPE_UNSIGNED:
			upgraded_array_const_unsigned(size, ret);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			GetConst(ARRAY_SINGLE_FLOAT, ret);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			GetConst(ARRAY_DOUBLE_FLOAT, ret);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			GetConst(ARRAY_LONG_FLOAT, ret);
			break;

		default:
			GetConst(ARRAY_T, ret);
			break;
	}
}

_g int upgraded_array_common(Execute ptr, addr env, addr pos, addr *ret)
{
	int size;
	enum ARRAY_TYPE type;

	if (parse_type(ptr, &pos, pos, env)) return 1;
	upgraded_array_optimize(ptr->local, pos, &type, &size);
	upgraded_array_const(type, size, ret);
	return 0;
}

/* make local */
_g void upgraded_array_t_local(LocalRoot local, addr *ret)
{
	type0_local(local, LISPDECL_T, ret);
}

_g void upgraded_array_bit_local(LocalRoot local, addr *ret)
{
	type0_local(local, LISPDECL_BIT, ret);
}

_g void upgraded_array_character_local(LocalRoot local, addr *ret)
{
	type0_local(local, LISPDECL_CHARACTER, ret);
}


/*
 *  upgraded-complex-part-type
 */
_g void upgraded_complex_type(addr type, addr *ret)
{
	int validp;
	addr right;

	CheckType(type, LISPTYPE_TYPE);
	/* integer */
	GetTypeTable(&right, Integer);
	if (subtypep_clang(type, right, &validp)) {
		*ret = right;
		return;
	}

	/* rational */
	GetTypeTable(&right, Rational);
	if (subtypep_clang(type, right, &validp)) {
		*ret = right;
		return;
	}

	/* single-float */
	GetTypeTable(&right, SingleFloat);
	if (subtypep_clang(type, right, &validp)) {
		*ret = right;
		return;
	}

	/* double-float */
	GetTypeTable(&right, DoubleFloat);
	if (subtypep_clang(type, right, &validp)) {
		*ret = right;
		return;
	}

	/* long-float */
	GetTypeTable(&right, LongFloat);
	if (subtypep_clang(type, right, &validp)) {
		*ret = right;
		return;
	}

	/* Real */
	GetTypeTable(&right, Real);
	if (subtypep_clang(type, right, &validp)) {
		GetTypeTable(ret, SingleFloat); /* single-float */
		return;
	}

	/* error */
	type_object(&type, type);
	fmte("COMPLEX type ~S must be a subtype of a real.", type, NULL);
	*ret = 0;
}

static void upgraded_complex_const(addr pos, addr *ret)
{
	int validp;
	addr right;

	CheckType(pos, LISPTYPE_TYPE);
	/* integer */
	GetTypeTable(&right, Integer);
	if (subtypep_clang(pos, right, &validp)) {
		GetConst(COMMON_INTEGER, ret);
		return;
	}

	/* rational */
	GetTypeTable(&right, Rational);
	if (subtypep_clang(pos, right, &validp)) {
		GetConst(COMMON_RATIONAL, ret);
		return;
	}

	/* single-float */
	GetTypeTable(&right, SingleFloat);
	if (subtypep_clang(pos, right, &validp)) {
		GetConst(COMMON_SINGLE_FLOAT, ret);
		return;
	}

	/* double-float */
	GetTypeTable(&right, DoubleFloat);
	if (subtypep_clang(pos, right, &validp)) {
		GetConst(COMMON_DOUBLE_FLOAT, ret);
		return;
	}

	/* long-float */
	GetTypeTable(&right, LongFloat);
	if (subtypep_clang(pos, right, &validp)) {
		GetConst(COMMON_LONG_FLOAT, ret);
		return;
	}

	/* short-float */
	GetTypeTable(&right, Real);
	if (subtypep_clang(pos, right, &validp)) {
		GetConst(COMMON_SINGLE_FLOAT, ret); /* single-float */
		return;
	}

	/* error */
	type_object(&pos, pos);
	fmte("COMPLEX type ~S must be a subtype of a real.", pos, NULL);
	*ret = 0;
}

_g int upgraded_complex_common(Execute ptr, addr env, addr pos, addr *ret)
{
	if (parse_type(ptr, &pos, pos, env)) return 1;
	upgraded_complex_const(pos, ret);
	CheckType(*ret, LISPTYPE_SYMBOL);
	return 0;
}

