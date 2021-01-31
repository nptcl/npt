#include "bignum_data.h"
#include "bignum_object.h"
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
void build_type_upgraded(void)
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
int upgraded_array0_equal(addr left, addr right)
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

static int upgraded_array_optimize_(LocalRoot local,
		addr type, enum ARRAY_TYPE *ret, int *size)
{
	int ignore;
	LocalStack stack;

	CheckType(type, LISPTYPE_TYPE);
	/* local */
	push_local(local, &stack);
	/* upgraded-array */
	Return(type_optimize_local_(local, type, &type, &ignore));
	Check(! type_optimized_p(type), "optimize error");
	get_type_optimized(&type, type);
	*size = 0;
	*ret = upgraded_array_decl(type, size);
	/* free */
	rollback_local(local, stack);

	return 0;
}

int upgraded_array_value_(addr type, enum ARRAY_TYPE *ret, int *size)
{
	return upgraded_array_optimize_(Local_Thread, type, ret, size);
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

void upgraded_array_object(enum ARRAY_TYPE type, int size, addr *ret)
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

static int type_upgraded_type_local_(LocalRoot local, addr type, addr *ret)
{
	enum ARRAY_TYPE value;
	int size;

	CheckType(type, LISPTYPE_TYPE);
	size = 0;
	Return(upgraded_array_optimize_(local, type, &value, &size));
	upgraded_array_object(value, size, ret);

	return 0;
}

int upgraded_array_type_(addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	return type_upgraded_type_local_(Local_Thread, type, ret);
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

void upgraded_array_const(enum ARRAY_TYPE type, int size, addr *ret)
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

int upgraded_array_common(Execute ptr, addr env, addr pos, addr *ret)
{
	int size;
	enum ARRAY_TYPE type;

	if (env == Unbound)
		env = Nil;
	Return(parse_type(ptr, &pos, pos, env));
	Return(upgraded_array_optimize_(ptr->local, pos, &type, &size));
	upgraded_array_const(type, size, ret);

	return 0;
}

/* make local */
void upgraded_array_t_local(LocalRoot local, addr *ret)
{
	type0_local(local, LISPDECL_T, ret);
}

void upgraded_array_bit_local(LocalRoot local, addr *ret)
{
	type0_local(local, LISPDECL_BIT, ret);
}

void upgraded_array_character_local(LocalRoot local, addr *ret)
{
	type0_local(local, LISPDECL_CHARACTER, ret);
}


/*
 *  upgraded-complex-part-type
 */
int upgraded_complex_type_(Execute ptr, addr env, addr type, addr *ret)
{
	int value;
	addr right;

	CheckType(type, LISPTYPE_TYPE);
	/* integer */
	GetTypeTable(&right, Integer);
	Return(subtypep_check_(ptr, type, right, env, &value, NULL));
	if (value)
		return Result(ret, right);

	/* rational */
	GetTypeTable(&right, Rational);
	Return(subtypep_check_(ptr, type, right, env, &value, NULL));
	if (value)
		return Result(ret, right);

	/* single-float */
	GetTypeTable(&right, SingleFloat);
	Return(subtypep_check_(ptr, type, right, env, &value, NULL));
	if (value)
		return Result(ret, right);

	/* double-float */
	GetTypeTable(&right, DoubleFloat);
	Return(subtypep_check_(ptr, type, right, env, &value, NULL));
	if (value)
		return Result(ret, right);

	/* long-float */
	GetTypeTable(&right, LongFloat);
	Return(subtypep_check_(ptr, type, right, env, &value, NULL));
	if (value)
		return Result(ret, right);

	/* Real */
	GetTypeTable(&right, Real);
	Return(subtypep_check_(ptr, type, right, env, &value, NULL));
	if (value) {
		GetTypeTable(ret, SingleFloat); /* single-float */
		return 0;
	}

	/* error */
	*ret = 0;
	Return(type_object_(&type, type));
	return fmte_("COMPLEX type ~S must be a subtype of a real.", type, NULL);
}

static int upgraded_complex_const_(Execute ptr, addr env, addr pos, addr *ret)
{
	int value;
	addr right;

	CheckType(pos, LISPTYPE_TYPE);
	/* integer */
	GetTypeTable(&right, Integer);
	Return(subtypep_check_(ptr, pos, right, env, &value, NULL));
	if (value) {
		GetConst(COMMON_INTEGER, ret);
		return 0;
	}

	/* rational */
	GetTypeTable(&right, Rational);
	Return(subtypep_check_(ptr, pos, right, env, &value, NULL));
	if (value) {
		GetConst(COMMON_RATIONAL, ret);
		return 0;
	}

	/* single-float */
	GetTypeTable(&right, SingleFloat);
	Return(subtypep_check_(ptr, pos, right, env, &value, NULL));
	if (value) {
		GetConst(COMMON_SINGLE_FLOAT, ret);
		return 0;
	}

	/* double-float */
	GetTypeTable(&right, DoubleFloat);
	Return(subtypep_check_(ptr, pos, right, env, &value, NULL));
	if (value) {
		GetConst(COMMON_DOUBLE_FLOAT, ret);
		return 0;
	}

	/* long-float */
	GetTypeTable(&right, LongFloat);
	Return(subtypep_check_(ptr, pos, right, env, &value, NULL));
	if (value) {
		GetConst(COMMON_LONG_FLOAT, ret);
		return 0;
	}

	/* short-float */
	GetTypeTable(&right, Real);
	Return(subtypep_check_(ptr, pos, right, env, &value, NULL));
	if (value) {
		GetConst(COMMON_SINGLE_FLOAT, ret); /* single-float */
		return 0;
	}

	/* error */
	*ret = 0;
	Return(type_object_(&pos, pos));
	return fmte_("COMPLEX type ~S must be a subtype of a real.", pos, NULL);
}

int upgraded_complex_common(Execute ptr, addr env, addr pos, addr *ret)
{
	Return(parse_type(ptr, &pos, pos, env));
	Return(upgraded_complex_const_(ptr, env, pos, ret));
	CheckType(*ret, LISPTYPE_SYMBOL);
	return 0;
}

