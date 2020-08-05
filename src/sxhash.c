#include "bignum_data.h"
#include "bignum_object.h"
#include "build.h"
#include "character.h"
#include "condition.h"
#include "heap.h"
#include "heap_memory.h"
#include "memory.h"
#include "object.h"
#include "pathname_object.h"
#include "ratio.h"
#include "real_decode.h"
#include "strtype.h"
#include "typedef.h"

/*
 *  Declarations
 */
#define FixedSize (sizeof(fixed))
#define FIXNUM_BIT1M (LISP_INTEGER_BIT - 1ULL)
#define FIXNUM_MASK_SIGN (1ULL << FIXNUM_BIT1M)
#define FIXNUM_MASK_BODY (FIXNUM_MASK_SIGN - 1ULL)
#define FIXNUM_BODY(x)  ((fixnum)((x) & FIXNUM_MASK_BODY))

typedef int (*calltype_sxhash)(addr pos, int depth, fixed *ret);
static calltype_sxhash SxhashTableEqual[LISPTYPE_SIZE];
static calltype_sxhash SxhashTableEqualp[LISPTYPE_SIZE];
static int sxfixed_equal_(addr pos, int depth, fixed *ret);
static int sxfixed_equalp_(addr pos, int depth, fixed *ret);


/*
 *  eq
 */
#define sxhash_diffshift(x,y,z) ((((uintptr_t)(x)) - ((uintptr_t)(y))) >> (z))
#define sxhash_diffheap(x,s) ((fixed)sxhash_diffshift(heap_alloc, (x), (s)))
static int sxfixed_eq_(addr pos, int depth, fixed *ret)
{
#ifdef LISP_ARCH_64BIT
	return Result(ret, sxhash_diffheap(pos, 3));
#else
	return Result(ret, sxhash_diffheap(pos, 2));
#endif
}


/*
 *  character
 */
static int sxfixed_character_(addr pos, int depth, fixed *ret)
{
	unicode c;
	GetCharacter(pos, &c);
	return Result(ret, (fixed)c);
}

static int sxfixed_character_p_(addr pos, int depth, fixed *ret)
{
	unicode c;
	GetCharacter(pos, &c);
	return Result(ret, (fixed)toUpperUnicode(c));
}


/*
 *  character2
 */
static int sxfixed_character2_(addr pos, int depth, fixed *ret)
{
	fixed a, b;

	a = (fixed)refcharacter2a(pos);
	b = (fixed)refcharacter2b(pos);

	return Result(ret, a + b);
}

static int sxfixed_character2_p_(addr pos, int depth, fixed *ret)
{
	unicode a, b;
	fixed c, d;

	getcharacter2a(pos, &a);
	getcharacter2b(pos, &b);
	c = (fixed)toUpperUnicode(a);
	d = (fixed)toUpperUnicode(b);

	return Result(ret, c + d);
}


/*
 *  binary
 */
static int sxfixed_binary_(const byte *u, size_t size, fixed *ret)
{
	int m;
	size_t i;
	fixed p[FixedSize], value;

	cleartype(p);
	for (i = 0; i < size; i++) {
		m = i % FixedSize;
		p[m] += u[i];
	}
	value = (fixed)size;
	for (i = 0; i < FixedSize; i++)
		value += p[i] << (i * 8);

	return Result(ret, value);
}

static int sxhash_binary_equal_(const void *pos, size_t size, fixnum *ret)
{
	fixed value;
	Return(sxfixed_binary_((const byte *)pos, size, &value));
	return Result(ret, FIXNUM_BODY(value));
}

_g int sxhash_char_equal_(const char *pos, fixnum *ret)
{
	return sxhash_binary_equal_((const void *)pos, strlen(pos), ret);
}

static int fixed_binary_p_(const byte *u, size_t size, fixed *ret)
{
	int m;
	size_t i;
	fixed p[FixedSize], value;

	cleartype(p);
	for (i = 0; i < size; i++) {
		m = i % FixedSize;
		p[m] += toupperunicode(u[i]);
	}
	value = (fixed)size;
	for (i = 0; i < FixedSize; i++)
		value += p[i] << (i * 8);

	return Result(ret, value);
}

static int sxhash_binary_equalp_(const void *pos, size_t size, fixnum *ret)
{
	fixed value;
	Return(fixed_binary_p_((const byte *)pos, size, &value));
	return Result(ret, FIXNUM_BODY(value));
}

_g int sxhash_char_equalp_(const char *pos, fixnum *ret)
{
	return sxhash_binary_equalp_((const void *)pos, strlen(pos), ret);
}


/*
 *  string
 */
static int sxfixed_string_(addr pos, int depth, fixed *ret)
{
	int m;
	size_t i, len;
	fixed p[FixedSize], value;
	unicode u;

	cleartype(p);
	string_length(pos, &len);
	for (i = 0; i < len; i++) {
		Return(string_getc_(pos, i, &u));
		m = i % FixedSize;
		p[m] += u;
	}
	value = (fixed)len;
	for (i = 0; i < FixedSize; i++)
		value += p[i] << (i * 8);

	return Result(ret, value);
}

static int sxfixed_string_p_(addr pos, int depth, fixed *ret)
{
	int m;
	size_t i, len;
	fixed p[FixedSize], value;
	unicode u;

	cleartype(p);
	string_length(pos, &len);
	for (i = 0; i < len; i++) {
		Return(string_getc_(pos, i, &u));
		m = i % FixedSize;
		p[m] += toupperunicode(u);
	}
	value = (fixed)len;
	for (i = 0; i < FixedSize; i++)
		value += p[i] << (i * 8);

	return Result(ret, value);
}


/*
 *  fixnum
 */
static int sxfixed_fixnum_(addr pos, int depth, fixed *ret)
{
	fixnum value;
	GetFixnum(pos, &value);
	return Result(ret, (fixed)value);
}


/*
 *  bignum
 */
static int sxfixed_bignum_(addr pos, int depth, fixed *ret)
{
	int sign;
	bigtype *data;
	size_t size, i;
	fixed value;

	GetSignBignum(pos, &sign);
	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);

	value = (fixed)sign;
	value += (fixed)size;
	for (i = 0; i < size; i++)
		value += (fixed)data[i];

	return Result(ret, value);
}


/*
 *  ratio
 */
static int sxfixed_ratio_(addr pos, int depth, fixed *ret)
{
	int sign;
	addr numer, denom;
	fixed value, v;

	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	value = (fixed)sign;
	Return(sxfixed_bignum_(numer, depth, &v));
	value += v;
	Return(sxfixed_bignum_(numer, depth, &v));
	value += v;

	return Result(ret, value);
}


/*
 *  float
 */
static int sxfixed_single_float_(addr pos, int depth, fixed *ret)
{
	single_float value;
	GetSingleFloat(pos, &value);
	return sxfixed_binary_((const byte *)&value, sizeoft(value), ret);
}

static int sxfixed_double_float_(addr pos, int depth, fixed *ret)
{
	double_float value;
	GetDoubleFloat(pos, &value);
	return sxfixed_binary_((const byte *)&value, sizeoft(value), ret);
}

static int sxfixed_long_float_(addr pos, int depth, fixed *ret)
{
	long_float value;
	GetLongFloat(pos, &value);
	return sxfixed_binary_((const byte *)&value, sizeoft(value), ret);
}

static int sxfixed_pathname_(addr pos, int depth, fixed *ret)
{
	int i;
	addr child;
	fixed result, v;

	result = 0;
	depth--;
	for (i = 0; i < PATHNAME_INDEX_SIZE; i++) {
		GetArrayA2(pos, i, &child);
		Return(sxfixed_equal_(child, depth, &v));
		result += v;
	}

	return Result(ret, result);
}

static int sxfixed_float_p_(addr pos, int depth, fixed *ret)
{
	Execute ptr;

	ptr = Execute_Thread;
	Return(rationalize_common_(ptr, pos, &pos));
	return sxfixed_equalp_(pos, depth, ret);
}


/*
 *  Global function
 */
static int sxfixed_equal_(addr pos, int depth, fixed *ret)
{
	if (pos == Nil)
		return Result(ret, 0);
	if (pos == T)
		return Result(ret, 1);

	return (SxhashTableEqual[(size_t)GetType(pos)])(pos, depth, ret);
}

static int sxfixed_equalp_(addr pos, int depth, fixed *ret)
{
	if (pos == Nil)
		return Result(ret, 0);
	if (pos == T)
		return Result(ret, 1);

	return (SxhashTableEqualp[(size_t)GetType(pos)])(pos, depth, ret);
}

static int sxhash_call_(addr pos, int depth, fixnum *ret, calltype_sxhash call)
{
	fixed value;
	Return((*call)(pos, depth, &value));
	return Result(ret, FIXNUM_BODY(value));
}

_g int sxhash_equal_depth_(addr pos, int depth, fixnum *ret)
{
	return sxhash_call_(pos, depth, ret, sxfixed_equal_);
}

_g int sxhash_equal_(addr pos, fixnum *ret)
{
	return sxhash_call_(pos, -1, ret, sxfixed_equal_);
}

_g int sxhash_equalp_depth_(addr pos, int depth, fixnum *ret)
{
	return sxhash_call_(pos, depth, ret, sxfixed_equalp_);
}

_g int sxhash_equalp_(addr pos, fixnum *ret)
{
	return sxhash_call_(pos, -1, ret, sxfixed_equalp_);
}

_g int sxhash_eq_(addr pos, fixnum *ret)
{
	return sxhash_call_(pos, -1, ret, sxfixed_eq_);
}

_g int sxhash_unicode_equalp_(unicode pos, fixnum *ret)
{
	fixed value;
	value = (fixed)toUpperUnicode(pos);
	return Result(ret, FIXNUM_BODY(value));
}

_g int sxhash_unicode_equal_(unicode pos, fixnum *ret)
{
	fixnum value;
	value = (fixed)pos;
	return Result(ret, FIXNUM_BODY(value));
}

_g int sxhash_character2_equal_(unicode a, unicode b, fixnum *ret)
{
	fixed c, d;

	c = (fixed)a;
	d = (fixed)b;

	return Result(ret, FIXNUM_BODY(c + d));
}

_g int sxhash_character2_equalp_(unicode a, unicode b, fixnum *ret)
{
	fixed c, d;

	c = (fixed)toUpperUnicode(a);
	d = (fixed)toUpperUnicode(b);

	return Result(ret, FIXNUM_BODY(c + d));
}


/*
 *  cons
 */
static int sxfixed_cons_(addr pos, int depth, fixed *ret)
{
	addr right;
	fixed v1, v2;

	if (depth == 0)
		return Result(ret, 0);
	GetCons(pos, &pos, &right);
	depth--;
	Return(sxfixed_equal_(pos, depth, &v1));
	Return(sxfixed_equal_(right, depth, &v2));

	return Result(ret, depth + v1 + (3 * v2));
}

static int sxfixed_cons_p_(addr pos, int depth, fixed *ret)
{
	addr right;
	fixed v1, v2;

	if (depth == 0)
		return Result(ret, 0);
	GetCons(pos, &pos, &right);
	depth--;
	Return(sxfixed_equalp_(pos, depth, &v1));
	Return(sxfixed_equalp_(right, depth, &v2));

	return Result(ret, depth + v1 + (3 * v2));
}


/*
 *  vector
 */
static int sxfixed_vector_(addr pos, int depth, fixed *ret)
{
	size_t len;
	lenarray(pos, &len);
	return Result(ret, (fixed)len);
}


/*
 *  array
 */
static int sxfixed_array_(addr pos, int depth, fixed *ret)
{
	if (strarrayp(pos))
		return sxfixed_string_(pos, depth, ret);
	else
		return sxfixed_eq_(pos, depth, ret);
}

static int sxfixed_array_p_(addr pos, int depth, fixed *ret)
{
	if (strarrayp(pos))
		return sxfixed_string_p_(pos, depth, ret);
	else
		return sxfixed_eq_(pos, depth, ret);
}


/*
 *  Initialize
 */
static void SetSxhashEqual(enum LISPTYPE type, calltype_sxhash call)
{
	SxhashTableEqual[(size_t)type] = call;
}

static void SetSxhashEqualp(enum LISPTYPE type, calltype_sxhash call)
{
	SxhashTableEqualp[(size_t)type] = call;
}

_g void init_sxhash(void)
{
	int i;

	/*
	 *  equal
	 */
	for (i = 0; i < LISPTYPE_SIZE; i++)
		SxhashTableEqual[i] = sxfixed_eq_;
	SetSxhashEqual(LISPTYPE_CONS, sxfixed_cons_);
	SetSxhashEqual(LISPTYPE_VECTOR, sxfixed_vector_);
	SetSxhashEqual(LISPTYPE_ARRAY, sxfixed_array_);
	/* object */
	SetSxhashEqual(LISPTYPE_CHARACTER, sxfixed_character_);
	SetSxhashEqual(LISPSYSTEM_CHARACTER2, sxfixed_character2_);
	SetSxhashEqual(LISPTYPE_STRING, sxfixed_string_);
	SetSxhashEqual(LISPTYPE_FIXNUM, sxfixed_fixnum_);
	SetSxhashEqual(LISPTYPE_BIGNUM, sxfixed_bignum_);
	SetSxhashEqual(LISPTYPE_RATIO, sxfixed_ratio_);
	SetSxhashEqual(LISPTYPE_SINGLE_FLOAT, sxfixed_single_float_);
	SetSxhashEqual(LISPTYPE_DOUBLE_FLOAT, sxfixed_double_float_);
	SetSxhashEqual(LISPTYPE_LONG_FLOAT, sxfixed_long_float_);
	SetSxhashEqual(LISPTYPE_PATHNAME, sxfixed_pathname_);


	/*
	 *  equlap
	 */
	for (i = 0; i < LISPTYPE_SIZE; i++)
		SxhashTableEqualp[i] = SxhashTableEqual[i];
	SetSxhashEqualp(LISPTYPE_CONS, sxfixed_cons_p_);
	SetSxhashEqualp(LISPTYPE_ARRAY, sxfixed_array_p_);
	/* object */
	SetSxhashEqualp(LISPTYPE_CHARACTER, sxfixed_character_p_);
	SetSxhashEqualp(LISPSYSTEM_CHARACTER2, sxfixed_character2_p_);
	SetSxhashEqualp(LISPTYPE_STRING, sxfixed_string_p_);
	SetSxhashEqual(LISPTYPE_SINGLE_FLOAT, sxfixed_float_p_);
	SetSxhashEqual(LISPTYPE_DOUBLE_FLOAT, sxfixed_float_p_);
	SetSxhashEqual(LISPTYPE_LONG_FLOAT, sxfixed_float_p_);
}

