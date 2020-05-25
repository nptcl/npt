#include "bigdata.h"
#include "bignum.h"
#include "build.h"
#include "character.h"
#include "condition.h"
#include "heap.h"
#include "heap_memory.h"
#include "memory.h"
#include "object.h"
#include "pathname.h"
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
#define FIXNUM_BODY(x)  ((x) & FIXNUM_MASK_BODY)

typedef fixed (*calltype_sxhash)(addr pos, int depth);
static calltype_sxhash SxhashTableEqual[LISPTYPE_SIZE];
static calltype_sxhash SxhashTableEqualp[LISPTYPE_SIZE];
static fixed sxfixed_equal(addr pos, int depth);
static fixed sxfixed_equalp(addr pos, int depth);


/*
 *  eq
 */
#define sxhash_diffshift(x,y,z) ((((uintptr_t)(x)) - ((uintptr_t)(y))) >> (z))
#define sxhash_diffheap(x,s) ((fixed)sxhash_diffshift(heap_alloc, (x), (s)))
static fixed sxfixed_eq(addr pos, int depth)
{
#ifdef LISP_ARCH_64BIT
	return sxhash_diffheap(pos, 3);
#else
	return sxhash_diffheap(pos, 2);
#endif
}


/*
 *  character
 */
static fixed sxfixed_character(addr pos, int depth)
{
	unicode u;
	GetCharacter(pos, &u);
	return (fixed)u;
}

static fixed sxfixed_character_p(addr pos, int depth)
{
	unicode u;
	GetCharacter(pos, &u);
	return (fixed)toUpperUnicode(u);
}


/*
 *  character2
 */
static fixed sxfixed_character2(addr pos, int depth)
{
	fixed a, b;
	a = (fixed)refcharacter2a(pos);
	b = (fixed)refcharacter2b(pos);
	return a + b;
}

static fixed sxfixed_character2_p(addr pos, int depth)
{
	unicode a, b;
	fixed c, d;
	getcharacter2a(pos, &a);
	getcharacter2b(pos, &b);
	c = (fixed)toUpperUnicode(a);
	d = (fixed)toUpperUnicode(b);
	return c + d;
}


/*
 *  binary
 */
static fixed sxfixed_binary(const byte *u, size_t size)
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

	return value;
}

static fixnum sxhash_binary_equal(const void *pos, size_t size)
{
	return (fixnum)FIXNUM_BODY(sxfixed_binary((const byte *)pos, size));
}

_g fixnum sxhash_char_equal(const char *pos)
{
	return sxhash_binary_equal((const void *)pos, strlen(pos));
}

static fixed fixed_binary_p(const byte *u, size_t size)
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

	return value;
}

static fixnum sxhash_binary_equalp(const void *pos, size_t size)
{
	return (fixnum)FIXNUM_BODY(fixed_binary_p((const byte *)pos, size));
}

_g fixnum sxhash_char_equalp(const char *pos)
{
	return sxhash_binary_equalp((const void *)pos, strlen(pos));
}


/*
 *  string
 */
static fixed sxfixed_string(addr pos, int depth)
{
	int m;
	size_t i, len;
	fixed p[FixedSize], value;
	unicode u;

	cleartype(p);
	string_length(pos, &len);
	for (i = 0; i < len; i++) {
		string_getc(pos, i, &u);
		m = i % FixedSize;
		p[m] += u;
	}
	value = (fixed)len;
	for (i = 0; i < FixedSize; i++)
		value += p[i] << (i * 8);

	return value;
}

static fixed sxfixed_string_p(addr pos, int depth)
{
	int m;
	size_t i, len;
	fixed p[FixedSize], value;
	unicode u;

	cleartype(p);
	string_length(pos, &len);
	for (i = 0; i < len; i++) {
		string_getc(pos, i, &u);
		m = i % FixedSize;
		p[m] += toupperunicode(u);
	}
	value = (fixed)len;
	for (i = 0; i < FixedSize; i++)
		value += p[i] << (i * 8);

	return value;
}


/*
 *  fixnum
 */
static fixed sxfixed_fixnum(addr pos, int depth)
{
	fixnum value;
	GetFixnum(pos, &value);
	return (fixed)value;
}


/*
 *  bignum
 */
static fixed sxfixed_bignum(addr pos, int depth)
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

	return value;
}


/*
 *  ratio
 */
static fixed sxfixed_ratio(addr pos, int depth)
{
	int sign;
	addr numer, denom;
	fixed value;

	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	value = (fixed)sign;
	value += sxfixed_bignum(numer, depth);
	value += sxfixed_bignum(denom, depth);

	return value;
}


/*
 *  float
 */
static fixed sxfixed_single_float(addr pos, int depth)
{
	single_float value;
	GetSingleFloat(pos, &value);
	return sxfixed_binary((const byte *)&value, sizeoft(value));
}

static fixed sxfixed_double_float(addr pos, int depth)
{
	double_float value;
	GetDoubleFloat(pos, &value);
	return sxfixed_binary((const byte *)&value, sizeoft(value));
}

static fixed sxfixed_long_float(addr pos, int depth)
{
	long_float value;
	GetLongFloat(pos, &value);
	return sxfixed_binary((const byte *)&value, sizeoft(value));
}

static fixed sxfixed_pathname(addr pos, int depth)
{
	int i;
	addr child;
	fixed result;

	result = 0;
	depth--;
	for (i = 0; i < PATHNAME_INDEX_SIZE; i++) {
		GetArrayA2(pos, i, &child);
		result += sxfixed_equal(child, depth);
	}

	return result;
}

static fixed sxfixed_float_p(addr pos, int depth)
{
	LocalRoot local;

	local = Local_Thread;
	rationalize_common(local, pos, &pos);
	return sxfixed_equalp(pos, depth);
}


/*
 *  Global function
 */
static fixed sxfixed_equal(addr pos, int depth)
{
	if (pos == Nil) return 0;
	if (pos == T) return 1;
	return (SxhashTableEqual[(size_t)GetType(pos)])(pos, depth);
}

static fixed sxfixed_equalp(addr pos, int depth)
{
	if (pos == Nil) return 0;
	if (pos == T) return 1;
	return (SxhashTableEqualp[(size_t)GetType(pos)])(pos, depth);
}

_g fixnum sxhash_equal_depth(addr pos, int depth)
{
	return (fixnum)FIXNUM_BODY(sxfixed_equal(pos, depth));
}

_g fixnum sxhash_equal(addr pos)
{
	return sxhash_equal_depth(pos, -1);
}

_g fixnum sxhash_equalp_depth(addr pos, int depth)
{
	return (fixnum)FIXNUM_BODY(sxfixed_equalp(pos, depth));
}

_g fixnum sxhash_equalp(addr pos)
{
	return sxhash_equalp_depth(pos, -1);
}

_g fixnum sxhash_eq(addr pos)
{
	return (fixnum)FIXNUM_BODY(sxfixed_eq(pos, -1));
}

_g fixnum sxhash_unicode_equalp(unicode pos)
{
	return (fixnum)FIXNUM_BODY((fixed)toUpperUnicode(pos));
}

_g fixnum sxhash_unicode_equal(unicode pos)
{
	return (fixnum)FIXNUM_BODY((fixed)pos);
}

_g fixnum sxhash_character2_equalp(unicode a, unicode b)
{
	fixed c, d;
	c = (fixed)toUpperUnicode(a);
	d = (fixed)toUpperUnicode(b);
	return (fixnum)FIXNUM_BODY(c + d);
}

_g fixnum sxhash_character2_equal(unicode a, unicode b)
{
	fixed c, d;
	c = (fixed)a;
	d = (fixed)b;
	return (fixnum)FIXNUM_BODY(c + d);
}


/*
 *  cons
 */
static fixed sxfixed_cons(addr pos, int depth)
{
	addr right;
	fixed v1, v2;

	if (depth == 0) return 0;
	GetCons(pos, &pos, &right);
	depth--;
	v1 = sxfixed_equal(pos, depth);
	v2 = sxfixed_equal(right, depth);

	return depth + v1 + (3 * v2);
}

static fixed sxfixed_cons_p(addr pos, int depth)
{
	addr right;
	fixed v1, v2;

	if (depth == 0) return 0;
	GetCons(pos, &pos, &right);
	depth--;
	v1 = sxfixed_equalp(pos, depth);
	v2 = sxfixed_equalp(right, depth);

	return depth + v1 + (3 * v2);
}


/*
 *  vector
 */
static fixed sxfixed_vector(addr pos, int depth)
{
	size_t len;
	lenarray(pos, &len);
	return (fixed)len;
}


/*
 *  array
 */
static fixed sxfixed_array(addr pos, int depth)
{
	if (strarrayp(pos))
		return sxfixed_string(pos, depth);
	else
		return sxfixed_eq(pos, depth);
}

static fixed sxfixed_array_p(addr pos, int depth)
{
	if (strarrayp(pos))
		return sxfixed_string_p(pos, depth);
	else
		return sxfixed_eq(pos, depth);
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
		SxhashTableEqual[i] = sxfixed_eq;
	SetSxhashEqual(LISPTYPE_CONS, sxfixed_cons);
	SetSxhashEqual(LISPTYPE_VECTOR, sxfixed_vector);
	SetSxhashEqual(LISPTYPE_ARRAY, sxfixed_array);
	/* object */
	SetSxhashEqual(LISPTYPE_CHARACTER, sxfixed_character);
	SetSxhashEqual(LISPSYSTEM_CHARACTER2, sxfixed_character2);
	SetSxhashEqual(LISPTYPE_STRING, sxfixed_string);
	SetSxhashEqual(LISPTYPE_FIXNUM, sxfixed_fixnum);
	SetSxhashEqual(LISPTYPE_BIGNUM, sxfixed_bignum);
	SetSxhashEqual(LISPTYPE_RATIO, sxfixed_ratio);
	SetSxhashEqual(LISPTYPE_SINGLE_FLOAT, sxfixed_single_float);
	SetSxhashEqual(LISPTYPE_DOUBLE_FLOAT, sxfixed_double_float);
	SetSxhashEqual(LISPTYPE_LONG_FLOAT, sxfixed_long_float);
	SetSxhashEqual(LISPTYPE_PATHNAME, sxfixed_pathname);


	/*
	 *  equlap
	 */
	for (i = 0; i < LISPTYPE_SIZE; i++)
		SxhashTableEqualp[i] = SxhashTableEqual[i];
	SetSxhashEqualp(LISPTYPE_CONS, sxfixed_cons_p);
	SetSxhashEqualp(LISPTYPE_ARRAY, sxfixed_array_p);
	/* object */
	SetSxhashEqualp(LISPTYPE_CHARACTER, sxfixed_character_p);
	SetSxhashEqualp(LISPSYSTEM_CHARACTER2, sxfixed_character2_p);
	SetSxhashEqualp(LISPTYPE_STRING, sxfixed_string_p);
	SetSxhashEqual(LISPTYPE_SINGLE_FLOAT, sxfixed_float_p);
	SetSxhashEqual(LISPTYPE_DOUBLE_FLOAT, sxfixed_float_p);
	SetSxhashEqual(LISPTYPE_LONG_FLOAT, sxfixed_float_p);
}

