#include "build.h"
#include "character.h"
#include "condition.h"
#include "heap.h"
#include "memory.h"
#include "object.h"
#include "pathname.h"
#include "typedef.h"
#include "unicode.h"
#include "strtype.h"

/*
 *  Declarations
 */
#define FixedSize (sizeof(fixed))

typedef fixed (*call_sxhash)(addr pos, int depth);
static call_sxhash TableEqual[LISPTYPE_SIZE];
static call_sxhash TableEqualp[LISPTYPE_SIZE];
static fixed fixed_equal(addr pos, int depth);
static fixed fixed_equalp(addr pos, int depth);


/*
 *  sxhash
 */
#define diffshift(x,y,z) ((((uintptr_t)(x)) - ((uintptr_t)(y))) >> (z))
#define diffheap(x,s) ((fixed)diffshift(heap_alloc, (x), (s)))
static fixed fixed_pointer(addr pos, int depth)
{
#ifdef LISP_ARCH_64BIT
	return diffheap(pos, 3);
#else
	return diffheap(pos, 2);
#endif
}

static fixed fixed_equal_cons(addr pos, int depth)
{
	addr right;
	fixed v1, v2;

	if (depth == 0) return 0;
	GetCons(pos, &pos, &right);
	depth--;
	v1 = fixed_equal(pos, depth);
	v2 = fixed_equal(right, depth);

	return depth + v1 + (3 * v2);
}

static fixed fixed_equalp_cons(addr pos, int depth)
{
	addr right;
	fixed v1, v2;

	if (depth == 0) return 0;
	GetCons(pos, &pos, &right);
	depth--;
	v1 = fixed_equalp(pos, depth);
	v2 = fixed_equalp(right, depth);

	return depth + v1 + (3 * v2);
}

static fixed fixed_vector(addr pos, int depth)
{
	size_t len;
	lenarray(pos, &len);
	return (fixed)len;
}

static fixed fixed_character(addr pos, int depth)
{
	unicode u;
	GetCharacter(pos, &u);
	return (fixed)u;
}

static fixed fixed_character2(addr pos, int depth)
{
	fixed a, b;
	a = (fixed)refcharacter2a(pos);
	b = (fixed)refcharacter2b(pos);
	return a + b;
}

static fixed fixed_characterp(addr pos, int depth)
{
	unicode u;
	GetCharacter(pos, &u);
	return (fixed)toUpperUnicode(u);
}

static fixed fixed_character2p(addr pos, int depth)
{
	unicode a, b;
	fixed c, d;
	getcharacter2a(pos, &a);
	getcharacter2b(pos, &b);
	c = (fixed)toUpperUnicode(a);
	d = (fixed)toUpperUnicode(b);
	return c + d;
}

static fixed fixed_binary(const byte *u, size_t size)
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

static fixed fixed_string(addr pos, int depth)
{
	int m;
	size_t i, len;
	fixed p[FixedSize], value;
	const unicode *u;

	cleartype(p);
	string_posbodylen(pos, &u, &len);
	for (i = 0; i < len; i++) {
		m = i % FixedSize;
		p[m] += u[i];
	}
	value = (fixed)len;
	for (i = 0; i < FixedSize; i++)
		value += p[i] << (i * 8);

	return value;
}

static fixed fixed_array(addr pos, int depth)
{
	if (! strarrayp(pos))
		fmte("TODO: type error", NULL);
	return fixed_string(pos, depth);
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

static fixed fixed_string_p(addr pos, int depth)
{
	int m;
	size_t i, len;
	fixed p[FixedSize], value;
	const unicode *u;

	cleartype(p);
	string_posbodylen(pos, &u, &len);
	for (i = 0; i < len; i++) {
		m = i % FixedSize;
		p[m] += toupperunicode(u[i]);
	}
	value = (fixed)len;
	for (i = 0; i < FixedSize; i++)
		value += p[i] << (i * 8);

	return value;
}

static fixed fixed_array_p(addr pos, int depth)
{
	if (! strarrayp(pos))
		fmte("TODO: type error", NULL);
	return fixed_string_p(pos, depth);
}

static fixed fixed_fixnum(addr pos, int depth)
{
	fixnum value;
	GetFixnum(pos, &value);
	return (fixed)value;
}

static fixed fixed_float_single(addr pos, int depth)
{
	union {
		fixed result;
		single_float value;
	} u;

	cleartype(u);
	GetSingleFloat(pos, &u.value);
	return u.result;
}

static fixed fixed_float_double(addr pos, int depth)
{
#ifdef LISP_64BIT
	union {
		fixed result;
		double_float value;
	} u;
	cleartype(u);
	GetDoubleFloat(pos, &u.value);
	return u.result;
#else
	union {
		fixed result[2];
		double_float value;
	} u;
	cleartype(u);
	GetDoubleFloat(pos, &u.value);
	return u.result[0] + u.result[1];
#endif
}

static fixed fixed_pathname(addr pos, int depth)
{
	int i;
	addr child;
	fixed result;

	result = 0;
	depth--;
	for (i = 0; i < PATHNAME_INDEX_SIZE; i++) {
		GetArrayA2(pos, i, &child);
		result += fixed_equal(child, depth);
	}

	return result;
}


/*
 *  Global function
 */
static fixed fixed_equal(addr pos, int depth)
{
	if (pos == Nil) return 0;
	if (pos == T) return 1;
	return (TableEqual[(size_t)GetType(pos)])(pos, depth);
}

static fixed fixed_equalp(addr pos, int depth)
{
	if (pos == Nil) return 0;
	if (pos == T) return 1;
	return (TableEqualp[(size_t)GetType(pos)])(pos, depth);
}

#define FIXNUM_BIT1M (LISP_INTEGER_BIT - 1ULL)
#define FIXNUM_MASK_SIGN (1ULL << FIXNUM_BIT1M)
#define FIXNUM_MASK_BODY (FIXNUM_MASK_SIGN - 1ULL)
#define FIXNUM_BODY(x)  ((x) & FIXNUM_MASK_BODY)
fixnum sxhash_equal_depth(addr pos, int depth)
{
	return (fixnum)FIXNUM_BODY(fixed_equal(pos, depth));
}

fixnum sxhash_equal(addr pos)
{
	return sxhash_equal_depth(pos, -1);
}

fixnum sxhash_equalp_depth(addr pos, int depth)
{
	return (fixnum)FIXNUM_BODY(fixed_equalp(pos, depth));
}

fixnum sxhash_equalp(addr pos)
{
	return sxhash_equalp_depth(pos, -1);
}

fixnum sxhash_pointer(addr pos)
{
	return (fixnum)FIXNUM_BODY(fixed_pointer(pos, -1));
}

fixnum sxhash_binary_equalp(const void *pos, size_t size)
{
	return (fixnum)FIXNUM_BODY(fixed_binary_p((const byte *)pos, size));
}

fixnum sxhash_binary_equal(const void *pos, size_t size)
{
	return (fixnum)FIXNUM_BODY(fixed_binary((const byte *)pos, size));
}

fixnum sxhash_char_equalp(const char *pos)
{
	return sxhash_binary_equalp((const void *)pos, strlen(pos));
}

fixnum sxhash_char_equal(const char *pos)
{
	return sxhash_binary_equal((const void *)pos, strlen(pos));
}

fixnum sxhash_unicode_equalp(unicode pos)
{
	return (fixnum)FIXNUM_BODY((fixed)toUpperUnicode(pos));
}

fixnum sxhash_unicode_equal(unicode pos)
{
	return (fixnum)FIXNUM_BODY((fixed)pos);
}

fixnum sxhash_character2_equalp(unicode a, unicode b)
{
	fixed c, d;
	c = (fixed)toUpperUnicode(a);
	d = (fixed)toUpperUnicode(b);
	return (fixnum)FIXNUM_BODY(c + d);
}

fixnum sxhash_character2_equal(unicode a, unicode b)
{
	fixed c, d;
	c = (fixed)a;
	d = (fixed)b;
	return (fixnum)FIXNUM_BODY(c + d);
}


/*
 *  Initialize
 */
static void SetTable(enum LISPTYPE type, call_sxhash call)
{
	TableEqual[(size_t)type] = call;
}

static void SetTablep(enum LISPTYPE type, call_sxhash call)
{
	TableEqualp[(size_t)type] = call;
}

void init_sxhash(void)
{
	int i;

	/*
	 *  equal
	 */
	for (i = 0; i < LISPTYPE_SIZE; i++)
		TableEqual[i] = fixed_pointer;
	SetTable(LISPTYPE_CONS, fixed_equal_cons);
	SetTable(LISPTYPE_VECTOR, fixed_vector);
	SetTable(LISPTYPE_CHARACTER, fixed_character);
	SetTable(LISPSYSTEM_CHARACTER2, fixed_character2);
	SetTable(LISPTYPE_STRING, fixed_string);
	SetTable(LISPTYPE_ARRAY, fixed_array);
	SetTable(LISPTYPE_FIXNUM, fixed_fixnum);
	/* SetTable(LISPTYPE_BIGNUM, fixed_bignum); */
	SetTable(LISPTYPE_SINGLE_FLOAT, fixed_float_single);
	SetTable(LISPTYPE_DOUBLE_FLOAT, fixed_float_double);
	SetTable(LISPTYPE_PATHNAME, fixed_pathname);


	/*
	 *  equlap
	 */
	for (i = 0; i < LISPTYPE_SIZE; i++)
		TableEqualp[i] = TableEqual[i];
	SetTable(LISPTYPE_CONS, fixed_equalp_cons);
	SetTablep(LISPTYPE_CHARACTER, fixed_characterp);
	SetTablep(LISPSYSTEM_CHARACTER2, fixed_character2p);
	SetTablep(LISPTYPE_STRING, fixed_string_p);
	SetTablep(LISPTYPE_ARRAY, fixed_array_p);
}

