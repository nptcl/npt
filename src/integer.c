#include "bignum.h"
#include "bignum_equal.h"
#include "bignum_object.h"
#include "bignum_plus.h"
#include "condition.h"
#include "integer.h"
#include "type_table.h"

/*
 *  type
 */
int integerp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_FIXNUM || type == LISPTYPE_BIGNUM;
}

int minusp_integerp(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return minusp_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return minusp_bignum(pos);

		default:
			return 0;
	}
}


/*
 *  throw
 */
int integer_throw_alloc_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_alloc(local, pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_throw_alloc(local, pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}
}

int integer_throw_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return integer_throw_alloc_(local, pos, ret);
}

int integer_throw_heap_(addr pos, addr *ret)
{
	return integer_throw_alloc_(NULL, pos, ret);
}

int integer_result_alloc_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_result_alloc(local, pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_result_alloc(local, pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}
}

int integer_result_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_result_local(local, pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_result_local(local, pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}
}

int integer_result_heap_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_result_heap(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_result_heap(pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}
}

void fixnum_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	fixnum_alloc(local, ret, RefFixnum(pos));
}

void fixnum_copy_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	fixnum_copy_alloc(local, pos, ret);
}

void fixnum_copy_heap(LocalRoot local, addr pos, addr *ret)
{
	fixnum_copy_alloc(NULL, pos, ret);
}

int integer_copy_alloc_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_copy_alloc(local, pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_copy_alloc(local, ret, pos);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}
}

int integer_copy_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return integer_copy_alloc_(local, pos, ret);
}

int integer_copy_heap_(addr pos, addr *ret)
{
	return integer_copy_alloc_(NULL, pos, ret);
}


/*
 *  compare
 */
int getsign_integer_(addr pos, int *ret)
{
	fixed v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			castfixed_fixnum(pos, ret, &v);
			break;

		case LISPTYPE_BIGNUM:
			GetSignBignum(pos, ret);
			break;

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}

	return 0;
}

int zerop_or_plusp_integer_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, zerop_or_plusp_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, zerop_or_plusp_bignum(pos));

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

int plusp_integer_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, plusp_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, plusp_bignum(pos));

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

int minusp_integer_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, minusp_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, minusp_bignum(pos));

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

int zerop_integer_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, zerop_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, zerop_bignum(pos));

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

static inline int equal_fixnum_integer_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, equal_ff_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, equal_fb_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, INTEGER);
	}
}

static inline int equal_bignum_integer_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, equal_bf_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, equal_bb_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, INTEGER);
	}
}

int equal_integer_(addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return equal_fixnum_integer_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return equal_bignum_integer_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, INTEGER);
	}
}

int not_equal_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(equal_integer_(left, right, &check));
	return Result(ret, ! check);
}

static inline int compare_fixnum_integer_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, compare_ff_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, compare_fb_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, INTEGER);
	}
}

static inline int compare_bignum_integer_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, compare_bf_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, compare_bb_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, INTEGER);
	}
}

int compare_integer_(addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return compare_fixnum_integer_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return compare_bignum_integer_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, INTEGER);
	}
}

int less_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_integer_(left, right, &check));
	return Result(ret, check < 0);
}

int less_equal_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_integer_(left, right, &check));
	return Result(ret, check <= 0);
}

int greater_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_integer_(left, right, &check));
	return Result(ret, check > 0);
}

int greater_equal_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_integer_(left, right, &check));
	return Result(ret, check >= 0);
}

int zerop_integer_debug(addr pos)
{
	int check;

	Check(! integerp(pos), "left error");
	check = 0;
	Error(zerop_integer_(pos, &check));

	return check;
}

int plusp_integer_debug(addr pos)
{
	int check;

	Check(! integerp(pos), "left error");
	check = 0;
	Error(plusp_integer_(pos, &check));

	return check;
}

int minusp_integer_debug(addr pos)
{
	int check;

	Check(! integerp(pos), "left error");
	check = 0;
	Error(minusp_integer_(pos, &check));

	return check;
}

int less_integer_debug(addr left, addr right)
{
	int check;

	Check(! integerp(left), "left error");
	Check(! integerp(right), "right error");
	check = 0;
	Error(less_integer_(left, right, &check));

	return check;
}

int less_equal_integer_debug(addr left, addr right)
{
	int check;

	Check(! integerp(left), "left error");
	Check(! integerp(right), "right error");
	check = 0;
	Error(less_equal_integer_(left, right, &check));

	return check;
}

int sign_reverse_integer_common_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_common(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_common(pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}
}

static int evenp_fixnum(addr left)
{
	fixnum value;
	GetFixnum(left, &value);
	return (value & 1) == 0;
}

int evenp_integer_(addr left, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, evenp_fixnum(left));

		case LISPTYPE_BIGNUM:
			return Result(ret, evenp_bignum(left));

		default:
			*ret = 0;
			return TypeError_(left, INTEGER);
	}
}


/*
 *  size
 */
void make_index_integer_alloc(LocalRoot local, addr *ret, size_t value)
{
	size_t i, count;
	bigtype *data;

	if (value <= FIXNUM_MAX) {
		fixnum_alloc(local, ret, (fixnum)value);
	}
	else if (value <= BIGNUM_FULL) {
		bignum_value_alloc(local, ret, signplus_bignum, (fixed)value);
	}
	else {
		count = value / BIGNUM_FULL;
		bignum_alloc(local, ret, signplus_bignum, count);
		GetDataBignum(*ret, &data);
		for (i = 0; i < count; i++) {
			data[i] = value % BIGNUM_FULL;
			value /= BIGNUM_FULL;
		}
	}
}

void make_index_integer_local(LocalRoot local, addr *ret, size_t value)
{
	CheckLocal(local);
	make_index_integer_alloc(local, ret, value);
}

void make_index_integer_heap(addr *ret, size_t value)
{
	make_index_integer_alloc(NULL, ret, value);
}

void make_indexmax_alloc(LocalRoot local, addr *ret)
{
#if (SIZE_MAX <= FIXNUM_MAX)
	fixnum_alloc(local, ret, (fixnum)SIZE_MAX);
#elif (SIZE_MAX <= BIGNUM_FULL)
	bignum_value_alloc(local, ret, signplus_bignum, (fixed)SIZE_MAX);
#else
	size_t size, i, count;
	bigtype *data;

	size = SIZE_MAX;
	count = SIZE_MAX / BIGNUM_FULL;
	bignum_alloc(local, ret, signplus_bignum, count);
	GetDataBignum(*ret, &data);
	for (i = 0; i < count; i++) {
		data[i] = size % BIGNUM_FULL;
		size /= BIGNUM_FULL;
	}
#endif
}

#if (FIXNUM_MAX <= SIZE_MAX)
static int getindex_integer_fixnum(addr pos, size_t *ret)
{
	fixnum value;

	GetFixnum(pos, &value);
	if (value < 0)
		return 1;
	*ret = (size_t)value;

	return 0;
}
#else
static int getindex_integer_fixnum(addr pos, size_t *ret)
{
	fixnum value;

	GetFixnum(pos, &value);
	if (value < 0)
		return 1;
	if (SIZE_MAX < value)
		return 1;
	*ret = (size_t)value;

	return 0;
}
#endif

#if (BIGNUM_FULL < SIZE_MAX)
static int getindex_integer_bignum(addr pos, size_t *ret)
{
	int sign;
	size_t size, i, value;
	bigtype *data;

	GetSignBignum(pos, &sign);
	if (IsMinus(sign))
		return 1;
	GetSizeBignum(pos, &size);
	if (SIZE_MAX / BIGNUM_FULL < size)
		return 1;

	value = 0;
	GetDataBignum(pos, &data);
	for (i = 0; i < size; i++) {
		value <<= BIGNUM_FULLBIT;
		value |= data[i];
	}
	*ret = value;

	return 0;
}

#else
static int getindex_integer_bignum(addr pos, size_t *ret)
{
	int sign;
	bigtype value;
	size_t size;

	GetSignBignum(pos, &sign);
	if (IsMinus(sign))
		return 1;
	GetSizeBignum(pos, &size);
	if (size != 1)
		return 1;
	getfixed_bignum(pos, 0, &value);
	if (SIZE_MAX < value)
		return 1;
	*ret = (size_t)value;

	return 0;
}
#endif

int GetIndex_integer(addr pos, size_t *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return getindex_integer_fixnum(pos, ret);

		case LISPTYPE_BIGNUM:
			return getindex_integer_bignum(pos, ret);

		default:
			return 1;
	}
}

int getindex_integer_(addr pos, size_t *ret)
{
	addr type;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			if (getindex_integer_fixnum(pos, ret))
				break;
			return 0;

		case LISPTYPE_BIGNUM:
			if (getindex_integer_bignum(pos, ret))
				break;
			return 0;

		default:
			break;
	}

	/* error */
	GetTypeTable(&type, Index);
	return call_type_error_(NULL, pos, type);
}

addr reference_index_integer_alloc(LocalRoot local, size_t value)
{
	addr pos;
	make_index_integer_alloc(local, &pos, value);
	return pos;
}

addr reference_index_integer_local(LocalRoot local, size_t value)
{
	Check(local == NULL, "local error");
	return reference_index_integer_alloc(local, value);
}

addr reference_index_integer_heap(size_t value)
{
	return reference_index_integer_alloc(NULL, value);
}

static int cast_bigtype_index(bigtype value, size_t *ret)
{
#if (BIGNUM_FULL < SIZE_MAX)
	*ret = (size_t)value;
	return 0;
#else
	if (SIZE_MAX < value) {
		return 1;
	}
	else {
		*ret = (size_t)value;
		return 0;
	}
#endif
}

static int cast_fixnum_index_(addr pos, int *sign, size_t *value, int *ret)
{
	fixed body;

	CheckType(pos, LISPTYPE_FIXNUM);
	castfixed_fixnum(pos, sign, &body);
	*ret = cast_bigtype_index(body, value);
	return 0;
}

static int cast_bignum_index_(addr pos, int *sign, size_t *value, int *ret)
{
	bigtype *data;
	size_t size;

	CheckType(pos, LISPTYPE_BIGNUM);
	GetSizeBignum(pos, &size);
	GetSignBignum(pos, sign);
	if (size != 1) {
		*ret = 1;
		return 0;
	}
	GetDataBignum(pos, &data);
	*ret = cast_bigtype_index(data[0], value);
	return 0;
}

int getindex_sign_integer_(addr pos, int *sign, size_t *value, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return cast_fixnum_index_(pos, sign, value, ret);

		case LISPTYPE_BIGNUM:
			return cast_bignum_index_(pos, sign, value, ret);

		default:
			*sign = 0;
			*value = 0;
			*ret = 1;
			return TypeError_(pos, INTEGER);
	}
}

int GetIndex_fixnum(addr pos, size_t *ret)
{
	fixnum value;

	if (! fixnump(pos))
		return 1;
	GetFixnum(pos, &value);
	if (value < 0)
		return 1;
	*ret = (size_t)value;

	return 0;
}

int getindex_fixnum_(addr pos, size_t *ret)
{
	if (GetIndex_fixnum(pos, ret)) {
		*ret = 0;
		return TypeError_(pos, FIXNUM);
	}

	return 0;
}

int fixnum_index_heap_(addr *ret, size_t value)
{
	if ((size_t)FIXNUM_MAX <= value)
		return fmte_("Too large value ~S.", intsizeh(value), NULL);
	fixnum_heap(ret, (fixnum)value);

	return 0;
}

int GetByte_integer(addr pos, byte *ret)
{
	fixnum v;

	if (GetFixnum_signed(pos, &v))
		goto error;
	if (! IsByteSign(v))
		goto error;
	*ret = (byte)v;
	return 0;

error:
	*ret = 0;
	return 1;
}

int getunicode_integer_(addr pos, unicode *ret)
{
	size_t value;

	if (GetIndex_integer(pos, &value)) {
		*ret = 0;
		goto error;
	}

#ifdef LISP_64BIT
	if (0xFFFFFFFFULL < value)
		goto error;
#endif
	return Result(ret, (unicode)value);

error:
	return fmte_("Invalid character code ~A.", pos, NULL);
}


/*
 *  standard type
 */
void int8_integer_alloc(LocalRoot local, addr *ret, int8_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

void int16_integer_alloc(LocalRoot local, addr *ret, int16_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

void int32_integer_alloc(LocalRoot local, addr *ret, int32_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

void uint8_integer_alloc(LocalRoot local, addr *ret, uint8_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

void uint16_integer_alloc(LocalRoot local, addr *ret, uint16_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

#ifdef LISP_64BIT
void uint32_integer_alloc(LocalRoot local, addr *ret, uint32_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}
#else
void uint32_integer_alloc(LocalRoot local, addr *ret, uint32_t value)
{
	bignum_value_alloc(local, ret, signplus_bignum, (fixed)value);
}
#endif

#ifdef LISP_64BIT
void int64_integer_alloc(LocalRoot local, addr *ret, int64_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

void uint64_integer_alloc(LocalRoot local, addr *ret, uint64_t value)
{
	bignum_value_alloc(local, ret, signplus_bignum, (fixed)value);
}
#endif

