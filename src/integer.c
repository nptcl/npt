#include "bigdata.h"
#include "bignum.h"
#include "calltype.h"
#include "condition.h"
#include "integer.h"
#include "memory.h"
#include "stream.h"
#include "stream_string.h"

/*
 *  type
 */
int fixnump(addr pos)
{
	return GetType(pos) == LISPTYPE_FIXNUM;
}

int integerp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_FIXNUM || type == LISPTYPE_BIGNUM;
}


/*
 *  throw
 */
int integer_result_alloc(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			*ret = pos;
			return 0;

		case LISPTYPE_BIGNUM:
			return bignum_result_alloc(local, pos, ret);

		default:
			TypeError(pos, INTEGER);
			return 0;
	}
}

void integer_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_alloc(local, pos, ret);
			break;

		default:
			TypeError(pos, INTEGER);
			break;
	}
}

void integer_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	integer_throw_alloc(local, pos, ret);
}

void integer_throw_heap(addr pos, addr *ret)
{
	integer_throw_alloc(NULL, pos, ret);
}


/*
 *  compare
 */
int zerop_or_plusp_integer(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return zerop_or_plusp_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return zerop_or_plusp_bignum(pos);

		default:
			TypeError(pos, INTEGER);
			return 0;
	}
}

int plusp_integer(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return plusp_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return plusp_bignum(pos);

		default:
			TypeError(pos, INTEGER);
			return 0;
	}
}

int minusp_integer(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return minusp_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return minusp_bignum(pos);

		default:
			TypeError(pos, INTEGER);
			return 0;
	}
}

int zerop_integer(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return zerop_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return zerop_bignum(pos);

		default:
			TypeError(pos, INTEGER);
			return 0;
	}
}

static inline int equal_fixnum_integer(addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_ff_real(left, right);

		case LISPTYPE_BIGNUM:
			return equal_fb_real(left, right);

		default:
			TypeError(right, INTEGER);
			return 0;
	}
}

static inline int equal_bignum_integer(addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_bf_real(left, right);

		case LISPTYPE_BIGNUM:
			return equal_bb_real(left, right);

		default:
			TypeError(right, INTEGER);
			return 0;
	}
}

int equal_integer(addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return equal_fixnum_integer(left, right);

		case LISPTYPE_BIGNUM:
			return equal_bignum_integer(left, right);

		default:
			TypeError(left, INTEGER);
			return 0;
	}
}

static inline int compare_fixnum_integer(addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return compare_ff_real(left, right);

		case LISPTYPE_BIGNUM:
			return compare_fb_real(left, right);

		default:
			TypeError(right, INTEGER);
			return 0;
	}
}

static inline int compare_bignum_integer(addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return compare_bf_real(left, right);

		case LISPTYPE_BIGNUM:
			return compare_bb_real(left, right);

		default:
			TypeError(right, INTEGER);
			return 0;
	}
}

int compare_integer(addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return compare_fixnum_integer(left, right);

		case LISPTYPE_BIGNUM:
			return compare_bignum_integer(left, right);

		default:
			TypeError(left, INTEGER);
			return 0;
	}
}

int less_integer_clang(addr left, addr right)
{
	return less_integer(left, right);
}

int less_equal_integer_clang(addr left, addr right)
{
	return less_equal_integer(left, right);
}


/*
 *  calculate
 */
void oneplus_integer_common(LocalRoot local, addr value, addr *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, 1, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, 1, ret);
			break;

		default:
			TypeError(value, INTEGER);
			break;
	}
}

void oneminus_integer_common(LocalRoot local, addr value, addr *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, -1, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, -1, ret);
			break;

		default:
			TypeError(value, INTEGER);
			break;
	}
}

static inline void plus_fixnum_integer(LocalRoot local,
		addr left, addr right, addr *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_common(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_fb_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, INTEGER);
			break;
	}
}

static inline void plus_bignum_integer(LocalRoot local,
		addr left, addr right, addr *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bb_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, INTEGER);
			break;
	}
}

void plus_integer_common(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			plus_fixnum_integer(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bignum_integer(local, left, right, ret);
			break;

		default:
			TypeError(left, INTEGER);
			break;
	}
}

void minus_integer(LocalRoot local, addr left, addr right, addr *ret)
{
	*ret = Nil;
}

void multi_integer(LocalRoot local, addr left, addr right, addr *ret)
{
	*ret = Nil;
}

static int evenp_fixnum(addr left)
{
	fixnum value;
	GetFixnum(left, &value);
	return (value & 1) == 0;
}

int evenp_integer(addr left)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return evenp_fixnum(left);

		case LISPTYPE_BIGNUM:
			return evenp_bignum(left);

		default:
			TypeError(left, INTEGER);
			return 0;
	}
}


/*
 *  output
 */
void output_nosign_integer(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			output_nosign_fixnum(local, stream, RefFixnum(pos), base, upperp);
			break;

		case LISPTYPE_BIGNUM:
			output_nosign_bignum(local, stream, pos, base, upperp);
			break;

		default:
			TypeError(pos, INTEGER);
			break;
	}
}

void output_nosign_comma_integer(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp, size_t range, unicode comma)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			output_nosign_comma_fixnum(local, stream, RefFixnum(pos),
					base, upperp, range, comma);
			break;

		case LISPTYPE_BIGNUM:
			output_nosign_comma_bignum(local, stream, pos,
					base, upperp, range, comma);
			break;

		default:
			TypeError(pos, INTEGER);
			break;
	}
}

#define INTEGER_STREAM_SIZE		64
void string_nosign_comma_integer(LocalRoot local, addr *ret, addr pos,
		unsigned base, int upperp, size_t range, unicode comma)
{
	addr stream;

	open_output_string_stream(&stream, INTEGER_STREAM_SIZE);
	output_nosign_comma_integer(local, stream, pos, base, upperp, range, comma);
	string_stream_alloc(local, stream, ret);
	close_stream(stream);
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
static int getindex_fixnum(addr pos, size_t *ret)
{
	fixnum value;

	GetFixnum(pos, &value);
	if (value < 0) return 1;
	*ret = (size_t)value;

	return 0;
}
#else
static int getindex_fixnum(addr pos, size_t *ret)
{
	fixnum value;

	GetFixnum(pos, &value);
	if (value < 0) return 1;
	if (SIZE_MAX < value) return 1;
	*ret = (size_t)value;

	return 0;
}
#endif

#if (BIGNUM_FULL < SIZE_MAX)
static int getindex_bignum(addr pos, size_t *ret)
{
	int sign;
	size_t size, i, value;
	bigtype *data;

	GetSignBignum(pos, &sign);
	if (IsMinus(sign)) return 1;
	GetSizeBignum(pos, &size);
	if (SIZE_MAX / BIGNUM_FULL < size) return 1;

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
static int getindex_bignum(addr pos, size_t *ret)
{
	int sign;
	bigtype value;
	size_t size;

	GetSignBignum(pos, &sign);
	if (IsMinus(sign)) return 1;
	GetSizeBignum(pos, &size);
	if (size != 1) return 1;
	getfixed_bignum(pos, 0, &value);
	if (SIZE_MAX < value) return 1;
	*ret = (size_t)value;

	return 0;
}
#endif

int getindex_integer(addr pos, size_t *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return getindex_fixnum(pos, ret);

		case LISPTYPE_BIGNUM:
			return getindex_bignum(pos, ret);

		default:
			Abort("type error");
			return 1;
	}
}

void getindex_error(addr pos, size_t *ret)
{
	addr type;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			if (getindex_fixnum(pos, ret))
				goto error;
			break;

		case LISPTYPE_BIGNUM:
			if (getindex_bignum(pos, ret))
				goto error;
			break;

		default:
			goto error;
	}
	return;

error:
	GetCallType(&type, Index);
	type_error(pos, type);
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

