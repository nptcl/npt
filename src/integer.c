#include "bigcons.h"
#include "bigdata.h"
#include "bignum.h"
#include "calltype.h"
#include "character.h"
#include "condition.h"
#include "integer.h"
#include "memory.h"
#include "stream.h"
#include "stream_string.h"
#include "strtype.h"
#include "token.h"

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

void integer_result_alloc(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_result_alloc(local, pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_result_alloc(local, pos, ret);
			break;

		default:
			TypeError(pos, INTEGER);
			break;
	}
}

void integer_result_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_result_local(local, pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_result_local(local, pos, ret);
			break;

		default:
			TypeError(pos, INTEGER);
			break;
	}
}

void integer_result_heap(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_result_heap(pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_result_heap(pos, ret);
			break;

		default:
			TypeError(pos, INTEGER);
			break;
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

void integer_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_copy_alloc(local, pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_copy_alloc(local, ret, pos);
			break;

		default:
			TypeError(pos, INTEGER);
			break;
	}
}

void integer_copy_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	integer_copy_alloc(local, pos, ret);
}

void integer_copy_heap(addr pos, addr *ret)
{
	integer_copy_alloc(NULL, pos, ret);
}


/*
 *  compare
 */
void getsign_integer(addr pos, int *ret)
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
			TypeError(pos, INTEGER);
			*ret = 0;
			return;
	}
}

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
	CheckType(left, LISPTYPE_FIXNUM);
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
	CheckType(left, LISPTYPE_BIGNUM);
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
	CheckType(left, LISPTYPE_FIXNUM);
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
	CheckType(left, LISPTYPE_BIGNUM);
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

void sign_reverse_integer_common(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_common(pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_common(pos, ret);
			break;

		default:
			TypeError(pos, INTEGER);
			break;
	}
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
	addr type;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return getindex_fixnum(pos, ret);

		case LISPTYPE_BIGNUM:
			return getindex_bignum(pos, ret);

		default:
			GetCallType(&type, Index);
			type_error(pos, type);
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

static int cast_fixnum_index(addr pos, int *sign, size_t *ret)
{
	fixed value;

	CheckType(pos, LISPTYPE_FIXNUM);
	castfixed_fixnum(pos, sign, &value);
	return cast_bigtype_index(value, ret);
}

static int cast_bignum_index(addr pos, int *sign, size_t *ret)
{
	bigtype *data;
	size_t size;

	CheckType(pos, LISPTYPE_BIGNUM);
	GetSizeBignum(pos, &size);
	GetSignBignum(pos, sign);
	if (size != 1) return 1;
	GetDataBignum(pos, &data);
	return cast_bigtype_index(data[0], ret);
}

int getindex_sign_integer(addr pos, int *sign, size_t *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return cast_fixnum_index(pos, sign, ret);

		case LISPTYPE_BIGNUM:
			return cast_bignum_index(pos, sign, ret);

		default:
			TypeError(pos, INTEGER);
			*sign = 0;
			*ret = 0;
			return 1;
	}
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


/*
 *  oneplus
 */
void oneplus_integer_common(LocalRoot local, addr value, addr *ret)
{
	CheckLocal(local);
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
	CheckLocal(local);
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


/*
 *  plus
 */
void plus_fi_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_bignum_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_fb_bignum_local(local, left, right, ret);
			break;

		default:
			TypeError(right, INTEGER);
			break;
	}
}

void plus_fi_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_fb_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, INTEGER);
			break;
	}
}

void plus_fi_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
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

void plus_bi_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_bignum_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bb_bignum_local(local, left, right, ret);
			break;

		default:
			TypeError(right, INTEGER);
			break;
	}
}

void plus_bi_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bb_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, INTEGER);
			break;
	}
}

void plus_bi_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
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

void plus_ii_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			plus_fi_bignum_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bi_bignum_local(local, left, right, ret);
			break;

		default:
			TypeError(left, INTEGER);
			break;
	}
}

void plus_ii_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			plus_fi_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bi_real_local(local, left, right, ret);
			break;

		default:
			TypeError(left, INTEGER);
			break;
	}
}

void plus_ii_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			plus_fi_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bi_real_common(local, left, right, ret);
			break;

		default:
			TypeError(left, INTEGER);
			break;
	}
}


/*
 *  minus
 */
static void minus_fi_integer_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_common(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_fb_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, INTEGER);
			*ret = 0;
			return;
	}
}

static void minus_bi_integer_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_bf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_bb_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, INTEGER);
			*ret = 0;
			return;
	}
}

void minus_ii_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fi_integer_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_bi_integer_common(local, left, right, ret);
			break;

		default:
			TypeError(left, INTEGER);
			*ret = 0;
			return;
	}
}


/*
 *  multi
 */
static void multi_fi_integer_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_ff_real_common(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_fb_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, INTEGER);
			*ret = 0;
			return;
	}
}

static void multi_bi_integer_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_bf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_bb_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, INTEGER);
			*ret = 0;
			return;
	}
}

void multi_ii_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			multi_fi_integer_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_bi_integer_common(local, left, right, ret);
			break;

		default:
			TypeError(left, INTEGER);
			*ret = 0;
			return;
	}
}


/*
 *  ash
 */
void ash_bignum_common(LocalRoot local, addr pos, int sign2, size_t size, addr *ret)
{
	int sign1;

	GetSignBignum(pos, &sign1);
	if (IsPlus(sign2))
		shiftup_bigdata_alloc(local, &pos, pos, size);
	else
		shiftdown_bigdata_alloc(local, &pos, pos, size);
	SetSignBignum(pos, sign1);
	bignum_result_heap(pos, ret);
}

void ash_integer_common(LocalRoot local, addr pos, addr count, addr *ret)
{
	int sign2;
	size_t size;
	LocalStack stack;

	if (getindex_sign_integer(count, &sign2, &size)) {
		fmte("Too large shift value ~S.", count, NULL);
		*ret = 0;
		return;
	}

	push_local(local, &stack);
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			bignum_fixnum_local(local, &pos, pos);
			/* FALLTHRU */
		case LISPTYPE_BIGNUM:
			ash_bignum_common(local, pos, sign2, size, ret);
			break;

		default:
			TypeError(pos, INTEGER);
			*ret = 0;
			break;
	}
	rollback_local(local, stack);
}


/*
 *  integer-length
 */
static size_t integer_length_bigtype(bigtype value)
{
	size_t size;

	for (size = 0; value; size++)
		value >>= 1;

	return size;
}

static size_t inverse_length_bigtype(bigtype value)
{
	if (value <= 1)
		return 0;
	else
		return integer_length_bigtype(value - 1);
}

static void integer_length_fixnum(addr pos, addr *ret)
{
	int sign;
	bigtype value;
	size_t size;

	castfixed_fixnum(pos, &sign, &value);
	if (IsPlus(sign))
		size = integer_length_bigtype(value);
	else
		size = inverse_length_bigtype(value);
	make_index_integer_alloc(NULL, ret, size);
}

static size_t integer_length_bigdata(addr pos)
{
	size_t size, size1, size2;
	bigtype *data;

	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	if (size == 1 && data[0] == 0)
		return 0;
	size--;
	size1 = size * BIGNUM_FULLBIT;
	size2 = integer_length_bigtype(data[size]);

	return size1 + size2;
}

static int check_length_bignum(addr pos)
{
	int sign;
	bigtype *data;
	size_t size, i;

	GetSignBignum(pos, &sign);
	if (IsPlus(sign))
		return 0;
	GetSizeBignum(pos, &size);
	size--;
	GetDataBignum(pos, &data);
	for (i = 0; i < size; i++) {
		if (data[i] != 0)
			return 0;
	}

	return data[i] == 1;
}

static void integer_length_bignum(addr pos, addr *ret)
{
	size_t size;

	size = integer_length_bigdata(pos);
	if (check_length_bignum(pos))
		size--;
	make_index_integer_alloc(NULL, ret, size);
}

void integer_length_common(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			integer_length_fixnum(pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			integer_length_bignum(pos, ret);
			break;

		default:
			TypeError(pos, INTEGER);
			*ret = 0;
			return;
	}
}


/*
 *  parse-integer
 */
void parse_integer_common(LocalRoot local,
		addr string, size_t start, size_t end, unsigned radix, int junk,
		addr *ret, addr *position)
{
	int mode, sign;
	unsigned v;
	unicode c;
	addr cons;
	size_t i;
	LocalStack stack;

	push_local(local, &stack);
	bigcons_local(local, &cons);
	mode = 0;
	sign = SignPlus;
	for (i = start; i < end; i++) {
		string_getc(string, i, &c);
		/* white space */
		if (mode == 0) {
			if (isSpaceUnicode(c))
				continue;
			if (c == '+') {
				sign = SignPlus;
				mode = 1;
				continue;
			}
			if (c == '-') {
				sign = SignMinus;
				mode = 1;
				continue;
			}
			if (getvalue_digit(radix, c, &v))
				goto error;
			mode = 1;
		}
		/* digit */
		if (mode == 1) {
			if (! getvalue_digit(radix, c, &v)) {
				push_bigcons(local, cons, radix, v);
				continue;
			}
			if (! isSpaceUnicode(c))
				goto error;
			if (junk)
				goto error; /* Don't read white-space (junk-allowed). */
			continue;
		}
		/* white-space */
		if (! isSpaceUnicode(c))
			goto error;
	}
	/* success */
	integer_cons_heap(ret, sign, cons);
	make_index_integer_heap(position, i);
	rollback_local(local, stack);
	return;

error:
	if (junk) {
		if (bigcons_empty_p(cons))
			*ret = Nil;
		else
			integer_cons_heap(ret, sign, cons);
		make_index_integer_heap(position, i);
		rollback_local(local, stack);
	}
	else {
		rollback_local(local, stack);
		fmte("Invalid string ~A.", character_heapr(c), NULL);
		*ret = *position = 0;
	}
}

