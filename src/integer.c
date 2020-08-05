#include "bignum.h"
#include "bignum_cons.h"
#include "bignum_data.h"
#include "bignum_equal.h"
#include "bignum_multi.h"
#include "bignum_object.h"
#include "bignum_plus.h"
#include "bignum_output.h"
#include "character.h"
#include "condition.h"
#include "integer.h"
#include "memory.h"
#include "stream.h"
#include "stream_string.h"
#include "strtype.h"
#include "token.h"
#include "type_table.h"

/*
 *  type
 */
_g int fixnump(addr pos)
{
	return GetType(pos) == LISPTYPE_FIXNUM;
}

_g int integerp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_FIXNUM || type == LISPTYPE_BIGNUM;
}


/*
 *  throw
 */
_g void integer_throw_alloc(LocalRoot local, addr pos, addr *ret)
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

_g void integer_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	integer_throw_alloc(local, pos, ret);
}

_g void integer_throw_heap(addr pos, addr *ret)
{
	integer_throw_alloc(NULL, pos, ret);
}

_g void integer_result_alloc(LocalRoot local, addr pos, addr *ret)
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

_g void integer_result_local(LocalRoot local, addr pos, addr *ret)
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

_g void integer_result_heap(addr pos, addr *ret)
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

_g void fixnum_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	fixnum_alloc(local, ret, RefFixnum(pos));
}

_g void fixnum_copy_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	fixnum_copy_alloc(local, pos, ret);
}

_g void fixnum_copy_heap(LocalRoot local, addr pos, addr *ret)
{
	fixnum_copy_alloc(NULL, pos, ret);
}

_g void integer_copy_alloc(LocalRoot local, addr pos, addr *ret)
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

_g void integer_copy_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	integer_copy_alloc(local, pos, ret);
}

_g void integer_copy_heap(addr pos, addr *ret)
{
	integer_copy_alloc(NULL, pos, ret);
}


/*
 *  compare
 */
_g void getsign_integer(addr pos, int *ret)
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

_g int zerop_or_plusp_integer(addr pos)
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

_g int plusp_integer(addr pos)
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

_g int minusp_integer(addr pos)
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

_g int zerop_integer(addr pos)
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

_g int equal_integer(addr left, addr right)
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

_g int compare_integer(addr left, addr right)
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

_g int less_integer_clang(addr left, addr right)
{
	return less_integer(left, right);
}

_g int less_equal_integer_clang(addr left, addr right)
{
	return less_equal_integer(left, right);
}

_g void sign_reverse_integer_common(addr pos, addr *ret)
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

_g int evenp_integer(addr left)
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
_g int output_nosign_integer_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return output_nosign_fixnum_(stream, RefFixnum(pos), base, upperp);

		case LISPTYPE_BIGNUM:
			return output_nosign_bignum_(local, stream, pos, base, upperp);

		default:
			return TypeError_(pos, INTEGER);
	}
}

_g int output_nosign_comma_integer_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp, size_t range, unicode comma)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return output_nosign_comma_fixnum_(local, stream, RefFixnum(pos),
					base, upperp, range, comma);

		case LISPTYPE_BIGNUM:
			return output_nosign_comma_bignum_(local, stream, pos,
					base, upperp, range, comma);

		default:
			return TypeError_(pos, INTEGER);
	}
}

#define INTEGER_STREAM_SIZE		64
_g int string_nosign_comma_integer_(LocalRoot local, addr *ret, addr pos,
		unsigned base, int upperp, size_t range, unicode comma)
{
	addr stream;

	open_output_string_stream(&stream, INTEGER_STREAM_SIZE);
	Return(output_nosign_comma_integer_(local,
			stream, pos, base, upperp, range, comma));
	Return(string_stream_alloc_(local, stream, ret));
	close_output_string_stream(stream);

	return 0;
}


/*
 *  size
 */
_g void make_index_integer_alloc(LocalRoot local, addr *ret, size_t value)
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

_g void make_index_integer_local(LocalRoot local, addr *ret, size_t value)
{
	CheckLocal(local);
	make_index_integer_alloc(local, ret, value);
}

_g void make_index_integer_heap(addr *ret, size_t value)
{
	make_index_integer_alloc(NULL, ret, value);
}

_g void make_indexmax_alloc(LocalRoot local, addr *ret)
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
	if (value < 0) return 1;
	*ret = (size_t)value;

	return 0;
}
#else
static int getindex_integer_fixnum(addr pos, size_t *ret)
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
static int getindex_integer_bignum(addr pos, size_t *ret)
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
static int getindex_integer_bignum(addr pos, size_t *ret)
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

_g int GetIndex_integer(addr pos, size_t *ret)
{
	addr type;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return getindex_integer_fixnum(pos, ret);

		case LISPTYPE_BIGNUM:
			return getindex_integer_bignum(pos, ret);

		default:
			GetTypeTable(&type, Index);
			type_error(pos, type);
			return 1;
	}
}

_g void getindex_integer(addr pos, size_t *ret)
{
	addr type;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			if (getindex_integer_fixnum(pos, ret))
				goto error;
			break;

		case LISPTYPE_BIGNUM:
			if (getindex_integer_bignum(pos, ret))
				goto error;
			break;

		default:
			goto error;
	}
	return;

error:
	GetTypeTable(&type, Index);
	type_error(pos, type);
}

_g addr reference_index_integer_alloc(LocalRoot local, size_t value)
{
	addr pos;
	make_index_integer_alloc(local, &pos, value);
	return pos;
}

_g addr reference_index_integer_local(LocalRoot local, size_t value)
{
	Check(local == NULL, "local error");
	return reference_index_integer_alloc(local, value);
}

_g addr reference_index_integer_heap(size_t value)
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

_g int getindex_sign_integer(addr pos, int *sign, size_t *ret)
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

_g int GetIndex_fixnum(addr pos, size_t *ret)
{
	fixnum value;

	if (! fixnump(pos)) return 1;
	GetFixnum(pos, &value);
	if (value < 0) return 1;
	*ret = (size_t)value;

	return 0;
}

_g void getindex_fixnum(addr pos, size_t *ret)
{
	if (GetIndex_fixnum(pos, ret))
		TypeError(pos, FIXNUM);
}

_g int fixnum_index_heap_(addr *ret, size_t value)
{
	if ((size_t)FIXNUM_MAX <= value)
		return fmte_("Too large value ~S.", intsizeh(value), NULL);
	fixnum_heap(ret, (fixnum)value);

	return 0;
}


/*
 *  standard type
 */
_g void int8_integer_alloc(LocalRoot local, addr *ret, int8_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

_g void int16_integer_alloc(LocalRoot local, addr *ret, int16_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

_g void int32_integer_alloc(LocalRoot local, addr *ret, int32_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

_g void uint8_integer_alloc(LocalRoot local, addr *ret, uint8_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

_g void uint16_integer_alloc(LocalRoot local, addr *ret, uint16_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

#ifdef LISP_64BIT
_g void uint32_integer_alloc(LocalRoot local, addr *ret, uint32_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}
#else
_g void uint32_integer_alloc(LocalRoot local, addr *ret, uint32_t value)
{
	bignum_value_alloc(local, ret, signplus_bignum, (fixed)value);
}
#endif

#ifdef LISP_64BIT
_g void int64_integer_alloc(LocalRoot local, addr *ret, int64_t value)
{
	fixnum_alloc(local, ret, (fixnum)value);
}

_g void uint64_integer_alloc(LocalRoot local, addr *ret, uint64_t value)
{
	bignum_value_alloc(local, ret, signplus_bignum, (fixed)value);
}
#endif


/*
 *  oneplus
 */
_g void oneplus_integer_common(LocalRoot local, addr value, addr *ret)
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

_g void oneminus_integer_common(LocalRoot local, addr value, addr *ret)
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
_g void plus_fi_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
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

_g void plus_fi_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

_g void plus_fi_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void plus_bi_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
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

_g void plus_bi_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

_g void plus_bi_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void plus_ii_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
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

_g void plus_ii_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

_g void plus_ii_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void minus_ii_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void multi_ii_real_common(LocalRoot local, addr left, addr right, addr *ret)
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
_g void ash_bignum_common(LocalRoot local, addr pos, int sign2, size_t size, addr *ret)
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

_g int ash_integer_common_(LocalRoot local, addr pos, addr count, addr *ret)
{
	int sign2;
	size_t size;
	LocalStack stack;

	if (getindex_sign_integer(count, &sign2, &size)) {
		*ret = 0;
		return fmte_("Too large shift value ~S.", count, NULL);
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
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
	rollback_local(local, stack);
	return 0;
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

static void integer_length_fixnum(addr pos, size_t *ret)
{
	int sign;
	bigtype value;

	castfixed_fixnum(pos, &sign, &value);
	if (IsPlus(sign))
		*ret = integer_length_bigtype(value);
	else
		*ret = inverse_length_bigtype(value);
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

static void integer_length_bignum(addr pos, size_t *ret)
{
	size_t size;

	size = integer_length_bigdata(pos);
	if (check_length_bignum(pos))
		size--;
	*ret = size;
}

_g void integer_length_value(addr pos, size_t *ret)
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

_g void integer_length_common(addr pos, addr *ret)
{
	size_t size;
	integer_length_value(pos, &size);
	make_index_integer_alloc(NULL, ret, size);
}


/*
 *  parse-integer
 */
_g int parse_integer_clang(LocalRoot local,
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
		Return(string_getc_(string, i, &c));
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
	return 0;

error:
	if (junk) {
		if (bigcons_empty_p(cons))
			*ret = Nil;
		else
			integer_cons_heap(ret, sign, cons);
		make_index_integer_heap(position, i);
		rollback_local(local, stack);
		return 0;
	}
	else {
		rollback_local(local, stack);
		*ret = *position = 0;
		character_heap(&cons, c);
		return fmte_("Invalid string ~A.", cons, NULL);
	}
}

