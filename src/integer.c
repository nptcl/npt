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
_g int integer_throw_alloc_(LocalRoot local, addr pos, addr *ret)
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

_g int integer_throw_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return integer_throw_alloc_(local, pos, ret);
}

_g int integer_throw_heap_(addr pos, addr *ret)
{
	return integer_throw_alloc_(NULL, pos, ret);
}

_g int integer_result_alloc_(LocalRoot local, addr pos, addr *ret)
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

_g int integer_result_local_(LocalRoot local, addr pos, addr *ret)
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

_g int integer_result_heap_(addr pos, addr *ret)
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

_g int integer_copy_alloc_(LocalRoot local, addr pos, addr *ret)
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

_g int integer_copy_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return integer_copy_alloc_(local, pos, ret);
}

_g int integer_copy_heap_(addr pos, addr *ret)
{
	return integer_copy_alloc_(NULL, pos, ret);
}


/*
 *  compare
 */
_g int getsign_integer_(addr pos, int *ret)
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

_g int zerop_or_plusp_integer_(addr pos, int *ret)
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

_g int plusp_integer_(addr pos, int *ret)
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

_g int minusp_integer_(addr pos, int *ret)
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

_g int zerop_integer_(addr pos, int *ret)
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

_g int equal_integer_(addr left, addr right, int *ret)
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

_g int not_equal_integer_(addr left, addr right, int *ret)
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

_g int compare_integer_(addr left, addr right, int *ret)
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

_g int less_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_integer_(left, right, &check));
	return Result(ret, check < 0);
}

_g int less_equal_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_integer_(left, right, &check));
	return Result(ret, check <= 0);
}

_g int greater_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_integer_(left, right, &check));
	return Result(ret, check > 0);
}

_g int greater_equal_integer_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_integer_(left, right, &check));
	return Result(ret, check >= 0);
}

_g int zerop_integer_debug(addr pos)
{
	int check;

	Check(! integerp(pos), "left error");
	check = 0;
	Error(zerop_integer_(pos, &check));

	return check;
}

_g int plusp_integer_debug(addr pos)
{
	int check;

	Check(! integerp(pos), "left error");
	check = 0;
	Error(plusp_integer_(pos, &check));

	return check;
}

_g int minusp_integer_debug(addr pos)
{
	int check;

	Check(! integerp(pos), "left error");
	check = 0;
	Error(minusp_integer_(pos, &check));

	return check;
}

_g int less_integer_debug(addr left, addr right)
{
	int check;

	Check(! integerp(left), "left error");
	Check(! integerp(right), "right error");
	check = 0;
	Error(less_integer_(left, right, &check));

	return check;
}

_g int less_equal_integer_debug(addr left, addr right)
{
	int check;

	Check(! integerp(left), "left error");
	Check(! integerp(right), "right error");
	check = 0;
	Error(less_equal_integer_(left, right, &check));

	return check;
}

_g int sign_reverse_integer_common_(addr pos, addr *ret)
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

_g int evenp_integer_(addr left, int *ret)
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

_g int GetIndex_integer(addr pos, size_t *ret)
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

_g int getindex_integer_(addr pos, size_t *ret)
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

_g int getindex_sign_integer_(addr pos, int *sign, size_t *value, int *ret)
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

_g int GetIndex_fixnum(addr pos, size_t *ret)
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

_g int getindex_fixnum_(addr pos, size_t *ret)
{
	if (GetIndex_fixnum(pos, ret)) {
		*ret = 0;
		return TypeError_(pos, FIXNUM);
	}

	return 0;
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
_g int oneplus_integer_common_(LocalRoot local, addr value, addr *ret)
{
	CheckLocal(local);
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, 1, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, 1, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(value, INTEGER);
	}
}

_g int oneminus_integer_common_(LocalRoot local, addr value, addr *ret)
{
	CheckLocal(local);
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, -1, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, -1, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(value, INTEGER);
	}
}


/*
 *  plus
 */
_g int plus_fi_bignum_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_bignum_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_fb_bignum_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

_g int plus_fi_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_fb_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

_g int plus_fi_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_fb_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

_g int plus_bi_bignum_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_bignum_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bb_bignum_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

_g int plus_bi_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bb_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

_g int plus_bi_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bb_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

_g int plus_ii_bignum_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fi_bignum_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bi_bignum_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, INTEGER);
	}
}

_g int plus_ii_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fi_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bi_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, INTEGER);
	}
}

_g int plus_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fi_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bi_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, INTEGER);
	}
}


/*
 *  minus
 */
static int minus_fi_integer_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_fb_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

static int minus_bi_integer_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bb_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

_g int minus_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_fi_integer_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_bi_integer_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, INTEGER);
	}
}


/*
 *  multi
 */
static int multi_fi_integer_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_fb_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

static int multi_bi_integer_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_bb_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, INTEGER);
	}
}

_g int multi_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return multi_fi_integer_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_bi_integer_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, INTEGER);
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
	int sign2, check;
	size_t size;
	LocalStack stack;

	Return(getindex_sign_integer_(count, &sign2, &size, &check));
	if (check) {
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

_g int integer_length_value_(addr pos, size_t *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			integer_length_fixnum(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			integer_length_bignum(pos, ret);
			return 0;

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

_g int integer_length_common_(addr pos, addr *ret)
{
	size_t size;

	Return(integer_length_value_(pos, &size));
	make_index_integer_heap(ret, size);

	return 0;
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

