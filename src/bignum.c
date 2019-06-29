/*
 *  bignum
 */
#include <math.h>
#include <errno.h>
#include "arch.h"
#include "character.h"
#include "charqueue.h"
#include "condition.h"
#include "cons.h"
#include "bigcons.h"
#include "bigdata.h"
#include "bignum.h"
#include "condition.h"
#include "integer.h"
#include "number.h"
#include "real_float.h"
#include "sequence.h"
#include "stream.h"
#include "token.h"
#include "typedef.h"

/*
 *     user:  sign
 *    array:  [bigdata]
 *     body:  bignuminfo
 *
 *  bigdata
 *    [bigtype1] [bigtype2] ...
 */
_g int bignump(addr pos)
{
	return GetType(pos) == LISPTYPE_BIGNUM;
}

_g struct bignuminfo *struct_bignum(addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return StructBignum_Low(pos);
}

_g size_t refalloc_bignum(addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return RefAllocBignum_Low(pos);
}

_g void setsize_bignum(addr pos, size_t value)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	SetSizeBignum_Low(pos, value);
}

_g void getsize_bignum(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	GetSizeBignum_Low(pos, ret);
}

_g size_t refsize_bignum(addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return RefSizeBignum_Low(pos);
}

_g void setroot_bignum(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	SetRootBignum_Low(pos, value);
}

_g void getroot_bignum(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	GetRootBignum_Low(pos, ret);
}

_g void setsign_bignum(addr pos, int sign)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	SetSignBignum_Low(pos, sign);
}

_g void getsign_bignum(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	GetSignBignum_Low(pos, ret);
}

_g int refsign_bignum(addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return RefSignBignum_Low(pos);
}

_g void alloc_bignum(LocalRoot local, addr *ret, size_t alloc)
{
	addr ptr, data;
	struct bignuminfo *str;

	alloc_smallsize(local, &ptr, LISPTYPE_BIGNUM, 1, sizeoft(struct bignuminfo));
	alloc_bigdata(local, &data, alloc);
	SetSignBignum(ptr, SignPlus);
	str = StructBignum(ptr);
	str->size = 0;
	str->alloc = alloc;
	SetRootBignum(ptr, data);
	*ret = ptr;
}

_g void alloc_plus_bignum(LocalRoot local, addr *ret, size_t a, size_t b)
{
	addr pos;

	if (plussafe_size(a, b, &a)) {
		if (local == NULL)
			local = Local_Thread;
		plus_ii_real_common(local, intsizeh(a), intsizeh(b), &pos);
		fmte("Too large bignum size ~A.", pos, NULL);
		return;
	}
	alloc_bignum(local, ret, a);
}

_g void realloc_bignum(LocalRoot local, addr pos, size_t alloc, int force)
{
	addr src, dst;
	struct bignuminfo *info;
	size_t size, copy;

	info = StructBignum(pos);
	if (force || info->alloc < alloc) {
		size = info->size;
		GetRootBignum(pos, &src);
		alloc_bigdata(local, &dst, alloc);
		SetRootBignum(pos, dst);

		if (alloc < size) {
			copy = size;
			info->size = alloc;
		}
		else {
			copy = alloc;
#ifdef LISP_DEBUG
			bigset(PtrDataBignum(dst) + size, 0xAA, (alloc - size));
#endif
		}
		bigcpy(PtrDataBignum(dst), PtrDataBignum(src), copy);
		info->alloc = alloc;
	}
}

_g void bignum_alloc(LocalRoot local, addr *ret, int sign, size_t size)
{
	addr pos, root;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	alloc_bignum(local, &pos, size);
	SetSignBignum(pos, sign);
	SetSizeBignum(pos, 1);
	GetRootBignum(pos, &root);
	PtrDataBignum(root)[0] = 0;
	*ret = pos;
}

static void bigconscopy(addr root, addr cons)
{
	bigtype *data;
	struct bigbuffer *str;
	size_t i, count;

	data = PtrDataBignum(root);
	GetRootBigcons(cons, &root);
	for (i = 0; root != Nil; i += count) {
		str = StructBigbuffer(root);
		count = str->count;
		if (count == 0) break;
		bigcpy(data + i, str->buffer, count);
		GetNextBigbuffer(root, &root);
	}
}

_g void bignum_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons)
{
	addr pos, root;
	size_t count;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	GetCountBigcons(cons, &count);
	alloc_bignum(local, &pos, count);
	GetRootBignum(pos, &root);
	bigconscopy(root, cons);
	SetSignBignum(pos, sign);
	SetSizeBignum(pos, count);
	*ret = pos;
}

_g void bignum_copy_nosign_alloc(LocalRoot local, addr *ret, addr right)
{
	addr pos, root;
	size_t size;

	Check(GetType(right) != LISPTYPE_BIGNUM, "type error");
	GetSizeBignum(right, &size);
	alloc_bignum(local, &pos, size);
	SetSizeBignum(pos, size);
	GetRootBignum(right, &right);
	GetRootBignum(pos, &root);
	bigcpy(PtrDataBignum(root), PtrDataBignum(right), size);
	*ret = pos;
}

_g void bignum_copy_alloc(LocalRoot local, addr *ret, addr right)
{
	addr pos;

	bignum_copy_nosign_alloc(local, &pos, right);
	SetSignBignum(pos, RefSignBignum(right));
	*ret = pos;
}

_g void bignum_value_alloc(LocalRoot local, addr *ret, int sign, fixed value)
{
	addr pos, root;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	alloc_bignum(local, &pos, 1);
	SetSizeBignum(pos, 1);
	SetSignBignum(pos, sign);
	GetRootBignum(pos, &root);
	PtrDataBignum(root)[0] = value;
	*ret = pos;
}

_g void bignum_value2_alloc(LocalRoot local, addr *ret, int sign, fixed high, fixed low)
{
	addr pos, root;
	bigtype *data;

	if (high == 0) {
		bignum_value_alloc(local, ret, sign, low);
		return;
	}

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	alloc_bignum(local, &pos, 2);
	SetSizeBignum(pos, 2);
	SetSignBignum(pos, sign);
	GetRootDataBignum(pos, &root, &data);
	data[1] = high;
	data[0] = low;
	*ret = pos;
}

_g void bignum_zero_alloc(LocalRoot local, addr *ret)
{
	bignum_value_alloc(local, ret, SignPlus, 0);
}

_g void bignum_fixnum_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	bignum_fixnum_value_alloc(local, ret, RefFixnum(pos));
}

_g void bignum_fixnum_value_alloc(LocalRoot local, addr *ret, fixnum value)
{
	int sign;
	bigtype result;
	castfixed(value, &sign, &result);
	bignum_value_alloc(local, ret, sign, result);
}

_g void bignum_counter_alloc(LocalRoot local, addr *ret, addr index)
{
	if ((! integerp(index))
			|| minusp_integer(index)
			|| zerop_integer(index)) {
		fmte("The value ~S must be a positive integer.", index, NULL);
	}
	if (GetType(index) == LISPTYPE_FIXNUM)
		bignum_fixnum_value_alloc(local, ret, RefFixnum(index));
	else
		bignum_copy_alloc(local, ret, index);
}

#ifdef LISP_DEBUG
_g void bignum_debug(LocalRoot local, addr *ret, int sign, size_t size)
{
	Check(local == NULL, "local error");
	bignum_alloc(local, ret, sign, size);
}

_g void bignum_cons_debug(LocalRoot local, addr *ret, int sign, addr cons)
{
	Check(local == NULL, "local error");
	bignum_cons_alloc(local, ret, sign, cons);
}

_g void bignum_copy_nosign_debug(LocalRoot local, addr *ret, addr right)
{
	Check(local == NULL, "local error");
	bignum_copy_nosign_alloc(local, ret, right);
}

_g void bignum_copy_debug(LocalRoot local, addr *ret, addr right)
{
	Check(local == NULL, "local error");
	bignum_copy_alloc(local, ret, right);
}

_g void bignum_value_debug(LocalRoot local, addr *ret, int sign, fixed value)
{
	Check(local == NULL, "local error");
	bignum_value_alloc(local, ret, sign, value);
}

_g void bignum_value2_debug(LocalRoot local, addr *ret, int sign, fixed high, fixed low)
{
	Check(local == NULL, "local error");
	bignum_value2_alloc(local, ret, sign, high, low);
}

_g void bignum_zero_debug(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	bignum_zero_alloc(local, ret);
}

_g void bignum_fixnum_debug(LocalRoot local, addr *ret, addr value)
{
	Check(local == NULL, "local error");
	bignum_fixnum_alloc(local, ret, value);
}

_g void bignum_fixnum_value_debug(LocalRoot local, addr *ret, fixnum value)
{
	Check(local == NULL, "local error");
	bignum_fixnum_value_alloc(local, ret, value);
}

_g void bignum_counter_debug(LocalRoot local, addr *ret, addr index)
{
	Check(local == NULL, "local error");
	bignum_counter_alloc(local, ret, index);
}

_g void bignum_result_debug(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	bignum_result_alloc(local, pos, ret);
}

_g void bignum_integer_debug(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	bignum_integer_alloc(local, ret, pos);
}

#endif

_g void getfixed_bignum(addr pos, size_t index, fixed *ret)
{
	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	Check(RefSizeBignum(pos) <= index, "index error");
	GetRootBignum(pos, &pos);
	*ret = PtrDataBignum(pos)[index];
}

_g fixed reffixed_bignum(addr pos, size_t index)
{
	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	Check(RefSizeBignum(pos) <= index, "index error");
	GetRootBignum(pos, &pos);
	return (fixed)PtrDataBignum(pos)[index];
}

_g void setfixed_bignum(addr pos, size_t index, fixed value)
{
	struct bignuminfo *ptr;

	ptr = StructBignum(pos);
	Check(ptr->alloc <= index, "alloc size error");
	if (ptr->size <= index)
		ptr->size = index + 1;
	GetRootBignum(pos, &pos);
	PtrDataBignum(pos)[index] = value;
}

_g void diet_bignum(LocalRoot local, addr pos)
{
	struct bignuminfo *info;

	info = StructBignum(pos);
	if (info->size != info->alloc)
		realloc_bignum(local, pos, info->size, 1);
}

_g void sizepress_bignum(addr left)
{
	size_t size, i;
	bigtype *data;
	struct bignuminfo *ptr;

	ptr = StructBignum(left);
	size = ptr->size;
	GetRootDataBignum(left, &left, &data);
	if (1 < size) {
		for (i = size - 1; ; i--) {
			if (i == 0) {
				ptr->size = 1;
				break;
			}
			if (data[i] != 0) {
				ptr->size = i + 1;
				break;
			}
		}
	}
}


/*
 *  operation
 */
static void resize_nocopy_bignum(LocalRoot local, addr pos, size_t alloc, int force)
{
	addr src, dst;
	struct bignuminfo *info;
	size_t size;

	info = StructBignum(pos);
	if (force || info->alloc < alloc) {
		size = info->size;
		GetRootBignum(pos, &src);
		alloc_bigdata(local, &dst, alloc);
		SetRootBignum(pos, dst);

		if (alloc < size) {
			info->size = alloc;
		}
#ifdef LISP_DEBUG
		if (size < alloc) {
			bigset(PtrDataBignum(dst), 0xAA, alloc);
		}
#endif
		info->alloc = alloc;
	}
}

_g void copy_bignum(LocalRoot local, addr left, addr right, int force)
{
	size_t size;

	GetSizeBignum(right, &size);
	resize_nocopy_bignum(local, left, size, force);
	SetSizeBignum(left, size);
	SetSignBignum(left, RefSignBignum(right));
	GetRootBignum(left, &left);
	GetRootBignum(right, &right);
	bigcpy(PtrDataBignum(left), PtrDataBignum(right), size);
}

_g void copy_noexpand_bignum(addr left, addr right)
{
	addr dst, src;
	size_t size;

	GetSizeBignum(right, &size);
	Check(StructBignum(left)->alloc < size, "alloc size error");
	GetRootBignum(left, &dst);
	GetRootBignum(right, &src);
	bigcpy(PtrDataBignum(dst), PtrDataBignum(src), size);
	SetSignBignum(left, RefSignBignum(right));
	SetSizeBignum(left, size);
}

_g void setvalue_bignum(addr left, int sign, bigtype value)
{
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	SetSignBignum(left, sign);
	SetSizeBignum(left, 1);
	GetRootBignum(left, &left);
	PtrDataBignum(left)[0] = value;
}

_g void setzero_bignum(addr left)
{
	setvalue_bignum(left, SignPlus, 0);
}

_g int getbit_bignum(addr pos, size_t index)
{
	size_t count, front;
	bigtype value;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	count = index / BIGNUM_FULLBIT;
	front = index % BIGNUM_FULLBIT;
	if (RefSizeBignum(pos) <= count) return 0;
	getfixed_bignum(pos, count, &value);
	return ((1ULL << front) & value)? 1: 0;
}

_g void incf_bignum(addr pos, bigtype value)
{
	int sign;
	GetSignBignum(pos, &sign);
	setplusvalue_bigdata(pos, pos, SignPlus, value);
}

_g void decf_bignum(addr pos, bigtype value)
{
	int sign;
	GetSignBignum(pos, &sign);
	setminusvalue_bigdata(pos, pos, SignPlus, value);
}

_g void bignum_result_alloc(LocalRoot local, addr pos, addr *ret)
{
	addr root;
	bigtype *data, value;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	if (RefSizeBignum(pos) != 1) {
		bignum_throw_alloc(local, pos, ret);
		return;
	}
	GetRootDataBignum(pos, &root, &data);
	value = data[0];
	if (IsPlus(RefSignBignum(pos))) {
		if (value <= FIXNUM_MAX) {
			fixnum_alloc(local, ret, (fixnum)value);
			return;
		}
	}
	else {
		if (value <= FIXNUM_UMIN) {
			fixnum_alloc(local, ret, -(fixnum)value);
			return;
		}
	}
	bignum_throw_alloc(local, pos, ret);
}

_g void bignum_integer_alloc(LocalRoot local, addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			bignum_fixnum_alloc(local, ret, pos);
			break;

		case LISPTYPE_BIGNUM:
			bignum_copy_alloc(local, ret, pos);
			break;

		default:
			TypeError(pos, INTEGER);
			break;
	}
}

_g void bignum_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	if (GetStatusDynamic(pos))
		bignum_copy_alloc(NULL, ret, pos);
	else
		*ret = pos;
}

_g void fixnum_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	if (GetStatusDynamic(pos))
		fixnum_heap(ret, RefFixnum(pos));
	else
		*ret = pos;
}

_g void bignum_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		bignum_copy_alloc(local, ret, pos);
}

_g void fixnum_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		fixnum_local(local, ret, RefFixnum(pos));
}

_g void bignum_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		bignum_throw_local(local, pos, ret);
	else
		bignum_throw_heap(pos, ret);
}

_g void fixnum_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		fixnum_throw_local(local, pos, ret);
	else
		fixnum_throw_heap(pos, ret);
}

_g void power2_bignum_alloc(LocalRoot local, addr *ret, int sign, size_t value)
{
	addr pos;
	power2_bigdata_alloc(local, &pos, value);
	SetSignBignum(pos, sign);
	bignum_result_alloc(local, pos, ret);
}

_g void power2_bignum_local(LocalRoot local, addr *ret, int sign, size_t value)
{
	Check(local == NULL, "local error");
	power2_bignum_alloc(local, ret, sign, value);
}

_g void power2_bignum_heap(addr *ret, int sign, size_t value)
{
	power2_bignum_alloc(NULL, ret, sign, value);
}

_g void shiftup_bignum_alloc(LocalRoot local, addr *ret, addr left, size_t value)
{
	shiftup_bigdata_alloc(local, &left, left, value);
	bignum_result_alloc(local, left, ret);
}

_g void shiftup_bignum_local(LocalRoot local, addr *ret, addr left, size_t value)
{
	Check(local == NULL, "local error");
	shiftup_bignum_alloc(local, ret, left, value);
}

_g void shiftup_bignum_heap(addr *ret, addr left, size_t value)
{
	shiftup_bignum_alloc(NULL, ret, left, value);
}


/*
 *  integer
 */
static void carryvalue_alloc(LocalRoot local, addr *ret, int sign, bigtype value)
{
	addr pos, root;
	bigtype *data;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	alloc_bignum(local, &pos, 2);
	SetSignBignum(pos, sign);
	SetSizeBignum(pos, 2);
	GetRootDataBignum(pos, &root, &data);
	data[0] = (bigtype)value;
	data[1] = 1;
	*ret = pos;
}

static int castfixnum(int sign, bigtype value, fixnum *result)
{
	Check(sign != SignPlus && sign != SignMinus, "sign error");

	if (IsPlus(sign)) {
		if (value <= FIXNUM_MAX) {
			*result = (fixnum)value;
			return 1;
		}
	}
	else {
		if (value <= FIXNUM_MAX) {
			*result = -(fixnum)value;
			return 1;
		}
		if (value == FIXNUM_UMIN) {
			*result = FIXNUM_MIN;
			return 1;
		}
	}

	return 0;
}

#ifdef BIGNUM_FULLCODE
_g int fixnum_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons)
{
	fixnum result;
	addr root;
	struct bigbuffer *str;
	size_t size;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	GetCountBigcons(cons, &size);
	if (size == 1) {
		GetRootBigcons(cons, &root);
		str = StructBigbuffer(root);
		if (castfixnum(sign, str->buffer[0], &result)) {
			fixnum_alloc(local, ret, result);
			return 0;
		}
	}

	return 1;
}

#else
_g int fixnum_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons)
{
	bigtype value;
	fixnum result;
	addr root;
	struct bigbuffer *str;
	size_t size, i, rem;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	GetCountBigcons(cons, &size);
	if (size <= (LISP_INTEGER_BIT / BIGNUM_FULLBIT)) {
		GetRootBigcons(cons, &root);
		str = StructBigbuffer(root);
		value = 0;
		for (i = 0; i < size; i++) {
			rem = i % BIGCONS_SIZE;
			if (i && rem == 0) {
				GetNextBigbuffer(root, &root);
				str = StructBigbuffer(root);
			}
			value |= str->buffer[rem] << (i * BIGNUM_FULLBIT);
		}
		if (castfixnum(sign, value, &result)) {
			fixnum_alloc(local, ret, result);
			return 0;
		}
	}

	return 1;
}
#endif

_g int fixnum_cons_local(LocalRoot local, addr *ret, int sign, addr cons)
{
	Check(local == NULL, "local error");
	return fixnum_cons_alloc(local, ret, sign, cons);
}

_g int fixnum_cons_heap(addr *ret, int sign, addr cons)
{
	return fixnum_cons_alloc(NULL, ret, sign, cons);
}

_g void integer_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons)
{
	if (fixnum_cons_alloc(local, ret, sign, cons))
		bignum_cons_alloc(local, ret, sign, cons);
}

_g void integer_cons_local(LocalRoot local, addr *ret, int sign, addr cons)
{
	Check(local == NULL, "local error");
	integer_cons_alloc(local, ret, sign, cons);
}

_g void integer_cons_heap(addr *ret, int sign, addr cons)
{
	integer_cons_alloc(NULL, ret, sign, cons);
}

_g void integer_fixed_alloc(LocalRoot local, addr *ret, int sign, fixed value)
{
	fixnum result;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	if (castfixnum(sign, value, &result)) {
		fixnum_alloc(local, ret, result);
	}
	else {
		bignum_value_alloc(local, ret, sign, value);
	}
}

_g void integer_fixed_local(LocalRoot local, addr *ret, int sign, fixed value)
{
	Check(local == NULL, "local error");
	integer_fixed_alloc(local, ret, sign, value);
}

_g void integer_fixed_heap(addr *ret, int sign, fixed value)
{
	integer_fixed_alloc(NULL, ret, sign, value);
}

_g void integer_bignum_alloc(LocalRoot local, addr *ret, addr pos)
{
	int sign;
	size_t size;

	GetSizeBignum(pos, &size);
	if (size == 1) {
		GetSignBignum(pos, &sign);
		GetRootBignum(pos, &pos);
		integer_fixed_alloc(local, ret, sign, PtrDataBignum(pos)[0]);
	}
	else {
		bignum_throw_alloc(local, pos, ret);
	}
}

_g void integer_bignum_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	integer_bignum_alloc(local, ret, pos);
}

_g void integer_bignum_heap(addr *ret, addr pos)
{
	integer_bignum_alloc(NULL, ret, pos);
}


/*
 *  integer-copy
 */
static void fixnum_copysign_alloc(LocalRoot local, int sign, fixnum value, addr *ret)
{
	if (value == FIXNUM_MIN) {
		if (IsPlus(sign))
			bignum_value_alloc(local, ret, SignPlus, FIXNUM_UMIN);
		else
			fixnum_alloc(local, ret, value);
	}
	else if (IsPlus(sign)) {
		if (0 <= value)
			fixnum_alloc(local, ret, value);
		else
			fixnum_alloc(local, ret, -value);
	}
	else {
		if (0 <= value)
			fixnum_alloc(local, ret, -value);
		else
			fixnum_alloc(local, ret, value);
	}
}

static void bignum_copysign_alloc(LocalRoot local, int sign, addr pos, addr *ret)
{
	bignum_copy_nosign_alloc(local, &pos, pos);
	SetSignBignum(pos, sign);
	*ret = pos;
}

_g void integer_copysign_alloc(LocalRoot local, int sign, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_copysign_alloc(local, sign, RefFixnum(pos), ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_copysign_alloc(local, sign, pos, ret);
			break;

		default:
			TypeError(pos, INTEGER);
			break;
	}
}

_g void integer_copysign_local(LocalRoot local, int sign, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	integer_copysign_alloc(local, sign, pos, ret);
}

_g void integer_copysign_heap(int sign, addr pos, addr *ret)
{
	integer_copysign_alloc(NULL, sign, pos, ret);
}


/*****************************************************************************
  float
 *****************************************************************************/
#define HexToChar(x) (((x) < 10)? ('0' + (x)): ('A' - 10 + (x)))
#define HEXCHAR_BIGTYPE_SIZE (BIGNUM_FULLBIT >> 2)

static char *hexchar_bigtype(char *dst, bigtype v)
{
	int i, c;

	for (i = 0; i < HEXCHAR_BIGTYPE_SIZE; i++) {
		c = (int)(v & 0x0F);
		dst[HEXCHAR_BIGTYPE_SIZE - 1 - i] = HexToChar(c);
		v >>= 4;
	}

	return dst + HEXCHAR_BIGTYPE_SIZE;
}

static char *hexfraction_string(char *dst, addr pos, int frac, size_t *exponent)
{
	size_t i, size, tail;
	bigtype *data;

	GetSizeBignum(pos, &size);
	GetRootDataBignum(pos, &pos, &data);
	tail = size - 1;

	for (i = 0; ; i++, tail--) {
		if (size <= i) {
			*exponent = 0;
			break;
		}
		if (frac <= 0) {
			dst = hexchar_bigtype(dst, data[tail]);
			*exponent = tail * BIGNUM_FULLBIT;
			break;
		}
		dst = hexchar_bigtype(dst, data[tail]);
		frac -= BIGNUM_FULLBIT;
	}

	return dst;
}

static char *expchar_make_float(char *dst, size_t size)
{
	size_t i, m;
	char buffer[32];

	if (size == 0) {
		dst[0] = '0';
		return dst + 1;
	}
	for (i = 0; size; i++) {
		buffer[i] = '0' + (size % 10);
		size /= 10;
	}
	for (m = 0; m < i; m++) {
		dst[i - m - 1] = buffer[m];
	}

	return dst + i;
}

static void make_float_string(char *dst, addr pos, int size)
{
	int sign;
	size_t exponent;

	GetSignBignum(pos, &sign);
	*(dst++) = IsPlus(sign)? '+': '-';
	*(dst++) = '0';
	*(dst++) = 'x';
	dst = hexfraction_string(dst, pos, size, &exponent);
	*(dst++) = 'p';
	dst = expchar_make_float(dst, exponent);
	*dst = '\0';
}

_g single_float single_float_fixnum(addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	return (single_float)RefFixnum(pos);
}

_g double_float double_float_fixnum(addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	return (double_float)RefFixnum(pos);
}

_g long_float long_float_fixnum(addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	return (long_float)RefFixnum(pos);
}

_g single_float single_float_bignum(addr pos)
{
	char buffer[64];
	int sign;
	size_t size;
	single_float ret;

	GetSizeBignum(pos, &size);
	if (size == 1) {
		GetSignBignum(pos, &sign);
		GetRootBignum(pos, &pos);
		ret = (single_float)PtrDataBignum(pos)[0];
		return IsMinus(sign)? -ret: ret;
	}
	make_float_string(buffer, pos, LISP_FLOAT_SINGLE_FRACTION);

	return check_strtof(buffer, pos);
}

_g double_float double_float_bignum(addr pos)
{
	char buffer[64];
	int sign;
	size_t size;
	double_float ret;

	GetSizeBignum(pos, &size);
	if (size == 1) {
		GetSignBignum(pos, &sign);
		GetRootBignum(pos, &pos);
		ret = (double_float)PtrDataBignum(pos)[0];
		return IsMinus(sign)? -ret: ret;
	}
	make_float_string(buffer, pos, LISP_FLOAT_DOUBLE_FRACTION);

	return check_strtod(buffer, pos);
}

_g long_float long_float_bignum(addr pos)
{
	char buffer[64];
	int sign;
	size_t size;
	long_float ret;

	GetSizeBignum(pos, &size);
	if (size == 1) {
		GetSignBignum(pos, &sign);
		GetRootBignum(pos, &pos);
		ret = (long_float)PtrDataBignum(pos)[0];
		return IsMinus(sign)? -ret: ret;
	}
	make_float_string(buffer, pos, LISP_FLOAT_LONG_FRACTION);

	return check_strtold(buffer, pos);
}

_g void single_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	single_float_alloc(local, ret, single_float_fixnum(pos));
}
_g void single_float_fixnum_local(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	single_float_local(local, ret, single_float_fixnum(pos));
}
_g void single_float_fixnum_heap(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	single_float_heap(ret, single_float_fixnum(pos));
}

_g void double_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	double_float_alloc(local, ret, double_float_fixnum(pos));
}
_g void double_float_fixnum_local(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	double_float_local(local, ret, double_float_fixnum(pos));
}
_g void double_float_fixnum_heap(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	double_float_heap(ret, double_float_fixnum(pos));
}

_g void long_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	long_float_alloc(local, ret, long_float_fixnum(pos));
}
_g void long_float_fixnum_local(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	long_float_local(local, ret, long_float_fixnum(pos));
}
_g void long_float_fixnum_heap(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	long_float_heap(ret, long_float_fixnum(pos));
}

_g void single_float_bignum_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	single_float_alloc(local, ret, single_float_bignum(pos));
}
_g void single_float_bignum_local(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	single_float_local(local, ret, single_float_bignum(pos));
}
_g void single_float_bignum_heap(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	single_float_heap(ret, single_float_bignum(pos));
}

_g void double_float_bignum_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	double_float_alloc(local, ret, double_float_bignum(pos));
}
_g void double_float_bignum_local(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	double_float_local(local, ret, double_float_bignum(pos));
}
_g void double_float_bignum_heap(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	double_float_heap(ret, double_float_bignum(pos));
}

_g void long_float_bignum_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	long_float_alloc(local, ret, long_float_bignum(pos));
}
_g void long_float_bignum_local(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	long_float_local(local, ret, long_float_bignum(pos));
}
_g void long_float_bignum_heap(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	long_float_heap(ret, long_float_bignum(pos));
}


/*
 *  TODO: bignum_float must use printf("%A").
 */
static size_t printf_integer_float_size(int exp)
{
	size_t size;

	size = (31UL*exp / 100UL); /* log10(2) = 0.301 <= 0.31 */
	size += 1UL/*sign*/ + 1UL/*null*/ + 1UL/*1digit*/;

	return size;
}

_g int bignum_single_float_alloc(LocalRoot local,
		addr *ret, single_float value, int isheap)
{
	int exponent, sign;
	LocalStack stack;
	addr pos;
	char *ptr;
	size_t size;

	if (! IsIntegerFloat(value)) return 1;
	sign = (value < 0.0f)? SignMinus: SignPlus;
	frexpf(value, &exponent);
	Check(exponent < 0, "exponent error");
	size = printf_integer_float_size(exponent);
	if (isheap)
		push_local(local, &stack);
	ptr = (char *)lowlevel_local(local, size);
	snprintf(ptr, size, "%.0f", fabsf(value));
	bigcons_char_local(local, &pos, 10, ptr);
	if (isheap) {
		bignum_cons_alloc(NULL, ret, sign, pos);
		rollback_local(local, stack);
	}
	else {
		bignum_cons_alloc(local, ret, sign, pos);
	}

	return 0;
}

_g int bignum_double_float_alloc(LocalRoot local,
		addr *ret, double_float value, int isheap)
{
	int exponent, sign;
	LocalStack stack;
	addr pos;
	char *ptr;
	size_t size;

	if (! IsIntegerDouble(value)) return 1;
	sign = (value < 0.0)? SignMinus: SignPlus;
	frexp(value, &exponent);
	Check(exponent < 0, "exponent error");
	size = printf_integer_float_size(exponent);
	if (isheap)
		push_local(local, &stack);
	ptr = (char *)lowlevel_local(local, size);
	snprintf(ptr, size, "%.0f", fabs(value));
	bigcons_char_local(local, &pos, 10, ptr);
	if (isheap) {
		bignum_cons_alloc(NULL, ret, sign, pos);
		rollback_local(local, stack);
	}
	else {
		bignum_cons_alloc(local, ret, sign, pos);
	}

	return 0;
}

_g int bignum_long_float_alloc(LocalRoot local, addr *ret, long_float value, int isheap)
{
	int exponent, sign;
	LocalStack stack;
	addr pos;
	char *ptr;
	size_t size;

	if (! IsIntegerLongFloat(value)) return 1;
	sign = (value < 0.0L)? SignMinus: SignPlus;
	frexpl(value, &exponent);
	Check(exponent < 0, "exponent error");
	size = printf_integer_float_size(exponent);
	if (isheap)
		push_local(local, &stack);
	ptr = (char *)lowlevel_local(local, size);
	snprintf(ptr, size, "%.0Lf", fabsl(value));
	bigcons_char_local(local, &pos, 10, ptr);
	if (isheap) {
		bignum_cons_alloc(NULL, ret, sign, pos);
		rollback_local(local, stack);
	}
	else {
		bignum_cons_alloc(local, ret, sign, pos);
	}

	return 0;
}


/*****************************************************************************
  compare
 *****************************************************************************/
_g int zerop_or_plusp_bignum(addr pos)
{
	return zerop_bignum(pos) || (IsPlus(RefSignBignum(pos)));
}

_g int plusp_bignum(addr pos)
{
	return (! zerop_bignum(pos)) && (IsPlus(RefSignBignum(pos)));
}

_g int minusp_bignum(addr pos)
{
	return (! zerop_bignum(pos)) && (IsMinus(RefSignBignum(pos)));
}

_g int zerop_bignum(addr pos)
{
	addr root;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	GetRootBignum(pos, &root);

	return RefSizeBignum(pos) == 1 && PtrDataBignum(root)[0] == 0;
}

_g int evenp_bignum(addr pos)
{
	fixed value;
	getfixed_bignum(pos, 0, &value);
	return (value & 1) == 0;
}

_g int oddp_bignum(addr pos)
{
	fixed value;
	getfixed_bignum(pos, 0, &value);
	return (value & 1) != 0;
}

_g void castfixed(fixnum value, int *sign, fixed *result)
{
	if (0 <= value) {
		*sign = SignPlus;
		*result = (fixed)value;
	}
	else if (value == FIXNUM_MIN) {
		*sign = SignMinus;
		*result = FIXNUM_UMIN;
	}
	else {
		*sign = SignMinus;
		*result = (fixed)-value;
	}
}

_g void castfixed_fixnum(addr pos, int *sign, fixed *result)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	castfixed(RefFixnum(pos), sign, result);
}

_g int castfixed_integer(addr pos, int *sign, fixed *result)
{
	size_t check;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			castfixed_fixnum(pos, sign, result);
			return 0;

		case LISPTYPE_BIGNUM:
			GetSizeBignum(pos, &check);
			if (check != 1) return 1;
			GetSignBignum(pos, sign);
			getfixed_bignum(pos, 0, result);
			return 0;

		default:
			return 1;
	}
}

_g int equal_fb_real(addr left, addr right)
{
	int sign1, sign2;
	bigtype check1, check2;
	fixnum value;
	addr root;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (1 < RefSizeBignum(right)) return 0;
	GetFixnum(left, &value);
	GetRootBignum(right, &root);
	check2 = PtrDataBignum(root)[0];
	if (value == 0 && check2 == 0) return 1;
	GetSignBignum(right, &sign2);
	castfixed(value, &sign1, &check1);

	return sign1 == sign2 && check1 == check2;
}

_g int equal_bb_real(addr left, addr right)
{
	addr root;
	size_t size1, size2;
	bigtype check1, check2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	if (size1 == 1 && size2 == 1) {
		GetRootBignum(left, &root);
		check1 = PtrDataBignum(root)[0];
		GetRootBignum(right, &root);
		check2 = PtrDataBignum(root)[0];
		return (check1 == 0 && check2 == 0)
			|| ((RefSignBignum(left) == RefSignBignum(right)) && (check1 == check2));
	}

	if (RefSignBignum(left) != RefSignBignum(right)) return 0;
	if (size1 != size2) return 0;
	GetRootBignum(left, &left);
	GetRootBignum(right, &right);
	return bigcmp(PtrDataBignum(left), PtrDataBignum(right), size1) == 0;
}

_g int equal_nosign_bignum(addr left, addr right)
{
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	if (size1 != size2) return 0;
	GetRootBignum(left, &left);
	GetRootBignum(right, &right);
	return bigcmp(PtrDataBignum(left), PtrDataBignum(right), size1) == 0;
}

static int compare_bigtype(int sign1, bigtype check1, int sign2, bigtype check2)
{
	if (check1 == 0 && check2 == 0)
		return 0;
	if (sign1 != sign2)
		return IsPlus(sign1)? 1: -1;
	if (IsPlus(sign1)) {
		if (check1 < check2) return -1;
		if (check1 > check2) return 1;
	}
	else {
		if (check1 < check2) return 1;
		if (check1 > check2) return -1;
	}

	return 0;
}

_g int compare_value_bignum(fixnum left, addr right)
{
	int sign1, sign2;
	bigtype check1, check2;

	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (1 < RefSizeBignum(right))
		return IsPlus(RefSignBignum(right))? -1: 1;

	castfixed(left, &sign1, &check1);
	GetSignBignum(right, &sign2);
	GetRootBignum(right, &right);
	check2 = PtrDataBignum(right)[0];

	return compare_bigtype(sign1, check1, sign2, check2);
}

_g int compare_bignum_value(addr value, fixnum right)
{
	return -compare_value_bignum(right, value);
}

_g int compare_fb_real(addr left, addr right)
{
	int sign1, sign2;
	bigtype check1, check2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (1 < RefSizeBignum(right))
		return IsPlus(RefSignBignum(right))? -1: 1;

	castfixed_fixnum(left, &sign1, &check1);
	GetSignBignum(right, &sign2);
	GetRootBignum(right, &right);
	check2 = PtrDataBignum(right)[0];

	return compare_bigtype(sign1, check1, sign2, check2);
}

_g int compare_bf_real(addr left, addr right)
{
	return -(compare_fb_real(right, left));
}

_g int compare_bb_real(addr left, addr right)
{
	int sign1, sign2;

	if (zerop_bignum(left)) {
		if (zerop_bignum(right)) return 0;
		GetSignBignum(right, &sign2);
		return IsPlus(sign2)? -1: 1;
	}
	if (zerop_bignum(right)) {
		GetSignBignum(left, &sign1);
		return IsPlus(sign1)? 1: -1;
	}

	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 != sign2)
		return IsPlus(sign1)? 1: -1;

	sign2 = compare_bigdata(left, right);
	return IsPlus(sign1)? sign2: -sign2;
}

_g int equal_value_nosign_bignum(addr left, bigtype value)
{
	if (RefSizeBignum(left) != 1) return 0;
	GetRootBignum(left, &left);
	return PtrDataBignum(left)[0] == value;
}

_g int equal_value_bignum(addr left, int sign1, bigtype value1)
{
	int sign2;
	addr root;
	bigtype value2;

	if (RefSizeBignum(left) != 1) return 0;
	GetRootBignum(left, &root);
	value2 = PtrDataBignum(root)[0];
	if (value1 == 0 && value2 == 0) return 1;
	GetSignBignum(left, &sign2);
	if (sign1 != sign2) return 0;

	return value1 == value2;
}

_g int equal_value2_nosign_bignum(addr left, bigtype high, bigtype low)
{
	addr root;
	bigtype *data;

	if (high == 0)
		return equal_value_nosign_bignum(left, low);
	if (RefSizeBignum(left) != 2) return 0;
	GetRootDataBignum(left, &root, &data);

	return data[0] == low && data[1] == high;
}

_g int equal_value2_bignum(addr left, int sign1, bigtype high, bigtype low)
{
	int sign2;
	addr root;
	bigtype *data;

	if (high == 0)
		return equal_value_bignum(left, sign1, low);
	if (RefSizeBignum(left) != 2) return 0;
	GetRootDataBignum(left, &root, &data);
	if (data[0] != low || data[1] != high) return 0;
	GetSignBignum(left, &sign2);

	return sign1 == sign2;
}

_g int compare_bs_real(addr left, addr right)
{
	float value1, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value1 = single_float_bignum(left);
	GetSingleFloat(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;

	return 0;
}

_g int compare_bd_real(addr left, addr right)
{
	double_float value1, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value1 = double_float_bignum(left);
	GetDoubleFloat(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;

	return 0;
}

_g int compare_bl_real(addr left, addr right)
{
	long_float value1, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value1 = long_float_bignum(left);
	GetLongFloat(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;

	return 0;
}

_g int compare_sb_real(addr left, addr right)
{
	return -compare_bs_real(right, left);
}

_g int compare_db_real(addr left, addr right)
{
	return -compare_bd_real(right, left);
}

_g int compare_lb_real(addr left, addr right)
{
	return -compare_bl_real(right, left);
}

/* compare byte */
_g int fixnum_unsigned_byte_p(addr value, size_t size)
{
	fixnum check;

	Check(size == 0, "unsigned-byte error");
	GetFixnum(value, &check);
	if (check < 0) return 0;
	if (BIGNUM_FULLBIT <= size) return 1;

	return ((bigtype)check) < (1ULL << ((bigtype)size));
}

_g int bignum_unsigned_byte_p(addr value, size_t size)
{
	addr root;
	bigtype *data, left, right;
	size_t m, n;

	Check(size == 0, "unsigned-byte error");
	if (zerop_bignum(value)) return 1;
	if (IsMinus(RefSignBignum(value))) return 0;
	n = size / BIGNUM_FULLBIT;
	GetSizeBignum(value, &m);
	m--;
	if (m < n) return 1;
	if (n < m) return 0;
	GetRootDataBignum(value, &root, &data);
	left = data[n];
	m = size % BIGNUM_FULLBIT;
	right = (bigtype)(1ULL << m);

	return left < right;
}

_g int fixnum_signed_byte_p(addr value, size_t size)
{
	fixnum check;

	Check(size == 0, "signed-byte error");
	if (BIGNUM_FULLBIT <= size) return 1;
	GetFixnum(value, &check);
	size = 1ULL << ((bigtype)(size - 1ULL));
	if (0 <= check)
		return ((bigtype)check) < size;
	else
		return ((bigtype)-check) <= size;
}

_g int bignum_signed_byte_p(addr value, size_t size)
{
	addr root;
	bigtype *data, left, right;
	size_t m, n;

	Check(size == 0, "signed-byte error");
	if (zerop_bignum(value)) return 1;
	if (IsPlus(RefSignBignum(value))) {
		if (size <= 1) return 0;
		return bignum_unsigned_byte_p(value, size - 1);
	}
	size--;
	n = size / BIGNUM_FULLBIT;
	GetSizeBignum(value, &m);
	m--;
	if (m < n) return 1;
	if (n < m) return 0;
	GetRootDataBignum(value, &root, &data);
	left = data[n];
	m = size % BIGNUM_FULLBIT;
	right = (bigtype)(1ULL << m);
	if (left < right) return 1;
	if (right < left) return 0;
	if (n == 0) return 1;
	for (m = 0; m < n; m++) {
		if (data[m]) return 0;
	}

	return 1;
}

_g int getfixnum_bignum(addr pos, fixnum *ret)
{
	int sign;
	bigtype value;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	if (RefSizeBignum(pos) != 1) return 1;
	GetSignBignum(pos, &sign);
	getfixed_bignum(pos, 0, &value);
	if (IsPlus(sign)) {
		if (FIXNUM_MAX < value) return 1;
		*ret = (fixnum)value;
		return 0;
	}
	else {
		if (FIXNUM_UMIN < value) return 1;
		*ret = -(fixnum)value;
		return 0;
	}

	return 1;
}

_g int getfixnumtype(addr pos, fixnum *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			return getfixnum_bignum(pos, ret);

		default:
			break;
	}

	return 1;
}

_g int getfixed1_bignum(addr pos, int *sign, fixed *ret)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	if (RefSizeBignum(pos) != 1) return 1;
	GetSignBignum(pos, sign);
	getfixed_bignum(pos, 0, ret);

	return 0;
}


/*****************************************************************************
  plus
 *****************************************************************************/
static void plusvalue(LocalRoot local, addr left, bigtype value, addr *ret)
{
	int sign;
	size_t size;

	GetSignBignum(left, &sign);
	GetSizeBignum(left, &size);
	if (IsPlus(sign)) {
		alloc_bignum(local, ret, size + 1);
		setplusvalue_bigdata(*ret, left, sign, value);
	}
	else {
		alloc_bignum(local, ret, size);
		setminusvalue_bigdata(*ret, left, sign, value);
	}
}

static void minusvalue(LocalRoot local, addr left, bigtype value, addr *ret)
{
	int sign;
	size_t size;

	GetSignBignum(left, &sign);
	GetSizeBignum(left, &size);
	if (IsPlus(sign)) {
		alloc_bignum(local, ret, size);
		setminusvalue_bigdata(*ret, left, sign, value);
	}
	else {
		alloc_bignum(local, ret, size + 1);
		setplusvalue_bigdata(*ret, left, sign, value);
	}
}

#define plus_vv_overflow(v1, v2) \
	(((v2) > 0 && (v1) > (FIXNUM_MAX - (v2))) || \
	 ((v2) < 0 && (v1) < (FIXNUM_MIN - (v2))))

static inline void plus_vv_bignum_local(LocalRoot local,
		fixnum v1, fixnum v2, addr *ret)
{
	int sign;
	bigtype fixed1, fixed2;

	if (plus_vv_overflow(v1, v2)) {
		castfixed(v1, &sign, &fixed1);
		castfixed(v2, &sign, &fixed2);
		plusnumber_bigdata(&fixed1, &fixed2);
		if (fixed2)
			carryvalue_alloc(local, ret, sign, fixed1);
		else
			bignum_value_local(local, ret, sign, fixed1);
	}
	else {
		castfixed(v1 + v2, &sign, &fixed1);
		bignum_value_local(local, ret, sign, fixed1);
	}
}

static inline void plus_vv_real_alloc(LocalRoot local, fixnum v1, fixnum v2, addr *ret)
{
	int sign;
	bigtype fixed1, fixed2;

	if (plus_vv_overflow(v1, v2)) {
		castfixed(v1, &sign, &fixed1);
		castfixed(v2, &sign, &fixed2);
		plusnumber_bigdata(&fixed1, &fixed2);
		if (fixed2)
			carryvalue_alloc(local, ret, sign, fixed1);
		else
			bignum_value_alloc(local, ret, sign, fixed1);
	}
	else {
		fixnum_alloc(local, ret, v1 + v2);
	}
}

_g void plus_fv_bignum_local(LocalRoot local, addr left, fixnum value2, addr *ret)
{
	fixnum value1;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");

	/* left + 0 */
	GetFixnum(left, &value1);
	if (value2 == 0) {
		bignum_fixnum_value_local(local, ret, value1);
		return;
	}

	/* 0 + right */
	if (value1 == 0) {
		bignum_fixnum_value_local(local, ret, value2);
		return;
	}

	plus_vv_bignum_local(local, value1, value2, ret);
}

static void plus_fv_real_alloc(LocalRoot local, addr left, fixnum value2, addr *ret)
{
	fixnum value1;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");

	/* left + 0 */
	if (value2 == 0) {
		fixnum_throw_alloc(local, left, ret);
		return;
	}

	/* 0 + right */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		fixnum_alloc(local, ret, value2);
		return;
	}

	plus_vv_real_alloc(local, value1, value2, ret);
}

_g void plus_fv_real_local(LocalRoot local, addr left, fixnum value2, addr *ret)
{
	Check(local == NULL, "local error");
	plus_fv_real_alloc(local, left, value2, ret);
}

_g void plus_fv_real_common(addr left, fixnum value2, addr *ret)
{
	plus_fv_real_alloc(NULL, left, value2, ret);
}

_g void plus_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	GetFixnum(left, &value1);
	GetFixnum(right, &value2);
	if (value1 == 0) {
		bignum_fixnum_value_local(local, ret, value2);
		return;
	}

	/* left + 0 */
	if (value2 == 0) {
		bignum_fixnum_value_local(local, ret, value1);
		return;
	}

	plus_vv_bignum_local(local, value1, value2, ret);
}

static void plus_ff_real_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		fixnum_throw_alloc(local, right, ret);
		return;
	}

	/* left + 0 */
	GetFixnum(right, &value2);
	if (value2 == 0) {
		fixnum_throw_alloc(local, left, ret);
		return;
	}

	plus_vv_real_alloc(local, value1, value2, ret);
}

_g void plus_ff_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_ff_real_alloc(local, left, right, ret);
}

_g void plus_ff_real_common(addr left, addr right, addr *ret)
{
	plus_ff_real_alloc(NULL, left, right, ret);
}

static inline void plusfixnum_bignum_local(LocalRoot local,
		addr left, fixnum right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	castfixed(right, &sign, &value);
	if (IsPlus(sign)) {
		plusvalue(local, left, value, ret);
	}
	else {
		minusvalue(local, left, value, ret);
	}
}

static inline void plusfixnum_real_local(LocalRoot local,
		addr left, fixnum right, addr *ret)
{
	plusfixnum_bignum_local(local, left, right, &left);
	bignum_result_local(local, left, ret);
}

static inline void plusfixnum_real_common(LocalRoot local,
		addr left, fixnum right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	plusfixnum_bignum_local(local, left, right, &left);
	bignum_result_heap(left, ret);
	rollback_local(local, stack);
}

_g void plus_bv_bignum_local(LocalRoot local, addr left, fixnum right, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");

	/* left + 0 */
	if (right == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	/* 0 + right */
	if (zerop_bignum(left)) {
		bignum_fixnum_value_local(local, ret, right);
		return;
	}

	plusfixnum_bignum_local(local, left, right, ret);
}

_g void plus_bv_real_local(LocalRoot local, addr left, fixnum right, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");

	/* left + 0 */
	if (right == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	/* 0 + right */
	if (zerop_bignum(left)) {
		fixnum_local(local, ret, right);
		return;
	}

	plusfixnum_real_local(local, left, right, ret);
}

_g void plus_bv_real_common(LocalRoot local, addr left, fixnum right, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");

	/* left + 0 */
	if (right == 0) {
		bignum_throw_heap(left, ret);
		return;
	}

	/* 0 + right */
	if (zerop_bignum(left)) {
		fixnum_heap(ret, right);
		return;
	}

	plusfixnum_real_common(local, left, right, ret);
}

_g void plus_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	if (zerop_bignum(left)) {
		bignum_fixnum_local(local, ret, right);
		return;
	}

	/* left + 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	plusfixnum_bignum_local(local, left, check, ret);
}

_g void plus_bf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	if (zerop_bignum(left)) {
		fixnum_throw_local(local, right, ret);
		return;
	}

	/* left + 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	plusfixnum_real_local(local, left, check, ret);
}

_g void plus_bf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	if (zerop_bignum(left)) {
		fixnum_throw_heap(right, ret);
		return;
	}

	/* left + 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_heap(left, ret);
		return;
	}

	plusfixnum_real_common(local, left, check, ret);
}

_g void plus_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		bignum_throw_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 == sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		*ret = left;
		return;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		bignum_value_alloc(local, ret, SignPlus, 0);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = sign2;
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);
	*ret = left;
}

_g void plus_bb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		bignum_throw_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 == sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		goto finish;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = sign2;
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);

finish:
	bignum_result_local(local, left, ret);
}

_g void plus_bb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		bignum_throw_heap(right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_heap(left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 == sign2) {
		push_local(local, &stack);
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		goto finish;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		fixnum_heap(ret, 0);
		return;
	}
	push_local(local, &stack);
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = sign2;
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);

finish:
	bignum_result_heap(left, ret);
	rollback_local(local, stack);
}


/*****************************************************************************
  minus
 *****************************************************************************/
_g void sigrev_bignum_inplace(addr pos)
{
	int sign;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	GetSignBignum(pos, &sign);
	sign = SignNot(sign);
	SetSignBignum(pos, sign);
}

_g void sigrev_fixnum_bignum_local(LocalRoot local, addr left, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");
	castfixed_fixnum(left, &sign, &value);
	bignum_value_local(local, ret, SignNot(sign), value);
}

static void inline sigrev_fixnum_integer_alloc(LocalRoot local, addr left, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");
	GetFixnum(left, &value);
	if (value == FIXNUM_MIN) {
		bignum_value_alloc(local, ret, SignPlus, FIXNUM_UMIN);
	}
	else {
		fixnum_alloc(local, ret, -value);
	}
}

_g void sigrev_fixnum_integer_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	sigrev_fixnum_integer_alloc(local, left, ret);
}

_g void sigrev_fixnum_integer_common(addr left, addr *ret)
{
	sigrev_fixnum_integer_alloc(NULL, left, ret);
}

_g void sigrev_bignum_bignum_local(LocalRoot local, addr left, addr *ret)
{
	int sign;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");
	GetSignBignum(left, &sign);
	bignum_copy_local(local, &left, left);
	SetSignBignum(left, SignNot(sign));
	*ret = left;
}

static void inline sigrev_bignum_integer_alloc(LocalRoot local, addr left, addr *ret)
{
	int sign;
	addr root;
	bigtype *data, value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");
	GetSignBignum(left, &sign);
	if (RefSizeBignum(left) != 1) {
		bignum_copy_nosign_alloc(local, &left, left);
		SetSignBignum(left, SignNot(sign));
		*ret = left;
		return;
	}

	GetRootDataBignum(left, &root, &data);
	value = data[0];
	if (IsPlus(sign)) {
		if (value <= FIXNUM_UMIN)
			fixnum_alloc(local, ret, -(fixnum)value);
		else
			bignum_value_alloc(local, ret, SignMinus, value);
	}
	else {
		if (value <= FIXNUM_MAX)
			fixnum_alloc(local, ret, (fixnum)value);
		else
			bignum_value_alloc(local, ret, SignPlus, value);
	}
}

_g void sigrev_bignum_integer_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	sigrev_bignum_integer_alloc(local, left, ret);
}

_g void sigrev_bignum_integer_common(addr left, addr *ret)
{
	sigrev_bignum_integer_alloc(NULL, left, ret);
}

#define minus_vv_overflow(v1, v2) \
	(((v2) > 0 && (v1) < (FIXNUM_MIN + (v2))) || \
	 ((v2) < 0 && (v1) > (FIXNUM_MAX + (v2))))

static inline void minus_vv_bignum_local(LocalRoot local,
		fixnum v1, fixnum v2, addr *ret)
{
	int sign;
	bigtype fixed1, fixed2;

	if (minus_vv_overflow(v1, v2)) {
		castfixed(v2, &sign, &fixed2);
		castfixed(v1, &sign, &fixed1);
		bignum_value_local(local, ret, sign, fixed1 + fixed2);
	}
	else {
		castfixed(v1 - v2, &sign, &fixed1);
		bignum_value_local(local, ret, sign, fixed1);
	}
}

static inline void minus_vv_real_alloc(LocalRoot local,
		fixnum v1, fixnum v2, addr *ret)
{
	int sign;
	bigtype fixed1, fixed2;

	if (minus_vv_overflow(v1, v2)) {
		castfixed(v2, &sign, &fixed2);
		castfixed(v1, &sign, &fixed1);
		bignum_value_alloc(local, ret, sign, fixed1 + fixed2);
	}
	else {
		fixnum_alloc(local, ret, v1 - v2);
	}
}

_g void minus_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		sigrev_fixnum_bignum_local(local, right, ret);
		return;
	}

	/* left - 0 */
	GetFixnum(right, &value2);
	if (value2 == 0) {
		fixnum_throw_local(local, left, ret);
		return;
	}

	minus_vv_bignum_local(local, value1, value2, ret);
}

static void minus_ff_real_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		sigrev_fixnum_integer_alloc(local, right, ret);
		return;
	}

	/* left - 0 */
	GetFixnum(right, &value2);
	if (value2 == 0) {
		fixnum_throw_alloc(local, left, ret);
		return;
	}

	minus_vv_real_alloc(local, value1, value2, ret);
}

_g void minus_ff_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_ff_real_alloc(local, left, right, ret);
}

_g void minus_ff_real_common(addr left, addr right, addr *ret)
{
	minus_ff_real_alloc(NULL, left, right, ret);
}

static inline void minusfixnum_local(LocalRoot local,
		addr left, fixnum right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	castfixed(right, &sign, &value);
	if (IsPlus(sign)) {
		minusvalue(local, left, value, ret);
	}
	else {
		plusvalue(local, left, value, ret);
	}
}

_g void minus_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixnum check;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	GetFixnum(right, &check);
	if (zerop_bignum(left)) {
		castfixed(check, &sign, &value);
		bignum_value_local(local, ret, SignNot(sign), value);
		return;
	}

	/* left - 0 */
	if (check == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	minusfixnum_local(local, left, check, ret);
}

_g void minus_bf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	if (zerop_bignum(left)) {
		sigrev_fixnum_integer_local(local, right, ret);
		return;
	}

	/* left - 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	minusfixnum_local(local, left, check, &left);
	bignum_result_local(local, left, ret);
}

_g void minus_bf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	if (zerop_bignum(left)) {
		sigrev_fixnum_integer_common(right, ret);
		return;
	}

	/* left - 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_heap(left, ret);
		return;
	}

	push_local(local, &stack);
	minusfixnum_local(local, left, check, &left);
	bignum_result_heap(left, ret);
	rollback_local(local, stack);
}

_g void minus_fb_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &check);
	if (check == 0) {
		sigrev_bignum_bignum_local(local, right, ret);
		return;
	}

	/* left - 0 */
	if (zerop_bignum(right)) {
		bignum_fixnum_value_local(local, ret, check);
		return;
	}

	/* fixnum - bignum = -(bignum - fixnum) */
	minusfixnum_local(local, right, check, &right);
	sigrev_bignum_inplace(right);
	*ret = right;
}

_g void minus_fb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &check);
	if (check == 0) {
		sigrev_bignum_integer_local(local, right, ret);
		return;
	}

	/* left - 0 */
	if (zerop_bignum(right)) {
		fixnum_throw_local(local, left, ret);
		return;
	}

	/* fixnum - bignum = -(bignum - fixnum) */
	minusfixnum_local(local, right, check, &right);
	sigrev_bignum_inplace(right);
	bignum_result_local(local, right, ret);
}

_g void minus_fb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &check);
	if (check == 0) {
		sigrev_bignum_integer_common(right, ret);
		return;
	}

	/* left - 0 */
	if (zerop_bignum(right)) {
		fixnum_throw_heap(left, ret);
		return;
	}

	/* fixnum - bignum = -(bignum - fixnum) */
	push_local(local, &stack);
	minusfixnum_local(local, right, check, &right);
	sigrev_bignum_inplace(right);
	bignum_result_heap(right, ret);
	rollback_local(local, stack);
}

_g void minus_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		sigrev_bignum_bignum_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 != sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		*ret = left;
		return;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		bignum_zero_local(local, ret);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = SignNot(sign2);
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);
	*ret = left;
}

_g void minus_bb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		sigrev_bignum_integer_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 != sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		goto finish;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = SignNot(sign2);
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);

finish:
	bignum_result_local(local, left, ret);
}

_g void minus_bb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		sigrev_bignum_integer_common(right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_heap(left, ret);
		return;
	}
	push_local(local, &stack);
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 != sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		goto finish;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		fixnum_heap(ret, 0);
		rollback_local(local, stack);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = SignNot(sign2);
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);

finish:
	bignum_result_heap(left, ret);
	rollback_local(local, stack);
}


/*****************************************************************************
  multiple
 *****************************************************************************/
static inline int multisafe_fixnum(fixnum c1, fixnum c2, fixnum *ret)
{
	if (c1 > 0) {
		if (c2 > 0) {
			if (c1 > (FIXNUM_MAX / c2)) return 1;
		} else {
			if (c2 < (FIXNUM_MIN / c1)) return 1;
		}
	} else {
		if (c2 > 0) {
			if (c1 < (FIXNUM_MIN / c2)) return 1;
		} else {
			if ((c1 != 0) && (c2 < (FIXNUM_MAX / c1))) return 1;
		}
	}
	*ret = c1 * c2;

	return 0;
}

_g void multi_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixnum value1, value2, value;
	bigtype fixed1;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* left */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		bignum_zero_local(local, ret);
		return;
	}
	if (value1 == 1) {
		bignum_fixnum_local(local, ret, right);
		return;
	}
	if (value1 == -1) {
		castfixed_fixnum(right, &sign, &fixed1);
		bignum_value_local(local, ret, SignNot(sign), fixed1);
		return;
	}

	/* right */
	GetFixnum(right, &value2);
	if (value2 == 0) {
		bignum_zero_local(local, ret);
		return;
	}
	if (value2 == 1) {
		bignum_fixnum_value_local(local, ret, value1);
		return;
	}
	if (value2 == -1) {
		castfixed(value1, &sign, &fixed1);
		bignum_value_local(local, ret, SignNot(sign), fixed1);
		return;
	}

	/* multiple */
	if (! multisafe_fixnum(value1, value2, &value)) {
		bignum_fixnum_value_local(local, ret, value);
		return;
	}
	multicarry_bignum(local, value1, value2, ret);
}

static void multi_ff_real_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2, value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* left */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		fixnum_throw_alloc(local, left, ret);
		return;
	}
	if (value1 == 1) {
		fixnum_throw_alloc(local, right, ret);
		return;
	}
	if (value1 == -1) {
		sigrev_fixnum_integer_alloc(local, right, ret);
		return;
	}

	/* right */
	GetFixnum(right, &value2);
	if (value2 == 0) {
		fixnum_throw_alloc(local, right, ret);
		return;
	}
	if (value2 == 1) {
		fixnum_throw_alloc(local, left, ret);
		return;
	}
	if (value2 == -1) {
		sigrev_fixnum_integer_alloc(local, left, ret);
		return;
	}

	/* multiple */
	if (! multisafe_fixnum(value1, value2, &value)) {
		fixnum_alloc(local, ret, value);
		return;
	}
	multicarry_fixnum(local, value1, value2, ret);
}

_g void multi_ff_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_ff_real_alloc(local, left, right, ret);
}

_g void multi_ff_real_common(addr left, addr right, addr *ret)
{
	multi_ff_real_alloc(NULL, left, right, ret);
}

_g void multi_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	addr pos, root;
	fixnum check;
	fixed value;
	size_t size;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* right */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_zero_local(local, ret);
		return;
	}
	if (check == 1) {
		bignum_throw_local(local, left, ret);
		return;
	}
	if (check == -1) {
		sigrev_bignum_bignum_local(local, left, ret);
		return;
	}

	/* left */
	GetSignBignum(left, &sign1);
	GetSizeBignum(left, &size);
	if (size == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, left, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign1))
				bignum_fixnum_local(local, ret, right);
			else
				sigrev_fixnum_bignum_local(local, right, ret);
			return;
		}
	}

	/* multi */
	alloc_bignum(local, &pos, size + 1);
	castfixed(check, &sign2, &value);
	setmultivalue_bigdata(pos, left, value);
	SetSignBignum(pos, SignMulti(sign1, sign2));
	*ret = pos;
}

_g void multi_bf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	addr pos, root;
	fixnum check;
	fixed value;
	size_t size;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* right */
	GetFixnum(right, &check);
	if (check == 0) {
		fixnum_throw_local(local, right, ret);
		return;
	}
	if (check == 1) {
		bignum_throw_local(local, left, ret);
		return;
	}
	if (check == -1) {
		sigrev_bignum_integer_local(local, left, ret);
		return;
	}

	/* left */
	GetSignBignum(left, &sign1);
	GetSizeBignum(left, &size);
	if (size == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, left, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign1))
				fixnum_throw_local(local, right, ret);
			else
				sigrev_fixnum_integer_local(local, right, ret);
			return;
		}
	}

	/* multi */
	alloc_bignum(local, &pos, size + 1);
	castfixed(check, &sign2, &value);
	setmultivalue_bigdata(pos, left, value);
	SetSignBignum(pos, SignMulti(sign1, sign2));
	bignum_result_local(local, pos, ret);
}

_g void multi_bf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	addr pos, root;
	fixnum check;
	fixed value;
	size_t size;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* right */
	GetFixnum(right, &check);
	if (check == 0) {
		fixnum_throw_heap(right, ret);
		return;
	}
	if (check == 1) {
		bignum_throw_heap(left, ret);
		return;
	}
	if (check == -1) {
		sigrev_bignum_integer_common(left, ret);
		return;
	}

	/* left */
	GetSignBignum(left, &sign1);
	GetSizeBignum(left, &size);
	if (size == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_heap(left, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign1))
				fixnum_throw_heap(right, ret);
			else
				sigrev_fixnum_integer_common(right, ret);
			return;
		}
	}

	/* multi */
	push_local(local, &stack);
	alloc_bignum(local, &pos, size + 1);
	castfixed(check, &sign2, &value);
	setmultivalue_bigdata(pos, left, value);
	SetSignBignum(pos, SignMulti(sign1, sign2));
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

_g void multi_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	int sign1, sign2;
	size_t size1, size2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* left */
	GetSignBignum(left, &sign1);
	GetSizeBignum(left, &size1);
	if (size1 == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, left, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign1))
				bignum_throw_local(local, right, ret);
			else
				sigrev_bignum_bignum_local(local, right, ret);
			return;
		}
	}

	/* right */
	GetSizeBignum(right, &size2);
	GetSignBignum(right, &sign2);
	if (size2 == 1) {
		GetRootBignum(right, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, right, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign2))
				bignum_throw_local(local, left, ret);
			else
				sigrev_bignum_bignum_local(local, left, ret);
			return;
		}
	}

	/* multi */
	alloc_bignum(local, &root, size1 + size2 + 1);
	setmulti_bigdata(root, left, right);
	SetSignBignum(root, SignMulti(sign1, sign2));
	*ret = root;
}

_g void multi_bb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	int sign1, sign2;
	size_t size1, size2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* left */
	GetSignBignum(left, &sign1);
	GetSizeBignum(left, &size1);
	if (size1 == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, left, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign1))
				bignum_throw_local(local, right, ret);
			else
				sigrev_bignum_integer_local(local, right, ret);
			return;
		}
	}

	/* right */
	GetSizeBignum(right, &size2);
	GetSignBignum(right, &sign2);
	if (size2 == 1) {
		GetRootBignum(right, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, right, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign2))
				bignum_throw_local(local, left, ret);
			else
				sigrev_bignum_integer_local(local, left, ret);
			return;
		}
	}

	/* multi */
	alloc_bignum(local, &root, size1 + size2 + 1);
	setmulti_bigdata(root, left, right);
	SetSignBignum(root, SignMulti(sign1, sign2));
	bignum_result_local(local, root, ret);
}

_g void multi_bb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	int sign1, sign2;
	size_t size1, size2;
	bigtype value;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* left */
	GetSignBignum(left, &sign1);
	GetSizeBignum(left, &size1);
	if (size1 == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_heap(left, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign1))
				bignum_throw_heap(right, ret);
			else
				sigrev_bignum_integer_common(right, ret);
			return;
		}
	}

	/* right */
	GetSizeBignum(right, &size2);
	GetSignBignum(right, &sign2);
	if (size2 == 1) {
		GetRootBignum(right, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_heap(right, ret);
			return;
		}
		if (value == 1) {
			if (IsPlus(sign2))
				bignum_throw_heap(left, ret);
			else
				sigrev_bignum_integer_common(left, ret);
			return;
		}
	}

	/* multi */
	push_local(local, &stack);
	alloc_bignum(local, &root, size1 + size2 + 1);
	setmulti_bigdata(root, left, right);
	SetSignBignum(root, SignMulti(sign1, sign2));
	bignum_result_heap(root, ret);
	rollback_local(local, stack);
}

_g void multi_bb_nosign_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	size_t size1, size2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* left */
	GetSizeBignum(left, &size1);
	if (size1 == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, left, ret);
			return;
		}
		if (value == 1) {
			bignum_throw_local(local, right, ret);
			return;
		}
	}

	/* right */
	GetSizeBignum(right, &size2);
	if (size2 == 1) {
		GetRootBignum(right, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, right, ret);
			return;
		}
		if (value == 1) {
			bignum_throw_local(local, left, ret);
			return;
		}
	}

	/* multi */
	alloc_bignum(local, &root, size1 + size2 + 1);
	setmulti_bigdata(root, left, right);
	*ret = root;
}

_g void multi_bb_nosign_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	size_t size1, size2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* left */
	GetSizeBignum(left, &size1);
	if (size1 == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, left, ret);
			return;
		}
		if (value == 1) {
			bignum_throw_local(local, right, ret);
			return;
		}
	}

	/* right */
	GetSizeBignum(right, &size2);
	if (size2 == 1) {
		GetRootBignum(right, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_local(local, right, ret);
			return;
		}
		if (value == 1) {
			bignum_throw_local(local, left, ret);
			return;
		}
	}

	/* multi */
	alloc_bignum(local, &root, size1 + size2 + 1);
	setmulti_bigdata(root, left, right);
	bignum_result_local(local, root, ret);
}

_g void multi_bb_nosign_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	size_t size1, size2;
	bigtype value;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* left */
	GetSizeBignum(left, &size1);
	if (size1 == 1) {
		GetRootBignum(left, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_heap(left, ret);
			return;
		}
		if (value == 1) {
			bignum_throw_heap(right, ret);
			return;
		}
	}

	/* right */
	GetSizeBignum(right, &size2);
	if (size2 == 1) {
		GetRootBignum(right, &root);
		value = PtrDataBignum(root)[0];
		if (value == 0) {
			bignum_throw_heap(right, ret);
			return;
		}
		if (value == 1) {
			bignum_throw_heap(left, ret);
			return;
		}
	}

	/* multi */
	push_local(local, &stack);
	alloc_bignum(local, &root, size1 + size2 + 1);
	setmulti_bigdata(root, left, right);
	bignum_result_heap(root, ret);
	rollback_local(local, stack);
}


/*
 *  math
 */
_g void abs_fixnum_integer_local(LocalRoot local, addr left, addr *ret)
{
	fixnum value;

	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_FIXNUM);
	GetFixnum(left, &value);
	if (0 <= value)
		fixnum_throw_local(local, left, ret);
	else if (value == FIXNUM_MIN)
		bignum_value_local(local, ret, SignPlus, FIXNUM_UMIN);
	else
		fixnum_local(local, ret, -value);
}

_g void abs_fixnum_integer_common(addr left, addr *ret)
{
	fixnum value;

	CheckType(left, LISPTYPE_FIXNUM);
	GetFixnum(left, &value);
	if (0 <= value)
		fixnum_throw_heap(left, ret);
	else if (value == FIXNUM_MIN)
		bignum_value_heap(ret, SignPlus, FIXNUM_UMIN);
	else
		fixnum_heap(ret, -value);
}

_g void abs_bignum_integer_local(LocalRoot local, addr left, addr *ret)
{
	int sign;

	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_BIGNUM);
	GetSignBignum(left, &sign);
	if (IsPlus(sign))
		bignum_throw_local(local, left, ret);
	else
		bignum_copy_nosign_local(local, ret, left);
}

_g void abs_bignum_integer_common(addr left, addr *ret)
{
	int sign;

	CheckType(left, LISPTYPE_BIGNUM);
	GetSignBignum(left, &sign);
	if (IsPlus(sign))
		bignum_throw_heap(left, ret);
	else
		bignum_copy_nosign_heap(ret, left);
}


/*
 *  output
 */
_g void decimal_charqueue_fixnum_local(LocalRoot local, addr pos, addr queue)
{
	char buffer[256];

	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	snprintf(buffer, 256, "%" PRIdF, RefFixnum(pos));
	pushchar_charqueue_local(local, queue, buffer);
}

_g void decimal_charqueue_bignum_local(LocalRoot local, addr pos, addr queue)
{
	LocalStack stack;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	push_local(local, &stack);
	rollback_local(local, stack);
	Abort("TODO");
}

_g void decimal_charqueue_integer_local(LocalRoot local, addr pos, addr queue)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			decimal_charqueue_fixnum_local(local, pos, queue);
			break;

		case LISPTYPE_BIGNUM:
			decimal_charqueue_bignum_local(local, pos, queue);
			break;

		default:
			TypeError(pos, INTEGER);
			break;
	}
}


/*
 *  print
 */
/* base=2, 64bit -> 1+64+1 -> 66+padding -> 72? */
#define FIXNUM_BUFFER_SIZE  128
_g void output_nosign_fixnum(LocalRoot local,
		addr stream, fixnum value, unsigned base, int upperp)
{
	int sign;
	bigtype m, n;
	char buffer[FIXNUM_BUFFER_SIZE], *ptr, chara;

	/* zero */
	if (value == 0) {
		write_char_stream(stream, '0');
		return;
	}

	/* fixnum -> bigtype */
	castfixed(value, &sign, &n);

	/* loop */
	chara = upperp? 'A': 'a';
	ptr = buffer + FIXNUM_BUFFER_SIZE - 1;
	*(ptr--) = 0;
	while (n != 0) {
		m = n % base;
		n = n / base;
		*(ptr--) = (char)((m <= 9)? ('0' + m): (chara - 10 + m));
		Check(ptr <= buffer, "buffer error");
	}

	print_ascii_stream(stream, ptr + 1);
}

static int charbit_nil_p(addr pos)
{
	size_t size;

	if (pos == Nil) return 1;
	GetCharBitSize(pos, &size);

	return size == 0;
}

static void charqueue_nreverse(addr pos, addr *ret)
{
	addr tail, next;

	if (charbit_nil_p(pos)) {
		*ret = pos;
		return;
	}

	for (tail = Nil; ; tail = pos, pos = next) {
		GetCharBitNext(pos, &next);
		SetCharBitNext(pos, tail);
		if (charbit_nil_p(next)) break;
	}
	*ret = pos;
}

static void charqueue_nreverse_output(addr pos, addr stream)
{
	unicode *ptr;
	size_t size;

	GetCharQueueRoot(pos, &pos);
	charqueue_nreverse(pos, &pos);
	while (pos != Nil) {
		GetCharBitSize(pos, &size);
		if (size == 0) break;
		ptr = PtrCharBitChar(pos);
		do {
			size--;
			write_char_stream(stream, ptr[size]);
		}
		while (size);
		GetCharBitNext(pos, &pos);
	}
}

_g void output_nosign_bignum(LocalRoot local,
		addr stream, addr pos, unsigned base, int upperp)
{
	unicode u;
	addr queue, error_character;
	bigtype rem;
	size_t size;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(! isBaseChar(base), "base error");
	/* zero */
	if (zerop_bignum(pos)) {
		write_char_stream(stream, '0');
		return;
	}

	/* loop */
	push_local(local, &stack);
	GetSizeBignum(pos, &size);
	charqueue_local(local, &queue, size * 20); /* size * (log10(2**64)+1) */
	bignum_copy_local(local, &pos, pos);
	do {
		rem = letdiv_half_bigdata(pos, (bigtype)base);
		if (getchar_digit((unsigned)rem, upperp, &u)) {
			character_heap(&error_character, u);
			fmte("Invalid digit character ~S.", error_character, NULL);
			return;
		}
		push_charqueue_local(local, queue, u);
	}
	while (! zerop_bignum(pos));

	/* output */
	charqueue_nreverse_output(queue, stream);

	rollback_local(local, stack);
}

#define FIXNUM_BUFFER_DOUBLE_SIZE  (FIXNUM_BUFFER_SIZE * 2)
_g void output_nosign_comma_fixnum(LocalRoot local,
		addr stream, fixnum value, unsigned base, int upperp,
		size_t range, unicode comma)
{
	int sign;
	bigtype m, n;
	unicode buffer[FIXNUM_BUFFER_DOUBLE_SIZE], *ptr, chara;
	size_t index;

	Check(range < 2, "ragen error");
	/* zero */
	if (value == 0) {
		write_char_stream(stream, '0');
		return;
	}

	/* fixnum -> bigtype */
	castfixed(value, &sign, &n);

	/* loop */
	chara = upperp? 'A': 'a';
	ptr = buffer + FIXNUM_BUFFER_DOUBLE_SIZE - 1;
	*(ptr--) = 0;
	index = 0;
	while (n != 0) {
		m = n % base;
		n = n / base;
		if (index && (index % range) == 0)
			*(ptr--) = comma;
		*(ptr--) = (unicode)((m <= 9)? ('0' + m): (chara - 10 + m));
		Check(ptr <= buffer, "buffer error");
		index++;
	}

	print_unicode_stream(stream, ptr + 1);
}

_g void output_nosign_comma_bignum(LocalRoot local,
		addr stream, addr pos, unsigned base, int upperp,
		size_t range, unicode comma)
{
	unicode u;
	addr queue, error_character;
	bigtype rem;
	size_t size, index;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(! isBaseChar(base), "base error");
	Check(range < 2, "ragen error");
	/* zero */
	if (zerop_bignum(pos)) {
		write_char_stream(stream, '0');
		return;
	}

	/* loop */
	push_local(local, &stack);
	GetSizeBignum(pos, &size);
	charqueue_local(local, &queue, size * 20); /* size * (log10(2**64)+1) */
	bignum_copy_local(local, &pos, pos);
	index = 0;
	do {
		rem = letdiv_half_bigdata(pos, (bigtype)base);
		if (getchar_digit((unsigned)rem, upperp, &u)) {
			character_heap(&error_character, u);
			fmte("Invalid digit character ~S.", error_character, NULL);
			return;
		}
		if (index && (index % range) == 0)
			push_charqueue_local(local, queue, comma);
		push_charqueue_local(local, queue, u);
		index++;
	}
	while (! zerop_bignum(pos));

	/* output */
	charqueue_nreverse_output(queue, stream);

	rollback_local(local, stack);
}

