#include "arch.h"
#include "bignum_cons.h"
#include "bignum_data.h"
#include "bignum_object.h"
#include "condition.h"
#include "integer.h"
#include "local.h"
#include "memory.h"
#include "typedef.h"

/*
 *  object
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
	if (plussafe_size(a, b, &a)) {
		Abort("size error");
		*ret = NULL;
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
		if (count == 0)
			break;
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

static int bignum_counter_alloc_p_(addr index, int *ret)
{
	if (! integerp(index))
		return Result(ret, 1);

	Return(minusp_integer_(index, ret));
	if (*ret)
		return 0;

	return zerop_integer_(index, ret);
}

_g int bignum_counter_alloc_(LocalRoot local, addr *ret, addr index)
{
	int check;

	Return(bignum_counter_alloc_p_(index, &check));
	if (check) {
		*ret = Nil;
		return fmte_("The value ~S must be a positive integer.", index, NULL);
	}
	if (GetType(index) == LISPTYPE_FIXNUM)
		bignum_fixnum_value_alloc(local, ret, RefFixnum(index));
	else
		bignum_copy_alloc(local, ret, index);

	return 0;
}

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

_g int bignum_counter_debug_(LocalRoot local, addr *ret, addr index)
{
	Check(local == NULL, "local error");
	return bignum_counter_alloc_(local, ret, index);
}

_g void bignum_result_debug(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	bignum_result_alloc(local, pos, ret);
}

_g int bignum_integer_debug_(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	return bignum_integer_alloc_(local, ret, pos);
}


/*
 *  access
 */
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
	if (RefSizeBignum(pos) <= count)
		return 0;
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


/*
 *  result, throw
 */
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

_g int bignum_integer_alloc_(LocalRoot local, addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			bignum_fixnum_alloc(local, ret, pos);
			break;

		case LISPTYPE_BIGNUM:
			bignum_copy_alloc(local, ret, pos);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}

	return 0;
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


/*
 *  cast
 */
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
			if (check != 1)
				return 1;
			GetSignBignum(pos, sign);
			getfixed_bignum(pos, 0, result);
			return 0;

		default:
			return 1;
	}
}

