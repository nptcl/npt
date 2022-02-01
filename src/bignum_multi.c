#include "bignum_data.h"
#include "bignum_object.h"
#include "bignum_plus.h"
#include "bignum_multi.h"
#include "bignum.h"
#include "memory.h"
#include "typedef.h"

/*****************************************************************************
  multiple
 *****************************************************************************/
static inline int multisafe_fixnum(fixnum c1, fixnum c2, fixnum *ret)
{
	if (c1 > 0) {
		if (c2 > 0) {
			if (c1 > (FIXNUM_MAX / c2))
				return 1;
		} else {
			if (c2 < (FIXNUM_MIN / c1))
				return 1;
		}
	} else {
		if (c2 > 0) {
			if (c1 < (FIXNUM_MIN / c2))
				return 1;
		} else {
			if ((c1 != 0) && (c2 < (FIXNUM_MAX / c1)))
				return 1;
		}
	}
	*ret = c1 * c2;

	return 0;
}

void multi_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixnum value1, value2, value;
	fixed fixed1;

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

void multi_ff_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_ff_real_alloc(local, left, right, ret);
}

void multi_ff_real_common(addr left, addr right, addr *ret)
{
	multi_ff_real_alloc(NULL, left, right, ret);
}

void multi_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
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

void multi_bf_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

void multi_bf_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

void multi_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	int sign1, sign2;
	size_t size1, size2;
	fixed value;

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

void multi_bb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	int sign1, sign2;
	size_t size1, size2;
	fixed value;

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

void multi_bb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	int sign1, sign2;
	size_t size1, size2;
	fixed value;
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

void multi_bb_nosign_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	size_t size1, size2;
	fixed value;

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

void multi_bb_nosign_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	size_t size1, size2;
	fixed value;

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

void multi_bb_nosign_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	size_t size1, size2;
	fixed value;
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

