#include "bignum_cons.h"
#include "bignum_data.h"
#include "bignum_object.h"
#include "bignum.h"
#include "condition.h"
#include "float_object.h"
#include "local.h"
#include "typedef.h"
#include "type_table.h"

/*
 *  power
 */
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

_g int integer_copysign_alloc_(LocalRoot local, int sign, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_copysign_alloc(local, sign, RefFixnum(pos), ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_copysign_alloc(local, sign, pos, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, INTEGER);
	}

	return 0;
}

_g int integer_copysign_local_(LocalRoot local, int sign, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return integer_copysign_alloc_(local, sign, pos, ret);
}

_g int integer_copysign_heap_(int sign, addr pos, addr *ret)
{
	return integer_copysign_alloc_(NULL, sign, pos, ret);
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

_g int single_float_bignum_(addr pos, single_float *ret)
{
	char buffer[64];
	int sign;
	size_t size;
	single_float value;

	GetSizeBignum(pos, &size);
	if (size == 1) {
		GetSignBignum(pos, &sign);
		GetRootBignum(pos, &pos);
		value = (single_float)PtrDataBignum(pos)[0];
		return Result(ret, IsMinus(sign)? -value: value);
	}
	make_float_string(buffer, pos, LISP_FLOAT_SINGLE_FRACTION);

	return check_strtof_(buffer, pos, ret);
}

_g int double_float_bignum_(addr pos, double_float *ret)
{
	char buffer[64];
	int sign;
	size_t size;
	double_float value;

	GetSizeBignum(pos, &size);
	if (size == 1) {
		GetSignBignum(pos, &sign);
		GetRootBignum(pos, &pos);
		value = (double_float)PtrDataBignum(pos)[0];
		return Result(ret, IsMinus(sign)? -value: value);
	}
	make_float_string(buffer, pos, LISP_FLOAT_DOUBLE_FRACTION);

	return check_strtod_(buffer, pos, ret);
}

_g int long_float_bignum_(addr pos, long_float *ret)
{
	char buffer[64];
	int sign;
	size_t size;
	long_float value;

	GetSizeBignum(pos, &size);
	if (size == 1) {
		GetSignBignum(pos, &sign);
		GetRootBignum(pos, &pos);
		value = (long_float)PtrDataBignum(pos)[0];
		return Result(ret, IsMinus(sign)? -value: value);
	}
	make_float_string(buffer, pos, LISP_FLOAT_LONG_FRACTION);

	return check_strtold_(buffer, pos, ret);
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

_g int single_float_bignum_alloc_(LocalRoot local, addr *ret, addr pos)
{
	single_float value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(single_float_bignum_(pos, &value));
	single_float_alloc(local, ret, value);

	return 0;
}
_g int single_float_bignum_local_(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return single_float_bignum_alloc_(local, ret, pos);
}
_g int single_float_bignum_heap_(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return single_float_bignum_alloc_(NULL, ret, pos);
}

_g int double_float_bignum_alloc_(LocalRoot local, addr *ret, addr pos)
{
	double_float value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(double_float_bignum_(pos, &value));
	double_float_alloc(local, ret, value);

	return 0;
}
_g int double_float_bignum_local_(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return double_float_bignum_alloc_(local, ret, pos);
}
_g int double_float_bignum_heap_(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return double_float_bignum_alloc_(NULL, ret, pos);
}

_g int long_float_bignum_alloc_(LocalRoot local, addr *ret, addr pos)
{
	long_float value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(long_float_bignum_(pos, &value));
	long_float_alloc(local, ret, value);

	return 0;
}
_g int long_float_bignum_local_(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return long_float_bignum_alloc_(local, ret, pos);
}
_g int long_float_bignum_heap_(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	return long_float_bignum_alloc_(NULL, ret, pos);
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

_g void bignum_single_float_unsafe(
		LocalRoot local, single_float v, int is_heap, addr *ret)
{
	int expn, sign;
	addr pos;
	char *ptr;
	size_t size;

	Check(! IsIntegerFloat(v), "float error");
	sign = (v < 0.0f)? SignMinus: SignPlus;
	frexpf(v, &expn);
	Check(expn < 0, "exponent error");
	size = printf_integer_float_size(expn);
	ptr = (char *)lowlevel_local(local, size);
	snprintf(ptr, size, "%.0f", fabsf(v));
	bigcons_char_unsafe(local, &pos, 10, ptr);
	bignum_cons_alloc(is_heap? NULL: local, ret, sign, pos);
}

_g void bignum_double_float_unsafe(
		LocalRoot local, double_float v, int is_heap, addr *ret)
{
	int expn, sign;
	addr pos;
	char *ptr;
	size_t size;

	Check(! IsIntegerDouble(v), "float error");
	sign = (v < 0.0)? SignMinus: SignPlus;
	frexp(v, &expn);
	Check(expn < 0, "exponent error");
	size = printf_integer_float_size(expn);
	ptr = (char *)lowlevel_local(local, size);
	snprintf(ptr, size, "%.0f", fabs(v));
	bigcons_char_unsafe(local, &pos, 10, ptr);
	bignum_cons_alloc(is_heap? NULL: local, ret, sign, pos);
}

_g void bignum_long_float_unsafe(
		LocalRoot local, long_float v, int is_heap, addr *ret)
{
	int expn, sign;
	addr pos;
	char *ptr;
	size_t size;

	Check(! IsIntegerLongFloat(v), "float error");
	sign = (v < 0.0L)? SignMinus: SignPlus;
	frexpl(v, &expn);
	Check(expn < 0, "exponent error");
	size = printf_integer_float_size(expn);
	ptr = (char *)lowlevel_local(local, size);
	snprintf(ptr, size, "%.0Lf", fabsl(v));
	bigcons_char_unsafe(local, &pos, 10, ptr);
	bignum_cons_alloc(is_heap? NULL: local, ret, sign, pos);
}

static int bignum_single_float_alloc_(
		LocalRoot local, single_float v, int is_heap, addr *rv, int *ret)
{
	LocalStack stack;

	/* check */
	if (! IsIntegerFloat(v)) {
		if (ret)
			return Result(ret, 1);
		else
			return fmte_("Invalid float value.", NULL);
	}

	/* cast */
	if (is_heap) {
		push_local(local, &stack);
	}
	bignum_single_float_unsafe(local, v, is_heap, rv);
	if (is_heap) {
		rollback_local(local, stack);
	}

	/* result */
	if (ret)
		return Result(ret, 0);

	return 0;
}

_g int bignum_single_float_local_(LocalRoot local, single_float v, addr *rv, int *ret)
{
	CheckLocal(local);
	return bignum_single_float_alloc_(local, v, 0, rv, ret);
}

_g int bignum_single_float_heap_(LocalRoot local, single_float v, addr *rv, int *ret)
{
	CheckLocal(local);
	return bignum_single_float_alloc_(local, v, 1, rv, ret);
}

static int bignum_double_float_alloc_(
		LocalRoot local, double_float v, int is_heap, addr *rv, int *ret)
{
	LocalStack stack;

	/* check */
	if (! IsIntegerDouble(v)) {
		if (ret)
			return Result(ret, 1);
		else
			return fmte_("Invalid float value.", NULL);
	}

	/* cast */
	if (is_heap) {
		push_local(local, &stack);
	}
	bignum_double_float_unsafe(local, v, is_heap, rv);
	if (is_heap) {
		rollback_local(local, stack);
	}

	/* result */
	if (ret)
		return Result(ret, 0);

	return 0;
}

_g int bignum_double_float_local_(LocalRoot local, double_float v, addr *rv, int *ret)
{
	CheckLocal(local);
	return bignum_double_float_alloc_(local, v, 0, rv, ret);
}

_g int bignum_double_float_heap_(LocalRoot local, double_float v, addr *rv, int *ret)
{
	CheckLocal(local);
	return bignum_double_float_alloc_(local, v, 1, rv, ret);
}

static int bignum_long_float_alloc_(
		LocalRoot local, long_float v, int is_heap, addr *rv, int *ret)
{
	LocalStack stack;

	/* check */
	if (! IsIntegerLongFloat(v)) {
		if (ret)
			return Result(ret, 1);
		else
			return fmte_("Invalid float value.", NULL);
	}

	/* cast */
	if (is_heap) {
		push_local(local, &stack);
	}
	bignum_long_float_unsafe(local, v, is_heap, rv);
	if (is_heap) {
		rollback_local(local, stack);
	}

	/* result */
	if (ret)
		return Result(ret, 0);

	return 0;
}

_g int bignum_long_float_local_(LocalRoot local, long_float v, addr *rv, int *ret)
{
	CheckLocal(local);
	return bignum_long_float_alloc_(local, v, 0, rv, ret);
}

_g int bignum_long_float_heap_(LocalRoot local, long_float v, addr *rv, int *ret)
{
	CheckLocal(local);
	return bignum_long_float_alloc_(local, v, 1, rv, ret);
}


/*
 *  getvalue
 */
_g int GetFixnum_bignum(addr pos, fixnum *ret)
{
	int sign;
	bigtype value;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	if (RefSizeBignum(pos) != 1)
		return 1;
	GetSignBignum(pos, &sign);
	getfixed_bignum(pos, 0, &value);
	if (IsPlus(sign)) {
		if (FIXNUM_MAX < value)
			return 1;
		*ret = (fixnum)value;
		return 0;
	}
	else {
		if (FIXNUM_UMIN < value)
			return 1;
		*ret = -(fixnum)value;
		return 0;
	}

	return 1;
}

_g int GetFixnum_signed(addr pos, fixnum *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			return GetFixnum_bignum(pos, ret);

		default:
			break;
	}

	return 1;
}

_g int getfixnum_signed_(addr pos, fixnum *ret)
{
	if (GetFixnum_signed(pos, ret))
		return TypeError_(pos, FIXNUM);

	return 0;
}

_g int GetFixnum_unsigned(addr pos, fixnum *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				return 1;
			break;

		default:
			return 1;
	}
	if (v < 0)
		return 1;
	*ret = v;
	return 0;
}

_g int getfixnum_unsigned_(addr pos, fixnum *ret)
{
	addr type;

	if (GetFixnum_unsigned(pos, ret)) {
		type4integer_heap(Nil, 0, Nil, FIXNUM_MAX, &type);
		return call_type_error_(NULL, pos, type);
	}

	return 0;
}

_g int getfixed1_bignum(addr pos, int *sign, fixed *ret)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	if (RefSizeBignum(pos) != 1)
		return 1;
	GetSignBignum(pos, sign);
	getfixed_bignum(pos, 0, ret);

	return 0;
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

