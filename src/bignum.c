#include "bignum_cons.h"
#include "bignum_data.h"
#include "bignum_object.h"
#include "bignum.h"
#include "condition.h"
#include "local.h"
#include "real_float.h"
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

	if (! IsIntegerFloat(value))
		return 1;
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

	if (! IsIntegerDouble(value))
		return 1;
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

	if (! IsIntegerLongFloat(value))
		return 1;
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

_g void getfixnum_signed(addr pos, fixnum *ret)
{
	if (GetFixnum_signed(pos, ret))
		TypeError(pos, FIXNUM);
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

_g void getfixnum_unsigned(addr pos, fixnum *ret)
{
	addr type;

	if (GetFixnum_unsigned(pos, ret)) {
		type4integer_heap(Nil, 0, Nil, FIXNUM_MAX, &type);
		type_error(pos, type);
	}
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

