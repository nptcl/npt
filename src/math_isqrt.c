/*
 *  Kaiheihou  (extraction of square root)
 */
#include "bignum_data.h"
#include "bignum_object.h"
#include "format.h"
#include "integer.h"
#include "math_isqrt.h"
#include "object.h"

static void isqrt_shiftup(addr pos, unsigned shift)
{
	bigtype *data, x, carry;
	size_t size, i;

	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	carry = 0;
	for (i = 0; i < size; i++) {
		x = data[i];
		data[i] = (x << shift) | carry;
		carry = x >> (BIGNUM_FULLBIT - shift);
	}
	if (carry) {
		Check(RefAllocBignum(pos) <= size, "size error");
		data[i] = carry;
		SetSizeBignum(pos, size + 1);
	}
}

static void isqrt_letbit2(addr pos, unsigned a, unsigned b)
{
	bigtype *data;
	GetDataBignum(pos, &data);
	data[0] |= (a << 1) | b;
}

static void isqrt_letbit1(addr pos, unsigned a)
{
	bigtype *data;
	GetDataBignum(pos, &data);
	data[0] |= a;
}

static void isqrt_letbit1_remove(addr pos)
{
	bigtype *data;
	GetDataBignum(pos, &data);
	data[0] >>= 1;
	data[0] <<= 1;
}

static void isqrt_compare(addr x, addr y, addr z)
{
	isqrt_shiftup(x, 1);
	isqrt_shiftup(y, 1);
	isqrt_letbit1(y, 1);
	if (compare_bigdata(y, z) <= 0) {
		letminus_noexpand_bigdata(z, y);
		setplusvalue_bigdata(y, y, SignPlus, 1);
		isqrt_letbit1(x, 1);
	}
	else {
		isqrt_letbit1_remove(y);
	}
}

static void isqrt_loop(addr var, size_t size, addr x, addr y, addr z)
{
	unsigned a, b;

	Check(size % 2, "size error");
	while (size) {
		a = getbit_bignum(var, --size);
		Check(size == 0, "size zero error");
		b = getbit_bignum(var, --size);
		isqrt_shiftup(z, 2);
		isqrt_letbit2(z, a, b);
		isqrt_compare(x, y, z);
	}
}

static int isqrt_buffer_(addr var, addr x, addr y, addr z)
{
	unsigned a, b;
	bigtype value;
	size_t size;

	Return(integer_length_value_(var, &size));
	Check(size == 0, "size error");
	if (size % 2) {
		value = (bigtype)getbit_bignum(var, --size);
	}
	else {
		a = getbit_bignum(var, --size);
		b = getbit_bignum(var, --size);
		value = (bigtype)((a << 1) | b);
	}
	setvalue_bignum(x, SignPlus, 1);
	setvalue_bignum(y, SignPlus, 2); /* #b10 */
	setvalue_bignum(z, SignPlus, value);
	setminusvalue_bigdata(z, z, SignPlus, 1);
	isqrt_loop(var, size, x, y, z);

	return 0;
}

static int isqrt_bignum_(LocalRoot local, addr var, addr *ret)
{
	addr x, y, z;
	size_t size;

	/* local buffer */
	GetSizeBignum(var, &size);
	size = (size >> 1) + 1;
	bignum_local(local, &x, SignPlus, size);
	bignum_local(local, &y, SignPlus, size);
	bignum_local(local, &z, SignPlus, size);

	/* result */
	Return(isqrt_buffer_(var, x, y, z));
	*ret = x;

	return 0;
}

_g int isqrt_number_common_(LocalRoot local, addr var, addr *ret)
{
	int check;
	LocalStack stack;

	/* zero */
	Check(minusp_integer_debug(var), "minum error");
	Return(plusp_integer_(var, &check));
	if (! check) {
		fixnum_heap(ret, 0);
		return 0;
	}

	/* isqrt */
	push_local(local, &stack);
	if (fixnump(var))
		bignum_fixnum_local(local, &var, var);
	Return(isqrt_bignum_(local, var, &var));
	bignum_result_heap(var, ret);
	rollback_local(local, stack);

	return 0;
}

