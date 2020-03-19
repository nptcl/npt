#include "bigdata.h"
#include "bignum.h"
#include "condition.h"
#include "number_random.h"
#include "random.h"
#include "random_float.h"
#include "random_state.h"

/*
 *  random
 */
#ifdef LISP_32BIT
#define single_float_random float_random_32bit
#define double_float_random double_random_32bit
#define long_float_random long_random_32bit
static bigtype random_full_bigtype(struct random_state *state)
{
	return (bigtype)random_number_32bit(state);
}
static bigtype random_equal_bigtype(struct random_state *state, bigtype value)
{
	return (bigtype)random_equal_32bit(state, (uint32_t)value);
}
static bigtype random_less_bigtype(struct random_state *state, bigtype value)
{
	return (bigtype)random_less_32bit(state, (uint32_t)value);
}
#else
#define single_float_random float_random_64bit
#define double_float_random double_random_64bit
#define long_float_random long_random_64bit
static bigtype random_full_bigtype(struct random_state *state)
{
	return (bigtype)random_number_64bit(state);
}
static bigtype random_equal_bigtype(struct random_state *state, bigtype value)
{
	return (bigtype)random_equal_64bit(state, (uint64_t)value);
}
static bigtype random_less_bigtype(struct random_state *state, bigtype value)
{
	return (bigtype)random_less_64bit(state, (uint64_t)value);
}
#endif


/*
 *  random fixnum
 */
static void random_fixnum_common(struct random_state *state, addr pos, addr *ret)
{
	int ignore;
	bigtype value;

	castfixed_fixnum(pos, &ignore, &value);
	Check(value <= 0, "Invalid fixnum value.");
	value = random_less_bigtype(state, value);
	fixnum_heap(ret, (fixnum)value);
}


/*
 *  random bignum
 */
static void random_bignum1_common(LocalRoot local,
		struct random_state *state, addr pos, addr *ret)
{
	bigtype value, *data;
	LocalStack stack;

	GetDataBignum(pos, &data);
	value = random_less_bigtype(state, data[0]);
	push_local(local, &stack);
	bignum_value_local(local, &pos, SignPlus, value);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

static int random_insertbuffer(LocalRoot local, struct random_state *state, addr pos)
{
	bigtype *data, v1, v2;
	size_t size, i, index;

	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	index = size - 1;
	v1 = data[index];
	v2 = random_equal_bigtype(state, v1);
	if (v1 == v2) {
		for (i = 1; i < size; i++) {
			index = size - i - 1;
			v1 = data[index];
			v2 = random_full_bigtype(state);
			if (v1 < v2) return 1;
			if (v1 > v2) goto tail;
		}
		return 0;
	}

tail:
	data[index] = v2;
	for (i = 0; i < index; i++)
		data[i] = random_full_bigtype(state);

	return 0;
}

static void random_bigdata_common(LocalRoot local,
		struct random_state *state, addr pos, addr *ret)
{
	addr value;
	LocalStack stack;

	push_local(local, &stack);
	plus_bv_bignum_local(local, pos, -1, &value);
	while (random_insertbuffer(local, state, value))
		continue;
	sizepress_bignum(value);
	bignum_result_heap(value, ret);
	rollback_local(local, stack);
}

static void random_bignum_common(LocalRoot local,
		struct random_state *state, addr pos, addr *ret)
{
	size_t size;

	CheckLocal(local);
	CheckType(pos, LISPTYPE_BIGNUM);
	Check(! plusp_bignum(pos), "Invalid bignum value.");
	GetSizeBignum(pos, &size);
	if (size == 1)
		random_bignum1_common(local, state, pos, ret);
	else
		random_bigdata_common(local, state, pos, ret);
}


/*
 *  random float
 */
static void random_single_common(struct random_state *state, addr pos, addr *ret)
{
	single_float value, check;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	GetSingleFloat(pos, &value);
	for (;;) {
		check = single_float_random(state) * value;
		if (check < value)
			break;
	}
	single_float_heap(ret, check);
}

static void random_double_common(struct random_state *state, addr pos, addr *ret)
{
	double_float value, check;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(pos, &value);
	for (;;) {
		check = double_float_random(state) * value;
		if (check < value)
			break;
	}
	double_float_heap(ret, check);
}

static void random_long_common(struct random_state *state, addr pos, addr *ret)
{
	long_float value, check;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &value);
	for (;;) {
		check = long_float_random(state) * value;
		if (check < value)
			break;
	}
	long_float_heap(ret, check);
}


/*
 *  common
 */
_g void random_number_common(LocalRoot local, addr pos, addr state, addr *ret)
{
	struct random_state *ptr;

	CheckType(state, LISPTYPE_RANDOM_STATE);
	ptr = struct_random_state(state);
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			random_fixnum_common(ptr, pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			random_bignum_common(local, ptr, pos, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			random_single_common(ptr, pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			random_double_common(ptr, pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			random_long_common(ptr, pos, ret);
			break;

		default:
			_fmte("The random number ~S must be an integer or float.", pos, NULL);
			*ret = 0;
			break;
	}
}

