#include "bignum_data.h"
#include "bignum_multi.h"
#include "bignum_object.h"
#include "condition.h"
#include "cons.h"
#include "local.h"
#include "integer.h"
#include "math_gcd.h"
#include "ratio.h"

/*
 *  gcd
 */
static size_t gcd_buffer_size(addr pos)
{
	size_t size;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return 1;

		case LISPTYPE_BIGNUM:
			GetSizeBignum(pos, &size);
			return size;

		default:
			TypeError(pos, INTEGER);
			return 0;
	}
}

static void gcd_first_number(addr pos, addr *ret)
{
	Check(! integerp(pos), "type error");
	if (minusp_integer(pos))
		sign_reverse_integer_common(pos, ret);
	else
		integer_throw_heap(pos, ret);
}

static void copy_noexpand_integer(addr left, addr right)
{
	int sign;
	fixed value;

	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			castfixed_fixnum(right, &sign, &value);
			setvalue_bignum(left, sign, value);
			break;

		case LISPTYPE_BIGNUM:
			copy_noexpand_bignum(left, right);
			break;

		default:
			TypeError(right, INTEGER);
			break;
	}
}

static void gcd_loop_number(LocalRoot local, addr left, addr first, addr args)
{
	addr right;
	LocalStack stack;

	copy_noexpand_integer(left, first);
	while (args != Nil) {
		GetCons(args, &right, &args);
		if (zerop_integer(right))
			continue;
		push_local(local, &stack);
		bignum_integer_local(local, &right, right);
		euclidean_bignum(local, left, right);
		rollback_local(local, stack);
	}
}

_g int gcd_number_(LocalRoot local, addr args, addr *ret)
{
	addr left, right, pos, first_left, first_right;
	LocalStack stack;
	size_t count, check, size;

	/* check */
	first_left = Unbound;
	count = 0;
	size = 0;
	for (right = args; right != Nil; ) {
		Return_getcons(right, &left, &right);
		Check(! integerp(left), "type error");
		/* ignore */
		if (zerop_integer(left))
			continue;
		/* first */
		if (first_left == Unbound) {
			first_left = left;
			first_right = right;
		}
		/* max size */
		check = gcd_buffer_size(left);
		if (size < check)
			size = check;
		/* count */
		count++;
	}

	/* no argument */
	if (count == 0) {
		fixnum_heap(ret, 0);
		return 0;
	}

	/* only one argument */
	if (count == 1) {
		gcd_first_number(first_left, ret);
		return 0;
	}

	/* second */
	push_local(local, &stack);
	bignum_local(local, &pos, SignPlus, size);
	gcd_loop_number(local, pos, first_left, first_right);
	SetSignBignum(pos, SignPlus);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);

	return 0;
}


/*
 *  lcm
 */
static void lcm_calc_number(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;
	addr temp;

	/* (lcm a b) ==  (/ (abs (* a b)) (gcd a b)) */
	push_local(local, &stack);
	bignum_integer_local(local, &left, left);
	bignum_integer_local(local, &right, right);
	/* (* a b) */
	multi_bb_bignum_local(local, left, right, &temp);
	/* (gcd a b) */
	euclidean_bignum(local, left, right);
	/* (/ temp left) */
	letdiv_noexpand_bigdata(local, temp, left);
	bignum_throw_heap(temp, ret);
	/* result */
	rollback_local(local, stack);
}

static int lcm_loop_number_(LocalRoot local, addr left, addr args, addr *ret)
{
	addr right;

	while (args != Nil) {
		Return_getcons(args, &right, &args);
		Check(! integerp(right), "type error");
		if (zerop_integer(right)) {
			fixnum_heap(ret, 0);
			return 0;
		}
		lcm_calc_number(local, &left, left, right);
	}

	return Result(ret, left);
}

_g int lcm_number_(LocalRoot local, addr args, addr *ret)
{
	addr left;

	if (args == Nil) {
		fixnum_heap(ret, 1);
		return 0;
	}

	/* only one argument */
	Return_getcons(args, &left, &args);
	if (args == Nil) {
		gcd_first_number(left, ret);
		return 0;
	}

	/* zero */
	Check(! integerp(left), "type error");
	if (zerop_integer(left)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	/* loop */
	return lcm_loop_number_(local, left, args, ret);
}

