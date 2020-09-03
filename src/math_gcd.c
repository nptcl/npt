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
static int gcd_buffer_size_(addr pos, size_t *ret)
{
	size_t size;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, 1);

		case LISPTYPE_BIGNUM:
			GetSizeBignum(pos, &size);
			return Result(ret, size);

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

static int gcd_first_number_(addr pos, addr *ret)
{
	int check;

	Check(! integerp(pos), "type error");
	Return(minusp_integer_(pos, &check));
	if (check)
		return sign_reverse_integer_common_(pos, ret);
	else
		return integer_throw_heap_(pos, ret);
}

static int copy_noexpand_integer_(addr left, addr right)
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
			return TypeError_(right, INTEGER);
	}

	return 0;
}

static int gcd_loop_number_(LocalRoot local, addr left, addr first, addr args)
{
	int check;
	addr right;
	LocalStack stack;

	Return(copy_noexpand_integer_(left, first));
	while (args != Nil) {
		GetCons(args, &right, &args);
		Return(zerop_integer_(right, &check));
		if (check)
			continue;
		push_local(local, &stack);
		Return(bignum_integer_local_(local, &right, right));
		euclidean_bignum(local, left, right);
		rollback_local(local, stack);
	}

	return 0;
}

_g int gcd_number_(LocalRoot local, addr args, addr *ret)
{
	int check;
	addr left, right, pos, first_left, first_right;
	LocalStack stack;
	size_t count, value, size;

	/* check */
	first_left = first_right = Unbound;
	count = 0;
	size = 0;
	for (right = args; right != Nil; ) {
		Return_getcons(right, &left, &right);
		Check(! integerp(left), "type error");
		/* ignore */
		Return(zerop_integer_(left, &check));
		if (check)
			continue;
		/* first */
		if (first_left == Unbound) {
			first_left = left;
			first_right = right;
		}
		/* max size */
		Return(gcd_buffer_size_(left, &value));
		if (size < value)
			size = value;
		/* count */
		count++;
	}

	/* no argument */
	if (count == 0) {
		fixnum_heap(ret, 0);
		return 0;
	}

	/* only one argument */
	if (count == 1)
		return gcd_first_number_(first_left, ret);

	/* second */
	push_local(local, &stack);
	bignum_local(local, &pos, SignPlus, size);
	Return(gcd_loop_number_(local, pos, first_left, first_right));
	SetSignBignum(pos, SignPlus);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);

	return 0;
}


/*
 *  lcm
 */
static int lcm_calc_number_(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;
	addr temp;

	/* (lcm a b) ==  (/ (abs (* a b)) (gcd a b)) */
	push_local(local, &stack);
	Return(bignum_integer_local_(local, &left, left));
	Return(bignum_integer_local_(local, &right, right));
	/* (* a b) */
	multi_bb_bignum_local(local, left, right, &temp);
	/* (gcd a b) */
	euclidean_bignum(local, left, right);
	/* (/ temp left) */
	letdiv_noexpand_bigdata(local, temp, left);
	bignum_throw_heap(temp, ret);
	/* result */
	rollback_local(local, stack);

	return 0;
}

static int lcm_loop_number_(LocalRoot local, addr left, addr args, addr *ret)
{
	int check;
	addr right;

	while (args != Nil) {
		Return_getcons(args, &right, &args);
		Check(! integerp(right), "type error");
		Return(zerop_integer_(right, &check));
		if (check) {
			fixnum_heap(ret, 0);
			return 0;
		}
		Return(lcm_calc_number_(local, &left, left, right));
	}

	return Result(ret, left);
}

_g int lcm_number_(LocalRoot local, addr args, addr *ret)
{
	int check;
	addr left;

	if (args == Nil) {
		fixnum_heap(ret, 1);
		return 0;
	}

	/* only one argument */
	Return_getcons(args, &left, &args);
	if (args == Nil)
		return gcd_first_number_(left, ret);

	/* zero */
	Check(! integerp(left), "type error");
	Return(zerop_integer_(left, &check));
	if (check) {
		fixnum_heap(ret, 0);
		return 0;
	}

	/* loop */
	return lcm_loop_number_(local, left, args, ret);
}

