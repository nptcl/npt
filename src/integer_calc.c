#include "bignum_plus.h"
#include "bignum_multi.h"
#include "condition.h"
#include "local.h"
#include "integer_calc.h"
#include "typedef.h"

/*
 *  oneplus
 */
int oneplus_integer_common_(LocalRoot local, addr value, addr *ret)
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

int oneminus_integer_common_(LocalRoot local, addr value, addr *ret)
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
int plus_fi_bignum_local_(LocalRoot local, addr left, addr right, addr *ret)
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

int plus_fi_real_local_(LocalRoot local, addr left, addr right, addr *ret)
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

int plus_fi_real_common_(LocalRoot local, addr left, addr right, addr *ret)
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

int plus_bi_bignum_local_(LocalRoot local, addr left, addr right, addr *ret)
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

int plus_bi_real_local_(LocalRoot local, addr left, addr right, addr *ret)
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

int plus_bi_real_common_(LocalRoot local, addr left, addr right, addr *ret)
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

int plus_ii_bignum_local_(LocalRoot local, addr left, addr right, addr *ret)
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

int plus_ii_real_local_(LocalRoot local, addr left, addr right, addr *ret)
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

int plus_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret)
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

int minus_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret)
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

int multi_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret)
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

