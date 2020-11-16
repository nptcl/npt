#include "bignum.h"
#include "condition.h"
#include "float_equal.h"
#include "object.h"
#include "typedef.h"

/*
 *  boolean
 */
int zerop_single_float(addr pos)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	return RefSingleFloat(pos) == 0.0f;
}

int zerop_double_float(addr pos)
{
	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	return RefDoubleFloat(pos) == 0.0;
}

int zerop_long_float(addr pos)
{
	CheckType(pos, LISPTYPE_LONG_FLOAT);
	return RefLongFloat(pos) == 0.0L;
}


/*
 *  equal
 */
int equal_fs_real(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	return single_float_fixnum(left) == RefSingleFloat(right);
}

int equal_fd_real(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	return double_float_fixnum(left) == RefDoubleFloat(right);
}

int equal_fl_real(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	return long_float_fixnum(left) == RefLongFloat(right);
}

int equal_bs_real_(addr left, addr right, int *ret)
{
	single_float x, y;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &x));
	GetSingleFloat(right, &y);

	return Result(ret, x == y);
}

int equal_bd_real_(addr left, addr right, int *ret)
{
	double_float x, y;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &x));
	GetDoubleFloat(right, &y);

	return Result(ret, x == y);
}

int equal_bl_real_(addr left, addr right, int *ret)
{
	long_float x, y;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &x));
	GetLongFloat(right, &y);

	return Result(ret, x == y);
}


/*
 *  compare
 */
int compare_fs_real(addr left, addr right)
{
	single_float value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value1 = single_float_fixnum(left);
	value2 = RefSingleFloat(right);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_fd_real(addr left, addr right)
{
	double_float value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value1 = double_float_fixnum(left);
	value2 = RefDoubleFloat(right);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_fl_real(addr left, addr right)
{
	long_float value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value1 = long_float_fixnum(left);
	value2 = RefLongFloat(right);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_sf_real(addr left, addr right)
{
	return -compare_fs_real(right, left);
}

int compare_df_real(addr left, addr right)
{
	return -compare_fd_real(right, left);
}

int compare_lf_real(addr left, addr right)
{
	return -compare_fl_real(right, left);
}

int compare_ss_real(addr left, addr right)
{
	single_float value1, value2;
	GetSingleFloat(left, &value1);
	GetSingleFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_sd_real(addr left, addr right)
{
	single_float value1;
	double_float value2;
	GetSingleFloat(left, &value1);
	GetDoubleFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_sl_real(addr left, addr right)
{
	single_float value1;
	long_float value2;
	GetSingleFloat(left, &value1);
	GetLongFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_ds_real(addr left, addr right)
{
	double_float value1;
	single_float value2;
	GetDoubleFloat(left, &value1);
	GetSingleFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_dd_real(addr left, addr right)
{
	double_float value1, value2;
	GetDoubleFloat(left, &value1);
	GetDoubleFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_dl_real(addr left, addr right)
{
	double_float value1;
	long_float value2;
	GetDoubleFloat(left, &value1);
	GetLongFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_ls_real(addr left, addr right)
{
	long_float value1;
	single_float value2;
	GetLongFloat(left, &value1);
	GetSingleFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_ld_real(addr left, addr right)
{
	long_float value1;
	double_float value2;
	GetLongFloat(left, &value1);
	GetDoubleFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

int compare_ll_real(addr left, addr right)
{
	long_float value1, value2;
	GetLongFloat(left, &value1);
	GetLongFloat(right, &value2);
	if (value1 < value2)
		return -1;
	if (value1 > value2)
		return 1;

	return 0;
}

static int compare_single_float(addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			return compare_ss_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_sd_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_sl_real(left, right);

		default:
			break;
	}
	Abort("type error");
	return 0;
}

static int compare_double_float(addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			return compare_ds_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_dd_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_dl_real(left, right);

		default:
			break;
	}
	Abort("type error");
	return 0;
}

static int compare_long_float(addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			return compare_ls_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_ld_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_ll_real(left, right);

		default:
			break;
	}
	Abort("type error");
	return 0;
}

int compare_float(addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_SINGLE_FLOAT:
			return compare_single_float(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_double_float(left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_long_float(left, right);

		default:
			break;
	}
	Abort("type error");
	return 0;
}

int less_float_clang(addr left, addr right)
{
	return less_float(left, right);
}

int less_equal_float_clang(addr left, addr right)
{
	return less_equal_float(left, right);
}

int less_ss_clang(addr left, addr right)
{
	return less_ss_real(left, right);
}

int less_dd_clang(addr left, addr right)
{
	return less_dd_real(left, right);
}

int less_ll_clang(addr left, addr right)
{
	return less_ll_real(left, right);
}

int less_equal_ss_clang(addr left, addr right)
{
	return less_equal_ss_real(left, right);
}

int less_equal_dd_clang(addr left, addr right)
{
	return less_equal_dd_real(left, right);
}

int less_equal_ll_clang(addr left, addr right)
{
	return less_equal_ll_real(left, right);
}


/*
 *  compare
 */
static int compare_single_float_(addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			return Result(ret, compare_ss_real(left, right));

		case LISPTYPE_DOUBLE_FLOAT:
			return Result(ret, compare_sd_real(left, right));

		case LISPTYPE_LONG_FLOAT:
			return Result(ret, compare_sl_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, FLOAT);
	}
}

static int compare_double_float_(addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			return Result(ret, compare_ds_real(left, right));

		case LISPTYPE_DOUBLE_FLOAT:
			return Result(ret, compare_dd_real(left, right));

		case LISPTYPE_LONG_FLOAT:
			return Result(ret, compare_dl_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, FLOAT);
	}
}

static int compare_long_float_(addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			return Result(ret, compare_ls_real(left, right));

		case LISPTYPE_DOUBLE_FLOAT:
			return Result(ret, compare_ld_real(left, right));

		case LISPTYPE_LONG_FLOAT:
			return Result(ret, compare_ll_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, FLOAT);
	}
}

int compare_float_(addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_SINGLE_FLOAT:
			return compare_single_float_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_double_float_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return compare_long_float_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, FLOAT);
	}
}

int less_float_clang_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_float_(left, right, &check));
	return Result(ret, check < 0);
}

int less_equal_float_clang_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_float_(left, right, &check));
	return Result(ret, check <= 0);
}

