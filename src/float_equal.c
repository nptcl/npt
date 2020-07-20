#include "bignum.h"
#include "condition.h"
#include "float_equal.h"
#include "object.h"
#include "typedef.h"

/*
 *  boolean
 */
_g int zerop_single_float(addr pos)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	return RefSingleFloat(pos) == 0.0f;
}

_g int zerop_double_float(addr pos)
{
	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	return RefDoubleFloat(pos) == 0.0;
}

_g int zerop_long_float(addr pos)
{
	CheckType(pos, LISPTYPE_LONG_FLOAT);
	return RefLongFloat(pos) == 0.0L;
}

_g int zerop_float(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return zerop_single_float(pos);

		case LISPTYPE_DOUBLE_FLOAT:
			return zerop_double_float(pos);

		case LISPTYPE_LONG_FLOAT:
			return zerop_long_float(pos);

		case LISPTYPE_SHORT_FLOAT:
			Abort("short float is not implemented.");
			return 0;

		default:
			TypeError(pos, FLOAT);
			return 0;
	}
}


/*
 *  equal
 */
_g int equal_fs_real(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	return single_float_fixnum(left) == RefSingleFloat(right);
}

_g int equal_fd_real(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	return double_float_fixnum(left) == RefDoubleFloat(right);
}

_g int equal_fl_real(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	return long_float_fixnum(left) == RefLongFloat(right);
}


/*
 *  compare
 */
_g int compare_fs_real(addr left, addr right)
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

_g int compare_fd_real(addr left, addr right)
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

_g int compare_fl_real(addr left, addr right)
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

_g int compare_sf_real(addr left, addr right)
{
	return -compare_fs_real(right, left);
}

_g int compare_df_real(addr left, addr right)
{
	return -compare_fd_real(right, left);
}

_g int compare_lf_real(addr left, addr right)
{
	return -compare_fl_real(right, left);
}

_g int compare_ss_real(addr left, addr right)
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

_g int compare_sd_real(addr left, addr right)
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

_g int compare_sl_real(addr left, addr right)
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

_g int compare_ds_real(addr left, addr right)
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

_g int compare_dd_real(addr left, addr right)
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

_g int compare_dl_real(addr left, addr right)
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

_g int compare_ls_real(addr left, addr right)
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

_g int compare_ld_real(addr left, addr right)
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

_g int compare_ll_real(addr left, addr right)
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
	TypeError(right, FLOAT);
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
	TypeError(right, FLOAT);
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
	TypeError(right, FLOAT);
	return 0;
}

_g int compare_float(addr left, addr right)
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
	TypeError(right, FLOAT);
	return 0;
}

_g int less_float_clang(addr left, addr right)
{
	return less_float(left, right);
}

_g int less_equal_float_clang(addr left, addr right)
{
	return less_equal_float(left, right);
}

_g int less_ss_clang(addr left, addr right)
{
	return less_ss_real(left, right);
}

_g int less_dd_clang(addr left, addr right)
{
	return less_dd_real(left, right);
}

_g int less_ll_clang(addr left, addr right)
{
	return less_ll_real(left, right);
}

_g int less_equal_ss_clang(addr left, addr right)
{
	return less_equal_ss_real(left, right);
}

_g int less_equal_dd_clang(addr left, addr right)
{
	return less_equal_dd_real(left, right);
}

_g int less_equal_ll_clang(addr left, addr right)
{
	return less_equal_ll_real(left, right);
}
