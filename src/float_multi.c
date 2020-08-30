#include "bignum.h"
#include "bignum_equal.h"
#include "condition.h"
#include "float_object.h"
#include "float_multi.h"
#include "object.h"
#include "ratio.h"
#include "typedef.h"

/*
 *  multiple
 */
_g int multi_float_fs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = single_float_fixnum(left) * RefSingleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_fd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = double_float_fixnum(left) * RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_fl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = long_float_fixnum(left) * RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_fs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_fs_alloc_(local, left, right, ret);
}

_g int multi_float_fd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_fd_alloc_(local, left, right, ret);
}

_g int multi_float_fl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_fl_alloc_(local, left, right, ret);
}

_g int multi_float_fs_heap_(addr left, addr right, addr *ret)
{
	return multi_float_fs_alloc_(NULL, left, right, ret);
}

_g int multi_float_fd_heap_(addr left, addr right, addr *ret)
{
	return multi_float_fd_alloc_(NULL, left, right, ret);
}

_g int multi_float_fl_heap_(addr left, addr right, addr *ret)
{
	return multi_float_fl_alloc_(NULL, left, right, ret);
}

_g int multi_float_bs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	Return(single_float_bignum_(left, &value));
	value *= RefSingleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_bd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	Return(double_float_bignum_(left, &value));
	value *= RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_bl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	Return(long_float_bignum_(left, &value));
	value *= RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_bs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_bs_alloc_(local, left, right, ret);
}

_g int multi_float_bd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_bd_alloc_(local, left, right, ret);
}

_g int multi_float_bl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_bl_alloc_(local, left, right, ret);
}

_g int multi_float_bs_heap_(addr left, addr right, addr *ret)
{
	return multi_float_bs_alloc_(NULL, left, right, ret);
}

_g int multi_float_bd_heap_(addr left, addr right, addr *ret)
{
	return multi_float_bd_alloc_(NULL, left, right, ret);
}

_g int multi_float_bl_heap_(addr left, addr right, addr *ret)
{
	return multi_float_bl_alloc_(NULL, left, right, ret);
}

_g int multi_float_rs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	Return(single_float_ratio_(left, &value));
	value *= RefSingleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_rd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	Return(double_float_ratio_(left, &value));
	value *= RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_rl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	Return(long_float_ratio_(left, &value));
	value *= RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
	return 0;
}

_g int multi_float_rs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_rs_alloc_(local, left, right, ret);
}

_g int multi_float_rd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_rd_alloc_(local, left, right, ret);
}

_g int multi_float_rl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_rl_alloc_(local, left, right, ret);
}

_g int multi_float_rs_heap_(addr left, addr right, addr *ret)
{
	return multi_float_rs_alloc_(NULL, left, right, ret);
}

_g int multi_float_rd_heap_(addr left, addr right, addr *ret)
{
	return multi_float_rd_alloc_(NULL, left, right, ret);
}

_g int multi_float_rl_heap_(addr left, addr right, addr *ret)
{
	return multi_float_rl_alloc_(NULL, left, right, ret);
}

_g int multi_float_ss_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefSingleFloat(left) * RefSingleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_sd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = ((double_float)RefSingleFloat(left)) * RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_sl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefSingleFloat(left)) * RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_ds_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) * ((double_float)RefSingleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_dd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) * RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_dl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefDoubleFloat(left)) * RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_ls_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefLongFloat(left) * ((long_float)RefSingleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_ld_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefLongFloat(left) * ((long_float)RefDoubleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_ll_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = RefLongFloat(left) * RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int multi_float_ss_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_ss_alloc_(local, left, right, ret);
}

_g int multi_float_sd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_sd_alloc_(local, left, right, ret);
}

_g int multi_float_sl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_sl_alloc_(local, left, right, ret);
}

_g int multi_float_ds_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_ds_alloc_(local, left, right, ret);
}

_g int multi_float_dd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_dd_alloc_(local, left, right, ret);
}

_g int multi_float_dl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_dl_alloc_(local, left, right, ret);
}

_g int multi_float_ls_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_ls_alloc_(local, left, right, ret);
}

_g int multi_float_ld_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_ld_alloc_(local, left, right, ret);
}

_g int multi_float_ll_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return multi_float_ll_alloc_(local, left, right, ret);
}

_g int multi_float_ss_heap_(addr left, addr right, addr *ret)
{
	return multi_float_ss_alloc_(NULL, left, right, ret);
}

_g int multi_float_sd_heap_(addr left, addr right, addr *ret)
{
	return multi_float_sd_alloc_(NULL, left, right, ret);
}

_g int multi_float_sl_heap_(addr left, addr right, addr *ret)
{
	return multi_float_sl_alloc_(NULL, left, right, ret);
}

_g int multi_float_ds_heap_(addr left, addr right, addr *ret)
{
	return multi_float_ds_alloc_(NULL, left, right, ret);
}

_g int multi_float_dd_heap_(addr left, addr right, addr *ret)
{
	return multi_float_dd_alloc_(NULL, left, right, ret);
}

_g int multi_float_dl_heap_(addr left, addr right, addr *ret)
{
	return multi_float_dl_alloc_(NULL, left, right, ret);
}

_g int multi_float_ls_heap_(addr left, addr right, addr *ret)
{
	return multi_float_ls_alloc_(NULL, left, right, ret);
}

_g int multi_float_ld_heap_(addr left, addr right, addr *ret)
{
	return multi_float_ld_alloc_(NULL, left, right, ret);
}

_g int multi_float_ll_heap_(addr left, addr right, addr *ret)
{
	return multi_float_ll_alloc_(NULL, left, right, ret);
}


/*
 *  inverse
 */
_g int inverse_single_float_alloc_(LocalRoot local, addr pos, addr *ret)
{
	single_float value;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	GetSingleFloat(pos, &value);
	if (value == 0.0f) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, pos);
	}
	value = 1.0f / value;
	Return_float_errorcheck1(CONSTANT_COMMON_SLASH, value, pos);
	single_float_alloc(local, ret, value);

	return 0;
}

_g int inverse_double_float_alloc_(LocalRoot local, addr pos, addr *ret)
{
	double_float value;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(pos, &value);
	if (value == 0.0) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, pos);
	}
	value = 1.0 / value;
	Return_float_errorcheck1(CONSTANT_COMMON_SLASH, value, pos);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int inverse_long_float_alloc_(LocalRoot local, addr pos, addr *ret)
{
	long_float value;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, pos);
	}
	value = 1.0L / value;
	Return_float_errorcheck1(CONSTANT_COMMON_SLASH, value, pos);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int inverse_single_float_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return inverse_single_float_alloc_(local, pos, ret);
}

_g int inverse_double_float_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return inverse_double_float_alloc_(local, pos, ret);
}

_g int inverse_long_float_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return inverse_long_float_alloc_(local, pos, ret);
}

_g int inverse_single_float_heap_(addr pos, addr *ret)
{
	return inverse_single_float_alloc_(NULL, pos, ret);
}

_g int inverse_double_float_heap_(addr pos, addr *ret)
{
	return inverse_double_float_alloc_(NULL, pos, ret);
}

_g int inverse_long_float_heap_(addr pos, addr *ret)
{
	return inverse_long_float_alloc_(NULL, pos, ret);
}


/*
 *  division
 */
_g int div_float_fs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = single_float_fixnum(left) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_fd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = double_float_fixnum(left) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_fl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = long_float_fixnum(left) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_fs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_fs_alloc_(local, left, right, ret);
}

_g int div_float_fd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_fd_alloc_(local, left, right, ret);
}

_g int div_float_fl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_fl_alloc_(local, left, right, ret);
}

_g int div_float_fs_heap_(addr left, addr right, addr *ret)
{
	return div_float_fs_alloc_(NULL, left, right, ret);
}

_g int div_float_fd_heap_(addr left, addr right, addr *ret)
{
	return div_float_fd_alloc_(NULL, left, right, ret);
}

_g int div_float_fl_heap_(addr left, addr right, addr *ret)
{
	return div_float_fl_alloc_(NULL, left, right, ret);
}

_g int div_float_sf_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &check);
	if (check == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefSingleFloat(left) / ((single_float)check);
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_df_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type righterror");
	GetFixnum(right, &check);
	if (check == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefDoubleFloat(left) / ((double_float)check);
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_lf_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &check);
	if (check == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefLongFloat(left) / ((long_float)check);
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_sf_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_sf_alloc_(local, left, right, ret);
}

_g int div_float_df_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_df_alloc_(local, left, right, ret);
}

_g int div_float_lf_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_lf_alloc_(local, left, right, ret);
}

_g int div_float_sf_heap_(addr left, addr right, addr *ret)
{
	return div_float_sf_alloc_(NULL, left, right, ret);
}

_g int div_float_df_heap_(addr left, addr right, addr *ret)
{
	return div_float_df_alloc_(NULL, left, right, ret);
}

_g int div_float_lf_heap_(addr left, addr right, addr *ret)
{
	return div_float_lf_alloc_(NULL, left, right, ret);
}

_g int div_float_bs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(single_float_bignum_(left, &value2));
	value = value2 / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_bd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(double_float_bignum_(left, &value2));
	value = value2 / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_bl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(long_float_bignum_(left, &value2));
	value = value2 / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_bs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_bs_alloc_(local, left, right, ret);
}

_g int div_float_bd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_bd_alloc_(local, left, right, ret);
}

_g int div_float_bl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_bl_alloc_(local, left, right, ret);
}

_g int div_float_bs_heap_(addr left, addr right, addr *ret)
{
	return div_float_bs_alloc_(NULL, left, right, ret);
}

_g int div_float_bd_heap_(addr left, addr right, addr *ret)
{
	return div_float_bd_alloc_(NULL, left, right, ret);
}

_g int div_float_bl_heap_(addr left, addr right, addr *ret)
{
	return div_float_bl_alloc_(NULL, left, right, ret);
}

_g int div_float_sb_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value, value2;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(single_float_bignum_(right, &value2));
	value = RefSingleFloat(left) / value2;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_db_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value, value2;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(double_float_bignum_(right, &value2));
	value = RefDoubleFloat(left) / value2;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_lb_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value, value2;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetLongFloat(left, &value);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(long_float_bignum_(right, &value2));
	value = RefLongFloat(left) / value2;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_sb_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_sb_alloc_(local, left, right, ret);
}

_g int div_float_db_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_db_alloc_(local, left, right, ret);
}

_g int div_float_lb_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_lb_alloc_(local, left, right, ret);
}

_g int div_float_sb_heap_(addr left, addr right, addr *ret)
{
	return div_float_sb_alloc_(NULL, left, right, ret);
}

_g int div_float_db_heap_(addr left, addr right, addr *ret)
{
	return div_float_db_alloc_(NULL, left, right, ret);
}

_g int div_float_lb_heap_(addr left, addr right, addr *ret)
{
	return div_float_lb_alloc_(NULL, left, right, ret);
}

_g int div_float_rs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value, value2;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(single_float_ratio_(left, &value2));
	value = value2 / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_rd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value, value2;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(double_float_ratio_(left, &value2));
	value = value2 / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_rl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value, value2;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(long_float_ratio_(left, &value2));
	value = value2 / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_rs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_rs_alloc_(local, left, right, ret);
}

_g int div_float_rd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_rd_alloc_(local, left, right, ret);
}

_g int div_float_rl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_rl_alloc_(local, left, right, ret);
}

_g int div_float_rs_heap_(addr left, addr right, addr *ret)
{
	return div_float_rs_alloc_(NULL, left, right, ret);
}

_g int div_float_rd_heap_(addr left, addr right, addr *ret)
{
	return div_float_rd_alloc_(NULL, left, right, ret);
}

_g int div_float_rl_heap_(addr left, addr right, addr *ret)
{
	return div_float_rl_alloc_(NULL, left, right, ret);
}

_g int div_float_sr_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value, value2;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(single_float_ratio_(right, &value2));
	value = RefSingleFloat(left) / value2;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_dr_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value, value2;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(double_float_ratio_(right, &value2));
	value = RefDoubleFloat(left) / value2;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_lr_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value, value2;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	Return(long_float_ratio_(right, &value2));
	value = RefLongFloat(left) / value2;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_sr_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_sr_alloc_(local, left, right, ret);
}

_g int div_float_dr_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_dr_alloc_(local, left, right, ret);
}

_g int div_float_lr_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_lr_alloc_(local, left, right, ret);
}

_g int div_float_sr_heap_(addr left, addr right, addr *ret)
{
	return div_float_sr_alloc_(NULL, left, right, ret);
}

_g int div_float_dr_heap_(addr left, addr right, addr *ret)
{
	return div_float_dr_alloc_(NULL, left, right, ret);
}

_g int div_float_lr_heap_(addr left, addr right, addr *ret)
{
	return div_float_lr_alloc_(NULL, left, right, ret);
}

_g int div_float_ss_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefSingleFloat(left) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_sd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = ((double_float)RefSingleFloat(left)) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_sl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = ((long_float)RefSingleFloat(left)) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_ds_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float check;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &check);
	if (check == 0.0f) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefDoubleFloat(left) / ((double_float)check);
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_dd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefDoubleFloat(left) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_dl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = ((long_float)RefDoubleFloat(left)) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_ls_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float check;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &check);
	if (check == 0.0f) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefLongFloat(left) / ((long_float)check);
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_ld_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float check;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &check);
	if (check == 0.0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefLongFloat(left) / ((long_float)check);
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_ll_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}
	value = RefLongFloat(left) / value;
	Return_float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

_g int div_float_ss_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_ss_alloc_(local, left, right, ret);
}

_g int div_float_sd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_sd_alloc_(local, left, right, ret);
}

_g int div_float_sl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_sl_alloc_(local, left, right, ret);
}

_g int div_float_ds_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_ds_alloc_(local, left, right, ret);
}

_g int div_float_dd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_dd_alloc_(local, left, right, ret);
}

_g int div_float_dl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_dl_alloc_(local, left, right, ret);
}

_g int div_float_ls_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_ls_alloc_(local, left, right, ret);
}

_g int div_float_ld_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_ld_alloc_(local, left, right, ret);
}

_g int div_float_ll_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return div_float_ll_alloc_(local, left, right, ret);
}

_g int div_float_ss_heap_(addr left, addr right, addr *ret)
{
	return div_float_ss_alloc_(NULL, left, right, ret);
}

_g int div_float_sd_heap_(addr left, addr right, addr *ret)
{
	return div_float_sd_alloc_(NULL, left, right, ret);
}

_g int div_float_sl_heap_(addr left, addr right, addr *ret)
{
	return div_float_sl_alloc_(NULL, left, right, ret);
}

_g int div_float_ds_heap_(addr left, addr right, addr *ret)
{
	return div_float_ds_alloc_(NULL, left, right, ret);
}

_g int div_float_dd_heap_(addr left, addr right, addr *ret)
{
	return div_float_dd_alloc_(NULL, left, right, ret);
}

_g int div_float_dl_heap_(addr left, addr right, addr *ret)
{
	return div_float_dl_alloc_(NULL, left, right, ret);
}

_g int div_float_ls_heap_(addr left, addr right, addr *ret)
{
	return div_float_ls_alloc_(NULL, left, right, ret);
}

_g int div_float_ld_heap_(addr left, addr right, addr *ret)
{
	return div_float_ld_alloc_(NULL, left, right, ret);
}

_g int div_float_ll_heap_(addr left, addr right, addr *ret)
{
	return div_float_ll_alloc_(NULL, left, right, ret);
}

