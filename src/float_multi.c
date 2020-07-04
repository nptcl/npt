#include "bignum.h"
#include "bignum_equal.h"
#include "condition.h"
#include "float_object.h"
#include "object.h"
#include "ratio.h"
#include "typedef.h"

/*
 *  multiple
 */
_g void multi_float_fs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = single_float_fixnum(left) * RefSingleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);
}

_g void multi_float_fd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = double_float_fixnum(left) * RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void multi_float_fl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = long_float_fixnum(left) * RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void multi_float_fs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_fs_alloc(local, left, right, ret);
}

_g void multi_float_fd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_fd_alloc(local, left, right, ret);
}

_g void multi_float_fl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_fl_alloc(local, left, right, ret);
}

_g void multi_float_fs_heap(addr left, addr right, addr *ret)
{
	multi_float_fs_alloc(NULL, left, right, ret);
}

_g void multi_float_fd_heap(addr left, addr right, addr *ret)
{
	multi_float_fd_alloc(NULL, left, right, ret);
}

_g void multi_float_fl_heap(addr left, addr right, addr *ret)
{
	multi_float_fl_alloc(NULL, left, right, ret);
}

_g void multi_float_bs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = single_float_bignum(left) * RefSingleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);
}

_g void multi_float_bd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = double_float_bignum(left) * RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void multi_float_bl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = long_float_bignum(left) * RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void multi_float_bs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_bs_alloc(local, left, right, ret);
}

_g void multi_float_bd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_bd_alloc(local, left, right, ret);
}

_g void multi_float_bl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_bl_alloc(local, left, right, ret);
}

_g void multi_float_bs_heap(addr left, addr right, addr *ret)
{
	multi_float_bs_alloc(NULL, left, right, ret);
}

_g void multi_float_bd_heap(addr left, addr right, addr *ret)
{
	multi_float_bd_alloc(NULL, left, right, ret);
}

_g void multi_float_bl_heap(addr left, addr right, addr *ret)
{
	multi_float_bl_alloc(NULL, left, right, ret);
}

_g void multi_float_rs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = single_float_ratio(left) * RefSingleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);
}

_g void multi_float_rd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = double_float_ratio(left) * RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void multi_float_rl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = long_float_ratio(left) * RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void multi_float_rs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_rs_alloc(local, left, right, ret);
}

_g void multi_float_rd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_rd_alloc(local, left, right, ret);
}

_g void multi_float_rl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_rl_alloc(local, left, right, ret);
}

_g void multi_float_rs_heap(addr left, addr right, addr *ret)
{
	multi_float_rs_alloc(NULL, left, right, ret);
}

_g void multi_float_rd_heap(addr left, addr right, addr *ret)
{
	multi_float_rd_alloc(NULL, left, right, ret);
}

_g void multi_float_rl_heap(addr left, addr right, addr *ret)
{
	multi_float_rl_alloc(NULL, left, right, ret);
}

_g void multi_float_ss_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefSingleFloat(left) * RefSingleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);
}

_g void multi_float_sd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = ((double_float)RefSingleFloat(left)) * RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void multi_float_sl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefSingleFloat(left)) * RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void multi_float_ds_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) * ((double_float)RefSingleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void multi_float_dd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) * RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void multi_float_dl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefDoubleFloat(left)) * RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void multi_float_ls_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefLongFloat(left) * ((long_float)RefSingleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void multi_float_ld_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefLongFloat(left) * ((long_float)RefDoubleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void multi_float_ll_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = RefLongFloat(left) * RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void multi_float_ss_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_ss_alloc(local, left, right, ret);
}

_g void multi_float_sd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_sd_alloc(local, left, right, ret);
}

_g void multi_float_sl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_sl_alloc(local, left, right, ret);
}

_g void multi_float_ds_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_ds_alloc(local, left, right, ret);
}

_g void multi_float_dd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_dd_alloc(local, left, right, ret);
}

_g void multi_float_dl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_dl_alloc(local, left, right, ret);
}

_g void multi_float_ls_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_ls_alloc(local, left, right, ret);
}

_g void multi_float_ld_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_ld_alloc(local, left, right, ret);
}

_g void multi_float_ll_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_ll_alloc(local, left, right, ret);
}

_g void multi_float_ss_heap(addr left, addr right, addr *ret)
{
	multi_float_ss_alloc(NULL, left, right, ret);
}

_g void multi_float_sd_heap(addr left, addr right, addr *ret)
{
	multi_float_sd_alloc(NULL, left, right, ret);
}

_g void multi_float_sl_heap(addr left, addr right, addr *ret)
{
	multi_float_sl_alloc(NULL, left, right, ret);
}

_g void multi_float_ds_heap(addr left, addr right, addr *ret)
{
	multi_float_ds_alloc(NULL, left, right, ret);
}

_g void multi_float_dd_heap(addr left, addr right, addr *ret)
{
	multi_float_dd_alloc(NULL, left, right, ret);
}

_g void multi_float_dl_heap(addr left, addr right, addr *ret)
{
	multi_float_dl_alloc(NULL, left, right, ret);
}

_g void multi_float_ls_heap(addr left, addr right, addr *ret)
{
	multi_float_ls_alloc(NULL, left, right, ret);
}

_g void multi_float_ld_heap(addr left, addr right, addr *ret)
{
	multi_float_ld_alloc(NULL, left, right, ret);
}

_g void multi_float_ll_heap(addr left, addr right, addr *ret)
{
	multi_float_ll_alloc(NULL, left, right, ret);
}


/*
 *  inverse
 */
_g void inverse_single_float_alloc(LocalRoot local, addr pos, addr *ret)
{
	single_float value;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	GetSingleFloat(pos, &value);
	if (value == 0.0f)
		division_by_zero1(pos);
	value = 1.0f / value;
	float_errorcheck1(CONSTANT_COMMON_SLASH, value, pos);
	single_float_alloc(local, ret, value);
}

_g void inverse_double_float_alloc(LocalRoot local, addr pos, addr *ret)
{
	double_float value;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(pos, &value);
	if (value == 0.0)
		division_by_zero1(pos);
	value = 1.0 / value;
	float_errorcheck1(CONSTANT_COMMON_SLASH, value, pos);
	double_float_alloc(local, ret, value);
}

_g void inverse_long_float_alloc(LocalRoot local, addr pos, addr *ret)
{
	long_float value;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &value);
	if (value == 0.0L)
		division_by_zero1(pos);
	value = 1.0L / value;
	float_errorcheck1(CONSTANT_COMMON_SLASH, value, pos);
	long_float_alloc(local, ret, value);
}

_g void inverse_single_float_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	inverse_single_float_alloc(local, pos, ret);
}

_g void inverse_double_float_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	inverse_double_float_alloc(local, pos, ret);
}

_g void inverse_long_float_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	inverse_long_float_alloc(local, pos, ret);
}

_g void inverse_single_float_heap(addr pos, addr *ret)
{
	inverse_single_float_alloc(NULL, pos, ret);
}

_g void inverse_double_float_heap(addr pos, addr *ret)
{
	inverse_double_float_alloc(NULL, pos, ret);
}

_g void inverse_long_float_heap(addr pos, addr *ret)
{
	inverse_long_float_alloc(NULL, pos, ret);
}


/*
 *  division
 */
_g void div_float_fs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f)
		division_by_zero2(left, right);
	value = single_float_fixnum(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

_g void div_float_fd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0)
		division_by_zero2(left, right);
	value = double_float_fixnum(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void div_float_fl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L)
		division_by_zero2(left, right);
	value = long_float_fixnum(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void div_float_fs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_fs_alloc(local, left, right, ret);
}

_g void div_float_fd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_fd_alloc(local, left, right, ret);
}

_g void div_float_fl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_fl_alloc(local, left, right, ret);
}

_g void div_float_fs_heap(addr left, addr right, addr *ret)
{
	div_float_fs_alloc(NULL, left, right, ret);
}

_g void div_float_fd_heap(addr left, addr right, addr *ret)
{
	div_float_fd_alloc(NULL, left, right, ret);
}

_g void div_float_fl_heap(addr left, addr right, addr *ret)
{
	div_float_fl_alloc(NULL, left, right, ret);
}

_g void div_float_sf_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &check);
	if (check == 0)
		division_by_zero2(left, right);
	value = RefSingleFloat(left) / ((single_float)check);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

_g void div_float_df_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type righterror");
	GetFixnum(right, &check);
	if (check == 0)
		division_by_zero2(left, right);
	value = RefDoubleFloat(left) / ((double_float)check);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void div_float_lf_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &check);
	if (check == 0)
		division_by_zero2(left, right);
	value = RefLongFloat(left) / ((long_float)check);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void div_float_sf_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_sf_alloc(local, left, right, ret);
}

_g void div_float_df_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_df_alloc(local, left, right, ret);
}

_g void div_float_lf_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_lf_alloc(local, left, right, ret);
}

_g void div_float_sf_heap(addr left, addr right, addr *ret)
{
	div_float_sf_alloc(NULL, left, right, ret);
}

_g void div_float_df_heap(addr left, addr right, addr *ret)
{
	div_float_df_alloc(NULL, left, right, ret);
}

_g void div_float_lf_heap(addr left, addr right, addr *ret)
{
	div_float_lf_alloc(NULL, left, right, ret);
}

_g void div_float_bs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f)
		division_by_zero2(left, right);
	value = single_float_bignum(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

_g void div_float_bd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0)
		division_by_zero2(left, right);
	value = double_float_bignum(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void div_float_bl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L)
		division_by_zero2(left, right);
	value = long_float_bignum(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void div_float_bs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_bs_alloc(local, left, right, ret);
}

_g void div_float_bd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_bd_alloc(local, left, right, ret);
}

_g void div_float_bl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_bl_alloc(local, left, right, ret);
}

_g void div_float_bs_heap(addr left, addr right, addr *ret)
{
	div_float_bs_alloc(NULL, left, right, ret);
}

_g void div_float_bd_heap(addr left, addr right, addr *ret)
{
	div_float_bd_alloc(NULL, left, right, ret);
}

_g void div_float_bl_heap(addr left, addr right, addr *ret)
{
	div_float_bl_alloc(NULL, left, right, ret);
}

_g void div_float_sb_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (zerop_bignum(right))
		division_by_zero2(left, right);
	value = RefSingleFloat(left) / single_float_bignum(right);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

_g void div_float_db_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (zerop_bignum(right))
		division_by_zero2(left, right);
	value = RefDoubleFloat(left) / double_float_bignum(right);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void div_float_lb_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetLongFloat(left, &value);
	if (zerop_bignum(right))
		division_by_zero2(left, right);
	value = RefLongFloat(left) / long_float_bignum(right);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void div_float_sb_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_sb_alloc(local, left, right, ret);
}

_g void div_float_db_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_db_alloc(local, left, right, ret);
}

_g void div_float_lb_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_lb_alloc(local, left, right, ret);
}

_g void div_float_sb_heap(addr left, addr right, addr *ret)
{
	div_float_sb_alloc(NULL, left, right, ret);
}

_g void div_float_db_heap(addr left, addr right, addr *ret)
{
	div_float_db_alloc(NULL, left, right, ret);
}

_g void div_float_lb_heap(addr left, addr right, addr *ret)
{
	div_float_lb_alloc(NULL, left, right, ret);
}

_g void div_float_rs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f)
		division_by_zero2(left, right);
	value = single_float_ratio(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

_g void div_float_rd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0)
		division_by_zero2(left, right);
	value = double_float_ratio(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void div_float_rl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L)
		division_by_zero2(left, right);
	value = long_float_ratio(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void div_float_rs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_rs_alloc(local, left, right, ret);
}

_g void div_float_rd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_rd_alloc(local, left, right, ret);
}

_g void div_float_rl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_rl_alloc(local, left, right, ret);
}

_g void div_float_rs_heap(addr left, addr right, addr *ret)
{
	div_float_rs_alloc(NULL, left, right, ret);
}

_g void div_float_rd_heap(addr left, addr right, addr *ret)
{
	div_float_rd_alloc(NULL, left, right, ret);
}

_g void div_float_rl_heap(addr left, addr right, addr *ret)
{
	div_float_rl_alloc(NULL, left, right, ret);
}

_g void div_float_sr_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	if (zerop_ratio(right))
		division_by_zero2(left, right);
	value = RefSingleFloat(left) / single_float_ratio(right);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

_g void div_float_dr_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	if (zerop_ratio(right))
		division_by_zero2(left, right);
	value = RefDoubleFloat(left) / double_float_ratio(right);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void div_float_lr_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	if (zerop_ratio(right))
		division_by_zero2(left, right);
	value = RefLongFloat(left) / long_float_ratio(right);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void div_float_sr_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_sr_alloc(local, left, right, ret);
}

_g void div_float_dr_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_dr_alloc(local, left, right, ret);
}

_g void div_float_lr_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_lr_alloc(local, left, right, ret);
}

_g void div_float_sr_heap(addr left, addr right, addr *ret)
{
	div_float_sr_alloc(NULL, left, right, ret);
}

_g void div_float_dr_heap(addr left, addr right, addr *ret)
{
	div_float_dr_alloc(NULL, left, right, ret);
}

_g void div_float_lr_heap(addr left, addr right, addr *ret)
{
	div_float_lr_alloc(NULL, left, right, ret);
}

_g void div_float_ss_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f)
		division_by_zero2(left, right);
	value = RefSingleFloat(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

_g void div_float_sd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0)
		division_by_zero2(left, right);
	value = ((double_float)RefSingleFloat(left)) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void div_float_sl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L)
		division_by_zero2(left, right);
	value = ((long_float)RefSingleFloat(left)) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void div_float_ds_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float check;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &check);
	if (check == 0.0f)
		division_by_zero2(left, right);
	value = RefDoubleFloat(left) / ((double_float)check);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void div_float_dd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0)
		division_by_zero2(left, right);
	value = RefDoubleFloat(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void div_float_dl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L)
		division_by_zero2(left, right);
	value = ((long_float)RefDoubleFloat(left)) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void div_float_ls_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float check;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &check);
	if (check == 0.0f)
		division_by_zero2(left, right);
	value = RefLongFloat(left) / ((long_float)check);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void div_float_ld_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float check;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &check);
	if (check == 0.0)
		division_by_zero2(left, right);
	value = RefLongFloat(left) / ((long_float)check);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void div_float_ll_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L)
		division_by_zero2(left, right);
	value = RefLongFloat(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void div_float_ss_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_ss_alloc(local, left, right, ret);
}

_g void div_float_sd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_sd_alloc(local, left, right, ret);
}

_g void div_float_sl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_sl_alloc(local, left, right, ret);
}

_g void div_float_ds_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_ds_alloc(local, left, right, ret);
}

_g void div_float_dd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_dd_alloc(local, left, right, ret);
}

_g void div_float_dl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_dl_alloc(local, left, right, ret);
}

_g void div_float_ls_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_ls_alloc(local, left, right, ret);
}

_g void div_float_ld_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_ld_alloc(local, left, right, ret);
}

_g void div_float_ll_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_ll_alloc(local, left, right, ret);
}

_g void div_float_ss_heap(addr left, addr right, addr *ret)
{
	div_float_ss_alloc(NULL, left, right, ret);
}

_g void div_float_sd_heap(addr left, addr right, addr *ret)
{
	div_float_sd_alloc(NULL, left, right, ret);
}

_g void div_float_sl_heap(addr left, addr right, addr *ret)
{
	div_float_sl_alloc(NULL, left, right, ret);
}

_g void div_float_ds_heap(addr left, addr right, addr *ret)
{
	div_float_ds_alloc(NULL, left, right, ret);
}

_g void div_float_dd_heap(addr left, addr right, addr *ret)
{
	div_float_dd_alloc(NULL, left, right, ret);
}

_g void div_float_dl_heap(addr left, addr right, addr *ret)
{
	div_float_dl_alloc(NULL, left, right, ret);
}

_g void div_float_ls_heap(addr left, addr right, addr *ret)
{
	div_float_ls_alloc(NULL, left, right, ret);
}

_g void div_float_ld_heap(addr left, addr right, addr *ret)
{
	div_float_ld_alloc(NULL, left, right, ret);
}

_g void div_float_ll_heap(addr left, addr right, addr *ret)
{
	div_float_ll_alloc(NULL, left, right, ret);
}


/*
 *  multi
 */
static void multi_single_float_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			multi_float_ss_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_sd_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_sl_alloc(local, left, right, ret);
			break;

		default:
			TypeError(right, FLOAT);
			break;
	}
}

static void multi_double_float_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			multi_float_ds_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_dd_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_dl_alloc(local, left, right, ret);
			break;

		default:
			TypeError(right, FLOAT);
			break;
	}
}

static void multi_long_float_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			multi_float_ls_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_ld_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_ll_alloc(local, left, right, ret);
			break;

		default:
			TypeError(right, FLOAT);
			break;
	}
}

_g void multi_float_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_SINGLE_FLOAT:
			multi_single_float_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_double_float_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_long_float_alloc(local, left, right, ret);
			break;

		default:
			TypeError(left, FLOAT);
			break;
	}
}

_g void multi_float_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_alloc(local, left, right, ret);
}

_g void multi_float_heap(LocalRoot local, addr left, addr right, addr *ret)
{
	multi_float_alloc(NULL, left, right, ret);
}

