#include "bignum.h"
#include "bignum_equal.h"
#include "condition.h"
#include "float_object.h"
#include "object.h"
#include "ratio.h"
#include "typedef.h"

/*
 *  plus/minus value
 */
_g void plus_float_sv_alloc(LocalRoot local, addr left, single_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type error");
	value = RefSingleFloat(left) + right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		single_float_heap(&temp, right);
		float_fltclass(CONSTANT_COMMON_PLUS, type, left, temp, NULL);
	}
	single_float_alloc(local, ret, value);
}

_g void plus_float_dv_alloc(LocalRoot local, addr left, double_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type error");
	value = RefDoubleFloat(left) + right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		double_float_heap(&temp, right);
		float_fltclass(CONSTANT_COMMON_PLUS, type, left, temp, NULL);
	}
	double_float_alloc(local, ret, value);
}

_g void plus_float_lv_alloc(LocalRoot local, addr left, long_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type error");
	value = RefLongFloat(left) + right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&temp, right);
		float_fltclass(CONSTANT_COMMON_PLUS, type, left, temp, NULL);
	}
	long_float_alloc(local, ret, value);
}

_g void plus_float_sv_local(LocalRoot local, addr left, single_float right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_sv_alloc(local, left, right, ret);
}

_g void plus_float_dv_local(LocalRoot local, addr left, double_float right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_dv_alloc(local, left, right, ret);
}

_g void plus_float_lv_local(LocalRoot local, addr left, long_float right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_lv_alloc(local, left, right, ret);
}

_g void plus_float_sv_heap(addr left, single_float right, addr *ret)
{
	plus_float_sv_alloc(NULL, left, right, ret);
}

_g void plus_float_dv_heap(addr left, double_float right, addr *ret)
{
	plus_float_dv_alloc(NULL, left, right, ret);
}

_g void plus_float_lv_heap(addr left, long_float right, addr *ret)
{
	plus_float_lv_alloc(NULL, left, right, ret);
}

_g void minus_float_sv_alloc(LocalRoot local, addr left, single_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type error");
	value = RefSingleFloat(left) - right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		single_float_heap(&temp, right);
		float_fltclass(CONSTANT_COMMON_MINUS, type, left, temp, NULL);
	}
	single_float_alloc(local, ret, value);
}

_g void minus_float_dv_alloc(LocalRoot local, addr left, double_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type error");
	value = RefDoubleFloat(left) - right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		double_float_heap(&temp, right);
		float_fltclass(CONSTANT_COMMON_MINUS, type, left, temp, NULL);
	}
	double_float_alloc(local, ret, value);
}

_g void minus_float_lv_alloc(LocalRoot local, addr left, long_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type error");
	value = RefLongFloat(left) - right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&temp, right);
		float_fltclass(CONSTANT_COMMON_MINUS, type, left, temp, NULL);
	}
	long_float_alloc(local, ret, value);
}

_g void minus_float_sv_local(LocalRoot local, addr left, single_float right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_sv_alloc(local, left, right, ret);
}

_g void minus_float_dv_local(LocalRoot local, addr left, double_float right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_dv_alloc(local, left, right, ret);
}

_g void minus_float_lv_local(LocalRoot local, addr left, long_float right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_lv_alloc(local, left, right, ret);
}

_g void minus_float_sv_heap(addr left, single_float right, addr *ret)
{
	minus_float_sv_alloc(NULL, left, right, ret);
}

_g void minus_float_dv_heap(addr left, double_float right, addr *ret)
{
	minus_float_dv_alloc(NULL, left, right, ret);
}

_g void minus_float_lv_heap(addr left, long_float right, addr *ret)
{
	minus_float_lv_alloc(NULL, left, right, ret);
}

_g void minus_float_vs_alloc(LocalRoot local, single_float left, addr right, addr *ret)
{
	fltclasstype type;
	addr temp;
	single_float value;

	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type error");
	value = left - RefSingleFloat(right);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		single_float_heap(&temp, left);
		float_fltclass(CONSTANT_COMMON_MINUS, type, temp, right, NULL);
	}
	single_float_alloc(local, ret, value);
}

_g void minus_float_vd_alloc(LocalRoot local, double_float left, addr right, addr *ret)
{
	fltclasstype type;
	addr temp;
	double_float value;

	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type error");
	value = left - RefDoubleFloat(right);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		double_float_heap(&temp, left);
		float_fltclass(CONSTANT_COMMON_MINUS, type, temp, right, NULL);
	}
	double_float_alloc(local, ret, value);
}

_g void minus_float_vl_alloc(LocalRoot local, long_float left, addr right, addr *ret)
{
	fltclasstype type;
	addr temp;
	long_float value;

	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type error");
	value = left - RefLongFloat(right);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&temp, left);
		float_fltclass(CONSTANT_COMMON_MINUS, type, temp, right, NULL);
	}
	long_float_alloc(local, ret, value);
}

_g void minus_float_vs_local(LocalRoot local, single_float left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_vs_alloc(local, left, right, ret);
}

_g void minus_float_vd_local(LocalRoot local, double_float left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_vd_alloc(local, left, right, ret);
}

_g void minus_float_vl_local(LocalRoot local, long_float left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_vl_alloc(local, left, right, ret);
}

_g void minus_float_vs_heap(single_float left, addr right, addr *ret)
{
	minus_float_vs_alloc(NULL, left, right, ret);
}

_g void minus_float_vd_heap(double_float left, addr right, addr *ret)
{
	minus_float_vd_alloc(NULL, left, right, ret);
}

_g void minus_float_vl_heap(long_float left, addr right, addr *ret)
{
	minus_float_vl_alloc(NULL, left, right, ret);
}

_g void sign_reverse_floats_alloc(LocalRoot local, addr value, addr *ret)
{
	single_float_alloc(local, ret, -RefSingleFloat(value));
}

_g void sign_reverse_floatd_alloc(LocalRoot local, addr value, addr *ret)
{
	double_float_alloc(local, ret, -RefDoubleFloat(value));
}

_g void sign_reverse_floatl_alloc(LocalRoot local, addr value, addr *ret)
{
	long_float_alloc(local, ret, -RefLongFloat(value));
}

_g void sign_reverse_floats_local(LocalRoot local, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	sign_reverse_floats_alloc(local, value, ret);
}

_g void sign_reverse_floatd_local(LocalRoot local, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	sign_reverse_floatd_alloc(local, value, ret);
}

_g void sign_reverse_floatl_local(LocalRoot local, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	sign_reverse_floatl_alloc(local, value, ret);
}

_g void sign_reverse_floats_heap(addr value, addr *ret)
{
	sign_reverse_floats_alloc(NULL, value, ret);
}

_g void sign_reverse_floatd_heap(addr value, addr *ret)
{
	sign_reverse_floatd_alloc(NULL, value, ret);
}

_g void sign_reverse_floatl_heap(addr value, addr *ret)
{
	sign_reverse_floatl_alloc(NULL, value, ret);
}


/*
 *  plus fixnum
 */
_g void plus_float_fs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0)
		single_float_throw_alloc(local, right, ret);
	else
		plus_float_sv_alloc(local, right, (single_float)value, ret);
}

_g void plus_float_fd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0)
		double_float_throw_alloc(local, right, ret);
	else
		plus_float_dv_alloc(local, right, (double_float)value, ret);
}

_g void plus_float_fl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0)
		long_float_throw_alloc(local, right, ret);
	else
		plus_float_lv_alloc(local, right, (long_float)value, ret);
}

_g void plus_float_fs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_fs_alloc(local, left, right, ret);
}

_g void plus_float_fd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_fd_alloc(local, left, right, ret);
}

_g void plus_float_fl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_fl_alloc(local, left, right, ret);
}

_g void plus_float_fs_heap(addr left, addr right, addr *ret)
{
	plus_float_fs_alloc(NULL, left, right, ret);
}

_g void plus_float_fd_heap(addr left, addr right, addr *ret)
{
	plus_float_fd_alloc(NULL, left, right, ret);
}

_g void plus_float_fl_heap(addr left, addr right, addr *ret)
{
	plus_float_fl_alloc(NULL, left, right, ret);
}

_g void plus_float_bs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	if (zerop_bignum(left))
		single_float_throw_alloc(local, right, ret);
	else
		plus_float_sv_alloc(local, right, single_float_bignum(left), ret);
}

_g void plus_float_bd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_bignum(left))
		double_float_throw_alloc(local, right, ret);
	else
		plus_float_dv_alloc(local, right, double_float_bignum(left), ret);
}

_g void plus_float_bl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_bignum(left))
		long_float_throw_alloc(local, right, ret);
	else
		plus_float_lv_alloc(local, right, long_float_bignum(left), ret);
}

_g void plus_float_bs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_bs_alloc(local, left, right, ret);
}

_g void plus_float_bd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_bd_alloc(local, left, right, ret);
}

_g void plus_float_bl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_bl_alloc(local, left, right, ret);
}

_g void plus_float_bs_heap(addr left, addr right, addr *ret)
{
	plus_float_bs_alloc(NULL, left, right, ret);
}

_g void plus_float_bd_heap(addr left, addr right, addr *ret)
{
	plus_float_bd_alloc(NULL, left, right, ret);
}

_g void plus_float_bl_heap(addr left, addr right, addr *ret)
{
	plus_float_bl_alloc(NULL, left, right, ret);
}

_g void plus_float_rs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	if (zerop_ratio(left))
		single_float_throw_alloc(local, right, ret);
	else
		plus_float_sv_alloc(local, right, single_float_ratio(left), ret);
}

_g void plus_float_rd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_ratio(left))
		double_float_throw_alloc(local, right, ret);
	else
		plus_float_dv_alloc(local, right, double_float_ratio(left), ret);
}

_g void plus_float_rl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_ratio(left))
		long_float_throw_alloc(local, right, ret);
	else
		plus_float_lv_alloc(local, right, long_float_ratio(left), ret);
}

_g void plus_float_rs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_rs_alloc(local, left, right, ret);
}

_g void plus_float_rd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_rd_alloc(local, left, right, ret);
}

_g void plus_float_rl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_rl_alloc(local, left, right, ret);
}

_g void plus_float_rs_heap(addr left, addr right, addr *ret)
{
	plus_float_rs_alloc(NULL, left, right, ret);
}

_g void plus_float_rd_heap(addr left, addr right, addr *ret)
{
	plus_float_rd_alloc(NULL, left, right, ret);
}

_g void plus_float_rl_heap(addr left, addr right, addr *ret)
{
	plus_float_rl_alloc(NULL, left, right, ret);
}

_g void plus_float_ss_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefSingleFloat(left) + RefSingleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	single_float_alloc(local, ret, value);
}

_g void plus_float_sd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = ((double_float)RefSingleFloat(left)) + RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void plus_float_sl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefSingleFloat(left)) + RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void plus_float_ds_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) + ((double_float)RefSingleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void plus_float_dd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) + RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void plus_float_dl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefDoubleFloat(left)) + RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void plus_float_ls_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefLongFloat(left) + ((long_float)RefSingleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void plus_float_ld_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefLongFloat(left) + ((long_float)RefDoubleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void plus_float_ll_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = RefLongFloat(left) + RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void plus_float_ss_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_ss_alloc(local, left, right, ret);
}

_g void plus_float_sd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_sd_alloc(local, left, right, ret);
}

_g void plus_float_sl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_sl_alloc(local, left, right, ret);
}

_g void plus_float_ds_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_ds_alloc(local, left, right, ret);
}

_g void plus_float_dd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_dd_alloc(local, left, right, ret);
}

_g void plus_float_dl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_dl_alloc(local, left, right, ret);
}

_g void plus_float_ls_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_ls_alloc(local, left, right, ret);
}

_g void plus_float_ld_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_ld_alloc(local, left, right, ret);
}

_g void plus_float_ll_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_ll_alloc(local, left, right, ret);
}

_g void plus_float_ss_heap(addr left, addr right, addr *ret)
{
	plus_float_ss_alloc(NULL, left, right, ret);
}

_g void plus_float_sd_heap(addr left, addr right, addr *ret)
{
	plus_float_sd_alloc(NULL, left, right, ret);
}

_g void plus_float_sl_heap(addr left, addr right, addr *ret)
{
	plus_float_sl_alloc(NULL, left, right, ret);
}

_g void plus_float_ds_heap(addr left, addr right, addr *ret)
{
	plus_float_ds_alloc(NULL, left, right, ret);
}

_g void plus_float_dd_heap(addr left, addr right, addr *ret)
{
	plus_float_dd_alloc(NULL, left, right, ret);
}

_g void plus_float_dl_heap(addr left, addr right, addr *ret)
{
	plus_float_dl_alloc(NULL, left, right, ret);
}

_g void plus_float_ls_heap(addr left, addr right, addr *ret)
{
	plus_float_ls_alloc(NULL, left, right, ret);
}

_g void plus_float_ld_heap(addr left, addr right, addr *ret)
{
	plus_float_ld_alloc(NULL, left, right, ret);
}

_g void plus_float_ll_heap(addr left, addr right, addr *ret)
{
	plus_float_ll_alloc(NULL, left, right, ret);
}


/*
 *  minus
 */
_g void minus_float_fs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0)
		sign_reverse_floats_heap(right, ret);
	else
		minus_float_vs_alloc(local, (single_float)value, right, ret);
}

_g void minus_float_fd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0)
		sign_reverse_floatd_heap(right, ret);
	else
		minus_float_vd_alloc(local, (double_float)value, right, ret);
}

_g void minus_float_fl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0)
		sign_reverse_floatl_heap(right, ret);
	else
		minus_float_vl_alloc(local, (long_float)value, right, ret);
}

_g void minus_float_fs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_fs_alloc(local, left, right, ret);
}

_g void minus_float_fd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_fd_alloc(local, left, right, ret);
}

_g void minus_float_fl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_fl_alloc(local, left, right, ret);
}

_g void minus_float_fs_heap(addr left, addr right, addr *ret)
{
	minus_float_fs_alloc(NULL, left, right, ret);
}

_g void minus_float_fd_heap(addr left, addr right, addr *ret)
{
	minus_float_fd_alloc(NULL, left, right, ret);
}

_g void minus_float_fl_heap(addr left, addr right, addr *ret)
{
	minus_float_fl_alloc(NULL, left, right, ret);
}

_g void minus_float_sf_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &value);
	if (value == 0)
		single_float_throw_alloc(local, left, ret);
	else
		minus_float_sv_alloc(local, left, (single_float)value, ret);
}

_g void minus_float_df_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &value);
	if (value == 0)
		double_float_throw_alloc(local, left, ret);
	else
		minus_float_dv_alloc(local, left, (double_float)value, ret);
}

_g void minus_float_lf_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &value);
	if (value == 0)
		long_float_throw_alloc(local, left, ret);
	else
		minus_float_lv_alloc(local, left, (long_float)value, ret);
}

_g void minus_float_sf_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_sf_alloc(local, left, right, ret);
}

_g void minus_float_df_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_df_alloc(local, left, right, ret);
}

_g void minus_float_lf_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_lf_alloc(local, left, right, ret);
}

_g void minus_float_sf_heap(addr left, addr right, addr *ret)
{
	minus_float_sf_alloc(NULL, left, right, ret);
}

_g void minus_float_df_heap(addr left, addr right, addr *ret)
{
	minus_float_df_alloc(NULL, left, right, ret);
}

_g void minus_float_lf_heap(addr left, addr right, addr *ret)
{
	minus_float_lf_alloc(NULL, left, right, ret);
}

_g void minus_float_bs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type right error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type left error");
	if (zerop_bignum(left))
		sign_reverse_floats_heap(right, ret);
	else
		minus_float_vs_alloc(local, single_float_bignum(left), right, ret);
}

_g void minus_float_bd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_bignum(left))
		sign_reverse_floatd_heap(right, ret);
	else
		minus_float_vd_alloc(local, double_float_bignum(left), right, ret);
}

_g void minus_float_bl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_bignum(left))
		sign_reverse_floatl_heap(right, ret);
	else
		minus_float_vl_alloc(local, long_float_bignum(left), right, ret);
}

_g void minus_float_bs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_bs_alloc(local, left, right, ret);
}

_g void minus_float_bd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_bd_alloc(local, left, right, ret);
}

_g void minus_float_bl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_bl_alloc(local, left, right, ret);
}

_g void minus_float_bs_heap(addr left, addr right, addr *ret)
{
	minus_float_bs_alloc(NULL, left, right, ret);
}

_g void minus_float_bd_heap(addr left, addr right, addr *ret)
{
	minus_float_bd_alloc(NULL, left, right, ret);
}

_g void minus_float_bl_heap(addr left, addr right, addr *ret)
{
	minus_float_bl_alloc(NULL, left, right, ret);
}

_g void minus_float_sb_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	minus_float_sv_alloc(local, left, single_float_bignum(right), ret);
}

_g void minus_float_db_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	minus_float_dv_alloc(local, left, double_float_bignum(right), ret);
}

_g void minus_float_lb_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	minus_float_lv_alloc(local, left, long_float_bignum(right), ret);
}

_g void minus_float_sb_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_sb_alloc(local, left, right, ret);
}

_g void minus_float_db_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_db_alloc(local, left, right, ret);
}

_g void minus_float_lb_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_lb_alloc(local, left, right, ret);
}

_g void minus_float_sb_heap(addr left, addr right, addr *ret)
{
	minus_float_sb_alloc(NULL, left, right, ret);
}

_g void minus_float_db_heap(addr left, addr right, addr *ret)
{
	minus_float_db_alloc(NULL, left, right, ret);
}

_g void minus_float_lb_heap(addr left, addr right, addr *ret)
{
	minus_float_lb_alloc(NULL, left, right, ret);
}

_g void minus_float_rs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	if (zerop_ratio(left))
		sign_reverse_floats_heap(right, ret);
	else
		minus_float_vs_alloc(local, single_float_ratio(left), right, ret);
}

_g void minus_float_rd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_ratio(left))
		sign_reverse_floatd_heap(right, ret);
	else
		minus_float_vd_alloc(local, double_float_ratio(left), right, ret);
}

_g void minus_float_rl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_ratio(left))
		sign_reverse_floatl_heap(right, ret);
	else
		minus_float_vl_alloc(local, long_float_ratio(left), right, ret);
}

_g void minus_float_rs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_rs_alloc(local, left, right, ret);
}

_g void minus_float_rd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_rd_alloc(local, left, right, ret);
}

_g void minus_float_rl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_rl_alloc(local, left, right, ret);
}

_g void minus_float_rs_heap(addr left, addr right, addr *ret)
{
	minus_float_rs_alloc(NULL, left, right, ret);
}

_g void minus_float_rd_heap(addr left, addr right, addr *ret)
{
	minus_float_rd_alloc(NULL, left, right, ret);
}

_g void minus_float_rl_heap(addr left, addr right, addr *ret)
{
	minus_float_rl_alloc(NULL, left, right, ret);
}

_g void minus_float_sr_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	minus_float_sv_alloc(local, left, single_float_ratio(right), ret);
}

_g void minus_float_dr_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	minus_float_dv_alloc(local, left, double_float_ratio(right), ret);
}

_g void minus_float_lr_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	minus_float_lv_alloc(local, left, long_float_ratio(right), ret);
}

_g void minus_float_sr_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_sr_alloc(local, left, right, ret);
}

_g void minus_float_dr_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_dr_alloc(local, left, right, ret);
}

_g void minus_float_lr_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_lr_alloc(local, left, right, ret);
}

_g void minus_float_sr_heap(addr left, addr right, addr *ret)
{
	minus_float_sr_alloc(NULL, left, right, ret);
}

_g void minus_float_dr_heap(addr left, addr right, addr *ret)
{
	minus_float_dr_alloc(NULL, left, right, ret);
}

_g void minus_float_lr_heap(addr left, addr right, addr *ret)
{
	minus_float_lr_alloc(NULL, left, right, ret);
}

_g void minus_float_ss_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefSingleFloat(left) - RefSingleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	single_float_alloc(local, ret, value);
}

_g void minus_float_sd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = ((double_float)RefSingleFloat(left)) - RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void minus_float_sl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefSingleFloat(left)) - RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void minus_float_ds_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) - ((double_float)RefSingleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void minus_float_dd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) - RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	double_float_alloc(local, ret, value);
}

_g void minus_float_dl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefDoubleFloat(left)) - RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void minus_float_ls_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefLongFloat(left) - ((long_float)RefSingleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void minus_float_ld_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefLongFloat(left) - ((long_float)RefDoubleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void minus_float_ll_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = RefLongFloat(left) - RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);
}

_g void minus_float_ss_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_ss_alloc(local, left, right, ret);
}

_g void minus_float_sd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_sd_alloc(local, left, right, ret);
}

_g void minus_float_sl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_sl_alloc(local, left, right, ret);
}

_g void minus_float_ds_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_ds_alloc(local, left, right, ret);
}

_g void minus_float_dd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_dd_alloc(local, left, right, ret);
}

_g void minus_float_dl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_dl_alloc(local, left, right, ret);
}

_g void minus_float_ls_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_ls_alloc(local, left, right, ret);
}

_g void minus_float_ld_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_ld_alloc(local, left, right, ret);
}

_g void minus_float_ll_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_ll_alloc(local, left, right, ret);
}

_g void minus_float_ss_heap(addr left, addr right, addr *ret)
{
	minus_float_ss_alloc(NULL, left, right, ret);
}

_g void minus_float_sd_heap(addr left, addr right, addr *ret)
{
	minus_float_sd_alloc(NULL, left, right, ret);
}

_g void minus_float_sl_heap(addr left, addr right, addr *ret)
{
	minus_float_sl_alloc(NULL, left, right, ret);
}

_g void minus_float_ds_heap(addr left, addr right, addr *ret)
{
	minus_float_ds_alloc(NULL, left, right, ret);
}

_g void minus_float_dd_heap(addr left, addr right, addr *ret)
{
	minus_float_dd_alloc(NULL, left, right, ret);
}

_g void minus_float_dl_heap(addr left, addr right, addr *ret)
{
	minus_float_dl_alloc(NULL, left, right, ret);
}

_g void minus_float_ls_heap(addr left, addr right, addr *ret)
{
	minus_float_ls_alloc(NULL, left, right, ret);
}

_g void minus_float_ld_heap(addr left, addr right, addr *ret)
{
	minus_float_ld_alloc(NULL, left, right, ret);
}

_g void minus_float_ll_heap(addr left, addr right, addr *ret)
{
	minus_float_ll_alloc(NULL, left, right, ret);
}


/*
 *  plus
 */
static void plus_single_float_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			plus_float_ss_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_sd_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_sl_alloc(local, left, right, ret);
			break;

		default:
			TypeError(right, FLOAT);
			break;
	}
}

static void plus_double_float_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			plus_float_ds_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_dd_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_dl_alloc(local, left, right, ret);
			break;

		default:
			TypeError(right, FLOAT);
			break;
	}
}

static void plus_long_float_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_SINGLE_FLOAT:
			plus_float_ls_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_ld_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_ll_alloc(local, left, right, ret);
			break;

		default:
			TypeError(right, FLOAT);
			break;
	}
}

_g void plus_float_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_SINGLE_FLOAT:
			plus_single_float_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_double_float_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_long_float_alloc(local, left, right, ret);
			break;

		default:
			TypeError(left, FLOAT);
			break;
	}
}

_g void plus_float_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_alloc(local, left, right, ret);
}

_g void plus_float_heap(LocalRoot local, addr left, addr right, addr *ret)
{
	plus_float_alloc(NULL, left, right, ret);
}

