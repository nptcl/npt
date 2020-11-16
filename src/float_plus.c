#include "bignum.h"
#include "bignum_equal.h"
#include "condition.h"
#include "float_object.h"
#include "float_plus.h"
#include "object.h"
#include "ratio.h"
#include "typedef.h"

/*
 *  plus/minus value
 */
int plus_float_sv_alloc_(LocalRoot local, addr left, single_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type error");
	value = RefSingleFloat(left) + right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		single_float_heap(&temp, right);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_PLUS, type, left, temp, NULL);
	}
	single_float_alloc(local, ret, value);

	return 0;
}

int plus_float_dv_alloc_(LocalRoot local, addr left, double_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type error");
	value = RefDoubleFloat(left) + right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		double_float_heap(&temp, right);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_PLUS, type, left, temp, NULL);
	}
	double_float_alloc(local, ret, value);

	return 0;
}

int plus_float_lv_alloc_(LocalRoot local, addr left, long_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type error");
	value = RefLongFloat(left) + right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&temp, right);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_PLUS, type, left, temp, NULL);
	}
	long_float_alloc(local, ret, value);

	return 0;
}

int plus_float_sv_local_(LocalRoot local, addr left, single_float right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_sv_alloc_(local, left, right, ret);
}

int plus_float_dv_local_(LocalRoot local, addr left, double_float right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_dv_alloc_(local, left, right, ret);
}

int plus_float_lv_local_(LocalRoot local, addr left, long_float right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_lv_alloc_(local, left, right, ret);
}

int plus_float_sv_heap_(addr left, single_float right, addr *ret)
{
	return plus_float_sv_alloc_(NULL, left, right, ret);
}

int plus_float_dv_heap_(addr left, double_float right, addr *ret)
{
	return plus_float_dv_alloc_(NULL, left, right, ret);
}

int plus_float_lv_heap_(addr left, long_float right, addr *ret)
{
	return plus_float_lv_alloc_(NULL, left, right, ret);
}

int minus_float_sv_alloc_(LocalRoot local, addr left, single_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type error");
	value = RefSingleFloat(left) - right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		single_float_heap(&temp, right);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_MINUS, type, left, temp, NULL);
	}
	single_float_alloc(local, ret, value);

	return 0;
}

int minus_float_dv_alloc_(LocalRoot local, addr left, double_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type error");
	value = RefDoubleFloat(left) - right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		double_float_heap(&temp, right);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_MINUS, type, left, temp, NULL);
	}
	double_float_alloc(local, ret, value);

	return 0;
}

int minus_float_lv_alloc_(LocalRoot local, addr left, long_float right, addr *ret)
{
	fltclasstype type;
	addr temp;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type error");
	value = RefLongFloat(left) - right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&temp, right);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_MINUS, type, left, temp, NULL);
	}
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_sv_local_(LocalRoot local, addr left, single_float right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_sv_alloc_(local, left, right, ret);
}

int minus_float_dv_local_(LocalRoot local, addr left, double_float right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_dv_alloc_(local, left, right, ret);
}

int minus_float_lv_local_(LocalRoot local, addr left, long_float right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_lv_alloc_(local, left, right, ret);
}

int minus_float_sv_heap_(addr left, single_float right, addr *ret)
{
	return minus_float_sv_alloc_(NULL, left, right, ret);
}

int minus_float_dv_heap_(addr left, double_float right, addr *ret)
{
	return minus_float_dv_alloc_(NULL, left, right, ret);
}

int minus_float_lv_heap_(addr left, long_float right, addr *ret)
{
	return minus_float_lv_alloc_(NULL, left, right, ret);
}

int minus_float_vs_alloc_(LocalRoot local, single_float left, addr right, addr *ret)
{
	fltclasstype type;
	addr temp;
	single_float value;

	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type error");
	value = left - RefSingleFloat(right);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		single_float_heap(&temp, left);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_MINUS, type, temp, right, NULL);
	}
	single_float_alloc(local, ret, value);

	return 0;
}

int minus_float_vd_alloc_(LocalRoot local, double_float left, addr right, addr *ret)
{
	fltclasstype type;
	addr temp;
	double_float value;

	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type error");
	value = left - RefDoubleFloat(right);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		double_float_heap(&temp, left);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_MINUS, type, temp, right, NULL);
	}
	double_float_alloc(local, ret, value);

	return 0;
}

int minus_float_vl_alloc_(LocalRoot local, long_float left, addr right, addr *ret)
{
	fltclasstype type;
	addr temp;
	long_float value;

	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type error");
	value = left - RefLongFloat(right);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&temp, left);
		*ret = Nil;
		return float_fltclass_(CONSTANT_COMMON_MINUS, type, temp, right, NULL);
	}
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_vs_local_(LocalRoot local, single_float left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_vs_alloc_(local, left, right, ret);
}

int minus_float_vd_local_(LocalRoot local, double_float left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_vd_alloc_(local, left, right, ret);
}

int minus_float_vl_local_(LocalRoot local, long_float left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_vl_alloc_(local, left, right, ret);
}

int minus_float_vs_heap_(single_float left, addr right, addr *ret)
{
	return minus_float_vs_alloc_(NULL, left, right, ret);
}

int minus_float_vd_heap_(double_float left, addr right, addr *ret)
{
	return minus_float_vd_alloc_(NULL, left, right, ret);
}

int minus_float_vl_heap_(long_float left, addr right, addr *ret)
{
	return minus_float_vl_alloc_(NULL, left, right, ret);
}

void sign_reverse_floats_alloc(LocalRoot local, addr value, addr *ret)
{
	single_float_alloc(local, ret, -RefSingleFloat(value));
}

void sign_reverse_floatd_alloc(LocalRoot local, addr value, addr *ret)
{
	double_float_alloc(local, ret, -RefDoubleFloat(value));
}

void sign_reverse_floatl_alloc(LocalRoot local, addr value, addr *ret)
{
	long_float_alloc(local, ret, -RefLongFloat(value));
}

void sign_reverse_floats_local(LocalRoot local, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	sign_reverse_floats_alloc(local, value, ret);
}

void sign_reverse_floatd_local(LocalRoot local, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	sign_reverse_floatd_alloc(local, value, ret);
}

void sign_reverse_floatl_local(LocalRoot local, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	sign_reverse_floatl_alloc(local, value, ret);
}

void sign_reverse_floats_heap(addr value, addr *ret)
{
	sign_reverse_floats_alloc(NULL, value, ret);
}

void sign_reverse_floatd_heap(addr value, addr *ret)
{
	sign_reverse_floatd_alloc(NULL, value, ret);
}

void sign_reverse_floatl_heap(addr value, addr *ret)
{
	sign_reverse_floatl_alloc(NULL, value, ret);
}


/*
 *  plus fixnum
 */
int plus_float_fs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0) {
		single_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		return plus_float_sv_alloc_(local, right, (single_float)value, ret);
	}
}

int plus_float_fd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0) {
		double_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		return plus_float_dv_alloc_(local, right, (double_float)value, ret);
	}
}

int plus_float_fl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0) {
		long_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		return plus_float_lv_alloc_(local, right, (long_float)value, ret);
	}
}

int plus_float_fs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_fs_alloc_(local, left, right, ret);
}

int plus_float_fd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_fd_alloc_(local, left, right, ret);
}

int plus_float_fl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_fl_alloc_(local, left, right, ret);
}

int plus_float_fs_heap_(addr left, addr right, addr *ret)
{
	return plus_float_fs_alloc_(NULL, left, right, ret);
}

int plus_float_fd_heap_(addr left, addr right, addr *ret)
{
	return plus_float_fd_alloc_(NULL, left, right, ret);
}

int plus_float_fl_heap_(addr left, addr right, addr *ret)
{
	return plus_float_fl_alloc_(NULL, left, right, ret);
}

int plus_float_bs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	if (zerop_bignum(left)) {
		single_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		Return(single_float_bignum_(left, &value));
		return plus_float_sv_alloc_(local, right, value, ret);
	}
}

int plus_float_bd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_bignum(left)) {
		double_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		Return(double_float_bignum_(left, &value));
		return plus_float_dv_alloc_(local, right, value, ret);
	}
}

int plus_float_bl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_bignum(left)) {
		long_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		Return(long_float_bignum_(left, &value));
		return plus_float_lv_alloc_(local, right, value, ret);
	}
}

int plus_float_bs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_bs_alloc_(local, left, right, ret);
}

int plus_float_bd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_bd_alloc_(local, left, right, ret);
}

int plus_float_bl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_bl_alloc_(local, left, right, ret);
}

int plus_float_bs_heap_(addr left, addr right, addr *ret)
{
	return plus_float_bs_alloc_(NULL, left, right, ret);
}

int plus_float_bd_heap_(addr left, addr right, addr *ret)
{
	return plus_float_bd_alloc_(NULL, left, right, ret);
}

int plus_float_bl_heap_(addr left, addr right, addr *ret)
{
	return plus_float_bl_alloc_(NULL, left, right, ret);
}

int plus_float_rs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	if (zerop_ratio(left)) {
		single_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		Return(single_float_ratio_(left, &value));
		return plus_float_sv_alloc_(local, right, value, ret);
	}
}

int plus_float_rd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_ratio(left)) {
		double_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		Return(double_float_ratio_(left, &value));
		return plus_float_dv_alloc_(local, right, value, ret);
	}
}

int plus_float_rl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_ratio(left)) {
		long_float_throw_alloc(local, right, ret);
		return 0;
	}
	else {
		Return(long_float_ratio_(left, &value));
		return plus_float_lv_alloc_(local, right, value, ret);
	}
}

int plus_float_rs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_rs_alloc_(local, left, right, ret);
}

int plus_float_rd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_rd_alloc_(local, left, right, ret);
}

int plus_float_rl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_rl_alloc_(local, left, right, ret);
}

int plus_float_rs_heap_(addr left, addr right, addr *ret)
{
	return plus_float_rs_alloc_(NULL, left, right, ret);
}

int plus_float_rd_heap_(addr left, addr right, addr *ret)
{
	return plus_float_rd_alloc_(NULL, left, right, ret);
}

int plus_float_rl_heap_(addr left, addr right, addr *ret)
{
	return plus_float_rl_alloc_(NULL, left, right, ret);
}

int plus_float_ss_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefSingleFloat(left) + RefSingleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int plus_float_sd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = ((double_float)RefSingleFloat(left)) + RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int plus_float_sl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefSingleFloat(left)) + RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int plus_float_ds_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) + ((double_float)RefSingleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int plus_float_dd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) + RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int plus_float_dl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefDoubleFloat(left)) + RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int plus_float_ls_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefLongFloat(left) + ((long_float)RefSingleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int plus_float_ld_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefLongFloat(left) + ((long_float)RefDoubleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int plus_float_ll_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = RefLongFloat(left) + RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int plus_float_ss_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_ss_alloc_(local, left, right, ret);
}

int plus_float_sd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_sd_alloc_(local, left, right, ret);
}

int plus_float_sl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_sl_alloc_(local, left, right, ret);
}

int plus_float_ds_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_ds_alloc_(local, left, right, ret);
}

int plus_float_dd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_dd_alloc_(local, left, right, ret);
}

int plus_float_dl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_dl_alloc_(local, left, right, ret);
}

int plus_float_ls_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_ls_alloc_(local, left, right, ret);
}

int plus_float_ld_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_ld_alloc_(local, left, right, ret);
}

int plus_float_ll_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return plus_float_ll_alloc_(local, left, right, ret);
}

int plus_float_ss_heap_(addr left, addr right, addr *ret)
{
	return plus_float_ss_alloc_(NULL, left, right, ret);
}

int plus_float_sd_heap_(addr left, addr right, addr *ret)
{
	return plus_float_sd_alloc_(NULL, left, right, ret);
}

int plus_float_sl_heap_(addr left, addr right, addr *ret)
{
	return plus_float_sl_alloc_(NULL, left, right, ret);
}

int plus_float_ds_heap_(addr left, addr right, addr *ret)
{
	return plus_float_ds_alloc_(NULL, left, right, ret);
}

int plus_float_dd_heap_(addr left, addr right, addr *ret)
{
	return plus_float_dd_alloc_(NULL, left, right, ret);
}

int plus_float_dl_heap_(addr left, addr right, addr *ret)
{
	return plus_float_dl_alloc_(NULL, left, right, ret);
}

int plus_float_ls_heap_(addr left, addr right, addr *ret)
{
	return plus_float_ls_alloc_(NULL, left, right, ret);
}

int plus_float_ld_heap_(addr left, addr right, addr *ret)
{
	return plus_float_ld_alloc_(NULL, left, right, ret);
}

int plus_float_ll_heap_(addr left, addr right, addr *ret)
{
	return plus_float_ll_alloc_(NULL, left, right, ret);
}


/*
 *  minus
 */
int minus_float_fs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0) {
		sign_reverse_floats_heap(right, ret);
		return 0;
	}
	else {
		return minus_float_vs_alloc_(local, (single_float)value, right, ret);
	}
}

int minus_float_fd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0) {
		sign_reverse_floatd_heap(right, ret);
		return 0;
	}
	else {
		return minus_float_vd_alloc_(local, (double_float)value, right, ret);
	}
}

int minus_float_fl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetFixnum(left, &value);
	if (value == 0) {
		sign_reverse_floatl_heap(right, ret);
		return 0;
	}
	else {
		return minus_float_vl_alloc_(local, (long_float)value, right, ret);
	}
}

int minus_float_fs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_fs_alloc_(local, left, right, ret);
}

int minus_float_fd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_fd_alloc_(local, left, right, ret);
}

int minus_float_fl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_fl_alloc_(local, left, right, ret);
}

int minus_float_fs_heap_(addr left, addr right, addr *ret)
{
	return minus_float_fs_alloc_(NULL, left, right, ret);
}

int minus_float_fd_heap_(addr left, addr right, addr *ret)
{
	return minus_float_fd_alloc_(NULL, left, right, ret);
}

int minus_float_fl_heap_(addr left, addr right, addr *ret)
{
	return minus_float_fl_alloc_(NULL, left, right, ret);
}

int minus_float_sf_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &value);
	if (value == 0) {
		single_float_throw_alloc(local, left, ret);
		return 0;
	}
	else {
		return minus_float_sv_alloc_(local, left, (single_float)value, ret);
	}
}

int minus_float_df_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &value);
	if (value == 0) {
		double_float_throw_alloc(local, left, ret);
		return 0;
	}
	else {
		return minus_float_dv_alloc_(local, left, (double_float)value, ret);
	}
}

int minus_float_lf_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &value);
	if (value == 0) {
		long_float_throw_alloc(local, left, ret);
		return 0;
	}
	else {
		return minus_float_lv_alloc_(local, left, (long_float)value, ret);
	}
}

int minus_float_sf_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_sf_alloc_(local, left, right, ret);
}

int minus_float_df_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_df_alloc_(local, left, right, ret);
}

int minus_float_lf_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_lf_alloc_(local, left, right, ret);
}

int minus_float_sf_heap_(addr left, addr right, addr *ret)
{
	return minus_float_sf_alloc_(NULL, left, right, ret);
}

int minus_float_df_heap_(addr left, addr right, addr *ret)
{
	return minus_float_df_alloc_(NULL, left, right, ret);
}

int minus_float_lf_heap_(addr left, addr right, addr *ret)
{
	return minus_float_lf_alloc_(NULL, left, right, ret);
}

int minus_float_bs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type right error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type left error");
	if (zerop_bignum(left)) {
		sign_reverse_floats_heap(right, ret);
		return 0;
	}
	else {
		Return(single_float_bignum_(left, &value));
		return minus_float_vs_alloc_(local, value, right, ret);
	}
}

int minus_float_bd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_bignum(left)) {
		sign_reverse_floatd_heap(right, ret);
		return 0;
	}
	else {
		Return(double_float_bignum_(left, &value));
		return minus_float_vd_alloc_(local, value, right, ret);
	}
}

int minus_float_bl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_bignum(left)) {
		sign_reverse_floatl_heap(right, ret);
		return 0;
	}
	else {
		Return(long_float_bignum_(left, &value));
		return minus_float_vl_alloc_(local, value, right, ret);
	}
}

int minus_float_bs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_bs_alloc_(local, left, right, ret);
}

int minus_float_bd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_bd_alloc_(local, left, right, ret);
}

int minus_float_bl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_bl_alloc_(local, left, right, ret);
}

int minus_float_bs_heap_(addr left, addr right, addr *ret)
{
	return minus_float_bs_alloc_(NULL, left, right, ret);
}

int minus_float_bd_heap_(addr left, addr right, addr *ret)
{
	return minus_float_bd_alloc_(NULL, left, right, ret);
}

int minus_float_bl_heap_(addr left, addr right, addr *ret)
{
	return minus_float_bl_alloc_(NULL, left, right, ret);
}

int minus_float_sb_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	Return(single_float_bignum_(right, &value));
	return minus_float_sv_alloc_(local, left, value, ret);
}

int minus_float_db_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	Return(double_float_bignum_(right, &value));
	return minus_float_dv_alloc_(local, left, value, ret);
}

int minus_float_lb_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	Return(long_float_bignum_(right, &value));
	return minus_float_lv_alloc_(local, left, value, ret);
}

int minus_float_sb_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_sb_alloc_(local, left, right, ret);
}

int minus_float_db_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_db_alloc_(local, left, right, ret);
}

int minus_float_lb_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_lb_alloc_(local, left, right, ret);
}

int minus_float_sb_heap_(addr left, addr right, addr *ret)
{
	return minus_float_sb_alloc_(NULL, left, right, ret);
}

int minus_float_db_heap_(addr left, addr right, addr *ret)
{
	return minus_float_db_alloc_(NULL, left, right, ret);
}

int minus_float_lb_heap_(addr left, addr right, addr *ret)
{
	return minus_float_lb_alloc_(NULL, left, right, ret);
}

int minus_float_rs_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	if (zerop_ratio(left)) {
		sign_reverse_floats_heap(right, ret);
		return 0;
	}
	else {
		Return(single_float_ratio_(left, &value));
		return minus_float_vs_alloc_(local, value, right, ret);
	}
}

int minus_float_rd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_ratio(left)) {
		sign_reverse_floatd_heap(right, ret);
		return 0;
	}
	else {
		Return(double_float_ratio_(left, &value));
		return minus_float_vd_alloc_(local, value, right, ret);
	}
}

int minus_float_rl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_ratio(left)) {
		sign_reverse_floatl_heap(right, ret);
		return 0;
	}
	else {
		Return(long_float_ratio_(left, &value));
		return minus_float_vl_alloc_(local, value, right, ret);
	}
}

int minus_float_rs_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_rs_alloc_(local, left, right, ret);
}

int minus_float_rd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_rd_alloc_(local, left, right, ret);
}

int minus_float_rl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_rl_alloc_(local, left, right, ret);
}

int minus_float_rs_heap_(addr left, addr right, addr *ret)
{
	return minus_float_rs_alloc_(NULL, left, right, ret);
}

int minus_float_rd_heap_(addr left, addr right, addr *ret)
{
	return minus_float_rd_alloc_(NULL, left, right, ret);
}

int minus_float_rl_heap_(addr left, addr right, addr *ret)
{
	return minus_float_rl_alloc_(NULL, left, right, ret);
}

int minus_float_sr_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	Return(single_float_ratio_(right, &value));
	return minus_float_sv_alloc_(local, left, value, ret);
}

int minus_float_dr_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	Return(double_float_ratio_(right, &value));
	return minus_float_dv_alloc_(local, left, value, ret);
}

int minus_float_lr_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	Return(long_float_ratio_(right, &value));
	return minus_float_lv_alloc_(local, left, value, ret);
}

int minus_float_sr_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_sr_alloc_(local, left, right, ret);
}

int minus_float_dr_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_dr_alloc_(local, left, right, ret);
}

int minus_float_lr_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_lr_alloc_(local, left, right, ret);
}

int minus_float_sr_heap_(addr left, addr right, addr *ret)
{
	return minus_float_sr_alloc_(NULL, left, right, ret);
}

int minus_float_dr_heap_(addr left, addr right, addr *ret)
{
	return minus_float_dr_alloc_(NULL, left, right, ret);
}

int minus_float_lr_heap_(addr left, addr right, addr *ret)
{
	return minus_float_lr_alloc_(NULL, left, right, ret);
}

int minus_float_ss_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefSingleFloat(left) - RefSingleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	single_float_alloc(local, ret, value);

	return 0;
}

int minus_float_sd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = ((double_float)RefSingleFloat(left)) - RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int minus_float_sl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefSingleFloat(left)) - RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_ds_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) - ((double_float)RefSingleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int minus_float_dd_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) - RefDoubleFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	double_float_alloc(local, ret, value);

	return 0;
}

int minus_float_dl_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefDoubleFloat(left)) - RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_ls_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefLongFloat(left) - ((long_float)RefSingleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_ld_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefLongFloat(left) - ((long_float)RefDoubleFloat(right));
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_ll_alloc_(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = RefLongFloat(left) - RefLongFloat(right);
	Return_float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);

	return 0;
}

int minus_float_ss_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_ss_alloc_(local, left, right, ret);
}

int minus_float_sd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_sd_alloc_(local, left, right, ret);
}

int minus_float_sl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_sl_alloc_(local, left, right, ret);
}

int minus_float_ds_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_ds_alloc_(local, left, right, ret);
}

int minus_float_dd_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_dd_alloc_(local, left, right, ret);
}

int minus_float_dl_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_dl_alloc_(local, left, right, ret);
}

int minus_float_ls_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_ls_alloc_(local, left, right, ret);
}

int minus_float_ld_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_ld_alloc_(local, left, right, ret);
}

int minus_float_ll_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	return minus_float_ll_alloc_(local, left, right, ret);
}

int minus_float_ss_heap_(addr left, addr right, addr *ret)
{
	return minus_float_ss_alloc_(NULL, left, right, ret);
}

int minus_float_sd_heap_(addr left, addr right, addr *ret)
{
	return minus_float_sd_alloc_(NULL, left, right, ret);
}

int minus_float_sl_heap_(addr left, addr right, addr *ret)
{
	return minus_float_sl_alloc_(NULL, left, right, ret);
}

int minus_float_ds_heap_(addr left, addr right, addr *ret)
{
	return minus_float_ds_alloc_(NULL, left, right, ret);
}

int minus_float_dd_heap_(addr left, addr right, addr *ret)
{
	return minus_float_dd_alloc_(NULL, left, right, ret);
}

int minus_float_dl_heap_(addr left, addr right, addr *ret)
{
	return minus_float_dl_alloc_(NULL, left, right, ret);
}

int minus_float_ls_heap_(addr left, addr right, addr *ret)
{
	return minus_float_ls_alloc_(NULL, left, right, ret);
}

int minus_float_ld_heap_(addr left, addr right, addr *ret)
{
	return minus_float_ld_alloc_(NULL, left, right, ret);
}

int minus_float_ll_heap_(addr left, addr right, addr *ret)
{
	return minus_float_ll_alloc_(NULL, left, right, ret);
}

