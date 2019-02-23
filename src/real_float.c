#include <ctype.h>
#include <float.h>
#include <locale.h>
#include "bignum.h"
#include "cmpl.h"
#include "condition.h"
#include "object.h"
#include "ratio.h"
#include "real_float.h"
#include "stream.h"

void single_float_check_alloc(LocalRoot local, addr *ret, single_float value)
{
	enum fltclass type;

	type = getfltclassify(value);
	if (type != fltclass_normal)
		float_fltclass(CONSTANT_COMMON_FLOAT, type, NULL);
	single_float_alloc(local, ret, value);
}

void single_float_check_local(LocalRoot local, addr *ret, single_float value)
{
	Check(local == NULL, "local error");
	single_float_check_alloc(local, ret, value);
}

void single_float_check_heap(addr *ret, single_float value)
{
	single_float_check_alloc(NULL, ret, value);
}

void double_float_check_alloc(LocalRoot local, addr *ret, double_float value)
{
	enum fltclass type;

	type = getfltclassify(value);
	if (type != fltclass_normal)
		float_fltclass(CONSTANT_COMMON_FLOAT, type, NULL);
	double_float_alloc(local, ret, value);
}

void double_float_check_local(LocalRoot local, addr *ret, double_float value)
{
	Check(local == NULL, "local error");
	double_float_check_alloc(local, ret, value);
}

void double_float_check_heap(addr *ret, double_float value)
{
	double_float_check_alloc(NULL, ret, value);
}

void long_float_check_alloc(LocalRoot local, addr *ret, long_float value)
{
	enum fltclass type;

	type = getfltclassify(value);
	if (type != fltclass_normal)
		float_fltclass(CONSTANT_COMMON_FLOAT, type, NULL);
	long_float_alloc(local, ret, value);
}

void long_float_check_local(LocalRoot local, addr *ret, long_float value)
{
	Check(local == NULL, "local error");
	long_float_check_alloc(local, ret, value);
}

void long_float_check_heap(addr *ret, long_float value)
{
	long_float_check_alloc(NULL, ret, value);
}

void single_float_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		single_float_heap(ret, RefSingleFloat(pos));
	else
		*ret = pos;
}

void double_float_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		double_float_heap(ret, RefDoubleFloat(pos));
	else
		*ret = pos;
}

void long_float_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		long_float_heap(ret, RefLongFloat(pos));
	else
		*ret = pos;
}

void single_float_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		single_float_local(local, ret, RefSingleFloat(pos));
}

void double_float_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		double_float_local(local, ret, RefDoubleFloat(pos));
}

void long_float_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		long_float_local(local, ret, RefLongFloat(pos));
}

void single_float_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		single_float_throw_local(local, pos, ret);
	else
		single_float_throw_heap(pos, ret);
}

void double_float_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		double_float_throw_local(local, pos, ret);
	else
		double_float_throw_heap(pos, ret);
}

void long_float_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		long_float_throw_local(local, pos, ret);
	else
		long_float_throw_heap(pos, ret);
}

void float_throw_heap(addr pos, addr *ret)
{
	float_throw_alloc(NULL, pos, ret);
}

void float_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	float_throw_alloc(local, pos, ret);
}

void float_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			single_float_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_SHORT_FLOAT:
			Abort("short float is not implemented.");
			break;

		default:
			TypeError(pos, FLOAT);
			break;
	}
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
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;

	return 0;
}

int compare_fd_real(addr left, addr right)
{
	double_float value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value1 = double_float_fixnum(left);
	value2 = RefDoubleFloat(right);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;

	return 0;
}

int compare_fl_real(addr left, addr right)
{
	long_float value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value1 = long_float_fixnum(left);
	value2 = RefLongFloat(right);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;

	return 0;
}

int compare_sf_real(addr left, addr right)
{
	return -compare_fs_real(left, right);
}

int compare_df_real(addr left, addr right)
{
	return -compare_fd_real(left, right);
}

int compare_lf_real(addr left, addr right)
{
	return -compare_fl_real(left, right);
}

int compare_ss_real(addr left, addr right)
{
	single_float value1, value2;
	GetSingleFloat(left, &value1);
	GetSingleFloat(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;
	return 0;
}

int compare_sd_real(addr left, addr right)
{
	single_float value1;
	double_float value2;
	GetSingleFloat(left, &value1);
	GetDoubleFloat(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;
	return 0;
}

int compare_sl_real(addr left, addr right)
{
	single_float value1;
	long_float value2;
	GetSingleFloat(left, &value1);
	GetLongFloat(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;
	return 0;
}

int compare_ds_real(addr left, addr right)
{
	double_float value1;
	single_float value2;
	GetDoubleFloat(left, &value1);
	GetSingleFloat(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;
	return 0;
}

int compare_dd_real(addr left, addr right)
{
	double_float value1, value2;
	GetDoubleFloat(left, &value1);
	GetDoubleFloat(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;
	return 0;
}

int compare_dl_real(addr left, addr right)
{
	double_float value1;
	long_float value2;
	GetDoubleFloat(left, &value1);
	GetLongFloat(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;
	return 0;
}

int compare_ls_real(addr left, addr right)
{
	long_float value1;
	single_float value2;
	GetLongFloat(left, &value1);
	GetSingleFloat(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;
	return 0;
}

int compare_ld_real(addr left, addr right)
{
	long_float value1;
	double_float value2;
	GetLongFloat(left, &value1);
	GetDoubleFloat(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;
	return 0;
}

int compare_ll_real(addr left, addr right)
{
	long_float value1, value2;
	GetLongFloat(left, &value1);
	GetLongFloat(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;
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
	TypeError(right, FLOAT);
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
 *  strtof
 */
enum fltclass fltclassify(int check, int sign)
{
	if (check == FP_INFINITE) {
		if (sign)
			return fltclass_underflow;
		else
			return fltclass_overflow;
	}
	if (check == FP_NAN) {
		return fltclass_nan;
	}

	return fltclass_normal;
}

void float_fltclass(constindex index, enum fltclass type, ...)
{
	va_list args;
	addr list;

	va_start(args, type);
	list_alloc_stdarg(NULL, &list, args);
	va_end(args);

	switch (type) {
		case fltclass_overflow:
			floating_point_overflow_constant(index, list);
			break;

		case fltclass_underflow:
			floating_point_underflow_constant(index, list);
			break;

		case fltclass_nan:
		default:
			floating_point_invalid_operation_constant(index, list);
			break;
	}
}

static single_float strtof_c(const char *str)
{
	single_float value;
	const char *check;
#ifdef BIGNUM_DEBUG
	char *endptr;
#endif

	/* setlocale C */
	check = setlocale_c(LC_NUMERIC);

	/* convert */
#ifdef BIGNUM_DEBUG
	value = strtof(str, &endptr);
	Check(*endptr, "strtof error");
#else
	value = strtof(str, NULL);
#endif

	/* setlocale */
	if (check)
		setlocale(LC_NUMERIC, check);

	/* result */
	return value;
}

static double_float strtod_c(const char *str)
{
	double_float value;
	const char *check;
#ifdef BIGNUM_DEBUG
	char *endptr;
#endif

	/* setlocale C */
	check = setlocale_c(LC_NUMERIC);

	/* convert */
#ifdef BIGNUM_DEBUG
	value = strtod(str, &endptr);
	Check(*endptr, "strtod error");
#else
	value = strtod(str, NULL);
#endif

	/* setlocale */
	if (check)
		setlocale(LC_NUMERIC, check);

	/* result */
	return value;
}

static long_float strtold_c(const char *str)
{
	long_float value;
	const char *check;
#ifdef BIGNUM_DEBUG
	char *endptr;
#endif

	/* setlocale C */
	check = setlocale_c(LC_NUMERIC);

	/* convert */
#ifdef BIGNUM_DEBUG
	value = strtold(str, &endptr);
	Check(*endptr, "strtold error");
#else
	value = strtold(str, NULL);
#endif

	/* setlocale */
	if (check)
		setlocale(LC_NUMERIC, check);

	/* result */
	return value;
}

single_float check_strtof(const char *str, addr pos)
{
	enum fltclass type;
	single_float value;

	value = strtof_c(str);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		if (pos == NULL) strvect_char_heap(&pos, str);
		float_fltclass(CONSTANT_SYSTEM_CAST_SINGLE_FLOAT, type, pos, NULL);
	}

	return value;
}

double_float check_strtod(const char *str, addr pos)
{
	enum fltclass type;
	double_float value;

	value = strtod_c(str);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		if (pos == NULL) strvect_char_heap(&pos, str);
		float_fltclass(CONSTANT_SYSTEM_CAST_DOUBLE_FLOAT, type, pos, NULL);
	}

	return value;
}

long_float check_strtold(const char *str, addr pos)
{
	enum fltclass type;
	long_float value;

	value = strtold_c(str);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		if (pos == NULL) strvect_char_heap(&pos, str);
		float_fltclass(CONSTANT_SYSTEM_CAST_LONG_FLOAT, type, pos, NULL);
	}

	return value;
}

single_float check_strtof_reverse(const char *str, addr pos)
{
	enum fltclass type;
	single_float value;

	value = strtof_c(str);
	type = getfltclassify_reverse(value);
	if (type != fltclass_normal) {
		if (pos == NULL) strvect_char_heap(&pos, str);
		float_fltclass(CONSTANT_SYSTEM_CAST_SINGLE_FLOAT, type, pos, NULL);
	}

	return value;
}

double_float check_strtod_reverse(const char *str, addr pos)
{
	enum fltclass type;
	double_float value;

	value = strtod_c(str);
	type = getfltclassify_reverse(value);
	if (type != fltclass_normal) {
		if (pos == NULL) strvect_char_heap(&pos, str);
		float_fltclass(CONSTANT_SYSTEM_CAST_DOUBLE_FLOAT, type, pos, NULL);
	}

	return value;
}

long_float check_strtold_reverse(const char *str, addr pos)
{
	enum fltclass type;
	long_float value;

	value = strtold_c(str);
	type = getfltclassify_reverse(value);
	if (type != fltclass_normal) {
		if (pos == NULL) strvect_char_heap(&pos, str);
		float_fltclass(CONSTANT_SYSTEM_CAST_LONG_FLOAT, type, pos, NULL);
	}

	return value;
}


/*
 *  plus/minus value
 */
void plus_float_sv_alloc(LocalRoot local, addr left, single_float right, addr *ret)
{
	enum fltclass type;
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

void plus_float_dv_alloc(LocalRoot local, addr left, double_float right, addr *ret)
{
	enum fltclass type;
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

void plus_float_lv_alloc(LocalRoot local, addr left, long_float right, addr *ret)
{
	enum fltclass type;
	addr temp;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type error");
	value = RefLongFloat(left) + right;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&temp, right);
		float_fltclass(CONSTANT_COMMON_PLUS, type, left, temp, NULL);
	}
	long_float_alloc(local, ret, right);
}

void plus_float_sv_local(LocalRoot local, addr left, single_float right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_sv_alloc(local, left, right, ret);
}

void plus_float_dv_local(LocalRoot local, addr left, double_float right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_dv_alloc(local, left, right, ret);
}

void plus_float_lv_local(LocalRoot local, addr left, long_float right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_lv_alloc(local, left, right, ret);
}

void plus_float_sv_heap(addr left, single_float right, addr *ret)
{
	plus_float_sv_alloc(NULL, left, right, ret);
}

void plus_float_dv_heap(addr left, double_float right, addr *ret)
{
	plus_float_dv_alloc(NULL, left, right, ret);
}

void plus_float_lv_heap(addr left, long_float right, addr *ret)
{
	plus_float_lv_alloc(NULL, left, right, ret);
}

void minus_float_sv_alloc(LocalRoot local, addr left, single_float right, addr *ret)
{
	enum fltclass type;
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

void minus_float_dv_alloc(LocalRoot local, addr left, double_float right, addr *ret)
{
	enum fltclass type;
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

void minus_float_lv_alloc(LocalRoot local, addr left, long_float right, addr *ret)
{
	enum fltclass type;
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

void minus_float_sv_local(LocalRoot local, addr left, single_float right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_sv_alloc(local, left, right, ret);
}

void minus_float_dv_local(LocalRoot local, addr left, double_float right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_dv_alloc(local, left, right, ret);
}

void minus_float_lv_local(LocalRoot local, addr left, long_float right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_lv_alloc(local, left, right, ret);
}

void minus_float_sv_heap(addr left, single_float right, addr *ret)
{
	minus_float_sv_alloc(NULL, left, right, ret);
}

void minus_float_dv_heap(addr left, double_float right, addr *ret)
{
	minus_float_dv_alloc(NULL, left, right, ret);
}

void minus_float_lv_heap(addr left, long_float right, addr *ret)
{
	minus_float_lv_alloc(NULL, left, right, ret);
}

void minus_float_vs_alloc(LocalRoot local, single_float left, addr right, addr *ret)
{
	enum fltclass type;
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

void minus_float_vd_alloc(LocalRoot local, double_float left, addr right, addr *ret)
{
	enum fltclass type;
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

void minus_float_vl_alloc(LocalRoot local, long_float left, addr right, addr *ret)
{
	enum fltclass type;
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

void minus_float_vs_local(LocalRoot local, single_float left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_vs_alloc(local, left, right, ret);
}

void minus_float_vd_local(LocalRoot local, double_float left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_vd_alloc(local, left, right, ret);
}

void minus_float_vl_local(LocalRoot local, long_float left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_vl_alloc(local, left, right, ret);
}

void minus_float_vs_heap(single_float left, addr right, addr *ret)
{
	minus_float_vs_alloc(NULL, left, right, ret);
}

void minus_float_vd_heap(double_float left, addr right, addr *ret)
{
	minus_float_vd_alloc(NULL, left, right, ret);
}

void minus_float_vl_heap(long_float left, addr right, addr *ret)
{
	minus_float_vl_alloc(NULL, left, right, ret);
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
void plus_float_fs_alloc(LocalRoot local, addr left, addr right, addr *ret)
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

void plus_float_fd_alloc(LocalRoot local, addr left, addr right, addr *ret)
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

void plus_float_fl_alloc(LocalRoot local, addr left, addr right, addr *ret)
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

void plus_float_fs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_fs_alloc(local, left, right, ret);
}

void plus_float_fd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_fd_alloc(local, left, right, ret);
}

void plus_float_fl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_fl_alloc(local, left, right, ret);
}

void plus_float_fs_heap(addr left, addr right, addr *ret)
{
	plus_float_fs_alloc(NULL, left, right, ret);
}

void plus_float_fd_heap(addr left, addr right, addr *ret)
{
	plus_float_fd_alloc(NULL, left, right, ret);
}

void plus_float_fl_heap(addr left, addr right, addr *ret)
{
	plus_float_fl_alloc(NULL, left, right, ret);
}

void plus_float_bs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	if (zerop_bignum(left))
		single_float_throw_alloc(local, right, ret);
	else
		plus_float_sv_alloc(local, right, single_float_bignum(left), ret);
}

void plus_float_bd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_bignum(left))
		double_float_throw_alloc(local, right, ret);
	else
		plus_float_dv_alloc(local, right, double_float_bignum(left), ret);
}

void plus_float_bl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_bignum(left))
		long_float_throw_alloc(local, right, ret);
	else
		plus_float_lv_alloc(local, right, long_float_bignum(left), ret);
}

void plus_float_bs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_bs_alloc(local, left, right, ret);
}

void plus_float_bd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_bd_alloc(local, left, right, ret);
}

void plus_float_bl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_bl_alloc(local, left, right, ret);
}

void plus_float_bs_heap(addr left, addr right, addr *ret)
{
	plus_float_bs_alloc(NULL, left, right, ret);
}

void plus_float_bd_heap(addr left, addr right, addr *ret)
{
	plus_float_bd_alloc(NULL, left, right, ret);
}

void plus_float_bl_heap(addr left, addr right, addr *ret)
{
	plus_float_bl_alloc(NULL, left, right, ret);
}

void plus_float_rs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	if (zerop_ratio(left))
		single_float_throw_alloc(local, right, ret);
	else
		plus_float_sv_alloc(local, right, single_float_ratio(left), ret);
}

void plus_float_rd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_ratio(left))
		double_float_throw_alloc(local, right, ret);
	else
		plus_float_dv_alloc(local, right, double_float_ratio(left), ret);
}

void plus_float_rl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_ratio(left))
		long_float_throw_alloc(local, right, ret);
	else
		plus_float_lv_alloc(local, right, long_float_ratio(left), ret);
}

void plus_float_rs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_rs_alloc(local, left, right, ret);
}

void plus_float_rd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_rd_alloc(local, left, right, ret);
}

void plus_float_rl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_rl_alloc(local, left, right, ret);
}

void plus_float_rs_heap(addr left, addr right, addr *ret)
{
	plus_float_rs_alloc(NULL, left, right, ret);
}

void plus_float_rd_heap(addr left, addr right, addr *ret)
{
	plus_float_rd_alloc(NULL, left, right, ret);
}

void plus_float_rl_heap(addr left, addr right, addr *ret)
{
	plus_float_rl_alloc(NULL, left, right, ret);
}

void plus_float_ss_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefSingleFloat(left) + RefSingleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	single_float_alloc(local, ret, value);
}

void plus_float_sd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = ((double_float)RefSingleFloat(left)) + RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	double_float_alloc(local, ret, value);
}

void plus_float_sl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefSingleFloat(left)) + RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);
}

void plus_float_ds_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) + ((double_float)RefSingleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	double_float_alloc(local, ret, value);
}

void plus_float_dd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) + RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	double_float_alloc(local, ret, value);
}

void plus_float_dl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefDoubleFloat(left)) + RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);
}

void plus_float_ls_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefLongFloat(left) + ((long_float)RefSingleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);
}

void plus_float_ld_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefLongFloat(left) + ((long_float)RefDoubleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);
}

void plus_float_ll_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = RefLongFloat(left) + RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_PLUS, value, left, right);
	long_float_alloc(local, ret, value);
}

void plus_float_ss_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_ss_alloc(local, left, right, ret);
}

void plus_float_sd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_sd_alloc(local, left, right, ret);
}

void plus_float_sl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_sl_alloc(local, left, right, ret);
}

void plus_float_ds_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_ds_alloc(local, left, right, ret);
}

void plus_float_dd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_dd_alloc(local, left, right, ret);
}

void plus_float_dl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_dl_alloc(local, left, right, ret);
}

void plus_float_ls_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_ls_alloc(local, left, right, ret);
}

void plus_float_ld_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_ld_alloc(local, left, right, ret);
}

void plus_float_ll_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_ll_alloc(local, left, right, ret);
}

void plus_float_ss_heap(addr left, addr right, addr *ret)
{
	plus_float_ss_alloc(NULL, left, right, ret);
}

void plus_float_sd_heap(addr left, addr right, addr *ret)
{
	plus_float_sd_alloc(NULL, left, right, ret);
}

void plus_float_sl_heap(addr left, addr right, addr *ret)
{
	plus_float_sl_alloc(NULL, left, right, ret);
}

void plus_float_ds_heap(addr left, addr right, addr *ret)
{
	plus_float_ds_alloc(NULL, left, right, ret);
}

void plus_float_dd_heap(addr left, addr right, addr *ret)
{
	plus_float_dd_alloc(NULL, left, right, ret);
}

void plus_float_dl_heap(addr left, addr right, addr *ret)
{
	plus_float_dl_alloc(NULL, left, right, ret);
}

void plus_float_ls_heap(addr left, addr right, addr *ret)
{
	plus_float_ls_alloc(NULL, left, right, ret);
}

void plus_float_ld_heap(addr left, addr right, addr *ret)
{
	plus_float_ld_alloc(NULL, left, right, ret);
}

void plus_float_ll_heap(addr left, addr right, addr *ret)
{
	plus_float_ll_alloc(NULL, left, right, ret);
}


/*
 *  minus
 */
void minus_float_fs_alloc(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_float_fd_alloc(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_float_fl_alloc(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_float_fs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_fs_alloc(local, left, right, ret);
}

void minus_float_fd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_fd_alloc(local, left, right, ret);
}

void minus_float_fl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_fl_alloc(local, left, right, ret);
}

void minus_float_fs_heap(addr left, addr right, addr *ret)
{
	minus_float_fs_alloc(NULL, left, right, ret);
}

void minus_float_fd_heap(addr left, addr right, addr *ret)
{
	minus_float_fd_alloc(NULL, left, right, ret);
}

void minus_float_fl_heap(addr left, addr right, addr *ret)
{
	minus_float_fl_alloc(NULL, left, right, ret);
}

void minus_float_sf_alloc(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_float_df_alloc(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_float_lf_alloc(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_float_sf_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_sf_alloc(local, left, right, ret);
}

void minus_float_df_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_df_alloc(local, left, right, ret);
}

void minus_float_lf_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_lf_alloc(local, left, right, ret);
}

void minus_float_sf_heap(addr left, addr right, addr *ret)
{
	minus_float_sf_alloc(NULL, left, right, ret);
}

void minus_float_df_heap(addr left, addr right, addr *ret)
{
	minus_float_df_alloc(NULL, left, right, ret);
}

void minus_float_lf_heap(addr left, addr right, addr *ret)
{
	minus_float_lf_alloc(NULL, left, right, ret);
}

void minus_float_bs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type right error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type left error");
	if (zerop_bignum(left))
		sign_reverse_floats_heap(right, ret);
	else
		minus_float_vs_alloc(local, single_float_bignum(left), right, ret);
}

void minus_float_bd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_bignum(left))
		sign_reverse_floatd_heap(right, ret);
	else
		minus_float_vd_alloc(local, double_float_bignum(left), right, ret);
}

void minus_float_bl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_bignum(left))
		sign_reverse_floatl_heap(right, ret);
	else
		minus_float_vl_alloc(local, long_float_bignum(left), right, ret);
}

void minus_float_bs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_bs_alloc(local, left, right, ret);
}

void minus_float_bd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_bd_alloc(local, left, right, ret);
}

void minus_float_bl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_bl_alloc(local, left, right, ret);
}

void minus_float_bs_heap(addr left, addr right, addr *ret)
{
	minus_float_bs_alloc(NULL, left, right, ret);
}

void minus_float_bd_heap(addr left, addr right, addr *ret)
{
	minus_float_bd_alloc(NULL, left, right, ret);
}

void minus_float_bl_heap(addr left, addr right, addr *ret)
{
	minus_float_bl_alloc(NULL, left, right, ret);
}

void minus_float_sb_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	minus_float_sv_alloc(local, left, single_float_bignum(right), ret);
}

void minus_float_db_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	minus_float_dv_alloc(local, left, double_float_bignum(right), ret);
}

void minus_float_lb_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	minus_float_lv_alloc(local, left, long_float_bignum(right), ret);
}

void minus_float_sb_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_sb_alloc(local, left, right, ret);
}

void minus_float_db_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_db_alloc(local, left, right, ret);
}

void minus_float_lb_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_lb_alloc(local, left, right, ret);
}

void minus_float_sb_heap(addr left, addr right, addr *ret)
{
	minus_float_sb_alloc(NULL, left, right, ret);
}

void minus_float_db_heap(addr left, addr right, addr *ret)
{
	minus_float_db_alloc(NULL, left, right, ret);
}

void minus_float_lb_heap(addr left, addr right, addr *ret)
{
	minus_float_lb_alloc(NULL, left, right, ret);
}

void minus_float_rs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	if (zerop_ratio(left))
		sign_reverse_floats_heap(right, ret);
	else
		minus_float_vs_alloc(local, single_float_ratio(left), right, ret);
}

void minus_float_rd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_ratio(left))
		sign_reverse_floatd_heap(right, ret);
	else
		minus_float_vd_alloc(local, double_float_ratio(left), right, ret);
}

void minus_float_rl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_ratio(left))
		sign_reverse_floatl_heap(right, ret);
	else
		minus_float_vl_alloc(local, long_float_ratio(left), right, ret);
}

void minus_float_rs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_rs_alloc(local, left, right, ret);
}

void minus_float_rd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_rd_alloc(local, left, right, ret);
}

void minus_float_rl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_rl_alloc(local, left, right, ret);
}

void minus_float_rs_heap(addr left, addr right, addr *ret)
{
	minus_float_rs_alloc(NULL, left, right, ret);
}

void minus_float_rd_heap(addr left, addr right, addr *ret)
{
	minus_float_rd_alloc(NULL, left, right, ret);
}

void minus_float_rl_heap(addr left, addr right, addr *ret)
{
	minus_float_rl_alloc(NULL, left, right, ret);
}

void minus_float_sr_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	minus_float_sv_alloc(local, left, single_float_ratio(right), ret);
}

void minus_float_dr_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	minus_float_dv_alloc(local, left, double_float_ratio(right), ret);
}

void minus_float_lr_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	minus_float_lv_alloc(local, left, long_float_ratio(right), ret);
}

void minus_float_sr_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_sr_alloc(local, left, right, ret);
}

void minus_float_dr_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_dr_alloc(local, left, right, ret);
}

void minus_float_lr_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_lr_alloc(local, left, right, ret);
}

void minus_float_sr_heap(addr left, addr right, addr *ret)
{
	minus_float_sr_alloc(NULL, left, right, ret);
}

void minus_float_dr_heap(addr left, addr right, addr *ret)
{
	minus_float_dr_alloc(NULL, left, right, ret);
}

void minus_float_lr_heap(addr left, addr right, addr *ret)
{
	minus_float_lr_alloc(NULL, left, right, ret);
}

void minus_float_ss_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefSingleFloat(left) - RefSingleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	single_float_alloc(local, ret, value);
}

void minus_float_sd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = ((double_float)RefSingleFloat(left)) - RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	double_float_alloc(local, ret, value);
}

void minus_float_sl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefSingleFloat(left)) - RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);
}

void minus_float_ds_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) - ((double_float)RefSingleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	double_float_alloc(local, ret, value);
}

void minus_float_dd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) - RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	double_float_alloc(local, ret, value);
}

void minus_float_dl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefDoubleFloat(left)) - RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);
}

void minus_float_ls_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefLongFloat(left) - ((long_float)RefSingleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);
}

void minus_float_ld_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefLongFloat(left) - ((long_float)RefDoubleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);
}

void minus_float_ll_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = RefLongFloat(left) - RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_MINUS, value, left, right);
	long_float_alloc(local, ret, value);
}

void minus_float_ss_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_ss_alloc(local, left, right, ret);
}

void minus_float_sd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_sd_alloc(local, left, right, ret);
}

void minus_float_sl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_sl_alloc(local, left, right, ret);
}

void minus_float_ds_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_ds_alloc(local, left, right, ret);
}

void minus_float_dd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_dd_alloc(local, left, right, ret);
}

void minus_float_dl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_dl_alloc(local, left, right, ret);
}

void minus_float_ls_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_ls_alloc(local, left, right, ret);
}

void minus_float_ld_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_ld_alloc(local, left, right, ret);
}

void minus_float_ll_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_float_ll_alloc(local, left, right, ret);
}

void minus_float_ss_heap(addr left, addr right, addr *ret)
{
	minus_float_ss_alloc(NULL, left, right, ret);
}

void minus_float_sd_heap(addr left, addr right, addr *ret)
{
	minus_float_sd_alloc(NULL, left, right, ret);
}

void minus_float_sl_heap(addr left, addr right, addr *ret)
{
	minus_float_sl_alloc(NULL, left, right, ret);
}

void minus_float_ds_heap(addr left, addr right, addr *ret)
{
	minus_float_ds_alloc(NULL, left, right, ret);
}

void minus_float_dd_heap(addr left, addr right, addr *ret)
{
	minus_float_dd_alloc(NULL, left, right, ret);
}

void minus_float_dl_heap(addr left, addr right, addr *ret)
{
	minus_float_dl_alloc(NULL, left, right, ret);
}

void minus_float_ls_heap(addr left, addr right, addr *ret)
{
	minus_float_ls_alloc(NULL, left, right, ret);
}

void minus_float_ld_heap(addr left, addr right, addr *ret)
{
	minus_float_ld_alloc(NULL, left, right, ret);
}

void minus_float_ll_heap(addr left, addr right, addr *ret)
{
	minus_float_ll_alloc(NULL, left, right, ret);
}


/*
 *  multiple
 */
void multi_float_fs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = single_float_fixnum(left) * RefSingleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);
}

void multi_float_fd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = double_float_fixnum(left) * RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);
}

void multi_float_fl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = long_float_fixnum(left) * RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

void multi_float_fs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_fs_alloc(local, left, right, ret);
}

void multi_float_fd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_fd_alloc(local, left, right, ret);
}

void multi_float_fl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_fl_alloc(local, left, right, ret);
}

void multi_float_fs_heap(addr left, addr right, addr *ret)
{
	multi_float_fs_alloc(NULL, left, right, ret);
}

void multi_float_fd_heap(addr left, addr right, addr *ret)
{
	multi_float_fd_alloc(NULL, left, right, ret);
}

void multi_float_fl_heap(addr left, addr right, addr *ret)
{
	multi_float_fl_alloc(NULL, left, right, ret);
}

void multi_float_bs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = single_float_bignum(left) * RefSingleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);
}

void multi_float_bd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = double_float_bignum(left) * RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);
}

void multi_float_bl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = long_float_bignum(left) * RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

void multi_float_bs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_bs_alloc(local, left, right, ret);
}

void multi_float_bd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_bd_alloc(local, left, right, ret);
}

void multi_float_bl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_bl_alloc(local, left, right, ret);
}

void multi_float_bs_heap(addr left, addr right, addr *ret)
{
	multi_float_bs_alloc(NULL, left, right, ret);
}

void multi_float_bd_heap(addr left, addr right, addr *ret)
{
	multi_float_bd_alloc(NULL, left, right, ret);
}

void multi_float_bl_heap(addr left, addr right, addr *ret)
{
	multi_float_bl_alloc(NULL, left, right, ret);
}

void multi_float_rs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = single_float_ratio(left) * RefSingleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);
}

void multi_float_rd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = double_float_ratio(left) * RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);
}

void multi_float_rl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = long_float_ratio(left) * RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

void multi_float_rs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_rs_alloc(local, left, right, ret);
}

void multi_float_rd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_rd_alloc(local, left, right, ret);
}

void multi_float_rl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_rl_alloc(local, left, right, ret);
}

void multi_float_rs_heap(addr left, addr right, addr *ret)
{
	multi_float_rs_alloc(NULL, left, right, ret);
}

void multi_float_rd_heap(addr left, addr right, addr *ret)
{
	multi_float_rd_alloc(NULL, left, right, ret);
}

void multi_float_rl_heap(addr left, addr right, addr *ret)
{
	multi_float_rl_alloc(NULL, left, right, ret);
}

void multi_float_ss_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefSingleFloat(left) * RefSingleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	single_float_alloc(local, ret, value);
}

void multi_float_sd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = ((double_float)RefSingleFloat(left)) * RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);
}

void multi_float_sl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefSingleFloat(left)) * RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

void multi_float_ds_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) * ((double_float)RefSingleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);
}

void multi_float_dd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefDoubleFloat(left) * RefDoubleFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	double_float_alloc(local, ret, value);
}

void multi_float_dl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = ((long_float)RefDoubleFloat(left)) * RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

void multi_float_ls_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	value = RefLongFloat(left) * ((long_float)RefSingleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

void multi_float_ld_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	value = RefLongFloat(left) * ((long_float)RefDoubleFloat(right));
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

void multi_float_ll_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	value = RefLongFloat(left) * RefLongFloat(right);
	float_errorcheck2(CONSTANT_COMMON_ASTERISK, value, left, right);
	long_float_alloc(local, ret, value);
}

void multi_float_ss_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_ss_alloc(local, left, right, ret);
}

void multi_float_sd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_sd_alloc(local, left, right, ret);
}

void multi_float_sl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_sl_alloc(local, left, right, ret);
}

void multi_float_ds_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_ds_alloc(local, left, right, ret);
}

void multi_float_dd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_dd_alloc(local, left, right, ret);
}

void multi_float_dl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_dl_alloc(local, left, right, ret);
}

void multi_float_ls_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_ls_alloc(local, left, right, ret);
}

void multi_float_ld_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_ld_alloc(local, left, right, ret);
}

void multi_float_ll_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_ll_alloc(local, left, right, ret);
}

void multi_float_ss_heap(addr left, addr right, addr *ret)
{
	multi_float_ss_alloc(NULL, left, right, ret);
}

void multi_float_sd_heap(addr left, addr right, addr *ret)
{
	multi_float_sd_alloc(NULL, left, right, ret);
}

void multi_float_sl_heap(addr left, addr right, addr *ret)
{
	multi_float_sl_alloc(NULL, left, right, ret);
}

void multi_float_ds_heap(addr left, addr right, addr *ret)
{
	multi_float_ds_alloc(NULL, left, right, ret);
}

void multi_float_dd_heap(addr left, addr right, addr *ret)
{
	multi_float_dd_alloc(NULL, left, right, ret);
}

void multi_float_dl_heap(addr left, addr right, addr *ret)
{
	multi_float_dl_alloc(NULL, left, right, ret);
}

void multi_float_ls_heap(addr left, addr right, addr *ret)
{
	multi_float_ls_alloc(NULL, left, right, ret);
}

void multi_float_ld_heap(addr left, addr right, addr *ret)
{
	multi_float_ld_alloc(NULL, left, right, ret);
}

void multi_float_ll_heap(addr left, addr right, addr *ret)
{
	multi_float_ll_alloc(NULL, left, right, ret);
}


/*
 *  inverse
 */
void inverse_single_float_alloc(LocalRoot local, addr pos, addr *ret)
{
	single_float value;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	GetSingleFloat(pos, &value);
	if (value == 0.0f)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, pos, NULL);
	value = 1.0f / value;
	float_errorcheck1(CONSTANT_COMMON_SLASH, value, pos);
	single_float_alloc(local, ret, value);
}

void inverse_double_float_alloc(LocalRoot local, addr pos, addr *ret)
{
	double_float value;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(pos, &value);
	if (value == 0.0)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, pos, NULL);
	value = 1.0 / value;
	float_errorcheck1(CONSTANT_COMMON_SLASH, value, pos);
	double_float_alloc(local, ret, value);
}

void inverse_long_float_alloc(LocalRoot local, addr pos, addr *ret)
{
	long_float value;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &value);
	if (value == 0.0L)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, pos, NULL);
	value = 1.0L / value;
	float_errorcheck1(CONSTANT_COMMON_SLASH, value, pos);
	long_float_alloc(local, ret, value);
}

void inverse_single_float_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	inverse_single_float_alloc(local, pos, ret);
}

void inverse_double_float_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	inverse_double_float_alloc(local, pos, ret);
}

void inverse_long_float_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	inverse_long_float_alloc(local, pos, ret);
}

void inverse_single_float_heap(addr pos, addr *ret)
{
	inverse_single_float_alloc(NULL, pos, ret);
}

void inverse_double_float_heap(addr pos, addr *ret)
{
	inverse_double_float_alloc(NULL, pos, ret);
}

void inverse_long_float_heap(addr pos, addr *ret)
{
	inverse_long_float_alloc(NULL, pos, ret);
}


/*
 *  division
 */
void div_float_fs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = single_float_fixnum(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

void div_float_fd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = double_float_fixnum(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

void div_float_fl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = long_float_fixnum(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

void div_float_fs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_fs_alloc(local, left, right, ret);
}

void div_float_fd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_fd_alloc(local, left, right, ret);
}

void div_float_fl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_fl_alloc(local, left, right, ret);
}

void div_float_fs_heap(addr left, addr right, addr *ret)
{
	div_float_fs_alloc(NULL, left, right, ret);
}

void div_float_fd_heap(addr left, addr right, addr *ret)
{
	div_float_fd_alloc(NULL, left, right, ret);
}

void div_float_fl_heap(addr left, addr right, addr *ret)
{
	div_float_fl_alloc(NULL, left, right, ret);
}

void div_float_sf_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetFixnum(right, &check);
	if (check == 0)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefSingleFloat(left) / ((single_float)check);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

void div_float_df_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type righterror");
	GetFixnum(right, &check);
	if (check == 0)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefDoubleFloat(left) / ((double_float)check);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

void div_float_lf_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	long_float value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetFixnum(right, &check);
	if (check == 0)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefLongFloat(left) / ((long_float)check);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

void div_float_sf_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_sf_alloc(local, left, right, ret);
}

void div_float_df_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_df_alloc(local, left, right, ret);
}

void div_float_lf_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_lf_alloc(local, left, right, ret);
}

void div_float_sf_heap(addr left, addr right, addr *ret)
{
	div_float_sf_alloc(NULL, left, right, ret);
}

void div_float_df_heap(addr left, addr right, addr *ret)
{
	div_float_df_alloc(NULL, left, right, ret);
}

void div_float_lf_heap(addr left, addr right, addr *ret)
{
	div_float_lf_alloc(NULL, left, right, ret);
}

void div_float_bs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = single_float_bignum(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

void div_float_bd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = double_float_bignum(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

void div_float_bl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = long_float_bignum(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

void div_float_bs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_bs_alloc(local, left, right, ret);
}

void div_float_bd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_bd_alloc(local, left, right, ret);
}

void div_float_bl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_bl_alloc(local, left, right, ret);
}

void div_float_bs_heap(addr left, addr right, addr *ret)
{
	div_float_bs_alloc(NULL, left, right, ret);
}

void div_float_bd_heap(addr left, addr right, addr *ret)
{
	div_float_bd_alloc(NULL, left, right, ret);
}

void div_float_bl_heap(addr left, addr right, addr *ret)
{
	div_float_bl_alloc(NULL, left, right, ret);
}

void div_float_sb_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (zerop_bignum(right))
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefSingleFloat(left) / single_float_bignum(right);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

void div_float_db_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (zerop_bignum(right))
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefDoubleFloat(left) / double_float_bignum(right);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

void div_float_lb_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetLongFloat(right, &value);
	if (zerop_bignum(right))
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefLongFloat(left) / long_float_bignum(right);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

void div_float_sb_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_sb_alloc(local, left, right, ret);
}

void div_float_db_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_db_alloc(local, left, right, ret);
}

void div_float_lb_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_lb_alloc(local, left, right, ret);
}

void div_float_sb_heap(addr left, addr right, addr *ret)
{
	div_float_sb_alloc(NULL, left, right, ret);
}

void div_float_db_heap(addr left, addr right, addr *ret)
{
	div_float_db_alloc(NULL, left, right, ret);
}

void div_float_lb_heap(addr left, addr right, addr *ret)
{
	div_float_lb_alloc(NULL, left, right, ret);
}

void div_float_rs_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = single_float_ratio(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

void div_float_rd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = double_float_ratio(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

void div_float_rl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = long_float_ratio(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

void div_float_rs_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_rs_alloc(local, left, right, ret);
}

void div_float_rd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_rd_alloc(local, left, right, ret);
}

void div_float_rl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_rl_alloc(local, left, right, ret);
}

void div_float_rs_heap(addr left, addr right, addr *ret)
{
	div_float_rs_alloc(NULL, left, right, ret);
}

void div_float_rd_heap(addr left, addr right, addr *ret)
{
	div_float_rd_alloc(NULL, left, right, ret);
}

void div_float_rl_heap(addr left, addr right, addr *ret)
{
	div_float_rl_alloc(NULL, left, right, ret);
}

void div_float_sr_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	if (zerop_ratio(right))
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefSingleFloat(left) / single_float_ratio(right);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

void div_float_dr_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	if (zerop_ratio(right))
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefDoubleFloat(left) / double_float_ratio(right);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

void div_float_lr_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	if (zerop_ratio(right))
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefLongFloat(left) / long_float_ratio(right);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

void div_float_sr_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_sr_alloc(local, left, right, ret);
}

void div_float_dr_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_dr_alloc(local, left, right, ret);
}

void div_float_lr_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_lr_alloc(local, left, right, ret);
}

void div_float_sr_heap(addr left, addr right, addr *ret)
{
	div_float_sr_alloc(NULL, left, right, ret);
}

void div_float_dr_heap(addr left, addr right, addr *ret)
{
	div_float_dr_alloc(NULL, left, right, ret);
}

void div_float_lr_heap(addr left, addr right, addr *ret)
{
	div_float_lr_alloc(NULL, left, right, ret);
}

void div_float_ss_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &value);
	if (value == 0.0f)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefSingleFloat(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	single_float_alloc(local, ret, value);
}

void div_float_sd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = ((double_float)RefSingleFloat(left)) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

void div_float_sl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = ((long_float)RefSingleFloat(left)) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

void div_float_ds_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float check;
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &check);
	if (check == 0.0f)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefDoubleFloat(left) / ((double_float)check);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

void div_float_dd_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &value);
	if (value == 0.0)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefDoubleFloat(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	double_float_alloc(local, ret, value);
}

void div_float_dl_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = ((long_float)RefDoubleFloat(left)) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

void div_float_ls_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	single_float check;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetSingleFloat(right, &check);
	if (check == 0.0f)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefLongFloat(left) / ((long_float)check);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

void div_float_ld_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	double_float check;
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetDoubleFloat(right, &check);
	if (check == 0.0)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefLongFloat(left) / ((long_float)check);
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

void div_float_ll_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	long_float value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetLongFloat(right, &value);
	if (value == 0.0L)
		division_by_zero_stdarg(CONSTANT_COMMON_SLASH, left, right, NULL);
	value = RefLongFloat(left) / value;
	float_errorcheck2(CONSTANT_COMMON_SLASH, value, left, right);
	long_float_alloc(local, ret, value);
}

void div_float_ss_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_ss_alloc(local, left, right, ret);
}

void div_float_sd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_sd_alloc(local, left, right, ret);
}

void div_float_sl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_sl_alloc(local, left, right, ret);
}

void div_float_ds_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_ds_alloc(local, left, right, ret);
}

void div_float_dd_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_dd_alloc(local, left, right, ret);
}

void div_float_dl_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_dl_alloc(local, left, right, ret);
}

void div_float_ls_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_ls_alloc(local, left, right, ret);
}

void div_float_ld_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_ld_alloc(local, left, right, ret);
}

void div_float_ll_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	div_float_ll_alloc(local, left, right, ret);
}

void div_float_ss_heap(addr left, addr right, addr *ret)
{
	div_float_ss_alloc(NULL, left, right, ret);
}

void div_float_sd_heap(addr left, addr right, addr *ret)
{
	div_float_sd_alloc(NULL, left, right, ret);
}

void div_float_sl_heap(addr left, addr right, addr *ret)
{
	div_float_sl_alloc(NULL, left, right, ret);
}

void div_float_ds_heap(addr left, addr right, addr *ret)
{
	div_float_ds_alloc(NULL, left, right, ret);
}

void div_float_dd_heap(addr left, addr right, addr *ret)
{
	div_float_dd_alloc(NULL, left, right, ret);
}

void div_float_dl_heap(addr left, addr right, addr *ret)
{
	div_float_dl_alloc(NULL, left, right, ret);
}

void div_float_ls_heap(addr left, addr right, addr *ret)
{
	div_float_ls_alloc(NULL, left, right, ret);
}

void div_float_ld_heap(addr left, addr right, addr *ret)
{
	div_float_ld_alloc(NULL, left, right, ret);
}

void div_float_ll_heap(addr left, addr right, addr *ret)
{
	div_float_ll_alloc(NULL, left, right, ret);
}


/*
 *  abs
 */
void abs_floats_alloc(LocalRoot local, addr left, addr *ret)
{
	single_float value;

	GetSingleFloat(left, &value);
	if (0.0f <= value)
		single_float_throw_alloc(local, left, ret);
	else
		single_float_alloc(local, ret, fabsf(value));
}

void abs_floatd_alloc(LocalRoot local, addr left, addr *ret)
{
	double_float value;

	GetDoubleFloat(left, &value);
	if (0.0 <= value)
		double_float_throw_alloc(local, left, ret);
	else
		double_float_alloc(local, ret, fabs(value));
}

void abs_floatl_alloc(LocalRoot local, addr left, addr *ret)
{
	long_float value;

	GetLongFloat(left, &value);
	if (0.0L <= value)
		long_float_throw_alloc(local, left, ret);
	else
		long_float_alloc(local, ret, fabsl(value));
}

void abs_floats_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	abs_floats_alloc(local, left, ret);
}

void abs_floatd_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	abs_floatd_alloc(local, left, ret);
}

void abs_floatl_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	abs_floatl_alloc(local, left, ret);
}

void abs_floats_heap(addr left, addr *ret)
{
	abs_floats_alloc(NULL, left, ret);
}

void abs_floatd_heap(addr left, addr *ret)
{
	abs_floatd_alloc(NULL, left, ret);
}

void abs_floatl_heap(addr left, addr *ret)
{
	abs_floatl_alloc(NULL, left, ret);
}


/*
 *  cast
 */
void cast_float_alloc(LocalRoot local, addr left, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_alloc(local, ret, left);
			break;

		case LISPTYPE_BIGNUM:
			single_float_bignum_alloc(local, ret, left);
			break;

		case LISPTYPE_RATIO:
			single_float_alloc(local, ret, single_float_ratio(left));
			break;

		case LISPTYPE_SINGLE_FLOAT:
			single_float_throw_alloc(local, left, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_throw_alloc(local, left, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_throw_alloc(local, left, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

void cast_float_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	cast_float_alloc(local, left, ret);
}

void cast_float_heap(addr left, addr *ret)
{
	cast_float_alloc(NULL, left, ret);
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

void multi_float_alloc(LocalRoot local, addr left, addr right, addr *ret)
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

void multi_float_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	multi_float_alloc(local, left, right, ret);
}

void multi_float_heap(LocalRoot local, addr left, addr right, addr *ret)
{
	multi_float_alloc(NULL, left, right, ret);
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

void plus_float_alloc(LocalRoot local, addr left, addr right, addr *ret)
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

void plus_float_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_float_alloc(local, left, right, ret);
}

void plus_float_heap(LocalRoot local, addr left, addr right, addr *ret)
{
	plus_float_alloc(NULL, left, right, ret);
}


/*
 *  sqrt
 */
void sqrt_single_float_alloc(LocalRoot local, addr left, addr *ret)
{
	addr zero;
	single_float value;

	GetSingleFloat(left, &value);
	if (value < 0) {
		single_float_alloc(local, &zero, 0.0f);
		single_float_alloc(local, &left, sqrtf(-value));
		complex_alloc(local, ret, zero, left);
	}
	else {
		single_float_alloc(local, ret, sqrtf(value));
	}
}

void sqrt_double_float_alloc(LocalRoot local, addr left, addr *ret)
{
	addr zero;
	double_float value;

	GetDoubleFloat(left, &value);
	if (value < 0) {
		double_float_alloc(local, &zero, 0.0);
		double_float_alloc(local, &left, sqrt(-value));
		complex_alloc(local, ret, zero, left);
	}
	else {
		double_float_alloc(local, ret, sqrt(value));
	}
}

void sqrt_long_float_alloc(LocalRoot local, addr left, addr *ret)
{
	addr zero;
	long_float value;

	GetLongFloat(left, &value);
	if (value < 0) {
		long_float_alloc(local, &zero, 0.0L);
		long_float_alloc(local, &left, sqrtl(-value));
		complex_alloc(local, ret, zero, left);
	}
	else {
		long_float_alloc(local, ret, sqrtl(value));
	}
}

void sqrt_float_alloc(LocalRoot local, addr left, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_SINGLE_FLOAT:
			sqrt_single_float_alloc(local, left, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			sqrt_double_float_alloc(local, left, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			sqrt_long_float_alloc(local, left, ret);
			break;

		default:
			TypeError(left, FLOAT);
			break;
	}
}

void sqrt_float_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	sqrt_float_alloc(local, left, ret);
}

void sqrt_float_heap(LocalRoot local, addr left, addr *ret)
{
	sqrt_float_alloc(NULL, left, ret);
}

