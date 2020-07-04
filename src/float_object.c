#include <locale.h>
#include "bignum.h"
#include "c99.h"
#include "cmpl.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "float_object.h"
#include "object.h"
#include "ratio.h"
#include "strvect.h"
#include "typedef.h"

_g void single_float_check_alloc(LocalRoot local, addr *ret, single_float value)
{
	fltclasstype type;

	type = getfltclassify(value);
	if (type != fltclass_normal)
		float_fltclass(CONSTANT_COMMON_FLOAT, type, NULL);
	single_float_alloc(local, ret, value);
}

_g void single_float_check_local(LocalRoot local, addr *ret, single_float value)
{
	Check(local == NULL, "local error");
	single_float_check_alloc(local, ret, value);
}

_g void single_float_check_heap(addr *ret, single_float value)
{
	single_float_check_alloc(NULL, ret, value);
}

_g void double_float_check_alloc(LocalRoot local, addr *ret, double_float value)
{
	fltclasstype type;

	type = getfltclassify(value);
	if (type != fltclass_normal)
		float_fltclass(CONSTANT_COMMON_FLOAT, type, NULL);
	double_float_alloc(local, ret, value);
}

_g void double_float_check_local(LocalRoot local, addr *ret, double_float value)
{
	Check(local == NULL, "local error");
	double_float_check_alloc(local, ret, value);
}

_g void double_float_check_heap(addr *ret, double_float value)
{
	double_float_check_alloc(NULL, ret, value);
}

_g void long_float_check_alloc(LocalRoot local, addr *ret, long_float value)
{
	fltclasstype type;

	type = getfltclassify(value);
	if (type != fltclass_normal)
		float_fltclass(CONSTANT_COMMON_FLOAT, type, NULL);
	long_float_alloc(local, ret, value);
}

_g void long_float_check_local(LocalRoot local, addr *ret, long_float value)
{
	Check(local == NULL, "local error");
	long_float_check_alloc(local, ret, value);
}

_g void long_float_check_heap(addr *ret, long_float value)
{
	long_float_check_alloc(NULL, ret, value);
}

_g void single_float_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		single_float_heap(ret, RefSingleFloat(pos));
	else
		*ret = pos;
}

_g void double_float_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		double_float_heap(ret, RefDoubleFloat(pos));
	else
		*ret = pos;
}

_g void long_float_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		long_float_heap(ret, RefLongFloat(pos));
	else
		*ret = pos;
}

_g void single_float_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		single_float_local(local, ret, RefSingleFloat(pos));
}

_g void double_float_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		double_float_local(local, ret, RefDoubleFloat(pos));
}

_g void long_float_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		long_float_local(local, ret, RefLongFloat(pos));
}

_g void single_float_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		single_float_throw_local(local, pos, ret);
	else
		single_float_throw_heap(pos, ret);
}

_g void double_float_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		double_float_throw_local(local, pos, ret);
	else
		double_float_throw_heap(pos, ret);
}

_g void long_float_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		long_float_throw_local(local, pos, ret);
	else
		long_float_throw_heap(pos, ret);
}

_g void float_throw_heap(addr pos, addr *ret)
{
	float_throw_alloc(NULL, pos, ret);
}

_g void float_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	float_throw_alloc(local, pos, ret);
}

_g void float_throw_alloc(LocalRoot local, addr pos, addr *ret)
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

static void single_float_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	single_float_alloc(local, ret, RefSingleFloat(pos));
}

static void double_float_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	double_float_alloc(local, ret, RefDoubleFloat(pos));
}

static void long_float_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_LONG_FLOAT);
	long_float_alloc(local, ret, RefLongFloat(pos));
}

_g void float_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			single_float_copy_alloc(local, pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_copy_alloc(local, pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_copy_alloc(local, pos, ret);
			break;

		case LISPTYPE_SHORT_FLOAT:
			Abort("short float is not implemented.");
			break;

		default:
			TypeError(pos, FLOAT);
			break;
	}
}

_g void float_copy_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	float_copy_alloc(local, pos, ret);
}

_g void float_copy_heap(addr pos, addr *ret)
{
	float_copy_alloc(NULL, pos, ret);
}


/*
 *  strtof
 */
_g fltclasstype fltclassify(int check, int sign)
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

_g void float_fltclass(constindex index, fltclasstype type, ...)
{
	va_list args;
	addr list;

	va_start(args, type);
	list_stdarg_alloc(NULL, &list, args);
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

_g single_float check_strtof(const char *str, addr pos)
{
	fltclasstype type;
	single_float value;

	value = strtof_c(str);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		float_fltclass(CONSTANT_SYSTEM_CAST_SINGLE_FLOAT, type, pos, NULL);
	}

	return value;
}

_g double_float check_strtod(const char *str, addr pos)
{
	fltclasstype type;
	double_float value;

	value = strtod_c(str);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		float_fltclass(CONSTANT_SYSTEM_CAST_DOUBLE_FLOAT, type, pos, NULL);
	}

	return value;
}

_g long_float check_strtold(const char *str, addr pos)
{
	fltclasstype type;
	long_float value;

	value = strtold_c(str);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		float_fltclass(CONSTANT_SYSTEM_CAST_LONG_FLOAT, type, pos, NULL);
	}

	return value;
}

_g single_float check_strtof_reverse(const char *str, addr pos)
{
	fltclasstype type;
	single_float value;

	value = strtof_c(str);
	type = getfltclassify_reverse(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		float_fltclass(CONSTANT_SYSTEM_CAST_SINGLE_FLOAT, type, pos, NULL);
	}

	return value;
}

_g double_float check_strtod_reverse(const char *str, addr pos)
{
	fltclasstype type;
	double_float value;

	value = strtod_c(str);
	type = getfltclassify_reverse(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		float_fltclass(CONSTANT_SYSTEM_CAST_DOUBLE_FLOAT, type, pos, NULL);
	}

	return value;
}

_g long_float check_strtold_reverse(const char *str, addr pos)
{
	fltclasstype type;
	long_float value;

	value = strtold_c(str);
	type = getfltclassify_reverse(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		float_fltclass(CONSTANT_SYSTEM_CAST_LONG_FLOAT, type, pos, NULL);
	}

	return value;
}


/*
 *  abs
 */
_g void abs_floats_alloc(LocalRoot local, addr left, addr *ret)
{
	single_float value;

	GetSingleFloat(left, &value);
	if (0.0f <= value)
		single_float_throw_alloc(local, left, ret);
	else
		single_float_alloc(local, ret, fabsf(value));
}

_g void abs_floatd_alloc(LocalRoot local, addr left, addr *ret)
{
	double_float value;

	GetDoubleFloat(left, &value);
	if (0.0 <= value)
		double_float_throw_alloc(local, left, ret);
	else
		double_float_alloc(local, ret, fabs(value));
}

_g void abs_floatl_alloc(LocalRoot local, addr left, addr *ret)
{
	long_float value;

	GetLongFloat(left, &value);
	if (0.0L <= value)
		long_float_throw_alloc(local, left, ret);
	else
		long_float_alloc(local, ret, fabsl(value));
}

_g void abs_floats_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	abs_floats_alloc(local, left, ret);
}

_g void abs_floatd_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	abs_floatd_alloc(local, left, ret);
}

_g void abs_floatl_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	abs_floatl_alloc(local, left, ret);
}

_g void abs_floats_heap(addr left, addr *ret)
{
	abs_floats_alloc(NULL, left, ret);
}

_g void abs_floatd_heap(addr left, addr *ret)
{
	abs_floatd_alloc(NULL, left, ret);
}

_g void abs_floatl_heap(addr left, addr *ret)
{
	abs_floatl_alloc(NULL, left, ret);
}


/*
 *  cast
 */
_g double_float cast_sd_float(single_float v)
{
	return (double_float)v;
}

_g long_float cast_sl_float(single_float v)
{
	return (long_float)v;
}

_g single_float cast_ds_float(double_float v)
{
	fltclasstype type;
	single_float ret;
	addr pos;

	ret = (single_float)v;
	type = getfltclassify(ret);
	if (type != fltclass_normal) {
		double_float_heap(&pos, v);
		float_fltclass(CONSTANT_COMMON_COERCE, type, pos, NULL);
	}

	return ret;
}

_g long_float cast_dl_float(double_float v)
{
	return (long_float)v;
}

_g single_float cast_ls_float(long_float v)
{
	fltclasstype type;
	single_float ret;
	addr pos;

	ret = (single_float)v;
	type = getfltclassify(ret);
	if (type != fltclass_normal) {
		long_float_heap(&pos, v);
		float_fltclass(CONSTANT_COMMON_COERCE, type, pos, NULL);
	}

	return ret;
}

_g double_float cast_ld_float(long_float v)
{
	fltclasstype type;
	double_float ret;
	addr pos;

	ret = (double_float)v;
	type = getfltclassify(ret);
	if (type != fltclass_normal) {
		long_float_heap(&pos, v);
		float_fltclass(CONSTANT_COMMON_COERCE, type, pos, NULL);
	}

	return ret;
}

_g double_float cast_sd_value(addr pos)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	return (double_float)RefSingleFloat(pos);
}

_g long_float cast_sl_value(addr pos)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	return (long_float)RefSingleFloat(pos);
}

_g single_float cast_ds_value(addr pos)
{
	single_float v;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	v = (single_float)RefDoubleFloat(pos);
	float_errorcheck1(CONSTANT_COMMON_COERCE, v, pos);

	return v;
}

_g long_float cast_dl_value(addr pos)
{
	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	return (long_float)RefDoubleFloat(pos);
}

_g single_float cast_ls_value(addr pos)
{
	single_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	v = (single_float)RefLongFloat(pos);
	float_errorcheck1(CONSTANT_COMMON_COERCE, v, pos);

	return v;
}

_g double_float cast_ld_value(addr pos)
{
	double_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	v = (double_float)RefLongFloat(pos);
	float_errorcheck1(CONSTANT_COMMON_COERCE, v, pos);

	return v;
}

_g void cast_float_alloc(LocalRoot local, addr left, addr *ret)
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

_g void cast_float_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	cast_float_alloc(local, left, ret);
}

_g void cast_float_heap(addr left, addr *ret)
{
	cast_float_alloc(NULL, left, ret);
}


/*
 *  sqrt
 */
_g void sqrt_single_float_alloc(LocalRoot local, addr left, addr *ret)
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

_g void sqrt_double_float_alloc(LocalRoot local, addr left, addr *ret)
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

_g void sqrt_long_float_alloc(LocalRoot local, addr left, addr *ret)
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

_g void sqrt_float_alloc(LocalRoot local, addr left, addr *ret)
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

_g void sqrt_float_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	sqrt_float_alloc(local, left, ret);
}

_g void sqrt_float_heap(LocalRoot local, addr left, addr *ret)
{
	sqrt_float_alloc(NULL, left, ret);
}

