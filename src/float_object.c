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

_g int single_float_check_alloc_(LocalRoot local, addr *ret, single_float value)
{
	Return_float_errorcheck0(CONSTANT_COMMON_FLOAT, value);
	single_float_alloc(local, ret, value);
	return 0;
}

_g int single_float_check_local_(LocalRoot local, addr *ret, single_float value)
{
	Check(local == NULL, "local error");
	return single_float_check_alloc_(local, ret, value);
}

_g int single_float_check_heap_(addr *ret, single_float value)
{
	return single_float_check_alloc_(NULL, ret, value);
}

_g int double_float_check_alloc_(LocalRoot local, addr *ret, double_float value)
{
	Return_float_errorcheck0(CONSTANT_COMMON_FLOAT, value);
	double_float_alloc(local, ret, value);
	return 0;
}

_g int double_float_check_local_(LocalRoot local, addr *ret, double_float value)
{
	Check(local == NULL, "local error");
	return double_float_check_alloc_(local, ret, value);
}

_g int double_float_check_heap_(addr *ret, double_float value)
{
	return double_float_check_alloc_(NULL, ret, value);
}

_g int long_float_check_alloc_(LocalRoot local, addr *ret, long_float value)
{
	Return_float_errorcheck0(CONSTANT_COMMON_FLOAT, value);
	long_float_alloc(local, ret, value);
	return 0;
}

_g int long_float_check_local_(LocalRoot local, addr *ret, long_float value)
{
	Check(local == NULL, "local error");
	return long_float_check_alloc_(local, ret, value);
}

_g int long_float_check_heap_(addr *ret, long_float value)
{
	return long_float_check_alloc_(NULL, ret, value);
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

_g int float_throw_heap_(addr pos, addr *ret)
{
	return float_throw_alloc_(NULL, pos, ret);
}

_g int float_throw_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return float_throw_alloc_(local, pos, ret);
}

_g int float_throw_alloc_(LocalRoot local, addr pos, addr *ret)
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
			*ret = Nil;
			return TypeError_(pos, FLOAT);
	}

	return 0;
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
			*ret = Nil;
			Abort("type error");
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

_g int float_fltclass_(constindex index, fltclasstype type, ...)
{
	va_list args;
	addr list;

	va_start(args, type);
	list_stdarg_alloc(NULL, &list, args);
	va_end(args);

	switch (type) {
		case fltclass_overflow:
			return call_float_overflow_const_(NULL, index, list);

		case fltclass_underflow:
			return call_float_underflow_const_(NULL, index, list);

		case fltclass_nan:
		default:
			return call_float_invalid_const_(NULL, index, list);
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

_g int check_strtof_(const char *str, addr pos, single_float *ret)
{
	fltclasstype type;
	single_float value;

	value = strtof_c(str);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		*ret = 0.0f;
		return float_fltclass_(CONSTANT_SYSTEM_CAST_SINGLE_FLOAT, type, pos, NULL);
	}

	return Result(ret, value);
}

_g int check_strtod_(const char *str, addr pos, double_float *ret)
{
	fltclasstype type;
	double_float value;

	value = strtod_c(str);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		*ret = 0.0;
		return float_fltclass_(CONSTANT_SYSTEM_CAST_DOUBLE_FLOAT, type, pos, NULL);
	}

	return Result(ret, value);
}

_g int check_strtold_(const char *str, addr pos, long_float *ret)
{
	fltclasstype type;
	long_float value;

	value = strtold_c(str);
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		*ret = 0.0L;
		return float_fltclass_(CONSTANT_SYSTEM_CAST_LONG_FLOAT, type, pos, NULL);
	}

	return Result(ret, value);
}

_g int check_strtof_reverse_(const char *str, addr pos, single_float *ret)
{
	fltclasstype type;
	single_float value;

	value = strtof_c(str);
	type = getfltclassify_reverse(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		*ret = 0.0f;
		return float_fltclass_(CONSTANT_SYSTEM_CAST_SINGLE_FLOAT, type, pos, NULL);
	}

	return Result(ret, value);
}

_g int check_strtod_reverse_(const char *str, addr pos, double_float *ret)
{
	fltclasstype type;
	double_float value;

	value = strtod_c(str);
	type = getfltclassify_reverse(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		*ret = 0.0;
		return float_fltclass_(CONSTANT_SYSTEM_CAST_DOUBLE_FLOAT, type, pos, NULL);
	}

	return Result(ret, value);
}

_g int check_strtold_reverse_(const char *str, addr pos, long_float *ret)
{
	fltclasstype type;
	long_float value;

	value = strtold_c(str);
	type = getfltclassify_reverse(value);
	if (type != fltclass_normal) {
		if (pos == NULL)
			strvect_char_heap(&pos, str);
		*ret = 0.0L;
		return float_fltclass_(CONSTANT_SYSTEM_CAST_LONG_FLOAT, type, pos, NULL);
	}

	return Result(ret, value);
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
_g int cast_sd_float_(single_float v, double_float *ret)
{
	return Result(ret, (double_float)v);
}

_g int cast_sl_float_(single_float v, long_float *ret)
{
	return Result(ret, (long_float)v);
}

_g int cast_ds_float_(double_float v, single_float *ret)
{
	fltclasstype type;
	single_float value;
	addr pos;

	value = (single_float)v;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		double_float_heap(&pos, v);
		*ret = 0.0f;
		return float_fltclass_(CONSTANT_COMMON_COERCE, type, pos, NULL);
	}

	return Result(ret, value);
}

_g int cast_dl_float_(double_float v, long_float *ret)
{
	return Result(ret, (long_float)v);
}

_g int cast_ls_float_(long_float v, single_float *ret)
{
	fltclasstype type;
	single_float value;
	addr pos;

	value = (single_float)v;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&pos, v);
		*ret = 0.0f;
		return float_fltclass_(CONSTANT_COMMON_COERCE, type, pos, NULL);
	}

	return Result(ret, value);
}

_g int cast_ld_float_(long_float v, double_float *ret)
{
	fltclasstype type;
	double_float value;
	addr pos;

	value = (double_float)v;
	type = getfltclassify(value);
	if (type != fltclass_normal) {
		long_float_heap(&pos, v);
		*ret = 0.0;
		return float_fltclass_(CONSTANT_COMMON_COERCE, type, pos, NULL);
	}

	return Result(ret, value);
}

_g int cast_sd_value_(addr pos, double_float *ret)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	return Result(ret, (double_float)RefSingleFloat(pos));
}

_g int cast_sl_value_(addr pos, long_float *ret)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	return Result(ret, (long_float)RefSingleFloat(pos));
}

_g int cast_ds_value_(addr pos, single_float *ret)
{
	single_float v;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	v = (single_float)RefDoubleFloat(pos);
	Return_float_errorcheck1(CONSTANT_COMMON_COERCE, v, pos);

	return Result(ret, v);
}

_g int cast_dl_value_(addr pos, long_float *ret)
{
	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	return Result(ret, (long_float)RefDoubleFloat(pos));
}

_g int cast_ls_value_(addr pos, single_float *ret)
{
	single_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	v = (single_float)RefLongFloat(pos);
	Return_float_errorcheck1(CONSTANT_COMMON_COERCE, v, pos);

	return Result(ret, v);
}

_g int cast_ld_value_(addr pos, double_float *ret)
{
	double_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	v = (double_float)RefLongFloat(pos);
	Return_float_errorcheck1(CONSTANT_COMMON_COERCE, v, pos);

	return Result(ret, v);
}

_g int cast_ss_value_(addr pos, single_float *ret)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	GetSingleFloat(pos, ret);
	return 0;
}

_g int cast_dd_value_(addr pos, double_float *ret)
{
	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(pos, ret);
	return 0;
}

_g int cast_ll_value_(addr pos, long_float *ret)
{
	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, ret);
	return 0;
}


/*
 *  sqrt
 */
static int sqrt_single_float_alloc_(LocalRoot local, addr left, addr *ret)
{
	addr zero;
	single_float value;

	GetSingleFloat(left, &value);
	if (value < 0) {
		single_float_alloc(local, &zero, 0.0f);
		single_float_alloc(local, &left, sqrtf(-value));
		return complex_alloc_(local, ret, zero, left);
	}
	else {
		single_float_alloc(local, ret, sqrtf(value));
		return 0;
	}
}

static int sqrt_double_float_alloc_(LocalRoot local, addr left, addr *ret)
{
	addr zero;
	double_float value;

	GetDoubleFloat(left, &value);
	if (value < 0) {
		double_float_alloc(local, &zero, 0.0);
		double_float_alloc(local, &left, sqrt(-value));
		return complex_alloc_(local, ret, zero, left);
	}
	else {
		double_float_alloc(local, ret, sqrt(value));
		return 0;
	}
}

static int sqrt_long_float_alloc_(LocalRoot local, addr left, addr *ret)
{
	addr zero;
	long_float value;

	GetLongFloat(left, &value);
	if (value < 0) {
		long_float_alloc(local, &zero, 0.0L);
		long_float_alloc(local, &left, sqrtl(-value));
		return complex_alloc_(local, ret, zero, left);
	}
	else {
		long_float_alloc(local, ret, sqrtl(value));
		return 0;
	}
}

_g int sqrt_float_alloc_(LocalRoot local, addr left, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_SINGLE_FLOAT:
			return sqrt_single_float_alloc_(local, left, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return sqrt_double_float_alloc_(local, left, ret);

		case LISPTYPE_LONG_FLOAT:
			return sqrt_long_float_alloc_(local, left, ret);

		default:
			*ret = Nil;
			return TypeError_(left, FLOAT);
	}
}

_g int sqrt_float_local_(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	return sqrt_float_alloc_(local, left, ret);
}

_g int sqrt_float_heap_(LocalRoot local, addr left, addr *ret)
{
	return sqrt_float_alloc_(NULL, left, ret);
}

