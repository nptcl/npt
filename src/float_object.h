#ifndef __FLOAT_OBJECT_HEADER__
#define __FLOAT_OBJECT_HEADER__

#include <math.h>
#include "constant.h"
#include "local.h"
#include "typedef.h"

enum fltclass {
	fltclass_normal,
	fltclass_overflow,
	fltclass_underflow,
	fltclass_nan
};
#ifdef __cplusplus
typedef int fltclasstype;
#else
typedef enum fltclass fltclasstype;
#endif

_g fltclasstype fltclassify(int check, int sign);
_g void float_fltclass(constindex index, fltclasstype type, ...);
#define getfltclassify(v) fltclassify(fpclassify(v), signbit(v))
#define getfltclassify_reverse(v) fltclassify(fpclassify(v), (! signbit(v)))
#define float_errorcheck1(index, v, left) { \
	fltclasstype __type = getfltclassify(v); \
	if (__type != fltclass_normal) { \
		float_fltclass((index), __type, (left), NULL); \
	} \
}
#define float_errorcheck2(index, v, left, right) { \
	fltclasstype __type = getfltclassify(v); \
	if (__type != fltclass_normal) { \
		float_fltclass((index), __type, (left), (right), NULL); \
	} \
}

_g void single_float_check_alloc(LocalRoot local, addr *ret, single_float value);
_g void single_float_check_local(LocalRoot local, addr *ret, single_float value);
_g void single_float_check_heap(addr *ret, single_float value);
_g void double_float_check_local(LocalRoot local, addr *ret, double_float value);
_g void double_float_check_alloc(LocalRoot local, addr *ret, double_float value);
_g void double_float_check_heap(addr *ret, double_float value);
_g void long_float_check_alloc(LocalRoot local, addr *ret, long_float value);
_g void long_float_check_local(LocalRoot local, addr *ret, long_float value);
_g void long_float_check_heap(addr *ret, long_float value);

_g void single_float_throw_heap(addr pos, addr *ret);
_g void double_float_throw_heap(addr pos, addr *ret);
_g void long_float_throw_heap(addr pos, addr *ret);
_g void single_float_throw_local(LocalRoot local, addr pos, addr *ret);
_g void double_float_throw_local(LocalRoot local, addr pos, addr *ret);
_g void long_float_throw_local(LocalRoot local, addr pos, addr *ret);
_g void single_float_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void double_float_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void long_float_throw_alloc(LocalRoot local, addr pos, addr *ret);

_g void float_throw_heap(addr pos, addr *ret);
_g void float_throw_local(LocalRoot local, addr pos, addr *ret);
_g void float_throw_alloc(LocalRoot local, addr pos, addr *ret);
#define float_result_heap float_throw_heap
#define float_result_local float_throw_local
#define float_result_alloc float_throw_alloc
_g void float_copy_alloc(LocalRoot local, addr pos, addr *ret);
_g void float_copy_local(LocalRoot local, addr pos, addr *ret);
_g void float_copy_heap(addr pos, addr *ret);

_g single_float check_strtof(const char *str, addr pos);
_g double_float check_strtod(const char *str, addr pos);
_g long_float check_strtold(const char *str, addr pos);
_g single_float check_strtof_reverse(const char *str, addr pos);
_g double_float check_strtod_reverse(const char *str, addr pos);
_g long_float check_strtold_reverse(const char *str, addr pos);

_g void abs_floats_alloc(LocalRoot local, addr left, addr *ret);
_g void abs_floatd_alloc(LocalRoot local, addr left, addr *ret);
_g void abs_floatl_alloc(LocalRoot local, addr left, addr *ret);
_g void abs_floats_local(LocalRoot local, addr left, addr *ret);
_g void abs_floatd_local(LocalRoot local, addr left, addr *ret);
_g void abs_floatl_local(LocalRoot local, addr left, addr *ret);
_g void abs_floats_heap(addr left, addr *ret);
_g void abs_floatd_heap(addr left, addr *ret);
_g void abs_floatl_heap(addr left, addr *ret);

_g double_float cast_sd_float(single_float v);
_g long_float cast_sl_float(single_float v);
_g single_float cast_ds_float(double_float v);
_g long_float cast_dl_float(double_float v);
_g single_float cast_ls_float(long_float v);
_g double_float cast_ld_float(long_float v);
#define cast_ss_float(x) (x)
#define cast_dd_float(x) (x)
#define cast_ll_float(x) (x)

_g double_float cast_sd_value(addr pos);
_g long_float cast_sl_value(addr pos);
_g single_float cast_ds_value(addr pos);
_g long_float cast_dl_value(addr pos);
_g single_float cast_ls_value(addr pos);
_g double_float cast_ld_value(addr pos);
#define cast_ss_value RefSingleFloat
#define cast_dd_value RefDoubleFloat
#define cast_ll_value RefLongFloat

_g void cast_float_alloc(LocalRoot local, addr left, addr *ret);
_g void cast_float_local(LocalRoot local, addr left, addr *ret);
_g void cast_float_heap(addr left, addr *ret);

_g void sqrt_float_alloc(LocalRoot local, addr left, addr *ret);
_g void sqrt_float_local(LocalRoot local, addr left, addr *ret);
_g void sqrt_float_heap(LocalRoot local, addr left, addr *ret);

#endif

