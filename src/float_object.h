#ifndef __FLOAT_OBJECT_HEADER__
#define __FLOAT_OBJECT_HEADER__

#include <math.h>
#include "constant.h"
#include "local.h"
#include "typedef.h"

#define fltclassify _n(fltclassify)
#define float_fltclass_ _n(float_fltclass_)
#define single_float_check_alloc_ _n(single_float_check_alloc_)
#define single_float_check_local_ _n(single_float_check_local_)
#define single_float_check_heap_ _n(single_float_check_heap_)
#define double_float_check_local_ _n(double_float_check_local_)
#define double_float_check_alloc_ _n(double_float_check_alloc_)
#define double_float_check_heap_ _n(double_float_check_heap_)
#define long_float_check_alloc_ _n(long_float_check_alloc_)
#define long_float_check_local_ _n(long_float_check_local_)
#define long_float_check_heap_ _n(long_float_check_heap_)
#define single_float_throw_heap _n(single_float_throw_heap)
#define double_float_throw_heap _n(double_float_throw_heap)
#define long_float_throw_heap _n(long_float_throw_heap)
#define single_float_throw_local _n(single_float_throw_local)
#define double_float_throw_local _n(double_float_throw_local)
#define long_float_throw_local _n(long_float_throw_local)
#define single_float_throw_alloc _n(single_float_throw_alloc)
#define double_float_throw_alloc _n(double_float_throw_alloc)
#define long_float_throw_alloc _n(long_float_throw_alloc)
#define float_throw_heap_ _n(float_throw_heap_)
#define float_throw_local_ _n(float_throw_local_)
#define float_throw_alloc_ _n(float_throw_alloc_)
#define float_copy_alloc _n(float_copy_alloc)
#define float_copy_local _n(float_copy_local)
#define float_copy_heap _n(float_copy_heap)
#define check_strtof_ _n(check_strtof_)
#define check_strtod_ _n(check_strtod_)
#define check_strtold_ _n(check_strtold_)
#define check_strtof_reverse_ _n(check_strtof_reverse_)
#define check_strtod_reverse_ _n(check_strtod_reverse_)
#define check_strtold_reverse_ _n(check_strtold_reverse_)
#define abs_floats_alloc _n(abs_floats_alloc)
#define abs_floatd_alloc _n(abs_floatd_alloc)
#define abs_floatl_alloc _n(abs_floatl_alloc)
#define abs_floats_local _n(abs_floats_local)
#define abs_floatd_local _n(abs_floatd_local)
#define abs_floatl_local _n(abs_floatl_local)
#define abs_floats_heap _n(abs_floats_heap)
#define abs_floatd_heap _n(abs_floatd_heap)
#define abs_floatl_heap _n(abs_floatl_heap)
#define cast_sd_float_ _n(cast_sd_float_)
#define cast_sl_float_ _n(cast_sl_float_)
#define cast_ds_float_ _n(cast_ds_float_)
#define cast_dl_float_ _n(cast_dl_float_)
#define cast_ls_float_ _n(cast_ls_float_)
#define cast_ld_float_ _n(cast_ld_float_)
#define cast_sd_value_ _n(cast_sd_value_)
#define cast_sl_value_ _n(cast_sl_value_)
#define cast_ds_value_ _n(cast_ds_value_)
#define cast_dl_value_ _n(cast_dl_value_)
#define cast_ls_value_ _n(cast_ls_value_)
#define cast_ld_value_ _n(cast_ld_value_)
#define cast_ss_value_ _n(cast_ss_value_)
#define cast_dd_value_ _n(cast_dd_value_)
#define cast_ll_value_ _n(cast_ll_value_)
#define sqrt_float_alloc_ _n(sqrt_float_alloc_)
#define sqrt_float_local_ _n(sqrt_float_local_)
#define sqrt_float_heap_ _n(sqrt_float_heap_)

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
_g int float_fltclass_(constindex index, fltclasstype type, ...);
#define getfltclassify(v) fltclassify(fpclassify(v), signbit(v))
#define getfltclassify_reverse(v) fltclassify(fpclassify(v), (! signbit(v)))

#define Return_float_errorcheck0(index, v) { \
	fltclasstype __type = getfltclassify(v); \
	if (__type != fltclass_normal) { \
		Return(float_fltclass_((index), __type, NULL)); \
	} \
}
#define Return_float_errorcheck1(index, v, left) { \
	fltclasstype __type = getfltclassify(v); \
	if (__type != fltclass_normal) { \
		Return(float_fltclass_((index), __type, (left), NULL)); \
	} \
}
#define Return_float_errorcheck2(index, v, left, right) { \
	fltclasstype __type = getfltclassify(v); \
	if (__type != fltclass_normal) { \
		Return(float_fltclass_((index), __type, (left), (right), NULL)); \
	} \
}

_g int single_float_check_alloc_(LocalRoot local, addr *ret, single_float value);
_g int single_float_check_local_(LocalRoot local, addr *ret, single_float value);
_g int single_float_check_heap_(addr *ret, single_float value);
_g int double_float_check_local_(LocalRoot local, addr *ret, double_float value);
_g int double_float_check_alloc_(LocalRoot local, addr *ret, double_float value);
_g int double_float_check_heap_(addr *ret, double_float value);
_g int long_float_check_alloc_(LocalRoot local, addr *ret, long_float value);
_g int long_float_check_local_(LocalRoot local, addr *ret, long_float value);
_g int long_float_check_heap_(addr *ret, long_float value);

_g void single_float_throw_heap(addr pos, addr *ret);
_g void double_float_throw_heap(addr pos, addr *ret);
_g void long_float_throw_heap(addr pos, addr *ret);
_g void single_float_throw_local(LocalRoot local, addr pos, addr *ret);
_g void double_float_throw_local(LocalRoot local, addr pos, addr *ret);
_g void long_float_throw_local(LocalRoot local, addr pos, addr *ret);
_g void single_float_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void double_float_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void long_float_throw_alloc(LocalRoot local, addr pos, addr *ret);

_g int float_throw_heap_(addr pos, addr *ret);
_g int float_throw_local_(LocalRoot local, addr pos, addr *ret);
_g int float_throw_alloc_(LocalRoot local, addr pos, addr *ret);
#define float_result_heap_ float_throw_heap_
#define float_result_local_ float_throw_local_
#define float_result_alloc_ float_throw_alloc_
_g void float_copy_alloc(LocalRoot local, addr pos, addr *ret);
_g void float_copy_local(LocalRoot local, addr pos, addr *ret);
_g void float_copy_heap(addr pos, addr *ret);

_g int check_strtof_(const char *str, addr pos, single_float *ret);
_g int check_strtod_(const char *str, addr pos, double_float *ret);
_g int check_strtold_(const char *str, addr pos, long_float *ret);
_g int check_strtof_reverse_(const char *str, addr pos, single_float *ret);
_g int check_strtod_reverse_(const char *str, addr pos, double_float *ret);
_g int check_strtold_reverse_(const char *str, addr pos, long_float *ret);

_g void abs_floats_alloc(LocalRoot local, addr left, addr *ret);
_g void abs_floatd_alloc(LocalRoot local, addr left, addr *ret);
_g void abs_floatl_alloc(LocalRoot local, addr left, addr *ret);
_g void abs_floats_local(LocalRoot local, addr left, addr *ret);
_g void abs_floatd_local(LocalRoot local, addr left, addr *ret);
_g void abs_floatl_local(LocalRoot local, addr left, addr *ret);
_g void abs_floats_heap(addr left, addr *ret);
_g void abs_floatd_heap(addr left, addr *ret);
_g void abs_floatl_heap(addr left, addr *ret);

_g int cast_sd_float_(single_float v, double_float *ret);
_g int cast_sl_float_(single_float v, long_float *ret);
_g int cast_ds_float_(double_float v, single_float *ret);
_g int cast_dl_float_(double_float v, long_float *ret);
_g int cast_ls_float_(long_float v, single_float *ret);
_g int cast_ld_float_(long_float v, double_float *ret);
#define cast_ss_float(x,y) (*(y) = (x))
#define cast_dd_float(x,y) (*(y) = (x))
#define cast_ll_float(x,y) (*(y) = (x))

_g int cast_sd_value_(addr pos, double_float *ret);
_g int cast_sl_value_(addr pos, long_float *ret);
_g int cast_ds_value_(addr pos, single_float *ret);
_g int cast_dl_value_(addr pos, long_float *ret);
_g int cast_ls_value_(addr pos, single_float *ret);
_g int cast_ld_value_(addr pos, double_float *ret);
_g int cast_ss_value_(addr pos, single_float *ret);
_g int cast_dd_value_(addr pos, double_float *ret);
_g int cast_ll_value_(addr pos, long_float *ret);

_g int sqrt_float_alloc_(LocalRoot local, addr left, addr *ret);
_g int sqrt_float_local_(LocalRoot local, addr left, addr *ret);
_g int sqrt_float_heap_(LocalRoot local, addr left, addr *ret);

#endif

