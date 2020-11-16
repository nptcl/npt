#ifndef __REAL_EQUAL_HEADER__
#define __REAL_EQUAL_HEADER__

#include "local.h"
#include "typedef.h"

#define plusp_realp _n(plusp_realp)
#define plusp_real_ _n(plusp_real_)
#define minusp_realp _n(minusp_realp)
#define minusp_real_ _n(minusp_real_)
#define zerop_real_ _n(zerop_real_)
#define equal_fixnum_real_ _n(equal_fixnum_real_)
#define equal_bignum_real_ _n(equal_bignum_real_)
#define equal_ratio_real_ _n(equal_ratio_real_)
#define equal_single_float_real_ _n(equal_single_float_real_)
#define equal_double_float_real_ _n(equal_double_float_real_)
#define equal_long_float_real_ _n(equal_long_float_real_)
#define equal_real_ _n(equal_real_)
#define not_equal_real_ _n(not_equal_real_)
#define compare_ratio_real_ _n(compare_ratio_real_)
#define compare_real_ _n(compare_real_)
#define less_real_ _n(less_real_)
#define less_equal_real_ _n(less_equal_real_)
#define greater_real_ _n(greater_real_)
#define greater_equal_real_ _n(greater_equal_real_)
#define plusp_real_debug _n(plusp_real_debug)
#define minusp_real_debug _n(minusp_real_debug)
#define zerop_real_debug _n(zerop_real_debug)
#define equal_fixnum_real_debug _n(equal_fixnum_real_debug)
#define equal_bignum_real_debug _n(equal_bignum_real_debug)
#define equal_ratio_real_debug _n(equal_ratio_real_debug)
#define equal_single_float_real_debug _n(equal_single_float_real_debug)
#define equal_double_float_real_debug _n(equal_double_float_real_debug)
#define equal_long_float_real_debug _n(equal_long_float_real_debug)
#define equal_real_debug _n(equal_real_debug)
#define less_real_debug _n(less_real_debug)
#define less_equal_real_debug _n(less_equal_real_debug)
#define greater_real_debug _n(greater_real_debug)
#define greater_equal_real_debug _n(greater_equal_real_debug)

int plusp_realp(addr pos, int *ret);
int plusp_real_(addr pos, int *ret);
int minusp_realp(addr pos, int *ret);
int minusp_real_(addr pos, int *ret);
int zerop_real_(addr pos, int *ret);
int equal_fixnum_real_(addr left, addr right, int *ret);
int equal_bignum_real_(addr left, addr right, int *ret);
int equal_ratio_real_(LocalRoot local, addr left, addr right, int *ret);
int equal_single_float_real_(LocalRoot local, addr left, addr right, int *ret);
int equal_double_float_real_(LocalRoot local, addr left, addr right, int *ret);
int equal_long_float_real_(LocalRoot local, addr left, addr right, int *ret);
int equal_real_(LocalRoot local, addr left, addr right, int *ret);
int not_equal_real_(LocalRoot local, addr left, addr right, int *ret);
int compare_ratio_real_(LocalRoot local, addr left, addr right, int *ret);
int compare_real_(LocalRoot local, addr left, addr right, int *ret);
int less_real_(LocalRoot local, addr left, addr right, int *ret);
int less_equal_real_(LocalRoot local, addr left, addr right, int *ret);
int greater_real_(LocalRoot local, addr left, addr right, int *ret);
int greater_equal_real_(LocalRoot local, addr left, addr right, int *ret);

int plusp_real_debug(addr pos);
int minusp_real_debug(addr pos);
int zerop_real_debug(addr pos);
int equal_fixnum_real_debug(addr left, addr right);
int equal_bignum_real_debug(addr left, addr right);
int equal_ratio_real_debug(LocalRoot local, addr left, addr right);
int equal_single_float_real_debug(LocalRoot local, addr left, addr right);
int equal_double_float_real_debug(LocalRoot local, addr left, addr right);
int equal_long_float_real_debug(LocalRoot local, addr left, addr right);
int equal_real_debug(LocalRoot local, addr left, addr right);
int less_real_debug(LocalRoot local, addr left, addr right);
int less_equal_real_debug(LocalRoot local, addr left, addr right);
int greater_real_debug(LocalRoot local, addr left, addr right);
int greater_equal_real_debug(LocalRoot local, addr left, addr right);

#endif

