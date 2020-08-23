#ifndef __REAL_EQUAL_HEADER__
#define __REAL_EQUAL_HEADER__

#include "local.h"
#include "typedef.h"

_g int plusp_realp(addr pos, int *ret);
_g int plusp_real_(addr pos, int *ret);
_g int minusp_realp(addr pos, int *ret);
_g int minusp_real_(addr pos, int *ret);
_g int zerop_real_(addr pos, int *ret);
_g int equal_fixnum_real_(addr left, addr right, int *ret);
_g int equal_bignum_real_(addr left, addr right, int *ret);
_g int equal_ratio_real_(LocalRoot local, addr left, addr right, int *ret);
_g int equal_single_float_real_(LocalRoot local, addr left, addr right, int *ret);
_g int equal_double_float_real_(LocalRoot local, addr left, addr right, int *ret);
_g int equal_long_float_real_(LocalRoot local, addr left, addr right, int *ret);
_g int equal_real_(LocalRoot local, addr left, addr right, int *ret);
_g int not_equal_real_(LocalRoot local, addr left, addr right, int *ret);
_g int compare_ratio_real_(LocalRoot local, addr left, addr right, int *ret);
_g int compare_real_(LocalRoot local, addr left, addr right, int *ret);
_g int less_real_(LocalRoot local, addr left, addr right, int *ret);
_g int less_equal_real_(LocalRoot local, addr left, addr right, int *ret);
_g int greater_real_(LocalRoot local, addr left, addr right, int *ret);
_g int greater_equal_real_(LocalRoot local, addr left, addr right, int *ret);

_g int plusp_real_debug(addr pos);
_g int minusp_real_debug(addr pos);
_g int zerop_real_debug(addr pos);
_g int equal_fixnum_real_debug(addr left, addr right);
_g int equal_bignum_real_debug(addr left, addr right);
_g int equal_ratio_real_debug(LocalRoot local, addr left, addr right);
_g int equal_single_float_real_debug(LocalRoot local, addr left, addr right);
_g int equal_double_float_real_debug(LocalRoot local, addr left, addr right);
_g int equal_long_float_real_debug(LocalRoot local, addr left, addr right);
_g int equal_real_debug(LocalRoot local, addr left, addr right);
_g int less_real_debug(LocalRoot local, addr left, addr right);
_g int less_equal_real_debug(LocalRoot local, addr left, addr right);
_g int greater_real_debug(LocalRoot local, addr left, addr right);
_g int greater_equal_real_debug(LocalRoot local, addr left, addr right);

#endif

