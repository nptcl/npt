#ifndef __REAL_EQUAL_HEADER__
#define __REAL_EQUAL_HEADER__

#include "local.h"
#include "typedef.h"

_g int plusp_real(addr pos);
_g int minusp_real(addr pos);
_g int zerop_real(addr pos);

_g int equal_fixnum_real(addr left, addr right);
_g int equal_bignum_real(addr left, addr right);
_g int equal_ratio_real(LocalRoot local, addr left, addr right);
_g int equal_single_float_real(LocalRoot local, addr left, addr right);
_g int equal_double_float_real(LocalRoot local, addr left, addr right);
_g int equal_long_float_real(LocalRoot local, addr left, addr right);
_g int equal_real(LocalRoot local, addr left, addr right);
#define not_equal_real(a,b) (! equal_real((a), (b)))
_g int compare_ratio_real(LocalRoot local, addr left, addr right);
_g int compare_real(LocalRoot local, addr left, addr right);
#define less_real(m,a,b) (compare_real((m),(a), (b)) < 0)
#define less_equal_real(m,a,b) (compare_real((m),(a), (b)) <= 0)
#define greater_real(m,a,b) (compare_real((m),(a), (b)) > 0)
#define greater_equal_real(m,a,b) (compare_real((m),(a), (b)) >= 0)

_g int less_real_clang(LocalRoot local, addr left, addr right);
_g int less_equal_real_clang(LocalRoot local, addr left, addr right);
_g int greater_real_clang(LocalRoot local, addr left, addr right);
_g int greater_equal_real_clang(LocalRoot local, addr left, addr right);

#endif

