#ifndef __RATIONAL_HEADER__
#define __RATIONAL_HEADER__

#include "local.h"
#include "typedef.h"

_g int ratiop(addr pos);
_g int rationalp(addr pos);

_g int rational_result_local_(LocalRoot local, addr pos, addr *ret);
_g int rational_result_heap_(LocalRoot local, addr pos, addr *ret);
_g int rational_throw_alloc_(LocalRoot local, addr pos, addr *ret);
_g int rational_throw_local_(LocalRoot local, addr pos, addr *ret);
_g int rational_throw_heap_(addr pos, addr *ret);
_g int rational_copy_alloc_(LocalRoot local, addr pos, addr *ret);
_g int rational_copy_local_(LocalRoot local, addr pos, addr *ret);
_g int rational_copy_heap_(addr pos, addr *ret);

_g int single_float_rational_(addr pos, single_float *ret);
_g int double_float_rational_(addr pos, double_float *ret);
_g int long_float_rational_(addr pos, long_float *ret);

_g int numerator_common_(addr pos, addr *ret);
_g int denominator_common_(addr pos, addr *ret);

#endif

