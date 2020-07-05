#ifndef __RATIONAL_HEADER__
#define __RATIONAL_HEADER__

#include "local.h"
#include "typedef.h"

_g int ratiop(addr pos);
_g int rationalp(addr pos);

_g void rational_result_local(LocalRoot local, addr pos, addr *ret);
_g void rational_result_heap(LocalRoot local, addr pos, addr *ret);
_g void rational_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void rational_throw_local(LocalRoot local, addr pos, addr *ret);
_g void rational_throw_heap(addr pos, addr *ret);
_g void rational_copy_alloc(LocalRoot local, addr pos, addr *ret);
_g void rational_copy_local(LocalRoot local, addr pos, addr *ret);
_g void rational_copy_heap(addr pos, addr *ret);

_g single_float single_float_rational(addr pos);
_g double_float double_float_rational(addr pos);
_g long_float long_float_rational(addr pos);

_g void numerator_common(addr pos, addr *ret);
_g void denominator_common(addr pos, addr *ret);

#endif

