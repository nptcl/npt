#ifndef __RATIONAL_HEADER__
#define __RATIONAL_HEADER__

#include "local.h"
#include "typedef.h"

#define rationalp _n(rationalp)
#define rational_result_local_ _n(rational_result_local_)
#define rational_result_heap_ _n(rational_result_heap_)
#define rational_throw_alloc_ _n(rational_throw_alloc_)
#define rational_throw_local_ _n(rational_throw_local_)
#define rational_throw_heap_ _n(rational_throw_heap_)
#define rational_copy_alloc_ _n(rational_copy_alloc_)
#define rational_copy_local_ _n(rational_copy_local_)
#define rational_copy_heap_ _n(rational_copy_heap_)
#define single_float_rational_ _n(single_float_rational_)
#define double_float_rational_ _n(double_float_rational_)
#define long_float_rational_ _n(long_float_rational_)
#define numerator_common_ _n(numerator_common_)
#define denominator_common_ _n(denominator_common_)

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

