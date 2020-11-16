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

int rationalp(addr pos);

int rational_result_local_(LocalRoot local, addr pos, addr *ret);
int rational_result_heap_(LocalRoot local, addr pos, addr *ret);
int rational_throw_alloc_(LocalRoot local, addr pos, addr *ret);
int rational_throw_local_(LocalRoot local, addr pos, addr *ret);
int rational_throw_heap_(addr pos, addr *ret);
int rational_copy_alloc_(LocalRoot local, addr pos, addr *ret);
int rational_copy_local_(LocalRoot local, addr pos, addr *ret);
int rational_copy_heap_(addr pos, addr *ret);

int single_float_rational_(addr pos, single_float *ret);
int double_float_rational_(addr pos, double_float *ret);
int long_float_rational_(addr pos, long_float *ret);

int numerator_common_(addr pos, addr *ret);
int denominator_common_(addr pos, addr *ret);

#endif

