#ifndef __RATIONAL_EQUAL_HEADER__
#define __RATIONAL_EQUAL_HEADER__

#include "local.h"
#include "typedef.h"

#define plusp_rational_ _n(plusp_rational_)
#define minusp_rational_ _n(minusp_rational_)
#define zerop_rational_ _n(zerop_rational_)
#define equal_rational_ _n(equal_rational_)
#define not_equal_rational_ _n(not_equal_rational_)
#define compare_rational_ _n(compare_rational_)
#define less_rational_ _n(less_rational_)
#define less_equal_rational_ _n(less_equal_rational_)
#define greater_rational_ _n(greater_rational_)
#define greater_equal_rational_ _n(greater_equal_rational_)
#define less_rational_debug _n(less_rational_debug)
#define less_equal_rational_debug _n(less_equal_rational_debug)

_g int plusp_rational_(addr pos, int *ret);
_g int minusp_rational_(addr pos, int *ret);
_g int zerop_rational_(addr pos, int *ret);
_g int equal_rational_(addr left, addr right, int *ret);
_g int not_equal_rational_(addr left, addr right, int *ret);
_g int compare_rational_(LocalRoot local, addr left, addr right, int *ret);
_g int less_rational_(LocalRoot local, addr left, addr right, int *ret);
_g int less_equal_rational_(LocalRoot local, addr left, addr right, int *ret);
_g int greater_rational_(LocalRoot local, addr left, addr right, int *ret);
_g int greater_equal_rational_(LocalRoot local, addr left, addr right, int *ret);

_g int less_rational_debug(LocalRoot local, addr left, addr right);
_g int less_equal_rational_debug(LocalRoot local, addr left, addr right);

#endif

