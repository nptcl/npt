#ifndef __RATIONAL_EQUAL_HEADER__
#define __RATIONAL_EQUAL_HEADER__

#include "local.h"
#include "typedef.h"

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

