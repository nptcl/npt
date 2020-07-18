#ifndef __RADIX_HEADER__
#define __RADIX_HEADER__

#include "local.h"
#include "typedef.h"

_g int english_integer_(LocalRoot local, addr stream, addr pos, int cardinal);
_g void english_unit_heap(LocalRoot local, addr *ret, addr pos, int cardinal);
_g void english_unit_local(LocalRoot local, addr *ret, addr pos, int cardinal);
_g int roma_integer_(addr stream, fixnum value, int subp);

#endif

