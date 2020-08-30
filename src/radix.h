#ifndef __RADIX_HEADER__
#define __RADIX_HEADER__

#include "local.h"
#include "typedef.h"

#define english_integer_ _n(english_integer_)
#define english_unit_heap_ _n(english_unit_heap_)
#define english_unit_local_ _n(english_unit_local_)
#define roma_integer_ _n(roma_integer_)

_g int english_integer_(LocalRoot local, addr stream, addr pos, int cardinal);
_g int english_unit_heap_(LocalRoot local, addr *ret, addr pos, int cardinal);
_g int english_unit_local_(LocalRoot local, addr *ret, addr pos, int cardinal);
_g int roma_integer_(addr stream, fixnum value, int subp);

#endif

