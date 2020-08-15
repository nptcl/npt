#ifndef __TYPE_OPTIMIZE_HEADER__
#define __TYPE_OPTIMIZE_HEADER__

#include "local.h"
#include "typedef.h"

_g int type_optimize_local_(LocalRoot local, addr type, addr *value, int *ret);
_g int type_optimize_heap_(LocalRoot local, addr type, addr *value, int *ret);
_g int type_optimized_p(addr type);
_g void get_type_optimized(addr *ret, addr type);

#endif

