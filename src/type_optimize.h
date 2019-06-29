#ifndef __TYPE_OPTIMIZE_HEADER__
#define __TYPE_OPTIMIZE_HEADER__

#include "local.h"
#include "typedef.h"

_g int type_optimize_local(LocalRoot local, addr *ret, addr type);
_g int type_optimize_heap(LocalRoot local, addr *ret, addr type);
_g int type_optimized_p(addr type);
_g void get_type_optimized(addr *ret, addr type);

#endif

