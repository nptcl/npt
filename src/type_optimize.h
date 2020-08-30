#ifndef __TYPE_OPTIMIZE_HEADER__
#define __TYPE_OPTIMIZE_HEADER__

#include "local.h"
#include "typedef.h"

#define type_optimize_local_ _n(type_optimize_local_)
#define type_optimize_heap_ _n(type_optimize_heap_)
#define type_optimized_p _n(type_optimized_p)
#define get_type_optimized _n(get_type_optimized)

_g int type_optimize_local_(LocalRoot local, addr type, addr *value, int *ret);
_g int type_optimize_heap_(LocalRoot local, addr type, addr *value, int *ret);
_g int type_optimized_p(addr type);
_g void get_type_optimized(addr *ret, addr type);

#endif

