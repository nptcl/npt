#ifndef __SUBTYPEP_OPTIMIZE_HEADER__
#define __SUBTYPEP_OPTIMIZE_HEADER__

#include "local.h"
#include "typedef.h"

#define type_optimize_local_ _n(type_optimize_local_)
#define type_optimize_heap_ _n(type_optimize_heap_)
#define type_optimized_p _n(type_optimized_p)
#define get_type_optimized _n(get_type_optimized)
#define type_optimize_throw_heap_ _n(type_optimize_throw_heap_)

int type_optimize_local_(LocalRoot local, addr type, addr *value, int *ret);
int type_optimize_heap_(LocalRoot local, addr type, addr *value, int *ret);
int type_optimized_p(addr type);
void get_type_optimized(addr *ret, addr type);
int type_optimize_throw_heap_(LocalRoot local, addr type, addr *ret);

#endif

