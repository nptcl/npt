#ifndef __TYPE_OPTIMIZE_HEADER__
#define __TYPE_OPTIMIZE_HEADER__

#include "typedef.h"
#include "local.h"

int type_optimize_local(LocalRoot local, addr *ret, addr type);
int type_optimize_heap(LocalRoot local, addr *ret, addr type);
int type_optimized_p(addr type);
void get_type_optimized(addr *ret, addr type);

#endif

