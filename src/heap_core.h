#ifndef __HEAP_CORE_HEADER__
#define __HEAP_CORE_HEADER__

#include "file_type.h"
#include "typedef.h"

#define save_heap _n(save_heap)
#define load_heap _n(load_heap)
#define init_heap_core _n(init_heap_core)

_g int save_heap(struct filememory *fm);
_g int load_heap(struct filememory *fm);
_g void init_heap_core(void);

#endif

