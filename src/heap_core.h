#ifndef __HEAP_CORE_HEADER__
#define __HEAP_CORE_HEADER__

#include "file_type.h"
#include "typedef.h"

_g int save_heap(struct filememory *fm);
_g int load_heap(struct filememory *fm);
_g void init_heap_core(void);

#endif

