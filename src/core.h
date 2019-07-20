#ifndef __CORE_HEADER__
#define __CORE_HEADER__

#include "typedef.h"

_g void savecore_execute(addr path);
_g int save_core(void);
_g int load_core(const unicode *name, size_t size);

#endif

