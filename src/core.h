#ifndef __CORE_HEADER__
#define __CORE_HEADER__

#include "typedef.h"

#define savecore_execute_ _n(savecore_execute_)
#define save_core _n(save_core)
#define load_core _n(load_core)

_g int savecore_execute_(Execute ptr, addr path);
_g int save_core(Execute ptr);
_g int load_core(const unicode *name, size_t size);

#endif

