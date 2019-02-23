#ifndef __CORE_HEADER__
#define __CORE_HEADER__

#include "typedef.h"

void savecore_execute(addr path);
int make_core(void);
int load_core(const void *name, size_t size);

#endif

