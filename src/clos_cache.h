#ifndef __CLOS_CACHE_HEADER__
#define __CLOS_CACHE_HEADER__

#include "typedef.h"

void hashindex_cache(addr key, size_t size, size_t *ret);
int cache_equal_function(addr left, addr right);

#endif

