#ifndef __CLOS_CACHE_HEADER__
#define __CLOS_CACHE_HEADER__

#include "typedef.h"

_g void hashindex_cache(addr key, size_t size, size_t *ret);
_g int cache_equal_function(addr left, addr right);

#endif

