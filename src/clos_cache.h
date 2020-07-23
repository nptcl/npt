#ifndef __CLOS_CACHE_HEADER__
#define __CLOS_CACHE_HEADER__

#include "typedef.h"

_g int hashindex_cache_(addr key, size_t size, size_t *ret);
_g int cache_equal_function_(addr left, addr right, int *ret);
_g int cache_equal_debug(addr left, addr right);

#endif

