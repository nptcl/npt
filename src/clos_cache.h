#ifndef __CLOS_CACHE_HEADER__
#define __CLOS_CACHE_HEADER__

#include "typedef.h"

#define hashindex_cache_ _n(hashindex_cache_)
#define cache_equal_function_ _n(cache_equal_function_)
#define cache_equal_debug _n(cache_equal_debug)

int hashindex_cache_(addr key, size_t size, size_t *ret);
int cache_equal_function_(addr left, addr right, int *ret);
int cache_equal_debug(addr left, addr right);

#endif

