#ifndef __ARRAY_COPY_HEADER__
#define __ARRAY_COPY_HEADER__

#include "local.h"
#include "typedef.h"

#define array_size_copy_ _n(array_size_copy_)
#define array_copy_alloc_ _n(array_copy_alloc_)
#define array_copy_local_ _n(array_copy_local_)
#define array_copy_heap_ _n(array_copy_heap_)

int array_size_copy_(LocalRoot local, addr pos, addr array);
int array_copy_alloc_(LocalRoot local, addr *ret, addr array);
int array_copy_local_(LocalRoot local, addr *ret, addr array);
int array_copy_heap_(addr *ret, addr array);

#endif

