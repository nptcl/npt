#ifndef __ARRAY_COPY_HEADER__
#define __ARRAY_COPY_HEADER__

#include "local.h"
#include "typedef.h"

_g int array_size_copy_(LocalRoot local, addr pos, addr array);
_g int array_copy_alloc_(LocalRoot local, addr *ret, addr array);
_g int array_copy_local_(LocalRoot local, addr *ret, addr array);
_g int array_copy_heap_(addr *ret, addr array);

#endif

