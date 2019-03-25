#ifndef __ARRAY_COPY_HEADER__
#define __ARRAY_COPY_HEADER__

#include "local.h"
#include "typedef.h"

void array_size_copy(LocalRoot local, addr pos, addr array);
void array_copy_alloc(LocalRoot local, addr *ret, addr array);
void array_copy_local(LocalRoot local, addr *ret, addr array);
void array_copy_heap(addr *ret, addr array);

#endif

