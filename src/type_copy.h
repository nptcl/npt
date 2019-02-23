#ifndef __HEADER_TYPE_COPY__
#define __HEADER_TYPE_COPY__

#include "local.h"

addr type_copy_allocr(LocalRoot local, addr type);
addr type_copy_localr(LocalRoot local, addr type);
addr type_copy_heapr(addr type);
void type_copy_alloc(LocalRoot local, addr *ret, addr type);
void type_copy_local(LocalRoot local, addr *ret, addr type);
void type_copy_heap(addr *ret, addr type);

void init_type_copy(void);

#endif

