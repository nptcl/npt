#ifndef __TYPE_COPY_HEADER__
#define __TYPE_COPY_HEADER__

#include "local.h"

addr type_copy_allocr(LocalRoot local, addr type);
addr type_copy_localr(LocalRoot local, addr type);
addr type_copy_heapr(addr type);
void type_copy_alloc(LocalRoot local, addr *ret, addr type);
void type_copy_local(LocalRoot local, addr *ret, addr type);
void type_copy_heap(addr *ret, addr type);

void type_throw_alloc(LocalRoot local, addr pos, addr *ret);
void type_throw_local(LocalRoot local, addr pos, addr *ret);
void type_throw_heap(addr pos, addr *ret);

void init_type_copy(void);

#endif

