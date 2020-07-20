#ifndef __TYPE_COPY_HEADER__
#define __TYPE_COPY_HEADER__

#include "define.h"
#include "local.h"

_g void type_copy_alloc(LocalRoot local, addr *ret, addr type);
_g void type_copy_local(LocalRoot local, addr *ret, addr type);
_g void type_copy_heap(addr *ret, addr type);

_g void type_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void type_throw_local(LocalRoot local, addr pos, addr *ret);
_g void type_throw_heap(addr pos, addr *ret);

_g void init_type_copy(void);

#endif

