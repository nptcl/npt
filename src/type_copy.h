#ifndef __TYPE_COPY_HEADER__
#define __TYPE_COPY_HEADER__

#include "define.h"
#include "local.h"

#define type_copy_alloc _n(type_copy_alloc)
#define type_copy_local _n(type_copy_local)
#define type_copy_heap _n(type_copy_heap)
#define type_throw_alloc _n(type_throw_alloc)
#define type_throw_local _n(type_throw_local)
#define type_throw_heap _n(type_throw_heap)
#define init_type_copy _n(init_type_copy)

_g void type_copy_alloc(LocalRoot local, addr *ret, addr type);
_g void type_copy_local(LocalRoot local, addr *ret, addr type);
_g void type_copy_heap(addr *ret, addr type);

_g void type_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void type_throw_local(LocalRoot local, addr pos, addr *ret);
_g void type_throw_heap(addr pos, addr *ret);

_g void init_type_copy(void);

#endif

