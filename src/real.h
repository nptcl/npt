#ifndef __REAL_HEADER__
#define __REAL_HEADER__

#include "build.h"
#include "typedef.h"

_g void build_real(void);
_g int floatp(addr pos);
_g int realp(addr pos);

_g void real_result_local(LocalRoot local, addr pos, addr *ret);
_g void real_result_heap(LocalRoot local, addr pos, addr *ret);
_g void real_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void real_throw_local(LocalRoot local, addr pos, addr *ret);
_g void real_throw_heap(addr pos, addr *ret);
_g void real_copy_alloc(LocalRoot local, addr pos, addr *ret);
_g void real_copy_local(LocalRoot local, addr pos, addr *ret);
_g void real_copy_heap(addr pos, addr *ret);

_g double_float cast_double_float_unsafe(addr value);

#endif

