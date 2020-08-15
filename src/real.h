#ifndef __REAL_HEADER__
#define __REAL_HEADER__

#include "build.h"
#include "typedef.h"

_g void build_real(void);
_g int floatp(addr pos);
_g int realp(addr pos);

_g int real_result_local_(LocalRoot local, addr pos, addr *ret);
_g int real_result_heap_(LocalRoot local, addr pos, addr *ret);
_g int real_throw_alloc_(LocalRoot local, addr pos, addr *ret);
_g int real_throw_local_(LocalRoot local, addr pos, addr *ret);
_g int real_throw_heap_(addr pos, addr *ret);
_g int real_copy_alloc_(LocalRoot local, addr pos, addr *ret);
_g int real_copy_local_(LocalRoot local, addr pos, addr *ret);
_g int real_copy_heap_(addr pos, addr *ret);

_g int cast_double_float_unsafe_(addr value, double_float *ret);

#endif

