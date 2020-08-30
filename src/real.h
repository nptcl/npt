#ifndef __REAL_HEADER__
#define __REAL_HEADER__

#include "build.h"
#include "typedef.h"

#define floatp _n(floatp)
#define realp _n(realp)
#define real_result_local_ _n(real_result_local_)
#define real_result_heap_ _n(real_result_heap_)
#define real_throw_alloc_ _n(real_throw_alloc_)
#define real_throw_local_ _n(real_throw_local_)
#define real_throw_heap_ _n(real_throw_heap_)
#define real_copy_alloc_ _n(real_copy_alloc_)
#define real_copy_local_ _n(real_copy_local_)
#define real_copy_heap_ _n(real_copy_heap_)
#define cast_double_float_unsafe_ _n(cast_double_float_unsafe_)
#define build_real _n(build_real)

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

_g void build_real(void);

#endif

