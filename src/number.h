#ifndef __NUMBER_HEADER__
#define __NUMBER_HEADER__

#include "build.h"
#include "local.h"
#include "real.h"

_g int numberp(addr pos);

_g int number_result_local_(LocalRoot local, addr pos, addr *ret);
_g int number_result_heap_(LocalRoot local, addr pos, addr *ret);
_g void number_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void number_throw_local(LocalRoot local, addr pos, addr *ret);
_g void number_throw_heap(addr pos, addr *ret);
_g void number_copy_alloc(LocalRoot local, addr pos, addr *ret);
_g void number_copy_local(LocalRoot local, addr pos, addr *ret);
_g void number_copy_heap(addr pos, addr *ret);

_g int abs_number_common_(addr left, addr *ret);
_g int signum_number_common_(addr pos, addr *ret);
_g int sqrt_number_common_(addr pos, addr *ret);

#endif

