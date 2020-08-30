#ifndef __NUMBER_HEADER__
#define __NUMBER_HEADER__

#include "build.h"
#include "local.h"
#include "real.h"

#define numberp _n(numberp)
#define number_result_local_ _n(number_result_local_)
#define number_result_heap_ _n(number_result_heap_)
#define number_throw_alloc_ _n(number_throw_alloc_)
#define number_throw_local_ _n(number_throw_local_)
#define number_throw_heap_ _n(number_throw_heap_)
#define number_copy_alloc_ _n(number_copy_alloc_)
#define number_copy_local_ _n(number_copy_local_)
#define number_copy_heap_ _n(number_copy_heap_)
#define abs_number_common_ _n(abs_number_common_)
#define signum_number_common_ _n(signum_number_common_)
#define sqrt_number_common_ _n(sqrt_number_common_)

_g int numberp(addr pos);

_g int number_result_local_(LocalRoot local, addr pos, addr *ret);
_g int number_result_heap_(LocalRoot local, addr pos, addr *ret);
_g int number_throw_alloc_(LocalRoot local, addr pos, addr *ret);
_g int number_throw_local_(LocalRoot local, addr pos, addr *ret);
_g int number_throw_heap_(addr pos, addr *ret);
_g int number_copy_alloc_(LocalRoot local, addr pos, addr *ret);
_g int number_copy_local_(LocalRoot local, addr pos, addr *ret);
_g int number_copy_heap_(addr pos, addr *ret);

_g int abs_number_common_(addr left, addr *ret);
_g int signum_number_common_(addr pos, addr *ret);
_g int sqrt_number_common_(addr pos, addr *ret);

#endif

