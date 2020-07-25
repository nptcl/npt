#ifndef __NUMBER_MULTI_HEADER__
#define __NUMBER_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

_g int multi_number_heap_(LocalRoot local, addr left, addr right, addr *ret);
_g int inverse_number_heap_(LocalRoot local, addr left, addr *ret);
_g int div_number_heap_(LocalRoot local, addr left, addr right, addr *ret);

#endif

