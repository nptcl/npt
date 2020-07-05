#ifndef __NUMBER_MULTI_HEADER__
#define __NUMBER_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

_g void multi_number_heap(LocalRoot local, addr left, addr right, addr *ret);
_g void inverse_number_heap(LocalRoot local, addr left, addr *ret);
_g void div_number_heap(LocalRoot local, addr left, addr right, addr *ret);

#endif

