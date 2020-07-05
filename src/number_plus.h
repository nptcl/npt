#ifndef __NUMBER_PLUS_HEADER__
#define __NUMBER_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

_g void oneplus_number_common(LocalRoot local, addr value, addr *ret);
_g void oneminus_number_common(LocalRoot local, addr value, addr *ret);
_g void sign_reverse_number_common(addr left, addr *ret);
_g void sign_reverse_number_local(LocalRoot local, addr left, addr *ret);

_g void plus_number_heap(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_number_heap(LocalRoot local, addr left, addr right, addr *ret);

#endif

