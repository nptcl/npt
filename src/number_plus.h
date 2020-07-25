#ifndef __NUMBER_PLUS_HEADER__
#define __NUMBER_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

_g int oneplus_number_common_(LocalRoot local, addr value, addr *ret);
_g int oneminus_number_common_(LocalRoot local, addr value, addr *ret);
_g int sign_reverse_number_common_(addr left, addr *ret);

_g int plus_number_heap_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_number_heap_(LocalRoot local, addr left, addr right, addr *ret);

#endif

