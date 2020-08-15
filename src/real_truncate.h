#ifndef __REAL_TRUNCATE_HEADER__
#define __REAL_TRUNCATE_HEADER__

#include "local.h"
#include "typedef.h"

_g int truncate1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
_g int truncate2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g int truncate_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g int ftruncate1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
_g int ftruncate2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g int ftruncate_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g int rem_rational_common_(LocalRoot local, addr left, addr right, addr *ret);

#endif

