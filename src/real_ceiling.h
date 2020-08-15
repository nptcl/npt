#ifndef __REAL_CEILING_HEADER__
#define __REAL_CEILING_HEADER__

#include "local.h"
#include "typedef.h"

_g int ceiling1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
_g int ceiling2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g int ceiling_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g int fceiling1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
_g int fceiling2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g int fceiling_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);

#endif

