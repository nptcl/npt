#ifndef __REAL_FLOOR_HEADER__
#define __REAL_FLOOR_HEADER__

#include "local.h"
#include "typedef.h"

_g int floor1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
_g int floor2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g int floor_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g int ffloor1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
_g int ffloor2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g int ffloor_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g int mod_rational_common_(LocalRoot local, addr left, addr right, addr *ret);

#endif

