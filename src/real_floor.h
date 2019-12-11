#ifndef __REAL_FLOOR_HEADER__
#define __REAL_FLOOR_HEADER__

#include "local.h"
#include "typedef.h"

_g void floor1_common(LocalRoot local, addr *quot, addr *rem, addr left);
_g void floor2_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g void floor_common(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g void ffloor1_common(LocalRoot local, addr *quot, addr *rem, addr left);
_g void ffloor2_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g void ffloor_common(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g void mod_rational_common(LocalRoot local, addr left, addr right, addr *ret);

#endif

