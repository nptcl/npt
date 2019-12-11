#ifndef __REAL_CEILING_HEADER__
#define __REAL_CEILING_HEADER__

#include "local.h"
#include "typedef.h"

_g void ceiling1_common(LocalRoot local, addr *quot, addr *rem, addr left);
_g void ceiling2_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g void ceiling_common(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g void fceiling1_common(LocalRoot local, addr *quot, addr *rem, addr left);
_g void fceiling2_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g void fceiling_common(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);

#endif

