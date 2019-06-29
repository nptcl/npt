#ifndef __REAL_CEILING_HEADER__
#define __REAL_CEILING_HEADER__

#include "local.h"
#include "typedef.h"

_g void ceiling1_common(LocalRoot local, addr *quot, addr *rem, addr left);
_g void fceiling1_common(LocalRoot local, addr *quot, addr *rem, addr left);
_g void ceiling_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g void fceiling_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);

#endif

