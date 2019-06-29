#ifndef __REAL_ROUND_HEADER__
#define __REAL_ROUND_HEADER__

#include "local.h"
#include "typedef.h"

_g void round1_common(LocalRoot local, addr *quot, addr *rem, addr left);
_g void fround1_common(LocalRoot local, addr *quot, addr *rem, addr left);
_g void round_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g void fround_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);

#endif

