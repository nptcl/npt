#ifndef __REAL_ROUND_HEADER__
#define __REAL_ROUND_HEADER__

#include "local.h"
#include "typedef.h"

_g int round1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
_g int round2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g int round_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g int fround1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
_g int fround2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g int fround_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);

#endif

