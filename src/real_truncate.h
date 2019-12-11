#ifndef __REAL_TRUNCATE_HEADER__
#define __REAL_TRUNCATE_HEADER__

#include "local.h"
#include "typedef.h"

_g void truncate1_common(LocalRoot local, addr *quot, addr *rem, addr left);
_g void truncate2_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g void truncate_common(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g void ftruncate1_common(LocalRoot local, addr *quot, addr *rem, addr left);
_g void ftruncate2_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g void ftruncate_common(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g void rem_rational_common(LocalRoot local, addr left, addr right, addr *ret);

#endif

