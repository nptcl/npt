#ifndef __REAL_FLOOR_HEADER__
#define __REAL_FLOOR_HEADER__

#include "local.h"
#include "typedef.h"

void floor1_common(LocalRoot local, addr *quot, addr *rem, addr left);
void ffloor1_common(LocalRoot local, addr *quot, addr *rem, addr left);
void floor_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
void ffloor_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
void mod_rational_common(LocalRoot local, addr left, addr right, addr *ret);

#endif

