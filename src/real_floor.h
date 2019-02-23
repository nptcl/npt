#ifndef __REAL_FLOOR_HEADER__
#define __REAL_FLOOR_HEADER__

#include "local.h"
#include "typedef.h"

void floor_integer_common(LocalRoot local, addr *quot, addr *rem, addr left);
void ffloor_integer_common(LocalRoot local, addr *quot, addr *rem, addr left);
void floor_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
void ffloor_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);

#endif

