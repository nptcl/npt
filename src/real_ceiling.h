#ifndef __REAL_CEILING_HEADER__
#define __REAL_CEILING_HEADER__

#include "local.h"
#include "typedef.h"

void ceiling1_common(LocalRoot local, addr *quot, addr *rem, addr left);
void fceiling1_common(LocalRoot local, addr *quot, addr *rem, addr left);
void ceiling_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
void fceiling_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);

#endif

