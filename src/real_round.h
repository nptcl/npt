#ifndef __REAL_ROUND_HEADER__
#define __REAL_ROUND_HEADER__

#include "local.h"
#include "typedef.h"

void round_integer_common(LocalRoot local, addr *quot, addr *rem, addr left);
void fround_integer_common(LocalRoot local, addr *quot, addr *rem, addr left);
void round_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
void fround_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right);

#endif

