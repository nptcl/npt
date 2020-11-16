#ifndef __MATH_POWER_HEADER__
#define __MATH_POWER_HEADER__

#include "local.h"
#include "typedef.h"

#define expt_common_ _n(expt_common_)
int expt_common_(LocalRoot local, addr *ret, addr base, addr power);

#endif

