#ifndef __MATH_GCD_HEADER__
#define __MATH_GCD_HEADER__

#include "local.h"
#include "typedef.h"

#define gcd_number_ _n(gcd_number_)
#define lcm_number_ _n(lcm_number_)

int gcd_number_(LocalRoot local, addr args, addr *ret);
int lcm_number_(LocalRoot local, addr args, addr *ret);

#endif

