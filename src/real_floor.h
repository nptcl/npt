#ifndef __REAL_FLOOR_HEADER__
#define __REAL_FLOOR_HEADER__

#include "local.h"
#include "typedef.h"

#define floor1_common_ _n(floor1_common_)
#define floor2_common_ _n(floor2_common_)
#define floor_common_ _n(floor_common_)
#define ffloor1_common_ _n(ffloor1_common_)
#define ffloor2_common_ _n(ffloor2_common_)
#define ffloor_common_ _n(ffloor_common_)
#define mod_rational_common_ _n(mod_rational_common_)

int floor1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
int floor2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
int floor_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
int ffloor1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
int ffloor2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
int ffloor_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
int mod_rational_common_(LocalRoot local, addr left, addr right, addr *ret);

#endif

