#ifndef __REAL_ROUND_HEADER__
#define __REAL_ROUND_HEADER__

#include "local.h"
#include "typedef.h"

#define round1_common_ _n(round1_common_)
#define round2_common_ _n(round2_common_)
#define round_common_ _n(round_common_)
#define fround1_common_ _n(fround1_common_)
#define fround2_common_ _n(fround2_common_)
#define fround_common_ _n(fround_common_)

int round1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
int round2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
int round_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
int fround1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
int fround2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
int fround_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);

#endif

