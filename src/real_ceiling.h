#ifndef __REAL_CEILING_HEADER__
#define __REAL_CEILING_HEADER__

#include "local.h"
#include "typedef.h"

#define ceiling1_common_ _n(ceiling1_common_)
#define ceiling2_common_ _n(ceiling2_common_)
#define ceiling_common_ _n(ceiling_common_)
#define fceiling1_common_ _n(fceiling1_common_)
#define fceiling2_common_ _n(fceiling2_common_)
#define fceiling_common_ _n(fceiling_common_)

_g int ceiling1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
_g int ceiling2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g int ceiling_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g int fceiling1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
_g int fceiling2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g int fceiling_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);

#endif

