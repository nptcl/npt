#ifndef __REAL_TRUNCATE_HEADER__
#define __REAL_TRUNCATE_HEADER__

#include "local.h"
#include "typedef.h"

#define truncate1_common_ _n(truncate1_common_)
#define truncate2_common_ _n(truncate2_common_)
#define truncate_common_ _n(truncate_common_)
#define ftruncate1_common_ _n(ftruncate1_common_)
#define ftruncate2_common_ _n(ftruncate2_common_)
#define ftruncate_common_ _n(ftruncate_common_)
#define rem_rational_common_ _n(rem_rational_common_)

_g int truncate1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
_g int truncate2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g int truncate_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g int ftruncate1_common_(LocalRoot local, addr *quot, addr *rem, addr left);
_g int ftruncate2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right);
_g int ftruncate_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2);
_g int rem_rational_common_(LocalRoot local, addr left, addr right, addr *ret);

#endif

