#ifndef __REAL_PLUS_HEADER__
#define __REAL_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

_g int sign_reverse_real_common_(addr pos, addr *ret);
_g int sign_reverse_real_local_(LocalRoot local, addr pos, addr *ret);

_g int oneplus_real_common_(LocalRoot local, addr value, addr *ret);
_g int oneminus_real_common_(LocalRoot local, addr value, addr *ret);

_g int plus_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_single_real_common_(addr left, addr right, addr *ret);
_g int plus_double_real_common_(addr left, addr right, addr *ret);
_g int plus_long_real_common_(addr left, addr right, addr *ret);
_g int plus_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_real_local_(LocalRoot local, addr left, addr right, addr *ret);

_g int minus_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_real_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_real_bignum_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_real_ratio_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_single_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_real_single_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_double_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_real_double_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_long_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_real_long_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_real_local_(LocalRoot local, addr left, addr right, addr *ret);

#endif

