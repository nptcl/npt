#ifndef __REAL_PLUS_HEADER__
#define __REAL_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

_g void sign_reverse_real_common(addr pos, addr *ret);
_g void sign_reverse_real_local(LocalRoot local, addr pos, addr *ret);

_g void oneplus_real_common(LocalRoot local, addr value, addr *ret);
_g void oneminus_real_common(LocalRoot local, addr value, addr *ret);

_g void plus_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_single_real_common(addr left, addr right, addr *ret);
_g void plus_double_real_common(addr left, addr right, addr *ret);
_g void plus_long_real_common(addr left, addr right, addr *ret);
_g void plus_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_real_local(LocalRoot local, addr left, addr right, addr *ret);

_g void minus_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_fixnum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_bignum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_ratio_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_single_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_single_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_double_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_double_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_long_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_long_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_local(LocalRoot local, addr left, addr right, addr *ret);

#endif

