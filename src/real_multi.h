#ifndef __REAL_MULTI_HEADER__
#define __REAL_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

_g void multi_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_single_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_double_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_long_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_real_common(LocalRoot local, addr left, addr right, addr *ret);

_g void multi_fixnum_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bignum_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_ratio_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_single_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_double_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_long_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_real_local(LocalRoot local, addr left, addr right, addr *ret);

_g void div_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_fixnum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_bignum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_ratio_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_single_real_common(addr left, addr right, addr *ret);
_g void div_real_single_common(addr left, addr right, addr *ret);
_g void div_double_real_common(addr left, addr right, addr *ret);
_g void div_real_double_common(addr left, addr right, addr *ret);
_g void div_long_real_common(addr left, addr right, addr *ret);
_g void div_real_long_common(addr left, addr right, addr *ret);
_g void div_real_common(LocalRoot local, addr left, addr right, addr *ret);

_g void div_fixnum_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_fixnum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bignum_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_ratio_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_single_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_single_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_double_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_double_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_long_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_long_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_local(LocalRoot local, addr left, addr right, addr *ret);

#endif

