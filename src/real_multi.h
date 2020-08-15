#ifndef __REAL_MULTI_HEADER__
#define __REAL_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

_g int multi_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_single_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_double_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_long_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_real_common_(LocalRoot local, addr left, addr right, addr *ret);

_g int multi_fixnum_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_bignum_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_ratio_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_single_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_double_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_long_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_real_local_(LocalRoot local, addr left, addr right, addr *ret);

_g int div_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_real_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_real_bignum_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_real_ratio_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_single_real_common_(addr left, addr right, addr *ret);
_g int div_real_single_common_(addr left, addr right, addr *ret);
_g int div_double_real_common_(addr left, addr right, addr *ret);
_g int div_real_double_common_(addr left, addr right, addr *ret);
_g int div_long_real_common_(addr left, addr right, addr *ret);
_g int div_real_long_common_(addr left, addr right, addr *ret);
_g int div_real_common_(LocalRoot local, addr left, addr right, addr *ret);

_g int div_fixnum_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_real_fixnum_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_bignum_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_real_bignum_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_ratio_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_real_ratio_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_single_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_real_single_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_double_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_real_double_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_long_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_real_long_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_real_local_(LocalRoot local, addr left, addr right, addr *ret);

#endif

