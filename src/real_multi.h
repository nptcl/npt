#ifndef __REAL_MULTI_HEADER__
#define __REAL_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

#define multi_fixnum_real_common_ _n(multi_fixnum_real_common_)
#define multi_bignum_real_common_ _n(multi_bignum_real_common_)
#define multi_ratio_real_common_ _n(multi_ratio_real_common_)
#define multi_single_real_common_ _n(multi_single_real_common_)
#define multi_double_real_common_ _n(multi_double_real_common_)
#define multi_long_real_common_ _n(multi_long_real_common_)
#define multi_real_common_ _n(multi_real_common_)
#define multi_fixnum_real_local_ _n(multi_fixnum_real_local_)
#define multi_bignum_real_local_ _n(multi_bignum_real_local_)
#define multi_ratio_real_local_ _n(multi_ratio_real_local_)
#define multi_single_real_local_ _n(multi_single_real_local_)
#define multi_double_real_local_ _n(multi_double_real_local_)
#define multi_long_real_local_ _n(multi_long_real_local_)
#define multi_real_local_ _n(multi_real_local_)
#define div_fixnum_real_common_ _n(div_fixnum_real_common_)
#define div_real_fixnum_common_ _n(div_real_fixnum_common_)
#define div_bignum_real_common_ _n(div_bignum_real_common_)
#define div_real_bignum_common_ _n(div_real_bignum_common_)
#define div_ratio_real_common_ _n(div_ratio_real_common_)
#define div_real_ratio_common_ _n(div_real_ratio_common_)
#define div_single_real_common_ _n(div_single_real_common_)
#define div_real_single_common_ _n(div_real_single_common_)
#define div_double_real_common_ _n(div_double_real_common_)
#define div_real_double_common_ _n(div_real_double_common_)
#define div_long_real_common_ _n(div_long_real_common_)
#define div_real_long_common_ _n(div_real_long_common_)
#define div_real_common_ _n(div_real_common_)
#define div_fixnum_real_local_ _n(div_fixnum_real_local_)
#define div_real_fixnum_local_ _n(div_real_fixnum_local_)
#define div_bignum_real_local_ _n(div_bignum_real_local_)
#define div_real_bignum_local_ _n(div_real_bignum_local_)
#define div_ratio_real_local_ _n(div_ratio_real_local_)
#define div_real_ratio_local_ _n(div_real_ratio_local_)
#define div_single_real_local_ _n(div_single_real_local_)
#define div_real_single_local_ _n(div_real_single_local_)
#define div_double_real_local_ _n(div_double_real_local_)
#define div_real_double_local_ _n(div_real_double_local_)
#define div_long_real_local_ _n(div_long_real_local_)
#define div_real_long_local_ _n(div_real_long_local_)
#define div_real_local_ _n(div_real_local_)

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

