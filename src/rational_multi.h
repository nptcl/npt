#ifndef __RATIONAL_MULTI_HEADER__
#define __RATIONAL_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

_g int multi_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_single_rational_common_(addr left, addr right, addr *ret);
_g int multi_double_rational_common_(addr left, addr right, addr *ret);
_g int multi_long_rational_common_(addr left, addr right, addr *ret);
#define multi_rational_fixnum_common_(m,a,b,r) \
	multi_fixnum_rational_common_((m),(b),(a),(r))
#define multi_rational_bignum_common_(m,a,b,r) \
	multi_bignum_rational_common_((m),(b),(a),(r))
#define multi_rational_ratio_common_(m,a,b,r) \
	multi_ratio_rational_common_((m),(b),(a),(r))
#define multi_rational_single_common_(a,b,r) \
	multi_single_rational_common_((b),(a),(r))
#define multi_rational_double_common_(a,b,r) \
	multi_double_rational_common_((b),(a),(r))
#define multi_rational_long_common_(a,b,r) \
	multi_long_rational_common_((b),(a),(r))
_g int multi_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_fixnum_rational_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_bignum_rational_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_ratio_rational_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_rational_local_(LocalRoot local, addr left, addr right, addr *ret);

_g int div_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_rational_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_rational_bignum_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_rational_ratio_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_single_rational_common_(addr left, addr right, addr *ret);
_g int div_rational_single_common_(addr left, addr right, addr *ret);
_g int div_double_rational_common_(addr left, addr right, addr *ret);
_g int div_rational_double_common_(addr left, addr right, addr *ret);
_g int div_long_rational_common_(addr left, addr right, addr *ret);
_g int div_rational_long_common_(addr left, addr right, addr *ret);
_g int div_rational_local_(LocalRoot local, addr left, addr right, addr *ret);

_g int inverse_rational_common_(LocalRoot local, addr pos, addr *ret);

#endif

