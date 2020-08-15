#ifndef __RATIONAL_PLUS_HEADER__
#define __RATIONAL_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

_g int sign_reverse_rational_common_(addr pos, addr *ret);
_g int sign_reverse_rational_local_(LocalRoot local, addr pos, addr *ret);

_g int oneplus_rational_common_(LocalRoot local, addr value, addr *ret);
_g int oneminus_rational_common_(LocalRoot local, addr value, addr *ret);

_g int plus_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_single_rational_common_(addr left, addr right, addr *ret);
_g int plus_double_rational_common_(addr left, addr right, addr *ret);
_g int plus_long_rational_common_(addr left, addr right, addr *ret);
_g int plus_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
#define plus_rational_fixnum_common_(m,a,b,r) \
	plus_fixnum_rational_common_((m),(b),(a),(r))
#define plus_rational_bignum_common_(m,a,b,r) \
	plus_bignum_rational_common_((m),(b),(a),(r))
#define plus_rational_ratio_common_(m,a,b,r) \
	plus_ratio_rational_common_((m),(b),(a),(r))
#define plus_rational_single_common_(a,b,r) \
	plus_single_rational_common_((b),(a),(r))
#define plus_rational_double_common_(a,b,r) \
	plus_double_rational_common_((b),(a),(r))
#define plus_rational_long_common_(a,b,r) \
	plus_long_rational_common_((b),(a),(r))
_g int plus_rational_local_(LocalRoot local, addr left, addr right, addr *ret);

_g int minus_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_rational_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_rational_bignum_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_rational_ratio_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_single_rational_common_(addr left, addr right, addr *ret);
_g int minus_rational_single_common_(addr left, addr right, addr *ret);
_g int minus_double_rational_common_(addr left, addr right, addr *ret);
_g int minus_rational_double_common_(addr left, addr right, addr *ret);
_g int minus_long_rational_common_(addr left, addr right, addr *ret);
_g int minus_rational_long_common_(addr left, addr right, addr *ret);
_g int minus_rational_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_rational_common_(LocalRoot local, addr left, addr right, addr *ret);

#endif

