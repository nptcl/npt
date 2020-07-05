#ifndef __RATIONAL_PLUS_HEADER__
#define __RATIONAL_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

_g void sign_reverse_rational_common(addr pos, addr *ret);
_g void sign_reverse_rational_local(LocalRoot local, addr pos, addr *ret);

_g void oneplus_rational_common(LocalRoot local, addr value, addr *ret);
_g void oneminus_rational_common(LocalRoot local, addr value, addr *ret);

_g void plus_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_single_rational_common(addr left, addr right, addr *ret);
_g void plus_double_rational_common(addr left, addr right, addr *ret);
_g void plus_long_rational_common(addr left, addr right, addr *ret);
_g void plus_rational_common(LocalRoot local, addr left, addr right, addr *ret);
#define plus_rational_fixnum_common(m,a,b,r) \
	plus_fixnum_rational_common((m),(b),(a),(r))
#define plus_rational_bignum_common(m,a,b,r) \
	plus_bignum_rational_common((m),(b),(a),(r))
#define plus_rational_ratio_common(m,a,b,r) \
	plus_ratio_rational_common((m),(b),(a),(r))
#define plus_rational_single_common(a,b,r) \
	plus_single_rational_common((b),(a),(r))
#define plus_rational_double_common(a,b,r) \
	plus_double_rational_common((b),(a),(r))
#define plus_rational_long_common(a,b,r) \
	plus_long_rational_common((b),(a),(r))
_g void plus_rational_local(LocalRoot local, addr left, addr right, addr *ret);

_g void minus_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rational_fixnum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rational_bignum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rational_ratio_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_single_rational_common(addr left, addr right, addr *ret);
_g void minus_rational_single_common(addr left, addr right, addr *ret);
_g void minus_double_rational_common(addr left, addr right, addr *ret);
_g void minus_rational_double_common(addr left, addr right, addr *ret);
_g void minus_long_rational_common(addr left, addr right, addr *ret);
_g void minus_rational_long_common(addr left, addr right, addr *ret);
_g void minus_rational_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rational_common(LocalRoot local, addr left, addr right, addr *ret);

#endif

