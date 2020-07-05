#ifndef __RATIONAL_MULTI_HEADER__
#define __RATIONAL_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

_g void multi_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_single_rational_common(addr left, addr right, addr *ret);
_g void multi_double_rational_common(addr left, addr right, addr *ret);
_g void multi_long_rational_common(addr left, addr right, addr *ret);
#define multi_rational_fixnum_common(m,a,b,r) \
	multi_fixnum_rational_common((m),(b),(a),(r))
#define multi_rational_bignum_common(m,a,b,r) \
	multi_bignum_rational_common((m),(b),(a),(r))
#define multi_rational_ratio_common(m,a,b,r) \
	multi_ratio_rational_common((m),(b),(a),(r))
#define multi_rational_single_common(a,b,r) \
	multi_single_rational_common((b),(a),(r))
#define multi_rational_double_common(a,b,r) \
	multi_double_rational_common((b),(a),(r))
#define multi_rational_long_common(a,b,r) \
	multi_long_rational_common((b),(a),(r))
_g void multi_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_fixnum_rational_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bignum_rational_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_ratio_rational_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_rational_local(LocalRoot local, addr left, addr right, addr *ret);

_g void div_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rational_fixnum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rational_bignum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rational_ratio_common(LocalRoot local, addr left, addr right, addr *ret);

_g void div_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rational_fixnum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rational_bignum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rational_ratio_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_single_rational_common(addr left, addr right, addr *ret);
_g void div_rational_single_common(addr left, addr right, addr *ret);
_g void div_double_rational_common(addr left, addr right, addr *ret);
_g void div_rational_double_common(addr left, addr right, addr *ret);
_g void div_long_rational_common(addr left, addr right, addr *ret);
_g void div_rational_long_common(addr left, addr right, addr *ret);
_g void div_rational_local(LocalRoot local, addr left, addr right, addr *ret);

_g void inverse_rational_common(LocalRoot local, addr pos, addr *ret);

#endif

