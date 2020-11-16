#ifndef __RATIONAL_PLUS_HEADER__
#define __RATIONAL_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

#define sign_reverse_rational_common_ _n(sign_reverse_rational_common_)
#define sign_reverse_rational_local_ _n(sign_reverse_rational_local_)
#define oneplus_rational_common_ _n(oneplus_rational_common_)
#define oneminus_rational_common_ _n(oneminus_rational_common_)
#define plus_fixnum_rational_common_ _n(plus_fixnum_rational_common_)
#define plus_bignum_rational_common_ _n(plus_bignum_rational_common_)
#define plus_ratio_rational_common_ _n(plus_ratio_rational_common_)
#define plus_single_rational_common_ _n(plus_single_rational_common_)
#define plus_double_rational_common_ _n(plus_double_rational_common_)
#define plus_long_rational_common_ _n(plus_long_rational_common_)
#define plus_rational_common_ _n(plus_rational_common_)
#define plus_rational_local_ _n(plus_rational_local_)
#define minus_fixnum_rational_common_ _n(minus_fixnum_rational_common_)
#define minus_rational_fixnum_common_ _n(minus_rational_fixnum_common_)
#define minus_bignum_rational_common_ _n(minus_bignum_rational_common_)
#define minus_rational_bignum_common_ _n(minus_rational_bignum_common_)
#define minus_ratio_rational_common_ _n(minus_ratio_rational_common_)
#define minus_rational_ratio_common_ _n(minus_rational_ratio_common_)
#define minus_single_rational_common_ _n(minus_single_rational_common_)
#define minus_rational_single_common_ _n(minus_rational_single_common_)
#define minus_double_rational_common_ _n(minus_double_rational_common_)
#define minus_rational_double_common_ _n(minus_rational_double_common_)
#define minus_long_rational_common_ _n(minus_long_rational_common_)
#define minus_rational_long_common_ _n(minus_rational_long_common_)
#define minus_rational_local_ _n(minus_rational_local_)
#define minus_rational_common_ _n(minus_rational_common_)

int sign_reverse_rational_common_(addr pos, addr *ret);
int sign_reverse_rational_local_(LocalRoot local, addr pos, addr *ret);

int oneplus_rational_common_(LocalRoot local, addr value, addr *ret);
int oneminus_rational_common_(LocalRoot local, addr value, addr *ret);

int plus_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
int plus_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
int plus_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
int plus_single_rational_common_(addr left, addr right, addr *ret);
int plus_double_rational_common_(addr left, addr right, addr *ret);
int plus_long_rational_common_(addr left, addr right, addr *ret);
int plus_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
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
int plus_rational_local_(LocalRoot local, addr left, addr right, addr *ret);

int minus_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_rational_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_rational_bignum_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_rational_ratio_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_single_rational_common_(addr left, addr right, addr *ret);
int minus_rational_single_common_(addr left, addr right, addr *ret);
int minus_double_rational_common_(addr left, addr right, addr *ret);
int minus_rational_double_common_(addr left, addr right, addr *ret);
int minus_long_rational_common_(addr left, addr right, addr *ret);
int minus_rational_long_common_(addr left, addr right, addr *ret);
int minus_rational_local_(LocalRoot local, addr left, addr right, addr *ret);
int minus_rational_common_(LocalRoot local, addr left, addr right, addr *ret);

#endif

