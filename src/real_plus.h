#ifndef __REAL_PLUS_HEADER__
#define __REAL_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

#define sign_reverse_real_common_ _n(sign_reverse_real_common_)
#define sign_reverse_real_local_ _n(sign_reverse_real_local_)
#define oneplus_real_common_ _n(oneplus_real_common_)
#define oneminus_real_common_ _n(oneminus_real_common_)
#define plus_fixnum_real_common_ _n(plus_fixnum_real_common_)
#define plus_bignum_real_common_ _n(plus_bignum_real_common_)
#define plus_ratio_real_common_ _n(plus_ratio_real_common_)
#define plus_single_real_common_ _n(plus_single_real_common_)
#define plus_double_real_common_ _n(plus_double_real_common_)
#define plus_long_real_common_ _n(plus_long_real_common_)
#define plus_real_common_ _n(plus_real_common_)
#define plus_real_local_ _n(plus_real_local_)
#define minus_fixnum_real_common_ _n(minus_fixnum_real_common_)
#define minus_real_fixnum_common_ _n(minus_real_fixnum_common_)
#define minus_bignum_real_common_ _n(minus_bignum_real_common_)
#define minus_real_bignum_common_ _n(minus_real_bignum_common_)
#define minus_ratio_real_common_ _n(minus_ratio_real_common_)
#define minus_real_ratio_common_ _n(minus_real_ratio_common_)
#define minus_single_real_common_ _n(minus_single_real_common_)
#define minus_real_single_common_ _n(minus_real_single_common_)
#define minus_double_real_common_ _n(minus_double_real_common_)
#define minus_real_double_common_ _n(minus_real_double_common_)
#define minus_long_real_common_ _n(minus_long_real_common_)
#define minus_real_long_common_ _n(minus_real_long_common_)
#define minus_real_common_ _n(minus_real_common_)
#define minus_real_local_ _n(minus_real_local_)

int sign_reverse_real_common_(addr pos, addr *ret);
int sign_reverse_real_local_(LocalRoot local, addr pos, addr *ret);

int oneplus_real_common_(LocalRoot local, addr value, addr *ret);
int oneminus_real_common_(LocalRoot local, addr value, addr *ret);

int plus_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int plus_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int plus_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int plus_single_real_common_(addr left, addr right, addr *ret);
int plus_double_real_common_(addr left, addr right, addr *ret);
int plus_long_real_common_(addr left, addr right, addr *ret);
int plus_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int plus_real_local_(LocalRoot local, addr left, addr right, addr *ret);

int minus_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_real_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_real_bignum_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_real_ratio_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_single_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_real_single_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_double_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_real_double_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_long_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_real_long_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_real_local_(LocalRoot local, addr left, addr right, addr *ret);

#endif

