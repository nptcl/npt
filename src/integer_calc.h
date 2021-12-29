#ifndef __INTEGER_CALC_HEADER__
#define __INTEGER_CALC_HEADER__

#include "local.h"
#include "typedef.h"

#define oneplus_integer_common_ _n(oneplus_integer_common_)
#define oneminus_integer_common_ _n(oneminus_integer_common_)
#define plus_fi_bignum_local_ _n(plus_fi_bignum_local_)
#define plus_fi_real_local_ _n(plus_fi_real_local_)
#define plus_fi_real_common_ _n(plus_fi_real_common_)
#define plus_bi_bignum_local_ _n(plus_bi_bignum_local_)
#define plus_bi_real_local_ _n(plus_bi_real_local_)
#define plus_bi_real_common_ _n(plus_bi_real_common_)
#define plus_ii_bignum_local_ _n(plus_ii_bignum_local_)
#define plus_ii_real_local_ _n(plus_ii_real_local_)
#define plus_ii_real_common_ _n(plus_ii_real_common_)
#define minus_ii_real_common_ _n(minus_ii_real_common_)
#define multi_ii_real_common_ _n(multi_ii_real_common_)

int oneplus_integer_common_(LocalRoot local, addr value, addr *ret);
int oneminus_integer_common_(LocalRoot local, addr value, addr *ret);
int plus_fi_bignum_local_(LocalRoot local, addr left, addr right, addr *ret);
int plus_fi_real_local_(LocalRoot local, addr left, addr right, addr *ret);
int plus_fi_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int plus_bi_bignum_local_(LocalRoot local, addr left, addr right, addr *ret);
int plus_bi_real_local_(LocalRoot local, addr left, addr right, addr *ret);
int plus_bi_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int plus_ii_bignum_local_(LocalRoot local, addr left, addr right, addr *ret);
int plus_ii_real_local_(LocalRoot local, addr left, addr right, addr *ret);
int plus_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int multi_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret);

#endif

