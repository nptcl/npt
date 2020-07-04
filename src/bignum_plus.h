#ifndef __BIGNUM_PLUS_HEADER__
#define __BIGNUM_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

_g void plus_fv_bignum_local(LocalRoot local, addr left, fixnum value2, addr *ret);
_g void plus_fv_real_local(LocalRoot local, addr left, fixnum value2, addr *ret);
_g void plus_fv_real_common(addr left, fixnum value2, addr *ret);
_g void plus_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_ff_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_ff_real_common(addr left, addr right, addr *ret);
_g void plus_bv_bignum_local(LocalRoot local, addr left, fixnum right, addr *ret);
_g void plus_bv_real_local(LocalRoot local, addr left, fixnum right, addr *ret);
_g void plus_bv_real_common(LocalRoot local, addr left, fixnum right, addr *ret);
_g void plus_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_bf_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_bf_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define plus_fb_bignum_local(m,a,b,r) plus_bf_bignum_local((m),(b),(a),(r))
#define plus_fb_real_local(m,a,b,r) plus_bf_real_local((m),(b),(a),(r))
#define plus_fb_real_common(m,a,b,r) plus_bf_real_common((m),(b),(a),(r))
_g void plus_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_bb_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_bb_real_common(LocalRoot local, addr left, addr right, addr *ret);

_g void sigrev_bignum_inplace(addr pos);
_g void sigrev_fixnum_bignum_local(LocalRoot local, addr left, addr *ret);
_g void sigrev_fixnum_integer_alloc(LocalRoot local, addr left, addr *ret);
_g void sigrev_fixnum_integer_local(LocalRoot local, addr left, addr *ret);
_g void sigrev_fixnum_integer_common(addr left, addr *ret);
_g void sigrev_bignum_bignum_local(LocalRoot local, addr left, addr *ret);
_g void sigrev_bignum_integer_local(LocalRoot local, addr left, addr *ret);
_g void sigrev_bignum_integer_common(addr left, addr *ret);

_g void minus_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_ff_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_ff_real_common(addr left, addr right, addr *ret);
_g void minus_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_bf_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_bf_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_fb_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_fb_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_fb_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_bb_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_bb_real_common(LocalRoot local, addr left, addr right, addr *ret);

#endif

