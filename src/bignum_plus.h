#ifndef __BIGNUM_PLUS_HEADER__
#define __BIGNUM_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

#define plus_fv_bignum_local _n(plus_fv_bignum_local)
#define plus_fv_real_local _n(plus_fv_real_local)
#define plus_fv_real_common _n(plus_fv_real_common)
#define plus_ff_bignum_local _n(plus_ff_bignum_local)
#define plus_ff_real_local _n(plus_ff_real_local)
#define plus_ff_real_common _n(plus_ff_real_common)
#define plus_bv_bignum_local _n(plus_bv_bignum_local)
#define plus_bv_real_local _n(plus_bv_real_local)
#define plus_bv_real_common _n(plus_bv_real_common)
#define plus_bf_bignum_local _n(plus_bf_bignum_local)
#define plus_bf_real_local _n(plus_bf_real_local)
#define plus_bf_real_common _n(plus_bf_real_common)
#define plus_bb_bignum_local _n(plus_bb_bignum_local)
#define plus_bb_real_local _n(plus_bb_real_local)
#define plus_bb_real_common _n(plus_bb_real_common)
#define sigrev_bignum_inplace _n(sigrev_bignum_inplace)
#define sigrev_fixnum_bignum_local _n(sigrev_fixnum_bignum_local)
#define sigrev_fixnum_integer_alloc _n(sigrev_fixnum_integer_alloc)
#define sigrev_fixnum_integer_local _n(sigrev_fixnum_integer_local)
#define sigrev_fixnum_integer_common _n(sigrev_fixnum_integer_common)
#define sigrev_bignum_bignum_local _n(sigrev_bignum_bignum_local)
#define sigrev_bignum_integer_local _n(sigrev_bignum_integer_local)
#define sigrev_bignum_integer_common _n(sigrev_bignum_integer_common)
#define minus_ff_bignum_local _n(minus_ff_bignum_local)
#define minus_ff_real_local _n(minus_ff_real_local)
#define minus_ff_real_common _n(minus_ff_real_common)
#define minus_bf_bignum_local _n(minus_bf_bignum_local)
#define minus_bf_real_local _n(minus_bf_real_local)
#define minus_bf_real_common _n(minus_bf_real_common)
#define minus_fb_bignum_local _n(minus_fb_bignum_local)
#define minus_fb_real_local _n(minus_fb_real_local)
#define minus_fb_real_common _n(minus_fb_real_common)
#define minus_bb_bignum_local _n(minus_bb_bignum_local)
#define minus_bb_real_local _n(minus_bb_real_local)
#define minus_bb_real_common _n(minus_bb_real_common)

void plus_fv_bignum_local(LocalRoot local, addr left, fixnum value2, addr *ret);
void plus_fv_real_local(LocalRoot local, addr left, fixnum value2, addr *ret);
void plus_fv_real_common(addr left, fixnum value2, addr *ret);
void plus_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_ff_real_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_ff_real_common(addr left, addr right, addr *ret);
void plus_bv_bignum_local(LocalRoot local, addr left, fixnum right, addr *ret);
void plus_bv_real_local(LocalRoot local, addr left, fixnum right, addr *ret);
void plus_bv_real_common(LocalRoot local, addr left, fixnum right, addr *ret);
void plus_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_bf_real_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_bf_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define plus_fb_bignum_local(m,a,b,r) plus_bf_bignum_local((m),(b),(a),(r))
#define plus_fb_real_local(m,a,b,r) plus_bf_real_local((m),(b),(a),(r))
#define plus_fb_real_common(m,a,b,r) plus_bf_real_common((m),(b),(a),(r))
void plus_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_bb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_bb_real_common(LocalRoot local, addr left, addr right, addr *ret);

void sigrev_bignum_inplace(addr pos);
void sigrev_fixnum_bignum_local(LocalRoot local, addr left, addr *ret);
void sigrev_fixnum_integer_alloc(LocalRoot local, addr left, addr *ret);
void sigrev_fixnum_integer_local(LocalRoot local, addr left, addr *ret);
void sigrev_fixnum_integer_common(addr left, addr *ret);
void sigrev_bignum_bignum_local(LocalRoot local, addr left, addr *ret);
void sigrev_bignum_integer_local(LocalRoot local, addr left, addr *ret);
void sigrev_bignum_integer_common(addr left, addr *ret);

void minus_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_ff_real_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_ff_real_common(addr left, addr right, addr *ret);
void minus_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_bf_real_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_bf_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_fb_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_fb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_fb_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_bb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_bb_real_common(LocalRoot local, addr left, addr right, addr *ret);

#endif

