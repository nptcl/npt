#ifndef __RATIO_MULTI_HEADER__
#define __RATIO_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

_g void multi_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_rf_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_rf_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define multi_fr_ratio_local(m,a,b,r) multi_rf_ratio_local((m),(b),(a),(r))
#define multi_fr_real_local(m,a,b,r) multi_rf_real_local((m),(b),(a),(r))
#define multi_fr_real_common(m,a,b,r) multi_rf_real_common((m),(b),(a),(r))
_g void multi_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_rb_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_rb_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define multi_br_ratio_local(m,a,b,r) multi_rb_ratio_local((m),(b),(a),(r))
#define multi_br_real_local(m,a,b,r) multi_rb_real_local((m),(b),(a),(r))
#define multi_br_real_common(m,a,b,r) multi_rb_real_common((m),(b),(a),(r))
_g void multi_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_rr_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_rr_real_common(LocalRoot local, addr left, addr right, addr *ret);

_g void div_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rf_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rf_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_fr_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_fr_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_fr_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rb_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rb_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_br_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_br_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_br_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rr_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rr_real_common(LocalRoot local, addr left, addr right, addr *ret);

#if 0
_g void div_ff_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_fb_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bf_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bb_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
#endif
_g void div_ff_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_ff_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_fb_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_fb_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bf_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bf_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bb_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bb_real_common(LocalRoot local, addr left, addr right, addr *ret);

_g void inverse_fixnum_ratio_local(LocalRoot local, addr left, addr *ret);
_g void inverse_bignum_ratio_local(LocalRoot local, addr left, addr *ret);
_g void inverse_ratio_local(LocalRoot local, addr left, addr *ret);
_g void inverse_fixnum_common(addr left, addr *ret);
_g void inverse_bignum_common(addr left, addr *ret);
_g void inverse_ratio_common(LocalRoot local, addr left, addr *ret);

#endif

