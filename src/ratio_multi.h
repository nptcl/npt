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

_g int div_rf_ratio_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_rf_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_rf_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_fr_ratio_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_fr_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_fr_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_rb_ratio_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_rb_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_rb_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_br_ratio_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_br_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_br_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_rr_ratio_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_rr_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_rr_real_common_(LocalRoot local, addr left, addr right, addr *ret);

_g int div_ff_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_ff_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_fb_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_fb_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_bf_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_bf_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_bb_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_bb_real_common_(LocalRoot local, addr left, addr right, addr *ret);

_g int inverse_fixnum_ratio_local_(LocalRoot local, addr pos, addr *ret);
_g int inverse_bignum_ratio_local_(LocalRoot local, addr pos, addr *ret);
_g int inverse_ratio_local_(LocalRoot local, addr pos, addr *ret);
_g int inverse_fixnum_common_(addr left, addr *ret);
_g int inverse_bignum_common_(addr left, addr *ret);
_g int inverse_ratio_common_(LocalRoot local, addr left, addr *ret);

#endif

