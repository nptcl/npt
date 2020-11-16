#ifndef __RATIO_MULTI_HEADER__
#define __RATIO_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

#define multi_rf_ratio_local _n(multi_rf_ratio_local)
#define multi_rf_real_local _n(multi_rf_real_local)
#define multi_rf_real_common _n(multi_rf_real_common)
#define multi_rb_ratio_local _n(multi_rb_ratio_local)
#define multi_rb_real_local _n(multi_rb_real_local)
#define multi_rb_real_common _n(multi_rb_real_common)
#define multi_rr_ratio_local _n(multi_rr_ratio_local)
#define multi_rr_real_local _n(multi_rr_real_local)
#define multi_rr_real_common _n(multi_rr_real_common)
#define div_rf_ratio_local_ _n(div_rf_ratio_local_)
#define div_rf_real_local_ _n(div_rf_real_local_)
#define div_rf_real_common_ _n(div_rf_real_common_)
#define div_fr_ratio_local_ _n(div_fr_ratio_local_)
#define div_fr_real_local_ _n(div_fr_real_local_)
#define div_fr_real_common_ _n(div_fr_real_common_)
#define div_rb_ratio_local_ _n(div_rb_ratio_local_)
#define div_rb_real_local_ _n(div_rb_real_local_)
#define div_rb_real_common_ _n(div_rb_real_common_)
#define div_br_ratio_local_ _n(div_br_ratio_local_)
#define div_br_real_local_ _n(div_br_real_local_)
#define div_br_real_common_ _n(div_br_real_common_)
#define div_rr_ratio_local_ _n(div_rr_ratio_local_)
#define div_rr_real_local_ _n(div_rr_real_local_)
#define div_rr_real_common_ _n(div_rr_real_common_)
#define div_ff_real_local_ _n(div_ff_real_local_)
#define div_ff_real_common_ _n(div_ff_real_common_)
#define div_fb_real_local_ _n(div_fb_real_local_)
#define div_fb_real_common_ _n(div_fb_real_common_)
#define div_bf_real_local_ _n(div_bf_real_local_)
#define div_bf_real_common_ _n(div_bf_real_common_)
#define div_bb_real_local_ _n(div_bb_real_local_)
#define div_bb_real_common_ _n(div_bb_real_common_)
#define inverse_fixnum_ratio_local_ _n(inverse_fixnum_ratio_local_)
#define inverse_bignum_ratio_local_ _n(inverse_bignum_ratio_local_)
#define inverse_ratio_local_ _n(inverse_ratio_local_)
#define inverse_fixnum_common_ _n(inverse_fixnum_common_)
#define inverse_bignum_common_ _n(inverse_bignum_common_)
#define inverse_ratio_common_ _n(inverse_ratio_common_)

void multi_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_rf_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_rf_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define multi_fr_ratio_local(m,a,b,r) multi_rf_ratio_local((m),(b),(a),(r))
#define multi_fr_real_local(m,a,b,r) multi_rf_real_local((m),(b),(a),(r))
#define multi_fr_real_common(m,a,b,r) multi_rf_real_common((m),(b),(a),(r))
void multi_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_rb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_rb_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define multi_br_ratio_local(m,a,b,r) multi_rb_ratio_local((m),(b),(a),(r))
#define multi_br_real_local(m,a,b,r) multi_rb_real_local((m),(b),(a),(r))
#define multi_br_real_common(m,a,b,r) multi_rb_real_common((m),(b),(a),(r))
void multi_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_rr_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_rr_real_common(LocalRoot local, addr left, addr right, addr *ret);

int div_rf_ratio_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_rf_real_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_rf_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_fr_ratio_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_fr_real_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_fr_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_rb_ratio_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_rb_real_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_rb_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_br_ratio_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_br_real_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_br_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_rr_ratio_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_rr_real_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_rr_real_common_(LocalRoot local, addr left, addr right, addr *ret);

int div_ff_real_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_ff_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_fb_real_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_fb_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_bf_real_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_bf_real_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_bb_real_local_(LocalRoot local, addr left, addr right, addr *ret);
int div_bb_real_common_(LocalRoot local, addr left, addr right, addr *ret);

int inverse_fixnum_ratio_local_(LocalRoot local, addr pos, addr *ret);
int inverse_bignum_ratio_local_(LocalRoot local, addr pos, addr *ret);
int inverse_ratio_local_(LocalRoot local, addr pos, addr *ret);
int inverse_fixnum_common_(addr left, addr *ret);
int inverse_bignum_common_(addr left, addr *ret);
int inverse_ratio_common_(LocalRoot local, addr left, addr *ret);

#endif

