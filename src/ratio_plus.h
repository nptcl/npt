#ifndef __RATIO_PLUS_HEADER__
#define __RATIO_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

#define sign_reverse_ratio_inplace _n(sign_reverse_ratio_inplace)
#define sign_reverse_ratio_local _n(sign_reverse_ratio_local)
#define sign_reverse_ratio_common _n(sign_reverse_ratio_common)
#define sigrev_fixnum_ratio_local _n(sigrev_fixnum_ratio_local)
#define sigrev_bignum_ratio_local _n(sigrev_bignum_ratio_local)
#define plus_rv_ratio_local _n(plus_rv_ratio_local)
#define plus_rv_real_local _n(plus_rv_real_local)
#define plus_rv_real_common _n(plus_rv_real_common)
#define plus_rf_ratio_local _n(plus_rf_ratio_local)
#define plus_rf_real_local _n(plus_rf_real_local)
#define plus_rf_real_common _n(plus_rf_real_common)
#define minus_rf_ratio_local _n(minus_rf_ratio_local)
#define minus_rf_real_local _n(minus_rf_real_local)
#define minus_rf_real_common _n(minus_rf_real_common)
#define minus_fr_ratio_local _n(minus_fr_ratio_local)
#define minus_fr_real_local _n(minus_fr_real_local)
#define minus_fr_real_common _n(minus_fr_real_common)
#define plus_rb_ratio_local _n(plus_rb_ratio_local)
#define plus_rb_real_local _n(plus_rb_real_local)
#define plus_rb_real_common _n(plus_rb_real_common)
#define minus_rb_ratio_local _n(minus_rb_ratio_local)
#define minus_rb_real_local _n(minus_rb_real_local)
#define minus_rb_real_common _n(minus_rb_real_common)
#define minus_br_ratio_local _n(minus_br_ratio_local)
#define minus_br_real_local _n(minus_br_real_local)
#define minus_br_real_common _n(minus_br_real_common)
#define plus_rr_ratio_local _n(plus_rr_ratio_local)
#define plus_rr_real_local _n(plus_rr_real_local)
#define plus_rr_real_common _n(plus_rr_real_common)
#define minus_rr_ratio_local _n(minus_rr_ratio_local)
#define minus_rr_real_local _n(minus_rr_real_local)
#define minus_rr_real_common _n(minus_rr_real_common)

_g void sign_reverse_ratio_inplace(addr left);
_g void sign_reverse_ratio_local(LocalRoot local, addr left, addr *ret);
_g void sign_reverse_ratio_common(addr left, addr *ret);
_g void sigrev_fixnum_ratio_local(LocalRoot local, addr pos, addr *ret);
_g void sigrev_bignum_ratio_local(LocalRoot local, addr pos, addr *ret);

_g void plus_rv_ratio_local(LocalRoot local, addr left, fixnum right, addr *ret);
_g void plus_rv_real_local(LocalRoot local, addr left, fixnum right, addr *ret);
_g void plus_rv_real_common(LocalRoot local, addr left, fixnum right, addr *ret);
_g void plus_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_rf_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_rf_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define plus_fr_ratio_local(m,a,b,r) plus_rf_ratio_local((m),(b),(a),(r))
#define plus_fr_real_local(m,a,b,r) plus_rf_real_local((m),(b),(a),(r))
#define plus_fr_real_common(m,a,b,r) plus_rf_real_common((m),(b),(a),(r))
_g void minus_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rf_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rf_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_fr_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_fr_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_fr_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_rb_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_rb_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define plus_br_ratio_local(m,a,b,r) plus_rb_ratio_local((m),(b),(a),(r))
#define plus_br_real_local(m,a,b,r) plus_rb_real_local((m),(b),(a),(r))
#define plus_br_real_common(m,a,b,r) plus_rb_real_common((m),(b),(a),(r))
_g void minus_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rb_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rb_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_br_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_br_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_br_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_rr_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_rr_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rr_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rr_real_common(LocalRoot local, addr left, addr right, addr *ret);

#endif

