#ifndef __CMPL_PLUS_HEADER__
#define __CMPL_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

_g void oneplus_complex_heap(LocalRoot local, addr pos, addr *ret);
_g void oneminus_complex_heap(LocalRoot local, addr pos, addr *ret);

_g void plus_rational_complex_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_fc_number_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_bc_number_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_rc_number_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_sc_number_common(addr left, addr right, addr *ret);
_g void plus_dc_number_common(addr left, addr right, addr *ret);
_g void plus_lc_number_common(addr left, addr right, addr *ret);
_g void plus_cc_number_common(LocalRoot local, addr left, addr right, addr *ret);
#define plus_cf_number_common(m,a,b,r) plus_fc_number_common((m),(b),(a),(r))
#define plus_cb_number_common(m,a,b,r) plus_bc_number_common((m),(b),(a),(r))
#define plus_cr_number_common(m,a,b,r) plus_rc_number_common((m),(b),(a),(r))
#define plus_cs_number_common(a,b,r) plus_sc_number_common((b),(a),(r))
#define plus_cd_number_common(a,b,r) plus_dc_number_common((b),(a),(r))
#define plus_cl_number_common(a,b,r) plus_lc_number_common((b),(a),(r))

_g void minus_rational_complex_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_complex_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_fc_number_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_cf_number_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_bc_number_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_cb_number_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rc_number_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_cr_number_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_sc_number_common(addr left, addr right, addr *ret);
_g void minus_cs_number_common(addr left, addr right, addr *ret);
_g void minus_dc_number_common(addr left, addr right, addr *ret);
_g void minus_cd_number_common(addr left, addr right, addr *ret);
_g void minus_lc_number_common(addr left, addr right, addr *ret);
_g void minus_cl_number_common(addr left, addr right, addr *ret);
_g void minus_cc_number_common(LocalRoot local, addr left, addr right, addr *ret);

#endif

