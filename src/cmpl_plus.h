#ifndef __CMPL_PLUS_HEADER__
#define __CMPL_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

#define oneplus_complex_heap_ _n(oneplus_complex_heap_)
#define oneminus_complex_heap_ _n(oneminus_complex_heap_)
#define plus_rational_complex_common_ _n(plus_rational_complex_common_)
#define plus_fc_number_common_ _n(plus_fc_number_common_)
#define plus_bc_number_common_ _n(plus_bc_number_common_)
#define plus_rc_number_common_ _n(plus_rc_number_common_)
#define plus_sc_number_common_ _n(plus_sc_number_common_)
#define plus_dc_number_common_ _n(plus_dc_number_common_)
#define plus_lc_number_common_ _n(plus_lc_number_common_)
#define plus_cc_number_common_ _n(plus_cc_number_common_)
#define minus_rational_complex_common_ _n(minus_rational_complex_common_)
#define minus_complex_rational_common_ _n(minus_complex_rational_common_)
#define minus_fc_number_common_ _n(minus_fc_number_common_)
#define minus_cf_number_common_ _n(minus_cf_number_common_)
#define minus_bc_number_common_ _n(minus_bc_number_common_)
#define minus_cb_number_common_ _n(minus_cb_number_common_)
#define minus_rc_number_common_ _n(minus_rc_number_common_)
#define minus_cr_number_common_ _n(minus_cr_number_common_)
#define minus_sc_number_common_ _n(minus_sc_number_common_)
#define minus_cs_number_common_ _n(minus_cs_number_common_)
#define minus_dc_number_common_ _n(minus_dc_number_common_)
#define minus_cd_number_common_ _n(minus_cd_number_common_)
#define minus_lc_number_common_ _n(minus_lc_number_common_)
#define minus_cl_number_common_ _n(minus_cl_number_common_)
#define minus_cc_number_common_ _n(minus_cc_number_common_)

int oneplus_complex_heap_(LocalRoot local, addr pos, addr *ret);
int oneminus_complex_heap_(LocalRoot local, addr pos, addr *ret);

int plus_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret);
int plus_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int plus_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int plus_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int plus_sc_number_common_(addr left, addr right, addr *ret);
int plus_dc_number_common_(addr left, addr right, addr *ret);
int plus_lc_number_common_(addr left, addr right, addr *ret);
int plus_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
#define plus_cf_number_common_(m,a,b,r) plus_fc_number_common_((m),(b),(a),(r))
#define plus_cb_number_common_(m,a,b,r) plus_bc_number_common_((m),(b),(a),(r))
#define plus_cr_number_common_(m,a,b,r) plus_rc_number_common_((m),(b),(a),(r))
#define plus_cs_number_common_(a,b,r) plus_sc_number_common_((b),(a),(r))
#define plus_cd_number_common_(a,b,r) plus_dc_number_common_((b),(a),(r))
#define plus_cl_number_common_(a,b,r) plus_lc_number_common_((b),(a),(r))

int minus_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_complex_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_cf_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_cb_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_cr_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int minus_sc_number_common_(addr left, addr right, addr *ret);
int minus_cs_number_common_(addr left, addr right, addr *ret);
int minus_dc_number_common_(addr left, addr right, addr *ret);
int minus_cd_number_common_(addr left, addr right, addr *ret);
int minus_lc_number_common_(addr left, addr right, addr *ret);
int minus_cl_number_common_(addr left, addr right, addr *ret);
int minus_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret);

#endif

