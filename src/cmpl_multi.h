#ifndef __CMPL_MULTI_HEADER__
#define __CMPL_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

#define multi_rational_complex_common_ _n(multi_rational_complex_common_)
#define multi_fc_number_common_ _n(multi_fc_number_common_)
#define multi_bc_number_common_ _n(multi_bc_number_common_)
#define multi_rc_number_common_ _n(multi_rc_number_common_)
#define multi_sc_number_common_ _n(multi_sc_number_common_)
#define multi_dc_number_common_ _n(multi_dc_number_common_)
#define multi_lc_number_common_ _n(multi_lc_number_common_)
#define multi_cc_number_common_ _n(multi_cc_number_common_)
#define inverse_complex_common_ _n(inverse_complex_common_)
#define div_rational_complex_common_ _n(div_rational_complex_common_)
#define div_complex_rational_common_ _n(div_complex_rational_common_)
#define div_fc_number_common_ _n(div_fc_number_common_)
#define div_cf_number_common_ _n(div_cf_number_common_)
#define div_bc_number_common_ _n(div_bc_number_common_)
#define div_cb_number_common_ _n(div_cb_number_common_)
#define div_rc_number_common_ _n(div_rc_number_common_)
#define div_cr_number_common_ _n(div_cr_number_common_)
#define div_sc_number_common_ _n(div_sc_number_common_)
#define div_cs_number_common_ _n(div_cs_number_common_)
#define div_dc_number_common_ _n(div_dc_number_common_)
#define div_cd_number_common_ _n(div_cd_number_common_)
#define div_lc_number_common_ _n(div_lc_number_common_)
#define div_cl_number_common_ _n(div_cl_number_common_)
#define div_cc_number_common_ _n(div_cc_number_common_)

int multi_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret);
int multi_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int multi_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int multi_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int multi_sc_number_common_(addr left, addr right, addr *ret);
int multi_dc_number_common_(addr left, addr right, addr *ret);
int multi_lc_number_common_(addr left, addr right, addr *ret);
int multi_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
#define multi_cf_number_common_(m,a,b,r) multi_fc_number_common_((m),(b),(a),(r))
#define multi_cb_number_common_(m,a,b,r) multi_bc_number_common_((m),(b),(a),(r))
#define multi_cr_number_common_(m,a,b,r) multi_rc_number_common_((m),(b),(a),(r))
#define multi_cs_number_common_(a,b,r) multi_sc_number_common_((b),(a),(r))
#define multi_cd_number_common_(a,b,r) multi_dc_number_common_((b),(a),(r))
#define multi_cl_number_common_(a,b,r) multi_lc_number_common_((b),(a),(r))

int inverse_complex_common_(LocalRoot local, addr pos, addr *ret);

int div_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_complex_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_cf_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_cb_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_cr_number_common_(LocalRoot local, addr left, addr right, addr *ret);
int div_sc_number_common_(addr left, addr right, addr *ret);
int div_cs_number_common_(addr left, addr right, addr *ret);
int div_dc_number_common_(addr left, addr right, addr *ret);
int div_cd_number_common_(addr left, addr right, addr *ret);
int div_lc_number_common_(addr left, addr right, addr *ret);
int div_cl_number_common_(addr left, addr right, addr *ret);
int div_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret);

#endif

