#ifndef __CMPL_MULTI_HEADER__
#define __CMPL_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

_g int multi_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_sc_number_common_(addr left, addr right, addr *ret);
_g int multi_dc_number_common_(addr left, addr right, addr *ret);
_g int multi_lc_number_common_(addr left, addr right, addr *ret);
_g int multi_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
#define multi_cf_number_common_(m,a,b,r) multi_fc_number_common_((m),(b),(a),(r))
#define multi_cb_number_common_(m,a,b,r) multi_bc_number_common_((m),(b),(a),(r))
#define multi_cr_number_common_(m,a,b,r) multi_rc_number_common_((m),(b),(a),(r))
#define multi_cs_number_common_(a,b,r) multi_sc_number_common_((b),(a),(r))
#define multi_cd_number_common_(a,b,r) multi_dc_number_common_((b),(a),(r))
#define multi_cl_number_common_(a,b,r) multi_lc_number_common_((b),(a),(r))

_g int inverse_complex_common_(LocalRoot local, addr pos, addr *ret);

_g int div_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_complex_rational_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_cf_number_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_cb_number_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_cr_number_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_sc_number_common_(addr left, addr right, addr *ret);
_g int div_cs_number_common_(addr left, addr right, addr *ret);
_g int div_dc_number_common_(addr left, addr right, addr *ret);
_g int div_cd_number_common_(addr left, addr right, addr *ret);
_g int div_lc_number_common_(addr left, addr right, addr *ret);
_g int div_cl_number_common_(addr left, addr right, addr *ret);
_g int div_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret);

#endif

