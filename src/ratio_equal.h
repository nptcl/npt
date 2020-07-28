#ifndef __RATIO_EQUAL_HEADER__
#define __RATIO_EQUAL_HEADER__

#include "local.h"
#include "typedef.h"

_g int equal_value_nosign_ratio(addr pos, bigtype numer, bigtype denom);
_g int equal_value_ratio(addr pos, int sign, bigtype numer, bigtype denom);
_g int equal_fr_real(addr left, addr right);
_g int equal_br_real(addr left, addr right);
#define equal_rf_real(a,b) equal_fr_real((b),(a))
#define equal_rb_real(a,b) equal_br_real((b),(a))
_g int equal_rr_real(addr left, addr right);
_g int equal_rs_real_(LocalRoot local, addr left, addr right, int *ret);
_g int equal_rd_real_(LocalRoot local, addr left, addr right, int *ret);
_g int equal_rl_real_(LocalRoot local, addr left, addr right, int *ret);
#define equal_sr_real_(m,a,b,r) equal_rs_real_((m),(b),(a),(r))
#define equal_dr_real_(m,a,b,r) equal_rd_real_((m),(b),(a),(r))
#define equal_lr_real_(m,a,b,r) equal_rl_real_((m),(b),(a),(r))

_g int compare_fr_real(LocalRoot local, addr left, addr right);
_g int compare_rf_real(LocalRoot local, addr left, addr right);
_g int compare_br_real(LocalRoot local, addr left, addr right);
_g int compare_rb_real(LocalRoot local, addr left, addr right);
_g int compare_rr_real(LocalRoot local, addr left, addr right);
_g int compare_rs_real_(LocalRoot local, addr left, addr right, int *ret);
_g int compare_rd_real_(LocalRoot local, addr left, addr right, int *ret);
_g int compare_rl_real_(LocalRoot local, addr left, addr right, int *ret);
_g int compare_sr_real_(LocalRoot local, addr left, addr right, int *ret);
_g int compare_dr_real_(LocalRoot local, addr left, addr right, int *ret);
_g int compare_lr_real_(LocalRoot local, addr left, addr right, int *ret);

#endif

