#ifndef __RATIO_EQUAL_HEADER__
#define __RATIO_EQUAL_HEADER__

#include "local.h"
#include "typedef.h"

#define equal_value_nosign_ratio _n(equal_value_nosign_ratio)
#define equal_value_ratio _n(equal_value_ratio)
#define equal_fr_real _n(equal_fr_real)
#define equal_br_real _n(equal_br_real)
#define equal_rr_real _n(equal_rr_real)
#define equal_rs_real_ _n(equal_rs_real_)
#define equal_rd_real_ _n(equal_rd_real_)
#define equal_rl_real_ _n(equal_rl_real_)
#define compare_fr_real _n(compare_fr_real)
#define compare_rf_real _n(compare_rf_real)
#define compare_br_real _n(compare_br_real)
#define compare_rb_real _n(compare_rb_real)
#define compare_rr_real _n(compare_rr_real)
#define compare_rs_real_ _n(compare_rs_real_)
#define compare_rd_real_ _n(compare_rd_real_)
#define compare_rl_real_ _n(compare_rl_real_)
#define compare_sr_real_ _n(compare_sr_real_)
#define compare_dr_real_ _n(compare_dr_real_)
#define compare_lr_real_ _n(compare_lr_real_)

int equal_value_nosign_ratio(addr pos, bigtype numer, bigtype denom);
int equal_value_ratio(addr pos, int sign, bigtype numer, bigtype denom);
int equal_fr_real(addr left, addr right);
int equal_br_real(addr left, addr right);
#define equal_rf_real(a,b) equal_fr_real((b),(a))
#define equal_rb_real(a,b) equal_br_real((b),(a))
int equal_rr_real(addr left, addr right);
int equal_rs_real_(LocalRoot local, addr left, addr right, int *ret);
int equal_rd_real_(LocalRoot local, addr left, addr right, int *ret);
int equal_rl_real_(LocalRoot local, addr left, addr right, int *ret);
#define equal_sr_real_(m,a,b,r) equal_rs_real_((m),(b),(a),(r))
#define equal_dr_real_(m,a,b,r) equal_rd_real_((m),(b),(a),(r))
#define equal_lr_real_(m,a,b,r) equal_rl_real_((m),(b),(a),(r))

int compare_fr_real(LocalRoot local, addr left, addr right);
int compare_rf_real(LocalRoot local, addr left, addr right);
int compare_br_real(LocalRoot local, addr left, addr right);
int compare_rb_real(LocalRoot local, addr left, addr right);
int compare_rr_real(LocalRoot local, addr left, addr right);
int compare_rs_real_(LocalRoot local, addr left, addr right, int *ret);
int compare_rd_real_(LocalRoot local, addr left, addr right, int *ret);
int compare_rl_real_(LocalRoot local, addr left, addr right, int *ret);
int compare_sr_real_(LocalRoot local, addr left, addr right, int *ret);
int compare_dr_real_(LocalRoot local, addr left, addr right, int *ret);
int compare_lr_real_(LocalRoot local, addr left, addr right, int *ret);

#endif

