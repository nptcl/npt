#ifndef __FLOAT_EQUAL_HEADER__
#define __FLOAT_EQUAL_HEADER__

#include "bignum.h"
#include "typedef.h"

#define zerop_single_float _n(zerop_single_float)
#define zerop_double_float _n(zerop_double_float)
#define zerop_long_float _n(zerop_long_float)
#define equal_fs_real _n(equal_fs_real)
#define equal_fd_real _n(equal_fd_real)
#define equal_fl_real _n(equal_fl_real)
#define equal_bs_real_ _n(equal_bs_real_)
#define equal_bd_real_ _n(equal_bd_real_)
#define equal_bl_real_ _n(equal_bl_real_)
#define compare_fs_real _n(compare_fs_real)
#define compare_fd_real _n(compare_fd_real)
#define compare_fl_real _n(compare_fl_real)
#define compare_sf_real _n(compare_sf_real)
#define compare_df_real _n(compare_df_real)
#define compare_lf_real _n(compare_lf_real)
#define compare_ss_real _n(compare_ss_real)
#define compare_sd_real _n(compare_sd_real)
#define compare_sl_real _n(compare_sl_real)
#define compare_ds_real _n(compare_ds_real)
#define compare_dd_real _n(compare_dd_real)
#define compare_dl_real _n(compare_dl_real)
#define compare_ls_real _n(compare_ls_real)
#define compare_ld_real _n(compare_ld_real)
#define compare_ll_real _n(compare_ll_real)
#define compare_float _n(compare_float)
#define less_float_clang _n(less_float_clang)
#define less_equal_float_clang _n(less_equal_float_clang)
#define less_ss_clang _n(less_ss_clang)
#define less_dd_clang _n(less_dd_clang)
#define less_ll_clang _n(less_ll_clang)
#define less_equal_ss_clang _n(less_equal_ss_clang)
#define less_equal_dd_clang _n(less_equal_dd_clang)
#define less_equal_ll_clang _n(less_equal_ll_clang)
#define compare_float_ _n(compare_float_)
#define less_float_clang_ _n(less_float_clang_)
#define less_equal_float_clang_ _n(less_equal_float_clang_)

#define plusp_single_float(a) (0.0f < RefSingleFloat(a))
#define plusp_double_float(a) (0.0 < RefDoubleFloat(a))
#define plusp_long_float(a) (0.0L < RefLongFloat(a))
#define minusp_single_float(a) (RefSingleFloat(a) < 0.0f)
#define minusp_double_float(a) (RefDoubleFloat(a) < 0.0)
#define minusp_long_float(a) (RefLongFloat(a) < 0.0L)

int zerop_single_float(addr pos);
int zerop_double_float(addr pos);
int zerop_long_float(addr pos);

int equal_fs_real(addr left, addr right);
int equal_fd_real(addr left, addr right);
int equal_fl_real(addr left, addr right);
#define equal_sf_real(a,b) equal_fs_real((b),(a))
#define equal_df_real(a,b) equal_fd_real((b),(a))
#define equal_lf_real(a,b) equal_fl_real((b),(a))
int equal_bs_real_(addr left, addr right, int *ret);
int equal_bd_real_(addr left, addr right, int *ret);
int equal_bl_real_(addr left, addr right, int *ret);
#define equal_sb_real_(a,b,r) equal_bs_real_((b),(a),(r))
#define equal_db_real_(a,b,r) equal_bd_real_((b),(a),(r))
#define equal_lb_real_(a,b,r) equal_bl_real_((b),(a),(r))
#define equal_ss_real(a,b) (RefSingleFloat(a) == RefSingleFloat(b))
#define equal_dd_real(a,b) (RefDoubleFloat(a) == RefDoubleFloat(b))
#define equal_ll_real(a,b) (RefLongFloat(a) == RefLongFloat(b))
#define equal_sd_real(a,b) (RefSingleFloat(a) == RefDoubleFloat(b))
#define equal_sl_real(a,b) (RefSingleFloat(a) == RefLongFloat(b))
#define equal_ds_real(a,b) (RefDoubleFloat(a) == RefSingleFloat(b))
#define equal_dl_real(a,b) (RefDoubleFloat(a) == RefLongFloat(b))
#define equal_ls_real(a,b) (RefLongFloat(a) == RefSingleFloat(b))
#define equal_ld_real(a,b) (RefLongFloat(a) == RefDoubleFloat(b))

int compare_fs_real(addr left, addr right);
int compare_fd_real(addr left, addr right);
int compare_fl_real(addr left, addr right);
int compare_sf_real(addr left, addr right);
int compare_df_real(addr left, addr right);
int compare_lf_real(addr left, addr right);
int compare_ss_real(addr left, addr right);
int compare_sd_real(addr left, addr right);
int compare_sl_real(addr left, addr right);
int compare_ds_real(addr left, addr right);
int compare_dd_real(addr left, addr right);
int compare_dl_real(addr left, addr right);
int compare_ls_real(addr left, addr right);
int compare_ld_real(addr left, addr right);
int compare_ll_real(addr left, addr right);
int compare_float(addr left, addr right);
#define less_float(a,b) (compare_float((a), (b)) < 0)
#define less_equal_float(a,b) (compare_float((a), (b)) <= 0)
#define greater_float(a,b) (compare_float((a), (b)) > 0)
#define greater_equal_float(a,b) (compare_float((a), (b)) >= 0)
#define less_ss_real(a,b) (compare_ss_real((a), (b)) < 0)
#define less_equal_ss_real(a,b) (compare_ss_real((a), (b)) <= 0)
#define greater_ss_real(a,b) (compare_ss_real((a), (b)) > 0)
#define greater_equal_ss_real(a,b) (compare_ss_real((a), (b)) >= 0)
#define less_dd_real(a,b) (compare_dd_real((a), (b)) < 0)
#define less_equal_dd_real(a,b) (compare_dd_real((a), (b)) <= 0)
#define greater_dd_real(a,b) (compare_dd_real((a), (b)) > 0)
#define greater_equal_dd_real(a,b) (compare_dd_real((a), (b)) >= 0)
#define less_ll_real(a,b) (compare_ll_real((a), (b)) < 0)
#define less_equal_ll_real(a,b) (compare_ll_real((a), (b)) <= 0)
#define greater_ll_real(a,b) (compare_ll_real((a), (b)) > 0)
#define greater_equal_ll_real(a,b) (compare_ll_real((a), (b)) >= 0)
int less_float_clang(addr left, addr right);
int less_equal_float_clang(addr left, addr right);

int less_ss_clang(addr left, addr right);
int less_dd_clang(addr left, addr right);
int less_ll_clang(addr left, addr right);
int less_equal_ss_clang(addr left, addr right);
int less_equal_dd_clang(addr left, addr right);
int less_equal_ll_clang(addr left, addr right);

int compare_float_(addr left, addr right, int *ret);
int less_float_clang_(addr left, addr right, int *ret);
int less_equal_float_clang_(addr left, addr right, int *ret);

#endif

