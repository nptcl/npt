#ifndef __FLOAT_EQUAL_HEADER__
#define __FLOAT_EQUAL_HEADER__

#include "bignum.h"
#include "typedef.h"

#define plusp_single_float(a) (0.0f < RefSingleFloat(a))
#define plusp_double_float(a) (0.0 < RefDoubleFloat(a))
#define plusp_long_float(a) (0.0L < RefLongFloat(a))
#define minusp_single_float(a) (RefSingleFloat(a) < 0.0f)
#define minusp_double_float(a) (RefDoubleFloat(a) < 0.0)
#define minusp_long_float(a) (RefLongFloat(a) < 0.0L)

_g int zerop_single_float(addr pos);
_g int zerop_double_float(addr pos);
_g int zerop_long_float(addr pos);
_g int zerop_float(addr pos);

_g int equal_fs_real(addr left, addr right);
_g int equal_fd_real(addr left, addr right);
_g int equal_fl_real(addr left, addr right);
#define equal_sf_real(a,b) equal_fs_real((b),(a))
#define equal_df_real(a,b) equal_fd_real((b),(a))
#define equal_lf_real(a,b) equal_fl_real((b),(a))
#define equal_bs_real(a,b) (single_float_bignum(a) == RefSingleFloat(b))
#define equal_bd_real(a,b) (double_float_bignum(a) == RefDoubleFloat(b))
#define equal_bl_real(a,b) (long_float_bignum(a) == RefLongFloat(b))
#define equal_sb_real(a,b) equal_bs_real((b),(a))
#define equal_db_real(a,b) equal_bd_real((b),(a))
#define equal_lb_real(a,b) equal_bl_real((b),(a))
#define equal_ss_real(a,b) (RefSingleFloat(a) == RefSingleFloat(b))
#define equal_dd_real(a,b) (RefDoubleFloat(a) == RefDoubleFloat(b))
#define equal_ll_real(a,b) (RefLongFloat(a) == RefLongFloat(b))
#define equal_sd_real(a,b) (RefSingleFloat(a) == RefDoubleFloat(b))
#define equal_sl_real(a,b) (RefSingleFloat(a) == RefLongFloat(b))
#define equal_ds_real(a,b) (RefDoubleFloat(a) == RefSingleFloat(b))
#define equal_dl_real(a,b) (RefDoubleFloat(a) == RefLongFloat(b))
#define equal_ls_real(a,b) (RefLongFloat(a) == RefSingleFloat(b))
#define equal_ld_real(a,b) (RefLongFloat(a) == RefDoubleFloat(b))

_g int compare_fs_real(addr left, addr right);
_g int compare_fd_real(addr left, addr right);
_g int compare_fl_real(addr left, addr right);
_g int compare_sf_real(addr left, addr right);
_g int compare_df_real(addr left, addr right);
_g int compare_lf_real(addr left, addr right);
_g int compare_ss_real(addr left, addr right);
_g int compare_sd_real(addr left, addr right);
_g int compare_sl_real(addr left, addr right);
_g int compare_ds_real(addr left, addr right);
_g int compare_dd_real(addr left, addr right);
_g int compare_dl_real(addr left, addr right);
_g int compare_ls_real(addr left, addr right);
_g int compare_ld_real(addr left, addr right);
_g int compare_ll_real(addr left, addr right);
_g int compare_float(addr left, addr right);
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
_g int less_float_clang(addr left, addr right);
_g int less_equal_float_clang(addr left, addr right);

_g int less_ss_clang(addr left, addr right);
_g int less_dd_clang(addr left, addr right);
_g int less_ll_clang(addr left, addr right);
_g int less_equal_ss_clang(addr left, addr right);
_g int less_equal_dd_clang(addr left, addr right);
_g int less_equal_ll_clang(addr left, addr right);

#endif

