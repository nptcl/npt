#ifndef __REAL_FLOAT_HEADER__
#define __REAL_FLOAT_HEADER__

#include <math.h>
#include "constant.h"
#include "local.h"
#include "typedef.h"

enum fltclass {
	fltclass_normal,
	fltclass_overflow,
	fltclass_underflow,
	fltclass_nan
};
#ifdef __cplusplus
typedef int fltclasstype;
#else
typedef enum fltclass fltclasstype;
#endif

_g fltclasstype fltclassify(int check, int sign);
_g void float_fltclass(constindex index, fltclasstype type, ...);
#define getfltclassify(v) fltclassify(fpclassify(v), signbit(v))
#define getfltclassify_reverse(v) fltclassify(fpclassify(v), (! signbit(v)))
#define float_errorcheck1(index, v, left) { \
	fltclasstype __type = getfltclassify(v); \
	if (__type != fltclass_normal) { \
		float_fltclass((index), __type, (left), NULL); \
	} \
}
#define float_errorcheck2(index, v, left, right) { \
	fltclasstype __type = getfltclassify(v); \
	if (__type != fltclass_normal) { \
		float_fltclass((index), __type, (left), (right), NULL); \
	} \
}

_g void single_float_check_alloc(LocalRoot local, addr *ret, single_float value);
_g void single_float_check_local(LocalRoot local, addr *ret, single_float value);
_g void single_float_check_heap(addr *ret, single_float value);
_g void double_float_check_local(LocalRoot local, addr *ret, double_float value);
_g void double_float_check_alloc(LocalRoot local, addr *ret, double_float value);
_g void double_float_check_heap(addr *ret, double_float value);
_g void long_float_check_alloc(LocalRoot local, addr *ret, long_float value);
_g void long_float_check_local(LocalRoot local, addr *ret, long_float value);
_g void long_float_check_heap(addr *ret, long_float value);

_g void single_float_throw_heap(addr pos, addr *ret);
_g void double_float_throw_heap(addr pos, addr *ret);
_g void long_float_throw_heap(addr pos, addr *ret);
_g void single_float_throw_local(LocalRoot local, addr pos, addr *ret);
_g void double_float_throw_local(LocalRoot local, addr pos, addr *ret);
_g void long_float_throw_local(LocalRoot local, addr pos, addr *ret);
_g void single_float_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void double_float_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void long_float_throw_alloc(LocalRoot local, addr pos, addr *ret);

_g void float_throw_heap(addr pos, addr *ret);
_g void float_throw_local(LocalRoot local, addr pos, addr *ret);
_g void float_throw_alloc(LocalRoot local, addr pos, addr *ret);
#define float_result_heap float_throw_heap
#define float_result_local float_throw_local
#define float_result_alloc float_throw_alloc
_g void float_copy_alloc(LocalRoot local, addr pos, addr *ret);
_g void float_copy_local(LocalRoot local, addr pos, addr *ret);
_g void float_copy_heap(addr pos, addr *ret);

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

_g single_float check_strtof(const char *str, addr pos);
_g double_float check_strtod(const char *str, addr pos);
_g long_float check_strtold(const char *str, addr pos);
_g single_float check_strtof_reverse(const char *str, addr pos);
_g double_float check_strtod_reverse(const char *str, addr pos);
_g long_float check_strtold_reverse(const char *str, addr pos);

_g void plus_float_sv_alloc(LocalRoot local, addr left, single_float right, addr *ret);
_g void plus_float_dv_alloc(LocalRoot local, addr left, double_float right, addr *ret);
_g void plus_float_lv_alloc(LocalRoot local, addr left, long_float right, addr *ret);
_g void plus_float_sv_local(LocalRoot local, addr left, single_float right, addr *ret);
_g void plus_float_dv_local(LocalRoot local, addr left, double_float right, addr *ret);
_g void plus_float_lv_local(LocalRoot local, addr left, long_float right, addr *ret);
_g void plus_float_sv_heap(addr left, single_float right, addr *ret);
_g void plus_float_dv_heap(addr left, double_float right, addr *ret);
_g void plus_float_lv_heap(addr left, long_float right, addr *ret);

_g void minus_float_sv_alloc(LocalRoot local, addr left, single_float right, addr *ret);
_g void minus_float_dv_alloc(LocalRoot local, addr left, double_float right, addr *ret);
_g void minus_float_lv_alloc(LocalRoot local, addr left, long_float right, addr *ret);
_g void minus_float_sv_local(LocalRoot local, addr left, single_float right, addr *ret);
_g void minus_float_dv_local(LocalRoot local, addr left, double_float right, addr *ret);
_g void minus_float_lv_local(LocalRoot local, addr left, long_float right, addr *ret);
_g void minus_float_sv_heap(addr left, single_float right, addr *ret);
_g void minus_float_dv_heap(addr left, double_float right, addr *ret);
_g void minus_float_lv_heap(addr left, long_float right, addr *ret);

_g void minus_float_vs_alloc(LocalRoot local, single_float left, addr right, addr *ret);
_g void minus_float_vd_alloc(LocalRoot local, double_float left, addr right, addr *ret);
_g void minus_float_vl_alloc(LocalRoot local, long_float left, addr right, addr *ret);
_g void minus_float_vs_local(LocalRoot local, single_float left, addr right, addr *ret);
_g void minus_float_vd_local(LocalRoot local, double_float left, addr right, addr *ret);
_g void minus_float_vl_local(LocalRoot local, long_float left, addr right, addr *ret);
_g void minus_float_vs_heap(single_float left, addr right, addr *ret);
_g void minus_float_vd_heap(double_float left, addr right, addr *ret);
_g void minus_float_vl_heap(long_float left, addr right, addr *ret);

_g void sign_reverse_floats_alloc(LocalRoot local, addr value, addr *ret);
_g void sign_reverse_floatd_alloc(LocalRoot local, addr value, addr *ret);
_g void sign_reverse_floatl_alloc(LocalRoot local, addr value, addr *ret);
_g void sign_reverse_floats_local(LocalRoot local, addr value, addr *ret);
_g void sign_reverse_floatd_local(LocalRoot local, addr value, addr *ret);
_g void sign_reverse_floatl_local(LocalRoot local, addr value, addr *ret);
_g void sign_reverse_floats_heap(addr value, addr *ret);
_g void sign_reverse_floatd_heap(addr value, addr *ret);
_g void sign_reverse_floatl_heap(addr value, addr *ret);

_g void plus_float_fs_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_fd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_fl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_fs_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_fd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_fl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_fs_heap(addr left, addr right, addr *ret);
_g void plus_float_fd_heap(addr left, addr right, addr *ret);
_g void plus_float_fl_heap(addr left, addr right, addr *ret);
#define plus_float_sf_alloc(m,a,b,r) plus_float_fs_alloc((m),(b),(a),(r))
#define plus_float_df_alloc(m,a,b,r) plus_float_fd_alloc((m),(b),(a),(r))
#define plus_float_lf_alloc(m,a,b,r) plus_float_fl_alloc((m),(b),(a),(r))
#define plus_float_sf_local(m,a,b,r) plus_float_fs_local((m),(b),(a),(r))
#define plus_float_df_local(m,a,b,r) plus_float_fd_local((m),(b),(a),(r))
#define plus_float_lf_local(m,a,b,r) plus_float_fl_local((m),(b),(a),(r))
#define plus_float_sf_heap(a,b,r) plus_float_fs_heap((b),(a),(r))
#define plus_float_df_heap(a,b,r) plus_float_fd_heap((b),(a),(r))
#define plus_float_lf_heap(a,b,r) plus_float_fl_heap((b),(a),(r))

_g void plus_float_bs_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_bd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_bl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_bs_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_bd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_bl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_bs_heap(addr left, addr right, addr *ret);
_g void plus_float_bd_heap(addr left, addr right, addr *ret);
_g void plus_float_bl_heap(addr left, addr right, addr *ret);
#define plus_float_sb_alloc(m,a,b,r) plus_float_bs_alloc((m),(b),(a),(r))
#define plus_float_db_alloc(m,a,b,r) plus_float_bd_alloc((m),(b),(a),(r))
#define plus_float_lb_alloc(m,a,b,r) plus_float_bl_alloc((m),(b),(a),(r))
#define plus_float_sb_local(m,a,b,r) plus_float_bs_local((m),(b),(a),(r))
#define plus_float_db_local(m,a,b,r) plus_float_bd_local((m),(b),(a),(r))
#define plus_float_lb_local(m,a,b,r) plus_float_bl_local((m),(b),(a),(r))
#define plus_float_sb_heap(a,b,r) plus_float_bs_heap((b),(a),(r))
#define plus_float_db_heap(a,b,r) plus_float_bd_heap((b),(a),(r))
#define plus_float_lb_heap(a,b,r) plus_float_bl_heap((b),(a),(r))

_g void plus_float_rs_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_rd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_rl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_rs_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_rd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_rl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_rs_heap(addr left, addr right, addr *ret);
_g void plus_float_rd_heap(addr left, addr right, addr *ret);
_g void plus_float_rl_heap(addr left, addr right, addr *ret);
#define plus_float_sr_alloc(m,a,b,r) plus_float_rs_alloc((m),(b),(a),(r))
#define plus_float_dr_alloc(m,a,b,r) plus_float_rd_alloc((m),(b),(a),(r))
#define plus_float_lr_alloc(m,a,b,r) plus_float_rl_alloc((m),(b),(a),(r))
#define plus_float_sr_local(m,a,b,r) plus_float_rs_local((m),(b),(a),(r))
#define plus_float_dr_local(m,a,b,r) plus_float_rd_local((m),(b),(a),(r))
#define plus_float_lr_local(m,a,b,r) plus_float_rl_local((m),(b),(a),(r))
#define plus_float_sr_heap(a,b,r) plus_float_rs_heap((b),(a),(r))
#define plus_float_dr_heap(a,b,r) plus_float_rd_heap((b),(a),(r))
#define plus_float_lr_heap(a,b,r) plus_float_rl_heap((b),(a),(r))

_g void plus_float_ss_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_sd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_sl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_ds_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_dd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_dl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_ls_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_ld_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_ll_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_ss_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_sd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_sl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_ds_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_dd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_dl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_ls_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_ld_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_ll_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_ss_heap(addr left, addr right, addr *ret);
_g void plus_float_sd_heap(addr left, addr right, addr *ret);
_g void plus_float_sl_heap(addr left, addr right, addr *ret);
_g void plus_float_ds_heap(addr left, addr right, addr *ret);
_g void plus_float_dd_heap(addr left, addr right, addr *ret);
_g void plus_float_dl_heap(addr left, addr right, addr *ret);
_g void plus_float_ls_heap(addr left, addr right, addr *ret);
_g void plus_float_ld_heap(addr left, addr right, addr *ret);
_g void plus_float_ll_heap(addr left, addr right, addr *ret);

_g void minus_float_fs_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_fd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_fl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_fs_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_fd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_fl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_fs_heap(addr left, addr right, addr *ret);
_g void minus_float_fd_heap(addr left, addr right, addr *ret);
_g void minus_float_fl_heap(addr left, addr right, addr *ret);

_g void minus_float_sf_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_df_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_lf_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_sf_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_df_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_lf_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_sf_heap(addr left, addr right, addr *ret);
_g void minus_float_df_heap(addr left, addr right, addr *ret);
_g void minus_float_lf_heap(addr left, addr right, addr *ret);

_g void minus_float_bs_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_bd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_bl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_bs_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_bd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_bl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_bs_heap(addr left, addr right, addr *ret);
_g void minus_float_bd_heap(addr left, addr right, addr *ret);
_g void minus_float_bl_heap(addr left, addr right, addr *ret);

_g void minus_float_sb_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_db_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_lb_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_sb_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_db_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_lb_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_sb_heap(addr left, addr right, addr *ret);
_g void minus_float_db_heap(addr left, addr right, addr *ret);
_g void minus_float_lb_heap(addr left, addr right, addr *ret);

_g void minus_float_rs_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_rd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_rl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_rs_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_rd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_rl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_rs_heap(addr left, addr right, addr *ret);
_g void minus_float_rd_heap(addr left, addr right, addr *ret);
_g void minus_float_rl_heap(addr left, addr right, addr *ret);

_g void minus_float_sr_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_dr_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_lr_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_sr_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_dr_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_lr_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_sr_heap(addr left, addr right, addr *ret);
_g void minus_float_dr_heap(addr left, addr right, addr *ret);
_g void minus_float_lr_heap(addr left, addr right, addr *ret);

_g void minus_float_ss_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_sd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_sl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_ds_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_dd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_dl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_ls_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_ld_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_ll_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_ss_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_sd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_sl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_ds_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_dd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_dl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_ls_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_ld_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_ll_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_float_ss_heap(addr left, addr right, addr *ret);
_g void minus_float_sd_heap(addr left, addr right, addr *ret);
_g void minus_float_sl_heap(addr left, addr right, addr *ret);
_g void minus_float_ds_heap(addr left, addr right, addr *ret);
_g void minus_float_dd_heap(addr left, addr right, addr *ret);
_g void minus_float_dl_heap(addr left, addr right, addr *ret);
_g void minus_float_ls_heap(addr left, addr right, addr *ret);
_g void minus_float_ld_heap(addr left, addr right, addr *ret);
_g void minus_float_ll_heap(addr left, addr right, addr *ret);

_g void multi_float_fs_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_fd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_fl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_fs_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_fd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_fl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_fs_heap(addr left, addr right, addr *ret);
_g void multi_float_fd_heap(addr left, addr right, addr *ret);
_g void multi_float_fl_heap(addr left, addr right, addr *ret);
#define multi_float_sf_alloc(m,a,b,r) multi_float_fs_alloc((m),(b),(a),(r))
#define multi_float_df_alloc(m,a,b,r) multi_float_fd_alloc((m),(b),(a),(r))
#define multi_float_lf_alloc(m,a,b,r) multi_float_fl_alloc((m),(b),(a),(r))
#define multi_float_sf_local(m,a,b,r) multi_float_fs_local((m),(b),(a),(r))
#define multi_float_df_local(m,a,b,r) multi_float_fd_local((m),(b),(a),(r))
#define multi_float_lf_local(m,a,b,r) multi_float_fl_local((m),(b),(a),(r))
#define multi_float_sf_heap(a,b,r) multi_float_fs_heap((b),(a),(r))
#define multi_float_df_heap(a,b,r) multi_float_fd_heap((b),(a),(r))
#define multi_float_lf_heap(a,b,r) multi_float_fl_heap((b),(a),(r))

_g void multi_float_bs_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_bd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_bl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_bs_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_bd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_bl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_bs_heap(addr left, addr right, addr *ret);
_g void multi_float_bd_heap(addr left, addr right, addr *ret);
_g void multi_float_bl_heap(addr left, addr right, addr *ret);
#define multi_float_sb_alloc(m,a,b,r) multi_float_bs_alloc((m),(b),(a),(r))
#define multi_float_db_alloc(m,a,b,r) multi_float_bd_alloc((m),(b),(a),(r))
#define multi_float_lb_alloc(m,a,b,r) multi_float_bl_alloc((m),(b),(a),(r))
#define multi_float_sb_local(m,a,b,r) multi_float_bs_local((m),(b),(a),(r))
#define multi_float_db_local(m,a,b,r) multi_float_bd_local((m),(b),(a),(r))
#define multi_float_lb_local(m,a,b,r) multi_float_bl_local((m),(b),(a),(r))
#define multi_float_sb_heap(a,b,r) multi_float_bs_heap((b),(a),(r))
#define multi_float_db_heap(a,b,r) multi_float_bd_heap((b),(a),(r))
#define multi_float_lb_heap(a,b,r) multi_float_bl_heap((b),(a),(r))

_g void multi_float_rs_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_rd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_rl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_rs_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_rd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_rl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_rs_heap(addr left, addr right, addr *ret);
_g void multi_float_rd_heap(addr left, addr right, addr *ret);
_g void multi_float_rl_heap(addr left, addr right, addr *ret);
#define multi_float_sr_alloc(m,a,b,r) multi_float_rs_alloc((m),(b),(a),(r))
#define multi_float_dr_alloc(m,a,b,r) multi_float_rd_alloc((m),(b),(a),(r))
#define multi_float_lr_alloc(m,a,b,r) multi_float_rl_alloc((m),(b),(a),(r))
#define multi_float_sr_local(m,a,b,r) multi_float_rs_local((m),(b),(a),(r))
#define multi_float_dr_local(m,a,b,r) multi_float_rd_local((m),(b),(a),(r))
#define multi_float_lr_local(m,a,b,r) multi_float_rl_local((m),(b),(a),(r))
#define multi_float_sr_heap(a,b,r) multi_float_rs_heap((b),(a),(r))
#define multi_float_dr_heap(a,b,r) multi_float_rd_heap((b),(a),(r))
#define multi_float_lr_heap(a,b,r) multi_float_rl_heap((b),(a),(r))

_g void multi_float_ss_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_sd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_sl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_ds_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_dd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_dl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_ls_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_ld_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_ll_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_ss_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_sd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_sl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_ds_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_dd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_dl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_ls_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_ld_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_ll_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_ss_heap(addr left, addr right, addr *ret);
_g void multi_float_sd_heap(addr left, addr right, addr *ret);
_g void multi_float_sl_heap(addr left, addr right, addr *ret);
_g void multi_float_ds_heap(addr left, addr right, addr *ret);
_g void multi_float_dd_heap(addr left, addr right, addr *ret);
_g void multi_float_dl_heap(addr left, addr right, addr *ret);
_g void multi_float_ls_heap(addr left, addr right, addr *ret);
_g void multi_float_ld_heap(addr left, addr right, addr *ret);
_g void multi_float_ll_heap(addr left, addr right, addr *ret);

_g void inverse_single_float_alloc(LocalRoot local, addr pos, addr *ret);
_g void inverse_double_float_alloc(LocalRoot local, addr pos, addr *ret);
_g void inverse_long_float_alloc(LocalRoot local, addr pos, addr *ret);
_g void inverse_single_float_local(LocalRoot local, addr pos, addr *ret);
_g void inverse_double_float_local(LocalRoot local, addr pos, addr *ret);
_g void inverse_long_float_local(LocalRoot local, addr pos, addr *ret);
_g void inverse_single_float_heap(addr pos, addr *ret);
_g void inverse_double_float_heap(addr pos, addr *ret);
_g void inverse_long_float_heap(addr pos, addr *ret);

_g void div_float_fs_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_fd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_fl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_fs_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_fd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_fl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_fs_heap(addr left, addr right, addr *ret);
_g void div_float_fd_heap(addr left, addr right, addr *ret);
_g void div_float_fl_heap(addr left, addr right, addr *ret);

_g void div_float_sf_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_df_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_lf_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_sf_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_df_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_lf_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_sf_heap(addr left, addr right, addr *ret);
_g void div_float_df_heap(addr left, addr right, addr *ret);
_g void div_float_lf_heap(addr left, addr right, addr *ret);

_g void div_float_bs_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_bd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_bl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_bs_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_bd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_bl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_bs_heap(addr left, addr right, addr *ret);
_g void div_float_bd_heap(addr left, addr right, addr *ret);
_g void div_float_bl_heap(addr left, addr right, addr *ret);

_g void div_float_sb_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_db_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_lb_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_sb_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_db_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_lb_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_sb_heap(addr left, addr right, addr *ret);
_g void div_float_db_heap(addr left, addr right, addr *ret);
_g void div_float_lb_heap(addr left, addr right, addr *ret);

_g void div_float_rs_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_rd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_rl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_rs_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_rd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_rl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_rs_heap(addr left, addr right, addr *ret);
_g void div_float_rd_heap(addr left, addr right, addr *ret);
_g void div_float_rl_heap(addr left, addr right, addr *ret);

_g void div_float_sr_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_dr_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_lr_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_sr_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_dr_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_lr_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_sr_heap(addr left, addr right, addr *ret);
_g void div_float_dr_heap(addr left, addr right, addr *ret);
_g void div_float_lr_heap(addr left, addr right, addr *ret);

_g void div_float_ss_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_sd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_sl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_ds_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_dd_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_dl_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_ls_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_ld_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_ll_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_ss_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_sd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_sl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_ds_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_dd_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_dl_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_ls_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_ld_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_ll_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_float_ss_heap(addr left, addr right, addr *ret);
_g void div_float_sd_heap(addr left, addr right, addr *ret);
_g void div_float_sl_heap(addr left, addr right, addr *ret);
_g void div_float_ds_heap(addr left, addr right, addr *ret);
_g void div_float_dd_heap(addr left, addr right, addr *ret);
_g void div_float_dl_heap(addr left, addr right, addr *ret);
_g void div_float_ls_heap(addr left, addr right, addr *ret);
_g void div_float_ld_heap(addr left, addr right, addr *ret);
_g void div_float_ll_heap(addr left, addr right, addr *ret);

_g void abs_floats_alloc(LocalRoot local, addr left, addr *ret);
_g void abs_floatd_alloc(LocalRoot local, addr left, addr *ret);
_g void abs_floatl_alloc(LocalRoot local, addr left, addr *ret);
_g void abs_floats_local(LocalRoot local, addr left, addr *ret);
_g void abs_floatd_local(LocalRoot local, addr left, addr *ret);
_g void abs_floatl_local(LocalRoot local, addr left, addr *ret);
_g void abs_floats_heap(addr left, addr *ret);
_g void abs_floatd_heap(addr left, addr *ret);
_g void abs_floatl_heap(addr left, addr *ret);

_g double_float cast_sd_float(single_float v);
_g long_float cast_sl_float(single_float v);
_g single_float cast_ds_float(double_float v);
_g long_float cast_dl_float(double_float v);
_g single_float cast_ls_float(long_float v);
_g double_float cast_ld_float(long_float v);
#define cast_ss_float(x) (x)
#define cast_dd_float(x) (x)
#define cast_ll_float(x) (x)

_g double_float cast_sd_value(addr pos);
_g long_float cast_sl_value(addr pos);
_g single_float cast_ds_value(addr pos);
_g long_float cast_dl_value(addr pos);
_g single_float cast_ls_value(addr pos);
_g double_float cast_ld_value(addr pos);
#define cast_ss_value RefSingleFloat
#define cast_dd_value RefDoubleFloat
#define cast_ll_value RefLongFloat

_g void cast_float_alloc(LocalRoot local, addr left, addr *ret);
_g void cast_float_local(LocalRoot local, addr left, addr *ret);
_g void cast_float_heap(addr left, addr *ret);

_g void multi_float_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_heap(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_heap(LocalRoot local, addr left, addr right, addr *ret);
_g void sqrt_float_alloc(LocalRoot local, addr left, addr *ret);
_g void sqrt_float_local(LocalRoot local, addr left, addr *ret);
_g void sqrt_float_heap(LocalRoot local, addr left, addr *ret);

#endif

