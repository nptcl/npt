#ifndef __FLOAT_PLUS_HEADER__
#define __FLOAT_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

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

_g void plus_float_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_float_heap(LocalRoot local, addr left, addr right, addr *ret);

#endif

