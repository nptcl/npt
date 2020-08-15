#ifndef __FLOAT_PLUS_HEADER__
#define __FLOAT_PLUS_HEADER__

#include "local.h"
#include "typedef.h"

_g int plus_float_sv_alloc_(LocalRoot local, addr left, single_float right, addr *ret);
_g int plus_float_dv_alloc_(LocalRoot local, addr left, double_float right, addr *ret);
_g int plus_float_lv_alloc_(LocalRoot local, addr left, long_float right, addr *ret);
_g int plus_float_sv_local_(LocalRoot local, addr left, single_float right, addr *ret);
_g int plus_float_dv_local_(LocalRoot local, addr left, double_float right, addr *ret);
_g int plus_float_lv_local_(LocalRoot local, addr left, long_float right, addr *ret);
_g int plus_float_sv_heap_(addr left, single_float right, addr *ret);
_g int plus_float_dv_heap_(addr left, double_float right, addr *ret);
_g int plus_float_lv_heap_(addr left, long_float right, addr *ret);

_g int minus_float_sv_alloc_(LocalRoot local, addr left, single_float right, addr *ret);
_g int minus_float_dv_alloc_(LocalRoot local, addr left, double_float right, addr *ret);
_g int minus_float_lv_alloc_(LocalRoot local, addr left, long_float right, addr *ret);
_g int minus_float_sv_local_(LocalRoot local, addr left, single_float right, addr *ret);
_g int minus_float_dv_local_(LocalRoot local, addr left, double_float right, addr *ret);
_g int minus_float_lv_local_(LocalRoot local, addr left, long_float right, addr *ret);
_g int minus_float_sv_heap_(addr left, single_float right, addr *ret);
_g int minus_float_dv_heap_(addr left, double_float right, addr *ret);
_g int minus_float_lv_heap_(addr left, long_float right, addr *ret);

_g int minus_float_vs_alloc_(LocalRoot local, single_float left, addr right, addr *ret);
_g int minus_float_vd_alloc_(LocalRoot local, double_float left, addr right, addr *ret);
_g int minus_float_vl_alloc_(LocalRoot local, long_float left, addr right, addr *ret);
_g int minus_float_vs_local_(LocalRoot local, single_float left, addr right, addr *ret);
_g int minus_float_vd_local_(LocalRoot local, double_float left, addr right, addr *ret);
_g int minus_float_vl_local_(LocalRoot local, long_float left, addr right, addr *ret);
_g int minus_float_vs_heap_(single_float left, addr right, addr *ret);
_g int minus_float_vd_heap_(double_float left, addr right, addr *ret);
_g int minus_float_vl_heap_(long_float left, addr right, addr *ret);

_g void sign_reverse_floats_alloc(LocalRoot local, addr value, addr *ret);
_g void sign_reverse_floatd_alloc(LocalRoot local, addr value, addr *ret);
_g void sign_reverse_floatl_alloc(LocalRoot local, addr value, addr *ret);
_g void sign_reverse_floats_local(LocalRoot local, addr value, addr *ret);
_g void sign_reverse_floatd_local(LocalRoot local, addr value, addr *ret);
_g void sign_reverse_floatl_local(LocalRoot local, addr value, addr *ret);
_g void sign_reverse_floats_heap(addr value, addr *ret);
_g void sign_reverse_floatd_heap(addr value, addr *ret);
_g void sign_reverse_floatl_heap(addr value, addr *ret);

_g int plus_float_fs_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_fd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_fl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_fs_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_fd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_fl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_fs_heap_(addr left, addr right, addr *ret);
_g int plus_float_fd_heap_(addr left, addr right, addr *ret);
_g int plus_float_fl_heap_(addr left, addr right, addr *ret);
#define plus_float_sf_alloc_(m,a,b,r) plus_float_fs_alloc_((m),(b),(a),(r))
#define plus_float_df_alloc_(m,a,b,r) plus_float_fd_alloc_((m),(b),(a),(r))
#define plus_float_lf_alloc_(m,a,b,r) plus_float_fl_alloc_((m),(b),(a),(r))
#define plus_float_sf_local_(m,a,b,r) plus_float_fs_local_((m),(b),(a),(r))
#define plus_float_df_local_(m,a,b,r) plus_float_fd_local_((m),(b),(a),(r))
#define plus_float_lf_local_(m,a,b,r) plus_float_fl_local_((m),(b),(a),(r))
#define plus_float_sf_heap_(a,b,r) plus_float_fs_heap_((b),(a),(r))
#define plus_float_df_heap_(a,b,r) plus_float_fd_heap_((b),(a),(r))
#define plus_float_lf_heap_(a,b,r) plus_float_fl_heap_((b),(a),(r))

_g int plus_float_bs_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_bd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_bl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_bs_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_bd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_bl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_bs_heap_(addr left, addr right, addr *ret);
_g int plus_float_bd_heap_(addr left, addr right, addr *ret);
_g int plus_float_bl_heap_(addr left, addr right, addr *ret);
#define plus_float_sb_alloc_(m,a,b,r) plus_float_bs_alloc_((m),(b),(a),(r))
#define plus_float_db_alloc_(m,a,b,r) plus_float_bd_alloc_((m),(b),(a),(r))
#define plus_float_lb_alloc_(m,a,b,r) plus_float_bl_alloc_((m),(b),(a),(r))
#define plus_float_sb_local_(m,a,b,r) plus_float_bs_local_((m),(b),(a),(r))
#define plus_float_db_local_(m,a,b,r) plus_float_bd_local_((m),(b),(a),(r))
#define plus_float_lb_local_(m,a,b,r) plus_float_bl_local_((m),(b),(a),(r))
#define plus_float_sb_heap_(a,b,r) plus_float_bs_heap_((b),(a),(r))
#define plus_float_db_heap_(a,b,r) plus_float_bd_heap_((b),(a),(r))
#define plus_float_lb_heap_(a,b,r) plus_float_bl_heap_((b),(a),(r))

_g int plus_float_rs_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_rd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_rl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_rs_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_rd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_rl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_rs_heap_(addr left, addr right, addr *ret);
_g int plus_float_rd_heap_(addr left, addr right, addr *ret);
_g int plus_float_rl_heap_(addr left, addr right, addr *ret);
#define plus_float_sr_alloc_(m,a,b,r) plus_float_rs_alloc_((m),(b),(a),(r))
#define plus_float_dr_alloc_(m,a,b,r) plus_float_rd_alloc_((m),(b),(a),(r))
#define plus_float_lr_alloc_(m,a,b,r) plus_float_rl_alloc_((m),(b),(a),(r))
#define plus_float_sr_local_(m,a,b,r) plus_float_rs_local_((m),(b),(a),(r))
#define plus_float_dr_local_(m,a,b,r) plus_float_rd_local_((m),(b),(a),(r))
#define plus_float_lr_local_(m,a,b,r) plus_float_rl_local_((m),(b),(a),(r))
#define plus_float_sr_heap_(a,b,r) plus_float_rs_heap_((b),(a),(r))
#define plus_float_dr_heap_(a,b,r) plus_float_rd_heap_((b),(a),(r))
#define plus_float_lr_heap_(a,b,r) plus_float_rl_heap_((b),(a),(r))

_g int plus_float_ss_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_sd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_sl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_ds_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_dd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_dl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_ls_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_ld_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_ll_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_ss_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_sd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_sl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_ds_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_dd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_dl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_ls_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_ld_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_ll_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_float_ss_heap_(addr left, addr right, addr *ret);
_g int plus_float_sd_heap_(addr left, addr right, addr *ret);
_g int plus_float_sl_heap_(addr left, addr right, addr *ret);
_g int plus_float_ds_heap_(addr left, addr right, addr *ret);
_g int plus_float_dd_heap_(addr left, addr right, addr *ret);
_g int plus_float_dl_heap_(addr left, addr right, addr *ret);
_g int plus_float_ls_heap_(addr left, addr right, addr *ret);
_g int plus_float_ld_heap_(addr left, addr right, addr *ret);
_g int plus_float_ll_heap_(addr left, addr right, addr *ret);

_g int minus_float_fs_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_fd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_fl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_fs_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_fd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_fl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_fs_heap_(addr left, addr right, addr *ret);
_g int minus_float_fd_heap_(addr left, addr right, addr *ret);
_g int minus_float_fl_heap_(addr left, addr right, addr *ret);

_g int minus_float_sf_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_df_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_lf_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_sf_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_df_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_lf_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_sf_heap_(addr left, addr right, addr *ret);
_g int minus_float_df_heap_(addr left, addr right, addr *ret);
_g int minus_float_lf_heap_(addr left, addr right, addr *ret);

_g int minus_float_bs_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_bd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_bl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_bs_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_bd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_bl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_bs_heap_(addr left, addr right, addr *ret);
_g int minus_float_bd_heap_(addr left, addr right, addr *ret);
_g int minus_float_bl_heap_(addr left, addr right, addr *ret);

_g int minus_float_sb_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_db_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_lb_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_sb_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_db_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_lb_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_sb_heap_(addr left, addr right, addr *ret);
_g int minus_float_db_heap_(addr left, addr right, addr *ret);
_g int minus_float_lb_heap_(addr left, addr right, addr *ret);

_g int minus_float_rs_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_rd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_rl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_rs_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_rd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_rl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_rs_heap_(addr left, addr right, addr *ret);
_g int minus_float_rd_heap_(addr left, addr right, addr *ret);
_g int minus_float_rl_heap_(addr left, addr right, addr *ret);

_g int minus_float_sr_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_dr_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_lr_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_sr_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_dr_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_lr_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_sr_heap_(addr left, addr right, addr *ret);
_g int minus_float_dr_heap_(addr left, addr right, addr *ret);
_g int minus_float_lr_heap_(addr left, addr right, addr *ret);

_g int minus_float_ss_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_sd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_sl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_ds_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_dd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_dl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_ls_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_ld_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_ll_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_ss_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_sd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_sl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_ds_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_dd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_dl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_ls_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_ld_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_ll_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_float_ss_heap_(addr left, addr right, addr *ret);
_g int minus_float_sd_heap_(addr left, addr right, addr *ret);
_g int minus_float_sl_heap_(addr left, addr right, addr *ret);
_g int minus_float_ds_heap_(addr left, addr right, addr *ret);
_g int minus_float_dd_heap_(addr left, addr right, addr *ret);
_g int minus_float_dl_heap_(addr left, addr right, addr *ret);
_g int minus_float_ls_heap_(addr left, addr right, addr *ret);
_g int minus_float_ld_heap_(addr left, addr right, addr *ret);
_g int minus_float_ll_heap_(addr left, addr right, addr *ret);

#endif

