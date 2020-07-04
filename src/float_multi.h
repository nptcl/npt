#ifndef __FLOAT_MULTI_HEADER__
#define __FLOAT_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

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

_g void multi_float_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_float_heap(LocalRoot local, addr left, addr right, addr *ret);

#endif

