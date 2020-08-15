#ifndef __FLOAT_MULTI_HEADER__
#define __FLOAT_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

_g int multi_float_fs_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_fd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_fl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_fs_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_fd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_fl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_fs_heap_(addr left, addr right, addr *ret);
_g int multi_float_fd_heap_(addr left, addr right, addr *ret);
_g int multi_float_fl_heap_(addr left, addr right, addr *ret);
#define multi_float_sf_alloc_(m,a,b,r) multi_float_fs_alloc_((m),(b),(a),(r))
#define multi_float_df_alloc_(m,a,b,r) multi_float_fd_alloc_((m),(b),(a),(r))
#define multi_float_lf_alloc_(m,a,b,r) multi_float_fl_alloc_((m),(b),(a),(r))
#define multi_float_sf_local_(m,a,b,r) multi_float_fs_local_((m),(b),(a),(r))
#define multi_float_df_local_(m,a,b,r) multi_float_fd_local_((m),(b),(a),(r))
#define multi_float_lf_local_(m,a,b,r) multi_float_fl_local_((m),(b),(a),(r))
#define multi_float_sf_heap_(a,b,r) multi_float_fs_heap_((b),(a),(r))
#define multi_float_df_heap_(a,b,r) multi_float_fd_heap_((b),(a),(r))
#define multi_float_lf_heap_(a,b,r) multi_float_fl_heap_((b),(a),(r))

_g int multi_float_bs_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_bd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_bl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_bs_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_bd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_bl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_bs_heap_(addr left, addr right, addr *ret);
_g int multi_float_bd_heap_(addr left, addr right, addr *ret);
_g int multi_float_bl_heap_(addr left, addr right, addr *ret);
#define multi_float_sb_alloc_(m,a,b,r) multi_float_bs_alloc_((m),(b),(a),(r))
#define multi_float_db_alloc_(m,a,b,r) multi_float_bd_alloc_((m),(b),(a),(r))
#define multi_float_lb_alloc_(m,a,b,r) multi_float_bl_alloc_((m),(b),(a),(r))
#define multi_float_sb_local_(m,a,b,r) multi_float_bs_local_((m),(b),(a),(r))
#define multi_float_db_local_(m,a,b,r) multi_float_bd_local_((m),(b),(a),(r))
#define multi_float_lb_local_(m,a,b,r) multi_float_bl_local_((m),(b),(a),(r))
#define multi_float_sb_heap_(a,b,r) multi_float_bs_heap_((b),(a),(r))
#define multi_float_db_heap_(a,b,r) multi_float_bd_heap_((b),(a),(r))
#define multi_float_lb_heap_(a,b,r) multi_float_bl_heap_((b),(a),(r))

_g int multi_float_rs_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_rd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_rl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_rs_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_rd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_rl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_rs_heap_(addr left, addr right, addr *ret);
_g int multi_float_rd_heap_(addr left, addr right, addr *ret);
_g int multi_float_rl_heap_(addr left, addr right, addr *ret);
#define multi_float_sr_alloc_(m,a,b,r) multi_float_rs_alloc_((m),(b),(a),(r))
#define multi_float_dr_alloc_(m,a,b,r) multi_float_rd_alloc_((m),(b),(a),(r))
#define multi_float_lr_alloc_(m,a,b,r) multi_float_rl_alloc_((m),(b),(a),(r))
#define multi_float_sr_local_(m,a,b,r) multi_float_rs_local_((m),(b),(a),(r))
#define multi_float_dr_local_(m,a,b,r) multi_float_rd_local_((m),(b),(a),(r))
#define multi_float_lr_local_(m,a,b,r) multi_float_rl_local_((m),(b),(a),(r))
#define multi_float_sr_heap_(a,b,r) multi_float_rs_heap_((b),(a),(r))
#define multi_float_dr_heap_(a,b,r) multi_float_rd_heap_((b),(a),(r))
#define multi_float_lr_heap_(a,b,r) multi_float_rl_heap_((b),(a),(r))

_g int multi_float_ss_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_sd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_sl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_ds_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_dd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_dl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_ls_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_ld_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_ll_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_ss_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_sd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_sl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_ds_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_dd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_dl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_ls_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_ld_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_ll_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_float_ss_heap_(addr left, addr right, addr *ret);
_g int multi_float_sd_heap_(addr left, addr right, addr *ret);
_g int multi_float_sl_heap_(addr left, addr right, addr *ret);
_g int multi_float_ds_heap_(addr left, addr right, addr *ret);
_g int multi_float_dd_heap_(addr left, addr right, addr *ret);
_g int multi_float_dl_heap_(addr left, addr right, addr *ret);
_g int multi_float_ls_heap_(addr left, addr right, addr *ret);
_g int multi_float_ld_heap_(addr left, addr right, addr *ret);
_g int multi_float_ll_heap_(addr left, addr right, addr *ret);

_g int inverse_single_float_alloc_(LocalRoot local, addr pos, addr *ret);
_g int inverse_double_float_alloc_(LocalRoot local, addr pos, addr *ret);
_g int inverse_long_float_alloc_(LocalRoot local, addr pos, addr *ret);
_g int inverse_single_float_local_(LocalRoot local, addr pos, addr *ret);
_g int inverse_double_float_local_(LocalRoot local, addr pos, addr *ret);
_g int inverse_long_float_local_(LocalRoot local, addr pos, addr *ret);
_g int inverse_single_float_heap_(addr pos, addr *ret);
_g int inverse_double_float_heap_(addr pos, addr *ret);
_g int inverse_long_float_heap_(addr pos, addr *ret);

_g int div_float_fs_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_fd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_fl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_fs_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_fd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_fl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_fs_heap_(addr left, addr right, addr *ret);
_g int div_float_fd_heap_(addr left, addr right, addr *ret);
_g int div_float_fl_heap_(addr left, addr right, addr *ret);

_g int div_float_sf_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_df_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_lf_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_sf_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_df_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_lf_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_sf_heap_(addr left, addr right, addr *ret);
_g int div_float_df_heap_(addr left, addr right, addr *ret);
_g int div_float_lf_heap_(addr left, addr right, addr *ret);

_g int div_float_bs_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_bd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_bl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_bs_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_bd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_bl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_bs_heap_(addr left, addr right, addr *ret);
_g int div_float_bd_heap_(addr left, addr right, addr *ret);
_g int div_float_bl_heap_(addr left, addr right, addr *ret);

_g int div_float_sb_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_db_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_lb_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_sb_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_db_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_lb_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_sb_heap_(addr left, addr right, addr *ret);
_g int div_float_db_heap_(addr left, addr right, addr *ret);
_g int div_float_lb_heap_(addr left, addr right, addr *ret);

_g int div_float_rs_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_rd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_rl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_rs_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_rd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_rl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_rs_heap_(addr left, addr right, addr *ret);
_g int div_float_rd_heap_(addr left, addr right, addr *ret);
_g int div_float_rl_heap_(addr left, addr right, addr *ret);

_g int div_float_sr_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_dr_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_lr_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_sr_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_dr_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_lr_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_sr_heap_(addr left, addr right, addr *ret);
_g int div_float_dr_heap_(addr left, addr right, addr *ret);
_g int div_float_lr_heap_(addr left, addr right, addr *ret);

_g int div_float_ss_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_sd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_sl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_ds_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_dd_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_dl_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_ls_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_ld_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_ll_alloc_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_ss_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_sd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_sl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_ds_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_dd_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_dl_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_ls_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_ld_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_ll_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int div_float_ss_heap_(addr left, addr right, addr *ret);
_g int div_float_sd_heap_(addr left, addr right, addr *ret);
_g int div_float_sl_heap_(addr left, addr right, addr *ret);
_g int div_float_ds_heap_(addr left, addr right, addr *ret);
_g int div_float_dd_heap_(addr left, addr right, addr *ret);
_g int div_float_dl_heap_(addr left, addr right, addr *ret);
_g int div_float_ls_heap_(addr left, addr right, addr *ret);
_g int div_float_ld_heap_(addr left, addr right, addr *ret);
_g int div_float_ll_heap_(addr left, addr right, addr *ret);

#endif

