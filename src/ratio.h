#ifndef __HEADER_RATIO__
#define __HEADER_RATIO__

#include "local.h"
#include "typedef.h"

#define SetNumerRatio_Low(p,v) SetArrayA2((p),0,(v))
#define GetNumerRatio_Low(p,v) GetArrayA2((p),0,(v))
#define SetDenomRatio_Low(p,v) SetArrayA2((p),1,(v))
#define GetDenomRatio_Low(p,v) GetArrayA2((p),1,(v))
#define SetSignRatio_Low(p,v) SetUser((p), (byte)(v))
#define GetSignRatio_Low(p,v) (*(v) = (int)GetUser(p))
#define RefSignRatio_Low(p)   ((int)GetUser(p))

#ifdef LISP_DEBUG
#define SetNumerRatio setnumer_ratio
#define GetNumerRatio getnumer_ratio
#define SetDenomRatio setdenom_ratio
#define GetDenomRatio getdenom_ratio
#define SetSignRatio setsign_ratio
#define GetSignRatio getsign_ratio
#define RefSignRatio refsign_ratio
#else
#define SetNumerRatio SetNumerRatio_Low
#define GetNumerRatio GetNumerRatio_Low
#define SetDenomRatio SetDenomRatio_Low
#define GetDenomRatio GetDenomRatio_Low
#define SetSignRatio SetSignRatio_Low
#define GetSignRatio GetSignRatio_Low
#define RefSignRatio RefSignRatio_Low
#endif

int ratiop(addr pos);
void setnumer_ratio(addr pos, addr value);
void getnumer_ratio(addr pos, addr *ret);
void setdenom_ratio(addr pos, addr value);
void getdenom_ratio(addr pos, addr *ret);
void setsign_ratio(addr pos, int sign);
void getsign_ratio(addr pos, int *ret);
int refsign_ratio(addr pos);

void euclidean_bignum(LocalRoot local, addr numer, addr denom);
void reduction_local(LocalRoot local, addr numer, addr denom);
void make_ratio_reduction_heap(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
void make_ratio_reduction_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
void make_ratio_alloc_unsafe(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
void ratio_reduction_heap(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
void ratio_reduction_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);

void ratio_reduction_value_local(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom);
void ratio_reduction_value_heap(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom);
void ratio_noreduction_value_local(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom);
void ratio_noreduction_value_heap(addr *ret,
		int sign, bigtype numer, bigtype denom);

void ratio_zero_alloc(LocalRoot local, addr *ret);
void ratio_zero_local(LocalRoot local, addr *ret);
void ratio_zero_heap(addr *ret);

void ratio_copy_nosign_alloc(LocalRoot local, addr *ret, addr pos);
void ratio_copy_nosign_local(LocalRoot local, addr *ret, addr pos);
void ratio_copy_nosign_heap(addr *ret, addr pos);
void ratio_copy_alloc(LocalRoot local, addr *ret, addr pos);
void ratio_copy_local(LocalRoot local, addr *ret, addr pos);
void ratio_copy_heap(addr *ret, addr pos);
void ratio_throw_heap(addr pos, addr *ret);
void ratio_throw_local(LocalRoot local, addr pos, addr *ret);
void ratio_throw_alloc(LocalRoot local, addr pos, addr *ret);
void ratio_result_noreduction_local(LocalRoot local, addr pos, addr *ret);
void ratio_result_noreduction_heap(LocalRoot local, addr pos, addr *ret);

int zerop_ratio(addr left);
int plusp_ratio(addr left);
int minusp_ratio(addr left);
int equal_value_nosign_ratio(addr pos, bigtype numer, bigtype denom);
int equal_value_ratio(addr pos, int sign, bigtype numer, bigtype denom);
int equal_fr_real(addr left, addr right);
int equal_br_real(addr left, addr right);
#define equal_rf_real(a,b) equal_fr_real((b),(a))
#define equal_rb_real(a,b) equal_br_real((b),(a))
int equal_rr_real(addr left, addr right);
int equal_rs_real(LocalRoot local, addr left, addr right);
int equal_rd_real(LocalRoot local, addr left, addr right);
int equal_rl_real(LocalRoot local, addr left, addr right);
#define equal_sr_real(m,a,b) equal_rs_real((m),(b),(a))
#define equal_dr_real(m,a,b) equal_rd_real((m),(b),(a))
#define equal_lr_real(m,a,b) equal_rl_real((m),(b),(a))

int compare_fr_real(LocalRoot local, addr left, addr right);
int compare_rf_real(LocalRoot local, addr left, addr right);
int compare_br_real(LocalRoot local, addr left, addr right);
int compare_rb_real(LocalRoot local, addr left, addr right);
int compare_rr_real(LocalRoot local, addr left, addr right);
int compare_rs_real(LocalRoot local, addr left, addr right);
int compare_rd_real(LocalRoot local, addr left, addr right);
int compare_rl_real(LocalRoot local, addr left, addr right);
int compare_sr_real(LocalRoot local, addr left, addr right);
int compare_dr_real(LocalRoot local, addr left, addr right);
int compare_lr_real(LocalRoot local, addr left, addr right);

/* cast float */
single_float single_float_ratio(addr pos);
double_float double_float_ratio(addr pos);
long_float long_float_ratio(addr pos);


/*
 *  ratio calculation
 */
void sign_reverse_ratio_inplace(addr left);
void sign_reverse_ratio_local(LocalRoot local, addr left, addr *ret);
void sign_reverse_ratio_common(addr left, addr *ret);

void plus_rv_ratio_local(LocalRoot local, addr left, fixnum right, addr *ret);
void plus_rv_real_local(LocalRoot local, addr left, fixnum right, addr *ret);
void plus_rv_real_common(LocalRoot local, addr left, fixnum right, addr *ret);
void plus_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_rf_real_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_rf_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define plus_fr_ratio_local(m,a,b,r) plus_rf_ratio_local((m),(b),(a),(r))
#define plus_fr_real_local(m,a,b,r) plus_rf_real_local((m),(b),(a),(r))
#define plus_fr_real_common(m,a,b,r) plus_rf_real_common((m),(b),(a),(r))
void minus_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_rf_real_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_rf_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_fr_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_fr_real_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_fr_real_common(LocalRoot local, addr left, addr right, addr *ret);
void plus_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_rb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_rb_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define plus_br_ratio_local(m,a,b,r) plus_rb_ratio_local((m),(b),(a),(r))
#define plus_br_real_local(m,a,b,r) plus_rb_real_local((m),(b),(a),(r))
#define plus_br_real_common(m,a,b,r) plus_rb_real_common((m),(b),(a),(r))
void minus_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_rb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_rb_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_br_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_br_real_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_br_real_common(LocalRoot local, addr left, addr right, addr *ret);
void plus_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_rr_real_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_rr_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_rr_real_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_rr_real_common(LocalRoot local, addr left, addr right, addr *ret);

void multi_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_rf_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_rf_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define multi_fr_ratio_local(m,a,b,r) multi_rf_ratio_local((m),(b),(a),(r))
#define multi_fr_real_local(m,a,b,r) multi_rf_real_local((m),(b),(a),(r))
#define multi_fr_real_common(m,a,b,r) multi_rf_real_common((m),(b),(a),(r))
void multi_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_rb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_rb_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define multi_br_ratio_local(m,a,b,r) multi_rb_ratio_local((m),(b),(a),(r))
#define multi_br_real_local(m,a,b,r) multi_rb_real_local((m),(b),(a),(r))
#define multi_br_real_common(m,a,b,r) multi_rb_real_common((m),(b),(a),(r))
void multi_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_rr_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_rr_real_common(LocalRoot local, addr left, addr right, addr *ret);

void div_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void div_rf_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_rf_real_common(LocalRoot local, addr left, addr right, addr *ret);
void div_fr_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void div_fr_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_fr_real_common(LocalRoot local, addr left, addr right, addr *ret);
void div_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void div_rb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_rb_real_common(LocalRoot local, addr left, addr right, addr *ret);
void div_br_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void div_br_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_br_real_common(LocalRoot local, addr left, addr right, addr *ret);
void div_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void div_rr_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_rr_real_common(LocalRoot local, addr left, addr right, addr *ret);

void div_ff_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void div_ff_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_ff_real_common(LocalRoot local, addr left, addr right, addr *ret);
void div_fb_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void div_fb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_fb_real_common(LocalRoot local, addr left, addr right, addr *ret);
void div_bf_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void div_bf_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_bf_real_common(LocalRoot local, addr left, addr right, addr *ret);
void div_bb_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void div_bb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_bb_real_common(LocalRoot local, addr left, addr right, addr *ret);

void inverse_fixnum_ratio_local(LocalRoot local, addr left, addr *ret);
void inverse_bignum_ratio_local(LocalRoot local, addr left, addr *ret);
void inverse_ratio_local(LocalRoot local, addr left, addr *ret);
void inverse_fixnum_common(addr left, addr *ret);
void inverse_bignum_common(addr left, addr *ret);
void inverse_ratio_common(LocalRoot local, addr left, addr *ret);
void inverse_integer_common(addr left, addr *ret);

void abs_ratio_alloc(LocalRoot local, addr left, addr *ret);
void abs_ratio_local(LocalRoot local, addr left, addr *ret);
void abs_ratio_heap(addr left, addr *ret);

void output_nosign_ratio(LocalRoot local,
		addr stream, addr pos, unsigned base, int upperp);

#endif

