#ifndef __CMPL_HEADER__
#define __CMPL_HEADER__

#include "local.h"
#include "typedef.h"

#define GetRealComplex(x,y) GetArrayA2((x), 0, (y))
#define SetRealComplex(x,y) SetArrayA2((x), 0, (y))
#define GetImagComplex(x,y) GetArrayA2((x), 1, (y))
#define SetImagComplex(x,y) SetArrayA2((x), 1, (y))

void make_complex_alloc(LocalRoot local, addr *ret);
void complex_alloc(LocalRoot local, addr *ret, addr real, addr imag);
void complex_local(LocalRoot local, addr *ret, addr real, addr imag);
void complex_heap(addr *ret, addr real, addr imag);
void complex_copy_alloc(LocalRoot local, addr *ret, addr pos);
void complex_copy_local(LocalRoot local, addr *ret, addr pos);
void complex_copy_heap(addr *ret, addr pos);
int complexp(addr pos);

int complex_result_alloc(LocalRoot local, addr pos, addr *ret);
void complex_throw_alloc(LocalRoot local, addr pos, addr *ret);
void complex_throw_local(LocalRoot local, addr pos, addr *ret);
void complex_throw_heap(addr pos, addr *ret);

int zerop_complex(addr pos);
int eql_complex(addr left, addr right);
int equal_complex(LocalRoot local, addr left, addr right);
#define not_equal_complex(m,a,b) (! equal_complex((m),(a),(b)))
int equal_fc_number(addr left, addr right);
int equal_bc_number(addr left, addr right);
int equal_rc_number(LocalRoot local, addr left, addr right);
int equal_sc_number(LocalRoot local, addr left, addr right);
int equal_dc_number(LocalRoot local, addr left, addr right);
int equal_lc_number(LocalRoot local, addr left, addr right);
#define equal_cf_number(a,b) equal_fc_number((b),(a))
#define equal_cb_number(a,b) equal_bc_number((b),(a))
#define equal_cr_number(m,a,b) equal_rc_number((m),(b),(a))
#define equal_cs_number(m,a,b) equal_sc_number((m),(b),(a))
#define equal_cd_number(m,a,b) equal_dc_number((m),(b),(a))
#define equal_cl_number(m,a,b) equal_lc_number((m),(b),(a))

void oneplus_complex_heap(LocalRoot local, addr pos, addr *ret);
void oneminus_complex_heap(LocalRoot local, addr pos, addr *ret);
void sign_reverse_complex_common(addr pos, addr *ret);
void sign_reverse_complex_local(LocalRoot local, addr pos, addr *ret);

void plus_fc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void plus_bc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void plus_rc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void plus_sc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void plus_dc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void plus_lc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void plus_cc_number_common(LocalRoot local, addr left, addr right, addr *ret);
#define plus_cf_number_common(m,a,b,r) plus_fc_number_common((m),(b),(a),(r))
#define plus_cb_number_common(m,a,b,r) plus_bc_number_common((m),(b),(a),(r))
#define plus_cr_number_common(m,a,b,r) plus_rc_number_common((m),(b),(a),(r))
#define plus_cs_number_common(m,a,b,r) plus_sc_number_common((m),(b),(a),(r))
#define plus_cd_number_common(m,a,b,r) plus_dc_number_common((m),(b),(a),(r))
#define plus_cl_number_common(m,a,b,r) plus_lc_number_common((m),(b),(a),(r))

void minus_fc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_cf_number_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_bc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_cb_number_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_rc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_cr_number_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_sc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_cs_number_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_dc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_cd_number_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_lc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_cl_number_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_cc_number_common(LocalRoot local, addr left, addr right, addr *ret);

void multi_fc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void multi_bc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void multi_rc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void multi_sc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void multi_dc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void multi_lc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void multi_cc_number_common(LocalRoot local, addr left, addr right, addr *ret);
#define multi_cf_number_common(m,a,b,r) multi_fc_number_common((m),(b),(a),(r))
#define multi_cb_number_common(m,a,b,r) multi_bc_number_common((m),(b),(a),(r))
#define multi_cr_number_common(m,a,b,r) multi_rc_number_common((m),(b),(a),(r))
#define multi_cs_number_common(m,a,b,r) multi_sc_number_common((m),(b),(a),(r))
#define multi_cd_number_common(m,a,b,r) multi_dc_number_common((m),(b),(a),(r))
#define multi_cl_number_common(m,a,b,r) multi_lc_number_common((m),(b),(a),(r))

void inverse_complex_common(LocalRoot local, addr pos, addr *ret);
void div_fc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void div_cf_number_common(LocalRoot local, addr left, addr right, addr *ret);
void div_bc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void div_cb_number_common(LocalRoot local, addr left, addr right, addr *ret);
void div_rc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void div_cr_number_common(LocalRoot local, addr left, addr right, addr *ret);
void div_sc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void div_cs_number_common(LocalRoot local, addr left, addr right, addr *ret);
void div_dc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void div_cd_number_common(LocalRoot local, addr left, addr right, addr *ret);
void div_lc_number_common(LocalRoot local, addr left, addr right, addr *ret);
void div_cl_number_common(LocalRoot local, addr left, addr right, addr *ret);
void div_cc_number_common(LocalRoot local, addr left, addr right, addr *ret);

void abs_complex_common(LocalRoot local, addr left, addr *ret);

#endif

