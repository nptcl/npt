#ifndef __REAL_DIVISION_HEADER__
#define __REAL_DIVISION_HEADER__

#include "local.h"
#include "typedef.h"

#define float_floor_s_ _n(float_floor_s_)
#define float_floor_d_ _n(float_floor_d_)
#define float_floor_l_ _n(float_floor_l_)
#define float_floor1_s_ _n(float_floor1_s_)
#define float_floor1_d_ _n(float_floor1_d_)
#define float_floor1_l_ _n(float_floor1_l_)
#define float_ceiling_s_ _n(float_ceiling_s_)
#define float_ceiling_d_ _n(float_ceiling_d_)
#define float_ceiling_l_ _n(float_ceiling_l_)
#define float_ceiling1_s_ _n(float_ceiling1_s_)
#define float_ceiling1_d_ _n(float_ceiling1_d_)
#define float_ceiling1_l_ _n(float_ceiling1_l_)
#define float_truncate_s_ _n(float_truncate_s_)
#define float_truncate_d_ _n(float_truncate_d_)
#define float_truncate_l_ _n(float_truncate_l_)
#define float_truncate1_s_ _n(float_truncate1_s_)
#define float_truncate1_d_ _n(float_truncate1_d_)
#define float_truncate1_l_ _n(float_truncate1_l_)
#define float_round_s_ _n(float_round_s_)
#define float_round_d_ _n(float_round_d_)
#define float_round_l_ _n(float_round_l_)
#define float_round1_s_ _n(float_round1_s_)
#define float_round1_d_ _n(float_round1_d_)
#define float_round1_l_ _n(float_round1_l_)
#define float_floor_f_ _n(float_floor_f_)
#define float_ceiling_f_ _n(float_ceiling_f_)
#define float_truncate_f_ _n(float_truncate_f_)
#define float_round_f_ _n(float_round_f_)
#define float_floor_fixnum_ _n(float_floor_fixnum_)
#define float_ceiling_fixnum_ _n(float_ceiling_fixnum_)
#define float_truncate_fixnum_ _n(float_truncate_fixnum_)
#define float_round_fixnum_ _n(float_round_fixnum_)
#define float_ffloor_fixnum_ _n(float_ffloor_fixnum_)
#define float_fceiling_fixnum_ _n(float_fceiling_fixnum_)
#define float_ftruncate_fixnum_ _n(float_ftruncate_fixnum_)
#define float_fround_fixnum_ _n(float_fround_fixnum_)
#define float_floor_bignum_ _n(float_floor_bignum_)
#define float_ceiling_bignum_ _n(float_ceiling_bignum_)
#define float_truncate_bignum_ _n(float_truncate_bignum_)
#define float_round_bignum_ _n(float_round_bignum_)
#define float_ffloor_bignum_ _n(float_ffloor_bignum_)
#define float_fceiling_bignum_ _n(float_fceiling_bignum_)
#define float_ftruncate_bignum_ _n(float_ftruncate_bignum_)
#define float_fround_bignum_ _n(float_fround_bignum_)
#define float_floor1_ratio_ _n(float_floor1_ratio_)
#define float_ceiling1_ratio_ _n(float_ceiling1_ratio_)
#define float_truncate1_ratio_ _n(float_truncate1_ratio_)
#define float_round1_ratio_ _n(float_round1_ratio_)
#define float_ffloor1_ratio_ _n(float_ffloor1_ratio_)
#define float_fceiling1_ratio_ _n(float_fceiling1_ratio_)
#define float_ftruncate1_ratio_ _n(float_ftruncate1_ratio_)
#define float_fround1_ratio_ _n(float_fround1_ratio_)
#define float_floor_br_ratio_ _n(float_floor_br_ratio_)
#define float_ceiling_br_ratio_ _n(float_ceiling_br_ratio_)
#define float_truncate_br_ratio_ _n(float_truncate_br_ratio_)
#define float_round_br_ratio_ _n(float_round_br_ratio_)
#define float_ffloor_br_ratio_ _n(float_ffloor_br_ratio_)
#define float_fceiling_br_ratio_ _n(float_fceiling_br_ratio_)
#define float_ftruncate_br_ratio_ _n(float_ftruncate_br_ratio_)
#define float_fround_br_ratio_ _n(float_fround_br_ratio_)
#define float_floor_rb_ratio_ _n(float_floor_rb_ratio_)
#define float_ceiling_rb_ratio_ _n(float_ceiling_rb_ratio_)
#define float_truncate_rb_ratio_ _n(float_truncate_rb_ratio_)
#define float_round_rb_ratio_ _n(float_round_rb_ratio_)
#define float_ffloor_rb_ratio_ _n(float_ffloor_rb_ratio_)
#define float_fceiling_rb_ratio_ _n(float_fceiling_rb_ratio_)
#define float_ftruncate_rb_ratio_ _n(float_ftruncate_rb_ratio_)
#define float_fround_rb_ratio_ _n(float_fround_rb_ratio_)
#define float_floor_rr_ratio_ _n(float_floor_rr_ratio_)
#define float_ceiling_rr_ratio_ _n(float_ceiling_rr_ratio_)
#define float_truncate_rr_ratio_ _n(float_truncate_rr_ratio_)
#define float_round_rr_ratio_ _n(float_round_rr_ratio_)
#define float_ffloor_rr_ratio_ _n(float_ffloor_rr_ratio_)
#define float_fceiling_rr_ratio_ _n(float_fceiling_rr_ratio_)
#define float_ftruncate_rr_ratio_ _n(float_ftruncate_rr_ratio_)
#define float_fround_rr_ratio_ _n(float_fround_rr_ratio_)
#define float_mod_fixnum_ _n(float_mod_fixnum_)
#define float_mod_bignum_ _n(float_mod_bignum_)
#define float_mod_br_ratio_ _n(float_mod_br_ratio_)
#define float_mod_rb_ratio_ _n(float_mod_rb_ratio_)
#define float_mod_rr_ratio_ _n(float_mod_rr_ratio_)
#define mod_number_common_ _n(mod_number_common_)
#define float_rem_fixnum_ _n(float_rem_fixnum_)
#define float_rem_bignum_ _n(float_rem_bignum_)
#define float_rem_br_ratio_ _n(float_rem_br_ratio_)
#define float_rem_rb_ratio_ _n(float_rem_rb_ratio_)
#define float_rem_rr_ratio_ _n(float_rem_rr_ratio_)
#define rem_number_common_ _n(rem_number_common_)
#define single_float_integer_heap _n(single_float_integer_heap)
#define double_float_integer_heap _n(double_float_integer_heap)
#define long_float_integer_heap _n(long_float_integer_heap)

/*
 *  float
 */
int float_floor_s_(single_float a, single_float b, single_float *q, single_float *r);
int float_floor_d_(double_float a, double_float b, double_float *q, double_float *r);
int float_floor_l_(long_float a, long_float b, long_float *q, long_float *r);

int float_floor1_s_(single_float a, single_float *q, single_float *r);
int float_floor1_d_(double_float a, double_float *q, double_float *r);
int float_floor1_l_(long_float a, long_float *q, long_float *r);

int float_ceiling_s_(single_float a, single_float b, single_float *q, single_float *r);
int float_ceiling_d_(double_float a, double_float b, double_float *q, double_float *r);
int float_ceiling_l_(long_float a, long_float b, long_float *q, long_float *r);

int float_ceiling1_s_(single_float a, single_float *q, single_float *r);
int float_ceiling1_d_(double_float a, double_float *q, double_float *r);
int float_ceiling1_l_(long_float a, long_float *q, long_float *r);

int float_truncate_s_(single_float a, single_float b,
		single_float *q, single_float *r);
int float_truncate_d_(double_float a, double_float b,
		double_float *q, double_float *r);
int float_truncate_l_(long_float a, long_float b,
		long_float *q, long_float *r);

int float_truncate1_s_(single_float a, single_float *q, single_float *r);
int float_truncate1_d_(double_float a, double_float *q, double_float *r);
int float_truncate1_l_(long_float a, long_float *q, long_float *r);

int float_round_s_(single_float a, single_float b, single_float *q, single_float *r);
int float_round_d_(double_float a, double_float b, double_float *q, double_float *r);
int float_round_l_(long_float a, long_float b, long_float *q, long_float *r);

int float_round1_s_(single_float a, single_float *q, single_float *r);
int float_round1_d_(double_float a, double_float *q, double_float *r);
int float_round1_l_(long_float a, long_float *q, long_float *r);


/*
 *  fixnum
 */
int float_floor_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem);
int float_ceiling_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem);
int float_truncate_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem);
int float_round_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem);
int float_floor_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);
int float_ceiling_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);
int float_truncate_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);
int float_round_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);

int float_ffloor_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);
int float_fceiling_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);
int float_ftruncate_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);
int float_fround_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);


/*
 *  bignum
 */
int float_floor_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_ceiling_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_truncate_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_round_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);

int float_ffloor_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_fceiling_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_ftruncate_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_fround_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);


/*
 *  ratio
 */
int float_floor1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);
int float_ceiling1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);
int float_truncate1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);
int float_round1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);

int float_ffloor1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);
int float_fceiling1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);
int float_ftruncate1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);
int float_fround1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);

int float_floor_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_ceiling_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_truncate_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_round_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_ffloor_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_fceiling_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_ftruncate_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_fround_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);

int float_floor_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_ceiling_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_truncate_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_round_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_ffloor_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_fceiling_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_ftruncate_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_fround_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);

int float_floor_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_ceiling_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_truncate_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_round_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_ffloor_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_fceiling_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_ftruncate_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
int float_fround_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);


/*
 *  mod, rem
 */
int float_mod_fixnum_(addr *ret, fixnum a, fixnum b);
int float_mod_bignum_(LocalRoot local, addr *ret, addr a, addr b);
int float_mod_br_ratio_(LocalRoot local, addr *ret, addr a, addr b);
int float_mod_rb_ratio_(LocalRoot local, addr *ret, addr a, addr b);
int float_mod_rr_ratio_(LocalRoot local, addr *ret, addr a, addr b);
int mod_number_common_(LocalRoot local, addr a, addr b, addr *ret);

int float_rem_fixnum_(addr *ret, fixnum a, fixnum b);
int float_rem_bignum_(LocalRoot local, addr *ret, addr a, addr b);
int float_rem_br_ratio_(LocalRoot local, addr *ret, addr a, addr b);
int float_rem_rb_ratio_(LocalRoot local, addr *ret, addr a, addr b);
int float_rem_rr_ratio_(LocalRoot local, addr *ret, addr a, addr b);
int rem_number_common_(LocalRoot local, addr a, addr b, addr *ret);


/*
 *  integer-heap
 */
void single_float_integer_heap(LocalRoot local, addr *ret, single_float v);
void double_float_integer_heap(LocalRoot local, addr *ret, double_float v);
void long_float_integer_heap(LocalRoot local, addr *ret, long_float v);

#endif

