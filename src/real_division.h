#ifndef __REAL_DIVISION_HEADER__
#define __REAL_DIVISION_HEADER__

#include "local.h"
#include "typedef.h"

/*
 *  float
 */
_g single_float lisp_floor_s(single_float a, single_float b, single_float *rem);
_g double_float lisp_floor_d(double_float a, double_float b, double_float *rem);
_g long_float lisp_floor_l(long_float a, long_float b, long_float *rem);

_g single_float lisp_floor1_s(single_float a, single_float *rem);
_g double_float lisp_floor1_d(double_float a, double_float *rem);
_g long_float lisp_floor1_l(long_float a, long_float *rem);

_g single_float lisp_ceiling_s(single_float a, single_float b, single_float *rem);
_g double_float lisp_ceiling_d(double_float a, double_float b, double_float *rem);
_g long_float lisp_ceiling_l(long_float a, long_float b, long_float *rem);

_g single_float lisp_ceiling1_s(single_float a, single_float *rem);
_g double_float lisp_ceiling1_d(double_float a, double_float *rem);
_g long_float lisp_ceiling1_l(long_float a, long_float *rem);

_g single_float lisp_truncate_s(single_float a, single_float b, single_float *rem);
_g double_float lisp_truncate_d(double_float a, double_float b, double_float *rem);
_g long_float lisp_truncate_l(long_float a, long_float b, long_float *rem);

_g single_float lisp_truncate1_s(single_float a, single_float *rem);
_g double_float lisp_truncate1_d(double_float a, double_float *rem);
_g long_float lisp_truncate1_l(long_float a, long_float *rem);

_g single_float lisp_round_s(single_float a, single_float b, single_float *rem);
_g double_float lisp_round_d(double_float a, double_float b, double_float *rem);
_g long_float lisp_round_l(long_float a, long_float b, long_float *rem);

_g single_float lisp_round1_s(single_float a, single_float *rem);
_g double_float lisp_round1_d(double_float a, double_float *rem);
_g long_float lisp_round1_l(long_float a, long_float *rem);


/*
 *  fixnum
 */
_g fixnum lisp_floor_f(fixnum a, fixnum b, fixnum *rem);
_g fixnum lisp_ceiling_f(fixnum a, fixnum b, fixnum *rem);
_g fixnum lisp_truncate_f(fixnum a, fixnum b, fixnum *rem);
_g fixnum lisp_round_f(fixnum a, fixnum b, fixnum *rem);
_g void lisp_floor_fixnum(addr *quot, addr *rem, fixnum a, fixnum b);
_g void lisp_ceiling_fixnum(addr *quot, addr *rem, fixnum a, fixnum b);
_g void lisp_truncate_fixnum(addr *quot, addr *rem, fixnum a, fixnum b);
_g void lisp_round_fixnum(addr *quot, addr *rem, fixnum a, fixnum b);

_g void lisp_ffloor_fixnum(addr *quot, addr *rem, fixnum a, fixnum b);
_g void lisp_fceiling_fixnum(addr *quot, addr *rem, fixnum a, fixnum b);
_g void lisp_ftruncate_fixnum(addr *quot, addr *rem, fixnum a, fixnum b);
_g void lisp_fround_fixnum(addr *quot, addr *rem, fixnum a, fixnum b);


/*
 *  bignum
 */
_g void lisp_floor_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_ceiling_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_truncate_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_round_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b);

_g void lisp_ffloor_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_fceiling_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_ftruncate_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_fround_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b);


/*
 *  ratio
 */
_g void lisp_floor1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos);
_g void lisp_ceiling1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos);
_g void lisp_truncate1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos);
_g void lisp_round1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos);

_g void lisp_ffloor1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos);
_g void lisp_fceiling1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos);
_g void lisp_ftruncate1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos);
_g void lisp_fround1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos);

_g void lisp_floor_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_ceiling_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_truncate_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_round_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_ffloor_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_fceiling_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_ftruncate_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_fround_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);

_g void lisp_floor_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_ceiling_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_truncate_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_round_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_ffloor_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_fceiling_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_ftruncate_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_fround_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);

_g void lisp_floor_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_ceiling_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_truncate_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_round_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_ffloor_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_fceiling_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_ftruncate_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g void lisp_fround_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b);


/*
 *  mod, rem
 */
_g void lisp_mod_fixnum(addr *ret, fixnum a, fixnum b);
_g void lisp_rem_fixnum(addr *ret, fixnum a, fixnum b);
_g void lisp_mod_bignum(LocalRoot local, addr *ret, addr a, addr b);
_g void lisp_mod_br_ratio(LocalRoot local, addr *ret, addr a, addr b);
_g void lisp_mod_rb_ratio(LocalRoot local, addr *rem, addr a, addr b);
_g void lisp_mod_rr_ratio(LocalRoot local, addr *ret, addr a, addr b);
_g int mod_number_common_(LocalRoot local, addr a, addr b, addr *ret);

_g void lisp_rem_fixnum(addr *ret, fixnum a, fixnum b);
_g void lisp_rem_bignum(LocalRoot local, addr *ret, addr a, addr b);
_g void lisp_rem_br_ratio(LocalRoot local, addr *ret, addr a, addr b);
_g void lisp_rem_rb_ratio(LocalRoot local, addr *ret, addr a, addr b);
_g void lisp_rem_rr_ratio(LocalRoot local, addr *rem, addr a, addr b);
_g int rem_number_common_(LocalRoot local, addr a, addr b, addr *ret);


/*
 *  integer-heap
 */
_g void single_float_integer_heap(LocalRoot local, addr *ret, single_float v);
_g void double_float_integer_heap(LocalRoot local, addr *ret, double_float v);
_g void long_float_integer_heap(LocalRoot local, addr *ret, long_float v);

#endif

