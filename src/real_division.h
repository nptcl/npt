#ifndef __REAL_DIVISION_HEADER__
#define __REAL_DIVISION_HEADER__

#include "local.h"
#include "typedef.h"

/*
 *  float
 */
_g int lisp_floor_s_(single_float a, single_float b, single_float *q, single_float *r);
_g int lisp_floor_d_(double_float a, double_float b, double_float *q, double_float *r);
_g int lisp_floor_l_(long_float a, long_float b, long_float *q, long_float *r);

_g int lisp_floor1_s_(single_float a, single_float *q, single_float *r);
_g int lisp_floor1_d_(double_float a, double_float *q, double_float *r);
_g int lisp_floor1_l_(long_float a, long_float *q, long_float *r);

_g int lisp_ceiling_s_(single_float a, single_float b, single_float *q, single_float *r);
_g int lisp_ceiling_d_(double_float a, double_float b, double_float *q, double_float *r);
_g int lisp_ceiling_l_(long_float a, long_float b, long_float *q, long_float *r);

_g int lisp_ceiling1_s_(single_float a, single_float *q, single_float *r);
_g int lisp_ceiling1_d_(double_float a, double_float *q, double_float *r);
_g int lisp_ceiling1_l_(long_float a, long_float *q, long_float *r);

_g int lisp_truncate_s_(single_float a, single_float b,
		single_float *q, single_float *r);
_g int lisp_truncate_d_(double_float a, double_float b,
		double_float *q, double_float *r);
_g int lisp_truncate_l_(long_float a, long_float b,
		long_float *q, long_float *r);

_g int lisp_truncate1_s_(single_float a, single_float *q, single_float *r);
_g int lisp_truncate1_d_(double_float a, double_float *q, double_float *r);
_g int lisp_truncate1_l_(long_float a, long_float *q, long_float *r);

_g int lisp_round_s_(single_float a, single_float b, single_float *q, single_float *r);
_g int lisp_round_d_(double_float a, double_float b, double_float *q, double_float *r);
_g int lisp_round_l_(long_float a, long_float b, long_float *q, long_float *r);

_g int lisp_round1_s_(single_float a, single_float *q, single_float *r);
_g int lisp_round1_d_(double_float a, double_float *q, double_float *r);
_g int lisp_round1_l_(long_float a, long_float *q, long_float *r);


/*
 *  fixnum
 */
_g int lisp_floor_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem);
_g int lisp_ceiling_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem);
_g int lisp_truncate_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem);
_g int lisp_round_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem);
_g int lisp_floor_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);
_g int lisp_ceiling_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);
_g int lisp_truncate_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);
_g int lisp_round_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);

_g int lisp_ffloor_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);
_g int lisp_fceiling_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);
_g int lisp_ftruncate_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);
_g int lisp_fround_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b);


/*
 *  bignum
 */
_g int lisp_floor_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_ceiling_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_truncate_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_round_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);

_g int lisp_ffloor_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_fceiling_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_ftruncate_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_fround_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);


/*
 *  ratio
 */
_g int lisp_floor1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);
_g int lisp_ceiling1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);
_g int lisp_truncate1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);
_g int lisp_round1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);

_g int lisp_ffloor1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);
_g int lisp_fceiling1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);
_g int lisp_ftruncate1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);
_g int lisp_fround1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos);

_g int lisp_floor_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_ceiling_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_truncate_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_round_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_ffloor_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_fceiling_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_ftruncate_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_fround_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);

_g int lisp_floor_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_ceiling_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_truncate_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_round_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_ffloor_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_fceiling_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_ftruncate_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_fround_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);

_g int lisp_floor_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_ceiling_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_truncate_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_round_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_ffloor_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_fceiling_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_ftruncate_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);
_g int lisp_fround_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b);


/*
 *  mod, rem
 */
_g int lisp_mod_fixnum_(addr *ret, fixnum a, fixnum b);
_g int lisp_mod_bignum_(LocalRoot local, addr *ret, addr a, addr b);
_g int lisp_mod_br_ratio_(LocalRoot local, addr *ret, addr a, addr b);
_g int lisp_mod_rb_ratio_(LocalRoot local, addr *ret, addr a, addr b);
_g int lisp_mod_rr_ratio_(LocalRoot local, addr *ret, addr a, addr b);
_g int mod_number_common_(LocalRoot local, addr a, addr b, addr *ret);

_g int lisp_rem_fixnum_(addr *ret, fixnum a, fixnum b);
_g int lisp_rem_bignum_(LocalRoot local, addr *ret, addr a, addr b);
_g int lisp_rem_br_ratio_(LocalRoot local, addr *ret, addr a, addr b);
_g int lisp_rem_rb_ratio_(LocalRoot local, addr *ret, addr a, addr b);
_g int lisp_rem_rr_ratio_(LocalRoot local, addr *ret, addr a, addr b);
_g int rem_number_common_(LocalRoot local, addr a, addr b, addr *ret);


/*
 *  integer-heap
 */
_g void single_float_integer_heap(LocalRoot local, addr *ret, single_float v);
_g void double_float_integer_heap(LocalRoot local, addr *ret, double_float v);
_g void long_float_integer_heap(LocalRoot local, addr *ret, long_float v);

#endif

