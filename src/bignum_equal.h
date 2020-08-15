#ifndef __BIGNUM_EQUAL_HEADER__
#define __BIGNUM_EQUAL_HEADER__

#include "typedef.h"

_g int zerop_or_plusp_bignum(addr pos);
_g int plusp_bignum(addr pos);
_g int minusp_bignum(addr pos);
_g int zerop_bignum(addr pos);
_g int evenp_bignum(addr pos);
_g int oddp_bignum(addr pos);

_g int equal_bb_real(addr left, addr right);
_g int equal_nosign_bignum(addr left, addr right);
_g int equal_fb_real(addr left, addr right);
#define equal_bf_real(a,b) equal_fb_real((b), (a))
_g int equal_value_nosign_bignum(addr left, bigtype value);
_g int equal_value_bignum(addr left, int sign1, bigtype value);
_g int equal_value2_nosign_bignum(addr left, bigtype high, bigtype low);
_g int equal_value2_bignum(addr left, int sign1, bigtype high, bigtype low);

_g int compare_value_bignum(fixnum left, addr right);
_g int compare_bignum_value(addr value, fixnum right);
_g int compare_fb_real(addr left, addr right);
_g int compare_bf_real(addr left, addr right);
_g int compare_bb_real(addr left, addr right);
_g int compare_bs_real_(addr left, addr right, int *ret);
_g int compare_bd_real_(addr left, addr right, int *ret);
_g int compare_bl_real_(addr left, addr right, int *ret);
_g int compare_sb_real_(addr left, addr right, int *ret);
_g int compare_db_real_(addr left, addr right, int *ret);
_g int compare_lb_real_(addr left, addr right, int *ret);

#endif

