#ifndef __BIGNUM_EQUAL_HEADER__
#define __BIGNUM_EQUAL_HEADER__

#include "typedef.h"

#define zerop_or_plusp_bignum _n(zerop_or_plusp_bignum)
#define plusp_bignum _n(plusp_bignum)
#define minusp_bignum _n(minusp_bignum)
#define zerop_bignum _n(zerop_bignum)
#define evenp_bignum _n(evenp_bignum)
#define oddp_bignum _n(oddp_bignum)
#define equal_bb_real _n(equal_bb_real)
#define equal_nosign_bignum _n(equal_nosign_bignum)
#define equal_fb_real _n(equal_fb_real)
#define equal_value_nosign_bignum _n(equal_value_nosign_bignum)
#define equal_value_bignum _n(equal_value_bignum)
#define equal_value2_nosign_bignum _n(equal_value2_nosign_bignum)
#define equal_value2_bignum _n(equal_value2_bignum)
#define compare_value_bignum _n(compare_value_bignum)
#define compare_bignum_value _n(compare_bignum_value)
#define compare_fb_real _n(compare_fb_real)
#define compare_bf_real _n(compare_bf_real)
#define compare_bb_real _n(compare_bb_real)
#define compare_bs_real_ _n(compare_bs_real_)
#define compare_bd_real_ _n(compare_bd_real_)
#define compare_bl_real_ _n(compare_bl_real_)
#define compare_sb_real_ _n(compare_sb_real_)
#define compare_db_real_ _n(compare_db_real_)
#define compare_lb_real_ _n(compare_lb_real_)
#define fixnum_unsigned_byte_p _n(fixnum_unsigned_byte_p)
#define bignum_unsigned_byte_p _n(bignum_unsigned_byte_p)
#define fixnum_signed_byte_p _n(fixnum_signed_byte_p)
#define bignum_signed_byte_p _n(bignum_signed_byte_p)

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

_g int fixnum_unsigned_byte_p(addr value, size_t size);
_g int bignum_unsigned_byte_p(addr value, size_t size);
_g int fixnum_signed_byte_p(addr value, size_t size);
_g int bignum_signed_byte_p(addr value, size_t size);

#endif

