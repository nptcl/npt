#ifndef __RATIONAL_HEADER__
#define __RATIONAL_HEADER__

#include "local.h"
#include "number.h"
#include "typedef.h"

_g int ratiop(addr pos);
_g int rationalp(addr pos);

_g void rational_result_local(LocalRoot local, addr pos, addr *ret);
_g void rational_result_heap(LocalRoot local, addr pos, addr *ret);
_g void rational_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void rational_throw_local(LocalRoot local, addr pos, addr *ret);
_g void rational_throw_heap(addr pos, addr *ret);
_g void rational_copy_alloc(LocalRoot local, addr pos, addr *ret);
_g void rational_copy_local(LocalRoot local, addr pos, addr *ret);
_g void rational_copy_heap(addr pos, addr *ret);

_g int plusp_rational(addr pos);
_g int minusp_rational(addr pos);
_g int zerop_rational(addr pos);
_g int equal_rational(addr left, addr right);
#define not_equal_rational(a,b) (! equal_rational((a), (b)))
_g int compare_rational(LocalRoot local, addr left, addr right);
#define less_rational(m,a,b) (compare_rational((m),(a), (b)) < 0)
#define less_equal_rational(m,a,b) (compare_rational((m),(a), (b)) <= 0)
#define greater_rational(m,a,b) (compare_rational((m),(a), (b)) > 0)
#define greater_equal_rational(m,a,b) (compare_rational((m),(a), (b)) >= 0)

_g single_float single_float_rational(addr pos);
_g double_float double_float_rational(addr pos);
_g long_float long_float_rational(addr pos);

_g int less_rational_clang(LocalRoot local, addr left, addr right);
_g int less_equal_rational_clang(LocalRoot local, addr left, addr right);
_g void sign_reverse_rational_common(addr pos, addr *ret);
_g void sign_reverse_rational_local(LocalRoot local, addr pos, addr *ret);

_g void oneplus_rational_common(LocalRoot local, addr value, addr *ret);
_g void oneminus_rational_common(LocalRoot local, addr value, addr *ret);

_g void plus_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_single_rational_common(addr left, addr right, addr *ret);
_g void plus_double_rational_common(addr left, addr right, addr *ret);
_g void plus_long_rational_common(addr left, addr right, addr *ret);
_g void plus_rational_common(LocalRoot local, addr left, addr right, addr *ret);
#define plus_rational_fixnum_common(m,a,b,r) \
	plus_fixnum_rational_common((m),(b),(a),(r))
#define plus_rational_bignum_common(m,a,b,r) \
	plus_bignum_rational_common((m),(b),(a),(r))
#define plus_rational_ratio_common(m,a,b,r) \
	plus_ratio_rational_common((m),(b),(a),(r))
#define plus_rational_single_common(a,b,r) \
	plus_single_rational_common((b),(a),(r))
#define plus_rational_double_common(a,b,r) \
	plus_double_rational_common((b),(a),(r))
#define plus_rational_long_common(a,b,r) \
	plus_long_rational_common((b),(a),(r))
_g void plus_rational_local(LocalRoot local, addr left, addr right, addr *ret);

_g void minus_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rational_fixnum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rational_bignum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rational_ratio_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_single_rational_common(addr left, addr right, addr *ret);
_g void minus_rational_single_common(addr left, addr right, addr *ret);
_g void minus_double_rational_common(addr left, addr right, addr *ret);
_g void minus_rational_double_common(addr left, addr right, addr *ret);
_g void minus_long_rational_common(addr left, addr right, addr *ret);
_g void minus_rational_long_common(addr left, addr right, addr *ret);
_g void minus_rational_local(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_rational_common(LocalRoot local, addr left, addr right, addr *ret);

_g void multi_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_single_rational_common(addr left, addr right, addr *ret);
_g void multi_double_rational_common(addr left, addr right, addr *ret);
_g void multi_long_rational_common(addr left, addr right, addr *ret);
#define multi_rational_fixnum_common(m,a,b,r) \
	multi_fixnum_rational_common((m),(b),(a),(r))
#define multi_rational_bignum_common(m,a,b,r) \
	multi_bignum_rational_common((m),(b),(a),(r))
#define multi_rational_ratio_common(m,a,b,r) \
	multi_ratio_rational_common((m),(b),(a),(r))
#define multi_rational_single_common(a,b,r) \
	multi_single_rational_common((b),(a),(r))
#define multi_rational_double_common(a,b,r) \
	multi_double_rational_common((b),(a),(r))
#define multi_rational_long_common(a,b,r) \
	multi_long_rational_common((b),(a),(r))
_g void multi_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_fixnum_rational_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bignum_rational_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_ratio_rational_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_rational_local(LocalRoot local, addr left, addr right, addr *ret);

_g void div_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rational_fixnum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rational_bignum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rational_ratio_common(LocalRoot local, addr left, addr right, addr *ret);

_g void div_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rational_fixnum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rational_bignum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_rational_ratio_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_single_rational_common(addr left, addr right, addr *ret);
_g void div_rational_single_common(addr left, addr right, addr *ret);
_g void div_double_rational_common(addr left, addr right, addr *ret);
_g void div_rational_double_common(addr left, addr right, addr *ret);
_g void div_long_rational_common(addr left, addr right, addr *ret);
_g void div_rational_long_common(addr left, addr right, addr *ret);
_g void div_rational_local(LocalRoot local, addr left, addr right, addr *ret);

_g void inverse_rational_common(LocalRoot local, addr pos, addr *ret);

_g void numerator_common(addr pos, addr *ret);
_g void denominator_common(addr pos, addr *ret);

#endif

