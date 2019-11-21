#ifndef __REAL_HEADER__
#define __REAL_HEADER__

#include "build.h"
#include "typedef.h"

_g void build_real(void);
_g int floatp(addr pos);
_g int realp(addr pos);

_g void real_result_local(LocalRoot local, addr pos, addr *ret);
_g void real_result_heap(LocalRoot local, addr pos, addr *ret);
_g void real_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void real_throw_local(LocalRoot local, addr pos, addr *ret);
_g void real_throw_heap(addr pos, addr *ret);
_g void real_copy_alloc(LocalRoot local, addr pos, addr *ret);
_g void real_copy_local(LocalRoot local, addr pos, addr *ret);
_g void real_copy_heap(addr pos, addr *ret);

_g int plusp_real(addr pos);
_g int minusp_real(addr pos);
_g int zerop_real(addr pos);

_g int equal_fixnum_real(addr left, addr right);
_g int equal_bignum_real(addr left, addr right);
_g int equal_ratio_real(LocalRoot local, addr left, addr right);
_g int equal_single_float_real(LocalRoot local, addr left, addr right);
_g int equal_double_float_real(LocalRoot local, addr left, addr right);
_g int equal_long_float_real(LocalRoot local, addr left, addr right);
_g int equal_real(LocalRoot local, addr left, addr right);
#define not_equal_real(a,b) (! equal_real((a), (b)))
_g int compare_ratio_real(LocalRoot local, addr left, addr right);
_g int compare_real(LocalRoot local, addr left, addr right);
#define less_real(m,a,b) (compare_real((m),(a), (b)) < 0)
#define less_equal_real(m,a,b) (compare_real((m),(a), (b)) <= 0)
#define greater_real(m,a,b) (compare_real((m),(a), (b)) > 0)
#define greater_equal_real(m,a,b) (compare_real((m),(a), (b)) >= 0)

_g int less_real_clang(LocalRoot local, addr left, addr right);
_g int less_equal_real_clang(LocalRoot local, addr left, addr right);
_g int greater_real_clang(LocalRoot local, addr left, addr right);
_g int greater_equal_real_clang(LocalRoot local, addr left, addr right);

_g double_float cast_double_float_unsafe(addr value);

_g void sign_reverse_real_common(addr pos, addr *ret);
_g void sign_reverse_real_local(LocalRoot local, addr pos, addr *ret);

_g void oneplus_real_common(LocalRoot local, addr value, addr *ret);
_g void oneminus_real_common(LocalRoot local, addr value, addr *ret);

_g void plus_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_single_real_common(addr left, addr right, addr *ret);
_g void plus_double_real_common(addr left, addr right, addr *ret);
_g void plus_long_real_common(addr left, addr right, addr *ret);
_g void plus_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_real_local(LocalRoot local, addr left, addr right, addr *ret);

_g void minus_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_fixnum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_bignum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_ratio_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_single_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_single_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_double_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_double_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_long_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_long_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_real_local(LocalRoot local, addr left, addr right, addr *ret);

_g void multi_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_single_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_double_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_long_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_real_common(LocalRoot local, addr left, addr right, addr *ret);

_g void multi_fixnum_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bignum_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_ratio_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_single_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_double_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_long_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_real_local(LocalRoot local, addr left, addr right, addr *ret);

_g void div_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_fixnum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_bignum_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_ratio_common(LocalRoot local, addr left, addr right, addr *ret);
_g void div_single_real_common(addr left, addr right, addr *ret);
_g void div_real_single_common(addr left, addr right, addr *ret);
_g void div_double_real_common(addr left, addr right, addr *ret);
_g void div_real_double_common(addr left, addr right, addr *ret);
_g void div_long_real_common(addr left, addr right, addr *ret);
_g void div_real_long_common(addr left, addr right, addr *ret);
_g void div_real_common(LocalRoot local, addr left, addr right, addr *ret);

_g void div_fixnum_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_fixnum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_bignum_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_ratio_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_single_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_single_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_double_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_double_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_long_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_long_local(LocalRoot local, addr left, addr right, addr *ret);
_g void div_real_local(LocalRoot local, addr left, addr right, addr *ret);

#endif

