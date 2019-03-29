#ifndef __REAL_HEADER__
#define __REAL_HEADER__

#include "build.h"
#include "typedef.h"

void build_real(void);
int floatp(addr pos);
int realp(addr pos);

void real_result_local(LocalRoot local, addr pos, addr *ret);
void real_result_heap(LocalRoot local, addr pos, addr *ret);
void real_throw_alloc(LocalRoot local, addr pos, addr *ret);
void real_throw_local(LocalRoot local, addr pos, addr *ret);
void real_throw_heap(addr pos, addr *ret);
void real_copy_alloc(LocalRoot local, addr pos, addr *ret);
void real_copy_local(LocalRoot local, addr pos, addr *ret);
void real_copy_heap(addr pos, addr *ret);

int plusp_real(addr pos);
int minusp_real(addr pos);
int zerop_real(addr pos);

int equal_fixnum_real(addr left, addr right);
int equal_bignum_real(addr left, addr right);
int equal_ratio_real(LocalRoot local, addr left, addr right);
int equal_single_float_real(LocalRoot local, addr left, addr right);
int equal_double_float_real(LocalRoot local, addr left, addr right);
int equal_long_float_real(LocalRoot local, addr left, addr right);
int equal_real(LocalRoot local, addr left, addr right);
#define not_equal_real(a,b) (! equal_real((a), (b)))
int compare_ratio_real(LocalRoot local, addr left, addr right);
int compare_real(LocalRoot local, addr left, addr right);
#define less_real(m,a,b) (compare_real((m),(a), (b)) < 0)
#define less_equal_real(m,a,b) (compare_real((m),(a), (b)) <= 0)
#define greater_real(m,a,b) (compare_real((m),(a), (b)) > 0)
#define greater_equal_real(m,a,b) (compare_real((m),(a), (b)) >= 0)

int less_real_clang(LocalRoot local, addr left, addr right);
int less_equal_real_clang(LocalRoot local, addr left, addr right);

double_float cast_double_float_unsafe(addr value);

void sign_reverse_real_common(addr pos, addr *ret);
void sign_reverse_real_local(LocalRoot local, addr pos, addr *ret);

void oneplus_real_common(LocalRoot local, addr value, addr *ret);
void oneminus_real_common(LocalRoot local, addr value, addr *ret);

void plus_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret);
void plus_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret);
void plus_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret);
void plus_single_real_common(addr left, addr right, addr *ret);
void plus_double_real_common(addr left, addr right, addr *ret);
void plus_long_real_common(addr left, addr right, addr *ret);
void plus_real_common(LocalRoot local, addr left, addr right, addr *ret);
void plus_real_local(LocalRoot local, addr left, addr right, addr *ret);

void minus_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_real_fixnum_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_real_bignum_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_real_ratio_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_single_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_real_single_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_double_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_real_double_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_long_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_real_long_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_real_local(LocalRoot local, addr left, addr right, addr *ret);

void multi_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret);
void multi_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret);
void multi_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret);
void multi_single_real_common(LocalRoot local, addr left, addr right, addr *ret);
void multi_double_real_common(LocalRoot local, addr left, addr right, addr *ret);
void multi_long_real_common(LocalRoot local, addr left, addr right, addr *ret);
void multi_real_common(LocalRoot local, addr left, addr right, addr *ret);

void multi_fixnum_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_bignum_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_ratio_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_single_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_double_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_long_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_real_local(LocalRoot local, addr left, addr right, addr *ret);

void div_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret);
void div_real_fixnum_common(LocalRoot local, addr left, addr right, addr *ret);
void div_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret);
void div_real_bignum_common(LocalRoot local, addr left, addr right, addr *ret);
void div_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret);
void div_real_ratio_common(LocalRoot local, addr left, addr right, addr *ret);
void div_single_real_common(addr left, addr right, addr *ret);
void div_real_single_common(addr left, addr right, addr *ret);
void div_double_real_common(addr left, addr right, addr *ret);
void div_real_double_common(addr left, addr right, addr *ret);
void div_long_real_common(addr left, addr right, addr *ret);
void div_real_long_common(addr left, addr right, addr *ret);
void div_real_common(LocalRoot local, addr left, addr right, addr *ret);

void div_fixnum_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_real_fixnum_local(LocalRoot local, addr left, addr right, addr *ret);
void div_bignum_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_real_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void div_ratio_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_real_ratio_local(LocalRoot local, addr left, addr right, addr *ret);
void div_single_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_real_single_local(LocalRoot local, addr left, addr right, addr *ret);
void div_double_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_real_double_local(LocalRoot local, addr left, addr right, addr *ret);
void div_long_real_local(LocalRoot local, addr left, addr right, addr *ret);
void div_real_long_local(LocalRoot local, addr left, addr right, addr *ret);
void div_real_local(LocalRoot local, addr left, addr right, addr *ret);

#endif

