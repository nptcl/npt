#ifndef __REAL_COMMON_HEADER__
#define __REAL_COMMON_HEADER__

#include "local.h"
#include "typedef.h"

#define double_float_least_positive _n(double_float_least_positive)
#define double_float_least_positive_normalized _n(double_float_least_positive_normalized)
#define long_float_least_positive _n(long_float_least_positive)
#define long_float_least_positive_normalized _n(long_float_least_positive_normalized)
#define double_float_least_negative _n(double_float_least_negative)
#define double_float_least_negative_normalized _n(double_float_least_negative_normalized)
#define long_float_least_negative _n(long_float_least_negative)
#define long_float_least_negative_normalized _n(long_float_least_negative_normalized)
#define double_float_epsilon _n(double_float_epsilon)
#define double_float_negative_epsilon _n(double_float_negative_epsilon)
#define long_float_epsilon _n(long_float_epsilon)
#define long_float_negative_epsilon _n(long_float_negative_epsilon)
#define build_real_common _n(build_real_common)
#define float_common_ _n(float_common_)

/* constant */
void double_float_least_positive(addr *ret);
void double_float_least_positive_normalized(addr *ret);
void long_float_least_positive(addr *ret);
void long_float_least_positive_normalized(addr *ret);
void double_float_least_negative(addr *ret);
void double_float_least_negative_normalized(addr *ret);
void long_float_least_negative(addr *ret);
void long_float_least_negative_normalized(addr *ret);
void double_float_epsilon(addr *ret);
void double_float_negative_epsilon(addr *ret);
void long_float_epsilon(addr *ret);
void long_float_negative_epsilon(addr *ret);
void build_real_common(void);

/* common-lisp */
int float_common_(addr *ret, addr var, addr type);

#endif

