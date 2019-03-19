#ifndef __REAL_COMMON_HEADER__
#define __REAL_COMMON_HEADER__

#include "local.h"
#include "typedef.h"

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
void build_real(void);

/* common-lisp */
void float_common(addr *ret, addr var, addr type);

#endif

