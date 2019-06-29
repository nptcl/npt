#ifndef __REAL_COMMON_HEADER__
#define __REAL_COMMON_HEADER__

#include "local.h"
#include "typedef.h"

/* constant */
_g void double_float_least_positive(addr *ret);
_g void double_float_least_positive_normalized(addr *ret);
_g void long_float_least_positive(addr *ret);
_g void long_float_least_positive_normalized(addr *ret);
_g void double_float_least_negative(addr *ret);
_g void double_float_least_negative_normalized(addr *ret);
_g void long_float_least_negative(addr *ret);
_g void long_float_least_negative_normalized(addr *ret);
_g void double_float_epsilon(addr *ret);
_g void double_float_negative_epsilon(addr *ret);
_g void long_float_epsilon(addr *ret);
_g void long_float_negative_epsilon(addr *ret);
_g void build_real(void);

/* common-lisp */
_g void float_common(addr *ret, addr var, addr type);

#endif

