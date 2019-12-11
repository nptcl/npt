#ifndef __MATH_EXP_HEADER__
#define __MATH_EXP_HEADER__

#include "typedef.h"

_g void exp_common(addr pos, addr *ret);
_g void sin_common(addr pos, addr *ret);
_g void cos_common(addr pos, addr *ret);
_g void tan_common(addr pos, addr *ret);
_g void sinh_common(addr pos, addr *ret);
_g void cosh_common(addr pos, addr *ret);
_g void tanh_common(addr pos, addr *ret);
_g void asin_common(addr pos, addr *ret);
_g void acos_common(addr pos, addr *ret);
_g void atan_common(addr pos, addr *ret);
_g void asinh_common(addr pos, addr *ret);
_g void acosh_common(addr pos, addr *ret);
_g void atanh_common(addr pos, addr *ret);

_g void cis_common(addr pos, addr *ret);
_g void atan2_common(addr left, addr right, addr *ret);
_g void atan_optional_common(addr var, addr opt, addr *ret);
_g void log_natural_common(addr value, addr *ret);
_g void log_base_common(addr value, addr base, addr *ret);
_g void log_common(addr value, addr base, addr *ret);
_g void phase_common(addr pos, addr *ret);

#endif

