#ifndef __MATH_EXP_HEADER__
#define __MATH_EXP_HEADER__

#include "typedef.h"

_g int exp_common_(addr pos, addr *ret);
_g int sin_common_(addr pos, addr *ret);
_g int cos_common_(addr pos, addr *ret);
_g int tan_common_(addr pos, addr *ret);
_g int sinh_common_(addr pos, addr *ret);
_g int cosh_common_(addr pos, addr *ret);
_g int tanh_common_(addr pos, addr *ret);
_g int asin_common_(addr pos, addr *ret);
_g int acos_common_(addr pos, addr *ret);
_g int atan_common_(addr pos, addr *ret);
_g int asinh_common_(addr pos, addr *ret);
_g int acosh_common_(addr pos, addr *ret);
_g int atanh_common_(addr pos, addr *ret);

_g int cis_common_(addr pos, addr *ret);
_g int atan2_common_(addr left, addr right, addr *ret);
_g int atan_optional_common_(addr var, addr opt, addr *ret);
_g int log_natural_common_(addr value, addr *ret);
_g int log_base_common_(addr value, addr base, addr *ret);
_g int log_common_(addr value, addr base, addr *ret);
_g int phase_common_(addr pos, addr *ret);

#endif

