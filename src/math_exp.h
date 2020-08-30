#ifndef __MATH_EXP_HEADER__
#define __MATH_EXP_HEADER__

#include "typedef.h"

#define exp_common_ _n(exp_common_)
#define sin_common_ _n(sin_common_)
#define cos_common_ _n(cos_common_)
#define tan_common_ _n(tan_common_)
#define sinh_common_ _n(sinh_common_)
#define cosh_common_ _n(cosh_common_)
#define tanh_common_ _n(tanh_common_)
#define asin_common_ _n(asin_common_)
#define acos_common_ _n(acos_common_)
#define atan_common_ _n(atan_common_)
#define asinh_common_ _n(asinh_common_)
#define acosh_common_ _n(acosh_common_)
#define atanh_common_ _n(atanh_common_)
#define cis_common_ _n(cis_common_)
#define atan2_common_ _n(atan2_common_)
#define atan_optional_common_ _n(atan_optional_common_)
#define log_natural_common_ _n(log_natural_common_)
#define log_base_common_ _n(log_base_common_)
#define log_common_ _n(log_common_)
#define phase_common_ _n(phase_common_)

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

