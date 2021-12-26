#ifndef __MATH_EXP_HEADER__
#define __MATH_EXP_HEADER__

#include "typedef.h"

#define LISP_PI_SINGLE	3.14159265358979323844f
#define LISP_PI_DOUBLE	3.14159265358979323844
#define LISP_PI_LONG	3.14159265358979323844L

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

int exp_common_(addr pos, addr *ret);
int sin_common_(addr pos, addr *ret);
int cos_common_(addr pos, addr *ret);
int tan_common_(addr pos, addr *ret);
int sinh_common_(addr pos, addr *ret);
int cosh_common_(addr pos, addr *ret);
int tanh_common_(addr pos, addr *ret);
int asin_common_(addr pos, addr *ret);
int acos_common_(addr pos, addr *ret);
int atan_common_(addr pos, addr *ret);
int asinh_common_(addr pos, addr *ret);
int acosh_common_(addr pos, addr *ret);
int atanh_common_(addr pos, addr *ret);

int cis_common_(addr pos, addr *ret);
int atan2_common_(addr left, addr right, addr *ret);
int atan_optional_common_(addr var, addr opt, addr *ret);
int log_natural_common_(addr value, addr *ret);
int log_base_common_(addr value, addr base, addr *ret);
int log_common_(addr value, addr base, addr *ret);
int phase_common_(addr pos, addr *ret);

#endif

