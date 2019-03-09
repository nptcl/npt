#ifndef __MATH_EXP_HEADER__
#define __MATH_EXP_HEADER__

#include "typedef.h"

void exp_common(addr pos, addr *ret);
void sin_common(addr pos, addr *ret);
void cos_common(addr pos, addr *ret);
void tan_common(addr pos, addr *ret);
void sinh_common(addr pos, addr *ret);
void cosh_common(addr pos, addr *ret);
void tanh_common(addr pos, addr *ret);
void asin_common(addr pos, addr *ret);
void acos_common(addr pos, addr *ret);
void atan_common(addr pos, addr *ret);
void asinh_common(addr pos, addr *ret);
void acosh_common(addr pos, addr *ret);
void atanh_common(addr pos, addr *ret);

void cis_common(addr pos, addr *ret);
void atan2_common(addr left, addr right, addr *ret);
void log_natural_common(addr value, addr *ret);
void log_base_common(addr value, addr base, addr *ret);
void phase_common(addr pos, addr *ret);

#endif

