#ifndef __MATH_ISQRT_HEADER__
#define __MATH_ISQRT_HEADER__

#include "define.h"
#include "local.h"
#include "typedef.h"

#define isqrt_number_common_ _n(isqrt_number_common_)
int isqrt_number_common_(LocalRoot local, addr var, addr *ret);

#endif

