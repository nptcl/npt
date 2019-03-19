#ifndef __REAL_DECODE_HEADER__
#define __REAL_DECODE_HEADER__

#include "local.h"
#include "typedef.h"

void decode_float_common(addr pos, addr *rsig, addr *rexp, addr *sign);
void scale_float_common(addr pos, addr scale, addr *ret);
void float_radix_common(addr pos, addr *ret);
void float_sign_common(addr pos, addr opt, addr *ret);
void float_digits_common(addr pos, addr *ret);
void float_precision_common(addr pos, addr *ret);
void integer_decode_float_common(LocalRoot local,
		addr pos, addr *rsig, addr *rexp, addr *sign);
void rational_common(LocalRoot local, addr pos, addr *ret);
void rationalize_common(LocalRoot local, addr pos, addr *ret);

#endif

