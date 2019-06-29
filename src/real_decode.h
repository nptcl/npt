#ifndef __REAL_DECODE_HEADER__
#define __REAL_DECODE_HEADER__

#include "local.h"
#include "typedef.h"

_g void decode_float_common(addr pos, addr *rsig, addr *rexp, addr *sign);
_g void scale_float_common(addr pos, addr scale, addr *ret);
_g void float_radix_common(addr pos, addr *ret);
_g void float_sign_common(addr pos, addr opt, addr *ret);
_g void float_digits_common(addr pos, addr *ret);
_g void float_precision_common(addr pos, addr *ret);
_g void integer_decode_float_common(LocalRoot local,
		addr pos, addr *rsig, addr *rexp, addr *sign);
_g void rational_common(LocalRoot local, addr pos, addr *ret);
_g void rationalize_common(LocalRoot local, addr pos, addr *ret);

#endif

