#ifndef __REAL_DECODE_HEADER__
#define __REAL_DECODE_HEADER__

#include "execute.h"
#include "typedef.h"

_g int decode_float_common_(addr pos, addr *rsig, addr *rexp, addr *sign);
_g int scale_float_common_(addr pos, addr scale, addr *ret);
_g void float_radix_common(addr pos, addr *ret);
_g int float_sign_common_(addr pos, addr opt, addr *ret);
_g int float_digits_common_(addr pos, addr *ret);
_g int float_precision_common_(Execute ptr, addr pos, addr *ret);
_g int integer_decode_float_common_(Execute ptr,
		addr pos, addr *ret, addr *rexp, addr *rsign);
_g int rational_common_(Execute ptr, addr pos, addr *ret);
_g int rationalize_common_(Execute ptr, addr pos, addr *ret);

#endif

