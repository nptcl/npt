#ifndef __REAL_DECODE_HEADER__
#define __REAL_DECODE_HEADER__

#include "execute.h"
#include "typedef.h"

#define decode_float_common_ _n(decode_float_common_)
#define scale_float_common_ _n(scale_float_common_)
#define float_radix_common _n(float_radix_common)
#define float_sign_common_ _n(float_sign_common_)
#define float_digits_common_ _n(float_digits_common_)
#define float_precision_common_ _n(float_precision_common_)
#define integer_decode_float_common_ _n(integer_decode_float_common_)
#define rational_common_ _n(rational_common_)
#define rationalize_common_ _n(rationalize_common_)

int decode_float_common_(addr pos, addr *rsig, addr *rexp, addr *sign);
int scale_float_common_(addr pos, addr scale, addr *ret);
void float_radix_common(addr pos, addr *ret);
int float_sign_common_(addr pos, addr opt, addr *ret);
int float_digits_common_(addr pos, addr *ret);
int float_precision_common_(Execute ptr, addr pos, addr *ret);
int integer_decode_float_common_(Execute ptr,
		addr pos, addr *ret, addr *rexp, addr *rsign);
int rational_common_(Execute ptr, addr pos, addr *ret);
int rationalize_common_(Execute ptr, addr pos, addr *ret);

#endif

