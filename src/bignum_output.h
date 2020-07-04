#ifndef __BIGNUM_OUTPUT_HEADER__
#define __BIGNUM_OUTPUT_HEADER__

#include "local.h"
#include "typedef.h"

_g void decimal_charqueue_fixnum_local(LocalRoot local, addr pos, addr queue);
_g void decimal_charqueue_bignum_local(LocalRoot local, addr pos, addr queue);
_g void decimal_charqueue_integer_local(LocalRoot local, addr pos, addr queue);

_g void output_nosign_index(addr stream, size_t value, unsigned base, int upperp);
_g void output_nosign_fixnum(addr stream, fixnum value, unsigned base, int upperp);
_g void output_nosign_bignum(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp);
_g void output_nosign_comma_fixnum(LocalRoot local, addr stream,
		fixnum value, unsigned base, int upperp, size_t range, unicode comma);
_g void output_nosign_comma_bignum(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp, size_t range, unicode comma);

#endif

