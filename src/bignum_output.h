#ifndef __BIGNUM_OUTPUT_HEADER__
#define __BIGNUM_OUTPUT_HEADER__

#include "local.h"
#include "typedef.h"

_g int decimal_charqueue_fixnum_local_(LocalRoot local, addr pos, addr queue);
_g int decimal_charqueue_bignum_local_(LocalRoot local, addr pos, addr queue);
_g int decimal_charqueue_integer_local_(LocalRoot local, addr pos, addr queue);

_g int output_nosign_index_(addr stream, size_t value, unsigned base, int upperp);
_g int output_nosign_fixnum_(addr stream, fixnum value, unsigned base, int upperp);
_g int output_nosign_bignum_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp);
_g int output_nosign_comma_fixnum_(LocalRoot local, addr stream,
		fixnum value, unsigned base, int upperp, size_t range, unicode comma);
_g int output_nosign_comma_bignum_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp, size_t range, unicode comma);

#endif

