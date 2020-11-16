#ifndef __BIGNUM_OUTPUT_HEADER__
#define __BIGNUM_OUTPUT_HEADER__

#include "local.h"
#include "typedef.h"

#define decimal_charqueue_fixnum_local_ _n(decimal_charqueue_fixnum_local_)
#define decimal_charqueue_bignum_local_ _n(decimal_charqueue_bignum_local_)
#define decimal_charqueue_integer_local_ _n(decimal_charqueue_integer_local_)
#define output_nosign_index_ _n(output_nosign_index_)
#define output_nosign_fixnum_ _n(output_nosign_fixnum_)
#define output_nosign_bignum_ _n(output_nosign_bignum_)
#define output_nosign_comma_fixnum_ _n(output_nosign_comma_fixnum_)
#define output_nosign_comma_bignum_ _n(output_nosign_comma_bignum_)

int decimal_charqueue_fixnum_local_(LocalRoot local, addr pos, addr queue);
int decimal_charqueue_bignum_local_(LocalRoot local, addr pos, addr queue);
int decimal_charqueue_integer_local_(LocalRoot local, addr pos, addr queue);

int output_nosign_index_(addr stream, size_t value, unsigned base, int upperp);
int output_nosign_fixnum_(addr stream, fixnum value, unsigned base, int upperp);
int output_nosign_bignum_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp);
int output_nosign_comma_fixnum_(LocalRoot local, addr stream,
		fixnum value, unsigned base, int upperp, size_t range, unicode comma);
int output_nosign_comma_bignum_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp, size_t range, unicode comma);

#endif

