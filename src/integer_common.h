#ifndef __INTEGER_COMMON_HEADER__
#define __INTEGER_COMMON_HEADER__

#include "local.h"
#include "typedef.h"

#define output_nosign_integer_ _n(output_nosign_integer_)
#define output_nosign_comma_integer_ _n(output_nosign_comma_integer_)
#define string_nosign_comma_integer_ _n(string_nosign_comma_integer_)
#define ash_integer_common_ _n(ash_integer_common_)
#define integer_length_value_ _n(integer_length_value_)
#define integer_length_common_ _n(integer_length_common_)
#define parse_integer_clang_ _n(parse_integer_clang_)

int output_nosign_integer_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp);
int output_nosign_comma_integer_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp, size_t range, unicode comma);
int string_nosign_comma_integer_(LocalRoot local, addr *ret, addr pos,
		unsigned base, int upperp, size_t range, unicode comma);

int ash_integer_common_(LocalRoot local, addr pos, addr count, addr *ret);
int integer_length_value_(addr pos, size_t *ret);
int integer_length_common_(addr pos, addr *ret);
int parse_integer_clang_(LocalRoot local,
		addr string, size_t start, size_t end, unsigned radix, int junk,
		addr *ret, addr *position);

#endif

